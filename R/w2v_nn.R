# Define the softmax function
softmax <- function(x) {
  exp_x <- exp(x)
  if(is.null(dim(x))){
    return(exp_x/sum(exp_x))
  }
  exp_x / rowSums(exp_x)
}

# Define the cross-entropy loss function
ce_loss <- function(y_true, y_pred, mean = TRUE) {
  rslt <- -sum(y_true * log(y_pred))
  if(!mean){
    return(rslt)
  }
  if(is.null(dim(y_true))){
    m <- 1
  } else{
    m <- nrow(y_true)
  }
  return(rslt / nrow(y_true))
}

# Initialize weights for the neural network
w2v_init_weights <- function(input_dim, hidden_dim, output_dim) {
  list(
    W1 = Matrix(rnorm(input_dim * hidden_dim), nrow = input_dim, ncol = hidden_dim, sparse = TRUE),
    W2 = Matrix(rnorm(hidden_dim * output_dim), nrow = hidden_dim, ncol = output_dim, sparse = TRUE)
  )
}

# Forward pass through the network
w2v_feedforward <- function(X, weights) {
  Z1 <- X %*% weights$W1
  # A1 <- pmax(Z1, 0)  # ReLU activation
  A1 <- Z1  # Identity activation
  Z2 <- A1 %*% weights$W2
  A2 <- softmax(Z2)
  list(A1 = A1, A2 = A2)
}

# Backward pass and weight updates
w2v_backprop <- function(X, Y, forward_cache, weights, learning_rate) {
  N <- nrow(X)
  A1 <- forward_cache$A1
  A2 <- forward_cache$A2

  dZ2 <- A2 - Y
  dW2 <- t(A1) %*% dZ2

  # dZ1 <- (dZ2 %*% t(weights$W2)) * (A1 > 0) #ReLU
  dZ1 <- (dZ2 %*% t(weights$W2)) #Identity
  dW1 <- t(X) %*% dZ1

  weights$W1 <- weights$W1 - learning_rate * dW1/N
  weights$W2 <- weights$W2 - learning_rate * dW2/N

  weights
}


#' Word2vec Neural Network
#'
#' @param X Matrix of one-hot encoded input vectors
#' @param Y Matrix of one-hot encoded output vectors
#' @param hidden_dim Dimension of the hidden layer
#' @param epochs Number of training epochs
#' @param learning_rate Learning rate
#' @param weights Initial weights for the network (random if NULL)
#'
#' @returns Named list contaning trained weights for the network
#' @export
#' @import Matrix
#' @examples
#' # Example usage
#' library(Matrix)
#' n <- 10  # Dimension of input and output
#' m <- 3   # Dimension of the first hidden layer
#'
#' # Create input matrix with several one-hot vectors
#' X <- Matrix(0, nrow = 5, ncol = n, sparse = TRUE)
#' for (i in 1:5) {
#'   X[i, sample(1:n, 1)] <- 1
#' }
#'
#' # Create output matrix with corresponding one-hot vectors
#' Y <- Matrix(0, nrow = 5, ncol = n, sparse = TRUE)
#' for (i in 1:5) {
#'   Y[i, sample(1:n, 1)] <- 1
#' }
#'
#' # Train the network
#' trained_weights <- w2v_nn(X, Y, m, epochs = 100, learning_rate = 0.1, verbose = 10)

w2v_nn <- function(X, Y, hidden_dim, epochs, learning_rate, weights = NULL, verbose = 10) {
  input_dim <- ncol(X)
  output_dim <- ncol(Y)
  stopifnot(input_dim == output_dim)
  stopifnot(nrow(X) == nrow(Y))
  if (is.null(weights)){
    weights <- w2v_init_weights(input_dim, hidden_dim, output_dim)
  }

  for (epoch in 1:epochs) {
    forward_cache <- w2v_feedforward(X, weights)
    loss <- ce_loss(Y, forward_cache$A2)
    if(verbose>0 & epoch %% verbose == 0){
      cat("Epoch:", epoch, "Loss:", loss, "\n")
    }
    weights <- w2v_backprop(X, Y, forward_cache, weights, learning_rate)
  }

  weights
}
