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
#' @param verbose FIXME
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

w2v_find_context <- function(word, corpus, win) {
  i <- which(corpus == word)
  i_neg <- list()
  i_pos <- list()
  for(j in seq_len(win)){
    i_neg[[j]] <- i - j
    i_pos[[j]] <- i + j
  }
  ii <- c(unlist(i_neg), unlist(i_pos))
  ii <- ii[ii > 0 & ii <= length(corpus)]
  return(data.frame(input = word, context = corpus[ii]))
}

#' Build word2vec data from a corpus represented as a vector of words
#'
#' @param corpus vector of words
#' @param win integer, size of the context window
#'
#' @returns A list containing the following elements:
#' - `X`: Sparse matrix of one-hot encoded input vectors
#' - `Y`: Sparse matrix of one-hot encoded output vectors
#' - `vocab`: Vector of unique words in the corpus
#' - `pairs`: Data frame of word pairs in context
#' @export
#'
#' @examples
#' w2v_data <- w2v_build_data(bog, win = 1)
w2v_build_data <- function(corpus, win = 1){
  # Find unique words in the corpus
  vocab <- sort(unique(corpus))

  # Word pairs in context
  pairs <- lapply(vocab, w2v_find_context, corpus = corpus, win = win) |> Reduce(rbind, x = _)

  nn_input <- factor(pairs$input, levels = vocab)
  nn_output <- factor(pairs$context, levels = vocab)

  X <- Matrix(model.matrix(~ nn_input - 1), sparse = TRUE)
  Y <- Matrix(model.matrix(~ nn_output - 1), sparse = TRUE)
  colnames(X) <- colnames(Y) <- vocab
  rslt <- list(X = X, Y = Y, vocab = vocab, pairs = pairs)
  return(rslt)
}

#' Word2vec Neural Network
#'
#' @param corpus Vector of words
#' @param win Integer, size of the context window
#' @param hidden_dim Dimension of the hidden layer
#' @param epochs Number of training epochs
#' @param learning_rate Learning rate
#' @param verbose Integer, verbosity level (0 = no output, >0 = output every `verbose` epochs)
#' @param weights FIXME
#' 
#'
#' @examples
#' w <- w2v(bog, hidden_dim = 2, epochs = 10, learning_rate = 0.01, verbose = 1)
#'
#' @export
w2v <- function(corpus, win = 1, hidden_dim = 3, epochs = 100, learning_rate = 0.01, verbose = 10, weights = NULL){
  data <- w2v_build_data(corpus, win)
  X <- data$X
  Y <- data$Y
  vocab <- data$vocab

  weights <- w2v_nn(X, Y, hidden_dim, epochs, learning_rate, verbose = verbose, weights = weights)
  rslt <- list(W1 = weights$W1, W2 = weights$W2, vocab = vocab)
  class(rslt) <- "w2v"
  return(rslt)
}

#' Type function for words (non-exportet helper function)
type_fun <- function(x) {
  switch(x,
         "ko" = "dyr",
         "hest" = "dyr",
         "marsvin" = "dyr",
         "æsel" = "dyr",
         "bil" = "ting",
         "cykel" = "ting",
         "hus" = "ting",
         "skur" = "ting",
         "blå" = "farve",
         "grøn" = "farve",
         "blåt" = "farve",
         "grønt" = "farve",
         "ser" = "verbum",
         "får" = "verbum",
         "har" = "verbum",
         "og" = "og",
         "en" = "artikel",
         "et" = "artikel",
         "." = ".",
         "Ib" = "person",
         "Kim" = "person",
         "Ole" = "person",
         "Bo" = "person",
         "Anne" = "person",
         "Eva" = "person",
         "Ida" = "person",
         "Mia" = "person")
}

#' Plot word2vec model
#'
#' @param x Word2Vec model object (list with W1, W2 and vocab)
#' @param ... Additional arguments (not used)
#' @param which Integer, 1 or 2. If 1, plot W1 (input layer), if 2, plot W2 (output layer)
#'
#' @export
#'
#' @examples
#' w <- w2v(bog, hidden_dim = 2, epochs = 10, learning_rate = 0.01, verbose = 1)
#' plot(w, which = 1)
plot.w2v <- function(x, ..., which = 1){
  # if(!inherits(x, "w2v")){
  #   stop("x must be a w2v object")
  # }
  if(which==1){
    W <- x$W1
    } else if(which == 2){
      W <- t(x$W2)
    } else{
      stop("which must be 1 or 2")
    }
  requireNamespace(dplyr)   ## FIXME
  requireNamespace(plotly)  ## FIXME
  dat <- cbind(ord = x$vocab, as.data.frame(as.matrix(W)))
  if(all(unique(dat$ord) %in% unique(bog))){
    dat$type <- sapply(dat$ord, type_fun)
  } else{
    dat$type <- dat$ord
  }
  if(ncol(W) == 3){
    # stop("Plotting only works for three-dimensional word vectors")
  dat_dir <- rowwise(dat) |> mutate(u = V1/sqrt(V1^2 + V2^2 + V3^2),
                                    v = V2/sqrt(V1^2 + V2^2 + V3^2),
                                    w = V3/sqrt(V1^2 + V2^2 + V3^2))
  # Add zero and NA between each row of pos_dat_dir
  dat_dir |> group_by(ord) |> reframe(ord = rep(ord, 3), V1 = c(V1, 0, NA), V2 = c(V2, 0, NA), V3 = c(V3, 0, NA), type = rep(type, 3), u = c(u, NA, NA), v = c(v, NA, NA), w = c(w, NA, NA)) |>
    plot_ly() |>
    add_trace(x = ~V1, y = ~V2, z = ~V3, type = "scatter3d", mode = "lines", line = list(width = 2), color = ~type, text = ~ord) |>
    add_trace(x = ~V1, y = ~V2, z = ~V3, u = ~u, v = ~v, w = ~w, type = "cone", anchor = "tail", showscale = FALSE, colorscale = list(list(0, "black"), list(1, "black")), sizeref = 1, sizemode = "absolute")
  } else if(ncol(W) == 2){
    dat_dir <- rowwise(dat) |> mutate(u = V1/sqrt(V1^2 + V2^2),
                                      v = V2/sqrt(V1^2 + V2^2))
    # Add zero and NA between each row of pos_dat_dir
    dat_dir |> group_by(ord) |> reframe(ord = rep(ord, 3), V1 = c(V1, 0, NA), V2 = c(V2, 0, NA), type = rep(type, 3), u = c(u, NA, NA), v = c(v, NA, NA)) |>
      plot_ly() |>
      add_trace(x = ~V1, y = ~V2, type = "scatter", mode = "lines", line = list(width = 2), color = ~type, text = ~ord)
      # add_trace(x = ~V1, y = ~V2, u = ~u, v = ~v, type = "cone", anchor = "tail", showscale = FALSE, colorscale = list(list(0, "black"), list(1, "black")), sizeref = 1)
  } else{
    stop("Plotting only works for two- or three-dimensional word vectors")
  }
}

