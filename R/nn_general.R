# Define the sigmoid activation function
sigmoid <- function(x) {
1 / (1 + exp(-x))
}

# Gradient of loss function
loss_grad <- function(Y, output, loss_function, type = "klassifikation"){
num_classes <- nrow(Y)
if(type == "regression"){
  if(length(Y) != length(output)){
    stop("Y and output must have the same length for regression")
  }
  if(loss_function == "squared"){
    return(output - Y)
  } else if(loss_function == "cross-entropy"){
    stop("Cross-entropy loss is not applicable for regression")
  } else{
    stop("Unknown loss function")
  }
}
if(loss_function == "cross-entropy"){
    return(output - Y)
} else if(loss_function == "squared"){
  # Activation function sigmoid is implicitly assumed here
      return((output - Y) * output * (1 - output))
  } else{
    stop("Unknown loss function")
  }
}

# Activation function
activation_fun <- function(Z, activation = "Sigmoid") {
  if(activation == "Sigmoid"){
    return(sigmoid(Z))
  } else if(activation == "Tangenshyperbolsk"){
    return(tanh(Z))
  } else if(activation == "ReLu"){
    return(ifelse(Z > 0, Z, 0))
  } else if(activation == "Softsign"){
    return(Z / (1 + abs(Z)))
  } else if(activation == "Identitet"){
    return(Z)
  } else{
    stop("Unknown activation function")
  }
}

# Gradient of activation function
activation_grad <- function(A, activation = "Sigmoid") {
  if(activation == "Sigmoid"){
    return(A * (1 - A))
  } else if(activation == "Tangenshyperbolsk"){
    return(1 - A^2)
  } else if(activation == "ReLu"){
    return(ifelse(A > 0, 1, 0))
  } else if(activation == "Softsign"){
    # return(1 / (1 + abs(A))^2)
    return((1 - abs(A))^2)
  } else if(activation == "Identitet"){
    return(1)
  } else{
    stop("Unknown activation function")
  }
}

# Initialize weights and biases
initialize_parameters <- function(n, n1, n2, num_classes = 1, const = NA) {
  n_hidden_layers <- 2
  if(n2==0){
    n_hidden_layers <- 1
    n2 <- num_classes
  }
  if(n1==0){
    n_hidden_layers <- 0
    n1 <- num_classes
  }
  if(is.na(const)){
    weight_fun <- runif
  } else{
    weight_fun <- function(n){rep(const, n)}
  }
  out <- list(
    W1 = matrix(weight_fun(n * n1), nrow = n1, ncol = n),
    b1 = matrix(weight_fun(n1), nrow = n1, ncol = 1)
  )
  if(n_hidden_layers>0){
    out$W2 <- matrix(weight_fun(n1 * n2), nrow = n2, ncol = n1)
    out$b2 <- matrix(weight_fun(n2), nrow = n2, ncol = 1)
  }
  if(n_hidden_layers>1){
    out$W3 <- matrix(weight_fun(n2*num_classes), nrow = num_classes, ncol = n2)
    out$b3 <- matrix(weight_fun(num_classes), nrow = num_classes, ncol = 1)
  }
  return(out)
}

# Forward propagation
forward_propagation <- function(X, params, activation = "Sigmoid", type = "klassifikation") {
  Z1 <- params$W1 %*% X + matrix(params$b1, nrow = length(params$b1), ncol = ncol(X))
  A1 <- activation_fun(Z1, activation = activation)
  out <- list(Z1 = Z1, A1 = A1)
  if(is.null(params$W2)){ # No hidden layers
    if(type == "regression"){ # Regression
      out$A1 <- Z1 # No activation function for regression
    } else if(type == "klassifikation" && length(params$b1)==1){ # Sigmoid for binary classification
      out$A1 <- sigmoid(Z1)
    } else if(type == "klassifikation" && length(params$b1)>1){ # Change to softmax if num_classes > 1
      out$A1 <- t(softmax(t(Z1))) # Softmax expects input as rows
    } else{
      stop("Unknown type")
    }
    return(out)
  }
  # Now there is at least one hidden layer
  out$Z2 <- params$W2 %*% A1 + matrix(params$b2, nrow = length(params$b2), ncol = ncol(A1))
  if(!is.null(params$W3)){ # Two hidden layers
    out$A2 <- activation_fun(out$Z2, activation = activation)
    out$Z3 <- params$W3 %*% out$A2 + matrix(params$b3, nrow = length(params$b3), ncol = ncol(out$A2))
    if(type == "klassifikation"){
      if(length(params$b3)>1){
        out$A3 <- t(softmax(t(out$Z3))) # Softmax expects input as rows
      } else{
        out$A3 <- sigmoid(out$Z3)
      }
    } else if(type == "regression"){ # Regression
      out$A3 <- out$Z3 # No activation function for regression
    } else{
      stop("Unknown type")
    }
  } else{ # One hidden layer
    if(type == "klassifikation" && length(params$b2)>1){ # Change to softmax if num_classes > 1
      out$A2 <- t(softmax(t(out$Z2))) # Softmax expects input as rows
    } else if(type == "klassifikation" && length(params$b2)==1){ # Sigmoid for binary classification
      out$A2 <- sigmoid(out$Z2)
    } else if(type == "regression"){ # Regression
      out$A2 <- out$Z2 # No activation function for regression
    } else{
      stop("Unknown type")
    }
  }
  return(out)
}

# Compute the loss (binary cross-entropy or squared error)
compute_loss <- function(Y, output, loss_function = c("cross-entropy", "squared")) {
  num_classes <- nrow(Y)
  if(loss_function == "squared"){
      return(1/2*sum((Y - output)^2))
  } else if(loss_function == "cross-entropy"){
    if(is.null(num_classes) || num_classes==1){
      return(sum(-Y * log(output) - (1 - Y) * log(1 - output)))
    } else{
      return(-sum(log(output[Y!=0])))
      # return(-mean(rowSums(Y * log(output))))
    }
  } else{
    stop("Unknown loss function")
  }
}

# Backward propagation
backward_propagation <- function(X, Y, params, cache, loss_function = "cross-entropy", activation = "Sigmoid", type = "klassifikation") {
  m <- ncol(X)
  if(!is.null(params$W3)){ # Two hidden layers
    dZ3 <- loss_grad(Y, cache$A3, loss_function, type = type)
    dW3 <- dZ3 %*% t(cache$A2)
    db3 <- rowSums(dZ3)
    dA2 <- t(params$W3) %*% dZ3
    #    dZ2 <- dA2 * cache$A2 * (1 - cache$A2) # Sigmoid
    dZ2 <- dA2 * activation_grad(cache$A2, activation = activation)
  } else if(!is.null(params$W2)){ # One hidden layers
    dZ2 <- loss_grad(Y, cache$A2, loss_function, type = type)
  }
  if(!is.null(params$W2)){ # One hidden layer
    dW2 <- dZ2 %*% t(cache$A1)
    db2 <- rowSums(dZ2)
    dA1 <- t(params$W2) %*% dZ2
    #    dZ1 <- dA1 * cache$A1 * (1 - cache$A1) # Sigmoid
    dZ1 <- dA1 * activation_grad(cache$A1, activation = activation)
  } else{ # No hidden layers
    dZ1 <- loss_grad(Y, cache$A1, loss_function, type = type)
  }
  dW1 <- dZ1 %*% t(X)
  db1 <- rowSums(dZ1)
  out <- list(dW1 = dW1, db1 = db1)
  if(!is.null(params$W2)){
    out$dW2 <- dW2
    out$db2 <- db2
  }
  if(!is.null(params$W3)){
    out$dW3 <- dW3
    out$db3 <- db3
  }
  return(out)
}

# Update parameters
update_parameters <- function(params, grads, learning_rate) {
  params$W1 <- params$W1 - learning_rate * grads$dW1
  params$b1 <- params$b1 - learning_rate * grads$db1
  if(!is.null(params$W2)){
    params$W2 <- params$W2 - learning_rate * grads$dW2
    params$b2 <- params$b2 - learning_rate * grads$db2
  }
  if(!is.null(params$W3)){
    params$W3 <- params$W3 - learning_rate * grads$dW3
    params$b3 <- params$b3 - learning_rate * grads$db3
  }
  params
}

# Train the neural network
train_neural_network <- function(X, Y, n1, n2, iterations, learning_rate, params = NULL, loss_function = "cross-entropy", activation = "Sigmoid", trace = FALSE, type = "klassifikation") {
  if(n1==0 & n2>0){
    n1 <- n2
  }
  n <- nrow(X)
  if(is.null(params)){
    params <- initialize_parameters(n, n1, n2, num_classes = nrow(Y))
  }
  cache_list <- loss_list <- grads_list <- params_list <- list()
  for (i in 1:iterations) {
    cache <- forward_propagation(X, params, activation = activation, type = type)
    output <- if(!is.null(params$W3)){cache$A3} else{ if(!is.null(params$W2)){cache$A2} else{cache$A1} }
    loss_list[[i]] <- loss <- compute_loss(Y, output, loss_function)
    grads <- backward_propagation(X, Y, params, cache, loss_function = loss_function, activation = activation, type = type)
    params <- update_parameters(params, grads, learning_rate)
    if (iterations >= 5 && i %% floor(iterations/5) == 0) {
      cat("Iteration", i, "loss:", loss, "\n")
    }
    if(trace){
      cache_list[[i]] <- cache
      grads_list[[i]] <- grads
      params_list[[i]] <- params
    }
  }
  if(trace){
    return(list(cache = cache[[iterations]], loss = unlist(loss_list), grads = grads_list[[iterations]], params = params[[iterations]], grads_list = grads_list, cache_list = cache_list, params_list = params_list))
  } else{
    return(list(cache = cache, loss = unlist(loss_list), grads = grads, params = params))
  }
}

#' General neural network with at most two hidden layers
#'
#' @param formula response ~ predictors
#' @param data data frame
#' @param weights Starting weights
#' @param n_hidden integer vector of length 2 with number of neurons in the hidden layers (can be zero)
#' @param activation one of "Sigmoid", "Tangenshyperbolsk", "ReLu", "Softsign", "Identitet"
#' @param eta learning rate
#' @param iter number of iterations
#' @param scale logical to scale the input data
#' @param lossfun one of "squared", "cross-entropy"
#' @param type one of "regression", "klassifikation"
#' @param trace logical to save each iteration
#'
#' @returns Fitted neural network model as an object of class "nn"
#'
#'
#' @examples
#' ir <- iris
#' ir[,1:4] <- scale(ir[,1:4])
#' fit_ir <- nn_fun(Species ~ ., ir, n_hidden = c(3,5), eta = 0.01, iter = 1000,
#'   lossfun = "cross-entropy", activation = "Sigmoid", type = "klassifikation")
#'
#' @export
nn_fun <- function(formula, data, weights = NA, n_hidden = c(1,1), activation = "Sigmoid", eta = 0.01, iter = 1000, scale = TRUE, lossfun = "squared", type = "klassifikation", trace = FALSE){
  ## Assumes response variable has values +/-1.
  ## Unchanged for "identity" and "softsign". Changed to 0/1 for "sigmoid".
  x <- model.matrix(formula, data = data)
  ## Non-omitted (non-NA) rownames
  OK <- rownames(x)
  ## Omit intercept column
  x <- x[,colnames(x)!="(Intercept)"]
  y_name <- as.character(formula)[2]
  y <- data[OK,y_name,drop=TRUE]

  lvls <- NULL
  if(type == "klassifikation" | is.character(y) | is.factor(y)){
    # Ensure y is a factor
    if(!is.factor(y)){
      y <- factor(y)
    }
    lvls <- levels(y)
    if(length(levels(y))==1){stop("Response variable must have at least two levels")}
    if(length(levels(y))==2){
      if(length(setdiff(lvls, c(0,1))) == 0){ # 0-1 data given, so make 1 reference level
        y <- relevel(y, "1")
        lvls <- levels(y)
      }
      y <- ifelse(y==levels(y)[1], 1, 0)
    } else{
      y <- model.matrix(~y-1, data = data)
    }
  }
  y <- t(y)
  scale_val <- center_val <- NULL
  if(scale){
    x <- scale(x)
    scale_val <- attr(x, "scaled:scale")
    center_val <- attr(x, "scaled:center")
  }
  X <- t(x)
  if(length(weights)==1){
    params <- initialize_parameters(nrow(X), n_hidden[1], n_hidden[2], num_classes = nrow(y), const = weights)
  } else{
    params <- initialize_parameters(nrow(X), n_hidden[1], n_hidden[2], num_classes = nrow(y))
  }
  rslt <- train_neural_network(X, y, n1 = n_hidden[1], n2 = n_hidden[2], iterations = iter, learning_rate = eta, params = params, loss_function = lossfun, activation = activation, trace = trace, type = type)
  rslt$formula <- formula
  environment(rslt$formula) <- baseenv()
  rslt$levels <- lvls
  rslt$activation <- activation
  rslt$scale_val <- scale_val
  rslt$center_val <- center_val
  rslt$type <- type
  rslt$rowids <- OK
  class(rslt) <- "nn"
  return(rslt)
}
#
# Example usage
# set.seed(123)
# n_sample <- 5
# n <- 3  # Number of input neurons
# n1 <- 2  # Number of neurons in the first hidden layer
# n2 <- 4  # Number of neurons in the second hidden layer
# X <- matrix(runif(n * n_sample), nrow = n)  # Example input data
# # Y <- matrix(sample(0:1, n_sample, replace = TRUE), nrow = 1)  # Example output data
# Y <- replicate(n_sample, sample(c(1,0,0)))
# iterations <- 1000
# learning_rate <- 0.01
#
# trained_params <- train_neural_network(X, Y, n1, n2, iterations, learning_rate)

#' Predict method for neural network
#' @param object Fitted neural network model
#' @param newdata Data frame with new data
#' @param type One of "response", "class"
#' @param ... Additional arguments, currently not used
#' @return Predicted values
#'
#' @examples
#' nn_iris <- nn_fun(Species ~ ., iris, n_hidden = c(3,5), eta = 0.01, iter = 1000,
#'   lossfun = "cross-entropy", activation = "Sigmoid",
#'   type = "klassifikation", scale = TRUE)
#' predict(nn_iris, iris, type = "class")
#'
#' nn_trees <- nn_fun(Volume ~ ., trees, n_hidden = c(3,5), eta = 0.01, iter = 1000,
#'   lossfun = "squared", activation = "Sigmoid",
#'   type = "regression", scale = TRUE)
#' predict(nn_trees, trees, type = "response")
#' @export
predict.nn <- function(object, newdata, type = "response", ...) {
  x <- model.matrix(object$formula, data = newdata)
  x <- x[,colnames(x)!="(Intercept)",drop=FALSE]
  if(!is.null(object$scale_val)){
    x <- scale(x, center = object$center_val, scale = object$scale_val)
  }
  X <- t(x)
  cache <- forward_propagation(X, object$params, activation = object$activation, type = object$type)
  output <- if(!is.null(object$params$W3)){cache$A3} else{ if(!is.null(object$params$W2)){cache$A2} else{cache$A1} }
  output <- t(output)
  if(type == "response"){
    if(!is.null(object$levels)){
      #' For binary classification we only have a single probability for the first class
      object$levels <- object$levels[1]
      colnames(output) <- object$levels
    }
    return(output)
  } else{
    if(type != "class"){
      stop("Type must be 'response' or 'class'")
    }
    if(length(object$levels)==2){
      return(as.vector(ifelse(output>=0.5, object$levels[1], object$levels[2])))
    } else{
      return(object$levels[apply(output, 1, which.max)])
    }
  }
}

#' Cross-validation for neural network
#' @param formula response ~ predictors
#' @param data data frame
#' @param ... Additional arguments passed to `nn_fun`
#' @param k Number of folds for cross-validation
#' @return Mean accuracy across folds
#' @export
#' @examples
#' cv <- nn_fun_cv(Species ~ ., iris, n_hidden = c(3,5), eta = 0.01, iter = 1000,
#' lossfun = "cross-entropy")
nn_fun_cv <- function(formula, data, ..., k=5){
  # Cross-validation for neural network
  # Retain only valid rows
  junk <- model.matrix(formula, data = data)
  ## Non-omitted (non-NA) rownames
  OK <- rownames(junk)
  data <- data[OK,]
  # Now run cross-validation
  alist <- list(...)
  alist$formula <- formula
  y_name <- as.character(formula)[2]
  n <- nrow(data)
  n0 <- floor(n/k)
  full_folds <- rep(1:k, each = n0)
  partial_folds <- numeric(0)
  if(n%%k>0){
    partial_folds <- 1:(n-k*n0)
  }
  id <- sample(c(full_folds, partial_folds))
  prec <- rep(0, k)
  for(i in seq_len(k)){
    train <- data[id!=i,]
    test <- data[id==i,]
    alist$data <- train
    fit <- do.call(nn_fun, alist)
    if(is.null(fit$levels)){
      stop("Cross validation only possible for classification problems at the moment.")
    }
    pred <- predict(fit, newdata = test, type = "class")
    y <- test[[y_name]]
    prec[i] <- mean(pred==y)
    cat("Fold", i, "accuracy:", prec[i], "\n")
    # if(prec[i]<.5){
    #   prec[i] <- 1-prec[i]
    # }
  }
  return(mean(prec))
}
