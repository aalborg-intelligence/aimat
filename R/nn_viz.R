#' Visualize a neural network
#'
#' @param nn FIXME
#'
#' @examples
#' ir <- iris
#' ir[,1:4] <- scale(ir[,1:4])
#' fit_ir <- nn_fun(Species ~ ., ir, n_hidden = c(3,5), eta = 0.01, iter = 1000,
#'   lossfun = "cross-entropy", activation = "Sigmoid", type = "klassifikation")
#' if(requireNamespace("visNetwork", quietly = TRUE)){
#'   nn_viz(fit_ir)
#' }
#'
#' @export
nn_viz <- function(nn) {
  params <- nn$params
  cache <- nn$cache
  # Input layer
  n <- ncol(params$W1)
  label = c("X_bias", paste0("X_", 1:n))
  group = rep("Input", n+1)
  level = rep(1, n+1)
  shape = c("box", rep("circle", n))
  n1 <- params$W1 |> nrow()
  n2 <- params$W2 |> nrow()
  if(is.null(n2)){ n2 <- 0 }
  if(n2>0){ # At least one hidden layer
    label <- c(label, "Y_bias", paste0("Y_", seq_len(n1)))
    group <- c(group, rep("Hidden1", n1+1))
    level <- c(level, rep(2, n1+1))
    shape <- c(shape, "box", rep("circle", n1))
  }
  num_classes <- params$W3 |> nrow()
  if(!is.null(num_classes)){ # Two hidden layers
    label <- c(label, "Z_bias", paste0("Z_", seq_len(n2)))
    group <- c(group, rep("Hidden2", n2+1))
    level <- c(level, rep(3, n2+1))
    shape <- c(shape, "box", rep("circle", n2))
  } else{
    num_classes <- n2
  }
  if(num_classes==0){
    num_classes <- n1
  }
  if(n2==0){
    n2 <- num_classes
  }
  o_label <- if(num_classes==1){"O"} else{paste0("O_", seq_len(num_classes))}
  label <- c(label, o_label)
  id <- 1:length(label)
  group <- c(group, rep("Output", num_classes))
  level <- c(level, rep(max(level)+1, num_classes))
  shape <- c(shape, rep("circle", num_classes))
  nodes <- data.frame(
    id,
    label,
    group,
    level,
    shape
  )
  # nodes <- data.frame(
  #   # id = c(1:n, (n+1):(n+n1), (n+n1+1):(n+n1+n2), (n+n1+n2+1):(n+n1+n2+num_classes)),
  #   id = 1:(n+1+n1+1+n2+1+num_classes),
  #   label = c("X_bias", paste0("X_", 1:n), "Y_bias", paste0("Y_", seq_len(n1)), "Z_bias", paste0("Z_", 1:n2), paste0("O_", 1:num_classes)),
  #   group = c(rep("Input", n+1), rep("Hidden1", n1+1), rep("Hidden2", n2+1), rep("Output", num_classes)),
  #   level = c(rep(1, n+1), rep(2, n1+1), rep(3, n2+1), rep(4, num_classes)),
  #   shape = c("box", rep("circle", n), "box", rep("circle", n1), "box", rep("circle", n2), rep("circle", num_classes))
  # )
  # nodes$title <- nodes$label
  # nodes$title[(n+1):(n+n1)] <- paste0(nodes$title[(n+1):(n+n1)], "=", round(cache$A1, 2))

  edges <- data.frame(
    from = integer(),
    to = integer(),
    value = numeric(),
    label = character(),
    title = character()
  )

  # Bias 1
    for (j in 1:n1) {
      weight <- round(params$b1[j, 1], 3)
      edges <- rbind(edges, data.frame(from = 1, to = n+1 + j+ ifelse(is.null(params$b2), 0, 1), value = abs(weight), label = weight, title = paste("Weight:", weight)))
    }

  # Layer 1
  for (i in 1:n) {
    for (j in 1:n1) {
      weight <- round(params$W1[j, i], 3)
      edges <- rbind(edges, data.frame(from = i+1, to = n+1 + j + ifelse(is.null(params$b2), 0, 1), value = abs(weight), label = weight, title = paste("Weight:", weight)))
    }
  }
  if(!is.null(params$b2)){
  # Bias 2
  for (j in seq_len(n2)) {
    weight <- round(params$b2[j, 1], 3)
    edges <- rbind(edges, data.frame(from = n+2, to = n+2+n1 + j + ifelse(is.null(params$b3), 0, 1), value = abs(weight), label = weight, title = paste("Weight:", weight)))
  }

  # Layer 2
  for (i in seq_len(n1)) {
    for (j in seq_len(n2)) {
      weight <- round(params$W2[j, i], 3)
      edges <- rbind(edges, data.frame(from = n+2 + i, to = n+2 + n1 + j + ifelse(is.null(params$b3), 0, 1), value = abs(weight), label = weight, title = paste("Weight:", weight)))
    }
  }
  }

  # Bias 3
  if(!is.null(params$b3)){
  for (j in 1:num_classes) {
    weight <- round(params$b3[j, 1], 3)
    edges <- rbind(edges, data.frame(from = n+3+n1, to = n+3+n1+n2 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
  }

  # Layer 3
  for (i in 1:n2) {
    for (j in 1:num_classes) {
      weight <- round(params$W3[j, i], 3)
      edges <- rbind(edges, data.frame(from = n+3+n1 + i, to = n+3 + n1 + n2 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
    }
  }
  }

# browser()
  visNetwork::visNetwork(nodes, edges) |>
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    visNetwork::visInteraction(hover = TRUE) |>
    visNetwork::visHierarchicalLayout(direction = "LR")
}



### THINKING ABOUT HOW TO VISUALIZE THE NEURAL NETWORK WITHOUT BIAS TERMS ###
# nn_viz2 <- function(nn) {
#   params <- nn$params
#   cache <- nn$cache
#   # Input layer
#   n <- ncol(params$W1)
#   label = c(paste0("X_", 1:n))
#   group = rep("Input", n)
#   level = rep(1, n)
#   shape = c(rep("circle", n))
#   n1 <- params$W1 |> nrow()
#   n2 <- params$W2 |> nrow()
#   if(is.null(n2)){ n2 <- 0 }
#   if(n2>0){ # At least one hidden layer
#     label <- c(label, paste0("Y_", seq_len(n1)))
#     group <- c(group, rep("Hidden1", n1))
#     level <- c(level, rep(2, n1))
#     shape <- c(shape, rep("circle", n1))
#   }
#   num_classes <- params$W3 |> nrow()
#   if(!is.null(num_classes)){ # Two hidden layers
#     label <- c(label, paste0("Z_", seq_len(n2)))
#     group <- c(group, rep("Hidden2", n2))
#     level <- c(level, rep(3, n2))
#     shape <- c(shape, rep("circle", n2))
#   } else{
#     num_classes <- n2
#   }
#   if(num_classes==0){
#     num_classes <- n1
#   }
#   if(n2==0){
#     n2 <- num_classes
#   }
#   o_label <- if(num_classes==1){"O"} else{paste0("O_", seq_len(num_classes))}
#   label <- c(label, o_label)
#   id <- 1:length(label)
#   group <- c(group, rep("Output", num_classes))
#   level <- c(level, rep(max(level)+1, num_classes))
#   shape <- c(shape, rep("circle", num_classes))
#   nodes <- data.frame(
#     id,
#     label,
#     group,
#     level,
#     shape
#   )
#   # nodes <- data.frame(
#   #   # id = c(1:n, (n+1):(n+n1), (n+n1+1):(n+n1+n2), (n+n1+n2+1):(n+n1+n2+num_classes)),
#   #   id = 1:(n+1+n1+1+n2+1+num_classes),
#   #   label = c("X_bias", paste0("X_", 1:n), "Y_bias", paste0("Y_", seq_len(n1)), "Z_bias", paste0("Z_", 1:n2), paste0("O_", 1:num_classes)),
#   #   group = c(rep("Input", n+1), rep("Hidden1", n1+1), rep("Hidden2", n2+1), rep("Output", num_classes)),
#   #   level = c(rep(1, n+1), rep(2, n1+1), rep(3, n2+1), rep(4, num_classes)),
#   #   shape = c("box", rep("circle", n), "box", rep("circle", n1), "box", rep("circle", n2), rep("circle", num_classes))
#   # )
#   # nodes$title <- nodes$label
#   # nodes$title[(n+1):(n+n1)] <- paste0(nodes$title[(n+1):(n+n1)], "=", round(cache$A1, 2))
#
#   edges <- data.frame(
#     from = integer(),
#     to = integer(),
#     value = numeric(),
#     label = character(),
#     title = character()
#   )
#
#   # Bias 1
#     # for (j in 1:n1) {
#     #   weight <- round(params$b1[j, 1], 3)
#     #   edges <- rbind(edges, data.frame(from = 1, to = n+1 + j+ ifelse(is.null(params$b2), 0, 1), value = abs(weight), label = weight, title = paste("Weight:", weight)))
#     # }
#
#   # Layer 1
#   for (i in 1:n) {
#     for (j in 1:n1) {
#       weight <- round(params$W1[j, i], 3)
#       edges <- rbind(edges, data.frame(from = i+1, to = n+1 + j + ifelse(is.null(params$b2), 0, 1), value = abs(weight), label = weight, title = paste("Weight:", weight)))
#     }
#   }
#   if(!is.null(params$b2)){
#   # Bias 2
#   for (j in seq_len(n2)) {
#     weight <- round(params$b2[j, 1], 3)
#     edges <- rbind(edges, data.frame(from = n+2, to = n+2+n1 + j + ifelse(is.null(params$b3), 0, 1), value = abs(weight), label = weight, title = paste("Weight:", weight)))
#   }
#
#   # Layer 2
#   for (i in seq_len(n1)) {
#     for (j in seq_len(n2)) {
#       weight <- round(params$W2[j, i], 3)
#       edges <- rbind(edges, data.frame(from = n+2 + i, to = n+2 + n1 + j + ifelse(is.null(params$b3), 0, 1), value = abs(weight), label = weight, title = paste("Weight:", weight)))
#     }
#   }
#   }
#
#   # Bias 3
#   if(!is.null(params$b3)){
#   for (j in 1:num_classes) {
#     weight <- round(params$b3[j, 1], 3)
#     edges <- rbind(edges, data.frame(from = n+3+n1, to = n+3+n1+n2 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
#   }
#
#   # Layer 3
#   for (i in 1:n2) {
#     for (j in 1:num_classes) {
#       weight <- round(params$W3[j, i], 3)
#       edges <- rbind(edges, data.frame(from = n+3+n1 + i, to = n+3 + n1 + n2 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
#     }
#   }
#   }
#
# # browser()
#   visNetwork(nodes, edges) |>
#     visEdges(arrows = "to") |>
#     visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
#     visInteraction(hover = TRUE) |>
#     visHierarchicalLayout(direction = "LR")
# }
