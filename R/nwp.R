#### Functions for next word prediction

nwp_one_context <- function(i, context_size, corpus) {
  corpus[i:(i+context_size)]
}

nwp_one_numeric <- function(x, w2v){
  index <- match(x, w2v$vocab)
  if(any(is.na(index))){
    stop("Word not found in vocabulary: ", x[is.na(index)])
  }
  rslt <- as.data.frame(t(as.numeric(t(w2v$W1[head(index, -1),]))))
  rslt$target <- factor(w2v$vocab[tail(index,1)], levels = w2v$vocab)
  return(rslt)
}

#' Make next word prediction data
#'
#' @param corpus Vector of words.
#' @param context_size Integer, size of the context window (using this many words to predict the next).
#' @param w2v Word2Vec model object. This is a list with three elements: `W1`, `W2` and `vocab`. `W1` and `W2` are matrices of word vectors, and `vocab` is a vector of words.
#' @param return Character, one of "both", "words" or "numeric". If "both", returns a list with two elements: `words` and `numeric`. If "words", returns only the `words` element. If "numeric", returns only the `numeric` element. Default is "both".
#'
#' @returns A list containing data to train next word prediction. The list contains one or both of the following elements:
#' - words: a data.frame with `context_size`+1 columns of words where the last column is the target word.
#' - numeric: a numeric matrix
#' @export
#'
#' @examples
#' bog_w2v <- w2v(bog, win = 1, hidden_dim = 3, epochs = 5, learning_rate = 0.01, verbose = 0)
#' bog_data <- nwp_make_word_data(corpus = bog, context_size = 2, w2v = bog_w2v)
nwp_make_word_data <- function(corpus, context_size, w2v, return = "both") {
  # Create a list of data frames for each context
  dat_list <- lapply(seq_len(length(corpus)-context_size), nwp_one_context, context_size = context_size, corpus = corpus)
  neurale_data <- lapply(dat_list, nwp_one_numeric, w2v = w2v)
  # Combine the list of data frames into a single data frame
  rslt <- list(words = do.call(rbind, dat_list),
               numeric = do.call(rbind, neurale_data))
  if(return == "both"){
    return(rslt)
  } else if (return == "words"){
    return(rslt["words"])
  } else if (return == "numeric"){
    return(rslt["numeric"])
  } else {
    stop("Invalid return value. Use 'both', 'words', or 'numeric'.")
  }
}
#
# nwp_train <- function(data, n_hidden, epochs = 100, eta = 0.01, scale = TRUE){
#   fit <- nn_fun(target ~ ., data = data$numeric, n_hidden = n_hidden, epochs = epochs, eta = eta, scale = scale)
# }
#
