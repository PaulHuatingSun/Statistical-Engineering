#' k-Nearest Neighbors Cross-Validation Function
#'
#' This function predicts output from existed variables by using k-nearest 
#'neighbors cross-validation 
#'
#' @param train input data frame.
#' @param cl true class value of training data.
#' @param k_nn integer representing the number of neighbors.
#' @param k_cv integer representing the number of folds.
#' 
#' @keywords prediction
#' 
#' @return Return a list of objects:
#'  - \code{class}: Vector of predicted class for all observations,
#'  - \code{cv_err}: Numeric with the cross-validation misclassification error.
#' 
#' @importFrom class knn
#' 
#' @examples 
#' data(my_penguins)
#' penguins_df <- na.omit(my_penguins)
#' my_knn_cv(penguins_df[ , 3:6], penguins_df$species, 1, 5)
#' 
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Define a variable fold that randomly assigns observations to folds with equal probability
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  # Store the misclassification rate
  misclassification <- rep(NA, k_cv)
  
  for (i in 1:k_cv) {
    # Split input data frame to train and test
    data_train <- train[fold != i, ]
    data_test <- train[fold == i, ]
    # Split true class value of training data into train and test
    cl_train <- cl[fold != i]
    cl_test <- cl[fold == i]
    # store knn output into a variable
    output <- class::knn(train = data_train, test = data_test, cl = cl_train, k = k_nn)
    # Calculate the misclassification rate
    misclassification[i] <- sum(output != cl_test)/length(cl_test)
  }
  # Prediction output class
  class <- class::knn(train = train, test = train, cl = cl, k = k_nn)
  # Cross-validation misclassification error
  cv_err <- mean(misclassification)
  # Return the list of desired objects
  return(list("class" = class, "cv_err" = cv_err))
}
