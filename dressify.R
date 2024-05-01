data = read.csv("C:\\Users\\DELL\\OneDrive\\Desktop\\Desktop\\College\\DATA SCIENCE\\PROJECTS\\Dressify challenge\\Data\\RDS_Assignment_Train.csv")

#Duplicates
data[duplicated(data$Dress_CODE)]


#datatype of each column
sapply(data, class)

#converting into factors
convert_to_factors <- function(data) {
  for (col in names(data)) {
    if (is.character(data[[col]]) || is.factor(data[[col]])) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  
  return(data)
}

data = convert_to_factors(data)



#Converting each value to lowercase
data <- data.frame(lapply(data, tolower))

#Missing values
sapply(data, function(x) sum(x == "null"))


#A function that drop column that contain more than half the missing values
drop_null_mode_columns <- function(data) {
  mode_values <- sapply(data, function(col) {
    table_col <- table(col)
    mode_val <- names(sort(table_col, decreasing = TRUE))[1]
    mode_val
  })
  null_mode_columns <- mode_values == "null"
  data <- data[, !null_mode_columns, drop = FALSE]
  
  return(data)
}
data <- drop_null_mode_columns(data)

#Columns that contain 0
sapply(data, function(x) sum(x == "0"))


#Replacing 0 with null
replace_zero_with_null <- function(data) {
  for (col in names(data)) {
    if (col != "Recommendation" && col != "Rating") {
      data[[col]][data[[col]] == 0] <- "null"
    }
  }
  return(data)
}
data <- replace_zero_with_null(data)


dim(data)

#drops rows that contains at least 5 null values

drop_rows_with_null <- function(data, threshold = 5) {
  null_count <- rowSums(data == "null", na.rm = TRUE)
  rows_to_drop <- which(null_count >= threshold)
  data <- data[-rows_to_drop, ]
  return(data)
}
data <- drop_rows_with_null(data)


data$Price[data$Price == "medium"] <- "average"
data$Size[data$Size == "small"] <- "s"



#replace null with the most frequent value
replace_null_with_mode <- function(data) {
  exclude_columns <- c("Rating", "Recommendation", "Dress_CODE")
  for (col in names(data)) {
    if (!(col %in% exclude_columns)) {
      mode_value <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
      data[[col]][data[[col]] == "null"] <- mode_value
    }
  }
  
  return(data)
}

data <- replace_null_with_mode(data)



#Performing chi-square test
perform_chi_square_test <- function(data, target_column, categorical_columns) {
  p_values <- numeric()
  cols_to_drop <- character(0)
  
  for (col in categorical_columns) {
    contingency_table <- table(data[[col]], data[[target_column]])
    chi_square_result <- chisq.test(contingency_table, simulate.p.value = TRUE)
    
    p_value <- chi_square_result$p.value
    p_values <- c(p_values, p_value)
    
    
    if (p_value > 0.055) {
      cols_to_drop <- c(cols_to_drop, col)
    }
  }
  
  results <- data.frame(Categorical_Variable = categorical_columns, p_value = p_values)
  data <- data[, !(names(data) %in% cols_to_drop)]
  
  return(list(results = results, data = data))
}

categorical_columns <- c('Style','Rating','Size', 'Season', 'NeckLine', 'SleeveLength', 'waiseline', 'Material', 'Pattern.Type')

results <- perform_chi_square_test(data, "Recommendation", categorical_columns)
print(results$results)

new_data <- results$data



library(e1071)
set.seed(123)
train_indices <- sample(1:nrow(new_data), 0.7 * nrow(new_data))
train_data <- new_data[train_indices, ]
test_data <- new_data[-train_indices, ]

# Train the Naive Bayes model
naive_bayes_model <- naiveBayes(train_data[, -c(which(names(train_data) %in% c("Recommendation", "Dress_CODE")))], 
                                train_data$Recommendation)

# Make predictions on the test data
predicted_classes <- predict(naive_bayes_model, test_data[, -c(which(names(test_data) %in% c("Recommendation", "Dress_CODE")))])

# Evaluate the predictions
accuracy <- mean(predicted_classes == test_data$Recommendation)
print(paste("Accuracy:", accuracy))


#----------------------------------------TEST DATA----------------------------------------------------------------------------------------

data = read.csv( "C:\\Users\\DELL\\OneDrive\\Desktop\\Desktop\\College\\DATA SCIENCE\\PROJECTS\\Dressify challenge\\Data\\RDS_Assignment_Test.csv")


#Duplicates
data[duplicated(data$Dress_CODE)]


#datatype of each column
sapply(data, class)

#converting into factors
convert_to_factors <- function(data) {
  for (col in names(data)) {
    if (is.character(data[[col]]) || is.factor(data[[col]])) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  
  return(data)
}

data = convert_to_factors(data)



#Converting each value to lowercase
data <- data.frame(lapply(data, tolower))

#Missing values
sapply(data, function(x) sum(x == "null"))


#A function that drop column that contain more than half the missing values
drop_null_mode_columns <- function(data) {
  mode_values <- sapply(data, function(col) {
    table_col <- table(col)
    mode_val <- names(sort(table_col, decreasing = TRUE))[1]
    mode_val
  })
  null_mode_columns <- mode_values == "null"
  data <- data[, !null_mode_columns, drop = FALSE]
  
  return(data)
}
data <- drop_null_mode_columns(data)

#Columns that contain 0
sapply(data, function(x) sum(x == "0"))


#Replacing 0 with null
replace_zero_with_null <- function(data) {
  for (col in names(data)) {
    if (col != "Recommendation" && col != "Rating") {
      data[[col]][data[[col]] == 0] <- "null"
    }
  }
  return(data)
}
data <- replace_zero_with_null(data)


dim(data)

#drops rows that contains at least 5 null values

drop_rows_with_null <- function(data, threshold = 5) {
  null_count <- rowSums(data == "null", na.rm = TRUE)
  rows_to_drop <- which(null_count >= threshold)
  data <- data[-rows_to_drop, ]
  return(data)
}
data <- drop_rows_with_null(data)


data$Price[data$Price == "medium"] <- "average"
data$Size[data$Size == "small"] <- "s"



#replace null iwht the most frequent value
replace_null_with_mode <- function(data) {
  exclude_columns <- c("Rating", "Recommendation", "Dress_CODE")
  for (col in names(data)) {
    if (!(col %in% exclude_columns)) {
      mode_value <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
      data[[col]][data[[col]] == "null"] <- mode_value
    }
  }
  
  return(data)
}

data <- replace_null_with_mode(data)



#Performing chi square test
perform_chi_square_test <- function(data, target_column, categorical_columns) {
  p_values <- numeric()
  cols_to_drop <- character(0)
  
  for (col in categorical_columns) {
    contingency_table <- table(data[[col]], data[[target_column]])
    chi_square_result <- chisq.test(contingency_table, simulate.p.value = TRUE)
    
    p_value <- chi_square_result$p.value
    p_values <- c(p_values, p_value)
    
    
    if (p_value > 0.055) {
      cols_to_drop <- c(cols_to_drop, col)
    }
  }
  
  results <- data.frame(Categorical_Variable = categorical_columns, p_value = p_values)
  data <- data[, !(names(data) %in% cols_to_drop)]
  
  return(list(results = results, data = data))
}

categorical_columns <- c('Style','Rating','Size', 'Season', 'NeckLine', 'SleeveLength', 'waiseline', 'Material', 'Pattern.Type')

results <- perform_chi_square_test(data, "Recommendation", categorical_columns)
print(results$results)

test_data <- results$data

predicted_classes <- predict(naive_bayes_model, test_data[, -c(which(names(test_data) %in% c("Recommendation", "Dress_CODE")))])

accuracy <- mean(predicted_classes == test_data$Recommendation)
print(paste("Accuracy:", accuracy))
