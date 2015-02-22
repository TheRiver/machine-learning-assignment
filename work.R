library(dplyr)

clean.column <- function(col) {
    # Unknown values are often represented as empty strings. 
    # There are also values of #DIV/0!, which suggests bad treatment
    # of values in an excel database before exporting the data to CSV. 
    # We mark all of these as NA, and then coerce all the remaining values
    # to numeric.
    col[col == "" | col == "#DIV/0!"] <- NA
    as.numeric(col)
}

read.data <- function() {
    # We want to remove the initial X variable, which is just a line number.
    data <- read.csv('data/pml-training.csv') %>% select(-X)

    # The first six variables are integers and factor variables
    front.matter <- data %>% select(1:6)

    # The last column is our response variable
    response <- data %>% select(classe)

    # All the intervening variables are dirty numeric variables.
    # Here we detect missing and bad values.
    numeric.columns <- 7:(ncol(data) - 1)
    cleaned <- lapply(numeric.columns, function(col.i) clean.column(data[, col.i]))
    names(cleaned) <- colnames(data[, numeric.columns])
    cleaned <- as.data.frame(cleaned)
    cbind(front.matter, cleaned, response)
}

read.test.data <- function() {
    # We want to remove the initial X variable, which is just a line number.
    data <- read.csv('data/pml-testing.csv') %>% select(-X)
    
    # The first six variables are integers and factor variables
    front.matter <- data %>% select(1:6)
    
    # All the intervening variables are dirty numeric variables.
    # Here we detect missing and bad values.
    numeric.columns <- 7:(ncol(data) - 1)
    cleaned <- lapply(numeric.columns, function(col.i) clean.column(data[, col.i]))
    names(cleaned) <- colnames(data[, numeric.columns])
    cleaned <- as.data.frame(cleaned)
    cbind(front.matter, cleaned)
}

numeric.columns <- 7:158
