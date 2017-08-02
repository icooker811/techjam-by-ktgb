setwd("~/Documents/workspace/R")

rm(list=ls(all=TRUE))

library("e1071")

file1 <- read.csv("data/5/tj_05_credit_card_transaction.csv")
file2 <- read.csv("data/5/tj_05_training.csv")
file3 <- read.csv("data/5/tj_05_test.csv")

data1 <- data.frame(file1)
data2 <- data.frame(file2)
data3 <- data.frame(file3)

mer_cat_codes <- names(table(as.vector(unique(data1$mer_cat_code))))[-1]
max_mer_cat_codes <- length(mer_cat_codes)
Category <- function(x) {
  row <- c()
  for(i in 1:length(mer_cat_codes)) {
    count = length(mer_cat_codes[which(grepl(mer_cat_codes[i], x))])
    row[i] = if (count > 0) 1 else 0
  } 
  return(row)
}


prepareData <- function() { 
  df <- data.frame()
  
  for(i in 1:nrow(data2)) {
    row <- data2[i,]
    card_no <- row$card_no
    class <- row$class
    
    transaction <- data1[which(data1['card_no'] == card_no),]
    transaction <- transform(transaction, txn_hour = as.numeric(txn_hour))
    
    checked_null <- sum(sapply(transaction['mer_id'], sum, na.rm=TRUE)) == 28728
    if (!checked_null) {

      class_names <- mer_cat_codes
      class_names[max_mer_cat_codes+1] <- 'X0'
      class_names[max_mer_cat_codes+2] <- 'class'
      
      category_data <- Category(transaction$mer_cat_code)
      category_data[max_mer_cat_codes+1] <- 0
      category_data[max_mer_cat_codes+2] <- if (class==1) 'M' else 'F'
      names(category_data) <- class_names
      
      if (length(df) == 0) {
        df <- category_data
      } else {
        df <- rbind(df, category_data) 
      }
      
    } else {
      
      class_names <- mer_cat_codes
      class_names[max_mer_cat_codes+1] <- 'X0'
      class_names[max_mer_cat_codes+2] <- 'class'
      
      category_data <- integer(max_mer_cat_codes)
      category_data[max_mer_cat_codes+1] <- 1
      category_data[max_mer_cat_codes+2] <- if (class==1) 'M' else 'F'
      names(category_data) <- class_names
      
      if (length(df) == 0) {
        df <- category_data
      } else {
        df <- rbind(df, category_data) 
      }
    }
    print(i);
    #if (i >= 1000) break
  }
  return(df)
}

df <- prepareData()
write.csv(df, 'data/5/training01.csv')


prepareTestData <- function() { 
  df <- data.frame()
  
  for(i in 1:nrow(data3)) {
    row <- data3[i,]
    card_no <- row
    #class <- row$class
    
    transaction <- data1[which(data1['card_no'] == card_no),]
    transaction <- transform(transaction, txn_hour = as.numeric(txn_hour))
    
    checked_null <- sum(sapply(transaction['mer_id'], sum, na.rm=TRUE)) == 28728
    if (!checked_null) {
      class_names <- mer_cat_codes
      class_names[max_mer_cat_codes+1] <- 'X0'
      class_names[max_mer_cat_codes+2] <- 'class'
      
      category_data <- Category(transaction$mer_cat_code)
      category_data[max_mer_cat_codes+1] <- 0
      category_data[max_mer_cat_codes+2] <- 'M' 
      names(category_data) <- class_names
      
      if (length(df) == 0) {
        df <- category_data
      } else {
        df <- rbind(df, category_data) 
      }
      
    } else {
      class_names <- mer_cat_codes
      class_names[max_mer_cat_codes+1] <- 'X0'
      class_names[max_mer_cat_codes+2] <- 'class'
      
      category_data <- integer(max_mer_cat_codes)
      category_data[max_mer_cat_codes+1] <- 1
      category_data[max_mer_cat_codes+2] <- 'M'
      names(category_data) <- class_names
      
      if (length(df) == 0) {
        df <- category_data
      } else {
        df <- rbind(df, category_data) 
      }
    }
  }
  
  return(df)
}

dtf <- prepareTestData()
write.csv(dtf, 'data/5/testing01.csv')

training <- read.csv("data/5/training01.csv")
testing <- read.csv("data/5/testing01.csv")

training = training[which(training['X0'] == 0),]
svm_model <- svm(class ~ ., data=training, kernel="radial", cost=1, gamma=1)
result <- predict(svm_model, testing)

answer = c(0)
for(i in 1:length(result)) {
  answer[i] = if (result[i] == 'F') 0 else 1
}

#write.csv(answer, 'data/5/5.csv')
write(answer,'data/5/5.txt', sep="\n")



