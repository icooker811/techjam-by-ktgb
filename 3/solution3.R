setwd("~/Documents/workspace/R")

library("e1071")

rm(list=ls(all=TRUE))

file0 <- read.csv("data/3/tj_03_deposit_txn.csv", header=TRUE)
file1 <- read.csv("data/3/tj_03_account_info.csv", header=TRUE)
file2 <- read.csv("data/3/tj_03_training.csv", header=TRUE)
file3 <- read.csv("data/3/tj_03_test.csv", header=TRUE)

data0 <- data.frame(file0)
data1 <- data.frame(file1)
data2 <- data.frame(file2)
data3 <- data.frame(file3)

Mode <- function(x) {
  temp <- table(as.vector(x))
  return(names(temp)[temp == max(temp)])[1]
}

prepareData <- function() { 
  df <- data.frame()
  
  for(i in 1:nrow(data2)) {
    row <- data2[i,]
    account_no <- row$account_no
    class <- row$class
    
    transaction <- data0[which(data0['account_no'] == account_no),]
    account <- data1[which(data1['account_no'] == account_no),]
    
    max_txn_amount <- max(sapply(transaction['txn_amount'], max, na.rm=TRUE))
    min_txn_amount <- max(sapply(transaction['txn_amount'], min, na.rm=TRUE))
    mean_txn_amount <- max(sapply(transaction['txn_amount'], mean, na.rm=TRUE))
    sum_txn_amount <- sum(sapply(transaction['txn_amount'], sum, na.rm=TRUE))
    mode_txn_amount <-as.numeric(Mode(transaction['txn_amount']))[1]
    
    txn_type_dr <- if (length(which((transaction['txn_type']) == 'DR')) > 0) 1 else 0
    txn_type_cr <- if (length(which((transaction['txn_type']) == 'CR')) > 0) 1 else 0
    
    customer_type_704 <- (length(which((account['customer_type']) == '704')))
    customer_type_5112 <- (length(which((account['customer_type']) == '5112')))
    
    df <- rbind(df, data.frame(
      #card_no=report$card_no,
      max_txn_amount=max_txn_amount,
      min_txn_amount=min_txn_amount,
      mean_txn_amount=mean_txn_amount,
      sum_txn_amount=sum_txn_amount,
      mode_txn_amount=mode_txn_amount,
      txn_type_dr=txn_type_dr,
      txn_type_cr=txn_type_cr,
      customer_type_704=customer_type_704,
      customer_type_5112=customer_type_5112,
      class=if(row$class == 1) 'Y' else 'N'
    ))
    
    #print(i)
  }
  
  return(df)
}

df <- prepareData()
write.csv(df, 'data/3/training01.csv')

prepareTestData <- function() { 
  df <- data.frame()
  
  for(i in 1:nrow(data3)) {
    
    row <- data3[i,]
    account_no <- row
    class <- 1
    
    transaction <- data0[which(data0['account_no'] == account_no),]
    account <- data1[which(data1['account_no'] == account_no),]
    
    max_txn_amount <- max(sapply(transaction['txn_amount'], max, na.rm=TRUE))
    min_txn_amount <- max(sapply(transaction['txn_amount'], min, na.rm=TRUE))
    mean_txn_amount <- max(sapply(transaction['txn_amount'], mean, na.rm=TRUE))
    sum_txn_amount <- sum(sapply(transaction['txn_amount'], sum, na.rm=TRUE))
    mode_txn_amount <-as.numeric(Mode(transaction['txn_amount']))[1]
    
    txn_type_dr <- if (length(which((transaction['txn_type']) == 'DR')) > 0) 1 else 0
    txn_type_cr <- if (length(which((transaction['txn_type']) == 'CR')) > 0) 1 else 0
    
    customer_type_704 <- (length(which((account['customer_type']) == '704')))
    customer_type_5112 <- (length(which((account['customer_type']) == '5112')))
    
    df <- rbind(df, data.frame(
      #card_no=report$card_no,
      max_txn_amount=max_txn_amount,
      min_txn_amount=min_txn_amount,
      mean_txn_amount=mean_txn_amount,
      sum_txn_amount=sum_txn_amount,
      mode_txn_amount=mode_txn_amount,
      txn_type_dr=txn_type_dr,
      txn_type_cr=txn_type_cr,
      customer_type_704=customer_type_704,
      customer_type_5112=customer_type_5112,
      class='Y'
    ))
    
  }
  
  return(df)
}

dtf <- prepareTestData()
write.csv(dtf, 'data/3/testing01.csv')

training <- read.csv("data/3/training01.csv")
testing <- read.csv("data/3/testing01.csv")

svm_model <- svm(class ~ ., data=training, 
                 kernel="radial", 
                 cost=1,
                 gamma=0.5)
result <- predict(svm_model, training) 
#print(table(result, training$class))
#print((table(result, training$class)[1] + table(result, training$class)[4])/nrow(data2))

result <- predict(svm_model, testing) 
#print(table(result, training$class))

answer = c(0)
for(i in 1:length(result)) {
  answer[i] = if (result[i] == 'N') 0 else 1
}

#write.csv(answer, 'data/3/3.csv')
write(answer,'data/3/3.txt', sep="\n")