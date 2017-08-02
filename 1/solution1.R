# Please set your own directory
setwd("~/Documents/workspace/R")

library("e1071")

rm(list=ls(all=TRUE))

file1 <- read.csv("data/1/tj_01_creditcard_transaction.csv", header=TRUE)
file1_1 <- read.csv("data/1/tj_01_creditcard_card.csv", header=TRUE)
file1_2 <- read.csv("data/1/tj_01_creditcard_customer.csv", header=TRUE)
file2 <- read.csv("data/1/tj_01_training.csv", header=TRUE)
file3 <- read.csv("data/1/tj_01_test.csv", header=TRUE)

data0 <- data.frame(file1)
data1 <- merge(file1_1, file1_2)
data2 <- data.frame(file2)
data3 <- data.frame(file3)

prepareData <- function() { 
  df <- data.frame()
  
  for(i in 1:nrow(data2)) {
    row <- data2[i,]
    card_no <- row$card_no
    class <- row$class
    
    report <- data1[which(data1['card_no'] == card_no),]
    
    transaction <- data0[which(data0['card_no'] == card_no),]
    sum_txn_amount <- sum(sapply(transaction['txn_amount'], sum, na.rm=TRUE))
    mean_txn_amount <- mean(sapply(transaction['txn_amount'], mean, na.rm=TRUE))
    
    df <- rbind(df, data.frame(
        #card_no=report$card_no,
        bill_cyc=report$bill_cyc,
        sum_txn_amount=sum_txn_amount,
        mean_txn_amount=mean_txn_amount,
        cr_lmt_amt=report$cr_lmt_amt,
        prev_cr_lmt_amt=report$prev_cr_lmt_amt,
        incm_amt=report$incm_amt,
        age=as.numeric(report$age),
        main_zip_cd=report$main_zip_cd,
        cr_line_amt=report$cr_line_amt,
        class=if(row$class == 1) 'Y' else 'N'
    ))
    #print(i)
  }
  
  return(df)
}

df <- prepareData()
write.csv(df, 'data/1/training01.csv')

prepareTestData <- function() { 
  df <- data.frame()
  
  for(i in 1:nrow(data3)) {
    
    row <- data3[i,]
    card_no <- row
    class <- 1
    
    report <- data1[which(data1['card_no'] == card_no),]
    
    transaction <- data0[which(data0['card_no'] == card_no),]
    sum_txn_amount <- sum(sapply(transaction['txn_amount'], sum, na.rm=TRUE))
    mean_txn_amount <- mean(sapply(transaction['txn_amount'], mean, na.rm=TRUE))
    
    df <- rbind(df, data.frame(
      #card_no=report$card_no,
      bill_cyc=report$bill_cyc,
      sum_txn_amount=sum_txn_amount,
      mean_txn_amount=mean_txn_amount,
      cr_lmt_amt=report$cr_lmt_amt,
      prev_cr_lmt_amt=report$prev_cr_lmt_amt,
      incm_amt=report$incm_amt,
      age=as.numeric(report$age),
      main_zip_cd=report$main_zip_cd,
      cr_line_amt=report$cr_line_amt,
      class='Y'
    ))
  }
  
  return(df)
}

dtf <- prepareTestData()
write.csv(dtf, 'data/1/testing01.csv')

training <- read.csv("data/1/training01.csv")
testing <- read.csv("data/1/testing01.csv")

svm_model <- svm(class ~ ., data=training, 
                 kernel="radial", 
                 cost=1,
                 gamma=c(0.5,10,20))
result <- predict(svm_model, training) 
#print(table(result, training$class))
#print((table(result, training$class)[1] + table(result, training$class)[4])/nrow(data2))

result <- predict(svm_model, testing) 
#print(table(result, training$class))

answer = c(0)
for(i in 1:length(result)) {
  answer[i] = if (result[i] == 'N') 0 else 1
}

#write.csv(answer, 'data/1/1.csv')
write(answer,'data/1/1.txt', sep="\n")