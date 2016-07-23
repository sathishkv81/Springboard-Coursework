#Load the libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(caTools)
library(reshape2)
library(ROCR)

#This project will work on three datasets
#LoanStats3a_2007_2011.csv - Training Dataset1
#LoanStats3b_2012_2013.csv - Training Dataset2
#LoanStats3c_2014.csv - Testing Dataset

#Import both the directories into the workspace

#Import the LoanStats3a_2007_2011.csv into df1
df1 <- read.csv(file.choose(), header = TRUE)

#Import LoanStats3b_2012_2013.csv into df2
df2 <- read.csv(file.choose(), header = TRUE)

#Combine both the datasets
df <- rbind(df1, df2)

#Import the LoanStats3c_2014.csv into df3. This will be the test dataset
df3 <- read.csv(file.choose(), header = TRUE)

#Clean up the int_rate column and convert it to numeric value
df$int_rate <- as.numeric(gsub("%","",df$int_rate))

#The addr_state column has some missing values. Set it to CA
df$addr_state[df$addr_state == ""] <- "CA"

#The annual_inc column is stored as character. Convert it to numeric
df$annual_inc <- as.numeric(df$annual_inc)

#Define the criterion for bad loans
is_bad_loan <- c("Charged Off", "Late (31-120 days)", "Late (16-30 days)", "Default")
df$bad_loan <- ifelse(df$loan_status %in% is_bad_loan, 1, 0)

#Set all the NA values to the mean of the corresponding columns
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

#Clean up the test dataset
df3$int_rate <- as.numeric(gsub("%","",df3$int_rate))
df3$addr_state[df3$addr_state == ""] <- "CA"
df3$annual_inc <- as.numeric(df3$annual_inc)
df3$bad_loan <- ifelse(df3$loan_status %in% is_bad_loan, 1, 0)
df3$grade <- gsub("12489326","C",df3$grade)

#Plot the density plots and check for relevant variables
numeric_cols <- sapply(df, is.numeric)
dfden <- melt(df[,numeric_cols], id="bad_loan")
p <- ggplot(aes(x=value, group=bad_loan, colour=factor(bad_loan)), data=dfden)
p + geom_density() + facet_wrap(~variable, scales="free")

#Specify the Test dataset
dfTest <- subset(df3, select = c("member_id","loan_amnt","annual_inc","grade","int_rate","bad_loan","dti","addr_state"))
#Set all the NA values to the mean of the corresponding columns
for(i in 1:ncol(dfTest)){
  dfTest[is.na(dfTest[,i]), i] <- mean(dfTest[,i], na.rm = TRUE)
}

#Build Logistic Regression model on df
logmodel1 <- glm(bad_loan ~ loan_amnt + annual_inc + grade + int_rate + dti + addr_state, data = dfmodel1, family = "binomial")
summary(logmodel1)

logmodel <- glm(bad_loan ~ loan_amnt + annual_inc + grade + int_rate , data = df, family = "binomial")
summary(logmodel)

#Based on the above data, let us make our predictions on logmodel
predictlog <- predict(logmodel, newdata = dfTest, type = "response")

#The confidence matrix
table(round(predictlog), dfTest$bad_loan)


##      0     1
## 0 59099  8360
## 1    31    12
## From the confidence matrix, we see that sensitivity is 12/43 = 0.28
## From the confidence matrix, we see that specificity is 59099/ = 0.87

##Build randomForest on df
##Since the data has 230k observations, the randomForest model would throw up a memory error.
##The memory error was fixed by setting the nodesize to 6

X <- as.matrix(df$int_rate)
rfmodel <- randomForest(X,df$bad_loan, nodesize = 6)

## Run predictions on the rfmodel
Y <- as.matrix(dfTest$int_rate)
predictrf <- predict(rfmodel, Y, type = "response")
table(predictrf <= 0.2, dfTest$bad_loan)

##      0     1
##FALSE 12424  2980
##TRUE  46706  5392
##
## Sensitivity = 0.103, Specificity = 0.806

table(predictrf <= 0.1, dfTest$bad_loan)

##      0     1
##FALSE 40034  7132
##TRUE  19096  1240
##
## Sensitivity = 0.103, Specificity = 0.806


table(predictrf <= 0.3, dfTest$bad_loan)

##         0     1
##FALSE  1422   411
##TRUE  57708  7961
##
## Sensitivity = 0.121, Specificity = 0.776


## Lastly, let's build the ROC curve for each of these models to compute the area under curve

predlog <- predict(logmodel, df, type = "response")
ROCRlog <- prediction(predlog, df$bad_loan)
ROCRperflog <- performance(ROCRlog, "tpr","fpr")

Z <- as.matrix(df$int_rate)
predrf <- predict(rfmodel, Z, type = "response")
ROCRrf <- prediction(predrf, df$bad_loan)
ROCRperfrf <- performance(ROCRrf,"tpr","fpr")

plot(ROCRperflog, col = 'red')
plot(ROCRperfrf, add = TRUE, col = 'blue')

a <- rbinom(length(predlog), 1, 0.25)
roc1 <- roc(a ~ predlog)
roc2 <- roc(a ~ predrf)

roc1$auc
roc2$auc
## We observe that the ROC for each of these models is 0.50
