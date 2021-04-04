if(!require("ggplot2"))
{
  install.packages("ggplot2",repos = "http://cran.us.r-project.org")
  
}
if(!require("caTools"))
{
  install.packages("caTools",repos = "http://cran.us.r-project.org")
}
if(!require("tidyverse"))
{
  install.packages("tidyverse",repos = "http://cran.us.r-project.org")
}
if(!require("Hmisc"))
{
  install.packages("Hmisc",repos = "http://cran.us.r-project.org")
}
if(!require("gensvm"))
{
  install.packages("gensvm",repos = "http://cran.us.r-project.org")
}
if(!require("randomForest"))
{
  install.packages("randomForest",repos = "http://cran.us.r-project.org")
}
if(!require("glmnet"))
{
  install.packages("glmnet",repos = "http://cran.us.r-project.org")
}
if(!require("caret"))
{
  install.packages("caret",repos = "http://cran.us.r-project.org")
}

if(!require("pROC"))
{
  install.packages("pROC",repos = "http://cran.us.r-project.org")
}
if(!require("corrplot"))
{
  install.packages("corrplot",repos = "http://cran.us.r-project.org")
}
if(!require("ROCR"))
{
  install.packages("ROCR",repos = "http://cran.us.r-project.org")
}

if(!require("gbm"))
{
  install.packages("gbm",repos = "http://cran.us.r-project.org")
}
if(!require("readxl"))
{
  install.packages("readxl",repos = "http://cran.us.r-project.org")
}
#load the libraries
options(warn=-1)
library(ggplot2)
library(caTools)
library(tidyverse)
library(Hmisc)
library(gensvm)
library(randomForest)
library(glmnet)
library(caret)
library(pROC)
library(corrplot)
library(ROCR)
library(gbm)
#download the dataset
#download.file("https://drive.google.com/u/0/uc?id=1mnjmZmXp_ej1G4k7rj-cKA7tGKqiB5cc&export=download","dataset.xlsx",quiet=TRUE)
df <- readxl::read_excel('dataset.xlsx')
df = df %>% distinct()
df$ClaimStatus = factor(df$ClaimStatus, levels = c(0, 1)) #convert the target variable to the encoded values

total_cells <- prod(dim(df)) #check total number of cells
missing_vals <- sum(is.na(df)) #check the misisng values
percent_of_missing_data <- (missing_vals/total_cells)*100 #check percenatge of missing vals
colSums(is.na(df))
describe(df) #checking basic stats of the data
summary(df) #checking basic facts

#plot the scatterplot to see relationship with Premium and AGEofdriving in US
ggplot(df) +
  aes(x = Premium, y = AgeUSdriving_1) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()+ggtitle("AGE vs Premium ")
#plotting the boxplots to see the outliers and the distributions
boxplot(df$Total_Distance_To_Work)

boxplot(df$Premium,bins=20)
boxplot(df$AgeUSdriving_1,bins=20)
#we plot the barplot of the ClaimStatus count and see the types, this would be a pie plot by keeping the coord_polar() arguments
ggplot(df, aes(x="", y=ClaimStatus, fill=factor(Type))) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
#check the histogram the dustribution of the age
ggplot(df,aes(AgeUSdriving_1)) + geom_histogram(fill='blue',bins=30,alpha=0.5)

#next we plot the geom_bar to see the Type and their count
ggplot(data = df) +geom_bar(mapping = aes(x = factor(Type)),fill="blue")+xlab("Factors")+ggtitle("Number of factors")
#we see the barplots to see the count of CancellationType by the CoverageLiability of a customer
ggplot(data = df) +geom_bar(mapping = aes(x = factor(CancellationType),fill=factor(CoverageLiability)))+xlab("Cancellation type")+labs(fill="Coverage Liability",title="Coverage Liabilty vs Cancellation Type")

ggplot(df,aes(Premium)) + geom_histogram(fill='blue',bins=20,alpha=0.5)# Premium is not noramlized, we have to do it later
ggplot(df,aes(x=ClaimFrequency,y=ClaimStatus))+geom_point()

ggplot(data = df) +geom_bar(mapping = aes(x = factor(Type)),fill="red")

getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}


# Calculate the mode using the user function and then save the values inplace of missing values
result.most.age <- getmode(df$AgeUSdriving_1) #Most common age in 28
result.most.gender <- getmode(df$Sex_1) #Most of the drivers are male

# Filling in the missing values
df$Model_1[is.na(df$Model_1)] <-getmode(df$Model_1) 
# df$Make_1[is.na(df$Make_1)] <-getmode(df$Make_1) 

# df$CoverageLiability[is.na(df$CoverageLiability)] <-getmode(df$CoverageLiability) 

df$CoverageMP[is.na(df$CoverageMP)] <-getmode(df$CoverageMP) 
df$CoveragePD_1[is.na(df$CoveragePD_1)] <-getmode(df$CoveragePD_1) 
df$CoveragePIP_CDW[is.na(df$CoveragePIP_CDW)] <-getmode(df$CoveragePIP_CDW)
df$CoverageUMBI[is.na(df$CoverageUMBI)] <-getmode(df$CoverageUMBI)
df$CoverageUMPD[is.na(df$CoverageUMPD)] <-getmode(df$CoverageUMPD) 


#only 

p <- ggplot(df, aes(ClaimStatus, ClaimFrequency))
p + geom_point(aes(colour=factor(CoverageMP)))


#convert the values to factors so that we can have classes
#the logic is to first get the unqiue values and convert to list
#Next is to take the one elemet less than the length because the encoded values
#are starting from zero 
#in the factor argument the labels represnts the labels required

list.of.make.1 <- as.list(unique(df$Type))
length.of.make1 <- (length(list.of.make.1)-1)
df$Type<- factor(df$Type,levels = list.of.make.1,labels =c(0:8))

# 
list.of.make.1 <- as.list(unique(df$CoverageLiability))
length.of.make1 <- (length(list.of.make.1)-1)
df$CoverageLiability <- factor(df$CoverageLiability,levels = list.of.make.1,labels =c(0:3))

list.of.make.1 <- as.list(unique(df$Model_1))
length.of.make1 <- (length(list.of.make.1)-1)
df$Model_1 <- factor(df$Model_1,levels = list.of.make.1,labels =c(0:1447)) #ONEHOT
# 
list.of.make.1 <- as.list(unique(df$CoverageMP))
length.of.make1 <- (length(list.of.make.1)-1)
df$CoverageMP <- factor(df$CoverageMP,levels = list.of.make.1,labels = c(1:2))

# 

# 
list.of.make.1 <- as.list(unique(df$CoveragePD_1))
length.of.make1 <- (length(list.of.make.1)-1)
df$CoveragePD_1 <- factor(df$CoveragePD_1 ,levels = list.of.make.1,labels = c(1:3))
# 
list.of.make.1 <- as.list(unique(df$CoveragePIP_CDW))
length.of.make1 <- (length(list.of.make.1)-1)
df$CoveragePIP_CDW <- factor(df$CoveragePIP_CDW ,levels = list.of.make.1,labels = c(1:3))
# 

# 

# 

#here we replace the continous values by the MinMaxScaler so that every value is on the same scale

df$Premium <- (df$Premium-min(df$Premium))/(max(df$Premium) - min(df$Premium))
df$ClaimFrequency <- (df$ClaimFrequency-min(df$ClaimFrequency))/(max(df$ClaimFrequency) - min(df$ClaimFrequency))
df$VehicleInspected_1 <- (df$VehicleInspected_1-min(df$VehicleInspected_1))/(max(df$VehicleInspected_1) - min(df$VehicleInspected_1))
df$Units <- (df$Units-min(df$Units))/(max(df$Units) - min(df$Units))
df$Billing_Term <- (df$Billing_Term-min(df$Billing_Term))/(max(df$Billing_Term) - min(df$Billing_Term))
df$Renewed <- (df$Renewed-min(df$Renewed))/(max(df$Renewed) - min(df$Renewed))
df$Amendment <- (df$Amendment-min(df$Amendment))/(max(df$Amendment) - min(df$Amendment))

df$VehicleInspected_1 <-(df$VehicleInspected_1-min(df$VehicleInspected_1))/(max(df$VehicleInspected_1) - min(df$VehicleInspected_1))





#drop irrelevant features
df <- subset(df,select = -c(1,3,7:11,12,13:17,20,23,24,25:52,53,54:62,63,64:68,69,70:73,74,75,76,77,80:119,120:123,124,125,127))
#see the correlation plot of only of the numerical vals
only_num <- sapply(df, is.numeric)
corrplot(cor(df[,only_num]),method = 'circle')


M <- as.data.frame(cor(df[,only_num]))
table(df$ClaimStatus)
sum(df$ClaimStatus ==1 )/nrow(df)

#split data into train and test
#TRAIN TEST SPLIT
a=gensvm.train.test.split(x=df, train.size = 0.95,
                          shuffle = T,
                          return.idx = FALSE,random.state = 101)




#we want to predict the ClaimStatus so we write the formula s given below and then predict on test set
log.model <- glm(formula=ClaimStatus ~ . ,data = a$x.train,family=binomial)
summary(log.model)
prob_pred <- predict(log.model, type = 'response', newdata = a$x.test)

#convert probabilties to classes
fitted.results <- ifelse(prob_pred > 0.5,1,0)
#see the confusion matrix to get the performance of the model
confusionMatrix(as.factor(fitted.results),as.factor(a$x.test$ClaimStatus))

#see the ROC values and plot the ROC curve

roc(a$x.train$ClaimStatus, log.model$fitted.values, plot=TRUE,legacy.axes=TRUE,print.thres=T,print.auc=T)
par(pty = "s")

#this code is used to plot the values the TP,FP,TN,FN values are given as matrirx and the other arguments are for setting the graph look and feel
ctable <- as.table(matrix(c(355, 174, 8, 7), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Logistic Regression Confusion Matrix")

forest.model <- randomForest(x = a$x.train[-1],
                             y = a$x.train$ClaimStatus,
                             ntree = 100)
#predict on test set
y_pred.forest = predict(forest.model, newdata = a$x.test)
y_pred.forest.prob = predict(forest.model, newdata = a$x.test,type='prob')

#plot the roc curve
roc(a$x.train$ClaimStatus,forest.model$votes[,2] , plot=TRUE,legacy.axes=TRUE,print.thres=T,print.auc=T)


par(pty = "s")

confusionMatrix(as.factor(a$x.test$ClaimStatus),as.factor(y_pred.forest))

confusionMatrix(as.factor(y_pred.forest),as.factor(a$x.test$ClaimStatus))

ctable <- as.table(matrix(c(3350, 169, 13, 13), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Random Forest Confusion Matrix")
