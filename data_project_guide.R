#data cleaning guidance

#1.remove the columns that are duplicated-------
data<- data[,!duplicated(as.list(data))]

#2.columns no variation#------
library(caret)
remove_cols = nearZeroVar(data)
data = data[,-remove_cols]

#3.recoding/dummy coding-------
#return data type
str(data)
#return variable names
ls(data)
#return level of a variable
levels(data$CSA)
#return missing values, unique values, data descrpition,etc.
library(mice)
describe(data)
#change certain value
data$CSA[data$CSA=="HOP"]<-"LAX"
#set dummy
data$superhost<-ifelse(data$host_is_superhost=="t", 1, 0)

#4. Split dataset into train & test-----------
#testing data points represent real-world data, split the dataset first to avoid introducing future information into the training explanatory variables
I = sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
data_train = data[I == 1,]
data_test = data[I == 2,]


#5.remove columns with more than 20% missingmemory.limit(size=80000)---------
data_train = data_train[,-which(colMeans(is.na(data_train)) > 0.2)]

#6. NA observations imputation--------
#return number of NAs
sum(is.na(data_train))
#return the location of NA
which(is.na(data_train))  
#Plot the NAs
library(VIM)
aggr(data,prop=F,numbers=T)
#(1 delete rows with NAs (less than 1%)
data_train=complete.case(data_train)
data_train=na.omit(data_train)
#(2. mean/mode/median imputation
#use the mean/mode/median get from train dataset to impute test dataset.
library(imputeMissings)
values <- compute(data_train)
data_test <- impute(data_test, object = values,method = "median")
#(3. use regression method to impute NAs
model <- lm(sales ~ date, data = inputfile1) # build a model
inputfile2$sales <- predict(model, inputfile2) # predict
result3 <- rbind(inputfile1, inputfile2)
#(4. 
library(lattice)
library(MASS)
library(nnet)
library(mice) 
imp <- mice(inputfile, m = 4) 
fit <- with(imp,lm(sales ~ date, data = inputfile)) 
pooled <- pool(fit)
summary(pooled)
result4 <- complete(imp, action = 3) 


#7.remove the highcorrelation----------
Correlation=cor(data_train,use='pairwise')
options(scipen=200)#remove scientific notation
library(dplyr)
Correlation[is.na(Correlation)] <- -1.00000
roundCov <- round(Correlation, 5)
highcorr=findCorrelation(roundCov,cutoff = 0.75) 
highcorr
data_train=data_train[ ,-highcorr]







