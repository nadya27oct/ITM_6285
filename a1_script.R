#loading files
library(readr)
bank <- read_delim("~/Documents/homework/ITM_6285/bank-additional.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
class(bank)
sapply(bank,class)
names(bank)
str(bank)
summary(bank)
bank <- subset(bank, select = -c(duration))
sum(is.na(bank))
str(bank)
table(bank$y)
summary(bank$y)
plot(bank$Class, cex.names = 0.7)
library(ggplot2)
qplot(y, data=bank, geom = "bar") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
boxplot(bank$age~bank$y, main=" Age",ylab="age of customers",xlab="Subscribed")

barplot(table(bank$job),col="blue",main="Job")
barplot(table(bank$marital),col="red",main="JOB")

library(lattice)
set.seed(123456)
library(caret)
library(dplyr)
library(lattice)
train <- sample_frac(bank, 0.80, replace = TRUE)
library(plyr)
library(mlbench)
library(foreign)
library(ggplot2)
library(scales)
library(reshape)
library(e1071)
library(klaR)
library(MASS)

prop.table(table(train$y))
prop.table(table(test$y))
DistributionCompare <- cbind(prop.table(table(train$y)), prop.table(table(bank$y)))
colnames(DistributionCompare) <- c("Training", "Orig")

meltedDComp <- melt(DistributionCompare)
meltedDComp

result<-C5.0
# Plot to see distribution of training vs original - is it representative or is there over/under sampling?
ggplot(meltedDComp, aes(x= X1, y = value)) + geom_bar( aes(fill = X2), stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
rm(meltedDComp)
rm(DistributionCompare)

TrainingDataIndex <- createDataPartition(bank$y, p=0.75, list = FALSE)
train <- bank[TrainingDataIndex,]
test <-bank[-TrainingDataIndex,]
TrainingParameters <- trainControl(method = "cv", number = 10, repeats = 10)

DecTreeModel <- train(y ~ ., data = train, 
                      method = "C5.0",
                      trControl= TrainingParameters,
                      na.action = na.omit
)
#install package C50
library(C50)
library(plyr)
DecTreeModel
plot(DecTreeModel)

#parameter optimization
#winnow - reduce attributes or not
#over sample the misclassfied data and create a new model - boosting 
#kappa - how good is the model based on expected accuracy
# Plot performance
plot.train(DecTreeModel)
ggplot(DecTreeModel)

# Now make predictions on test set
DTPredictions <-predict(DecTreeModel, test, na.action = na.pass)
DTPredictions
#predictions for each test data row
# Print confusion matrix and results
cm <-confusionMatrix(DTPredictions, test$Class)
cm$overall
cm$byy
summary(DecTreeModel)
nrow(train)
nrow(test)
