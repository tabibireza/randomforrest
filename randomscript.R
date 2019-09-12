data <- read.csv("E:/towork/study/r-studio/projects/randomforrest/data2/CTG.csv", header = TRUE  )
str(data)

# response varible is NSP
# 1:normal  2:suspect   3:pathologic 
# convert NSP to Factor
data$NSP <- as.factor(data$NSP)
table(data$NSP)

# data partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE,prob = c(0.7,0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# random forest , aggrigating trees, classification or regression, not overfitting, 
# large number of features, Trees:500, mtry is sq.root & P/3 for regression
# Ntree bootstrap, each bootstrap sample grow tree, predict majority votes for classification, 
# average for regression 
install.packages("randomForest")
library(randomForest)


# modelling
rf <- randomForest(NSP ~ ., data = train)
print(rf)
attributes(rf)

# prediction and confusion matrix -train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$NSP)


# prediction with test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$NSP)

# error rate 
plot(rf)

# Tune mtry
t <- tuneRF(train[,-22], train[,22], 
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry=300,
            trace = TRUE,
            import=0.05)

# again run model and compare the result with pervious results
rf <- randomForest(NSP ~ ., data = train,
                   ntree=300,
                   mtry=8,
                   importance=TRUE,
                   Proximity=TRUE)
print(rf)
p1 <- predict(rf, train)
confusionMatrix(p1, train$NSP)
p2 <- predict(rf, test)
confusionMatrix(p2, test$NSP)

# No of nodes for the trees
hist(treesize(rf), 
     main= "No. Nodes for the Trees",
     col="green")

# varible importance 
varImpPlot(rf,
           sort=T,
           n.var = 10,
           main="Top 10")
importance(rf)
varUsed(rf) # lower number means No importancy


# Partial dependence plot 
partialPlot(rf,train, ASTV,"1")

# extract single tree 
# status - or negative means that prediction is node (end of system)
getTree(rf,1,labelVar = TRUE) # paitent no 9 is pathology
# for terminal node we dont have nober left daughter or Right daughter becuase it is the end

# mutidimensional scaling plot of proximity matrix 
MDSplot(rf, train$NSP)

