#name the file
w <-read.csv("smoking_dataset.csv")
str(w)
#rectification
w$gender<- as.factor(w$gender)
w$hearing.left.<- as.factor(w$hearing.left.)
w$hearing.right.<-as.factor(w$hearing.right.)
w$Urine.protein<-as.factor(w$Urine.protein)
w$oral<-as.factor(w$oral)
w$tartar<-as.factor(w$tartar)
w$dental.caries<-as.factor(w$dental.caries)
w$smoking<-as.factor(w$smoking)


str(w)
#test-train
set.seed(123)
sample_indices <- sample(nrow(x), 0.8 * nrow(x))  # 80% for training, 20% for testing

train_data <- x[sample_indices, ]

test_data <- x[-sample_indices, ]
#random forest
rfNews()
library(randomForest)
set.seed(123)
rf_model <- randomForest(smoking ~ tartar+age+weight.kg.+height.cm.+triglyceride+hemoglobin+ALT+Gtp+dental.caries+gender+tartar, data = train_data,
                         .data = train_data,ntree = 100)
summary(rf_model)
print(rf_model)
predictions <- predict(rf_model, newdata = test_data)
accuracy <- sum(predictions == test_data$smoking) / nrow(test_data)
print(paste("Accuracy:", accuracy))
