#name file
w <-read.csv("smoking_dataset.csv")

# structure analysis
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

#structure analysis
str(w)

#Treatment of missing values
w<-na.omit(w)

#identification of outliers
library(ggplot2)
ggplot(w,aes(x=age))+geom_boxplot()
ggplot(w,aes(x=height.cm.))+geom_boxplot()

#Treatment of outlier
IQR_age<-IQR(w$age)
IQR_age
lower_bound<-quantile(w$age ,0.25)-1.5*IQR_age  
upper_bound<-quantile(w$age ,0.25)+1.5*IQR_age  
x<-w[w$age>=lower_bound&w$age <=upper_bound,]
boxplot(x$age)


#structure analysis of data without outliers
str(x)

#summary
summary(x)

#statistics
sd(x$age)
var(x$age)

#Histogram for numeric variables
ggplot(x,aes(x=age))+geom_histogram()
ggplot(x,aes(x=height.cm.))+geom_histogram()

#barplot for categoric variable 
ggplot(x, aes(x=gender))+geom_bar()

#barplot between categoric variables
mno <- table(x$smoking, x$gender)
mno
barplot(mno, legend.text = TRUE)

#boxplot for relationship between numeric and categoric variables
ggplot(x, aes(y = weight.kg.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = weight.kg.)) + geom_boxplot(fill = 'skyblue', color = 
                                                                     'black', alpha = 0.7) + theme_minimal()

#Regression model
model <-glm(smoking~height.cm.+hemoglobin+triglyceride+Gtp+ALT+gender+tartar+dental.caries+weight.kg.,data=x,family=binomial())
summary(model)

# Make predictions on the data
predictions <- predict(model, newdata = x, type = "response")

# Convert predicted probabilities to binary outcomes
predicted_classes <- ifelse(predictions > 0.5, 1, 0) 

# Create a confusion matrix (Error Analysis)
conf_matrix <- table(predicted_classes,x$smoking)

#Accuracy (Number of accurate guesses)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)



