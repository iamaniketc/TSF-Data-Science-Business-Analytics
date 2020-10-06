data <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
View(data)
summary(data)

#Splitting the dataset into traing and testing set
library(caTools)
set.seed(123)
split<- sample.split(data$Scores, SplitRatio = 0.7)
training_set<- subset(data, split==TRUE)
View(training_set)
test_set<- subset(data, split==FALSE)
View(test_set)

#Fitting Simple Linear Regression on the training set
reg<- lm(formula = Scores~Hours, data = training_set)
summary(reg)

#Predicting SCORES for test set
p<- predict(reg, newdata = test_set)
View(p)

#PREDICT SCORE IF A STUDENT STUDIES FOR 9.25hrs A DAY
p_score<- predict(reg, data.frame(Hours=9.25))
p_score

#PLOTS

library(ggplot2)

#plotting the original data provided
ggplot()+
   geom_point(aes(x=data$Hours, y=data$Scores),
              colour="pink", size=2.5)+
   geom_line(aes(x=data$Hours, y=predict(reg, newdata = data)),
             colour="black", size=1)+
   xlab("HOURS STUDIED")+
   ylab("SCORE")+
   ggtitle("HOURS STUDIED vs SCORES")

#plotting training set
ggplot()+
   geom_point(aes(x=training_set$Hours, y=training_set$Scores),
              colour="blue", size=2.5)+
   geom_line(aes(x=training_set$Hours, y=predict(reg, newdata = training_set)),
             color="black", size=1)+
   xlab("HOURS")+
   ylab("SCORE")+
   ggtitle("HOURS STUDIED PER DAY vs SCORE")

#plotting test set
ggplot()+
   geom_point(aes(x=test_set$Hours, y=test_set$Scores),
              colour="red", size=2.5)+
   geom_line(aes(x=test_set$Hours, y=predict(reg, newdata = test_set)),
             color="black", size=1)+
   xlab("HOURS")+
   ylab("SCORE")+
   ggtitle("HOURS STUDIED PER DAY vs SCORE")