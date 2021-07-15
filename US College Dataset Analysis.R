
#######
  "Project:- Supervised and Unsupervised"
  "Name:- Satyam Srivastava"
  "Institute:- NIT Jamshedpur"
  "Submission Date:- 14/05/2021"
########
    
    # Applying Logistic Regression
    
getwd()
  path= "F:/Data_Science_Internship/R_Language/Project/Project - Supervised and Unsupervised"
setwd(path)

library(dplyr)

data= read.csv("College_Data.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = c(" ","NA"))
View(data)
names(data)
summary(data)
str(data)

table(data$Private)
table(data$Outstate)

d2= mutate(data, priv= ifelse(data$Private== 'Yes', 1, 0))
View(d2)

d2$Private <- d2$priv
d2= d2[, -20]

table(d2$Private)

######

train <- d2[1:500,]
test <- d2[501:777,]
dim(train)
dim(test)


library(e1071)
library(caret)
library(ggplot2)

#Now checking for the significancy of variables

model_1 <- glm(Private ~ Apps + Accept + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate +Room.Board
               + Books + Personal +PhD + Terminal + S.F.Ratio+ perc.alumni +Expend +Grad.Rate, family = binomial(link = "logit"), data= train)
summary(model_1)

#the above model tells us that the most significant variables are Private and Outstate, thus using these two only for making the final model

model_final= glm(Private ~ Outstate + S.F.Ratio, family = binomial(link = "logit"), data= train)
summary(model_final)

modelpr1= model_final$fitted.values
head(modelpr1)
model_prediction<- as.factor(ifelse(modelpr1>0.5, 1, 0))
head(model_prediction)
actual_value= as.factor(train$Private)
confusionMatrix(actual_value, model_prediction)           # 88% Accuracy


testprd <- predict(model_final, newdata= subset(test, select = c(2, 10, 16)))
pred_test <- as.factor(ifelse(testprd>0.5, 1, 0))
actual_test <- as.factor(test$Private)
confusionMatrix(actual_test, pred_test)                    #80.14% Accuracy


######################## Logistic Regretion Ends & Cluster Analysis Starts ###############



######
  "Cluster Analysis"
######
  
library(ggplot2) #visualisation
library(dplyr) #Data Manipulation
library(magrittr) #A forward pipe operator in R
library(cluster) #for Clustering
library(fpc) #Flexible procedures for Clustering

P= subset(d2, select = c(3))        # Storing "Applications filed" from given data in P
table(P)


# Using Algorithm to predict the no. of clusters for no. of Applications submitted


opt_cls <- function(k){
  cluster = kmeans(P, k)
  return(cluster$tot.withinss)
}

dpp <- sapply(1:20, opt_cls)
head(dpp)
optimK <- data.frame(k = 1:20, dpp)
head(optimK,20)

### ploting the k value from 1 to 20 against within sum of square (wss)

ggplot(data = optimK) + aes(x=k, y= dpp) + geom_point() + geom_line()+
  scale_x_continuous(breaks = seq(1,20,by = 1))

opt_clus <- 3

modelf= kmeans(P, opt_clus)

clus= modelf$cluster
modelf$centers

mapping= data.frame(Applications= P, Cluster= clus)

write.csv(mapping, "mapping.csv")
View(mapping)
ggplot(data= mapping) + aes(x=clus, y=P$Apps) + geom_point() + xlab("Cluster") + ylab("No. of Applications")

plotcluster(P$Apps, clus)

##################### END #############################