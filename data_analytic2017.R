#Young_people_survey
#1010 responses from students of age ranging 16-22
#150 factors
#most having values 1-5 and some contextual

#Libraries used


library(corrplot)
library(NbClust)
library(cluster)
library(ggplot2)
library(caret)
library(ROCR)
library(e1071)
library(class)
library(dplyr)
library(lubridate)

responses <- read.csv("C:/Users/ThePilot/Documents/PES/CSE_V/responses.csv")
responses_final <- responses


#preprocessing

#Numeric
responses$Gender_Numeric = 1
responses$Gender_Numeric[responses$Gender == 'female'] = 0
responses_final$Gender <- responses$Gender_Numeric

responses$Smoking_Numeric = 0
responses$Smoking_Numeric[responses$Smoking == "former smoker"] = 1
responses$Smoking_Numeric[responses$Smoking == "current smoker"] = 2
responses_final$Smoking <- responses$Smoking_Numeric

responses$Drinking_Numeric = 0
responses$Drinking_Numeric[responses$Alcohol == "social drinker"] = 1
responses$Drinking_Numeric[responses$Alcohol == "drink a lot"] = 2
responses_final$Alcohol <- responses$Drinking_Numeric

responses$Handed_Numeric = 0
responses$Handed_Numeric[responses$Left...right.handed == "left handed"] = 1
responses_final$Left...right.handed <- responses$Handed_Numeric

responses$Locality_Numeric = 0
responses$Locality_Numeric[responses$Village...town == "city"] = 1
responses_final$Village...town <- responses$Locality_Numeric

responses$Residence_Numeric = 0
responses$Residence_Numeric[responses$House...block.of.flats == "house/bungalow"] = 1
responses_final$House...block.of.flats <- responses$Residence_Numeric

responses$Only_child_Numeric = 0
responses$Only_child_Numeric[responses$Only.child == "yes"] = 1
responses_final$Only.child <- responses$Only_child_Numeric

responses$Education_Numeric = 0
responses$Education_Numeric[responses$Education == "secondary school"] = 1
responses$Education_Numeric[responses$Education == "college/bachelor degree"] = 2
responses$Education_Numeric[responses$Education == "masters degree"] = 3
responses$Education_Numeric[responses$Education == "doctorate degree"] = 4
responses_final$Education <- responses$Education_Numeric

responses$Punctuality_Numeric = 0
responses$Punctuality_Numeric[responses$Punctuality == "i am always on time"] = 1
responses$Punctuality_Numeric[responses$Punctuality == "i am often early"] = 2
responses_final$Punctuality <- responses$Punctuality_Numeric

responses$Internet_usage_Numeric = 0
responses$Internet_usage_Numeric[responses$Internet.usage == "less than an hour a day"] = 1
responses$Internet_usage_Numeric[responses$Internet.usage == "few hours a day"] = 2
responses$Internet_usage_Numeric[responses$Internet.usage == "most of the day"] = 3
responses_final$Internet.usage <- responses$Internet_usage_Numeric

responses$Lying_Numeric = 0
responses$Lying_Numeric[responses$Lying == "only to avoid hurting someone"] = 1
responses$Lying_Numeric[responses$Lying == "sometimes"] = 2
responses$Lying_Numeric[responses$Lying == "everytime it suits me"] = 3
responses_final$Lying <- responses$Lying_Numeric


#replacing them with the mean of every row due to lack of data

for(i in 1:ncol(responses_final)){
 responses_final[is.na(responses_final[,i]), i] <- as.integer(mean(responses_final[,i], na.rm = TRUE))
}

#categorising the given data



View(responses_final)

music_preferences <- names(responses_final[,1:19])
movie_preferences <- names(responses_final[,20:31])
hobbies_interests <- names(responses_final[,32:63])
phobias <- names(responses_final[,64:73])
health_habits <- names(responses_final[,74:76])
personality_traits_vol_opinions <- names(responses_final[,77:133])
spending_habits <- names(responses_final[,134:140])
demographics <- names(responses_final[,141:150])


#PCA
new_frame <- responses_final
new_frame$Criminal.damage=NULL
prin_comp <- prcomp(new_frame, scale. = TRUE)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# this graph becomes flat at 150 , so we cannont eliminate columns



#visualisation for each categorised data
options(scipen = 999)
cor_plot = cor(responses_final)
corrplot(cor_plot, type = "upper")


#Question 1. Lifestyle of underweight vs overweight

#Question 2. Lifestyle of overweight and only child, overweight and siblings

#Question 3.  Predicting taking out anger by dmg, alcoholics and smokers

#Question 4. Spending habits vs having siblings

#Question 5. Do people with regrets have lesser number of friends, lonely and would want to change past.

#Question 6. Which kinds of movies/music are preferred - male vs female

#Correlation between interests and gender

#make sure to eliminate the row varibales that resemble the vector passed with the df

#####################
# Utility Functions #
#####################
corr_matrix <- function(y,df){
  x = c()
  for(i in c(1:150)){
    x <- c(x,cor(df[,i],df[,y]))
  }
  names <- c()
  corr <- c()
  for(i in c(1:150)){
      names <- c(names,names(df)[i])
      corr <- c(corr,x[i])
  }
  result <- data.frame(names,corr)
  return(result)
}

rmse <- function(error){
  sqrt(mean(error^2))
}

#########################
# Full database cluster #
#########################
k_means <- NbClust(responses_final[,1:150],min.nc = 2, max.nc = 20, method = "kmeans")

set.seed(100)
km_mo <- kmeans(responses_final,2)


clusplot(responses_final, km_mo$cluster,color=TRUE, shade=TRUE,
         labels=3, lines=0, main="K-means cluster plot")
table(k_means$Best.n[1,])
#11.65% is quite low for classification
kmeans_df <- responses_final






##########################################
# clustering of people over their traits #
##########################################
triats <- NbClust(responses_final[,77:133],min.nc = 2,max.nc = 20,method = "kmeans")
set.seed(50)
triat_mat <- kmeans(responses_final[,77:133],3)
aggregate(movie_co, by=list(cluster=trair$cluster), mean)
clusplot(responses_final[,77:133],triat_mat$cluster, color=TRUE,shade=TRUE,labels = 4, lines = 0, main = "traits clusters")








#####################################################
# predicting damage makers, alcoholics and smokers. #
#####################################################
responses_final$criminal = 0
responses_final$criminal[responses_final$Criminal.damage > 3] = 1

corr_crime_others = corr_matrix("criminal",responses_final)



dotchart(corr_crime_others$corr,labels = corr_crime_others$names)

#predicting alcoholics(people who drink a lot and social drinker)


responses_final$social_drinker = 0
responses_final$social_drinker[responses_final$Alcohol == 1] = 1

responses_final$alcoholic = 0
responses_final$alcoholic[responses_final$Alcohol == 2] = 1

social_corr_matrix = corr_matrix("social_drinker",responses_final)

alcoholic_corr_matrix = corr_matrix("alcoholic",responses_final)


dotchart(social_corr_matrix$corr,labels = social_corr_matrix$names)
dotchart(alcoholic_corr_matrix$corr,labels = alcoholic_corr_matrix$names)


#Smokers club
responses_final$smoking_former = 0
responses_final$smoking_former[responses_final$Smoking == 1] = 1

former_smoker_matrix = corr_matrix("smoking_former",responses_final)
dotchart(former_smoker_matrix$corr,labels = former_smoker_matrix$names)


responses_final$smoking_current = 0
responses_final$smoking_current[responses_final$Smoking == 2] = 2

current_smoker_matrix = corr_matrix("smoking_current",responses_final)
dotchart(current_smoker_matrix$corr,labels = current_smoker_matrix$names)

responses_final$smoker = 0
responses_final$smoker[responses_final$Smoking > 0] = 1

smoker_corr_matrix = corr_matrix("smoker",responses_final)
dotchart(smoker_corr_matrix$corr, labels = smoker_corr_matrix$names)




######################################################################################
#Splitting for test and train
#####################################################################################
df <- responses_final[,c(1:151)]

set.seed(120)
## 70% of the sample size for training data
intermediate <- createDataPartition(y = df$criminal, p=0.7, list=FALSE)
train.df <- df[intermediate,]
test.df <- df[-intermediate,]





###############################
##Model for damage over anger##
###############################
train_dmg <- subset(train.df,select = -c(Criminal.damage))
test_dmg = subset(test.df,select = -c(Criminal.damage))


#######################
# Logistic Regression #
#######################
model <- glm(criminal~.,family=binomial(link='logit'),data=train_dmg)
summary(model)
anova(model,test = "Chisq")
#model performance#
p <- predict(model,newdata = subset(test_dmg, select = -c(criminal)))
p2 <- predict(model,newdata = subset(train_dmg,select = -c(criminal)))
rmse_test_dmg <- RMSE(p, test_dmg$criminal)
rmse_train_dmg<- RMSE(p2, train_dmg$criminal)
p <- ifelse(p > 0.5,1,0)
misClassificationError <- mean(p != test_dmg$criminal)
print(paste('Accuracy',1-misClassificationError))

#Rock.n.roll, Empathy, Getting.angry, Small...big.dogs, Gender, Entertainment.spending, Getting.up, Children, Waiting, Number.of.friends, Dreams, Cheating.in.school, Self.criticism, Smoking, Snakes, Celebrities, Reading, Politics, Action, Fantasy.Fairy.tales, Music
#gave p confidence values below or equal 0.01

#now running this on the reduced data set

train_dmg_r1 <- subset(train_dmg, select = c(Rock.n.roll, Empathy, Getting.angry, Small...big.dogs, Gender, Entertainment.spending, Getting.up, Children, Waiting, Number.of.friends, Dreams, Cheating.in.school, Self.criticism, Smoking, Snakes, Celebrities, Reading, Politics, Action, Fantasy.Fairy.tales, Music, criminal))
model1 <- glm(criminal~.,family=binomial(link='logit'),data=train_dmg_r1)
summary(model1)
anova(model1,test = "Chisq")
#model1 performance
p1 <- predict(model1,newdata = subset(test_dmg, select = -c(criminal)))

rmse_lr <- RMSE(p1,test_dmg$criminal)
rmse_lr


#Since this gave a lower accuracy we choose the prior model

#####
#SVM#
#####

model_svm <- svm(criminal~.,train_dmg)
pred_svm <- predict(model_svm, train_dmg)
train_dmg$criminal
svm1_rmse <- RMSE(pred_svm,train_dmg$criminal)
svm1_rmse

  tuneResult <- tune(svm, criminal~.,data = train_dmg, ranges = list(epsilon = seq(0,1,0.1),cost = 2^(2:9)))


print(tuneResult)
plot(tuneResult)

tuneResult <- tune(svm,criminal~.,data = train_dmg,ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9)))

print(tuneResult)
plot(tuneResult)


#tuned_svm

tuned_svm <- svm(criminal~.,train_dmg, epsilon = 0.2, cost = 4)
pred_tune_svm <- predict(tuned_svm,subset(train_dmg))
pred_tune_svm
rmse_tuned_svm <- RMSE(pred_tune_svm, train_dmg$criminal)
rmse_tuned_svm


pred_tune_test <- predict(tuned_svm,test_dmg)
rmse_test_tuned_svm <- RMSE(pred_tune_test,test_dmg$criminal)
rmse_test_tuned_svm


#we can see that the rmse error of the refined or further tuned svm has better value than
#that of the rmse of logistic regression




##################
# SMOKE DETECTOR #
##################


#train and test data

dfs <- subset(responses_final, select = -c(151,152,153,154,155))
set.seed(120)
## 70% of the sample size for training data
intermediate <- createDataPartition(y = dfs$smoker, p=0.7, list=FALSE)
train.dfs <- dfs[intermediate,]
test.dfs <- dfs[-intermediate,]


train_smoke <- subset(train.dfs,select = -c(Smoking))
test_smoke <- subset(test.dfs,select = -c(Smoking))


#######################
# Logistic Regression #
#######################
model_lr_smoker <- glm(smoker~.,family=binomial(link='logit'),data=train_smoke)
summary(model_lr_smoker)
#anova(model_lr_smoker,test = "Chisq")
#model performance#
p <- predict(model_lr_smoker,newdata = subset(test_smoke, select = -c(smoker)))
p2 <- predict(model_lr_smoker,newdata = subset(train_smoke, select = -c(smoker)))
rmse_smoke_train <- RMSE(p2,train_smoke$smoker)
rmse_smoke_lr <- RMSE(p,test_smoke$smoker)
print(paste("lr_rmse: ",rmse_smoke_lr))


#######
# KNN #
#######


rmse_knn_p <- rep(0, 100)
k <- 1:100
for(x in k){
  knn_smoke <- knn3(smoker~., data = train_smoke, k = x)
  pred_knn <- predict(knn_smoke,train_smoke)
  rmse_knn_p[x] <- RMSE(pred_knn, train_smoke$smoker)
}

plot(k, rmse_knn_p, type = 'b')

#around k = 50 the decrease in rmse is very low so we can choose k = 50 for the model

knn_smoke <- knn3(smoker~., data = train_smoke, k = 50)
pred_knn <- predict(knn_smoke,train_smoke)
rmse_knn <- RMSE(pred_knn, train_smoke$smoker)
rmse_knn
#giving us the error of 0.5249724

pred_knn_test <- predict(knn_smoke,test_smoke)
rmse_knn_test <- RMSE(pred_knn_test, test_smoke$smoker)
rmse_knn_test

#error for test is 0.5283913


#####
#SVM#
#####

model_svm_smoke <- svm(smoker~.,train_smoke)
pred_svm_smoke <- predict(model_svm_smoke, train_smoke)
svm1_rmse_smoke <- RMSE(pred_svm_smoke,train_smoke$smoker)
print(paste("untuned_svm_rmse: ",svm1_rmse_smoke))

tuneResult_smoke <- tune(svm, smoker~.,data = train_smoke, ranges = list(epsilon = seq(0,1,0.1),cost = 2^(2:9)))


print(tuneResult_smoke)
plot(tuneResult_smoke)

tuneResult <- tune(svm,smoker~.,data = train_smoke,ranges = list(epsilon = seq(0,0.3,0.01), cost = 2^(2:9)))

print(tuneResult)
plot(tuneResult)

#tuned_svm

tuned_svm <- svm(criminal~.,train_dmg, epsilon = 0.25, cost = 4)
pred_tune_svm <- predict(tuned_svm,subset(train_dmg))
pred_tune_svm
rmse_tuned_svm <- RMSE(pred_tune_svm, train_dmg$criminal)
print(paste("svm_train_rmse: ",rmse_tuned_svm))


pred_tune_test <- predict(tuned_svm,test_dmg)
rmse_test_tuned_svm <- RMSE(pred_tune_test,test_dmg$criminal)
print(paste("svm_test_rmse: ",rmse_test_tuned_svm))


#we can see that the rmse error of the refined or further tuned svm has better value than
#that of the rmse of logistic regression























  ###Music Preferences
```{r}
triats <- NbClust(responses_final[,1:19],min.nc = 2,max.nc = 20,method = "kmeans")
set.seed(100)
triat_mat <- kmeans(responses_final[,1:19],3)

clusplot(responses_final[,1:19],triat_mat$cluster, color=TRUE,shade=TRUE,labels = 4, lines = 0, main = "Music")
```

  ###Movie Preferences
```{r}
triats <- NbClust(responses_final[,20:31],min.nc = 2,max.nc = 20,method = "kmeans")
set.seed(100)
triat_mat <- kmeans(responses_final[,20:31],3)

clusplot(responses_final[,20:31],triat_mat$cluster, color=TRUE,shade=TRUE,labels = 4, lines = 0, main = "Movies")
```

  ###Hobbies & Interests
```{r}
triats <- NbClust(responses_final[,32:63],min.nc = 2,max.nc = 20,method = "kmeans")
set.seed(100)
triat_mat <- kmeans(responses_final[,32:63],2)

clusplot(responses_final[,32:63],triat_mat$cluster, color=TRUE,shade=TRUE,labels = 4, lines = 0, main = "Hobbies & Interests")
```

  ###Phobias
```{r}
triats <- NbClust(responses_final[,64:73],min.nc = 2,max.nc = 20,method = "kmeans")
set.seed(100)
triat_mat <- kmeans(responses_final[,64:73],2)

clusplot(responses_final[,64:73],triat_mat$cluster, color=TRUE,shade=TRUE,labels = 4, lines = 0, main = "Phobias")
summary(trait_mat$cluster)
```



music_preferences <- names(responses_final[,1:19])
movie_preferences <- names(responses_final[,20:31])
hobbies_interests <- names(responses_final[,32:63])
phobias <- names(responses_final[,64:73])
health_habits <- names(responses_final[,74:76])
personality_traits_vol_opinions <- names(responses_final[,77:133])
spending_habits <- names(responses_final[,134:140])
demographics <- names(responses_final[,141:150])






