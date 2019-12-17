
library("readxl")
library("dplyr")
library("ggplot2")

library("class")
library("visNetwork")
FU19_data <- read_excel("C:/Users/aayus/Desktop/FIFA19 - Ultimate Team players Filtered.xlsx")
View(FU19_data)
attach(FU19_data)
FU19_data<-FU19_data[complete.cases(FU19_data[ , 0:1]),]#Used to filter the read data and remove the rows with NA values in player_id and name
FU19_data<-FU19_data %>% distinct(player_name, player_extended_name, .keep_all = TRUE)
View(FU19_data)
FU19_data[c("player_name","player_extended_name","player_id","quality","origin","club","league","date_of_birth","height","weight","intl_rep","added_date","att_workrate","def_workrate","weak_foot","skill_moves","base_id","resource_id","ps4_last","ps4_min","ps4_max","ps4_prp","xbox_last","xbox_min","xbox_max","xbox_prp","pc_last","pc_min","pc_max","pc_prp","age","cb","rb","pref_foot","lb","rwb","lwb","cdm","cm","rm","lm","cam","cf","rf","lf","rw","lw","st","traits","specialities")] <- list(NULL)
icon_data <- FU19_data[which(FU19_data$revision == 'Icon',arr.ind = TRUE), ]
reg_data <- FU19_data[which(FU19_data$revision != 'Icon',arr.ind = TRUE), ]
FU19_data_except_GK <- FU19_data[which(FU19_data$position != 'GK',arr.ind = TRUE), ]
FU19_data_except_GK<- FU19_data_except_GK[,-c(40:45)]
View(FU19_data_except_GK)
icon_data <- subset(icon_data,icon_data$overall > 85)
reg_data <- subset(reg_data,reg_data$overall > 85)

View(reg_data)
View(icon_data)
hist(reg_data$overall,border="blue",col="green",main = "Histogram of overall rating > 85 for current players", xlab="Overall")
hist(icon_data$overall,border="blue",col="red",main = "Histogram of overall rating for iconic players", xlab="Overall")
sub_data_ST <- reg_data[ which(reg_data$position =='ST'), ]
View(sub_data_ST)
hist(sub_data_ST$overall, main="Histogram of Current Strikers overall ", xlab="Overall", border = "blue", col = "green")
icon_data_ST <- icon_data[ which(icon_data$position =='ST'), ]
View(icon_data_ST)
hist(icon_data_ST$overall, main="Histogram of iconic Strikers overall ", xlab="Overall", border = "blue", col = "green")


sub_data_CAM <- reg_data[ which(reg_data$position =='CAM'), ]
View(sub_data_CAM)
hist(sub_data_CAM$overall, main="Histogram of Current CAM overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_CAM <- icon_data[ which(icon_data$position =='CAM'), ]
View(icon_data_CAM)
hist(icon_data_CAM$overall, main="Histogram of iconic CAM overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_CF <- reg_data[ which(reg_data$position =='CF'), ]
View(sub_data_CAM)
hist(sub_data_CF$overall, main="Histogram of Current CF overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_CF <- icon_data[ which(icon_data$position =='CF'), ]
View(icon_data_CAM)
hist(icon_data_CF$overall, main="Histogram of iconic CF overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_CB <- reg_data[ which(reg_data$position =='CB'), ]
View(sub_data_CB)
hist(sub_data_CB$overall, main="Histogram of Current CB overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_CB <- icon_data[ which(icon_data$position =='CB'), ]
View(icon_data_CB)
hist(icon_data_CB$overall, main="Histogram of iconic CB overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_LW <- reg_data[ which(reg_data$position =='LW'), ]
View(sub_data_LW)
hist(sub_data_LW$overall, main="Histogram of Current LW overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_LW <- icon_data[ which(icon_data$position =='LW'), ]
View(icon_data_LW)
hist(icon_data_LW$overall, main="Histogram of iconic LW overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_RW <- reg_data[ which(reg_data$position =='RW'), ]
View(sub_data_RW)
hist(sub_data_RW$overall, main="Histogram of Current RW overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_RW <- icon_data[ which(icon_data$position =='RW'), ]
View(icon_data_RW)
hist(icon_data_RW$overall, main="Histogram of iconic RW overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_LM <- reg_data[ which(reg_data$position =='LM'), ]
View(sub_data_LM)
hist(sub_data_LM$overall, main="Histogram of Current LM overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_LM <- icon_data[ which(icon_data$position =='LM'), ]
View(icon_data_LM)
hist(icon_data_LM$overall, main="Histogram of iconic LM overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_LB <- reg_data[ which(reg_data$position =='LB'), ]
View(sub_data_LB)
hist(sub_data_LB$overall, main="Histogram of Current LB overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_LB <- icon_data[ which(icon_data$position =='LB'), ]
View(icon_data_LB)
hist(icon_data_LB$overall, main="Histogram of iconic LB overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_RB <- reg_data[ which(reg_data$position =='RB'), ]
View(sub_data_RB)
hist(sub_data_RB$overall, main="Histogram of Current RB overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_RB <- icon_data[ which(icon_data$position =='RB'), ]
View(icon_data_RB)
hist(icon_data_RB$overall, main="Histogram of iconic RB overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_CM <- reg_data[ which(reg_data$position =='CM'), ]
View(sub_data_CM)
hist(sub_data_CM$overall, main="Histogram of Current CM overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_CM <- icon_data[ which(icon_data$position =='CM'), ]
View(icon_data_CM)
hist(icon_data_CM$overall, main="Histogram of iconic CM overall Rating ", xlab="Overall", border = "blue", col = "red")

sub_data_CDM <- reg_data[ which(reg_data$position =='CDM'), ]
View(sub_data_CDM)
hist(sub_data_CDM$overall, main="Histogram of Current CDM overall Rating ", xlab="Overall", border = "blue", col = "green")
icon_data_CDM <- icon_data[ which(icon_data$position =='CDM'), ]
View(icon_data_CDM)
hist(icon_data_CDM$overall, main="Histogram of iconic CDM overall Rating ", xlab="Overall", border = "blue", col = "red")


sub_data_GK <- reg_data[ which(reg_data$position =='GK'), ]#regular GK data

sub_data_GK <- subset(sub_data_GK,sub_data_GK$overall > 85)
View(sub_data_GK)
icon_data_GK <- icon_data[ which(icon_data$position =='GK'),]#icon GK data
hist(icon_data_GK$overall, main="Histogram of Overall for iconic Goal Keepers ", xlab="Overall", border = "blue", col = "red")
hist(sub_data_GK$overall, main="Histogram of Overall for current Goal Keepers ", xlab="Overall", border = "blue", col = "green")
View(icon_data_GK)

p <- ggplot(icon_data_GK, aes(x= overall , y=gk_reflexes)) + 
  geom_violin(trim = FALSE) +
  labs(title="Violin Plot of Overall Ratings for Iconic GK" )
p

r <- ggplot(sub_data_GK, aes(x= overall , y=gk_reflexes,group = overall,fill = 'red')) + 
  geom_violin(trim = FALSE) +
  labs(title="Violin Plot of Overall Ratings for Current GK" )
r

plot(sub_data_ST$overall, sub_data_ST$shoot_finishing, main="Scatterplot ",
     xlab="Overall ", ylab="Finishing ", pch=19)
# Add fit lines
abline(lm(sub_data_ST$shoot_finishing~sub_data_ST$overall), col="red") # regression line (y~x)
lines(lowess(sub_data_ST$overall,sub_data_ST$shoot_finishing), col="blue") # lowess line (x,y)

plot(icon_data_ST$overall, icon_data_ST$shoot_finishing, main="Scatterplot ",
     xlab="Icon's Overall ", ylab="Icon's Finishing ", pch=19)
# Add fit lines
abline(lm(icon_data_ST$shoot_finishing~icon_data_ST$overall), col="red") # regression line (y~x)
lines(lowess(icon_data_ST$overall,icon_data_ST$shoot_finishing), col="blue") # lowess line (x,y)

as.factor(FU19_data_except_GK$overall)


##KNN
FU19_data_except_GK<- FU19_data_except_GK[,-1]
FU19_data_except_GK<- FU19_data_except_GK[,-c(2:3)]
ovr <- as.numeric(FU19_data_except_GK$overall)
ovr <- cut(FU19_data_except_GK$overall, br=c(-1,50,80,99), labels = c('bad', 'average', 'good'))
table(ovr)
summary(ovr)
x <- sample(13766, 11000)
FU19k_data_train <- FU19_data_except_GK[x,]
FU19k_data_test <- FU19_data_except_GK[-x,]
cl <- FU19k_data_train$overall
FU19k_data_KNN <- knn(train = FU19k_data_train[2:36], test = FU19k_data_test[2:36], cl, k = 50)
FU19k_data_KNN
table(FU19k_data_KNN) 
summary(FU19_data_except_GK)
View(FU19_data_except_GK)
confmat <- table(FU19k_data_KNN, FU19k_data_test$overall)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confmat)

#Decision Tree
library(rpart)
library(rpart.plot)
x <- sample(13766, 11000)
FU19k_data_train <- FU19_data_except_GK[x,]
FU19k_data_test <- FU19_data_except_GK[-x,]
# generate the decision tree model
dectionTreeModel <- rpart(overall~., FU19k_data_train)
dectionTreeModel
rpart.plot(dectionTreeModel)
visTree(dectionTreeModel)
