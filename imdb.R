library(Rcpp)
library(lattice)
library(mice)
library(plyr)
library(psych)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(cluster)
library(class)
library(caret)
library(corrplot)


# set working directory
setwd("~/Desktop/637 Final Project")

################################# Read in Data #########################################

# get data
movie<- read.csv('movie_metadata.csv')

sample = head(movie,n=1)
sample

# find the dimension of the data
dim(movie)

# list strcuture of the data
str(movie)

################################# EDA #####################################################
# Box plots with IMDB ratings by year. 
# There are alot more higher rated films towards the beginning of the 20th century.
# An explanation for this is because many older films are classics and classics, by nature, are usually the best films of their era. 
# Bad old films are unlikely to have any ratings.
ggplot(movie, aes(x =factor(title_year), y = imdb_score)) + geom_boxplot() + scale_x_discrete(breaks = pretty(movie$title_year,n=20))

# correlation matrix
M <- cor(movie_new)
corrplot(M, method="number")

# correlation between dependent variable and independent variables
correlation <- c()
for(i in 1:dim(movie_new)[2])
{
  correlation[i] <- cor(movie_new[,i],movie_new[,'imdb_score'])
}
correlation

# ggplot of top two variables which have high correlations with imdb_score
ggplot(movie_new, aes(x = num_voted_users, y = imdb_score)) + geom_point() + geom_smooth()
ggplot(movie_new, aes(x = num_critic_for_reviews, y = imdb_score)) + geom_point() + geom_smooth()


################################# Data Preparation #########################################

# use only numerical input variables, discard factor variables
movie_temp <- movie[, c(3, 4, 5, 6, 8, 9, 13, 14, 16, 19, 23, 24, 25, 26, 27, 28)]
write.csv(movie_temp,"movie_numeric.csv")


############# Missing Data #################

# find how many N/A data in total and in each column
sum(is.na(movie_temp))
colSums(is.na(movie_temp))


# use mice package to impute missing values
movie_new <- mice(movie_temp,m=5,method='cart',printFlag=FALSE)
movie_new <- complete(movie_new)

#check if there are still missing values
sum(is.na(movie_new))
dim(movie_new)
write.csv(movie_new, "Movie_nomissing.csv")


############# Outlier #################

# get the histograms of all variables & check outliers
multi.hist(movie_new)

# inspect variable aspect_ratio
hist(movie_new$aspect_ratio)
range(movie_new$aspect_ratio)
quantile <- quantile(movie_new$aspect_ratio)
range <- quantile[4] + 1.5 * (quantile[4] - quantile[2])
range

# delete rows where variable aspect_ratio > 3.1 (outliers)
movie_new = movie_new[!movie_new$aspect_ratio > 3.1,]
range(movie_new$aspect_ratio)
dim(movie_new)


############# Data Normalization #################

# normalize the data 
normalize <- function(x)
{
  z <- (x - min(x,na.rm = TRUE))/(max(x,na.rm = TRUE) - min(x,nna.rm = TRUE))
  return(z)
}

nom <- movie_new
for(i in 1:ncol(nom)){
    nom[,i] <- normalize(nom[,i])
  }
write.csv(nom, "Movie_nom.csv")


# split normalized data into training and test data
set.seed(100)
train_normalize <- sample(1:nrow(nom), 0.8*nrow(nom)) 
train_nom <- nom[train_normalize,]
test_nom <- nom[-train_normalize,]



############# Split Data into Training/Test #################

# split original data into training and test data
set.seed(100)
train <- sample(1:nrow(movie_new), 0.8*nrow(movie_new)) 
train_data <- movie_new[train,]
test_data <- movie_new[-train,]


############# Categorize Response Variable #################

# analysis on the response variable: imdb_score
ggplot(movie_new, aes(x=movie_new$imdb_score)) + geom_histogram(fill="white", colour="black")
range(movie_new$imdb_score) 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(movie_new$imdb_score)

# categorize the response variable to 9 groups
score_new <- cut(train_data$imdb_score, breaks=c(1,2,3,4,5,6,7,8,9,10))
train_new <- train_data[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16)]


################################# Decision Tree #########################################

# Approach 1: Visualize: Decision Tree using the traing data, imdb_score categorized into 9 groups
m.rpart <- rpart(score_new~.,data=train_new,method='class',cp=0.004)
m.rpart
rpart.plot(m.rpart)

# adjust the value of cp to get different complexity of the tree
plotcp(m.rpart)


# Approach 2: Decision Tree using the training data, no change on imdb_score
m.rpart2 <- rpart(imdb_score~.,data=train_nom,method='class',cp=0.002)
m.rpart2
rpart.plot(m.rpart2)
plotcp(m.rpart2)

# test the Decision Tree model and predict accuracy
pred_tree <- predict(m.rpart2,newdata=test_nom)

# calculate RMSE error
rmse_tree <- sqrt(mean((pred_tree-test_nom$imdb_score)^2))

# MSE
mean((pred_tree - test_nom$imdb_score)^2)



################################# K Means Clustering  #########################################

set.seed(100)
# decide on the number of k
# Compute and plot wss for k = 2 to k = 10
k.max <- 10 # Maximal number of clusters
wss <- sapply(1:k.max, 
              function(k){kmeans(nom[,c(1,6,11,14)], k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

abline(v = 4, lty =2)


movie.cluster <- kmeans(nom, 4)

movie.cluster <- kmeans(nom[,c(1,6,11,14)], 4)

# plot the cluster
# plot(nom[c("imdb_score","num_critic_for_reviews")], col = movie.cluster$cluster)
clusplot(nom, movie.cluster$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)



################################# Random Forest #########################################

# Random Forest using the training data
rf <- randomForest(imdb_score~., data = train_nom)

# test the Random Forest model and predict accuracy
pred_rf <- predict(rf, test_nom)
pred_rf <- as.numeric(paste(pred_rf))

# calculate RMSE error
rmse_rf <- sqrt(mean((pred_rf-test_nom$imdb_score)^2))

# MSE
mean((pred_rf - test_nom$imdb_score)^2)

# plot the variable importance
varImpPlot(rf,type=2)


################################# Linear Regression #########################################

# fit a linear regression model and test the accuracy
lm.fit <- glm(imdb_score~., data=train_data)
summary(lm.fit)
pred_lm <- predict(lm.fit,test_data)

#calculate RMSE error
rmse_lm <- sqrt(mean((pred_lm-test_data$imdb_score)^2))

# MSE
mean((pred_lm - test_data$imdb_score)^2)


################################# Neural Network #########################################

# Neural Network 
n <- names(train_nom)
nn <- neuralnet(imdb_score~num_critic_for_reviews+duration+director_facebook_likes+actor_3_facebook_likes+actor_1_facebook_likes+gross+num_voted_users+cast_total_facebook_likes+facenumber_in_poster+num_user_for_reviews+budget+title_year+actor_2_facebook_likes+aspect_ratio+movie_facebook_likes,data=train_nom,hidden=2,threshold=0.05)
plot(nn)
print(nn)
pred <- compute(nn,test_nom[,1:15])
pred_nn <- as.data.frame(pred$net.result)


#calculate RMSE error
rmse_nn <- sqrt(mean((pred_nn-test_nom$imdb_score)^2))

#MSE
mean((pred_nn - test_nom$imdb_score)^2)



################################# K Nearest Neighbor #########################################

# make dependent variable as factors
movie_new$imdb_score <- factor(movie_new$imdb_score)
movie_new$imdb_score <- as.numeric(paste(movie_new$imdb_score))

# apply the algorithm
pred_knn<- knn(train_nom, test_nom, train_nom$imdb_score, k=1)

# measure the accuracy when k=1
pred_knn <- as.numeric(paste(pred_knn))
mean(pred_knn == test_nom$imdb_score)

#calculate RMSE error
rmse_knn <- sqrt(mean((pred_knn-test_nom$imdb_score)^2))

# MSE
mean((pred_knn - test_nom$imdb_score)^2)

# decide on the number of k
accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  pred_knn <- knn(train_nom, test_nom,
                    train_nom$imdb_score, k = x)
  accuracy[x] <- mean(pred_knn == test_nom$imdb_score)
}

plot(k, accuracy, type = 'b')



################################# Compare Results #########################################

# plot the performance of each approach 

# RF
plot(test_data$imdb_score,pred_rf,col='red',main='Real vs predicted RF',pch=20,cex=0.5)
abline(0,1,lwd=2)
legend('bottomright',legend='RF',pch=18,col='red', bty='n')

# NN
pred_nn <- data.matrix(pred_nn)
plot(test_nom$imdb_score,pred_nn,col='green',main='Real vs predicted NN',pch=20,cex=0.5)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='green', bty='n')

# KNN
plot(test_nom$imdb_score,pred_knn,col='blue',main='Real vs predicted KNN',pch=20,cex=0.5)
abline(0,1,lwd=2)
legend('bottomright',legend='KNN',pch=18,col='blue', bty='n')


plot(test_nom$imdb_score,pred_rf,col='red',main='Real vs Predicted IMDB Ratings',pch=18,cex=0.7,xlab = 'Real Score',ylab = 'Predicted Score')
points(test_nom$imdb_score,pred_nn,col='green',pch=18,cex=0.7)
points(test_nom$imdb_score,pred_knn,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('RF','NN','KNN'),pch=18,col=c('red','green','blue'))




