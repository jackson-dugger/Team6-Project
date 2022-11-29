require(readr)
require(randomForest)
require(caret)
require(MASS)
require(neuralnet)
require(fdm2id)
require(nnet)
require(Metrics)

fires <- read_csv("forestfires.csv")
head(fires)

fires <- na.omit(fires)

fires <- fires %>% mutate_if(is.character, as.factor)

fires <- fires %>% mutate(area_log = log(area+1))

set.seed(1234)
samp <- sample(2, nrow(fires), replace=T, prob=c(0.7,0.3))
train <- fires[samp==1,]
test <- fires[samp==2,]

# change these kernels and types to minimize RMSE
svr_model_1 <- svm(area_log~., train)
svr_model_2 <- svm(area_log~temp+RH+DMC+rain, train)

pred_y_svr_1 <- predict(svr_model_1, test)
pred_y_svr_2 <- predict(svr_model_2, test)

plot(pred_y_svr_1,test$area_log)
lines(x=0:50,y=0:50)
plot(pred_y_svr_2,test$area_log)
lines(x=0:50,y=0:50)

plot(exp(pred_y_svr_1)-1,exp(test$area_log)-1,xlim=c(0,30))
lines(x=0:50, y=0:50)
plot(exp(pred_y_svr_2)-1,exp(test$area_log)-1,xlim=c(0,20))
lines(x=0:50, y=0:50)

coefs_1 <- t(svr_model_1$coefs) %*% svr_model_1$SV
intercept_1 <- svr_model_1$rho

coefs_2 <- t(svr_model_2$coefs) %*% svr_model_2$SV
intercept_2 <- svr_model_2$rho

RMSE_1 <- rmse(predicted=pred_y_svr_1,actual=test$area_log)
RMSE_1
RMSE_2 <- rmse(pred_y_svr_2,test$area_log)
RMSE_2

exp(coefs_1)-1
exp(intercept_1)-1

r2 <- R2(obs=test$area_log,pred=pred_y_svr_1,form='traditional')
mse_1 <- RMSE_1^2

r2_2 <- R2(obs=test$area_log,pred=pred_y_svr_2,form='traditional')

svr_model_1
mse_1
r2

# Try Random Forest Regression now

set.seed(123)
rf <- randomForest(area_log~temp+RH+DMC+rain, data=train, ntree=47)

rf

pred_y_rf <- predict(rf, test)

plot(pred_y_rf, test$area_log)
lines(x=0:50,y=0:50)

summary(rf)
varImpPlot(rf)

which(rf$mse == min(rf$mse))

R2(obs=test$area_log,pred=pred_y_rf,form='traditional')
