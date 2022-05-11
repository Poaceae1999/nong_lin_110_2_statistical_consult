library(rpart)
library(tidyverse)
library(glmnet)  
library(caret)  
library(dplyr)   
library(car)
library(nnet)
library(GGally)
library(lmtest)
library(pheatmap)
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
library(dplyr)
library(magrittr)


tea <- readRDS("nonglin_tea.RDS")
analyze_data <- readRDS('analyze.RDS') 

# CART ----


weight_rpart_model <- rpart(level~.,data = analyze_data)
plot(weight_rpart_model)
text(weight_rpart_model)

weight_rpart_predict <- predict(weight_rpart_model,analyze_data)
predict_vector <- function(one_row,model = weight_rpart_predict)
{
  return(colnames(model)[which(one_row==max(one_row))])
}
dim(weight_rpart_predict) <- c(129,1)
weight_predict_calss <- apply(weight_rpart_predict,1,predict_vector)
unclass(analyze_data$buds_weight_100) %>% 
  attr(.,'levels')[.] %>% 
  table(weight_predict_calss,.)

chem_rpart_model <- rpart(total_catechins~.,data = analyze_data)
plot(chem_rpart_model)
text(chem_rpart_model)

# lasso ----
numeric_dat <- analyze_data %>% select(.,-c(key,level,observe_ys,observe_date:sample_label,G1L:O9L,FAA:total_catechins,year,temp_differ))
train_idx <- sample(1:141,100)
test_idx <- !(1:141 %in%train_idx)
train <- numeric_dat[train_idx,]
test <- numeric_dat[test_idx,]
xtrain <- model.matrix(polyphenol~., train)[,-1]
ytrain <- train$polyphenol

ytest <- test$polyphenol
season7th <- 0
xtest <- model.matrix(polyphenol~., test)[,-1] %>% cbind(season7th) %>% as_tibble() %>% select(.,season2nd:season6th,season7th,everything()) %>% as.matrix()




lambdas_to_try <- 10^seq(-3, 7, length.out = 100)
lasso_cv <- cv.glmnet(xtrain, ytrain, alpha = 1, lambda = lambdas_to_try)
plot(lasso_cv)
best_lambda_lasso <- lasso_cv$lambda.min
best_lambda_lasso
lasso_mod <- glmnet(xtrain, ytrain, alpha = 1, lambda = best_lambda_lasso)
predict.glmnet(lasso_mod, type = 'coefficients')
fit <- lm(polyphenol~season+Total_leaf+OTRatio+cutting_height+leaf_number+open_plane+buds_weight_100+acu_mean_temp+growth_mean_temp,data = test)
lasso_pred <- predict(lasso_mod,  newx = xtest)
lm_pred <- predict(fit)
mean((lasso_pred-ytest)^2)
cbind(ytest,lasso_pred,lm_pred)
numeric_dat <- numeric_dat %>% select(,-c(duration_oc))
prin <- prcomp(scale(numeric_dat))
biplot(prin)
screeplot(prin)
pheatmap(cor(numeric_dat))

corrplot::corrplot(as.matrix(cor(numeric_dat)))
urlPackage <- 'https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-14.tar.gz'
install.packages(urlPackage, repos=NULL, type="source") 

analyze_data <- readRDS('analyze.RDS') 
numeric_dat <- analyze_data %>% select(.,-c(key,observe_ys,observe_date:sample_label,G1L:O9L,year,temp_differ))
train_idx <- sample(1:138,100)
test_idx <- !(1:138 %in%train_idx)
train <- numeric_dat[train_idx,]
test <- numeric_dat[test_idx,]

xtrain <- model.matrix(polyphenol~.+acu_mean_temp*RH*Solar_rad_H*Solar_rad_MJM2, train)[,-1]
ytrain <- train$polyphenol
ytest <- test$polyphenol
xtest <- model.matrix(polyphenol~.+acu_mean_temp*RH*Solar_rad_H*Solar_rad_MJM2, test)[,-1] %>%  as_tibble() %>% select(.,season2nd:season6th,season7th,everything()) %>% as.matrix()
data_polyphenol <- analyze_data %>% select(., c(cultivar,rain,polyphenol,acu_mean_temp,RH,Solar_rad_H,Solar_rad_MJM2))
and_rf <- randomForest(polyphenol~.+acu_mean_temp*RH*Solar_rad_H*Solar_rad_MJM2, data = data_polyphenol[train_idx,])
and_pred <- predict(and_rf,data_polyphenol[test_idx,])
(mean((and_pred-ytest)^2)/var(ytest))^0.5


xtrain <- model.matrix(FAA~., train)[,-1]
ytrain <- train$FAA
ytest <- test$FAA
xtest <- model.matrix(FAA~., test)[,-1] %>%  as_tibble() %>% select(.,season2nd:season6th,season7th,everything()) %>% as.matrix()
data_FAA <- analyze_data %>% select(., c(cultivar,FAA,rain,acu_mean_temp,RH,Solar_rad_H,Solar_rad_MJM2))
and_rf <- randomForest(FAA~., data = data_FAA[train_idx,])
and_pred <- predict(and_rf,data_FAA[test_idx,])
(mean((and_pred-ytest)^2)/var(ytest))^0.5

xtrain <- model.matrix(Gallic_Acid~., train)[,-1]
ytrain <- train$Gallic_Acid
ytest <- test$Gallic_Acid
xtest <- model.matrix(Gallic_Acid~., test)[,-1] %>%  as_tibble() %>% select(.,season2nd:season6th,season7th,everything()) %>% as.matrix()
data_Gallic_Acid <- analyze_data %>%  select(., c(cultivar,rain,Gallic_Acid,acu_mean_temp,RH,Solar_rad_H,Solar_rad_MJM2))
and_rf <- randomForest(Gallic_Acid~., data = data_Gallic_Acid[train_idx,])
and_pred <- predict(and_rf,data_Gallic_Acid[test_idx,])
(mean((and_pred-ytest)^2)/var(ytest))^0.5

xtrain <- model.matrix(GC~., train)[,-1]
ytrain <- train$GC
ytest <- test$GC
xtest <- model.matrix(GC~., test)[,-1] %>%  as_tibble() %>% select(.,season2nd:season6th,season7th,everything()) %>% as.matrix()
data_GC <- analyze_data %>%  select(., c(cultivar,rain,GC,acu_mean_temp,RH,Solar_rad_H,Solar_rad_MJM2))
and_rf <- randomForest(GC~., data = data_GC[train_idx,])
and_pred <- predict(and_rf,data_GC[test_idx,])
(mean((and_pred-ytest)^2)/var(ytest))^0.5

xtrain <- model.matrix(GCG~., train)[,-1]
ytrain <- train$GCG
ytest <- test$GCG
xtest <- model.matrix(GCG~., test)[,-1] %>%  as_tibble() %>% select(.,season2nd:season6th,season7th,everything()) %>% as.matrix()
data_GCG <- analyze_data %>% select(., c(cultivar,GCG,rain,acu_mean_temp,RH,Solar_rad_H,Solar_rad_MJM2))
and_rf <- randomForest(GCG~., data = data_GCG[train_idx,])
and_pred <- predict(and_rf,data_GCG[test_idx,])
(mean((and_pred-ytest)^2)/var(ytest))^0.5

xtrain <- model.matrix(Catechin~., train)[,-1]
ytrain <- train$Catechin
ytest <- test$Catechin
xtest <- model.matrix(Catechin~., test)[,-1] %>%  as_tibble() %>% select(.,season2nd:season6th,season7th,everything()) %>% as.matrix()
data_caffeine <- analyze_data %>% select(., c(cultivar,Catechin,rain,acu_mean_temp,RH,Solar_rad_H,Solar_rad_MJM2))
and_rf <- randomForest(Catechin~., data = data_caffeine[train_idx,])
and_pred <- predict(and_rf,data_caffeine[test_idx,])
(mean((and_pred-ytest)^2)/var(ytest))^0.5

chem <- analyze_data %>% select(,c(polyphenol:total_catechins,level))
chem$level <- factor(chem$level) %>% as.numeric()
chem$level <- ifelse(chem$level>1,1,0)

pr_chem <- prcomp(scale(chem) )
biplot(pr_chem)

xtrain <- model.matrix(level~., train)[,-1]
ytrain <- train$level
ytest <- test$level
xtest <- model.matrix(level~., test)[,-1] %>%  as_tibble() %>% select(.,season2nd:season6th,season7th,everything()) %>% as.matrix()
data_level <- analyze_data %>% select(., c(cultivar,level,polyphenol:total_catechins))
data_level$level <- factor(data_level$level)
and_rf <- randomForest(level~., data = data_level[train_idx,])
and_pred <- predict(and_rf,data_level[test_idx,])
table(data_level[test_idx,]$level,and_pred)

summary(fit)
chem[,1:13] <- log(chem[,1:13])
test_idx <- sample(1:138,size = 30)
train_idx <- not(1:138 %in% test_idx)
chem_test <- chem[test_idx,]
chem_train <- chem[train_idx,]

logit_level <- glm(level~caffeine+GCG ,data=chem_train,family=binomial())
# logit_level <- glm(level~Catechin+caffeine+FAA+polyphenol,data=chem,family=binomial())
summary(logit_level)
predicted_level <- predict(logit_level,chem_test)
predicted_level <- ifelse(predicted_level<=0.7,'B','C')
table(chem_test$level ,predicted_level)


# forward sekection ----

# 1.建立空的線性迴歸(只有截距項)
null <- glm(level~1 ,data=chem_train,family=binomial())
full <- glm(level~. ,data=chem_train,family=binomial())

# 2.使用step()，一個一個把變數丟進去
forward.lm = step(null, 
                  # 從空模型開始，一個一個丟變數，
                  # 最大不會超過完整的線性迴歸
                  # (一定要加上界 upper=full，不可以不加) 
                  scope=list(lower=null, upper=full), 
                  direction="forward")
backward.lm = step(full, 
                  # 這裡可以加下界(lower=null)，也可以不加
                  scope = list(upper=full), 
                  direction="backward")  

y_pred <- predict(forward.lm,chem_test)
cross_validation <- function(y_pred,ytest){
  y_pred <- ifelse(y_pred<=0.7,'B','C')
  ytest <- chem_test$level
  print(table(y_pred,ytest))
}



# lasso & ridge for chem ----

ridge = glmnet(x = as.matrix(chem[, -14]), 
               y = chem[, 14], 
               alpha = 0,
               family = "binomial")

lasso = glmnet(x = as.matrix(chem[, -14]), 
               y = chem[, 14], 
               alpha = 1,
               family = "binomial")

par(mfcol = c(1, 2)) # cool!!
plot(lasso, xvar='lambda', main="Lasso")
plot(ridge, xvar='lambda', main="Ridge")

# 經由 cv 的手法，評估每個模型在不同 lambda 下 
# 的 cvm(mean cross-validated error)
cv.lasso = cv.glmnet(x = as.matrix(chem[, -14]), 
                     y = chem[, 14], 
                     alpha = 1,  # lasso
                     family = "binomial")

# 評估每個模型的 cvm(mean cross-validated error)後
# 取最小 cvm 模型所對應的 lambda
best.lambda = cv.lasso$lambda.min
best.lambda

plot(lasso, xvar='lambda', main="Lasso")
abline(v=log(best.lambda), col="blue", lty=5.5 )
# 觀察哪些變數被挑選出來，其係數不為 0的那些
coef(cv.lasso, s = "lambda.min")
select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 # remove `Intercept` and 平移剩下的ind
select.ind # 第幾個變數是重要的 (不看 `Intercept`)
select.varialbes = colnames(chem)[select.ind]
select.varialbes
lasso.lm <- glm(level ~ ., family = 'binomial',data = chem_test[, c(select.varialbes, "level")])
lasso.test <- predict(lasso.lm,chem_test)
cross_validation(lasso.test,ytest)

# 經由 cv 的手法，評估每個模型在不同 lambda 下 
# 的 cvm(mean cross-validated error)
cv.ridge = cv.glmnet(x = as.matrix(chem[, -14]), 
                     y = chem[, 14], 
                     alpha = 0,  # lasso
                     family = "binomial")

# 評估每個模型的 cvm(mean cross-validated error)後
# 取最小 cvm 模型所對應的 lambda
best.lambda = cv.ridge$lambda.min
best.lambda

plot(lasso, xvar='lambda', main="Ridge")
abline(v=log(best.lambda), col="blue", lty=5.5 )
# 觀察哪些變數被挑選出來，其係數不為 0的那些
coef(cv.lasso, s = "lambda.min")
select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 # remove `Intercept` and 平移剩下的ind
select.ind # 第幾個變數是重要的 (不看 `Intercept`)
select.varialbes = colnames(chem)[select.ind]
select.varialbes
ridge.lm <- glm(level ~ ., family = 'binomial',data = chem_train[, c(select.varialbes, "level")])
ridge.test <- predict(ridge.lm,chem_test)
cross_validation(ridge.test,ytest)


=======
123
>>>>>>> 1489e354cecfa72dc6f26594e36adf81d86035ef
