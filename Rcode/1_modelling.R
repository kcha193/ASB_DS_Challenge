

# Note:
# This script contains some of my initial workings, so please referred to the final
# report in the DS_challenge_report.Rmd file which contains the final version 
# scripts. 
# 
# Modelling using linear regression ---------------------

# Fitting the model

linear_reg_fit <-
  lm(
    Sales ~ Ad_Spend + (Year* Month) / Day +
      COVID_Lockdown + Ultra_Edition + Competition + Phone_24 + 
      Positive_News + Negative_News,
    data = market_sale_final
  )

anova(linear_reg_fit)
summary(linear_reg_fit)

linear_reg_fit_final <- step(linear_reg_fit) 
# Choose a model by AIC in a Stepwise Algorithm

linear_reg_fit_final$anova

anova(linear_reg_fit_final)

summary(linear_reg_fit_final)
# This model explains 94.51% of the information

AIC(linear_reg_fit_final)

hist(linear_reg_fit_final$residuals)

plot(fitted(linear_reg_fit_final), resid(linear_reg_fit_final) )


mse_linear <- 
  sum(market_sale_final$Sales - 
  predict.glm(linear_reg_fit_final,
           newdata = market_sale_final, 
           type = "response") ) ^2

linear_reg_fit <-
  lm(
    Sales ~ Ad_Spend + (Year_num* Month) / Day +
       COVID_Lockdown + Ultra_Edition + Competition + Phone_24 + 
      Positive_News + Negative_News,
    data = market_sale_final
  )

anova(linear_reg_fit)
summary(linear_reg_fit)

linear_reg_fit_final <- step(linear_reg_fit) 
# Choose a model by AIC in a Stepwise Algorithm

linear_reg_fit_final$anova

anova(linear_reg_fit_final)

summary(linear_reg_fit_final)

linear_reg_fit_final <-
  lm(
    Sales ~ Ad_Spend + (Year_num* Month) / Day +
       COVID_Lockdown + Phone_24 + 
      Positive_News + Negative_News,
    data = market_sale_final
  )


AIC(linear_reg_fit_final)

hist(linear_reg_fit_final$residuals)

plot(fitted(linear_reg_fit_final), resid(linear_reg_fit_final) )


mse_linear <- 
  sum(market_sale_final$Sales - 
  predict.glm(linear_reg_fit_final,
           newdata = market_sale_final, 
           type = "response") ) ^2


# Prediction 

linear_reg_predict <- 
  predict.lm(linear_reg_fit_final,
           newdata = dec_ad_final, 
           type = "response",
           se.fit = TRUE) 

linear_reg_predict$fit |> sum()


with(linear_reg_predict, 
     fit + 1.96*se.fit) |> sum()


# Modelling using Poisson regression ---------------------

# Fitting the model

poisson_reg_fit <-
  glm(
    Sales ~ Ad_Spend + COVID_Lockdown + Ultra_Edition + Phone_24 + 
      Positive_News + Negative_News + Competition + (Year* Month) / Day,
    data = market_sale_final,
    family = poisson()
)

car::Anova(poisson_reg_fit)

poisson_reg_fit_final <- step(poisson_reg_fit)

car::Anova(poisson_reg_fit_final)

summary(poisson_reg_fit_final)

AIC(poisson_reg_fit_final)

# Prediction 

# calculate the mean squared error of the predictions
mse_poisson <- 
  sum(market_sale_final$Sales - 
  predict.glm(poisson_reg_fit_final,
           newdata = market_sale_final, 
           type = "response") ) ^2



# Modelling using XGBoost ---------------------

# Below is my testing using the XGBoost method, however, it was not included in the 
# final report, because the model performance was not as good as linear regression model. 

# load the library
library(xgboost)


market_sale_final_mat <- 
  market_sale_final %>% 
    select(Sales, Ad_Spend, Phone_24, Positive_News, Negative_News, Competition,
           Ultra_Edition, COVID_Lockdown, Year_num, Month_num, Day_num) %>% 
    mutate(Year_month = paste0(Year_num, Month_num)) %>%
    as.matrix() 

market_sale_final_mat <- 
  apply(market_sale_final_mat, 2, as.numeric)

# split the data into training and test sets
train <- market_sale_final_mat[market_sale_final_mat[, "Year_num"] != c(2019, 2020),]
test <-  market_sale_final_mat[market_sale_final_mat[, "Year_num"] == c(2019, 2020),]

# create DMatrix objects for training and test sets
dtrain <- xgb.DMatrix(data = train[,-1],  label = train[, "Sales"])
dtest <- xgb.DMatrix(data = test[,-1])

# specify the model parameters
params <- list(booster = "gbtree", objective = "reg:squarederror", 
               eta = 0.3, max_depth = 3,subsample = 0.8,
               colsample_bytree = 0.8, nthread = 2)


#tuning hyperparameter
gridsearch_params <- list(max_depth = c(2,4,6),min_child_weight = c(1,3,5))
cv_result <- 
  xgb.cv(params = params, data = dtrain, nfold = 5, nrounds = 100,
       num_boost_round = 100, early_stopping_rounds = 10, 
       metrics = "rmse", verbose = TRUE, 
       seed = 1, 
       maximize = FALSE, 
       param_comb = gridsearch_params)

# update the model parameters with the optimal values
params$max_depth <- cv_result$best_param["max_depth"]
params$min_child_weight <- cv_result$best_param["min_child_weight"]

# train the final model using the optimal parameters and number of rounds
xgboostModel <- xgb.train(params, dtrain, nrounds = 10)

# make predictions on the test set
predictions <- predict(xgboostModel, dtest)

# calculate the mean squared error of the predictions
mse_xgboost <- mean((test[, "Sales"] - predictions)^2)
mse_xgboost

imp <- xgb.importance(colnames(train[,-1]), model = xgboostModel)
print(imp)




## Modelling using eXtreme Gradient Boosting (XGBoost)

# Finally, I will also use the XGBoost model to see if we can obtain a model
#  with an even smaller mean square error to ensure the total sales prediction 
#  is accurate and robust. When prepping the data for the model training below, 
#  I did not split the data into training and testing. This is because the two models 
#  above both compute the mean square error with the training data set only. Thus, 
#  I would not split the testing dataset to consistently compare the mean square 
#  errors between different models.  

market_sale_final_mat <- 
  market_sale_final %>% 
    select(Sales, Ad_Spend, Phone_24, Positive_News, Negative_News, Competition,
           Ultra_Edition, COVID_Lockdown, Year_num, Month_num, Day_num) %>% 
    mutate(Year_month = paste0(Year_num, Month_num)) %>%
    as.matrix() 

market_sale_final_mat <- 
  apply(market_sale_final_mat, 2, as.numeric)

train <- market_sale_final_mat

# create DMatrix objects for training and test sets
dtrain <- xgb.DMatrix(data = train[,-1],  label = train[, "Sales"])

# specify the model parameters
params <- list(booster = "gbtree", objective = "reg:squarederror", 
               eta = 0.3, max_depth = 3,subsample = 0.8,
               colsample_bytree = 0.8, nthread = 2)

#tuning hyperparameter
gridsearch_params <- list(max_depth = c(2,4,6),min_child_weight = c(1,3,5))
cv_result <- 
  xgb.cv(params = params, data = dtrain, nfold = 5, nrounds = 100,
       num_boost_round = 100, early_stopping_rounds = 10, 
       metrics = "mse", verbose = TRUE, 
       seed = 1, 
       maximize = FALSE, 
       param_comb = gridsearch_params)

# update the model parameters with the optimal values
params$max_depth <- cv_result$best_param["max_depth"]
params$min_child_weight <- cv_result$best_param["min_child_weight"]

# train the final model using the optimal parameters and number of rounds
xgboostModel <- xgb.train(params, dtrain, nrounds = 10)



# Feature Importance of the model


xgb.importance(colnames(train[,-1]), model = xgboostModel)

predictions <- predict(xgboostModel, dtrain)

# calculate the mean squared error of the predictions
mse_xgboost <- mean((train[, "Sales"] - predictions)^2)


# The mean square error of this machine learning model using XGboost is `r `mse_xgboost`, 
# which much higher than the linear regession model of `r mse_linear`. Therefore, 
# I will use the multiple linear regression above for the making the final prediction. 

# Actual forecast 

dec_ad_final_mat <- 
  dec_ad_final %>% 
    select(Ad_Spend, Phone_24, Positive_News, Negative_News, Competition,
           Ultra_Edition, COVID_Lockdown, Year_num, Month_num, Day_num) %>% 
    mutate(Year_month = paste0(Year_num, Month_num)) %>%
    as.matrix() |>
    apply(2, as.numeric)



xgboost_predict <- 
  predict(xgboostModel, dec_ad_final_mat)

xgboost_predict_SE <- 
  predict(xgboostModel, dec_ad_final_mat, predleaf = TRUE)

SE <- sqrt(apply(xgboost_predict, 1, var))

# print the predictions and standard errors
print(cbind(xgboost_predict, SE))
print(cbind(linear_reg_predict$fit, linear_reg_predict$se.fit))

# Finalising the results 


sum(linear_reg_predict$fit)

sum(xgboost_predict)






