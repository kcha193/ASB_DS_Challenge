
# This script is to do some initial data cleaning and data exploration

# Install some require R packages -----------------------------------------

if (!require(tidyverse))
  install.packages("tidyverse")

if (!require(lubridate))
  install.packages("lubridate")

if (!require(s20x))
  install.packages("s20x")

if (!require(xgboost))
  install.packages("xgboost")

library(tidyverse)
library(lubridate)

# Importing Data ----------------------------------------------------------

# First import the Marketsale data 

market_sale_col <- readr::read_csv("Data/MarketingCols.csv", col_names = FALSE)

market_sale_raw <- readr::read_csv("Data/MarketingSales.csv", 
                                   col_names = market_sale_col$X1)

# Then import the December data for prediction 

dec_col <- readr::read_csv("Data/DecemberCols.csv", col_names = FALSE)

dec_ad_raw <- readr::read_csv("Data/DecemberAdData.csv", 
                                   col_names = dec_col$X1)

# Quick explore the dataset

dim(market_sale_raw)

glimpse(market_sale_raw)

# Tidy up the variables of both datasets

market_sale_final <- 
  market_sale_raw %>% 
  mutate(Date_raw = dmy(Date),
         Ad_Spend =  AdvertisingSpend,
         Phone_24 = factor(`0508Line_247`),
         Positive_News = factor(PositiveNews),
         Negative_News = factor(NegativeCoverage),
         Competition = factor(Competition),
         Ultra_Edition = factor( UltraEdition_Available), 
         COVID_Lockdown = factor(COVID_Lockdown),
         Year = factor(year(Date_raw)),
         Month = factor(Month, levels = month.name),
         Day = factor(Day, levels =
                        c("Monday", "Tuesday", "Wednesday", "Thursday",
                          "Friday", "Saturday", "Sunday")),
         Month_num = as.numeric(Month),
         Year_num = year(Date_raw),
         Day_num = as.numeric(Day),
         Date_num = date(Date_raw)) %>% 
  select(Sales, Ad_Spend, Date_raw, Phone_24, Positive_News, Negative_News, 
         Competition, Ultra_Edition, COVID_Lockdown,
         Year, Month, Day, Day_num, Date_num, Month_num, Year_num)

dim(market_sale_final)

glimpse(market_sale_final)

dec_ad_final <-
  dec_ad_raw %>%
  mutate(
    Date_raw = dmy(Date),
    Ad_Spend =  AdvertisingSpend,
    Phone_24 = factor(0, levels = c(0, 1)),
    Positive_News = factor(0, levels = c(0, 1)),
    Negative_News = factor(0, levels = c(0, 1)),
    Competition  = factor(0, levels = c(0, 1)),
    Ultra_Edition = factor(1, levels = c(0, 1)),
    COVID_Lockdown = factor(0, levels = c(0, 1)),
    Year = factor(2020, levels = market_sale_final$Year |> levels() ),
    Month = factor(Month, levels = month.name),
    Day = factor(Day, levels =
                   c("Monday", "Tuesday", "Wednesday", "Thursday",
                        "Friday", "Saturday", "Sunday")),
    Year_num = 2020,
    Month_num = 12,
    Day_num = as.numeric(Day),
    Date_num = date(Date_raw)
  ) %>% 
  select(Ad_Spend, Date_raw, Phone_24, Positive_News, Negative_News, 
         Competition, Ultra_Edition, 
         COVID_Lockdown, Year, Month, Day, Day_num, Date_num, Month_num, Year_num)

dim(dec_ad_final)

glimpse(dec_ad_final)

# Data exploration --------------------------------------------------------

# Generate some plots

ggplot(market_sale_final, aes(x = Date_raw, y = Sales)) +
  geom_path()




# There is clearing some seasonality on the sale versus time for adjustment

ggplot(market_sale_final, aes(x = Ad_Spend, y = Sales)) +
  geom_point()

with(market_sale_final, cor(Ad_Spend, Sales))

# There is a weak positive correlation between Advertising Spend and number of Sales


ggplot(market_sale_final, aes(x = Date_raw)) +
  geom_line(aes(y = Sales), linewidth = 1.5, color = "red", alpha = 0.8) + 
  geom_line(aes(y = Ad_Spend/100), color = "blue", alpha = 0.8) + 
  scale_y_continuous(
    name = "The units sold",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*100, name="Total Advertising Spend ($)")
  ) +
    scale_x_date(date_breaks = "3 month", expand = c(0.01, 0.01), 
                 date_labels = "%b %Y") + 
  theme_light() +
  theme(
    axis.title.y = element_text(color = "red", size=13),
    axis.title.y.right = element_text(color = "blue", size=13)
  )

hist(market_sale_final$Sales)

# The overall number of Sales is looking reasonably normal, thus there is no need to 
# apply any additional normalization methods. 

summary(market_sale_final$Ad_Spend)


# Descriptive summary -----------------------------------------------------

market_sale_final %>%
  group_by(Year) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))


market_sale_final %>%
  group_by(Month) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))


market_sale_final %>%
  group_by(Day) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))

market_sale_final %>%
  group_by(Year, Month) %>%
  summarise(
    AdSpend_mean = mean(Ad_Spend),
    AdSpend_sum = sum(Ad_Spend),
    Sales_mean = mean(Sales),
    Sales_sum = sum(Sales)
  ) %>%
  print(n = Inf)

dec_ad_final$Ad_Spend |> mean()


market_sale_final %>%
  group_by(Ultra_Edition) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))


market_sale_final %>%
  group_by(Positive_News) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))

market_sale_final %>%
  group_by(Negative_News) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))

market_sale_final %>%
  group_by(PositiveNews, NegativeCoverage) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))

market_sale_final %>%
  group_by(Competition)  %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))

market_sale_final %>%
  group_by(COVID_Lockdown) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))

market_sale_final %>%
  group_by(Phone_24) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))

# Modelling using linear regression ---------------------

# Fitting the model

linear_reg_fit <-
  lm(
    Sales ~ Ad_Spend + (Year * Month) / Day +
      COVID_Lockdown + Ultra_Edition + Phone_24 + 
      Positive_News + Negative_News + Competition,
    data = market_sale_final
  )

anova(linear_reg_fit)

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



market_sale_final_mat <- 
  market_sale_final %>% 
    select(Sales, Ad_Spend, Phone_24, Positive_News, Negative_News, Competition,
           Ultra_Edition, COVID_Lockdown, Year_num, Month_num, Day_num) %>% 
    mutate(Year_month = paste0(Year_num, Month_num)) %>%
    as.matrix() 

market_sale_final_mat <- 
  apply(market_sale_final_mat, 2, as.numeric)

# split the data into training and test sets
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
       metrics = "rmse", verbose = TRUE, 
       seed = 1, 
       maximize = FALSE, 
       param_comb = gridsearch_params)

# update the model parameters with the optimal values
params$max_depth <- cv_result$best_param["max_depth"]
params$min_child_weight <- cv_result$best_param["min_child_weight"]

# train the final model using the optimal parameters and number of rounds
xgboostModel <- xgb.train(params, dtrain, nrounds = 10)

imp <- xgb.importance(colnames(train[,-1]), model = xgboostModel)
print(imp)


# make predictions on the test set
predictions <- predict(xgboostModel, dtrain)

# calculate the mean squared error of the predictions
mse_xgboost <- mean((train[, "Sales"] - predictions)^2)
mse_xgboost


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











