## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----load_pkg, message=FALSE--------------------------------------------------------------------------------------------------------------------------
if (!require(tidyverse))
  install.packages("tidyverse")

if (!require(lubridate))
  install.packages("lubridate")

if (!require(plotly))
  install.packages("plotly")

if (!require(car))
  install.packages("stargazer")

if (!require(xgboost))
  install.packages("xgboost")

library(tidyverse)
library(lubridate)
library(stargazer)
library(xgboost)


## ----read_in_market_data, message=FALSE---------------------------------------------------------------------------------------------------------------
market_sale_col <- readr::read_csv("Data/MarketingCols.csv", col_names = FALSE)

market_sale_raw <- readr::read_csv("Data/MarketingSales.csv", 
                                   col_names = market_sale_col$X1)


## ----read_in_dec_data, message=FALSE------------------------------------------------------------------------------------------------------------------
dec_col <- readr::read_csv("Data/DecemberCols.csv", col_names = FALSE)

dec_ad_raw <- readr::read_csv("Data/DecemberAdData.csv", 
                                   col_names = dec_col$X1)


## ----quick_explore------------------------------------------------------------------------------------------------------------------------------------
dim(market_sale_raw)

glimpse(market_sale_raw)

dim(dec_ad_raw)

glimpse(dec_ad_raw)


## ----tidy_market_sale---------------------------------------------------------------------------------------------------------------------------------
market_sale_final <- 
  market_sale_raw %>% 
  mutate(Date = dmy(Date),
         Ad_Spend =  AdvertisingSpend,
         Phone_24 = factor(`0508Line_247`),
         Positive_News = factor(PositiveNews),
         Negative_News = factor(NegativeCoverage),
         Competition = factor(Competition),
         Ultra_Edition = factor( UltraEdition_Available), 
         COVID_Lockdown = factor(COVID_Lockdown),
         Year = factor(year(Date)),
         Month = factor(Month, levels = month.name),
         Day = factor(Day, levels =
                        c("Monday", "Tuesday", "Wednesday", "Thursday",
                          "Friday", "Saturday", "Sunday")),
         Month_num = as.numeric(Month),
         Year_num = year(Date),
         Day_num = as.numeric(Day),
         Date_num = date(Date)) %>% 
  select(Sales, Ad_Spend, Date, Phone_24, Positive_News, Negative_News, 
         Competition, Ultra_Edition, COVID_Lockdown,
         Year, Month, Day, Day_num, Date_num, Month_num, Year_num)

dim(market_sale_final)

glimpse(market_sale_final)


## ----tidy_dec_ad--------------------------------------------------------------------------------------------------------------------------------------
dec_ad_final <-
  dec_ad_raw %>%
  mutate(
    Date = dmy(Date),
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
    Date_num = date(Date)
  ) %>% 
  select(Ad_Spend, Date, Phone_24, Positive_News, Negative_News, 
         Competition, Ultra_Edition, 
         COVID_Lockdown, Year, Month, Day, Day_num, Date_num, Month_num, Year_num)

dim(dec_ad_final)

glimpse(dec_ad_final)


## ---- fig.width=12------------------------------------------------------------------------------------------------------------------------------------
ggplot(market_sale_final, aes(x = Date, y = Sales)) +
  geom_path() +
  scale_x_date(date_breaks = "6 month", expand = c(0.01, 0.01), 
                 date_labels = "%b %Y") + 
  theme_light()



## ---- fig.width=12------------------------------------------------------------------------------------------------------------------------------------
ggplot(market_sale_final, aes(x = Ad_Spend, y = Sales)) +
  geom_point()


## ---- fig.width=12------------------------------------------------------------------------------------------------------------------------------------
ggplot(market_sale_final, aes(x = log(Ad_Spend +1), y = Sales)) +
  geom_point()


## ---- fig.width=12------------------------------------------------------------------------------------------------------------------------------------
ggplot(market_sale_final, aes(x = Date)) +
  geom_line(aes(y = Sales), linewidth = 1.5, color = "red", alpha = 0.8) + 
  geom_line(aes(y = Ad_Spend/100), color = "blue", alpha = 0.8) + 
  scale_y_continuous(
    name = "The units sold",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*100, name="Total Advertising Spend ($)")
  ) +
  scale_x_date(date_breaks = "6 month", expand = c(0.01, 0.01), 
                 date_labels = "%b %Y") + 
  theme_light() +
  theme(
    axis.title.y = element_text(color = "red", size=13),
    axis.title.y.right = element_text(color = "blue", size=13)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
hist(market_sale_final$Sales)


## ---- message=FALSE-----------------------------------------------------------------------------------------------------------------------------------
market_sale_final %>%
  group_by(Year) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales)) %>% 
  knitr::kable()


## ---- message=FALSE-----------------------------------------------------------------------------------------------------------------------------------

market_sale_final %>%
  group_by(Month) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))%>% 
  knitr::kable()


## ---- message=FALSE-----------------------------------------------------------------------------------------------------------------------------------
market_sale_final %>%
  group_by(Day) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))%>% 
  knitr::kable()


## ---- message=FALSE-----------------------------------------------------------------------------------------------------------------------------------
market_sale_final %>%
  group_by(Ultra_Edition) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))%>% 
  knitr::kable()


## -----------------------------------------------------------------------------------------------------------------------------------------------------

market_sale_final %>%
  group_by(Positive_News) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales)) %>% 
  knitr::kable()


## -----------------------------------------------------------------------------------------------------------------------------------------------------
market_sale_final %>%
  group_by(Negative_News) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales)) %>% 
  knitr::kable()


## -----------------------------------------------------------------------------------------------------------------------------------------------------
market_sale_final %>%
  group_by(Competition)  %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))%>% 
  knitr::kable()


## -----------------------------------------------------------------------------------------------------------------------------------------------------
market_sale_final %>%
  group_by(COVID_Lockdown) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))%>% 
  knitr::kable()


## -----------------------------------------------------------------------------------------------------------------------------------------------------
market_sale_final %>%
  group_by(Phone_24) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))%>% 
  knitr::kable()


## -----------------------------------------------------------------------------------------------------------------------------------------------------
linear_reg_fit <-
  lm(
    Sales ~ Ad_Spend + (Year_num * Month) / Day +
      COVID_Lockdown + Ultra_Edition + Competition + Phone_24 + 
      Positive_News + Negative_News,
    data = market_sale_final
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
anova(linear_reg_fit)


## -----------------------------------------------------------------------------------------------------------------------------------------------------

linear_reg_fit_final <- step(linear_reg_fit) 



## -----------------------------------------------------------------------------------------------------------------------------------------------------
anova(linear_reg_fit_final) 


## -----------------------------------------------------------------------------------------------------------------------------------------------------
hist(linear_reg_fit_final$residuals)

plot(fitted(linear_reg_fit_final), resid(linear_reg_fit_final) )

# calculate the mean squared error
mse_linear <- 
  sum(market_sale_final$Sales - 
  predict.glm(linear_reg_fit_final,
           newdata = market_sale_final, 
           type = "response") ) ^2


## -----------------------------------------------------------------------------------------------------------------------------------------------------
summary(linear_reg_fit_final)  

sum_stats <- summary(linear_reg_fit_final)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
poisson_reg_fit <-
  glm(
   Sales ~ Ad_Spend + (Year_num * Month) / Day +
      COVID_Lockdown + Ultra_Edition + Competition + Phone_24 + 
      Positive_News + Negative_News,
    data = market_sale_final,
    family = poisson(link = "identity")
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
car::Anova(poisson_reg_fit, type = 3) 


## -----------------------------------------------------------------------------------------------------------------------------------------------------
poisson_reg_fit_final <- step(poisson_reg_fit)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
car::Anova(poisson_reg_fit_final, type = 3)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
hist(poisson_reg_fit_final$residuals)

plot(fitted(poisson_reg_fit_final), resid(poisson_reg_fit_final) )

# calculate the mean squared error
mse_poisson <- 
  sum(market_sale_final$Sales - 
  predict.glm(poisson_reg_fit_final,
           newdata = market_sale_final, 
           type = "response") ) ^2


## -----------------------------------------------------------------------------------------------------------------------------------------------------
summary(poisson_reg_fit_final)  

sum_stats <- summary(poisson_reg_fit_final)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
poisson_reg_predict <- 
  predict.glm(poisson_reg_fit_final,
           newdata = dec_ad_final, 
           type = "response",
           se.fit = TRUE) 

dec_ad_final$Sales <- round(poisson_reg_predict$fit)

dec_ad_final$Sales_max <- with(poisson_reg_predict, fit + 1.96*se.fit) |> round()
dec_ad_final$Sales_min <- with(poisson_reg_predict, fit - 1.96*se.fit) |> round()


## -----------------------------------------------------------------------------------------------------------------------------------------------------
dec_ad_final %>% 
  select(Date, Sales, Sales_min, Sales_max) %>% 
  knitr::kable()


## ---- fig.width = 12----------------------------------------------------------------------------------------------------------------------------------
market_sale_final$Type <- "Historical"
dec_ad_final$Type <- "Forecast"

market_sale_final$Sales_max <- 
  market_sale_final$Sales_min <- 
  market_sale_final$Sales

g <- 
  market_sale_final %>% 
  bind_rows(dec_ad_final) %>% 
  mutate(Type = factor(Type)) %>% 
  ggplot(aes(x = Date, y = Sales, col = Type, group = Type)) +
  geom_path() +
  geom_ribbon(aes(ymin = Sales_min, ymax = Sales_max, fill = Type), alpha = 0.2) +
  scale_x_date(date_breaks = "6 month", expand = c(0.01, 0.01), 
                 date_labels = "%b %Y") + 
  theme_light()


plotly::ggplotly(g) %>% plotly::hide_legend()

