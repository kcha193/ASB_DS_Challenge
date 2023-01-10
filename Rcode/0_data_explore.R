
# This script is to do some initial data cleaning and data exploration

# Install some require R packages -----------------------------------------

if(!require(tidyverse))
  install.packages("tidyverse")

if(!require(lubridate))
  install.packages("lubridate")

if(!require(s20x))
  install.packages("s20x")

library(tidyverse)
library(lubridate)



# Importing Data ----------------------------------------------------------

# First import the Marketsale data 

market_sale_col <- readr::read_csv("Data/MarketingCols.csv", col_names = FALSE)

market_sale_dat <- readr::read_csv("Data/MarketingSales.csv", 
                                   col_names = market_sale_col$X1)

dec_col <- readr::read_csv("Data/DecemberCols.csv", col_names = FALSE)

dec_ad_dat <- readr::read_csv("Data/DecemberAdData.csv", 
                                   col_names = dec_col$X1)



# Quick explore the dataset

dim(market_sale_dat)

glimpse(market_sale_dat)


market_sale_dat_final <- 
  market_sale_dat %>% 
  mutate(Date = dmy(Date),
         Phone24 = factor(`0508Line_247`),
         PositiveNews = factor(PositiveNews),
         NegativeCoverage = factor(NegativeCoverage),
         Competition = factor(Competition),
         UltraEdition_Available = factor( UltraEdition_Available), 
         COVID_Lockdown = factor(COVID_Lockdown),
         Year = year(Date),
         Month = as.numeric(factor(Month, levels = month.name)),
         Day = factor(Day, levels =
                        c("Monday", "Tuesday", "Wednesday", "Thursday",
                          "Friday", "Saturday", "Sunday"))) %>% 
  select(-`0508Line_247`)


# Generate some plots

ggplot(market_sale_dat_final, aes(x = Date, y = Sales)) +
  geom_path()

# There is clearing some seasonality on the sale versus time for adjustment

ggplot(market_sale_dat_final, aes(x = AdvertisingSpend, y = Sales)) +
  geom_point()

with(market_sale_dat_final, cor(AdvertisingSpend, Sales))

ggplot(market_sale_dat_final, aes(x = log(AdvertisingSpend), y = Sales)) +
  geom_point()


# There is some positive correlation between AdvertisingSpend and number of Sales


hist(market_sale_dat_final$Sales)


# Descriptive summary -----------------------------------------------------

market_sale_dat_final %>%
  group_by(Year) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))


market_sale_dat_final %>%
  group_by(Month) %>%
  summarise(Sales_mean = mean(Sales),
            Sales_sum = sum(Sales))

market_sale_dat_final %>%
  group_by(Year, Month) %>%
  summarise(
    AdSpend_mean = mean(AdvertisingSpend),
    AdSpend_sum = sum(AdvertisingSpend),
    Sales_mean = mean(Sales),
            Sales_sum = sum(Sales)) %>% 
  print(n = Inf)

dec_ad_dat_final$AdvertisingSpend |> mean()



market_sale_dat_final %>%
  group_by(UltraEdition_Available) %>%
  summarise(Sales = mean(Sales))

market_sale_dat_final %>%
  group_by(PositiveNews) %>%
  summarise(Sales = mean(Sales))
  
market_sale_dat_final %>%
  group_by(NegativeCoverage) %>%
  summarise(Sales = mean(Sales))

market_sale_dat_final %>%
  group_by(PositiveNews, NegativeCoverage) %>%
  summarise(Sales = mean(Sales))

market_sale_dat_final %>%
  group_by(Year, Month, Competition) %>%
  summarise(Sales = mean(Sales)) %>% 
  print(n = Inf)

market_sale_dat_final %>%
  group_by(COVID_Lockdown) %>%
  summarise(Sales = mean(Sales))

market_sale_dat_final %>%
  group_by(UltraEdition_Available) %>%
  summarise(Sales = mean(Sales))


# Modelling ---------------------



linear_reg_fit <-
  lm(
    Sales ~ AdvertisingSpend + UltraEdition_Available +
      PositiveNews + NegativeCoverage + Competition +
      COVID_Lockdown + Phone24 + Year / Month / Day,
    data = market_sale_dat_final
  )

anova(linear_reg_fit)

linear_reg_fit_final <- step(linear_reg_fit) # Choose a model by AIC in a Stepwise Algorithm

linear_reg_fit_final$anova

# only Year:Month:Day variable is not needed in the modelling 

summary(linear_reg_fit_final)
# This model explains 83.45% of the information


hist(linear_reg_fit_final$residuals)

plot(fitted(linear_reg_fit_final), resid(linear_reg_fit_final) )


# Prediction --------------------------------------------------------------


dec_ad_dat_final <-
  dec_ad_dat %>%
  mutate(
    PositiveNews = factor(0, levels = c(0, 1)),
    NegativeCoverage = factor(0, levels = c(0, 1)),
    COVID_Lockdown = factor(0, levels = c(0, 1)),
    Year = 2020,
    Month = 12,
    UltraEdition_Available = factor(1, levels = c(0, 1)),
    Phone24 = factor(0, levels = c(0, 1)),
    Competition = factor(0, levels = c(0, 1))
  )
  
predict.lm(linear_reg_fit_final,
           newdata = dec_ad_dat_final, 
           type = "response") |> sum()




fit <- lm(Sales ~ AdvertisingSpend + PositiveNews + NegativeCoverage + Competition + 
            UltraEdition_Available+ COVID_Lockdown + Phone24 + Month, data = market_sale_dat_final)
anova(fit)

fit <- glm(Sales ~ AdvertisingSpend + PositiveNews + NegativeCoverage + Competition + 
            UltraEdition_Available+ COVID_Lockdown + Phone24 +  Year/Month/Day,
           data = market_sale_dat_final,
           family = poisson())

car::Anova(fit)

fit_final <- step(fit)



car::Anova(fit_final)


predict.glm(fit_final,
           newdata = dec_ad_dat_final, 
           type = "response") |> sum()








































