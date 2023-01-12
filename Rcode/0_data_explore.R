

# Note:
# This script contains some of my initial workings, so please referred to the final
# report in the DS_challenge_report.Rmd file which contains the final version 
# scripts. 

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

