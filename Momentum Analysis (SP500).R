# Libraries
library(tidyquant)
library(tidyverse)
library(scales)

# Variables
history   <- 15 # Years of historical data

# Creating the DB with all the information that we need
DB_Prices <- tq_index("SP500") %>% 
  
  # Fixing some symbols for Yahoo Finance
  dplyr::mutate(symbol = ifelse(symbol == "BRK.B", "BRK-B",
                                ifelse(symbol == "BF.B", "BF-B", symbol))) %>%
  dplyr::select(symbol) %>%
  pull(1) %>%
  tq_get(get  = "stock.prices",
         from = Sys.Date() - years(history),
         to   = Sys.Date(),
         complete_cases = TRUE) %>%
  dplyr::select(symbol, date, adjusted) %>%
  dplyr::group_by(symbol) %>%
  dplyr::mutate(MA_50  = ifelse(adjusted > TTR::SMA(adjusted, n = 50), 1, 0),
                MA_100 = ifelse(adjusted > TTR::SMA(adjusted, n = 100), 1, 0),
                MA_200 = ifelse(adjusted > TTR::SMA(adjusted, n = 200), 1, 0)) %>%
  dplyr::ungroup() %>%
  drop_na()


# Splitting the dataframes into the types of Moving Average
DB_MA_50 <- DB_Prices %>%
  dplyr::select(date, MA_50) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(Pct_MA_50 = sum(MA_50)/n()) 

DB_MA_100 <- DB_Prices %>%
  dplyr::select(date, MA_100) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(Pct_MA_100 = sum(MA_100)/n()) 

DB_MA_200 <- DB_Prices %>%
  dplyr::select(date, MA_200) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(Pct_MA_200 = sum(MA_200)/n()) 


# Joining everything together
DB_Global <- DB_MA_50 %>%
  left_join(DB_MA_100, by = "date") %>%
  left_join(DB_MA_200, by = "date") %>%
  pivot_longer(!date, names_to = "Type", values_to = "Percentage")


# Getting the Last value for each category
Last_MA_50 <- DB_Global %>% 
  dplyr::filter(Type == "Pct_MA_50") %>% 
  tail(n = 1) %>%
  dplyr::select(Percentage) %>%
  pull(1) %>%
  percent(accuracy = 0.01)

Last_MA_100 <- DB_Global %>% 
  dplyr::filter(Type == "Pct_MA_100") %>% 
  tail(n = 1) %>%
  dplyr::select(Percentage) %>%
  pull(1) %>%
  percent(accuracy = 0.01)

Last_MA_200 <- DB_Global %>% 
  dplyr::filter(Type == "Pct_MA_200") %>% 
  tail(n = 1) %>%
  dplyr::select(Percentage) %>%
  pull(1) %>%
  percent(accuracy = 0.01)


# Plotting
Num_Years <- 6 # Years in the Chart

  # Using Facets
  DB_Global %>%
    dplyr::filter(date %>% lubridate::year() >= (Sys.Date() %>% lubridate::year()) - Num_Years) %>%
    dplyr::mutate(Type = ifelse(Type == "Pct_MA_50", "Above 50 SMA",
                                ifelse(Type == "Pct_MA_100", "Above 100 SMA", "Above 200 SMA"))) %>%
    ggplot(aes(x = date, y = Percentage, colour = Type)) +
    geom_line(size = 1) +
    scale_color_viridis_d(end = 0.9) +
    theme_minimal() +
    labs(title = "SP500 - Momentum Analysis (Percentage of companies above their Simple Moving Average)",
         subtitle = str_glue("Above SMA50: {Last_MA_50}. Above SMA100: {Last_MA_100}. Above SMA200: {Last_MA_200}"),
         caption = "By: Carlos Jimenez\nSource: Yahoo Finance",
         y       = "Percentage of Companies",
         x       = "Dates") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom",
          legend.title    = element_blank()) +
    facet_wrap( ~ Type,  ncol = 3, strip.position = "left")
  
  # All in the same plot
  DB_Global %>%
    dplyr::filter(date %>% lubridate::year() >= (Sys.Date() %>% lubridate::year()) - Num_Years) %>%
    dplyr::mutate(Type = ifelse(Type == "Pct_MA_50", "Above 50 SMA",
                                ifelse(Type == "Pct_MA_100", "Above 100 SMA", "Above 200 SMA"))) %>%
    ggplot(aes(x = date, y = Percentage, colour = Type)) +
    geom_line(size = 1) +
    scale_color_viridis_d(end = 0.9) +
    theme_minimal() +
    labs(title = "SP500 - Momentum Analysis (Percentage of companies above their Simple Moving Average)",
         subtitle = str_glue("Above SMA50: {Last_MA_50}. Above SMA100: {Last_MA_100}. Above SMA200: {Last_MA_200}"),
         caption = "By: Carlos Jimenez\nSource: Yahoo Finance",
         y       = "Percentage of Companies",
         x       = "Dates") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom",
          legend.title    = element_blank())


