# Libraries
library(quantmod)
library(lubridate)
library(TTR)
library(ggplot2)
library(reshape)
library(dplyr)
library(tidyquant)
library(rvest)
library(purrr)
library(tidyr)

# Variables
history <- 5 # Years of historical data

# Access to the database with all the tickets in SP500
SP500 <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table() %>%
  pluck(1)

# Fixing some tickets to be downloaded from yahoo finance
SP500[SP500$Symbol == "BRK.B", "Symbol"] <- "BRK-B"
SP500[SP500$Symbol == "BF.B", "Symbol"]  <- "BF-B"

# Download historical price data ("X" days) 
mult_stocks <- tq_get(SP500$Symbol,
                      get  = "stock.prices",
                      from = Sys.Date() - years(history),
                      to   = Sys.Date(),
                      complete_cases = TRUE) %>%
  select(symbol, date, adjusted) %>%
  spread(key = symbol, value = adjusted) %>%
  as.data.frame()

# Remove the columns with NAs
mult_stocks <- mult_stocks %>% select_if(~ !any(is.na(.)))

# Moving Average Calculation
SMA50  <- data.frame(matrix(data = 0, nrow = nrow(mult_stocks), ncol = ncol(mult_stocks) - 1))
SMA100 <- data.frame(matrix(data = 0, nrow = nrow(mult_stocks), ncol = ncol(mult_stocks) - 1))
SMA200 <- data.frame(matrix(data = 0, nrow = nrow(mult_stocks), ncol = ncol(mult_stocks) - 1))

for(i in 2:ncol(mult_stocks)){ # i <- 1
  
  SMA50[,i]  <- ifelse(mult_stocks[,i] > SMA(mult_stocks[,i], n = 50), 1, 0)
  SMA100[,i] <- ifelse(mult_stocks[,i] > SMA(mult_stocks[,i], n = 100), 1, 0)
  SMA200[,i] <- ifelse(mult_stocks[,i] > SMA(mult_stocks[,i], n = 200), 1, 0)

  print(paste("Progress... ", round(i/(ncol(mult_stocks) - 1)*100, digits = 2), "%", sep = ""))
}

# Results
SMA50  <- rowSums(SMA50)/ncol(SMA50)
SMA100 <- rowSums(SMA100)/ncol(SMA100)
SMA200 <- rowSums(SMA200)/ncol(SMA200)

# Create the final Data Base with all the info
finalDDBB <- data.frame(SMA50, SMA100, SMA200) %>%
  mutate(Dates = mult_stocks$date %>% as.Date(format = "%Y-%m-%d")) %>%
  na.omit() %>%
  `colnames<-`(c("Above SMA 50", "Above SMA 100", "Above SMA 200", "Dates")) %>%
  melt(id = "Dates") %>%
  `colnames<-`(c("Dates", "Momentum", "Value")) %>%
  mutate(Value = round(Value*100, digits = 2))

# Plot
LastDate    <- finalDDBB$Dates[nrow(finalDDBB)]
LastValues  <- finalDDBB[finalDDBB$Dates == LastDate, ]


ggplot(data=finalDDBB, aes(x = Dates, y = Value/100, colour = Momentum)) +
  geom_line(size = 1) +
  scale_color_viridis_d(end = 0.9)+
  theme_minimal() +
  labs(title = "SP500 - Momentum Analysis (Percentage of companies above their Simple Moving Average)",
       subtitle = paste("Above SMA50: ", LastValues$Value[1], "%. Above SMA100: ", LastValues$Value[2], "%.Above SMA200: ", LastValues$Value[3], "%", sep = ""),
       caption = "By: Carlos Jimenez (@cjimenezdiaz)\nSource: Yahoo Finance",
       y = "Percentage of Companies",
       x = "Dates") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom")
