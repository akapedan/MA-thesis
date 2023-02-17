# This code was written by Arbian Halilaj for the Master's Thesis at the
# University of St. Gallen.
# Supervisor: Prof. Jana Mareckova
# For questions contact arbian.halilaj@student.unisg.ch
# Last tested: 24/10/2022
# R Version: 4.0.3

#########################################################################
## Setup 
#########################################################################
# Memory cleaning
rm(list=ls()) 

# Load packages
library(readxl)      # Read excel data
library(dplyr)       # Data manipulation (0.8.0.1)
library(grf)         # Generalized random forests (0.10.2)
library(ggplot2)     # general plotting tool (3.1.0)
library(stargazer)   # LaTeX output
library(hrbrthemes)
library(maps)
library(MatchIt)
library(data.table)
library(lmtest)
library(gt)
library(gtExtras)
library(RColorBrewer)
library(car)
library(estimatr)
library(fastDummies)
#library(viridis)
#library(cowplot)
#library(ggrepel)
#library(tidyverse)

# Working directory
setwd("/Users/arbiun/Desktop/MECONI/MA")

# Load data
green_universe <- read_excel("Data/data.xlsx", sheet = "green universe")
data <- read_excel("Data/data2.xlsx", sheet = "full data (clean sample)")
data_matched <- read_excel("Data/data2.xlsx", sheet = "full data (issuer matched)")
cbi_matched <- read_excel("Data/data2.xlsx", sheet = "full data (CBI flag)")
usdeur_matched <- read_excel("Data/data2.xlsx", sheet = "full data (USDEUR)")

# Set seed
set.seed(18062022)

# Surpress scientific notation
options(scipen=999)

#########################################################################
## Data Preparation
#########################################################################
# Data handling for whole universe data
str(data)

data$`Issue Year` <- data$`Issue Date`
data$`Issue Year` <- as.numeric(format(data$`Issue Year`,'%Y'))
table(data$`Issue Year`)
data$`2008` <- ifelse(data$`Issue Year` == "2008", 1, 0)
data$`2009` <- ifelse(data$`Issue Year` == "2009", 1, 0)
data$`2010` <- ifelse(data$`Issue Year` == "2010", 1, 0)
data$`2011` <- ifelse(data$`Issue Year` == "2011", 1, 0)
data$`2012` <- ifelse(data$`Issue Year` == "2012", 1, 0)
data$`2013` <- ifelse(data$`Issue Year` == "2013", 1, 0)
data$`2014` <- ifelse(data$`Issue Year` == "2014", 1, 0)
data$`2015` <- ifelse(data$`Issue Year` == "2015", 1, 0)
data$`2016` <- ifelse(data$`Issue Year` == "2016", 1, 0)
data$`2017` <- ifelse(data$`Issue Year` == "2017", 1, 0)
data$`2018` <- ifelse(data$`Issue Year` == "2018", 1, 0)
data$`2019` <- ifelse(data$`Issue Year` == "2019", 1, 0)
data$`2020` <- ifelse(data$`Issue Year` == "2020", 1, 0)
data$`2021` <- ifelse(data$`Issue Year` == "2021", 1, 0)
data$`2022` <- ifelse(data$`Issue Year` == "2022", 1, 0)

data$rating <- data$`New Issues Ratings (Moodys)`
table(data$rating)
data <- data%>%
  mutate(rating=case_when(
    rating=="A1" ~ "A",
    rating=="A2" ~ "A",
    rating=="A3" ~ "A",
    rating=="Aa1" ~ "AA",
    rating=="Aa2" ~ "AA",
    rating=="Aa3" ~ "AA",
    rating=="Aaa" ~ "AAA",
    rating=="B1" ~ "B",
    rating=="B2" ~ "B",
    rating=="B3" ~ "B",
    rating=="Ba1" ~ "BB",
    rating=="Ba2" ~ "BB",
    rating=="Ba3" ~ "BB",
    rating=="Baa1" ~ "BBB",
    rating=="Baa2" ~ "BBB",
    rating=="Baa3" ~ "BBB",
    rating=="Caa1" ~ "CCC",
    rating=="Caa2" ~ "CCC",
    rating=="Caa3" ~ "CCC",
  ))

data$`AAA` <- ifelse(data$`rating` == "AAA", 1, 0)
data$`AA` <- ifelse(data$`rating` == "AA", 1, 0)
data$`A` <- ifelse(data$`rating` == "A", 1, 0)
data$`BBB` <- ifelse(data$`rating` == "BBB", 1, 0)
data$`BB` <- ifelse(data$`rating` == "BB", 1, 0)
data$`B` <- ifelse(data$`rating` == "B", 1, 0)
data$`CCC` <- ifelse(data$`rating` == "CCC", 1, 0)

table(data$`Coupon Payment Frequency`)
data$`Annual Coupon` <- ifelse(data$`Coupon Payment Frequency` == "Annual", 1, 0)
data$`Semi Annual Coupon` <- ifelse(data$`Coupon Payment Frequency` == "SemiAnnual", 1, 0)
data$`Quarterly` <- ifelse(data$`Coupon Payment Frequency` == "Quarterly", 1, 0)
data$`Monthly` <- ifelse(data$`Coupon Payment Frequency` == "Monthly", 1, 0)
data$`Variable` <- ifelse(data$`Coupon Payment Frequency` == "Variable", 1, 0)
data$`Maturity` <- ifelse(data$`Coupon Payment Frequency` == "Maturity", 1, 0)

table(data$`TRBC Economic Sector`)
data$`Academic & Educational Services` <- ifelse(data$`TRBC Economic Sector` == "Academic & Educational Services", 1, 0)
data$`Basic Materials` <- ifelse(data$`TRBC Economic Sector` == "Basic Materials", 1, 0)
data$`Consumer Cyclicals` <- ifelse(data$`TRBC Economic Sector` == "Consumer Cyclicals", 1, 0)
data$`Consumer Non-Cyclicals` <- ifelse(data$`TRBC Economic Sector` == "Consumer Non-Cyclicals", 1, 0)
data$`Energy` <- ifelse(data$`TRBC Economic Sector` == "Energy", 1, 0)
data$`Financials` <- ifelse(data$`TRBC Economic Sector` == "Financials", 1, 0)
data$`Government Activity` <- ifelse(data$`TRBC Economic Sector` == "Government Activity", 1, 0)
data$`Healthcare` <- ifelse(data$`TRBC Economic Sector` == "Healthcare", 1, 0)
data$`Industrials` <- ifelse(data$`TRBC Economic Sector` == "Industrials", 1, 0)
data$`Institutions, Associations & Organizations` <- ifelse(data$`TRBC Economic Sector` == "Institutions, Associations & Organizations", 1, 0)
data$`Real Estate` <- ifelse(data$`TRBC Economic Sector` == "Real Estate", 1, 0)
data$`Technology` <- ifelse(data$`TRBC Economic Sector` == "Technology", 1, 0)
data$`Utilities` <- ifelse(data$`TRBC Economic Sector` == "Utilities", 1, 0)

table(data$Currency)
data$`AUD` <- ifelse(data$`Currency` == "Australian Dollar", 1, 0)
data$`BRL` <- ifelse(data$`Currency` == "Brazilian Real", 1, 0)
data$`CAD` <- ifelse(data$`Currency` == "Canadian Dollar", 1, 0)
data$`CLP` <- ifelse(data$`Currency` == "Chilian Peso", 1, 0)
data$`CNY` <- ifelse(data$`Currency` == "Chinese Yuan Renminbi", 1, 0)
data$`COP` <- ifelse(data$`Currency` == "Colombian Peso", 1, 0)
data$`HRK` <- ifelse(data$`Currency` == "Croatia Kuna", 1, 0)
data$`EUR` <- ifelse(data$`Currency` == "Euro", 1, 0)
data$`GBP` <- ifelse(data$`Currency` == "Great Britain Pound", 1, 0)
data$`HKD` <- ifelse(data$`Currency` == "Hong Kong Dollar", 1, 0)
data$`JPY` <- ifelse(data$`Currency` == "Japanese Yen", 1, 0)
data$`KZT` <- ifelse(data$`Currency` == "Kazakhstan Tenge", 1, 0)
data$`MXN` <- ifelse(data$`Currency` == "Mexican Peso", 1, 0)
data$`NZD` <- ifelse(data$`Currency` == "New Zealand Dollar", 1, 0)
data$`NOK` <- ifelse(data$`Currency` == "Norwegian Krone", 1, 0)
data$`PEN` <- ifelse(data$`Currency` == "Peruvian Nuevo Sol", 1, 0)
data$`PHP` <- ifelse(data$`Currency` == "Philippine Peso", 1, 0)
data$`RUB` <- ifelse(data$`Currency` == "Russian New Ruble", 1, 0)
data$`SGD` <- ifelse(data$`Currency` == "Singapore Dollar", 1, 0)
data$`SEK` <- ifelse(data$`Currency` == "Swedish Krona", 1, 0)
data$`CHF` <- ifelse(data$`Currency` == "Swiss Franc", 1, 0)
data$`THB` <- ifelse(data$`Currency` == "Thai Baht", 1, 0)
data$`TRY` <- ifelse(data$`Currency` == "Turkish Lira (New)", 1, 0)
data$`USD` <- ifelse(data$`Currency` == "U.S. Dollar", 1, 0)
data$`UYU` <- ifelse(data$`Currency` == "Uruguayan Peso Uruguayano", 1, 0)

table(data$Seniority)
data$`First-Lien Loan` <- ifelse(data$`Seniority` == "1STLIEN", 1, 0)
data$`First Mortgage` <- ifelse(data$`Seniority` == "1STMTG", 1, 0)
data$`First Refunding Mortgage` <- ifelse(data$`Seniority` == "1STRFMTG", 1, 0)
data$`Second-Lien Loan` <- ifelse(data$`Seniority` == "2NDLIEN", 1, 0)
data$`Junior Subordinated` <- ifelse(data$`Seniority` == "JRSUB", 1, 0)
data$`Senior Secured Mortgage` <- ifelse(data$`Seniority` == "MTG", 1, 0)
data$`Refunding Mortgage` <- ifelse(data$`Seniority` == "REFMTG", 1, 0)
data$`Senior Secured` <- ifelse(data$`Seniority` == "SEC", 1, 0)
data$`Senior Unsecured` <- ifelse(data$`Seniority` == "SR", 1, 0)
data$`Senior Non-Preferred` <- ifelse(data$`Seniority` == "SRBN", 1, 0)
data$`Senior Preferred` <- ifelse(data$`Seniority` == "SRP", 1, 0)
data$`Senior Secured` <- ifelse(data$`Seniority` == "SRSEC", 1, 0)
data$`Senior Subordinated Unsecured` <- ifelse(data$`Seniority` == "SRSUB", 1, 0)
data$`Senior Subordinated Secured` <- ifelse(data$`Seniority` == "SRSUBSEC", 1, 0)
data$`Subordinated Unsecured` <- ifelse(data$`Seniority` == "SUB", 1, 0)
data$`Subordinated Secured` <- ifelse(data$`Seniority` == "SUBSEC", 1, 0)
data$`Unsecured` <- ifelse(data$`Seniority` == "UN", 1, 0)

table(data$`Issue Type`)
data$`Public Sector` <- ifelse(data$`Issue Type` == "Agency, Supranational, Sovereign" | data$`Issue Type` == "Federal Credit Agency", 1, 0)
data$`Corporate Sector` <- ifelse(data$`Issue Type` == "Investment Grade Corporate" | data$`Issue Type` == "Investment Grade Corporate|Emerging Market Corporate" | data$`Issue Type` == "Emerging Market Corporate" | data$`Issue Type` == "Emerging Market Corporate|Investment Grade Corporate" | data$`Issue Type` == "High Yield Corporate", 1, 0)

df_data <- data[-c(4:6,8:11,13:17,19:21,25,42)]
green <- data[data$`Green Flag` == 1,]
brown <- data[data$`Green Flag` == 0,]

library(fastDummies)
data2 <- fastDummies::dummy_cols(data, select_columns = "Issuer")

# Data handling for issue matched data
str(data_matched)

data_matched$`Issue Date` <- as.Date(data_matched$`Issue Date`, origin = "1899-12-30")
data_matched$`Maturity Date` <- as.Date(data_matched$`Maturity Date`, origin = "1899-12-30")
data_matched$`Issue Year` <- data_matched$`Issue Date`
data_matched$`Issue Year` <- as.numeric(format(data_matched$`Issue Year`,'%Y'))
table(data_matched$`Issue Year`)
data_matched$`2008` <- ifelse(data_matched$`Issue Year` == "2008", 1, 0)
data_matched$`2009` <- ifelse(data_matched$`Issue Year` == "2009", 1, 0)
data_matched$`2010` <- ifelse(data_matched$`Issue Year` == "2010", 1, 0)
data_matched$`2011` <- ifelse(data_matched$`Issue Year` == "2011", 1, 0)
data_matched$`2012` <- ifelse(data_matched$`Issue Year` == "2012", 1, 0)
data_matched$`2013` <- ifelse(data_matched$`Issue Year` == "2013", 1, 0)
data_matched$`2014` <- ifelse(data_matched$`Issue Year` == "2014", 1, 0)
data_matched$`2015` <- ifelse(data_matched$`Issue Year` == "2015", 1, 0)
data_matched$`2016` <- ifelse(data_matched$`Issue Year` == "2016", 1, 0)
data_matched$`2017` <- ifelse(data_matched$`Issue Year` == "2017", 1, 0)
data_matched$`2018` <- ifelse(data_matched$`Issue Year` == "2018", 1, 0)
data_matched$`2019` <- ifelse(data_matched$`Issue Year` == "2019", 1, 0)
data_matched$`2020` <- ifelse(data_matched$`Issue Year` == "2020", 1, 0)
data_matched$`2021` <- ifelse(data_matched$`Issue Year` == "2021", 1, 0)
data_matched$`2022` <- ifelse(data_matched$`Issue Year` == "2022", 1, 0)

data_matched$`Offer Yield to Maturity` <- as.numeric(data_matched$`Offer Yield to Maturity`)
data_matched$`Coupon Rate` <- as.numeric(data_matched$`Coupon Rate`)

data_matched$rating <- data_matched$`New Issues Ratings (Moodys)`
table(data_matched$rating)
data_matched <- data_matched%>%
  mutate(rating=case_when(
    rating=="A1" ~ "A",
    rating=="A2" ~ "A",
    rating=="A3" ~ "A",
    rating=="Aa1" ~ "AA",
    rating=="Aa2" ~ "AA",
    rating=="Aa3" ~ "AA",
    rating=="Aaa" ~ "AAA",
    rating=="B1" ~ "B",
    rating=="B2" ~ "B",
    rating=="B3" ~ "B",
    rating=="Ba1" ~ "BB",
    rating=="Ba2" ~ "BB",
    rating=="Ba3" ~ "BB",
    rating=="Baa1" ~ "BBB",
    rating=="Baa2" ~ "BBB",
    rating=="Baa3" ~ "BBB"
  ))

data_matched$`AAA` <- ifelse(data_matched$`rating` == "AAA", 1, 0)
data_matched$`AA` <- ifelse(data_matched$`rating` == "AA", 1, 0)
data_matched$`A` <- ifelse(data_matched$`rating` == "A", 1, 0)
data_matched$`BBB` <- ifelse(data_matched$`rating` == "BBB", 1, 0)
data_matched$`BB` <- ifelse(data_matched$`rating` == "BB", 1, 0)
data_matched$`B` <- ifelse(data_matched$`rating` == "B", 1, 0)

table(data_matched$`Coupon Payment Frequency`)
data_matched$`Annual Coupon` <- ifelse(data_matched$`Coupon Payment Frequency` == "Annual", 1, 0)
data_matched$`Semi Annual Coupon` <- ifelse(data_matched$`Coupon Payment Frequency` == "SemiAnnual", 1, 0)
data_matched$`Quarterly` <- ifelse(data_matched$`Coupon Payment Frequency` == "Quarterly", 1, 0)
data_matched$`Maturity` <- ifelse(data_matched$`Coupon Payment Frequency` == "Maturity", 1, 0)

table(data_matched$`TRBC Economic Sector`)
data_matched$`Basic Materials` <- ifelse(data_matched$`TRBC Economic Sector` == "Basic Materials", 1, 0)
data_matched$`Consumer Cyclicals` <- ifelse(data_matched$`TRBC Economic Sector` == "Consumer Cyclicals", 1, 0)
data_matched$`Consumer Non-Cyclicals` <- ifelse(data_matched$`TRBC Economic Sector` == "Consumer Non-Cyclicals", 1, 0)
data_matched$`Energy` <- ifelse(data_matched$`TRBC Economic Sector` == "Energy", 1, 0)
data_matched$`Financials` <- ifelse(data_matched$`TRBC Economic Sector` == "Financials", 1, 0)
data_matched$`Government Activity` <- ifelse(data_matched$`TRBC Economic Sector` == "Government Activity", 1, 0)
data_matched$`Healthcare` <- ifelse(data_matched$`TRBC Economic Sector` == "Healthcare", 1, 0)
data_matched$`Industrials` <- ifelse(data_matched$`TRBC Economic Sector` == "Industrials", 1, 0)
data_matched$`Institutions, Associations & Organizations` <- ifelse(data_matched$`TRBC Economic Sector` == "Institutions, Associations & Organizations", 1, 0)
data_matched$`Real Estate` <- ifelse(data_matched$`TRBC Economic Sector` == "Real Estate", 1, 0)
data_matched$`Technology` <- ifelse(data_matched$`TRBC Economic Sector` == "Technology", 1, 0)
data_matched$`Utilities` <- ifelse(data_matched$`TRBC Economic Sector` == "Utilities", 1, 0)

table(data_matched$Currency)
data_matched$`AUD` <- ifelse(data_matched$`Currency` == "Australian Dollar", 1, 0)
data_matched$`CAD` <- ifelse(data_matched$`Currency` == "Canadian Dollar", 1, 0)
data_matched$`CLP` <- ifelse(data_matched$`Currency` == "Chilian Peso", 1, 0)
data_matched$`CNY` <- ifelse(data_matched$`Currency` == "Chinese Yuan Renminbi", 1, 0)
data_matched$`EUR` <- ifelse(data_matched$`Currency` == "Euro", 1, 0)
data_matched$`GBP` <- ifelse(data_matched$`Currency` == "Great Britain Pound", 1, 0)
data_matched$`HKD` <- ifelse(data_matched$`Currency` == "Hong Kong Dollar", 1, 0)
data_matched$`JPY` <- ifelse(data_matched$`Currency` == "Japanese Yen", 1, 0)
data_matched$`NZD` <- ifelse(data_matched$`Currency` == "New Zealand Dollar", 1, 0)
data_matched$`NOK` <- ifelse(data_matched$`Currency` == "Norwegian Krone", 1, 0)
data_matched$`SEK` <- ifelse(data_matched$`Currency` == "Swedish Krona", 1, 0)
data_matched$`USD` <- ifelse(data_matched$`Currency` == "U.S. Dollar", 1, 0)
data_matched$`UYU` <- ifelse(data_matched$`Currency` == "Uruguayan Peso Uruguayano", 1, 0)

table(data_matched$Seniority)
data_matched$`Senior Secured Mortgage` <- ifelse(data_matched$`Seniority` == "MTG", 1, 0)
data_matched$`Senior Secured` <- ifelse(data_matched$`Seniority` == "SEC", 1, 0)
data_matched$`Senior Unsecured` <- ifelse(data_matched$`Seniority` == "SR", 1, 0)
data_matched$`Senior Non-Preferred` <- ifelse(data_matched$`Seniority` == "SRBN", 1, 0)
data_matched$`Senior Preferred` <- ifelse(data_matched$`Seniority` == "SRP", 1, 0)
data_matched$`Senior Secured` <- ifelse(data_matched$`Seniority` == "SRSEC", 1, 0)
data_matched$`Senior Subordinated Unsecured` <- ifelse(data_matched$`Seniority` == "SRSUB", 1, 0)
data_matched$`Subordinated Unsecured` <- ifelse(data_matched$`Seniority` == "SUB", 1, 0)
data_matched$`Unsecured` <- ifelse(data_matched$`Seniority` == "UN", 1, 0)

table(data_matched$`Issue Type`)
data_matched$`Public Sector` <- ifelse(data_matched$`Issue Type` == "Agency, Supranational, Sovereign" | data_matched$`Issue Type` == "Federal Credit Agency", 1, 0)
data_matched$`Corporate Sector` <- ifelse(data_matched$`Issue Type` == "Investment Grade Corporate" | data_matched$`Issue Type` == "Investment Grade Corporate|Emerging Market Corporate" | data_matched$`Issue Type` == "Emerging Market Corporate" | data_matched$`Issue Type` == "Emerging Market Corporate|Investment Grade Corporate" | data_matched$`Issue Type` == "High Yield Corporate", 1, 0)

df_data_matched <- data_matched[-c(4:6,8:11,13:17,19:21,25,42)]
green_matched <- data_matched[data_matched$`Green Flag` == 1,]
brown_matched <- data_matched[data_matched$`Green Flag` == 0,]

data_matched2 <- fastDummies::dummy_cols(data_matched, select_columns = "Issuer")

# USDEUR Subsample 
str(usdeur_matched)

usdeur_matched$`Issue Year` <- usdeur_matched$`Issue Date`
usdeur_matched$`Issue Year` <- as.numeric(format(usdeur_matched$`Issue Year`,'%Y'))
table(usdeur_matched$`Issue Year`)
usdeur_matched$`2008` <- ifelse(usdeur_matched$`Issue Year` == "2008", 1, 0)
usdeur_matched$`2009` <- ifelse(usdeur_matched$`Issue Year` == "2009", 1, 0)
usdeur_matched$`2010` <- ifelse(usdeur_matched$`Issue Year` == "2010", 1, 0)
usdeur_matched$`2011` <- ifelse(usdeur_matched$`Issue Year` == "2011", 1, 0)
usdeur_matched$`2012` <- ifelse(usdeur_matched$`Issue Year` == "2012", 1, 0)
usdeur_matched$`2013` <- ifelse(usdeur_matched$`Issue Year` == "2013", 1, 0)
usdeur_matched$`2014` <- ifelse(usdeur_matched$`Issue Year` == "2014", 1, 0)
usdeur_matched$`2015` <- ifelse(usdeur_matched$`Issue Year` == "2015", 1, 0)
usdeur_matched$`2016` <- ifelse(usdeur_matched$`Issue Year` == "2016", 1, 0)
usdeur_matched$`2017` <- ifelse(usdeur_matched$`Issue Year` == "2017", 1, 0)
usdeur_matched$`2018` <- ifelse(usdeur_matched$`Issue Year` == "2018", 1, 0)
usdeur_matched$`2019` <- ifelse(usdeur_matched$`Issue Year` == "2019", 1, 0)
usdeur_matched$`2020` <- ifelse(usdeur_matched$`Issue Year` == "2020", 1, 0)
usdeur_matched$`2021` <- ifelse(usdeur_matched$`Issue Year` == "2021", 1, 0)
usdeur_matched$`2022` <- ifelse(usdeur_matched$`Issue Year` == "2022", 1, 0)

usdeur_matched$rating <- usdeur_matched$`New Issues Ratings (Moodys)`
table(usdeur_matched$rating)
usdeur_matched <- usdeur_matched%>%
  mutate(rating=case_when(
    rating=="A1" ~ "A",
    rating=="A2" ~ "A",
    rating=="A3" ~ "A",
    rating=="Aa1" ~ "AA",
    rating=="Aa2" ~ "AA",
    rating=="Aa3" ~ "AA",
    rating=="Aaa" ~ "AAA",
    rating=="B1" ~ "B",
    rating=="B2" ~ "B",
    rating=="B3" ~ "B",
    rating=="Ba1" ~ "BB",
    rating=="Ba2" ~ "BB",
    rating=="Ba3" ~ "BB",
    rating=="Baa1" ~ "BBB",
    rating=="Baa2" ~ "BBB",
    rating=="Baa3" ~ "BBB"
  ))

usdeur_matched$`AAA` <- ifelse(usdeur_matched$`rating` == "AAA", 1, 0)
usdeur_matched$`AA` <- ifelse(usdeur_matched$`rating` == "AA", 1, 0)
usdeur_matched$`A` <- ifelse(usdeur_matched$`rating` == "A", 1, 0)
usdeur_matched$`BBB` <- ifelse(usdeur_matched$`rating` == "BBB", 1, 0)
usdeur_matched$`BB` <- ifelse(usdeur_matched$`rating` == "BB", 1, 0)
usdeur_matched$`B` <- ifelse(usdeur_matched$`rating` == "B", 1, 0)

table(usdeur_matched$Currency)
usdeur_matched$`EUR` <- ifelse(usdeur_matched$`Currency` == "Euro", 1, 0)
usdeur_matched$`USD` <- ifelse(usdeur_matched$`Currency` == "U.S. Dollar", 1, 0)

table(usdeur_matched$`Coupon Payment Frequency`)
usdeur_matched$`Annual Coupon` <- ifelse(usdeur_matched$`Coupon Payment Frequency` == "Annual", 1, 0)
usdeur_matched$`Semi Annual Coupon` <- ifelse(usdeur_matched$`Coupon Payment Frequency` == "SemiAnnual", 1, 0)
usdeur_matched$`Quarterly` <- ifelse(usdeur_matched$`Coupon Payment Frequency` == "Quarterly", 1, 0)
usdeur_matched$`Maturity` <- ifelse(usdeur_matched$`Coupon Payment Frequency` == "Maturity", 1, 0)

table(usdeur_matched$`TRBC Economic Sector`)
usdeur_matched$`Basic Materials` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Basic Materials", 1, 0)
usdeur_matched$`Consumer Cyclicals` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Consumer Cyclicals", 1, 0)
usdeur_matched$`Consumer Non-Cyclicals` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Consumer Non-Cyclicals", 1, 0)
usdeur_matched$`Energy` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Energy", 1, 0)
usdeur_matched$`Financials` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Financials", 1, 0)
usdeur_matched$`Government Activity` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Government Activity", 1, 0)
usdeur_matched$`Healthcare` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Healthcare", 1, 0)
usdeur_matched$`Industrials` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Industrials", 1, 0)
usdeur_matched$`Institutions, Associations & Organizations` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Institutions, Associations & Organizations", 1, 0)
usdeur_matched$`Real Estate` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Real Estate", 1, 0)
usdeur_matched$`Technology` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Technology", 1, 0)
usdeur_matched$`Utilities` <- ifelse(usdeur_matched$`TRBC Economic Sector` == "Utilities", 1, 0)

table(usdeur_matched$Seniority)
usdeur_matched$`Senior Secured Mortgage` <- ifelse(usdeur_matched$`Seniority` == "MTG", 1, 0)
usdeur_matched$`Senior Secured` <- ifelse(usdeur_matched$`Seniority` == "SEC", 1, 0)
usdeur_matched$`Senior Unsecured` <- ifelse(usdeur_matched$`Seniority` == "SR", 1, 0)
usdeur_matched$`Senior Non-Preferred` <- ifelse(usdeur_matched$`Seniority` == "SRBN", 1, 0)
usdeur_matched$`Senior Preferred` <- ifelse(usdeur_matched$`Seniority` == "SRP", 1, 0)
usdeur_matched$`Senior Secured` <- ifelse(usdeur_matched$`Seniority` == "SRSEC", 1, 0)
usdeur_matched$`Senior Subordinated Unsecured` <- ifelse(usdeur_matched$`Seniority` == "SRSUB", 1, 0)
usdeur_matched$`Subordinated Unsecured` <- ifelse(usdeur_matched$`Seniority` == "SUB", 1, 0)
usdeur_matched$`Unsecured` <- ifelse(usdeur_matched$`Seniority` == "UN", 1, 0)

table(usdeur_matched$`Issue Type`)
usdeur_matched$`Public Sector` <- ifelse(usdeur_matched$`Issue Type` == "Agency, Supranational, Sovereign" | usdeur_matched$`Issue Type` == "Federal Credit Agency", 1, 0)
usdeur_matched$`Corporate Sector` <- ifelse(usdeur_matched$`Issue Type` == "Investment Grade Corporate" | usdeur_matched$`Issue Type` == "Investment Grade Corporate|Emerging Market Corporate" | usdeur_matched$`Issue Type` == "Emerging Market Corporate" | usdeur_matched$`Issue Type` == "Emerging Market Corporate|Investment Grade Corporate" | usdeur_matched$`Issue Type` == "High Yield Corporate", 1, 0)

df_usdeur_matched <- usdeur_matched[-c(4:6,8:11,13:17,19:21,25,42)]
green_usdeur <- usdeur_matched[usdeur_matched$`Green Flag` == 1,]
brown_usdeur <- usdeur_matched[usdeur_matched$`Green Flag` == 0,]

usdeur_matched2 <- fastDummies::dummy_cols(usdeur_matched, select_columns = "Issuer")

# CBI Subsample
str(cbi_matched)

cbi_matched$`Issue Year` <- cbi_matched$`Issue Date`
cbi_matched$`Issue Year` <- as.numeric(format(cbi_matched$`Issue Year`,'%Y'))
table(cbi_matched$`Issue Year`)
cbi_matched$`2008` <- ifelse(cbi_matched$`Issue Year` == "2008", 1, 0)
cbi_matched$`2009` <- ifelse(cbi_matched$`Issue Year` == "2009", 1, 0)
cbi_matched$`2010` <- ifelse(cbi_matched$`Issue Year` == "2010", 1, 0)
cbi_matched$`2011` <- ifelse(cbi_matched$`Issue Year` == "2011", 1, 0)
cbi_matched$`2012` <- ifelse(cbi_matched$`Issue Year` == "2012", 1, 0)
cbi_matched$`2013` <- ifelse(cbi_matched$`Issue Year` == "2013", 1, 0)
cbi_matched$`2014` <- ifelse(cbi_matched$`Issue Year` == "2014", 1, 0)
cbi_matched$`2015` <- ifelse(cbi_matched$`Issue Year` == "2015", 1, 0)
cbi_matched$`2016` <- ifelse(cbi_matched$`Issue Year` == "2016", 1, 0)
cbi_matched$`2017` <- ifelse(cbi_matched$`Issue Year` == "2017", 1, 0)
cbi_matched$`2018` <- ifelse(cbi_matched$`Issue Year` == "2018", 1, 0)
cbi_matched$`2019` <- ifelse(cbi_matched$`Issue Year` == "2019", 1, 0)
cbi_matched$`2020` <- ifelse(cbi_matched$`Issue Year` == "2020", 1, 0)
cbi_matched$`2021` <- ifelse(cbi_matched$`Issue Year` == "2021", 1, 0)
cbi_matched$`2022` <- ifelse(cbi_matched$`Issue Year` == "2022", 1, 0)

cbi_matched$rating <- cbi_matched$`New Issues Ratings (Moodys)`
table(cbi_matched$rating)

cbi_matched <- cbi_matched%>%
  mutate(rating=case_when(
    rating=="A1" ~ "A",
    rating=="A2" ~ "A",
    rating=="A3" ~ "A",
    rating=="Aa1" ~ "AA",
    rating=="Aa2" ~ "AA",
    rating=="Aa3" ~ "AA",
    rating=="Aaa" ~ "AAA",
    rating=="B1" ~ "B",
    rating=="B2" ~ "B",
    rating=="B3" ~ "B",
    rating=="Ba1" ~ "BB",
    rating=="Ba2" ~ "BB",
    rating=="Ba3" ~ "BB",
    rating=="Baa1" ~ "BBB",
    rating=="Baa2" ~ "BBB",
    rating=="Baa3" ~ "BBB"
  ))

cbi_matched$`AAA` <- ifelse(cbi_matched$`rating` == "AAA", 1, 0)
cbi_matched$`AA` <- ifelse(cbi_matched$`rating` == "AA", 1, 0)
cbi_matched$`A` <- ifelse(cbi_matched$`rating` == "A", 1, 0)
cbi_matched$`BBB` <- ifelse(cbi_matched$`rating` == "BBB", 1, 0)
cbi_matched$`BB` <- ifelse(cbi_matched$`rating` == "BB", 1, 0)
cbi_matched$`B` <- ifelse(cbi_matched$`rating` == "B", 1, 0)

table(cbi_matched$Currency)
cbi_matched$`AUD` <- ifelse(cbi_matched$`Currency` == "Australian Dollar", 1, 0)
cbi_matched$`BRL` <- ifelse(cbi_matched$`Currency` == "Brazilian Real", 1, 0)
cbi_matched$`CAD` <- ifelse(cbi_matched$`Currency` == "Canadian Dollar", 1, 0)
cbi_matched$`CLP` <- ifelse(cbi_matched$`Currency` == "Chilian Peso", 1, 0)
cbi_matched$`CNY` <- ifelse(cbi_matched$`Currency` == "Chinese Yuan Renminbi", 1, 0)
cbi_matched$`EUR` <- ifelse(cbi_matched$`Currency` == "Euro", 1, 0)
cbi_matched$`GBP` <- ifelse(cbi_matched$`Currency` == "Great Britain Pound", 1, 0)
cbi_matched$`JPY` <- ifelse(cbi_matched$`Currency` == "Japanese Yen", 1, 0)
cbi_matched$`MXN` <- ifelse(cbi_matched$`Currency` == "Mexican Peso", 1, 0)
cbi_matched$`NZD` <- ifelse(cbi_matched$`Currency` == "New Zealand Dollar", 1, 0)
cbi_matched$`NOK` <- ifelse(cbi_matched$`Currency` == "Norwegian Krone", 1, 0)
cbi_matched$`SEK` <- ifelse(cbi_matched$`Currency` == "Swedish Krona", 1, 0)
cbi_matched$`CHF` <- ifelse(cbi_matched$`Currency` == "Swiss Franc", 1, 0)
cbi_matched$`TRY` <- ifelse(cbi_matched$`Currency` == "Turkish Lira (New)", 1, 0)
cbi_matched$`USD` <- ifelse(cbi_matched$`Currency` == "U.S. Dollar", 1, 0)

table(cbi_matched$`Coupon Payment Frequency`)
cbi_matched$`Annual Coupon` <- ifelse(cbi_matched$`Coupon Payment Frequency` == "Annual", 1, 0)
cbi_matched$`Semi Annual Coupon` <- ifelse(cbi_matched$`Coupon Payment Frequency` == "SemiAnnual", 1, 0)
cbi_matched$`Quarterly` <- ifelse(cbi_matched$`Coupon Payment Frequency` == "Quarterly", 1, 0)
cbi_matched$`Maturity` <- ifelse(cbi_matched$`Coupon Payment Frequency` == "Maturity", 1, 0)

table(cbi_matched$`TRBC Economic Sector`)
cbi_matched$`Basic Materials` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Basic Materials", 1, 0)
cbi_matched$`Consumer Cyclicals` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Consumer Cyclicals", 1, 0)
cbi_matched$`Consumer Non-Cyclicals` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Consumer Non-Cyclicals", 1, 0)
cbi_matched$`Financials` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Financials", 1, 0)
cbi_matched$`Government Activity` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Government Activity", 1, 0)
cbi_matched$`Healthcare` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Healthcare", 1, 0)
cbi_matched$`Industrials` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Industrials", 1, 0)
cbi_matched$`Institutions, Associations & Organizations` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Institutions, Associations & Organizations", 1, 0)
cbi_matched$`Real Estate` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Real Estate", 1, 0)
cbi_matched$`Technology` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Technology", 1, 0)
cbi_matched$`Utilities` <- ifelse(cbi_matched$`TRBC Economic Sector` == "Utilities", 1, 0)

table(cbi_matched$Seniority)
cbi_matched$`Senior Secured Mortgage` <- ifelse(cbi_matched$`Seniority` == "MTG", 1, 0)
cbi_matched$`Senior Secured` <- ifelse(cbi_matched$`Seniority` == "SEC", 1, 0)
cbi_matched$`Senior Unsecured` <- ifelse(cbi_matched$`Seniority` == "SR", 1, 0)
cbi_matched$`Senior Non-Preferred` <- ifelse(cbi_matched$`Seniority` == "SRBN", 1, 0)
cbi_matched$`Senior Preferred` <- ifelse(cbi_matched$`Seniority` == "SRP", 1, 0)
cbi_matched$`Senior Secured` <- ifelse(cbi_matched$`Seniority` == "SRSEC", 1, 0)
cbi_matched$`Senior Subordinated Unsecured` <- ifelse(cbi_matched$`Seniority` == "SRSUB", 1, 0)
cbi_matched$`Subordinated Unsecured` <- ifelse(cbi_matched$`Seniority` == "SUB", 1, 0)
cbi_matched$`Unsecured` <- ifelse(cbi_matched$`Seniority` == "UN", 1, 0)

table(cbi_matched$`Issue Type`)
cbi_matched$`Public Sector` <- ifelse(cbi_matched$`Issue Type` == "Agency, Supranational, Sovereign" | cbi_matched$`Issue Type` == "Federal Credit Agency", 1, 0)
cbi_matched$`Corporate Sector` <- ifelse(cbi_matched$`Issue Type` == "Investment Grade Corporate" | cbi_matched$`Issue Type` == "Investment Grade Corporate|Emerging Market Corporate" | cbi_matched$`Issue Type` == "Emerging Market Corporate" | cbi_matched$`Issue Type` == "Emerging Market Corporate|Investment Grade Corporate" | cbi_matched$`Issue Type` == "High Yield Corporate", 1, 0)

df_cbi_matched <- cbi_matched[-c(4:6,8:11,13:17,19:21,25,42)]
green_cbi <- cbi_matched[cbi_matched$`Green Flag` == 1,]
brown_cbi <- cbi_matched[cbi_matched$`Green Flag` == 0,]

cbi_matched2 <- fastDummies::dummy_cols(cbi_matched, select_columns = "Issuer")

########################################################################
# Green Market Plots
########################################################################
ggplot(green_universe, aes(fill=Group, y=`Issued Amount (USD)`/1000000, x=`Issue Year`)) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), labels = c("Before 2013", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")) +
  scale_y_continuous(breaks = c(0, 50000, 100000, 200000, 400000, 600000)) +
  scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')) +
  #scale_fill_manual(values = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')) +
  theme_ipsum(grid="Y") +
  ylab("Issued Amount (in million USD)") +
  xlab("Year") +
  labs(fill="Currency") +
  ggtitle("Development of the Green Bond Market by Currency")

ggplot(green_universe, aes(fill=Group2, y=`Issued Amount (USD)`/1000000, x=`Issue Year`)) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), labels = c("Before 2013", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")) +
  scale_y_continuous(breaks = c(0, 50000, 100000, 200000, 400000, 600000)) +
  scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928','#362419')) +
  theme_ipsum(grid="Y") +
  ylab("Issued Amount (in million USD)") +
  xlab("Year") +
  labs(fill="Use of Proceeds") +
  ggtitle("Development of the Green Bond Market by Use of Proceeds")

ggplot(green_universe, aes(fill=`Issuer Type`, y=`Issued Amount (USD)`/1000000, x=`Issue Year`)) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), labels = c("Before 2013", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")) +
  scale_y_continuous(breaks = c(0, 50000, 100000, 200000, 400000, 600000)) +
  scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99')) +
  theme_ipsum(grid="Y") +
  ylab("Issued Amount (in million USD)") +
  xlab("Year") +
  labs(fill="Issuer Type") +
  ggtitle("Development of the Green Bond Market by Issuer Type")

world_map <- map_data("world")

green_countries <- c(
  "Afghanistan","Albania","Algeria","American Samoa","Andorra","Angola","Anguilla","Antarctica","Antigua","Armenia","Aruba","Ascension Island","Azerbaijan","Azores","Bahamas","Bahrain","Bangladesh","Barbados","Barbuda","Belarus",
  "Belize","Benin","Bermuda","Bhutan","Bolivia","Bonaire","Bosnia and Herzegovina","Botswana","Brunei","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canary Islands","Cape Verde","Central African Republic",
  "Chad","Chagos Archipelago","Christmas Island","Cocos Islands","Colombia","Comoros","Cook Islands","Croatia","Cuba","Curacao","Cyprus","Democratic Republic of the Congo","Djibouti","Dominica,Dominican Republic",
  "Ecuador","Egypt","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Falkland Islands","Faroe Islands","Fiji","French Guiana","French Polynesia","French Southern and Antarctic Lands","Gabon","Gambia","Georgia","Ghana",
  "Greece","Greenland","Grenada","Grenadines","Guadeloupe","Guam","Guatemala","Guernsey","Guinea","Guinea-Bissau","Guyana","Haiti","Heard Island","Honduras","Iceland","Iran","Iraq","Isle of Man","Israel","Jamaica",
  "Jersey","Jordan","Kazakhstan","Kenya","Kiribati","Kosovo","Kuwait","Kyrgyzstan","Laos","Lebanon","Lesotho","Liberia","Libya","Liechtenstein","Lithuania","Madagascar","Madeira Islands","Malawi","Maldives","Mali","Malta","Marshall Islands",
  "Martinique","Mauritania","Mayotte","Micronesia","Moldova","Monaco","Mongolia","Montenegro","Montserrat","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Nevis","New Caledonia","Nicaragua","Niger","Nigeria",
  "Niue","Norfolk Island","North Korea","North Macedonia","Northern Mariana Islands","Oman","Pakistan","Palau","Palestine","Panama","Papua New Guinea","Paraguay","Pitcairn Islands","Puerto Rico","Qatar",
  "Republic of Congo","Reunion","Romania","Rwanda","Saba","Saint Barthelemy","Saint Helena","Saint Kitts","Saint Lucia","Saint Martin","Saint Pierre and Miquelon","Saint Vincent","Samoa","San Marino","Sao Tome and Principe","Saudi Arabia","Senegal","Seychelles",
  "Siachen Glacier","Sierra Leone","Sint Eustatius","Sint Maarten","Slovakia","Solomon Islands","Somalia","South Africa","South Georgia","South Sandwich Islands","South Sudan","Sri Lanka","Sudan","Suriname","Swaziland",
  "Syria","Taiwan","Tajikistan","Tanzania","Thailand","Timor-Leste","Tobago","Tonga","Trinidad","Tunisia","Turkmenistan","Turks and Caicos Islands","Uganda","Ukraine","Uzbekistan","Vanuatu","Vatican","Vietnam","Wallis and Futuna", "Western Sahara","Yemen","Zambia","Zimbabwe",
  "Argentina", "Australia", "Austria", "Belgium", "Brazil", "Virgin Islands", "Canada", "Cayman Islands", "Chile", "China",
  "Costa Rica", "Czech Republic", "Denmark","El Salvador","Finland","France","Germany", "Hungary","India","Indonesia","Ireland","Italy",
  "Ivory Coast","Japan","Latvia","Luxembourg", "Malaysia", "Mauritius","Mexico","Netherlands","New Zealand","Norway","Peru","Philippines",
  "Poland","Portugal","Russia","Serbia","Singapore","Slovenia","South Korea","Spain","Sweden","Switzerland","Togo","Turkey","United Arab Emirates",
  "UK","USA","Uruguay","Venezuela")

label_countries <- c("Argentina", "Australia", "Austria", "Belgium", "Brazil", "Virgin Islands", "Canada", "Cayman Islands", "Chile", "China",
                     "Costa Rica", "Czech Republic", "Denmark","El Salvador","Finland","France","Germany", "Hungary","India","Indonesia","Ireland","Italy",
                     "Ivory Coast","Japan","Latvia","Luxembourg", "Malaysia", "Mauritius","Mexico","Netherlands","New Zealand","Norway","Peru","Philippines",
                     "Poland","Portugal","Russia","Serbia","Singapore","Slovenia","South Korea","Spain","Sweden","Switzerland","Togo","Turkey","United Arab Emirates",
                     "UK","USA","Uruguay","Venezuela")

world.maps <- map_data("world", region = green_countries)
world.maps2 <- map_data("world", region = label_countries)

emission_count <- c(rep(0,200), 1,11,13,10,5,2,21,7,15,33,1,4,7,1,21,111,114,1,3,7,10,47,7,42,1,48,3,1,5,93,1,23,2,10,4,1,1,2,4,1,24,52,31,7,1,2,4,37,200,1,3)

df <- data.frame(region = green_countries, value = emission_count)

MAP <- left_join(df, world.maps, by = "region")

region.lab.data <- world.maps2 %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

MAP$brks <- cut(MAP$value, breaks=seq(from=0, to=200, length.out=8))

ggplot() +
  geom_polygon(data=MAP, aes(long, lat, fill = brks, group = group)) +
  scale_fill_brewer(palette="Greens", na.value="grey") +
  theme_void() +
  labs(title="Green Bond Issuance Worldwide", caption="Source: Refinitiv Eikon", fill="Number of Issues") +
  with(region.lab.data, annotate(geom="text", x = long, y=lat, label = label_countries, size = 2.5))

#########################################################################
## Descriptive Statistics
#########################################################################
# Universe data
stargazer(as.data.frame(data))
stargazer(as.data.frame(green))
stargazer(as.data.frame(brown))

# Issuer matched data
stargazer(as.data.frame(df_data_matched))
stargazer(as.data.frame(green_matched))
stargazer(as.data.frame(brown_matched))

# USDEUR data
stargazer(as.data.frame(df_usdeur_matched))
stargazer(as.data.frame(green_usdeur))
stargazer(as.data.frame(brown_usdeur))

# CBI data
stargazer(as.data.frame(df_cbi_matched))
stargazer(as.data.frame(green_cbi))
stargazer(as.data.frame(brown_cbi))

# Raincloud plot
library(ggdist)
library(tidyquant)
library(gghalves)
ggplot(data, aes(x = factor(`Green Flag`), y = `Issue Amount`, fill = factor(`Green Flag`))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) +
  scale_fill_manual(values = c("brown", "darkgreen")) + 
  theme_tq() +
  labs(
    title = "Raincloud Plot for Issue Amount",
    x = "Green Flag",
    y = "Issue Amount (in Million U.S. Dollar)"
  ) + 
  theme(legend.position = "none") +
  coord_flip()

ggplot(data, aes(x = factor(`Green Flag`), y = `Offer Yield to Maturity`, fill = factor(`Green Flag`))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) +
  scale_fill_manual(values = c("brown", "darkgreen")) + 
  theme_tq() +
  labs(
    title = "Raincloud Plot for Offer Yield to Maturity",
    x = "Green Flag",
    y = "Offer Yield to Maturity (%)"
  ) + 
  theme(legend.position = "none") +
  coord_flip()

ggplot(data, aes(x = factor(`Green Flag`), y = `Coupon Rate`, fill = factor(`Green Flag`))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) +
  scale_fill_manual(values = c("brown", "darkgreen")) + 
  theme_tq() +
  labs(
    title = "Raincloud Plot for Coupon",
    x = "Green Flag",
    y = "Coupon Rate (%)"
  ) + 
  theme(legend.position = "none") +
  coord_flip()

ggplot(data, aes(x = factor(`Green Flag`), y = `Time to Maturity (Days)`, fill = factor(`Green Flag`))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) +
  scale_fill_manual(values = c("brown", "darkgreen")) + 
  theme_tq() +
  labs(
    title = "Raincloud Plot for Time to Maturity (Days)",
    x = "Green Flag",
    y = "Time to Maturity (in days)"
  ) + 
  theme(legend.position = "none") +
  coord_flip()

# Piecharts
prop.table(table(green$`Public Sector`))
prop.table(table(brown$`Public Sector`))

pie_issuer_type <- data.frame(
  name=c("Public Sector", "Public Sector", "Private Sector", "Private Sector"),
  group=c("Green", "Brown", "Green", "Brown"),
  value=c(0.36, 0.38, 0.64, 0.62)
)

ggplot(pie_issuer_type, aes(x= "", y = value, fill=name)) + 
  geom_col(color = "black") +
  geom_label(aes(label = scales::percent(value)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  facet_wrap(~group) +
  guides(fill = guide_legend(title = "Issuer Type")) +
  scale_fill_viridis_d() +
  #scale_fill_manual(values = c("#fb9a99", "#a6cee3")) +
  coord_polar(theta = "y") + 
  theme_void()

prop.table(table(green$`Issue Year`))
prop.table(table(brown$`Issue Year`))

pie_issue_year <- data.frame(
  name=c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"),
  group=c("Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown"),
  value=c(0, 0.003,0.005,0,0.007,0.014,0.01,0.037,0.045,0.065,0.077,0.117,0.166,0.271,0.183,
          0.059,0.09,0.084,0.079,0.093,0.076,0.076,0.074,0.068,0.062,0.056,0.063,0.047,0.039,0.034)
)

ggplot(pie_issue_year, aes(x= "", y = value, fill= name)) + 
  geom_col(color = "black") +
  geom_label(aes(label = scales::percent(value)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  facet_wrap(~group) +
  guides(fill = guide_legend(title = "Issue Year")) +
  scale_fill_viridis_d() +
  #coord_polar(theta = "y") + 
  theme_bw() +
  labs(
    title = "",
    x = "",
    y = "Proportion"
  )

prop.table(table(green$`TRBC Economic Sector`))
prop.table(table(brown$`TRBC Economic Sector`))

pie_sector <- data.frame(
  name=c("Academic & Educational Services", "Basic Materials", "Consumer Cyclicals", "Consumer Non-Cyclicals", "Energy", "Financials", "Government Activity", "Healthcare", "Industrials", "Institutions, Associations & Organizations", "Real Estate", "Technology", "Utilities","Academic & Educational Services", "Basic Materials", "Consumer Cyclicals", "Consumer Non-Cyclicals", "Energy", "Financials", "Government Activity", "Healthcare", "Industrials", "Institutions, Associations & Organizations", "Real Estate", "Technology", "Utilities"),
  group=c("Green", "Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Brown", "Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown"),
  value=c(0, 0.022, 0.017, 0.015, 0.013, 0.484, 0.143, 0.007, 0.057, 0.064, 0.034, 0.021, 0.125,
          0, 0.012, 0.017, 0.014, 0.013, 0.693, 0.171, 0.003, 0.024, 0.020, 0.003, 0.016, 0.019)
)

ggplot(pie_sector, aes(x= "", y = value, fill= name)) + 
  geom_col(color = "black") +
  geom_label(aes(label = scales::percent(value)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  facet_wrap(~group) +
  guides(fill = guide_legend(title = "Issue Year")) +
  scale_fill_viridis_d() +
  #coord_polar(theta = "y") + 
  theme_bw() +
  labs(
    title = "",
    x = "",
    y = "Proportion"
  )

prop.table(table(green$Seniority))
prop.table(table(brown$Seniority))

pie_seniority <- data.frame(
  name=c("1STLIEN", "2NDLIEN", "1STMTG", "1STRFMTG", "JRSUB", "MTG", "NA", "REFMTG", "SEC", "SR", "SRBN", "SRP", "SRSEC", "SRSUB", "SRSUBSEC", "SUB", "SUBSEC", "UN", "1STLIEN", "2NDLIEN", "1STMTG", "1STRFMTG", "JRSUB", "MTG", "NA", "REFMTG", "SEC", "SR", "SRBN", "SRP", "SRSEC", "SRSUB", "SRSUBSEC", "SUB", "SUBSEC", "UN"),
  group=c("Green", "Green","Green","Green","Green", "Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green", "Green", "Brown", "Brown", "Brown","Brown","Brown","Brown", "Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown"),
  value=c(0.003, 0, 0.014, 0.001, 0.003, 0.045, 0.049, 0.002, 0.002, 0.723, 0.039, 0.047, 0.043, 0.001, 0, 0.005, 0, 0.023,
          0.001, 0.001, 0, 0, 0, 0.084, 0.020, 0.004, 0.017, 0.622, 0.014, 0.028, 0.076, 0.005, 0, 0.019, 0, 0.108)
)

ggplot(pie_seniority, aes(x= "", y = value, fill= name)) + 
  geom_col(color = "black") +
  geom_label(aes(label = scales::percent(value)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  facet_wrap(~group) +
  guides(fill = guide_legend(title = "Issue Year")) +
  scale_fill_viridis_d() +
  #coord_polar(theta = "y") + 
  theme_bw() +
  labs(
    title = "",
    x = "",
    y = "Proportion"
  )

prop.table(table(green$Currency))
prop.table(table(brown$Currency))

pie_currency <- data.frame(
  name=c("AUD", "BRL", "CAD", "CLP", "CNY", "COP", "HRK", "EUR", "GBP", "HKD", "JPY", "KZT", "MXN", "NZD", "NOK", "PEN", "PHP", "RUB", "SGD", "SEK", "CHF", "THB", "TRY", "USD", "UYU", "AUD", "BRL", "CAD", "CLP", "CNY", "COP", "HRK", "EUR", "GBP", "HKD", "JPY", "KZT", "MXN", "NZD", "NOK", "PEN", "PHP", "RUB", "SGD", "SEK", "CHF", "THB", "TRY", "USD", "UYU"),
  group=c("Green", "Green", "Green","Green","Green","Green", "Green", "Green", "Green","Green","Green","Green", "Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green","Green", "Green", "Brown", "Brown", "Brown","Brown","Brown","Brown", "Brown", "Brown", "Brown","Brown","Brown","Brown", "Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown","Brown"),
  value=c(0.004, 0, 0.019, 0, 0.001, 0, 0, 0.560, 0.029, 0.001, 0.012, 0, 0, 0.002, 0.001, 0, 0, 0.001, 0, 0.001, 0, 0, 0, 0.359, 0.001,
          0.001, 0, 0.009, 0, 0, 0, 0, 0.482, 0.054, 0,  0.031, 0, 0.001, 0, 0, 0, 0, 0.001, 0, 0, 0.010, 0, 0, 0.399, 0)
)

ggplot(pie_currency, aes(x= "", y = value, fill= name)) + 
  geom_col(color = "black") +
  geom_label(aes(label = scales::percent(value)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  facet_wrap(~group) +
  guides(fill = guide_legend(title = "Issue Year")) +
  scale_fill_viridis_d() +
  #coord_polar(theta = "y") + 
  theme_bw() +
  labs(
    title = "",
    x = "",
    y = "Proportion"
  )

prop.table(table(green$rating))
prop.table(table(brown$rating))

pie_rating <- data.frame(
  name=c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "AAA", "AA", "A", "BBB", "BB", "B", "CCC"),
  group=c("Green", "Green", "Green","Green","Green","Green", "Green", "Brown","Brown","Brown","Brown","Brown","Brown","Brown"),
  value=c(0.238, 0.180, 0.247, 0.015, 0.048, 0.27, 0.002,
          0.189, 0.225, 0.363, 0.021, 0.039, 0.160, 0.003)
)

ggplot(pie_rating, aes(x= "", y = value, fill= name)) + 
  geom_col(color = "black") +
  geom_label(aes(label = scales::percent(value)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  facet_wrap(~group) +
  guides(fill = guide_legend(title = "Issue Year")) +
  scale_fill_viridis_d() +
  #coord_polar(theta = "y") + 
  theme_bw() +
  labs(
    title = "",
    x = "",
    y = "Proportion"
  )

prop.table(table(green$`Coupon Payment Frequency`))
prop.table(table(brown$`Coupon Payment Frequency`))

pie_freq <- data.frame(
  name=c("Annual", "Maturity", "Monthly", "Quarterly", "SemiAnnual", "Variable", "Annual", "Maturity", "Monthly", "Quarterly", "SemiAnnual", "Variable"),
  group=c("Green", "Green","Green","Green","Green", "Green","Brown","Brown","Brown","Brown","Brown","Brown"),
  value=c(0.61, 0, 0, 0.002, 0.387, 0,
          0.580, 0.002, 0, 0.002, 0.416, 0)
)

ggplot(pie_freq, aes(x= "", y = value, fill= name)) + 
  geom_col(color = "black") +
  geom_label(aes(label = scales::percent(value)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  facet_wrap(~group) +
  guides(fill = guide_legend(title = "Issue Year")) +
  scale_fill_viridis_d() +
  coord_polar(theta = "y") + 
  theme_bw() +
  labs(
    title = "",
    x = "",
    y = "Proportion"
  )

desc_year <- data.frame(
  Issuer.Year = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  Green = c(0.0, 0.3, 0.5, 0.0, 0.7, 1.4, 1.0, 3.7, 4.5, 6.5, 7.7, 11.7, 16.6, 27.1, 18.3),
  Brown =  c(5.9, 9.0, 8.4, 7.9, 9.3, 7.6, 7.6, 7.4, 6.8, 6.2, 5.6, 6.3, 4.7, 3.9, 3.4))

library(stringr)
colnames(desc_year) <- str_replace_all(colnames(desc_year), "[:punct:]", " ")

desc_year %>%
  gt::gt() %>%
  gt_theme_espn() %>%
  gt_hulk_col_numeric(Green, trim=TRUE) %>%
  gt_hulk_col_numeric(Brown, trim=TRUE)

desc_rating <- data.frame(
  Rating = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"),
  Green = c(24.7, 18.0, 23.8, 27.0, 4.8, 1.5, 0.02),
  Brown =  c(36.3, 22.5, 18.9, 16.0, 3.9, 2.1, 0.03))

desc_rating %>%
  gt::gt() %>%
  gt_theme_pff() %>%
  gt_hulk_col_numeric(Green, trim=TRUE) %>%
  gt_hulk_col_numeric(Brown, trim=TRUE)

desc_freq <- data.frame(
  Coupon.Frequency = c("Annual Coupon", "Semi Annual Coupon", "Quarterly", "Monthly", "Variable", "Maturity"),
  Green = c(61.0, 38.7, 0.2, 0.0, 0.0, 0.1),
  Brown =  c(58.0, 41.6, 0.2, 0.02, 0.01, 0.02))

colnames(desc_freq) <- str_replace_all(colnames(desc_freq), "[:punct:]", " ")

z <- desc_freq %>%
  gt::gt() %>%
  gt_theme_espn() %>%
  gt_hulk_col_numeric(Green, trim=TRUE) %>%
  gt_hulk_col_numeric(Brown, trim=TRUE)

desc_industry <- data.frame(
  TRBC.Industry = c("Academic & Educational Services", "Basic Materials", "Consumer Cyclicals", "Consumer Non-Cyclicals", "Energy", "Financials", "Government Activity", "Healthcare", "Industrials", "Institutions, Associations & Organizations", "Real Estate", "Technology", "Utilities"),
  Green = c(0.0, 2.2, 1.7, 1.5, 1.3, 48.4, 14.3, 0.7, 5.7, 6.4, 3.4, 2.1, 12.5),
  Brown =  c(0.01, 1.2, 1.7, 1.4, 1.3, 68.9, 17.1, 0.3, 2.4, 2.0, 0.3, 1.6, 2.0))

colnames(desc_industry) <- str_replace_all(colnames(desc_industry), "[:punct:]", " ")

desc_industry %>%
  gt::gt() %>%
  gt_theme_espn() %>%
  gt_hulk_col_numeric(Green, trim=TRUE) %>%
  gt_hulk_col_numeric(Brown, trim=TRUE)

desc_currency <- data.frame(
  Currency = c("AUD", "BRL", "CAD", "CLP", "CNY", "COP", "HRK", "EUR", "GBP", "HKD", "JPY", "KZT", "MXN", "NZD", "NOK", "PEN", "PHP", "RUB", "SGD", "SEK", "CHF", "THB", "TRY", "USD", "UYU"),
  Green = c(0.4, 0.0, 2.0, 0.1, 0.1, 0.0, 0.0, 56.0, 2.9, 0.1, 1.2, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.1, 0.0, 0.9, 0.0, 0.0, 0.0, 35.9, 0.1),
  Brown =  c(0.9, 0.04, 0.9, 0.02, 0.04, 0.03, 0.01, 48.2, 5.4, 0.02, 3.1, 0.01, 0.1, 0.1, 0.04, 0.04, 0.02, 0.1, 0.03, 0.04, 1.0, 0.03, 0.02, 39.9, 0.03))

desc_currency %>%
  gt::gt() %>%
  gt_theme_espn() %>%
  gt_hulk_col_numeric(Green, trim=TRUE) %>%
  gt_hulk_col_numeric(Brown, trim=TRUE)

desc_seniority <- data.frame(
  Seniority = c("First-Lien Loan", "First Mortgage", "First Refunding Mortgage", "Second-Lien Loan", "Junior Subordinated", "Senior Secured Mortgage", "Refunding Mortgage", "Senior Secured", "Senior Unsecured", "Senior Non-Preferred", "Senior Preferred", "Senior Subordinated Unsecured", "Senior Subordinated Secured", "Subordinated Unsecured", "Subordinated Secured", "Unsecured"),
  Green = c(0.3, 1.4, 0.1, 0.0, 0.3, 4.5, 0.2, 4.3, 72.3, 3.9, 4.7, 0.1, 0.0, 0.5, 0.0, 2.3),
  Brown =  c(0.1, 0.0, 0.0, 0.1, 0.04, 8.4, 0.4, 7.6, 62.2, 1.4, 2.8, 0.5, 0.01, 2.0, 0.01, 10.8))

desc_seniority %>%
  gt::gt() %>%
  gt_theme_espn() %>%
  gt_hulk_col_numeric(Green, trim=TRUE) %>%
  gt_hulk_col_numeric(Brown, trim=TRUE)

desc_type <- data.frame(
  Issuer.Type = c("Public Sector", "Corporate Sector"),
  Green = c(36.2, 63.8),
  Brown =  c(38.2, 61.8))

colnames(desc_type) <- str_replace_all(colnames(desc_type), "[:punct:]", " ")

desc_type %>%
  gt::gt() %>%
  gt_theme_espn() %>%
  gt_hulk_col_numeric(Green, trim=TRUE) %>%
  gt_hulk_col_numeric(Brown, trim=TRUE)

desc_guarantor <- data.frame(
  Guarantor = c("Guarantor", "No Guarantor"),
  Green = c(18.4, 81.6),
  Brown =  c(24.0, 76.0))

desc_guarantor %>%
  gt::gt() %>%
  gt_theme_espn() %>%
  gt_hulk_col_numeric(Green, trim=TRUE) %>%
  gt_hulk_col_numeric(Brown, trim=TRUE)


labs <- c("Green Bonds", "Conventional Bonds")
df_desc <- data[,c(3,7,12,23,24)]
df_desc %>%
  mutate(`Green Flag` = ifelse(`Green Flag` == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = `Time to Maturity (Days)`)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  facet_wrap(~`Green Flag`) +
  xlab("Time to Maturity (Days)") +
  theme_bw()

df_desc %>%
  mutate(`Green Flag` = ifelse(`Green Flag` == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = `Issue Amount`)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  facet_wrap(~`Green Flag`) +
  xlab("Issue Amount") +
  theme_bw()

df_desc %>%
  mutate(`Green Flag` = ifelse(`Green Flag` == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = `Coupon Rate`)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  facet_wrap(~`Green Flag`) +
  xlab("Coupon Rate") +
  theme_bw()

df_desc %>%
  mutate(`Green Flag` = ifelse(`Green Flag` == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = `Offer Yield to Maturity`)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  facet_wrap(~`Green Flag`) +
  xlab("Offer Yield to Maturity") +
  theme_bw()

# Correlation matrix
library(ggheatmap)
library(reshape2)
df <- data[c(3,22,12,18,23,24,c(28:42),c(44:112))]
sds <- df
sds[] <- lapply(sds,as.numeric)


cormatweek <- round(cor(sds, method = "spearman"),2)

# Get upper triangle of the correlation matrix
get_upper_tri_week <- function(cormatweek){
  cormatweek[lower.tri(cormatweek)]<- NA
  return(cormatweek)
}

upper_tri_week <- get_upper_tri_week(cormatweek)
upper_tri_week
melted_cormat_week <- melt(upper_tri_week, na.rm = TRUE)
arranged_melted_cormat_week_asc <- arrange(melted_cormat_week, value)
arranged_melted_cormat_week_asc <- arranged_melted_cormat_week_asc[1:50,]
arranged_melted_cormat_week_desc <- arrange(melted_cormat_week, desc(value))
arranged_melted_cormat_week_desc <- arranged_melted_cormat_week_desc[1:137,]
melted_cormat_week <- merge(arranged_melted_cormat_week_asc, arranged_melted_cormat_week_desc, all = TRUE )

ggheatmap <- ggplot(data = melted_cormat_week, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 8, hjust = 1))+
  coord_fixed()+
  theme(axis.text.y = element_text(vjust = 1,
                                   size = 8, hjust = 1))
# add numbers
ggheatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (18), hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.75),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  ggtitle("Correlation Matrix")

devtools::install_github("laresbernardo/lares")
library(lares)

corr_cross(df, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)

res <- cor(df)
round(res, 2)

#########################################################################
## MATCHING
#########################################################################
# Step 1: Balance Check 
# The allowed methods are "nearest" for nearest neighbor matching (on the propensity score by default), "optimal" for optimal pair matching, "full" for optimal full matching, "genetic" for genetic
# matching, "cem" for coarsened exact matching, "exact" for exact matching, "cardinality" for cardinality and template matching, and "subclass" for subclassification.
data_matched$`Issue Year` <- as.character(data_matched$`Issue Year`)
usdeur_matched$`Issue Year` <- as.character(usdeur_matched$`Issue Year`)
cbi_matched$`Issue Year` <- as.character(cbi_matched$`Issue Year`)

balance_check1 <- matchit(formula = `Green Flag` ~ `Offer Yield to Maturity` + `Issue Year` + `Seniority` +
                            `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating` + `Issue Type`,
                          data = data_matched, method = NULL, distance = "glm")

balance_check2 <- matchit(formula = `Green Flag` ~ `Offer Yield to Maturity` + `Issue Year` + `Seniority` +
                            `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating` + `Issue Type`,
                          data = usdeur_matched, method = NULL, distance = "glm")

balance_check3 <- matchit(formula = `Green Flag` ~ `Offer Yield to Maturity` + `Issue Year` + `Seniority` +
                            `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating` + `Issue Type`,
                          data = cbi_matched, method = NULL, distance = "glm")

summary(balance_check1)
summary(balance_check2)
summary(balance_check3)

# Matching by Issuer Cluster
df2 <- list() 
formula <- list()
y = "`Green Flag`"
uniq <- unique(data_matched$Issuer)
for (i in 1:length(uniq)) {
  df <- subset(data_matched, data_matched$Issuer == uniq[i])
  if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Currency` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }  
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }  
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  } 
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }  
  else {
    x <- "`Time to Maturity (Days)`"
    formula = as.formula(paste(y, "~", x))
  }
  match <- matchit(formula = formula,
                   data = df, method = "nearest",
                   link = "probit", distance = "glm")
  df2[[i]] <- match.data(match)
}

zerbib_psm <- rbindlist(df2, fill=TRUE)

df2 <- list() 
formula <- list()
y = "`Green Flag`"
uniq <- unique(usdeur_matched$Issuer)
for (i in 1:length(uniq)) {
  df <- subset(usdeur_matched, usdeur_matched$Issuer == uniq[i])
  if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Currency` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }  
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }  
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  } 
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }  
  else {
    x <- "`Time to Maturity (Days)`"
    formula = as.formula(paste(y, "~", x))
  }
  match <- matchit(formula = formula,
                   data = df, method = "nearest",
                   link = "probit", distance = "glm")
  df2[[i]] <- match.data(match)
}

usdeur_zerbib_psm <- rbindlist(df2, fill=TRUE)

df2 <- list() 
formula <- list()
y = "`Green Flag`"
uniq <- unique(cbi_matched$Issuer)
for (i in 1:length(uniq)) {
  df <- subset(cbi_matched, cbi_matched$Issuer == uniq[i])
  if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ) {
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Currency` + `rating` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }  
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) > 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `rating`"
    formula = as.formula(paste(y, "~", x))
  }  
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) > 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Currency`"
    formula = as.formula(paste(y, "~", x))
  }
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) > 1 & length(unique(df$Guarantor)) == 1 ){
    x <- "`Time to Maturity (Days)` + `Seniority`"
    formula = as.formula(paste(y, "~", x))
  } 
  else if (length(unique(df$rating)) == 1 & length(unique(df$Currency)) == 1 & length(unique(df$Seniority)) == 1 & length(unique(df$Guarantor)) > 1 ){
    x <- "`Time to Maturity (Days)` + `Guarantor`"
    formula = as.formula(paste(y, "~", x))
  }  
  else {
    x <- "`Time to Maturity (Days)`"
    formula = as.formula(paste(y, "~", x))
  }
  match <- matchit(formula = formula,
                   data = df, method = "nearest",
                   link = "probit", distance = "glm")
  df2[[i]] <- match.data(match)
}

cbi_zerbib_psm <- rbindlist(df2, fill=TRUE)

# Matching without Issuer Cluster
# Data matched
match_prop_nnm <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                          data = data_matched, method = "nearest",
                          link = "probit", distance = "glm")

match_prop_nnm2 <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                           data = data_matched, method = "nearest",
                           link = "probit", distance = "glm")

match_prop_cem_k2k <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                              data = data_matched, method = "cem",
                              link = "probit", distance = "glm", k2k = TRUE)

match_prop_cem2_k2k <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                               data = data_matched, method = "cem",
                               link = "probit", distance = "glm", k2k = TRUE )

match_prop_cem <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                          data = data_matched, method = "cem",
                          link = "probit", distance = "glm")

match_prop_cem2 <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                           data = data_matched, method = "cem",
                           link = "probit", distance = "glm")

summary(match_prop_nnm, un=FALSE)
summary(match_prop_nnm2, un=FALSE)
summary(match_prop_cem_k2k, un=FALSE)
summary(match_prop_cem2_k2k, un=FALSE)
summary(match_prop_cem, un=FALSE)
summary(match_prop_cem2, un=FALSE)

plot(summary(match_prop_nnm, un=FALSE))
plot(summary(match_prop_nnm2, un=FALSE))
plot(summary(match_prop_cem_k2k, un=FALSE))
plot(summary(match_prop_cem2_k2k, un=FALSE))
plot(summary(match_prop_cem, un=FALSE))
plot(summary(match_prop_cem2, un=FALSE))

plot(match_prop_nnm, type = "jitter", interactive = FALSE)
plot(match_prop_nnm2, type = "jitter", interactive = FALSE)
plot(match_prop_cem_k2k, type = "jitter", interactive = FALSE)
plot(match_prop_cem2_k2k, type = "jitter", interactive = FALSE)
plot(match_prop_cem, type = "jitter", interactive = FALSE)
plot(match_prop_cem2, type = "jitter", interactive = FALSE)

prop_nnm <- match.data(match_prop_nnm)
prop_nnm2 <- match.data(match_prop_nnm2)
prop_cem_k2k <- match.data(match_prop_cem_k2k)
prop_cem2_k2k <- match.data(match_prop_cem2_k2k)
prop_cem <- match.data(match_prop_cem)
prop_cem2 <- match.data(match_prop_cem2)

library(openxlsx)
write.xlsx(prop_nnm, 'prop_nnm.xlsx')
write.xlsx(prop_nnm2, 'prop_nnm2.xlsx')

# Data USDEUR

match_usdeur_exact <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                              data = usdeur_matched, method = "exact",
                              link = "probit", distance = "glm")

match_usdeur_prop_nnm <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                                 data = usdeur_matched, method = "nearest",
                                 link = "probit", distance = "glm")

match_usdeur_prop_nnm2 <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                                  data = usdeur_matched, method = "nearest",
                                  link = "probit", distance = "glm")

match_usdeur_prop_cem_k2k <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                                     data = usdeur_matched, method = "cem",
                                     link = "probit", distance = "glm", k2k = TRUE)

match_usdeur_prop_cem2_k2k <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                                      data = usdeur_matched, method = "cem",
                                      link = "probit", distance = "glm", k2k = TRUE )

match_usdeur_prop_cem <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                                 data = usdeur_matched, method = "cem",
                                 link = "probit", distance = "glm")

match_usdeur_prop_cem2 <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                                  data = usdeur_matched, method = "cem",
                                  link = "probit", distance = "glm")

summary(match_usdeur_prop_nnm, un=FALSE)
summary(match_usdeur_prop_nnm2, un=FALSE)
summary(match_usdeur_prop_cem_k2k, un=FALSE)
summary(match_usdeur_prop_cem2_k2k, un=FALSE)
summary(match_usdeur_prop_cem, un=FALSE)
summary(match_usdeur_prop_cem2, un=FALSE)

plot(summary(match_usdeur_prop_nnm, un=FALSE))
plot(summary(match_usdeur_prop_nnm2, un=FALSE))
plot(summary(match_usdeur_prop_cem_k2k, un=FALSE))
plot(summary(match_usdeur_prop_cem2_k2k, un=FALSE))
plot(summary(match_usdeur_prop_cem, un=FALSE))
plot(summary(match_usdeur_prop_cem2, un=FALSE))

plot(match_usdeur_prop_nnm, type = "jitter", interactive = FALSE)
plot(match_usdeur_prop_nnm2, type = "jitter", interactive = FALSE)
plot(match_usdeur_prop_cem_k2k, type = "jitter", interactive = FALSE)
plot(match_usdeur_prop_cem2_k2k, type = "jitter", interactive = FALSE)
plot(match_usdeur_prop_cem, type = "jitter", interactive = FALSE)
plot(match_usdeur_prop_cem2, type = "jitter", interactive = FALSE)

usdeur_exact <- match.data(match_usdeur_exact)
usdeur_prop_nnm <- match.data(match_usdeur_prop_nnm)
usdeur_prop_nnm2 <- match.data(match_usdeur_prop_nnm2)
usdeur_prop_cem_k2k <- match.data(match_usdeur_prop_cem_k2k)
usdeur_prop_cem2_k2k <- match.data(match_usdeur_prop_cem2_k2k)
usdeur_prop_cem <- match.data(match_usdeur_prop_cem)
usdeur_prop_cem2 <- match.data(match_usdeur_prop_cem2)

write.xlsx(usdeur_prop_nnm, 'usdeur_prop_nnm.xlsx')
write.xlsx(usdeur_prop_nnm2, 'usdeur_prop_nnm2.xlsx')

# Data CBI Flag
match_cbi_prop_nnm <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                              data = cbi_matched, method = "nearest",
                              link = "probit", distance = "glm")

match_cbi_prop_nnm2 <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                               data = cbi_matched, method = "nearest",
                               link = "probit", distance = "glm")

match_cbi_prop_cem_k2k <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                                  data = cbi_matched, method = "cem",
                                  link = "probit", distance = "glm", k2k = TRUE)

match_cbi_prop_cem2_k2k <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                                   data = cbi_matched, method = "cem",
                                   link = "probit", distance = "glm", k2k = TRUE )

match_cbi_prop_cem <- matchit(formula = `Green Flag` ~ `Time to Maturity (Days)` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `rating` + `Seniority`,
                              data = cbi_matched, method = "cem",
                              link = "probit", distance = "glm")

match_cbi_prop_cem2 <- matchit(formula = `Green Flag` ~ `Issue Year` + `Seniority` + `Time to Maturity (Days)` + `Issue Amount` + `Guarantor` + `Coupon Payment Frequency` + `Currency` + `TRBC Economic Sector` + `rating`,
                               data = cbi_matched, method = "cem",
                               link = "probit", distance = "glm")

summary(match_cbi_prop_nnm, un=FALSE)
summary(match_cbi_prop_nnm2, un=FALSE)
summary(match_cbi_prop_cem_k2k, un=FALSE)
summary(match_cbi_prop_cem2_k2k, un=FALSE)
summary(match_cbi_prop_cem, un=FALSE)
summary(match_cbi_prop_cem2, un=FALSE)

plot(summary(match_cbi_prop_nnm, un=FALSE))
plot(summary(match_cbi_prop_nnm2, un=FALSE))
plot(summary(match_cbi_prop_cem_k2k, un=FALSE))
plot(summary(match_cbi_prop_cem2_k2k, un=FALSE))
plot(summary(match_cbi_prop_cem, un=FALSE))
plot(summary(match_cbi_prop_cem2, un=FALSE))

plot(match_cbi_prop_nnm, type = "jitter", interactive = FALSE)
plot(match_cbi_prop_nnm2, type = "jitter", interactive = FALSE)
plot(match_cbi_prop_cem_k2k, type = "jitter", interactive = FALSE)
plot(match_cbi_prop_cem2_k2k, type = "jitter", interactive = FALSE)
plot(match_cbi_prop_cem, type = "jitter", interactive = FALSE)
plot(match_cbi_prop_cem2, type = "jitter", interactive = FALSE)

cbi_prop_nnm <- match.data(match_cbi_prop_nnm)
cbi_prop_nnm2 <- match.data(match_cbi_prop_nnm2)
cbi_prop_cem_k2k <- match.data(match_cbi_prop_cem_k2k)
cbi_prop_cem2_k2k <- match.data(match_cbi_prop_cem2_k2k)
cbi_prop_cem <- match.data(match_cbi_prop_cem)
cbi_prop_cem2 <- match.data(match_cbi_prop_cem2)

write.xlsx(cbi_prop_nnm, 'cbi_prop_nnm.xlsx')
write.xlsx(cbi_prop_nnm2, 'cbi_prop_nnm2.xlsx')

# Step 3: Estimating Treatment Effects and Standard Errors After Matching with OLS
prop_nnm_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = prop_nnm)
summary(prop_nnm_ate)

prop_nnm2_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = prop_nnm2)
summary(prop_nnm2_ate)

prop_cem_k2k_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = prop_cem_k2k)
summary(prop_cem_k2k_ate)

prop_cem2_k2k_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = prop_cem2_k2k)
summary(prop_cem2_k2k_ate)

prop_cem_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = prop_cem, weights = weights)
coeftest(prop_cem_ate, vcov. = vcovCL, cluster = ~subclass)

prop_cem2_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = prop_cem2, weights = weights)
coeftest(prop_cem2_ate, vcov. = vcovCL, cluster = ~subclass)

####

usdeur_exact_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = usdeur_exact)
summary(usdeur_exact_ate)

usdeur_prop_nnm_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = usdeur_prop_nnm)
summary(usdeur_prop_nnm_ate)

usdeur_prop_nnm2_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = usdeur_prop_nnm2)
summary(usdeur_prop_nnm2_ate)

usdeur_prop_cem_k2k_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = usdeur_prop_cem_k2k)
summary(usdeur_prop_cem_k2k_ate)

usdeur_prop_cem2_k2k_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = usdeur_prop_cem2_k2k)
summary(usdeur_prop_cem2_k2k_ate)

usdeur_prop_cem_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = usdeur_prop_cem, weights = weights)
coeftest(usdeur_prop_cem_ate, vcov. = vcovCL, cluster = ~subclass)

usdeur_prop_cem2_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = usdeur_prop_cem2, weights = weights)
coeftest(usdeur_prop_cem2_ate, vcov. = vcovCL, cluster = ~subclass)

####

cbi_exact_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = cbi_exact)
summary(cbi_exact_ate)

cbi_prop_nnm_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = cbi_prop_nnm)
summary(cbi_prop_nnm_ate)

cbi_prop_nnm2_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = cbi_prop_nnm2)
summary(cbi_prop_nnm2_ate)

cbi_prop_cem_k2k_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = cbi_prop_cem_k2k)
summary(cbi_prop_cem_k2k_ate)

cbi_prop_cem2_k2k_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = cbi_prop_cem2_k2k)
summary(cbi_prop_cem2_k2k_ate)

cbi_prop_cem_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = cbi_prop_cem, weights = weights)
coeftest(cbi_prop_cem_ate, vcov. = vcovCL, cluster = ~subclass)

cbi_prop_cem2_ate <- lm(`Offer Yield to Maturity` ~ `Green Flag`, data = cbi_prop_cem2, weights = weights)
coeftest(cbi_prop_cem2_ate, vcov. = vcovCL, cluster = ~subclass)

#########################################################################
# CAUSAL FOREST
#########################################################################
# rapid check for ATE given randomization
data_treated=prop_nnm[prop_nnm$`Green Flag`==1,]
data_control=prop_nnm[prop_nnm$`Green Flag`==0,]
mean(data_treated$`Offer Yield to Maturity`)- mean(data_control$`Offer Yield to Maturity`)

covariate_names1 <- c("Time to Maturity (Days)","Issue Amount","Guarantor",
                      "2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
                      "Annual Coupon", "Semi Annual Coupon", "Quarterly",
                      "Senior Secured Mortgage", "Senior Secured", "Senior Unsecured", "Senior Non-Preferred", "Senior Preferred", "Senior Subordinated Unsecured", "Subordinated Unsecured",
                      "Basic Materials", "Consumer Cyclicals", "Consumer Non-Cyclicals", "Energy", "Financials", "Healthcare", "Industrials", "Institutions, Associations & Organizations", "Real Estate", "Technology", "Utilities",
                      "AAA", "AA", "A", "BBB", "BB",
                      "AUD", "CAD", "CLP", "CNY", "EUR", "GBP", "HKD", "JPY", "NZD", "NOK", "SEK", "UYU"
)

covariate_names2 <- c("Time to Maturity (Days)","Issue Amount","Guarantor",
                      "2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
                      "Annual Coupon", "Semi Annual Coupon", "Quarterly", "Monthly", "Variable",
                      "First-Lien Loan", "First Mortgage", "First Refunding Mortgage", "Second-Lien Loan", "Junior Subordinated", "Senior Secured Mortgage", "Refunding Mortgage", "Senior Secured", "Senior Unsecured", "Senior Non-Preferred", "Senior Preferred", "Senior Subordinated Unsecured", "Senior Subordinated Secured", "Subordinated Unsecured", "Subordinated Secured", 
                      "Academic & Educational Services", "Basic Materials", "Consumer Cyclicals", "Consumer Non-Cyclicals", "Energy", "Financials", "Healthcare", "Industrials", "Institutions, Associations & Organizations", "Real Estate", "Technology", "Utilities",
                      "AAA", "AA", "A", "BBB", "BB", "B",
                      "AUD", "BRL", "CAD", "CLP", "CNY", "COP", "HRK", "EUR", "GBP", "HKD", "JPY", "KZT", "MXN", "NZD", "NOK", "PEN", "PHP", "RUB", "SGD", "SEK", "CHF", "THB", "TRY", "UYU"
)

covariate_names3 <- c("Time to Maturity (Days)","Issue Amount","Guarantor",
                      "2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
                      "Annual Coupon", "Semi Annual Coupon", "Quarterly",
                      "Senior Secured Mortgage", "Senior Secured", "Senior Unsecured", "Senior Non-Preferred", "Senior Preferred", "Senior Subordinated Unsecured", "Subordinated Unsecured",
                      "Basic Materials", "Consumer Cyclicals", "Consumer Non-Cyclicals", "Energy", "Financials", "Healthcare", "Industrials", "Institutions, Associations & Organizations", "Real Estate", "Technology", "Utilities",
                      "AAA", "AA", "A", "BBB", "BB",
                      "EUR"
)

covariate_names4 <- c("Time to Maturity (Days)","Issue Amount","Guarantor",
                      "2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
                      "Annual Coupon", "Semi Annual Coupon", "Quarterly",
                      "Senior Secured Mortgage", "Senior Secured", "Senior Unsecured", "Senior Non-Preferred", "Senior Preferred", "Senior Subordinated Unsecured", "Subordinated Unsecured",
                      "Basic Materials", "Consumer Cyclicals", "Consumer Non-Cyclicals", "Financials", "Healthcare", "Industrials", "Institutions, Associations & Organizations", "Real Estate", "Technology", "Utilities",
                      "AAA", "AA", "A", "BBB", "BB",
                      "AUD", "CAD", "CLP", "CNY", "EUR", "GBP", "JPY", "NZD", "NOK", "SEK"
)

issuer <- unique(data2$Issuer)
issuer_ <- rep("Issuer_", 2211)
issuer_name <- paste0(issuer_, issuer)
covariate_names5 <- c(covariate_names2, issuer_name)

issuer <- unique(data_matched2$Issuer)
issuer_ <- rep("Issuer_", 283)
issuer_name <- paste0(issuer_, issuer)
covariate_names6 <- c(covariate_names1, issuer_name)

issuer <- unique(usdeur_matched2$Issuer)
issuer_ <- rep("Issuer_", 265)
issuer_name <- paste0(issuer_, issuer)
covariate_names7 <- c(covariate_names3, issuer_name)

issuer <- unique(cbi_matched2$Issuer)
issuer_ <- rep("Issuer_", 201)
issuer_name <- paste0(issuer_, issuer)
covariate_names8 <- c(covariate_names4, issuer_name)

issuer <- unique(prop_nnm$Issuer)
issuer_ <- rep("Issuer_", 283)
issuer_name <- paste0(issuer_, issuer)
covariate_names9 <- c(covariate_names1, issuer_name)

prop_nnm <- fastDummies::dummy_cols(prop_nnm, select_columns = "Issuer")

# 1.1) Build the forest with treatment = tech.support
# Data Universe without Matching
cf0 <- causal_forest(
  X = as.matrix(data[,covariate_names2]),
  Y = data$`Offer Yield to Maturity`,
  W = data$`Green Flag`,
  num.trees = nrow(data[,covariate_names2]))

# Data Universe with Issuer Controls
cf0.1 <- causal_forest(
  X = as.matrix(data2[,covariate_names5]),
  Y = data$`Offer Yield to Maturity`,
  W = data$`Green Flag`,
  num.trees = nrow(data2[,covariate_names5]))

# Data matched without Matching
cf1 <- causal_forest(
  X = as.matrix(data_matched[,covariate_names1]),
  Y = data_matched$`Offer Yield to Maturity`,
  W = data_matched$`Green Flag`,
  num.trees = nrow(data_matched[,covariate_names1]))

# Data matched with Issuer Controls
cf1.1 <- causal_forest(
  X = as.matrix(data_matched2[,covariate_names6]),
  Y = data_matched$`Offer Yield to Maturity`,
  W = data_matched$`Green Flag`,
  num.trees = nrow(data_matched2[,covariate_names6]))

# USDEUR without Matching
cf2 <- causal_forest(
  X = as.matrix(usdeur_matched[,covariate_names3]),
  Y = usdeur_matched$`Offer Yield to Maturity`,
  W = usdeur_matched$`Green Flag`,
  num.trees = nrow(usdeur_matched[,covariate_names3]))

# USDEUR with Issuer Controls
cf2.1 <- causal_forest(
  X = as.matrix(usdeur_matched2[,covariate_names7]),
  Y = usdeur_matched$`Offer Yield to Maturity`,
  W = usdeur_matched$`Green Flag`,
  num.trees = nrow(usdeur_matched2[,covariate_names7]))

# CBI without Matching
cf3 <- causal_forest(
  X = as.matrix(cbi_matched[,covariate_names4]),
  Y = cbi_matched$`Offer Yield to Maturity`,
  W = cbi_matched$`Green Flag`,
  num.trees = nrow(cbi_matched[,covariate_names4]))

# CBI with Issuer Controls
cf3.1 <- causal_forest(
  X = as.matrix(cbi_matched2[,covariate_names8]),
  Y = cbi_matched$`Offer Yield to Maturity`,
  W = cbi_matched$`Green Flag`,
  num.trees = nrow(cbi_matched2[,covariate_names8]))

# Data matched without issuer
cf4 <- causal_forest(
  X = as.matrix(prop_nnm[,covariate_names1]),
  Y = prop_nnm$`Offer Yield to Maturity`,
  W = prop_nnm$`Green Flag`,
  num.trees = nrow(prop_nnm[,covariate_names1]))

cf4.1 <- causal_forest(
  X = as.matrix(prop_nnm[,covariate_names9]),
  Y = prop_nnm$`Offer Yield to Maturity`,
  W = prop_nnm$`Green Flag`,
  num.trees = nrow(prop_nnm[,covariate_names9]))
                       
cf5 <- causal_forest(
  X = as.matrix(prop_nnm2[,covariate_names1]),
  Y = prop_nnm2$`Offer Yield to Maturity`,
  W = prop_nnm2$`Green Flag`,
  num.trees = nrow(prop_nnm2[,covariate_names1]))

cf6 <- causal_forest(
  X = as.matrix(prop_cem[,covariate_names1]),
  Y = prop_cem$`Offer Yield to Maturity`,
  W = prop_cem$`Green Flag`,
  num.trees = nrow(prop_cem[,covariate_names1]),
  sample.weights = prop_cem$weights)

cf7 <- causal_forest(
  X = as.matrix(prop_cem_k2k[,covariate_names1]),
  Y = prop_cem_k2k$`Offer Yield to Maturity`,
  W = prop_cem_k2k$`Green Flag`,
  num.trees = nrow(prop_cem_k2k[,covariate_names1]))

cf8 <- causal_forest(
  X = as.matrix(prop_cem2[,covariate_names1]),
  Y = prop_cem2$`Offer Yield to Maturity`,
  W = prop_cem2$`Green Flag`,
  num.trees = nrow(prop_cem2[,covariate_names1]),
  sample.weights = prop_cem2$weights)

cf9 <- causal_forest(
  X = as.matrix(prop_cem2_k2k[,covariate_names1]),
  Y = prop_cem2_k2k$`Offer Yield to Maturity`,
  W = prop_cem2_k2k$`Green Flag`,
  num.trees = nrow(prop_cem2_k2k[,covariate_names1]))

# USDEUR matched without issuer
cf10 <- causal_forest(
  X = as.matrix(usdeur_prop_nnm[,covariate_names3]),
  Y = usdeur_prop_nnm$`Offer Yield to Maturity`,
  W = usdeur_prop_nnm$`Green Flag`,
  num.trees = nrow(usdeur_prop_nnm[,covariate_names3]))

cf11 <- causal_forest(
  X = as.matrix(usdeur_prop_nnm2[,covariate_names3]),
  Y = usdeur_prop_nnm2$`Offer Yield to Maturity`,
  W = usdeur_prop_nnm2$`Green Flag`,
  num.trees = nrow(usdeur_prop_nnm2[,covariate_names3]))

cf12 <- causal_forest(
  X = as.matrix(usdeur_prop_cem[,covariate_names3]),
  Y = usdeur_prop_cem$`Offer Yield to Maturity`,
  W = usdeur_prop_cem$`Green Flag`,
  num.trees = nrow(usdeur_prop_cem[,covariate_names3]),
  sample.weights = usdeur_prop_cem$weights)

cf13 <- causal_forest(
  X = as.matrix(usdeur_prop_cem_k2k[,covariate_names3]),
  Y = usdeur_prop_cem_k2k$`Offer Yield to Maturity`,
  W = usdeur_prop_cem_k2k$`Green Flag`,
  num.trees = nrow(usdeur_prop_cem_k2k[,covariate_names3]))

cf14 <- causal_forest(
  X = as.matrix(usdeur_prop_cem2[,covariate_names3]),
  Y = usdeur_prop_cem2$`Offer Yield to Maturity`,
  W = usdeur_prop_cem2$`Green Flag`,
  num.trees = nrow(usdeur_prop_cem2[,covariate_names3]),
  sample.weights = usdeur_prop_cem2$weights)

cf15 <- causal_forest(
  X = as.matrix(usdeur_prop_cem2_k2k[,covariate_names3]),
  Y = usdeur_prop_cem2_k2k$`Offer Yield to Maturity`,
  W = usdeur_prop_cem2_k2k$`Green Flag`,
  num.trees = nrow(usdeur_prop_cem2_k2k[,covariate_names3]))

# CBI matched without issuer
cf16 <- causal_forest(
  X = as.matrix(cbi_prop_nnm[,covariate_names4]),
  Y = cbi_prop_nnm$`Offer Yield to Maturity`,
  W = cbi_prop_nnm$`Green Flag`,
  num.trees = nrow(cbi_prop_nnm[,covariate_names4]))

cf17 <- causal_forest(
  X = as.matrix(cbi_prop_nnm2[,covariate_names4]),
  Y = cbi_prop_nnm2$`Offer Yield to Maturity`,
  W = cbi_prop_nnm2$`Green Flag`,
  num.trees = nrow(cbi_prop_nnm2[,covariate_names4]))

cf18 <- causal_forest(
  X = as.matrix(cbi_prop_cem[,covariate_names4]),
  Y = cbi_prop_cem$`Offer Yield to Maturity`,
  W = cbi_prop_cem$`Green Flag`,
  num.trees = nrow(cbi_prop_cem[,covariate_names4]),
  sample.weights = cbi_prop_cem$weights)

cf19 <- causal_forest(
  X = as.matrix(cbi_prop_cem_k2k[,covariate_names4]),
  Y = cbi_prop_cem_k2k$`Offer Yield to Maturity`,
  W = cbi_prop_cem_k2k$`Green Flag`,
  num.trees = nrow(cbi_prop_cem_k2k[,covariate_names4]))

cf20 <- causal_forest(
  X = as.matrix(cbi_prop_cem2[,covariate_names4]),
  Y = cbi_prop_cem2$`Offer Yield to Maturity`,
  W = cbi_prop_cem2$`Green Flag`,
  num.trees = nrow(cbi_prop_cem2[,covariate_names4]),
  sample.weights = cbi_prop_cem2$weights)

cf21 <- causal_forest(
  X = as.matrix(cbi_prop_cem2_k2k[,covariate_names4]),
  Y = cbi_prop_cem2_k2k$`Offer Yield to Maturity`,
  W = cbi_prop_cem2_k2k$`Green Flag`,
  num.trees = nrow(cbi_prop_cem2_k2k[,covariate_names4]))

# Data matched issuer matched
zerbib_psm <- as.data.frame(zerbib_psm)
cf22 <- causal_forest(
  X = as.matrix(zerbib_psm[,covariate_names1]),
  Y = zerbib_psm$`Offer Yield to Maturity`,
  W = zerbib_psm$`Green Flag`,
  num.trees = nrow(zerbib_psm[,covariate_names1]))

# USDEUR issuer matched
usdeur_zerbib_psm <- as.data.frame(usdeur_zerbib_psm)
cf23 <- causal_forest(
  X = as.matrix(usdeur_zerbib_psm[,covariate_names3]),
  Y = usdeur_zerbib_psm$`Offer Yield to Maturity`,
  W = usdeur_zerbib_psm$`Green Flag`,
  num.trees = nrow(usdeur_zerbib_psm[,covariate_names3]))

# CBI issuer matched
cbi_zerbib_psm <- as.data.frame(cbi_zerbib_psm)
cf24 <- causal_forest(
  X = as.matrix(cbi_zerbib_psm[,covariate_names4]),
  Y = cbi_zerbib_psm$`Offer Yield to Maturity`,
  W = cbi_zerbib_psm$`Green Flag`,
  num.trees = nrow(cbi_zerbib_psm[,covariate_names4]))

# 2) Check the nuisance parameters
# 2.1) Propensity score distribution plots
causaltree_output_list <- list(cf0, cf0.1, cf1, cf1.1, cf2, cf2.1, cf3, cf3.1, cf4, cf5, cf6, cf7, cf8, cf9, cf10, cf11, cf12, cf13, cf14, cf15, cf16, cf17, cf18, cf19, cf20, cf21, cf22, cf23, cf24)
for (i in 1:29) {
  df <- causaltree_output_list[[i]]
  plot(ggplot(data.frame(W.hat = df$W.hat, W = factor(df$W.orig))) +
    geom_histogram(aes(x=W.hat, y=stat(density), fill= W), alpha = 0.3,
                   position = "identity") +
    geom_density(aes(x=W.hat, color=W)) +
    theme_bw() +
    xlim(0,1) +
    ggtitle(paste0("Causal Forest Propensity Scores (W=Green Flag), Plot: ",i)))
}

plot(ggplot(data.frame(W.hat = df$W.hat, W = factor(df$W.orig))) +
       geom_histogram(aes(x=W.hat, y=stat(density), fill= W), alpha = 0.3,
                      position = "identity") +
       geom_density(aes(x=W.hat, color=W)) +
       scale_fill_manual(values = c("tan4", "darkgreen")) +
       theme_bw() +
       xlim(0,1) +
       ggtitle(paste0("Causal Forest Propensity Scores (W=Green Flag), Matched Data CBI with Zerbib PSM")))

# 2.2) Calibration Regressions
# if e.bar coeff = 1 -> average prediction correct
# if e.residual coeff = 1 -> heterogeneity (if any) correct
dataframe_list <- list(data, data2, data_matched, data_matched2, usdeur_matched, usdeur_matched2, cbi_matched, cbi_matched2,
                       prop_nnm, prop_nnm2, prop_cem, prop_cem_k2k, prop_cem2, prop_cem2_k2k,
                       usdeur_prop_nnm, usdeur_prop_nnm2, usdeur_prop_cem, usdeur_prop_cem_k2k, usdeur_prop_cem2, usdeur_prop_cem2_k2k,
                       cbi_prop_nnm, cbi_prop_nnm2, cbi_prop_cem, cbi_prop_cem_k2k, cbi_prop_cem2, cbi_prop_cem2_k2k,
                       zerbib_psm, usdeur_zerbib_psm, cbi_zerbib_psm)
for (i in 1:29) {
  # Loop over causal forest objects
  df <- causaltree_output_list[[i]]
  
  # Loop over dataframes
  df2 <- dataframe_list[[i]]
  
  # Extract data
  DF <- data.frame(
    W          = df2$`Green Flag`,
    e.bar      = mean(df$W.hat),
    e.residual = df$W.hat - mean(df$W.hat))

  # Regression
  best.linear.predictor <- lm(W ~ e.bar + e.residual + 0, data = DF)
  blp.summary.outcome <- lmtest::coeftest(best.linear.predictor,
                                          vcov = sandwich::vcovCL,
                                          type = "HC3")
  # Show output
  blp.summary.outcome
  stargazer(blp.summary.outcome)
  
  # Response function check
  # Extract data
  DF <- data.frame(
    Y          = df2$`Offer Yield to Maturity`,
    m.bar      = mean(df$Y.hat),
    m.residual = df$Y.hat - mean(df$Y.hat))

  # Regression
  best.linear.predictor <- lm(Y ~ m.bar + m.residual + 0, data = DF)
  blp.summary.prop <- lmtest::coeftest(best.linear.predictor,
                                       vcov = sandwich::vcovCL,
                                       type = "HC3")
  blp.summary.prop
  stargazer(blp.summary.prop)
}

# Calculate ATE
ATE0 = average_treatment_effect(cf0)
ATE0.1 = average_treatment_effect(cf0.1)
ATE1 = average_treatment_effect(cf1)
ATE1.1 = average_treatment_effect(cf1.1)
ATE2 = average_treatment_effect(cf2)
ATE2.1 = average_treatment_effect(cf2.1)
ATE3 = average_treatment_effect(cf3)
ATE3.1 = average_treatment_effect(cf3.1)
ATE4 = average_treatment_effect(cf4)
ATE4.1 = average_treatment_effect(cf4.1)
ATE5 = average_treatment_effect(cf5)
ATE6 = average_treatment_effect(cf6)
ATE7 = average_treatment_effect(cf7)
ATE8 = average_treatment_effect(cf8)
ATE9 = average_treatment_effect(cf9)
ATE10 = average_treatment_effect(cf10)
ATE11 = average_treatment_effect(cf11)
ATE12 = average_treatment_effect(cf12)
ATE13 = average_treatment_effect(cf13)
ATE14 = average_treatment_effect(cf14)
ATE15 = average_treatment_effect(cf15)
ATE16 = average_treatment_effect(cf16)
ATE17 = average_treatment_effect(cf17)
ATE18 = average_treatment_effect(cf18)
ATE19 = average_treatment_effect(cf19)
ATE20 = average_treatment_effect(cf20)
ATE21 = average_treatment_effect(cf21)
ATE22 = average_treatment_effect(cf22)
ATE23 = average_treatment_effect(cf23)
ATE24 = average_treatment_effect(cf24)

ATE_name <- ATE_name <- c("Data Universe without Matching", "Data Universe with Issuer Controls", "Data matched without Matching", "Data matched with Issuer Controls", "USDEUR without Matching", "USDEUR with Issuer Controls", "CBI without Matching", "CBI with Issuer Controls",
                          "Data_matched_prop_nnm", "Data_matched_prop_nnm2", "Data_matched_cem", "Data_matched_cem_k2k", "Data_matched_cem2", "Data_matched_cem2_k2k",
                          "USDEUR_prop_nnm", "USDEUR_prop_nnm2", "USDEUR_cem", "USDEUR_cem_k2k", "USDEUR_cem2", "USDEUR_cem2_k2k",
                          "CBI_prop_nnm", "CBI_prop_nnm2", "CBI_cem", "CBI_cem_k2k", "CBI_cem2", "CBI_cem2_k2k",
                          "Data matched issuer matched", "USDEUR issuer matched", "CBI issuer matched")

ATE_estimate <- c(ATE0[1],ATE0.1[1],ATE1[1],ATE1.1[1],ATE2[1],ATE2.1[1],ATE3[1],ATE3.1[1],ATE4[1],ATE5[1],ATE6[1],ATE7[1],ATE8[1],ATE9[1],ATE10[1],ATE11[1],ATE12[1],ATE13[1],ATE14[1],ATE15[1],ATE16[1],ATE17[1],ATE18[1],ATE19[1],ATE20[1],ATE21[1],ATE22[1],ATE23[1],ATE24[1])
ATE_stderr <- c(ATE0[2],ATE0.1[2],ATE1[2],ATE1.1[2],ATE2[2],ATE2.1[2],ATE3[2],ATE3.1[2],ATE4[2],ATE5[2],ATE6[2],ATE7[2],ATE8[2],ATE9[2],ATE10[2],ATE11[2],ATE12[2],ATE13[2],ATE14[2],ATE15[2],ATE16[2],ATE17[2],ATE18[2],ATE19[2],ATE20[2],ATE21[2],ATE22[2],ATE23[2],ATE24[2])
ATE_dataset <- c(0,0,1,1,2,2,3,3,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,1,2,3)
ATE_dataset1 <- c("Data Universe", "Data Universe", "Data Matched", "Data Matched", "USDEUR Matched", "USDEUR Matched", "CBI Matched", "CBI Matched", "Data Matched", "Data Matched", "Data Matched", "Data Matched", "Data Matched", "Data Matched", "USDEUR Matched", "USDEUR Matched", "USDEUR Matched", "USDEUR Matched", "USDEUR Matched", "USDEUR Matched", "CBI Matched", "CBI Matched", "CBI Matched", "CBI Matched", "CBI Matched", "CBI Matched", "Data Matched", "USDEUR Matched", "CBI Matched")
ATE_method <- c(0,1,0,1,0,1,0,1,2,3,4,5,6,7,2,3,4,5,6,7,2,3,4,5,6,7,8,8,8)
ATE_method1 <- c("Raw", "Issuer Controls", "Raw", "Issuer Controls", "Raw", "Issuer Controls", "Raw", "Issuer Controls", "PSM Zerbib", "PSM Extensive", "CEM Zerbib", "CEM k2k Zerbib", "CEM Extensive", "CEM k2k Extensive", "PSM Zerbib", "PSM Extensive", "CEM Zerbib", "CEM k2k Zerbib", "CEM Extensive", "CEM k2k Extensive", "PSM Zerbib", "PSM Extensive", "CEM Zerbib", "CEM k2k Zerbib", "CEM Extensive", "CEM k2k Extensive", "PSM Zerbib Issuer Cluster",  "PSM Zerbib Issuer Cluster",  "PSM Zerbib Issuer Cluster")
df_ATE <- data.frame(ATE_dataset, ATE_dataset1, ATE_method, ATE_method1, ATE_estimate, ATE_stderr)
df_ATE %>%
  gt() %>%
  gt_color_rows(ATE_estimate, palette = "RColorBrewer::Greens", reverse = TRUE) %>%
  gt_color_rows(ATE_dataset, palette = "RColorBrewer::Set3", reverse = TRUE) %>%
  gt_color_rows(ATE_method, palette = "RColorBrewer::Set3", reverse = TRUE)

# 3) Heterogeneity Assessment
# 3.1) Histogram of CATEs
for (i in 1:29) {
  # Loop over causal forest objects
  df <- causaltree_output_list[[i]]
  
  # Predict point estimates (training, out-of-bag)
  # not passing any data set -> Out-Of-Bag predictions
  oob_pred <- predict(df, estimate.variance=TRUE)
  oob_tauhat_cf <- oob_pred$predictions
  oob_tauhat_cf_se <- sqrt(oob_pred$variance.estimates)
  
  # Histogram of CATEs
  plot(ggplot(as.data.frame(oob_tauhat_cf), aes(x=oob_tauhat_cf)) +
    geom_histogram(aes(x = oob_tauhat_cf),fill="#00BFC4", alpha = 0.8) +
    theme_bw() +
    xlab("CATE estimate") + ylab("Frequency") +
    ggtitle(paste0("Causal forests: out-of-bag CATE (W=Tech Support), Plot: ",i)))
}

plot(ggplot(as.data.frame(oob_tauhat_cf), aes(x=oob_tauhat_cf)) +
       geom_histogram(aes(x = oob_tauhat_cf),fill="darkgreen", alpha = 0.8) +
       theme_bw() +
       xlab("CATE estimate") + ylab("Frequency") +
       ggtitle(paste0("Causal forests: out-of-bag CATE (W=Tech Support), Matched Data CBI with Zerbib PSM")))

# 3.2) Variable Importance
varname_list <- list(covariate_names2, covariate_names5, covariate_names1, covariate_names6, covariate_names3, covariate_names7, covariate_names4, covariate_names8,
                     covariate_names1, covariate_names1, covariate_names1, covariate_names1, covariate_names1, covariate_names1,
                     covariate_names3, covariate_names3, covariate_names3, covariate_names3, covariate_names3, covariate_names3,
                     covariate_names4, covariate_names4, covariate_names4, covariate_names4, covariate_names4, covariate_names4,
                     covariate_names1, covariate_names3, covariate_names4)
for (i in 1:29) {
  # Loop over causal forest objects
  df <- causaltree_output_list[[i]]
  
  # Loop over covariate lists
  varname <- varname_list[[i]]
  
  # Var Importance
  value <- c(variable_importance(df))
  value <- data.frame(value)
  df_var_imp <- cbind(varname,value)
  sorted_df_var_imp <- arrange(df_var_imp,desc(value))
  var_imp <- sorted_df_var_imp %>%
    head() %>%
    gt() %>%
    gt_color_rows(value) %>%
    tab_header(title = paste0("Variable Importance: Model ",i))
  print(var_imp)
}

value <- c(variable_importance(df))
value <- data.frame(value)
df_var_imp <- cbind(varname,value)
sorted_df_var_imp <- arrange(df_var_imp,desc(value))
var_imp <- sorted_df_var_imp %>%
  head() %>%
  gt() %>%
  gt_color_rows(value, palette = "RColorBrewer::Greens") %>%
  tab_header(title = paste0("Variable Importance: Matched Data CBI with Zerbib PSM"))
print(var_imp)
##################################################################
# 4) Heterogeneity across subgroups
# Manually creating subgroups
num_tiles <- 4  # ntiles = CATE is above / below the median
df2$cate <- oob_tauhat_cf
df2$ntile <- factor(ntile(oob_tauhat_cf, n=num_tiles))

# Estimating the average effect in the subgroups
# Sample ATE

ols_sample_ate <- lm_robust(`Offer Yield to Maturity` ~ ntile + ntile:`Green Flag`, data=df2)
estimated_sample_ate <- coef(summary(ols_sample_ate))[(num_tiles+1):(2*num_tiles), c("Estimate", "Std. Error")]
estimated_sample_ate <- data.frame(estimated_sample_ate)
hypothesis_sample_ate <- paste0("ntile1:`Green Flag` = ", paste0("ntile", seq(2, num_tiles), ":`Green Flag`"))
ftest_pvalue_sample_ate <- linearHypothesis(ols_sample_ate, hypothesis_sample_ate, test="F")[2,"Pr(>F)"]

# AIPW
estimated_aipw_ate <- lapply(seq(num_tiles), function(w) {
  ate <- average_treatment_effect(cf0.1, subset = df2$ntile == w)
})
estimated_aipw_ate <- data.frame(do.call(rbind, estimated_aipw_ate))

# Testing for equality using Wald test
## define L matrix that allows us to test if the ATEs in ntile 2+ are equal to ntile 1
.L <- cbind(-1, diag(num_tiles - 1))
# e.g. [,1] [,2] [,3] [,4]
# [1,]   -1    1    0    0
# [2,]   -1    0    1    0
# [3,]   -1    0    0    1
library(aod)
waldtest_pvalue_aipw_ate <- wald.test(Sigma = diag(estimated_aipw_ate$std.err^2),
                                      b = estimated_aipw_ate$estimate,
                                      L = .L)$result$chi2[3]

# Make a nice table
table.df <- data.frame(Tile  = 1:4,
                       Sample.ATE = round(estimated_sample_ate$Estimate, 2),
                       AIPW.ATE = round(estimated_aipw_ate$estimate, 2)
)
na.df <- data.frame(Tile = "", Sample.ATE = NA, AIPW.ATE = NA)

table.df <- do.call(rbind, apply(table.df, 1, function(x) {rbind(x, na.df)}))

stat.test <- c("", paste("F-Test p-value:", round(ftest_pvalue_sample_ate, 2)), paste("Wald-Test p-value:", round(waldtest_pvalue_aipw_ate, 2)))

table.df <- rbind(table.df,stat.test)

table.df[seq(from = 2, to = nrow(table.df)-1, by = 2),2] <- round(estimated_sample_ate$Std..Error, 2)
table.df[seq(from = 2, to = nrow(table.df)-1, by = 2),3] <- round(estimated_aipw_ate$std.err, 2)

table.df[seq(2,nrow(table.df),2),2:3] <- paste0("(", format(unlist(table.df[seq(2,nrow(table.df),2),2:3])),")")

library(formattable)
table.df1 <- formattable(table.df)
table.df1

# Print SATE and AIPW to compare:
box.df <- data.frame (N.tile = rep(1:4,2),
                      ATE.estimate = c(estimated_sample_ate$Estimate,estimated_aipw_ate$estimate),
                      Method = c(rep("Sample ATE", each=4), rep("AIPW ATE", each=4)),
                      se = c(estimated_sample_ate$Std..Error,estimated_aipw_ate$std.err)
)

ggplot(box.df, aes(x = N.tile, y = ATE.estimate, col = Method)) +
  geom_point(position = position_dodge(.3)) +
  geom_errorbar(aes(ymin = ATE.estimate-2.*se, ymax = ATE.estimate+2.*se), position = position_dodge(.3), width=.2) +
  theme(legend.position = 'none') +
  theme_bw() + ggtitle("ATE within N-tiles (as defined by predicted CATE)")

# Test statistical significance (different TEs)
colnames(estimated_sample_ate)[2]<-"SE"
colnames(estimated_aipw_ate) <-colnames(estimated_sample_ate)

p_values_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                dimnames = list(c(1:num_tiles),c(1:num_tiles)))
differences_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                   dimnames = list(c(1:num_tiles),c(1:num_tiles)))
stderror_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                dimnames = list(c(1:num_tiles),c(1:num_tiles)))
hypotheses_grid <- combn(1:num_tiles, 2)

estimated_aipw_ate <- mutate(estimated_aipw_ate, Ntile = c(1:num_tiles))

invisible(apply(hypotheses_grid, 2, function(x) {
  .diff <- with(estimated_aipw_ate, Estimate[Ntile == x[2]] - Estimate[Ntile == x[1]])
  .se <- with(estimated_aipw_ate, sqrt(SE[Ntile == x[2]]^2 + SE[Ntile == x[1]]^2))
  
  label1 <- paste0("tile", x[1])
  label2 <- paste0("tile", x[2])
  
  differences_tile_by_tile[x[2], x[1]] <<- .diff
  rownames(differences_tile_by_tile)[x[2]] <<- label2
  colnames(differences_tile_by_tile)[x[2]] <<- label2
  rownames(differences_tile_by_tile)[x[1]] <<- label1
  colnames(differences_tile_by_tile)[x[1]] <<- label1
  
  stderror_tile_by_tile[x[2], x[1]] <<- .se
  rownames(stderror_tile_by_tile)[x[2]] <<- label2
  colnames(stderror_tile_by_tile)[x[2]] <<- label2
  rownames(stderror_tile_by_tile)[x[1]] <<- label1
  colnames(stderror_tile_by_tile)[x[1]] <<- label1
  
  p_values_tile_by_tile[x[2], x[1]] <<- 1 - pnorm(abs(.diff/.se)) + pnorm(-abs(.diff/.se))
  rownames(p_values_tile_by_tile)[x[2]] <<- label2
  colnames(p_values_tile_by_tile)[x[2]] <<- label2
  rownames(p_values_tile_by_tile)[x[1]] <<- label1
  colnames(p_values_tile_by_tile)[x[1]] <<- label1
  
}))

p_values_tile_by_tile = round(p_values_tile_by_tile, digits = 2)
# Bonferroni correction should be applied to decide about the rejection

h1 <- ggplot(df2, aes(x=cate)) +
  geom_histogram(aes(fill=factor(ntile)), bins = 1000) +
  ggtitle("Quartiles") +
  theme_bw()

h1

# Clustering
# Elbow method
library(factoextra)
fviz_nbclust(as.data.frame(oob_tauhat_cf), kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# 4-5 seem to be optimal -> flattening afterwards

# See the results for k=4
clusters <- kmeans(as.data.frame(oob_tauhat_cf), centers = 4, nstart = 25)

df3 <- as.data.frame(oob_tauhat_cf)
df3 <- mutate(df3, cl = clusters$cluster)

h2 <- ggplot(df3, aes(x=oob_tauhat_cf)) +
  geom_histogram(aes(fill=factor(cl)),bins = 1000) +
  ggtitle("Clusters") +
  theme_bw() +
  xlab("cate")

h2

library(gridExtra)
grid.arrange(h1, h2, ncol=2)

# 5) Heterogeneity across covariates (ntiles)
# Note: the same procedure could be done for clusters after running a corresponding regression

hypothesis <- paste0("ntile1 = ntile", seq(2, num_tiles))

# Regress each covariate on ntile assignment to get the means
cov_means <- lapply(covariate_names1, function(covariate) {
  lm_robust(as.formula(paste0(covariate, ' ~ 0 + ntile')), data = df2)
})

names(cov_means) <- covariate_names1

# Extract the mean and standard deviation of each covariate per ntile (nicer to print to RMarkdown)
cov_table <- lapply(cov_means, function(cov_mean) {
  as.data.frame(t(coef(summary(cov_mean))[,c("Estimate", "Std. Error")]))
})

# Test
cov_ftest <- sapply(cov_means, function(cov_mean) {
  # Sometimes the regression has no residual SSE = 0,
  # we cannot perform an F-test
  tryCatch({
    linearHypothesis(cov_mean, hypothesis, test="F")[2, c("F", "Pr(>F)")]
  },
  error = function(cond){
    message(paste0("Error message during F-test for ", cov_mean$terms[[2]],"`:"))
    message(cond)
    return(c("F"=NA, "Pr(>F)"=NA))
  })
})

# Make a nice table
cov_table_df <- ldply(cov_table, data.frame)
cov_table_df = cbind(cov_table_df[,1], round(cov_table_df[,2:5], digits = 2))

cov_names <- c("Global.Flag","","Major.Flag","","SMC.Flag","","Commercial.Flag","","IT.Spend","","Employee.Count","","PC.Count","","Size",           
               "", "Discount", "") 

cov_table_df <- cov_table_df %>%
  add_column(covariates = cov_names,
             .before = "leaf1")

cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)] <- paste0("(", format(unlist(cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)])),")")

formattable(cov_table_df, list(area(col = 2:ncol(cov_table_df)) ~ color_tile("transparent", "pink")))

# 5) Partial dependence plots
# Split up continuous and binary variables
library(tidyr)
df2 <- as.data.frame(df2)
binary_covariates <- sapply(covariate_names6,
                            function(x) length(unique(df2[, x])) <= 2)

q <- 5 # Eval at minimum and quartiles (1+4)
evaluate_partial_dependency <- function(var_of_interest, is_binary) {
  if(is_binary){
    # Get two unique values for the variable
    x_grid <- sort(unique(df2[,var_of_interest]))
  } else {
    # Get quantile values for the variable
    x_grid <- quantile(df2[,var_of_interest], probs = seq(0, 1, length.out = q))
  }
  df_grid <- setNames(data.frame(x_grid), var_of_interest)
  df_grid <- data.frame(df_grid)
  
  # For the other variables, keep them at their median
  other_covariates <- covariate_names6[which(covariate_names6 != var_of_interest)]
  df_median <- data.frame(lapply(df2[,other_covariates], median))
  df_eval <- expand_grid(df_median, df_grid)

  df <- data.frame(df2[,covariate_names6])
  colnames <- colnames(df)
  
  # Predict the treatment effect
  pred <- predict(cf1.1, newdata=df_eval[,colnames], estimate.variance=TRUE)
  rbind('Tau Hat' = pred$predictions,
        'Std. Error' = sqrt(pred$variance.estimates))
}

# Make the table for non-binary variables
nonbinary_partial_dependency_tauhats <- lapply(covariate_names6[!binary_covariates],
                                               function(variable) evaluate_partial_dependency(variable, FALSE))

names(nonbinary_partial_dependency_tauhats) <- covariate_names6[!binary_covariates]

# Make the table for binary variables
binary_partial_dependency_tauhats <- lapply(covariate_names6[binary_covariates],
                                            function(variable) evaluate_partial_dependency(variable, TRUE))

names(binary_partial_dependency_tauhats) <- covariate_names6[binary_covariates]

# Plot one variable
var_interest <- "Size"

plotPartialDep <- function(grid, values, stderr) {
  pos <- c(1:length(grid))
  df <- as.data.frame(cbind(pos,values,stderr))
  ggplot(df, aes(x=pos, y=values)) +
    geom_errorbar(aes(ymin = values - 2*stderr, ymax = values + 2*stderr), color = "blue", width = 0.1) +
    geom_line(color="red") +
    scale_x_continuous(name = paste0("Effect of ", var_interest, " evaluated at min and quartiles"),
                       breaks = pos,
                       labels = as.character(grid)) + 
    ggtitle("Partial Dependence Plot") +
    theme_bw()
}

var_interest <- "2018"
q = 2
plotPartialDep(quantile(df2[,var_interest], probs = seq(0, 1, length.out = q)),
               binary_partial_dependency_tauhats[[var_interest]]["Tau Hat",],
               binary_partial_dependency_tauhats[[var_interest]]["Std. Error",])

q = 4
var_interest <- "Time to Maturity (Days)"

plotPartialDep(quantile(df2[,var_interest], probs = seq(0, 1, length.out = q)),
               nonbinary_partial_dependency_tauhats[[var_interest]]["Tau Hat",],
               nonbinary_partial_dependency_tauhats[[var_interest]]["Std. Error",])

# 6) Two-way tables
# 6.1) Model 1
covariate_subset <- c("Issue Amount", "2022", "Time to Maturity (Days)", "Utilities", "BBB", "2018") # Analyse for 5 most important vars for splitting
all_pairs <- combn(covariate_subset, m = 2)

# The CATE function’s value for x1 and x2 evaluated at combinations of 
#‘high’ and ‘low’ (e.g. HL is High-Low with x1 at its 80th percentile, 
# x2 at its 20th percentile). All other covariates are fixed at their medians.

evaluate_twoway_partial_dependency <- function(vars_of_interest) {
  # Create a grid of values: if continuous, quintiles; else, plot the actual values
  x_grids <- list(NULL, NULL)
  for(i in 1:2) {
    is_binary <- (length(unique(data[,vars_of_interest[i]])) <= 2)
    if(is_binary) {
      x_grids[[i]] <- sort(unique(data[,vars_of_interest[i]]))
    } else {
      x_grids[[i]] <- quantile(data[,vars_of_interest[i]], probs = c(0.2, 0.8))
    }
  }
  x_grids <- setNames(x_grids, vars_of_interest)
  df_grid <- do.call(expand.grid, x_grids)
  
  # For the other variables, keep them at their median
  other_covariates <- covariate_names2[which(!covariate_names2 %in% vars_of_interest)]
  df_median <- data.frame(lapply(data[,other_covariates], median))
  df_eval <- crossing(df_median, df_grid)
  
  df <- data.frame(data[,covariate_names2])
  colnames <- colnames(df)
  
  # Predict the treatment effect
  pred <- predict(cf0, newdata=df_eval[,colnames], estimate.variance=TRUE)
  res <- rbind('Tau Hat' = pred$predictions,
               'Std. Error' = sqrt(pred$variance.estimates))
  colnames(res) <- c("LL", "HL", "LH", "HH")
  res
  
}

twoway_partial_dependency_tauhats <- lapply(1:ncol(all_pairs),
                                            function(j) evaluate_twoway_partial_dependency(all_pairs[, j]))

names(twoway_partial_dependency_tauhats) <- sapply(1:ncol(all_pairs),
                                                   function(j) paste0(all_pairs[1,j],"-",all_pairs[2,j]))

# Plot one combination
var_interest1 <- "Size"
var_interest2 <- "IT.Spend"

plotTwoWay <- function(values, var1, var2) {
  x1 <- c(1,2,1,2)
  x2 <- c(1,1,2,2)
  df <- as.data.frame(cbind(x1,x2,values))
  ggplot(df, aes(x=x1, y=x2, fill = values)) +
    geom_tile() +
    scale_x_continuous(name = var1, breaks = c(1,2), labels = c("L", "H")) +
    scale_y_continuous(name = var2, breaks = c(1,2), labels = c("L", "H")) +
    ggtitle(paste0("Two way plot of ", var1, " and ", var2, " evaluated at 20th and 80th percentiles")) +
    theme_bw()
}

plotTwoWay(twoway_partial_dependency_tauhats[[paste0(var_interest1,"-",var_interest2)]]["Tau Hat",],
           var_interest1, var_interest2)
