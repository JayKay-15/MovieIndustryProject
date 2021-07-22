##### working in R to find correlation between movie variables #####

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, caTools, lubridate,
               ggthemes, janitor, corrplot, corrgram, GGally)

rm(list=ls())
setwd("/Users/Jesse/R/MovieIndustryProject/")

# read in movie industry data
df <- read_csv("/Users/Jesse/R/MovieIndustryProject/movies.csv")

# checking out the data and structure
head(df)
str(df)
glimpse(df)

# check for missing data
any(is.na(df)) # returns FALSE - no missing data

### basic data cleaning
df$year_corrected <- as.numeric(str_sub(df$released, 1, 4)) # fixing years not matching

df <- df %>% arrange(desc(gross)) # sorting by highest grossing movies
options(scipen = 999) # get rid of scientific notation

get_dupes(df) # no duplicate values

#### looking for correlation ####

# scatterplot budget vs gross
scat_pl <- df %>% ggplot(aes(budget, gross)) + geom_point(color = "red", alpha = .5) + 
    ggtitle("Budget vs Gross Earnings",) + theme_bw() + theme(plot.title = element_text(hjust = .5)) +
    geom_smooth(method = "lm")
scat_pl

# correlation and correlation plots
df_cor <- df[,c(1,6,10,11,13,16)] # using only numeric features
cor(df_cor) # correlation matrix
ggpairs(df_cor) # matrix of plots

cor_mx <- cor(df_cor) 
corrplot(cor_mx,method = "color", title = "Correlation Matrix for Numeric Features", 
         mar=c(0,0,1,0)) # correlation plot


# create numeric factor for all character columns
numeric_factor <- function(x){
    as.numeric(as.factor(x))
}

df_numfac <- df # creating frame to change characters to factored numeric values
df_numfac[,c(2:5,7,8,12,14)] <- lapply(df_numfac[,c(2:5,7,8,12,14)], numeric_factor) # converting non-numeric
                                                                                     # to factored numeric

cor(df_numfac[,c(-9,-15)]) # correlation with all features - removing old release date and release year
cor_numfac_mx <- cor(df_numfac[,c(-9,-15)]) # creating correlation matrix 
corrplot(cor_numfac_mx, method = "color", title = "Correlation Matrix for All Features", 
         mar=c(0,0,1,0)) # correlation plot

cor_gross <- cor(df_numfac[,c(-6,-9,-15)], df_numfac$gross) # matrix of only correlation w/ gross
cor_gross <- as.matrix(cor_gross[order(cor_gross[,1], decreasing = T),]) # ordered by highest correlation
cor_gross

colnames(cor_gross) <- c("gross") # changing column name

high_cor_gross <- cor_gross[cor_gross[,1] >= .5,, drop = F] # budget and votes have highest correlation w/ gross
high_cor_gross




