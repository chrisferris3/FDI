### Canada Trade by country ###
library(tidyverse)
library(cansim)
library(xlsx)
library(lubridate)

### Functions and Values ---

# ---- A function to trim the right side of a string ---- #

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#### Values
Max_year = 2018
#Min_year = 2010


# GDP --------------- ###

### Convert Monthly data into annual data, after reducing the data set to chained 2012 dollars and seasonally adjusted at annual rates. 
CAN_Trade <- get_cansim("12-10-0011")

names(CAN_Trade)