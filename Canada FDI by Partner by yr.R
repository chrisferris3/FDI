### Canada FDI by country ###
library(tidyverse)
library(cansim)
library(xlsx)
library(lubridate)
library(formattable)

### Functions and Values ---

# ---- A function to trim the right side of a string ---- #

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#### Values
Max_year = 2018


### FTA
#### Pull ddata

### Pull the Canada data --- #####
fp <- "data/"
fn <- "Canada_FTA.csv"
fpn <- paste(fp,fn,sep="")


a <- as_tibble(read.table(file = fpn, sep=",", stringsAsFactors = FALSE,  header = TRUE))

names(a)

colnames(a)[1] <- c("Destination")

# drop second FTA from data set.
a <- subset(a, select = c(Destination, FTA_Flag, FTA, DIF_Yr, Date_In_Force, Year_Fraction))
a


### Annual data, 
b <- get_cansim("36-10-0008")

names(b)

b$REF_DATE <- as.numeric(b$REF_DATE)

b

# Rename variables that I intend to keep,
colnames(b)[4] <- c("FDI_types")
colnames(b)[5] <- c("Destination")

unique(b$FDI_types)

## level

FDI_Types_lvl <- c("Canadian direct investment abroad - Total Book Value",   
                   "Foreign direct investment in Canada - Total Book Value",
                   
                   "Canadian direct investment abroad - Equity",             
                   "Canadian direct investment abroad - Net Debt",          
                   "Canadian direct investment abroad - Debt assets",        
                   "Canadian direct investment abroad - Debt liabilities",  
                   
                   "Foreign direct investment in Canada - Equity",           
                   "Foreign direct investment in Canada - Net Debt",        
                   "Foreign direct investment in Canada - Debt assets",      
                   "Foreign direct investment in Canada - Debt liabilities")


FDI_Group <- function(element) {
  ## This groups into the FDI subgroups
  forcats::fct_collapse(element,
                        "Outbound_FDI" = c("Canadian direct investment abroad - Total Book Value"),
                        "Inbound_FDI" = c("Foreign direct investment in Canada - Total Book Value"),
                        "Other_Breakdowns" = c("Canadian direct investment abroad - Equity",             
                                               "Canadian direct investment abroad - Net Debt",          
                                               "Canadian direct investment abroad - Debt assets",        
                                               "Canadian direct investment abroad - Debt liabilities",  
                                               "Foreign direct investment in Canada - Equity",           
                                               "Foreign direct investment in Canada - Net Debt",        
                                               "Foreign direct investment in Canada - Debt assets",      
                                               "Foreign direct investment in Canada - Debt liabilities")
  )}




###### Group by Ag NAICS
b1 <- as_tibble(b %>%
                  mutate(FDI_type_grp = factor(b$FDI_types, levels = FDI_Types_lvl),
                         FDI_type_grp = FDI_Group(FDI_type_grp)
                         ))

# Setup a filter for Countries we have FTAs with, vs not with. 
b1 <- subset(b1, b1$FDI_type_grp != "Other_Breakdowns",
             select = c(REF_DATE, GEO, FDI_type_grp,
                        Destination, SCALAR_FACTOR,
                        VALUE)
             )


       
unique(b1$FDI_type_grp)

names(b1)

b1

b1$B_VALUE <- currency(b1$VALUE/1000, symbol = "$", digits = 1 )




p1b <- b1 %>%
  filter(Destination == "All countries" & REF_DATE >= "2010" ) %>%
  ggplot(aes(x = REF_DATE, y = B_VALUE, color = FDI_type_grp))+
  geom_point() +
  geom_line() +
    ylim(500, 1500)+
 #   facet_wrap (~FDI_type_grp)+
   geom_text(aes(label = B_VALUE), vjust = -1) +
  theme_bw()+
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())+
  labs(x = "Year",
       y = "Total Book Value of In/Outbound FDI ($billions) ",
       title = "Canada's Total Book Value of In/Outbound FDI ($billions)",
       caption = paste("Source: Statistics Canada, Table 36-10-0008-01"))


p1b


jpeg("images/FDI_BV_world.jpg", width = 800, height = 800, quality = 100, res = 100)
p1b
dev.off()


unique(b1$Destination)


### Merge the files
c <- left_join(b1, a, by = c("Destination"))

unique(c$FTA)

c

c1 <- subset(c, c$FTA_Flag == "FTA")


names(c1)


### Pull out the base year data


## Put the all industries GDP in a separate tibble
BaseYr_FDI <- c1 %>%
  filter(REF_DATE == DIF_Yr) %>%
  mutate(BY_VALUE = VALUE,
         BY = DIF_Yr)

names(BaseYr_FDI)

BaseYr_FDI2 <- subset(BaseYr_FDI, select = c(BY,  
                                             Destination,
                                             FDI_type_grp,
                                             BY_VALUE))


##### combine base Year with 
d <- left_join(c1, BaseYr_FDI2, by = c("Destination" ,"FDI_type_grp"))


# Index FDI Values against the Base year for each country
names(d)

d$FDI_Index <- round((d$VALUE/d$BY_VALUE)*100,0)
d$FDI_pc <- round(((d$VALUE/d$BY_VALUE)-1)*100,2)

names(d)

### chart 

p2a <- d %>%
  filter(Destination == "Mexico" | Destination == "United States") %>%
  ggplot(aes(x = REF_DATE, y = FDI_Index, color = Destination))+
  geom_point() +
  geom_line() +
  ylim(0, 2100)+
  geom_text(aes(label = ifelse(REF_DATE == "2018", FDI_Index, "")), vjust = -1) +
  geom_hline(yintercept=100, linetype="dashed", color = "blue") +
  facet_wrap (~FDI_type_grp) +
  theme_bw()+
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())+
  labs(x = "Year",
       y = "Index of FDI Book Value (Base Year = 100) ",
       title = "Index of FDI Book Value (Base Year = 100) by GEO for NAFTA",
       caption =  paste("Source: Statistics Canada, Table 36-10-0008-01, GOC; Calculations by EDW's Market Intelligence Department"))

p2a


p2b <- d %>%
  filter(Destination == "Mexico" | Destination == "United States") %>%
  ggplot(aes(x = REF_DATE, y = FDI_pc, color = Destination))+
  geom_point() +
  geom_line() +
  ylim(0, 2100)+
  geom_text(aes(label = ifelse(REF_DATE == "2018", FDI_pc, "")), vjust = -1) +
  geom_hline(yintercept=100, linetype="dashed", color = "blue") +
  facet_wrap (~FDI_type_grp) +
  theme_bw()+
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())+
  labs(x = "Year",
       y = "FDI Book Value % change from base Year",
       title = "FDI Book Value % change from base Year by GEO for NAFTA",
       caption =  paste("Source: Statistics Canada, Table 36-10-0008-01, GOC; 
                        Calculations by EDW's Market Intelligence Department"))

p2b



p2c <- d %>%
  filter((Destination == "Mexico" | Destination == "United States") & REF_DATE == "1994") %>%
  #filter( GeoUID == "602" & (ST_Grp == "Non-Residential" | ST_Grp == "Residential")) %>%  
  ggplot(aes(x = REF_DATE, y = VALUE, color = Destination))+
  geom_point() +
  geom_line() +
#  ylim(0, 2100)+
  geom_text(aes(label = ifelse(REF_DATE == "1994", VALUE, "")), vjust = -1) +
  geom_hline(yintercept=100, linetype="dashed", color = "blue") +
  facet_wrap (~FDI_type_grp) +
  theme_bw()+
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())


 # labs(x = "Year",
#       y = "Index of FDI Book Value (Base Year = 100) ",
#       title = "Index of FDI Book Value (Base Year = 100) by GEO for NAFTA",
#       caption =  paste("Source: Statistics Canada, Table 36-10-0008-01, GOC; Calculations by EDW's Market Intellige# nce Department"))

p2c



write.xlsx(d, 
           file ="data/Annual_FDI.xlsx",
           sheetName = "DATA",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)


jpeg("images/PVI_Wpg.jpg", width = 800, height = 800, quality = 100, res = 100)
p4
dev.off()


