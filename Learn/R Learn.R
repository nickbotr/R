# THIIS IS MY FIRST R SCRIPT

## I am about to load packages
# library(tidyverse)
# library(magrittr)
# library(caret)

#Creating path variable
path <- paste0("C:\\Users\\",Sys.getenv("USERNAME"),"\\Desktop\\")

#Creating file name variable
fname <- "A_and_E_data"

#Stitching the full file path together using paste0 function which acts as concatenate with 0 spaces. #handy
full <- paste0(path,fname,".csv")

#Set work directory
setwd(path)

# #Get work directory
# getwd()

# #Show full file path on console
# print(full)

#read.delim(full)

# CSV import into a variable
ED_Data <- read.csv(full)

#installing packages on script - case sensitive
# install.packages("RODBC")

#Cursory data analysis

summary(ED_Data)

# ED_Data <- ED_Data %>% 
#   drop_na() 

# #Ommit nulls from ED_Data and create new variable to store.
# ED_Data2 <- na.omit(ED_Data)

#Indexing / Slicing ED_Data[4 rows, 2 columns]
ED_Data[1:4,1:3]

#Chaining functions
ED_Data %>% 
  select(1:4) %>% 
  sample_n(40)

nrow(ED_Data)
ncol(ED_Data)


ED_Data$Age
str(ED_Data$Age)
hist(ED_Data$Age)

str(ED_Data)
