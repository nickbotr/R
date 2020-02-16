

 # install.packages("readxl")
 # install.packages
# install.packages("tidyr")
# install.packages("stringr")

library("readxl")
library("anchors")
library("stringr")
library("tidyr")

path <- "D:\\Repos\\R\\Makeover Monday\\WorldWealth\\"

fName <- "WorldWealth"

full <- paste0(path,fName,".xlsx")

setwd(path)

Data <- read_excel(full, sheet = 1)

# summary(Data)

# str(Data)

Data$'Wealth ($B)' <- gsub("\\$", "", Data$'Wealth ($B)')
Data$'Wealth ($B)' <- gsub("\\,", "", Data$'Wealth ($B)')


# Data$'Wealth' <- as.numeric( Data$'Wealth ($B)')#replace.value( Data, "Wealth ($B)", from="*$*", to="", verbose = FALSE)

Data2_na <- na.omit(Data)

write.csv(Data2_na,paste0(path,"Modified_Data.csv"), row.names = FALSE)

# plot(Data2$"Birth year",Data2$"% of your life the US has been at war",xlab="Year",ylab="% of your life the US has been at war")
