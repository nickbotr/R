

# install.packages("readxl")
# install.packages("anchors")

library("readxl")
library("anchors")

path <- paste0("C:\\Users\\",Sys.getenv("USERNAME"),"\\Desktop\\Makeover Monday\\")

fName <- "US life at war"

full <- paste0(path,fName,".xlsx")

setwd(path)

Data <- read_excel(full, sheet = 1)

summary(Data)

str(Data$"Birth year")


Data2 <- replace.value( Data, "% of your life the US has been at war", from=100, to=1, verbose = FALSE)

write.csv(Data2,paste0(path,"Modified_Data.csv"), row.names = FALSE)

plot(Data2$"Birth year",Data2$"% of your life the US has been at war",xlab="Year",ylab="% of your life the US has been at war")
