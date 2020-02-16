start_time <- Sys.time()

options(warn=-1)

list.of.packages <- c("readxl", "caret", "partykit","data.table", "xts","tidyverse","forecast", "janitor","forecastHybrid","pracma","lubridate","odbc","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

####################### REDO

setwd("C:/Users/alfonso.portabales/Desktop/Draper/A&E Attendances")

library(readxl)
library(caret)
library(partykit)
library(data.table)
library(xts)
library(tidyverse)
library(forecast)
library(janitor)
library(forecastHybrid)
library(pracma)
library(lubridate)
library(readr)


#A_E$date <- excel_numeric_to_date(A_E$date)
library(odbc)
con <- odbc::dbConnect(odbc::odbc(),
                 Driver ="SQL Server",
                 Server ="ccdraper2.clevacloud.co.uk\\ccdraper2",
                 Database ="Alteryx",
                 UID = "Sa",
                 PWD = rstudioapi::askForPassword("Database password"),
                 Port = 1433)

result <- odbc::dbSendQuery(con, "SELECT  [Code]
     ,[Total attendances]
     ,[Date]
 FROM [Alteryx].[dbo].[ED]")
A_E <- odbc::dbFetch(result)

data.table::setnames(A_E, "Total attendances", "Attendances")
A_E$Attendances<-as.numeric(A_E$Attendances)

# A_E$temp <- paste0(A_E$date,A_E$Name)
# 
# 
# sums <- A_E %>%
#   group_by(temp) %>%
#   dplyr::summarise(sum = sum(`Total attendances`))
# 
# sums$date <- substr(sums$temp, 1, 10)
# sums$Name <- substr(sums$temp, 11, nchar(sums$temp))
# sums$temp <- NULL
# 
# A_E<- sums
# 
#setnames(A_E,"Total attendances","Attendances")
A_E$Name <- A_E$Code
oldNames<- unique(A_E$Name)
A_E$Name<-gsub("[[:punct:]]", "", A_E$Name)
A_E$Name<-gsub(" ","_",A_E$Name)
A_E$Name<-gsub("'","",A_E$Name)
A_E$Code<-NULL
A_E$Attendances<-ifelse(A_E$Attendances == 0, NA, A_E$Attendances)

foo <- with(A_E,by(Attendances,Name,function(xx)sum(!is.na(xx))))
A_E<-A_E[A_E$Name %in% names(which(foo>=12)),]
# 
# A_E$Zeros <- ifelse(A_E$Attendances <5,1,0)
# listZeros<-A_E$Name[A_E$Zeros==1]
# '%!in%' <- function(x,y)!('%in%'(x,y))
# A_E<-A_E[A_E$Name %!in% listZeros,]
# Order the dataset

keycol <-c("Name","date")
setorderv(A_E, keycol)

# Sub missing values by 

# for (i in 1:nrow(A_E)) {
#   if(is.na(A_E$Attendances[i]) & (strcmp(A_E[i,]$Name,A_E[i-7,]$Name))){
#     A_E$Attendances[i] <- A_E$Attendances[i-7] 
#   }
# }

#Clean NA´s
A_E<-A_E[complete.cases(A_E), ]

# Obtain list of sites
sites <- unique(A_E$Name)
numSites <- length(sites)





#Define variable
daysAhead<-6
results <- list()


for(i in 1:numSites){
  ## Generate TimeSeries and Test data
  assign(paste0("ts",sites[i]), A_E[A_E$Name %in% sites[i],1])
 # assign(paste0("test",sites[i]), ts(eval(parse(text = paste0("ts",sites[i],"[((nrow(",paste0("ts",sites[i]),")-",daysAhead,"):nrow(",paste0("ts",sites[i]),")),]"))),frequency=7))
#  assign(paste0("tss",sites[i]), ts(eval(parse(text = paste0("ts",sites[i],"[(1:(nrow(",paste0("ts",sites[i]),")-",daysAhead,")),]"))),frequency=7))
  assign(paste0("tss2",sites[i]), ts(eval(parse(text = paste0("ts",sites[i]))),frequency=12))
  #assign(paste0("tss",sites[i]), ts(eval(parse(text = paste0("ts",sites[i]))),frequency=1))
  
  ## We run five models per Trust
  #hybrid
  #assign(paste0("autoarima",sites[i]), auto.arima(eval(parse(text = paste0("tss",sites[i])))))
  #assign(paste0("predArima",sites[i]), forecast(eval(parse(text = paste0("autoarima",sites[i]))), h = daysAhead))
  # assign(paste0("autoarima2",sites[i]), auto.arima(eval(parse(text = paste0("tss2",sites[i])))))
  # assign(paste0("pred2Arima",sites[i]), forecast(eval(parse(text = paste0("autoarima2",sites[i]))), h = daysAhead))
  # #plot(eval(parse(text = paste0("predArima",sites[i]))), lty = c(1,3), col=c(5,2))
  # #TBATS
  # assign(paste0("tbats",sites[i]), tbats(eval(parse(text = paste0("tss",sites[i])))))
  # assign(paste0("predtbats",sites[i]), forecast(eval(parse(text = paste0("tbats",sites[i]))), h = daysAhead))
  # assign(paste0("tbats2",sites[i]), tbats(eval(parse(text = paste0("tss2",sites[i])))))
  # assign(paste0("pred2tbats",sites[i]), forecast(eval(parse(text = paste0("tbats2",sites[i]))), h = daysAhead))
  # #plot(eval(parse(text = paste0("predtbats",sites[i]))), lty = c(1,3), col=c(8,3))
  # # #ETS
  # # assign(paste0("ets",sites[i]), ets(eval(parse(text = paste0("tss",sites[i]))), model="MAM")) #Holt-Winters
  # assign(paste0("predets",sites[i]), forecast(eval(parse(text = paste0("ets",sites[i]))),h=daysAhead,method ='ets'))
  # assign(paste0("ets2",sites[i]), ets(eval(parse(text = paste0("tss2",sites[i]))), model="MAM")) #Holt-Winters
  # assign(paste0("pred2ets",sites[i]), forecast(eval(parse(text = paste0("ets2",sites[i]))),h=daysAhead,method ='ets'))
  # #plot(eval(parse(text = paste0("predets",sites[i]))), lty = c(1,3), col=c(3,6))
  # #Ensemble
  # assign(paste0("predEnsemble",sites[i]),(((1/3)*(eval(parse(text = paste0("data.frame(predArima",sites[i],")")))))+((1/3)*(eval(parse(text = paste0("data.frame(predtbats",sites[i],")")))))+((1/3)*(eval(parse(text = paste0("data.frame(predets",sites[i],")")))))))
  # assign(paste0("pred2Ensemble",sites[i]),(((1/3)*(eval(parse(text = paste0("data.frame(pred2Arima",sites[i],")")))))+((1/3)*(eval(parse(text = paste0("data.frame(pred2tbats",sites[i],")")))))+((1/3)*(eval(parse(text = paste0("data.frame(pred2ets",sites[i],")")))))))
  # # Hybrid
  # assign(paste0("hybrid",sites[i]), hybridModel(eval(parse(text = paste0("tss",sites[i])))))
  # assign(paste0("predhybrid",sites[i]), forecast(eval(parse(text = paste0("hybrid",sites[i]))), h = daysAhead))
   assign(paste0("hybrid2",sites[i]), hybridModel(eval(parse(text = paste0("tss2",sites[i])))))
   assign(paste0("pred2hybrid",sites[i]), forecast(eval(parse(text = paste0("hybrid2",sites[i]))), h = daysAhead))

 
} 


a<-data.frame(matrix(NA, nrow = numSites, ncol = 1))


for(i in 1:numSites){
    ## Generate the csv's
    a[i,1]<-1
    names(a)<-c("hybrid")         
    rownames(a)<-sites

}

#write.csv(a, file="MAPEByModelNHSI.csv")




for(i in 1:numSites){
  # if(hybrid %in% "Ensemble"){
  #   jpeg(paste0(sites[i],"pred", hybrid,".jpg"), width = 350, height = 350)
  #   graphics::plot(data.frame(cbind(data.frame(rownames(data.frame(rbind.data.frame(eval(parse(text = paste0("tss2",sites[i]))),eval(parse(text = paste0("pred", hybrid,sites[i],"$Point.Forecast")))))), rbind.data.frame(eval(parse(text = paste0("tss2",sites[i]))),eval(parse(text = paste0("pred", hybrid,sites[i],"$Point.Forecast"))))))), lty = c(1,3), col=c(5,2))
  #   #polygon(c(rev(eval(parse(text = paste0("pred", hybrid,sites[i])))), eval(parse(text = paste0("pred", hybrid,sites[i])))), c(rev(eval(parse(text = paste0("pred", hybrid,sites[i])))[ ,2]), eval(parse(text = paste0("pred", hybrid,sites[i]))[ ,3]), col = 'grey80', border = NA)
  #   dev.off()
  # }else{
  #   jpeg(paste0(sites[i],"pred", hybrid,".jpg"), width = 350, height = 350)
  #   plot(eval(parse(text = paste0("pred", hybrid,sites[i]))), lty = c(1,3), col=c(5,2))
  #   dev.off()
  # }
    assign(paste0("dtpred",sites[i]),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2","hybrid",sites[i],")")))),data.frame(sites[i])))
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),1))
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),"hybrid"))
    assign(paste0("dtpred",sites[i]),eval(parse(text = paste0("dtpred",sites[i],"[,c(1,2,4,3,5,6,7,8)]"))))

    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("data.frame(pred2","hybrid",sites[i],"$mean)"))))),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2","hybrid",sites[i],"$lower)"))))),data.frame(cbind(eval(parse(text=paste0("data.frame(pred2","hybrid",sites[i],"$upper)"))))),data.frame(sites[i])))
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),1))
    assign(paste0("dtpred",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dtpred",sites[i]))))),"hybrid"))
    #assign(eval(parse(text=paste0("names(dtpred",sites[i],"[1])"))),substr(1,4,eval(parse(text=paste0("names(dtpred",sites[i],"[1])")))))
  
  #assign(colnames(eval(parse(text=paste0("dtpred",sites[i])))),c("mean","high80","high95","site","low80","low95","forecasted"))
  assign(paste0("dthist",sites[i]),cbind(data.frame(eval(parse(text=paste0("ts",sites[i])))),data.frame(sites[i])))
  #assign(paste0("dthist",sites[i]),cbind(data.frame(cbind(eval(parse(text=paste0("dthist",sites[i]))))),0))
}



for(i in 1:numSites){
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), d = NA, .after = 2))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), e = NA, .after = 3))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), f = NA, .after = 4))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), g = NA, .after = 5))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), h = 0, .after = 6))
  assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), i = NA, .after = 7))
  #assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), j = NA, .after = 8))
  #assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), k = NA, .after = 9))
  # assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), l = NA, .after = 10))
  # assign(paste0("dthist",sites[i]),add_column(eval(parse(text=paste0("dthist",sites[i]))), m = NA, .after = 11))
  
  assign(paste0("dtpred",sites[i]),add_column(eval(parse(text=paste0("dtpred",sites[i]))), date = seq.Date(from = as.Date(max(A_E$date)%m+% months(1)) , by="month", length.out  = daysAhead)))
  #assign(paste0("dtpred",sites[i]),add_column(eval(parse(text=paste0("dtpred",sites[i]))), i = NA, .after = 8))
  #assign(paste0("dtpred",sites[i]),add_column(eval(parse(text=paste0("dtpred",sites[i]))), j = NA, .after = 9))
  # assign(paste0("dtpred",sites[i]),add_column(eval(parse(text=paste0("dtpred",sites[i]))), k = NA, .after = 10))
  
}

for(i in 1:numSites){
  #assign(paste0("dtpred",sites[i]),`names<-`(get(paste0("dtpred",sites[i])), c("Count","low80","low95","high80","high95","Name","forecasted","Date","Division","Day of week","model")))
  assign(paste0("dtpred",sites[i]),`names<-`(get(paste0("dtpred",sites[i])), c("Attendances","low80","low95","high80","high95","Name","forecasted","model","date")))
  
  assign(paste0("dthist",sites[i]),`names<-`(get(paste0("dthist",sites[i])), c("Attendances","Name","high95","high80","low80","low95","forecasted","model")))
  
  assign(paste0("dthist",sites[i]),left_join(get(paste0("dthist",sites[i])),A_E,copy=FALSE))
  
  #assign(paste0("dthist",sites[i]), eval(parse(text=paste0("dthist",sites[i],"[,-10]"))))
}
final<-data.frame()

for(i in 1:numSites){
  final<- rbind.data.frame(final,eval(parse(text=paste0("dthist",sites[i]))),eval(parse(text=paste0("dtpred",sites[i]))))
}
dthistkeycol <-c("Name","date")
setorderv(final, keycol)

final$date<- as.Date(final$date)
final$Attendances<-ifelse(final$Attendances<0,0, final$Attendances)
final$low80<-ifelse(final$low80<0,0, final$low80)
final$low95<-ifelse(final$low95<0,0, final$low95)
final$high80<-ifelse(final$high80<0,0, final$high80)
final$high95<-ifelse(final$high95<0,0, final$high95)
final$Name<-gsub("\\...*","",final$Name)
#final<-unique(final)




rownames(final)<-(1:nrow(final))


#gsub(final$Name, sites, oldNames)

write.csv(final, file = "A&E-Prediction.csv")
