#  -------------------------------------import data--------------------------------------
rm(list = ls())
library("data.table")
library("dplyr")
library('plyr')
library("bit64")
# to see SimNum in a normal form
library("lubridate")
fileNames <- dir("C:/Users/lenovo/Desktop/work/")
loc <- paste0("C:/Users/lenovo/Desktop/work/",fileNames)
raw_data <- NULL
for (i in loc){
  raw_data_temp <- fread(i, sep = ",", header = T)
  raw_data <- rbind(raw_data, raw_data_temp)
}
# --------------------------------------缺失值处理---------------------------------------
# 数据诊断
summary(raw_data)
#head(raw_data,n=50)
# 所有变量均无缺失值
# 重复性检验
# 无重复值
# 异常值检验
# 把速度大于120公里/小时的数据标记为缺失值

# filter(raw_data, Speed >120 )
# raw_data_test <- raw_data %>% filter(Longitude <  73.33 | Latitude < 3.51 )
# summary(raw_data_test)
raw_data <- raw_data %>% select(-V1) %>% filter(Latitude > 3.51 & Longitude > 73.33)
# nrow(raw_data[raw_data$Speed > 120,]) 等于107，全部标记为缺失值
raw_data[raw_data$Speed > 120,]$Speed <- NA
# GpsTime <- unique(raw_data$GpsTime)
# nrow(raw_data)
# length(GpsTime)
raw_data <- raw_data[!duplicated(raw_data$GpsTime),]
#raw_data_test <- raw_data[raw_data$del == 0,]
# raw_data_test <- raw_data
# raw_data_test$dMileage <- c(diff(raw_data_test$Mileage,1),0)
# raw_data_temp <- filter(raw_data_test, Speed == 0 & dMileage > 0)
# raw_data_temp <- filter(raw_data_test, Speed != 0 & dMileage == 0)
# head(raw_data, n =100)
# nrow(filter(raw_data,SimNum == 64610283798 ))
SimNum_sp <- as.character(c(64610283798,64610283811))
raw_data$SimNum <- as.character(raw_data$SimNum)
ymd_hm(raw_data$GpsTime[2])
# str(raw_data_test)
# raw_data_test <- raw_data
# 问题可能出在同一个GpsTime的类型不太一致，所以R强行把GpsTime变成了整数
raw_data_temp1 <- raw_data[raw_data$SimNum %in% SimNum_sp,]
raw_data_temp2 <- raw_data[!raw_data$SimNum %in% SimNum_sp,]
raw_data_temp1$GpsTime <- ymd_hm(raw_data_temp1$GpsTime)
raw_data_temp2$GpsTime <- ymd_hms(raw_data_temp2$GpsTime) 
raw_data <- rbind(raw_data_temp1,raw_data_temp2)
rm(list = c("raw_data_temp1","raw_data_temp2"))
# raw_data_test[raw_data_test$SimNum %in% SimNum_sp,]$GpsTime <- ymd_hm(raw_data_test[raw_data_test$SimNum %in% SimNum_sp,]$GpsTime, quiet = T)
# raw_data_test[!raw_data$SimNum %in% SimNum_sp,]$GpsTime <- ymd_hms(raw_data_test[!raw_data_test$SimNum %in% SimNum_sp,]$GpsTime)
# tail(raw_data)
# str(raw_data)
raw_data$year <- year(raw_data$GpsTime)
raw_data$month <- month(raw_data$GpsTime)
raw_data$day <- day(raw_data$GpsTime)
raw_data$hour <- hour(raw_data$GpsTime)
raw_data$minute <- minute(raw_data$GpsTime)
# str(raw_data)
# raw_data$second <- second(raw_data$GpsTime)
# remind to add "summarize"
# mean(GpsTime) is valid
raw_data <- ddply(raw_data, .(SimNum, year,month,day,hour,minute),summarize,
                  SimNum = SimNum[1],GpsTime = mean(GpsTime), Longitude = mean(Longitude),  
                  Latitude = mean(Latitude), Speed = mean(Speed),Mileage = mean(Mileage))
# 数据采集间隔接近1min G
head(raw_data,n=20)
nrow(raw_data)
summary(raw_data)
unique(table(diff(raw_data$GpsTime)))
a <- table(diff(raw_data$GpsTime))
time_interval <- unique(round(as.numeric(rownames(a)),0))
save(raw_data, file = "C:/Users/lenovo/Desktop/在线实习任务/raw_data.RData")
