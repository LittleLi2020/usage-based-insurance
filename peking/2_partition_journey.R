# -----------------------------------导入数据---------------------------------------------------------
rm(list = ls())
fileNames <- dir("C:/Users/lenovo/Desktop/RData")
loc <- paste0("C:/Users/lenovo/Desktop/RData/",fileNames)
raw_data_full <- NULL
for (j in loc){
  load(j)
  raw_data_full <- rbind(raw_data_full, raw_data)
}
# load("C:/Users/lenovo/Desktop/在线实习任务/raw_data.RData")
# summary(raw_data_full)
# head(raw_data_full)
# raw_data_test <- filter(raw_data_full,Mileage == 0)
# summary(raw_data_test)
# unique(raw_data_test$SimNum)
# hist(raw_data_test$Speed,breaks = 10000)
# Mileage = 0的观测速度大多数为0，这些可能是新车，不予处理
# --------------------------------间隔时间为 15 分钟试验-----------------------------------------------
# 打补丁
raw_data_test <- raw_data_full
raw_data_test$SimNum <- as.numeric(raw_data_test$SimNum)
# split_point=as.vector(diff(raw_data_test$GpsTime))
#间隔时间为 15 分
mytrip15<-function(x){
  split_point=which(as.vector(diff(x$GpsTime))>15*60)
  split_point=c(0,split_point,nrow(x))
  trip<-rep(c(1:(length(split_point)-1)),diff(split_point))
  return(trip)
}
raw_data_temp <- split(raw_data_test,raw_data_test$SimNum)
trip <- lapply(raw_data_temp,function(x){mytrip15(x)})
trip <- as.vector(unlist(trip))
raw_data_test=cbind(raw_data_test,trip)
# head(raw_data_test)
# summary(raw_data_test)

#每辆车的驾驶行程数量
tripnum<-function(x){
  tnum=length(unique(x$trip))
  return(tnum)
}
options(digits = 5)
#驾驶行程表
decri <- ddply(raw_data_test, .(SimNum, trip), summarize, start.time = GpsTime[1], stop.time = GpsTime[length(trip)],
               start.Mileage = Mileage[1], stop.Mileage = Mileage[length(trip)])
decri$SimNum <- as.numeric(decri$SimNum)
decri$span.hours <- as.numeric(difftime(decri$stop.time,decri$start.time,units = "hours"))
decri$span.distance <- decri$stop.Mileage - decri$start.Mileage
# head(decri)
# decri <- ddply(raw_data_test, .(SimNum, trip),summarize,start.time=GpsTime[1],stop.time=GpsTime[length(trip)],
#                 span.hours=as.numeric(difftime(raw_data$GpsTime[length(trip)],raw_data$GpsTime[1],units="hours")),
#                 distance=Mileage[length(trip)]-Mileage[1])
decri$rest.mins=as.numeric(c("NA",difftime(decri$start.time[-1],decri$stop.time[-length(decri[,1])],units="mins")))
decri$del <- c(1,diff(decri$SimNum,1))
str(raw_data_test)
decri[decri$del != 0, ]$rest.mins <- "NA"
# select(decri,SimNum, span.distance, span.hours)
# summary(decri)
decri_temp <- split(decri,decri$SimNum)
raw_trip_number15 <- lapply(decri_temp,function(x){tripnum(x)})
raw_trip_number15 <- unlist(raw_trip_number15)
######黎韬注，data.frame中传入SimNum = 和 number = 后已经可以实现数据框的自行命名，不需要再写colnames(raw_trip_number15)=c('SimNum','raw_trip_number15')
raw_trip_number15 <- data.frame(SimNum=unique(decri$SimNum),number=raw_trip_number15)
# colnames(raw_trip_number15)=c('SimNum','raw_trip_number15')

#删除span.hours等于0&span.hours极端值&distance等于0的trip
# spanhours_95pre=quantile(decri$span.hours,0.95)
decri15 <-filter(decri,span.hours > 0 & span.distance > 0)
# summary(decri15)
decri_temp=split(decri15,decri15$SimNum)
trip_number15=lapply(decri_temp,function(x){tripnum(x)})
trip_number15=unlist(trip_number15)
trip_number15=data.frame(SimNum=unique(decri15$SimNum),number=trip_number15)
# colnames(trip_number15)=c('SimNum','trip_number15')
tripnum15_compare=merge(raw_trip_number15,trip_number15,by.x='SimNum',by.y='SimNum',all=T)
# 检查merge后是否出现了缺失值
sum(is.na(tripnum15_compare))
colnames(tripnum15_compare) <- c("SimNum","number_before_filter","number_after_filter")
# tripnum15_compare[is.na(tripnum15_compare)]=0
# data_trip15=merge(decri15[which(colnames(data_trip15) %in% c("SimNum","trip"))],raw_data_test,by=c('SimNum','trip'))
# ------------------------------------------------生成unique的指标trip_id----------------------------------------------------
#####黎韬注，merge出来的结果是混乱的，它改变了raw_data_test中各个行驶时点的排序，一个可行的办法是改为使用
# raw_data_val <- raw_data_test[raw_data_test$trip_id %in% decri15$trip_id , ], 其中decri15$trip_id中全都是valid trip，
# 我们只取raw_data_test中含有decri15$trip_id的trip_id
raw_data_test$trip_id <- paste(raw_data_test$SimNum,raw_data_test$trip,sep = "")
decri15$trip_id <- paste(decri15$SimNum,decri15$trip,sep = "")
raw_data_val <- raw_data_test[raw_data_test$trip_id %in% decri15$trip_id , ]
# unique(raw_data_val$trip_id)
save(raw_data_val,file = "C:/Users/lenovo/Desktop/在线实习任务/raw_data_val.RData")
save(decri15,file = "C:/Users/lenovo/Desktop/在线实习任务/decri15.RData")

# --------------------------------间隔时间为 60 分钟试验-----------------------------------------
#### 黎韬注，已经选定十五分钟，检验六十分钟的代码以及画行驶路径的代码暂注释掉
# raw_data_test <- raw_data
# #间隔时间为 60 分
# mytrip60<-function(x){
#   split_point=which(as.vector(diff(x$GpsTime))>60*60)
#   split_point=c(0,split_point,nrow(x))
#   trip<-rep(c(1:(length(split_point)-1)),diff(split_point))
#   return(trip)
# }
# library('bit64')
# library('plyr')
# raw_data_temp=split(raw_data_test,raw_data_test$SimNum)
# trip=lapply(raw_data_temp,function(x){mytrip60(x)})
# trip=unlist(trip)
# raw_data_test=cbind(raw_data_test,trip)
# head(raw_data_test)
# 
# #每辆车的驾驶行程数量
# tripnum<-function(x){
#   tnum=length(unique(x$trip))
#   return(tnum)
# }
# 
# #驾驶行程表
# decri <- ddply(raw_data_test, .(SimNum, trip),summarize,start.time=GpsTime[1],stop.time=GpsTime[length(trip)],
#                span.hours=as.numeric(difftime(raw_data$GpsTime[length(trip)],raw_data$GpsTime[1],units="hours")),
#                distance=Mileage[length(trip)]-Mileage[1])
# decri$rest.mins=as.numeric(c("NA",difftime(decri$start.time[-1],decri$stop.time[-length(decri[,1])],units="mins"))) 
# summary(decri)  
# head(decri)
# decri_temp=split(decri,decri$SimNum)
# raw_trip_number60=lapply(decri_temp,function(x){tripnum(x)})
# raw_trip_number60=unlist(raw_trip_number60)
# raw_trip_number60=data.frame(SimNum=unique(decri$SimNum),number=raw_trip_number60)
# colnames(raw_trip_number60)=c('SimNum','raw_trip_number60')
# 
# #删除span.hours等于0&span.hours极端值&distance等于0的trip
# spanhours_95pre=quantile(decri$span.hours,0.95)
# decri60 <-filter(decri,span.hours>0 & span.hours<spanhours_95pre & distance>0)
# summary(decri60)
# decri_temp=split(decri60,decri60$SimNum)
# trip_number60=lapply(decri_temp,function(x){tripnum(x)})
# trip_number60=unlist(trip_number60)
# trip_number60=data.frame(SimNum=unique(decri60$SimNum),number=trip_number60)
# # colnames(trip_number60)=c('SimNum','trip_number60')
# # tripnum60_compare=merge(raw_trip_number60,trip_number60,by.x='SimNum',by.y='SimNum',all=T)
# # tripnum60_compare[is.na(tripnum60_compare)]=0
# # 
# # data_trip60=merge(decri60,raw_data_test,by=c('SimNum','trip'))
# # summary(data_trip60)
# # 
# # tripnum_compare=merge(tripnum15_compare,tripnum60_compare,by.x='SimNum',by.y='SimNum',all=T)
# 
# 
# # ------------------------------------驾驶行程表----------------------------------------------
# data_trip_64610283801=decri[which(decri$SimNum==64610283801),]
# data_trip_64610283801
# 
# # ------------------------------------绘制车辆轨迹图----------------------------------------------
# library(devtools)
# library(ggplot2)
# library(ggmap)
# #install_github('badbye/baidumap')
# library(baidumap)
# options(baidumap.key='aISNQNysfhnwdXX5TScvVltulhTzxiAn')
# data_trip=data_trip15
# data_trip=data_trip[which(data_trip$SimNum==64610283801),]
# trip1=data_trip[which(data_trip$trip==3),]
# trip1=trip1[order(trip1$Mileage),]
# summary(trip1)
# map_center1=colMeans(trip1[,c("Longitude","Latitude")])
# map1=getBaiduMap(map_center1,width = 800,height = 800,zoom = 13)
# ggmap(map1)+ geom_path(data = trip1,aes(x=Longitude,y=Latitude,color= -Speed),size=1.5)+
#   geom_point(data = trip1,aes(x=Longitude[1],y=Latitude[1]),size=3,color="green")+
#   geom_point(data = trip1,aes(x=Longitude[length(trip1$Longitude)],y=Latitude[length(trip1$Latitude)]),size=3,col="red")
# 
# trip2=data_trip[which(data_trip$trip==5),]
# trip2=trip2[order(trip2$Mileage),]
# summary(trip2)
# map_center2=colMeans(trip2[,c("Longitude","Latitude")])
# map2=getBaiduMap(map_center2,width = 600,height = 600,zoom = 13)
# ggmap(map2)+ geom_path(data = trip2,aes(x=Longitude,y=Latitude,color= -Speed),size=1.5)+
#   geom_point(data = trip2,aes(x=Longitude[1],y=Latitude[1]),size=3,color="green")+
#   geom_point(data = trip2,aes(x=Longitude[length(trip2$Longitude)],y=Latitude[length(trip2$Latitude)]),size=3,col="red")
