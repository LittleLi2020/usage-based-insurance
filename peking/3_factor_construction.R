rm(list = ls())
load("C:/Users/lenovo/Desktop/在线实习任务/raw_data_val.RData")
load("C:/Users/lenovo/Desktop/在线实习任务/decri15.RData")
# head(raw_data_val,n=2)
# head(decri15,n=2)
library("bit64")
# ------------------------------------------------风险指标构建(每段行程)-----------------------------------------------------------
instability <- function(x){
  Speed_next <- x[-1]
  Speed_now <- x[-length(x)]
  fit <- lm(Speed_next ~ Speed_now)
  fit <- summary(fit)
  return(fit$r.squared)
}

#改了一下指标的顺序
raw_data_val$trip_id <- as.numeric(raw_data_val$trip_id)
raw_data_val$dtime <- c(diff(raw_data_val$GpsTime,1),"NA")
raw_data_val$del <- c(diff(raw_data_val$trip_id,1),1)
raw_data_val[raw_data_val$del != 0,]$dtime <- "NA"
raw_data_val$dtime <- as.numeric(raw_data_val$dtime)
val_trip_group <- dplyr::group_by(raw_data_val,trip_id)
val_trip_summary <- dplyr::summarise(val_trip_group,
                                     SimNum = SimNum[1],
                                     trip = trip[1],
                                     obs.num = length(trip_id),
                                     drivingHour = sum(dtime,na.rm = T)/3600,
                                     fatigue_drivingHour = (max(0,sum(dtime,na.rm = T) - 7200))/3600,
                                     meanSpeed = mean(Speed,na.rm = T),
                                     maxSpeed = max(Speed,na.rm = T),
                                     sdSpeed = sd(Speed,na.rm = T),
                                     jam_obs = length(Speed[Speed < 12])/length(Speed),
                                     fast_obs = length(Speed[Speed > 90])/length(Speed),
                                     instability_coef = instability(Speed),
                                     Morning = length(hour[hour >=7 & hour <9])/length(hour),
                                     Evening = length(hour[hour >=17 & hour < 20 ])/length(hour),
                                     Midnight = length(hour[hour == 23 | (hour >=0 & hour < 3)])/length(hour)
)
val_trip_summary <- as.data.frame(val_trip_summary)
val_trip_summary$trip_id <- as.character(val_trip_summary$trip_id)
trip_id_temp <- unique(val_trip_summary[is.na(val_trip_summary$instability_coef),])
val_trip_summary <- val_trip_summary[!val_trip_summary$trip_id %in% trip_id_temp$trip_id,]
save(val_trip_summary,file ="C:/Users/lenovo/Desktop/在线实习任务/val_trip_summary.RData")

# summary(val_trip_summary)
# filter(raw_data_val, trip_id == "646102838063") # remove
# filter(raw_data_val, trip_id == "6461028380244") # remove
# # raw_data_test <- filter(raw_data_val, trip_id == "6461028380327") # remove
# filter(raw_data_val, trip_id == "6461028382652") # remove
# filter(raw_data_val, trip_id == "64610283816355") # remove 

# hist(raw_data_test$Speed,breaks = 1000)
# summary(raw_data_test)
# instability <- function(x){
#   Speed_next <- x[-1]
#   Speed_now <- x[-length(x)]
#   fit <- lm(Speed_next ~ Speed_now)
#   fit <- summary(fit)
#   return(fit$r.squared)
# }
# # summary(decri15)
# # summary(raw_data_val)
# # head(decri15)
# # decri15$hour <- hour(decri15$start.time)
# # decri15$label <- NA
# # decri15[decri15$hour >= 7 & decri15$hour < 9,]$label <- "Morning"
# # decri15[decri15$hour >= 17 & decri15$hour < 20,]$label <- "Evening"
# # decri15[decri15$hour == 23 | (decri15$hour >= 0 & decri15$hour < 3) , ]$label <- "Morning"
# raw_data_val$trip_id <- as.numeric(raw_data_val$trip_id)
# raw_data_val$dtime <- c(diff(raw_data_val$GpsTime,1),"NA")
# raw_data_val$del <- c(diff(raw_data_val$trip_id,1),1)
# raw_data_val[raw_data_val$del != 0,]$dtime <- "NA"
# raw_data_val$dtime <- as.numeric(raw_data_val$dtime)
# val_trip_group <- dplyr::group_by(raw_data_val,trip_id)
# val_trip_summary <- dplyr::summarise(val_trip_group,
#                                 drivingHour = sum(dtime,na.rm = T)/3600,
#                                 fatigue_drivingHour = (max(0,sum(dtime,na.rm = T) - 7200))/3600,
#                                 meanSpeed = mean(Speed),
#                                 maxSpeed = max(Speed),
#                                 sdSpeed = sd(Speed),
#                                 jam_obs = length(Speed[Speed < 12])/length(Speed),
#                                 fast_obs = length(Speed[Speed > 90])/length(Speed),
#                                 instability_coef = instability(Speed),
#                                 SimNum = SimNum[1],
#                                 trip = trip[1],
#                                 Morning = length(hour[hour >=7 & hour <9])/length(hour),
#                                 Evening = length(hour[hour >=17 & hour < 20 ])/length(hour),
#                                 Midnight = length(hour[hour == 23 | (hour >=0 & hour < 3)])/length(hour),
#                                 obs.num = length(trip_id)
#                                 )
# # fit <- lm(Longitude ~ Latitude,data = raw_data_val) 
# # fit_1 <- summary(fit)
# # fit_1$r.squared
# ------------------------------------------------风险指标构建(每个驾驶人)-----------------------------------------------------------
# val_trip_summary <- as.data.frame(val_trip_summary)
# head(val_trip_summary)
# summary(val_trip_summary)
# library("bit64")
# head(decri15)
# val_trip_summary <- merge(val_trip_summary, decri15[c("span.distance","rest.mins","span.hours","trip_id")], by.x = "trip_id" , by.y = "trip_id")
# val_SimNum_group <- dplyr::group_by(val_trip_summary,SimNum)
# val_SimNum_summary <- dplyr::summarise(val_SimNum_group,
#                                        sum_Mileage = sum(span.distance),
#                                        sum_drivingHour = sum(drivingHour),
#                                        ratio_fatigue_drivingHour = sum(fatigue_drivingHour)/sum(drivingHour),
#                                        mean_Morning = sum(Morning*obs.num/sum(obs.num)),
#                                        mean_Evening = sum(Evening*obs.num/sum(obs.num)),
#                                        mean_Midnight = sum(Midnight*obs.num/sum(obs.num)),
#                                        mean_meanSpeed = sum(meanSpeed*obs.num/sum(obs.num)),
#                                        mean_sdSpeed = sum(sdSpeed*obs.num/sum(obs.num)),
#                                        mean_jam_obs =  sum(jam_obs*obs.num/sum(obs.num)),
#                                        mean_fast_obs = sum(fast_obs*obs.num/sum(obs.num)),
#                                        mean_instability = sum((instability_coef/mean(instability_coef,na.rm = T))*obs.num/sum(obs.num),na.rm = T),
#                                        max_maxSpeed = max(maxSpeed)
#                                        )
# val_SimNum_summary <- as.data.frame(val_SimNum_summary)
