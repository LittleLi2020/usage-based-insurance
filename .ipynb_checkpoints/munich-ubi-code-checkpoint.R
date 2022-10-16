# %% [code]
# --------------------------------------------------------------------------------------------------------
# ---------------------------------------import data------------------------------------------------------
library(data.table)
library(lubridate)
loc <- "//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/UBI/rawdata/"
sect <- "//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/"

i<-4
loc_mon <- NULL
for(mon in paste0("2017-0",4,"-")){
  day <- c(paste0("0",1:9,".RData"),paste0(10:days_in_month(i),".RData"))
  days <- paste0(loc,mon,day)
  loc_mon <- c(loc_mon, days) 
  rm(days)
  #i=i+1
}
# 导入1月份的数据
raw_data <- NULL
for (j in loc_mon ){
  load(j)
  raw_data <- rbind(raw_data,raw_data_temp)
}

save(raw_data, file = paste0(sect, "raw_data_org_2017_04.RData"))
# --------------------------------------------------------------------------------------------------------
# -------------------------------------------------acceleration calculatin---------------------------------

library(plyr)
library(dplyr)
library(xlsx)
library(data.table)
library(lubridate)
# vld_trip_data <- trip_data[(!is.na(trip_data$total_journey_actual_score) & year(ymd_hms(trip_data$start_date)) == 2017 & 
#                               !is.na(ymd_hms(trip_data$start_date)) & trip_data$valid_journey == 'validJourney'),]
# vld_trip_data <- select(vld_trip_data, journey_id, start_date, total_journey_actual_score, length)
# vld_trip_data <- vld_trip_data[!duplicated(vld_trip_data$journey_id),]
# save(vld_trip_data, file = paste0("//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/",
#                                   "Vld_trip_data.RData")) 
# --------------------------- import data ----------------------------------------------------
rm(list = ls())
load(paste0("//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/", "raw_data_org_2017_04.RData"))

one_list <- unique(raw_data$journeyId)[1:40000]

raw_data<- raw_data[raw_data$journeyId %in% one_list,]

# raw_data<- raw_data[!raw_data$journeyId %in% one_list,]

load(paste0("//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/", "vld_trip_data.RData"))
raw_data_val <- raw_data[raw_data$journeyId %in% vld_trip_data$journey_id, ] 
trip_data <- vld_trip_data
rm(vld_trip_data)
rm(raw_data)

# ---------------------------- calculate start time -------------------------------------------
trip_hour <- trip_data %>% select(journey_id, start_date)
# summary(trip_hour$start_date)
trip_hour$start_date <- ymd_hms(trip_hour$start_date)
trip_hour$year <- year(trip_hour$start_date)
trip_hour <- filter(trip_hour, year == 2017)
#summary(trip_hour)
trip_hour$hour <- hour(trip_hour$start_date)
trip_hour$label <- NA
trip_hour <- within(trip_hour,{
  label[hour >= 0  & hour < 6 ] <- "midnight"
  label[hour >= 7  & hour < 9  ] <- "morning"
  label[hour >= 11 & hour < 14] <- "midday"
  label[hour >= 17 & hour < 20] <- "afternoon"
  label[hour >= 20 & hour < 24] <- "evening"
})
#summary(trip_hour$label)
trip_hour[is.na(trip_hour$label),]$label <- "others"
save(trip_hour, file = paste0("//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/", "trip_hour_2017_04_p1.RData"))
# ---------------------------- calculate distance----------------------------------------------
# get latitude and longitude
temp <- select(raw_data_val, journeyId,lat,log)
temp$journeyId <- as.numeric(temp$journeyId)
# split temp into pieces of journeyId
loglat_temp <- split(temp,temp$journeyId)
# use funciton lapply to manipulate objects(that is dataframes) in list, and get latitude in last seconds 
lat1 <- lapply(loglat_temp, function(x){
  c(x$lat[-dim(x)[1]],0)
})
# use funciton lapply to manipulate objects(that is dataframes) in list, and get latitude in next seconds 
lat2 <- lapply(loglat_temp, function(x){
  c(x$lat[-1],0)
})

temp$lat1 <- unlist(lat1)
temp$lat2 <- unlist(lat2)

# the same as longitude 
log1 <- lapply(loglat_temp, function(x){
  c(x$log[-dim(x)[1]],0)
})
log2 <- lapply(loglat_temp, function(x){
  c(x$log[-1],0)
})
rm(loglat_temp)
temp$log1 <- unlist(log1)
temp$log2 <- unlist(log2)

options(digits = 22)
dist_prepare <- with(temp,{
  dist_prepare <- sin(lat1*pi/180)*sin(lat2*pi/180)+cos(lat1*pi/180)*cos(lat2*pi/180)*cos(log2*pi/180-log1*pi/180)
  ifelse(dist_prepare > 1, 1, dist_prepare)
})
dist <- acos(dist_prepare)*6371000
# summary(dist)
raw_data_val <- cbind(raw_data_val, dist)
options(digits = 5)
# -----------------------------------------------transform -------------------------------------------
raw_data_val$dtime <- c(diff(raw_data_val$timestamp,1),1)
raw_data_val$dspeed <- c(diff(raw_data_val$speed,1),0)
raw_data_val$del <- c(diff(raw_data_val$journeyId,1),1)
# delete some observations with wrong distance and acceleration 
raw_data_test <- raw_data_val[raw_data_val$del == 0,]
# 16923597 obs. of 16 variables
# ----------------------------------------------calculate rd_acce and tg_acce------------------------
temp_del <- raw_data_test %>% filter(dtime < 0)
jny_del <- unique(temp_del$journeyId)
raw_data_test <- raw_data_test[!raw_data_test$journeyId %in% jny_del,]
raw_data_test <- filter(raw_data_test, dtime !=0)
# 15514824 obs. of 16 variables
# calculate rd_acce and tg_acce
raw_data_test <- within(raw_data_test,{
  rd_acce <- heading/dtime*pi/180*speed
  tg_acce <- dspeed/dtime
})
# -------------------------------------data cleaning for dtime------------------------------
raw_data_test$del <- c(diff(raw_data_test$journeyId,1),1)
# delete
raw_data_test <- raw_data_test[raw_data_test$del == 0,]
# 15498620 obs. of 18 variables
# summary(raw_data_test$dtime)
raw_data_test <- raw_data_test[!raw_data_test$journeyId %in% unique(raw_data_test[raw_data_test$dtime > 600,"journeyId"]),]
# 15496504 obs.
# summary(raw_data_test)
# -------------------------------------data cleaning for speed------------------------------
raw_data_test[raw_data_test$speed == -1,]$speed <- 0 
# summary(raw_data_test$speed)
save(raw_data_test,file = "G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/raw_data_test_2017_04_p1.RData")

# --------------------------------------------------------------------------------------------------------
# ---------------------------------------feature_engineering------------------------------------------------
# --------------------------------------读入数据------------------------------------------------------
rm(list = ls())
load("G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/raw_data_test_2017_03_p2.RData") 
load(paste0("//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/", "vld_trip_data.RData"))
trip_data <- vld_trip_data
load(paste0("//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/", "trip_hour_2017_03_p2.RData"))
# ------------------------------------------统计量识别--------------------------------------------------
# ------------------------------------每段出行的行驶里程-------------------------------
# 距离单位是m
# 生成各段journeyId的里程
mileage_journey <- ddply(raw_data_test,.(journeyId),function(x){sum(x$dist)})
names(mileage_journey)[which(names(mileage_journey) == "V1")] <- "mileage"

trip_score_length <- trip_data 
mileage_journey <- merge(mileage_journey,trip_score_length, by.x ="journeyId", by.y = "journey_id")
ratio <- (mileage_journey$mileage - mileage_journey$length*1000)/(mileage_journey$length*1000)
mileage_journey$ratio <- ratio
#nrow(mileage_journey[abs(mileage_journey$ratio) > 0.5,])
jny_del <-  unique(mileage_journey[abs(mileage_journey$ratio) > 0.5,]$journeyId)
# hist(ratio,breaks = 1000)
raw_data_test <- raw_data_test[!raw_data_test$journeyId %in% jny_del,]
mileage_journey <- mileage_journey[!mileage_journey$journeyId %in% jny_del, ]
# 15484554 obs. 
#save(mileage_journey,file = paste0("C:/Users/ny42068/work/UBI/","mileage_journey_2017_01.RData"))

# ----------------------------------每段出行的行驶时间--------------------------------
driveTime_journey <- ddply(raw_data_test, .(journeyId), 
                           function(x){
                             sum(x$dtime)
                           })
names(driveTime_journey)[which(names(driveTime_journey) == "V1")] <- "driveTime"
#save(driveTime_journey,file = paste0("C:/Users/ny42068/work/UBI/","driveTime_journey_2017_01.RData"))
# summary(driveTime_journey)
# -----------------------------------每段出行的行驶范围----------------------------------
range_journey <- ddply(raw_data_test,.(journeyId),
                       function(x){
                         max(x$lat)+max(x$log)-min(x$lat)-min(x$log)
                       })
names(range_journey)[which(names(range_journey)== "V1")] <- "range"
#head(range_journey)
#save(range_journey,file = paste0("C:/Users/ny42068/work/UBI/","range_journey_2017_01.RData"))
# ------------------------------------每段出行的平均、最大、标准差、MAE、最小、各个分位点速度------------------------------
speed_group <- dplyr::group_by(raw_data_test, journeyId)
speed_distribution <- dplyr::summarise(speed_group,
                                       speed_average = mean(speed),
                                       speed_max = max(speed),
                                       speed_min = min(speed),
                                       speed_sd = sd(speed),
                                       speed_MAE = mean(abs(speed-mean(speed))),
                                       speed_ten_percentile = quantile(speed,0.1),
                                       speed_thirty_percentile = quantile(speed,0.3),
                                       speed_seventy_percentle = quantile(speed,0.7),
                                       speed_ninty_percentie = quantile(speed,0.9))
speed_journey <- data.frame(speed_distribution)
names(speed_journey) <- c("journeyId","mean_speed","max_speed","min_speed","sd_speed","MAE_speed","tenPercentile_speed",
                          "thirtyPercentile_speed","seventyPercentile_speed",
                          "nintyPercentile_speed")
#save(speed_journey,file = paste0("C:/Users/ny42068/work/UBI/","speed_journey_2017_01.RData"))
# ------------------------------------每段出行的平均、最大、标准差、MAE、最小、各个分位点切向加速度------------------------------
rd_acce_group <-dplyr:: group_by(raw_data_test, journeyId)
rd_acce_distribution <- dplyr:: summarise(rd_acce_group,
                                          rd_acce_average <- mean(rd_acce),
                                          rd_acce_max = max(rd_acce),
                                          rd_acce_min = min(rd_acce),
                                          rd_acce_sd = sd(rd_acce),
                                          rd_acce_MAE = mean(abs(rd_acce-mean(rd_acce))),
                                          rd_acce_ten_percentile = quantile(rd_acce,0.1),
                                          rd_acce_thirty_percentile = quantile(rd_acce,0.3),
                                          rd_acce_seventy_percentle = quantile(rd_acce,0.7),
                                          rd_acce_ninty_percentle = quantile(rd_acce,0.9)
)
rd_acce_journey <- data.frame(rd_acce_distribution)
names(rd_acce_journey) <- c("journeyId","mean_rd","max_rd","min_rd","sd_rd","MAE_rd","tenPercentile_rd","thirtyPercentile_rd",
                            "seventyPercentile_rd",
                            "nintyPercentile_rd")
#save(rd_acce_journey,file = paste0("C:/Users/ny42068/work/UBI/","rd_acce_journey.RData_2017_01"))
# ------------------------------------每段出行的平均、最大、标准差、MAE、最小、各个分位点向心加速度------------------------------
tg_acce_group <- dplyr::group_by(raw_data_test, journeyId)
tg_acce_distribution <- dplyr::summarise(tg_acce_group,
                                         tg_acce_average = mean(tg_acce),
                                         tg_acce_max = max(tg_acce),
                                         tg_acce_min = min(tg_acce),
                                         tg_acce_sd = sd(tg_acce),
                                         tg_acce_MAE = mean(abs(tg_acce-mean(tg_acce))),
                                         tg_acce_ten_percentile = quantile(tg_acce,0.1),
                                         tg_acce_thirty_percentile = quantile(tg_acce,0.3),
                                         tg_acce_seventy_percentle = quantile(tg_acce,0.7),
                                         tg_acce_ninty_percentle = quantile(tg_acce,0.9)
)
tg_acce_journey <- data.frame(tg_acce_distribution)
names(tg_acce_journey) <- c("journeyId","mean_tg","max_tg","min_tg","sd_tg","MAE_tg","tenPercentile_tg",
                            "thirtyPercentile_tg","seventyPercentile_tg",
                            "nintyPercentile_tg")
#save(tg_acce_journey,file = paste0("C:/Users/ny42068/work/UBI/","tg_acce_journey.RData_2017_01"))
# ------------------------------------每段出行的平均、最大、标准差、MAE、最小、各个分位点jerk------------------------------
jerk <- raw_data_test %>% select(journeyId, tg_acce,dtime)
jerk$dtg_acce <- c(diff(jerk$tg_acce,1),0)
jerk$del <- c(diff(jerk$journeyId,1),1)
jerk <- jerk[jerk$del == 0,]
jerk$jerk <- jerk$dtg_acce/jerk$dtime
jerk_group <- dplyr::group_by(jerk, journeyId)
jerk_distribution <- dplyr::summarise(jerk_group,
                                      jerk_average = mean(abs(jerk)),
                                      jerk_max = max(abs(jerk)),
                                      jerk_seventy_percentle = quantile(abs(jerk),0.7),
                                      jerk_ninty_percentle = quantile(abs(jerk),0.9)
)
jerk_journey <- data.frame(jerk_distribution)
names(jerk_journey) <- c("journeyId","mean_jerk","max_jerk","seventyPercentile_jerk","nintyPercentile_jerk")
#save(jerk_journey,file = paste0("C:/Users/ny42068/work/UBI/","jerk_journey_2017_01.RData"))
# ------------------------------------每段出行急加速次数、急减速、急转弯次数-------------------------------------------------------
# rapidAcce_group <- dplyr::group_by(raw_data_test, journeyId)
# rapidAcce_distribution <- dplyr::summarise(rapidAcce_group,
#                                     # 急加速
#                                     acce_times <- length(tg_acce[tg_acce > 2]),
#                                     # 急减速
#                                     dece_times <- length(tg_acce[tg_acce < -2]),
#                                     # 急转弯
#                                     turn_times <- length(rd_acce[rd_acce > 3])
#                                     ) 
# rapidAcce_seconds_journey <- data.frame(rapidAcce_distribution)
# names(rapidAcce_seconds_journey) <- c("journeyId","acce_times","dece_times","turn_times")
# save(rapidAcce_seconds_journey,file = paste0("C:/Users/ny42068/work/UBI/","rapidAcce_seconds_journey_2017_02.RData"))                                    
# -----------------------------------是否在凌晨、早高峰、午间、晚高峰、晚上行驶--------------------------------------------
driveLabel_journey <- trip_hour %>% select(journey_id, label)
driveLabel_journey <- merge(driveLabel_journey, mileage_journey[1], by.x = "journey_id", by.y ="journeyId" )
#save(driveLabel_journey,file = paste0("C:/Users/ny42068/work/UBI/","driveLabel_journey_2017_01.RData"))    
# ------------------------------每段行程恶劣行驶次数--------------------------
# 认为等效加速度大于2m/s^2为一次恶劣驾驶行为

harshDriving_seconds_journey <- plyr::ddply(raw_data_test,.(journeyId),function(x){
  length(sqrt(x$tg_acce^2 + x$rd_acce^2)[sqrt(x$tg_acce^2 + x$rd_acce^2) > 2])
})

names(harshDriving_seconds_journey)[which(names(harshDriving_seconds_journey) == "V1")] <- "harshDriving_seconds"
#save(harshDriving_seconds_journey,file = paste0("C:/Users/ny42068/work/UBI/","harshDriving_seconds_journey_2017_01.RData"))
# ----------------------------每段行程静止、低速行驶时间占比---------------------------
# 静止、低速行驶时间的单位是s
slowTime_journey <- plyr::ddply(raw_data_test, .(journeyId),
                                function(x){       
                                  sum(x$dtime[x$speed < 5.56])/driveTime_journey$driveTime[driveTime_journey$journeyId == x$journeyId[1]]
                                })
names(slowTime_journey)[which(names(slowTime_journey) == "V1")] <- "slowTime"
#save(slowTime_journey,file = paste0("C:/Users/ny42068/work/UBI/","slowTime_journey.RData_2017_01"))
# ----------------------------连续行驶两个小时以上的时间占比--------------------
fatigue_journey <-plyr::ddply(raw_data_test, .(journeyId),
                              function(x){
                                max(sum(x$dtime)-7200,0)/driveTime_journey$driveTime[driveTime_journey$journeyId == x$journeyId[1]]
                              })
names(fatigue_journey)[which(names(fatigue_journey) == "V1")] <- "fatigue"
#save(fatigue_journey,file = paste0("C:/Users/ny42068/work/UBI/","fatigue_journey.RData_2017_01"))
# -----------------------------驾驶平稳度-----------------------------------------------------------------
instability <- function(x){
  speed_actual <- x$speed[c(-1,-2)]
  tg_acce_temp <- x$tg_acce[-length(x$tg_acce)]
  dtime_temp <- x$dtime[-1]
  speed_temp <- x$speed[-1]
  speed_pred <- speed_temp + tg_acce_temp * dtime_temp
  speed_pred <- speed_pred[-length(speed_pred)]
  #return(speed_pred)
  sum(abs(speed_pred - speed_actual))/length(speed_pred)
}
instability_journey <- plyr::ddply(raw_data_test, .(journeyId), function(x){
  instability(x)
})
names(instability_journey)[which(names(instability_journey) == "V1")] <- "instability"
#save(instability_journey,file = paste0("C:/Users/ny42068/work/UBI/","instability_journey_2017_01.RData"))
# ------------------------------------------事件识别-------------------------------------------------------
# ---------------------------------------每次出行急转弯事件个数--------------------------------------------
# recognization function
# function1 
rapidTurn_begin_identify <- function(x){
  rapidTurn_begin_index <- NULL
  rapidTurn_index <- which(x$rd_acce > 3)
  if(!length(rapidTurn_index)){
    return(rapidTurn_begin_index)
  }
  
  if(length(rapidTurn_index) == 1){
    rapidTurn_begin_index <- rapidTurn_index
    return(rapidTurn_begin_index)
  }
  
  for (i in 1:(length(rapidTurn_index)-1)){
    for (j in rapidTurn_index[i]:rapidTurn_index[i+1]){
      if(x$rd_acce[j] < 2.5 ){
        rapidTurn_begin_index <- c(rapidTurn_begin_index,rapidTurn_index[i])
        break
      }
    }
  }
  
  if(!is.na(x$rd_acce[rapidTurn_index[length(rapidTurn_index)] + 1])){
    for (n in rapidTurn_index[length(rapidTurn_index)]: length(x$rd_acce)){
      if(x$rd_acce[n] < 2.5 ){
        rapidTurn_begin_index <- c(rapidTurn_begin_index,rapidTurn_index[length(rapidTurn_index)])
        break
      }
    }
  }
  else if(is.na(x$rd_acce[rapidTurn_index[length(rapidTurn_index)] + 1])){
    rapidTurn_begin_index <- c(rapidTurn_begin_index,length(x$rd_acce))
  }
  return(rapidTurn_begin_index)
}


# function 2 
rapidTurn_identify_pro <- function(x){
  begin_point <- x >= 3
  end_point <- x < 2.5
  begin_point_id <- cumsum(begin_point)
  point <- data.frame(end_point,begin_point_id)
  point <-  dplyr::group_by(point, begin_point_id)
  turn_recognize <-  dplyr::summarise(point, 
                                      comb = any(end_point),
                                      number = length(end_point))
  turn_recognize <- as.data.frame(turn_recognize)
  ifelse(turn_recognize[nrow(turn_recognize),]$number == 1, turn_recognize[nrow(turn_recognize),]$comb <- T, 
         turn_recognize[nrow(turn_recognize),]$comb)
  ifelse(turn_recognize[1,]$begin_point_id  == 0, turn_recognize[1,]$comb <-  F, 
         turn_recognize[1,]$comb)
  
  begin_point_id_actual <- turn_recognize[turn_recognize$comb == T, ]$begin_point_id
  begin_point_index <- match(begin_point_id_actual, begin_point_id)
  return(begin_point_index)
}


rapidTurn_events_journey <- plyr::ddply(raw_data_test, .(journeyId),
                                        function(x){
                                          length(rapidTurn_begin_identify(x))
                                        }
)

names(rapidTurn_events_journey)[2] <- "rapidTurn_events_times"
#save(rapidTurn_events_journey,file = paste0("C:/Users/ny42068/work/UBI/","rapidTurn_journey_2017_01.RData"))


rapidTurn_journey_2 <- ddply(raw_data_test, .(journeyId),
                             function(x){
                               length(rapidTurn_identify_pro(x$rd_acce))
                             }
)


# ----------------------------------------每次出行急加速事件个数--------------------------------------------------------------
# function
rapidAcce_begin_identify <- function(x){
  acce_index <- which(x$tg_acce > 2)
  acce_begin_index <- NULL
  for(i in acce_index){
    if(!length(x$tg_acce[i-1])){
      acce_begin_index <- c(acce_begin_index,i)
      next
    }
    else if(x$tg_acce[i-1] <= 2){
      acce_begin_index <- c(acce_begin_index,i)
    }
  }
  return(acce_begin_index)
}

rapidAcce_events_journey <- plyr::ddply(raw_data_test, .(journeyId),
                                        function(x){
                                          length(rapidAcce_begin_identify(x))
                                        }
)
names(rapidAcce_events_journey)[2] <- "rapidAcce_events_times"
#save(rapidAcce_events_journey,file = paste0("C:/Users/ny42068/work/UBI/","rapidAcce_events_journey_2017_01.RData"))

# ---------------------------------------每次出行急减速事件个数---------------------------------------------------------------- 
rapidDece_begin_identify <- function(x){
  dece_index <- which(x$tg_acce < -2)
  dece_begin_index <- NULL
  for(i in dece_index){
    if(!length(x$tg_acce[i-1])){
      dece_begin_index <- c(dece_begin_index,i)
      next
    }
    else if(x$tg_acce[i-1] >= -2){
      dece_begin_index <- c(dece_begin_index,i)
    }
  }
  return(dece_begin_index)
}

rapidDece_events_journey <- plyr::ddply(raw_data_test, .(journeyId),
                                        function(x){
                                          length(rapidDece_begin_identify(x))
                                        }
)
names(rapidDece_events_journey)[2] <- "rapidDece_events_times"
#save(rapidDece_events_journey,file = paste0("C:/Users/ny42068/work/UBI/","rapidDece_events_journey_2017_01.RData"))

# ---------------------------------------每次出行恶劣行驶事件个数---------------------------------------------------------------
raw_data_test$cmb_acce <- sqrt(raw_data_test$tg_acce^2 + raw_data_test$rd_acce^2)
harshDriving_events_begin_identify <- function(x){
  harshDriving_events_index <- which(x$cmb_acce > 2)
  harshDriving_events_begin_index <- NULL
  for(i in harshDriving_events_index){
    if(!length(x$cmb_acce[i-1])){
      harshDriving_events_begin_index <- c(harshDriving_events_begin_index,i)
      next
    }
    else if(x$cmb_acce[i-1] <= 2){
      harshDriving_events_begin_index <- c(harshDriving_events_begin_index,i)
    }
  }
  return(harshDriving_events_begin_index)
}

harshDriving_events_journey <- plyr::ddply(raw_data_test, .(journeyId),
                                           function(x){
                                             length(harshDriving_events_begin_identify(x))
                                           }
)
names(harshDriving_events_journey)[2] <- "harshDriving_events_times"
#save(harshDriving_events_journey,file = paste0("C:/Users/ny42068/work/UBI/","harshDriving_events_journey_2017_01.RData"))
# ----------------------------------------打电话次数-----------------------------------------------------------
onPhone_begin_identify <- function(x){
  onPhone_index <- which(x$callState > 0)
  onPhone_begin_index <- NULL
  for(i in onPhone_index){
    if(!length(x$callState[i-1])){
      onPhone_begin_index <- c(onPhone_begin_index,i)
      next
    }
    else if(x$callState[i-1] == 0){
      onPhone_begin_index <- c(onPhone_begin_index,i)
    }
  }
  return(onPhone_begin_index)
}

onPhone_journey <- plyr::ddply(raw_data_test, .(journeyId),
                               function(x){
                                 length(onPhone_begin_identify(x))
                               }
)
names(onPhone_journey)[2] <- "onPhone"
#save(onPhone_journey,file = paste0("C:/Users/ny42068/work/UBI/","onPhone_journey_2017_01.RData"))
# ----------------------------------------合并------------------------------------------------------------------
rm(list = "driving_behavior")
driving_behavior <- mileage_journey
driving_behavior <- merge(driving_behavior,driveTime_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,onPhone_journey,by = "journeyId")
#driving_behavior <- merge(driving_behavior,badTime_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,driveLabel_journey,by.x = "journeyId", by.y = "journey_id")
driving_behavior <- merge(driving_behavior,jerk_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,range_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,instability_journey,by = "journeyId")
#driving_behavior <- merge(driving_behavior,rapidAcce_seconds_journey,by = "journeyId")
#driving_behavior <- merge(driving_behavior,badTimes_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,harshDriving_events_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,slowTime_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,fatigue_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,rapidAcce_events_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,rapidDece_events_journey,by = "journeyId")
#driving_behavior <- merge(driving_behavior,rapidTurn_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,rd_acce_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,speed_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,tg_acce_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,harshDriving_seconds_journey,by = "journeyId")
driving_behavior <- merge(driving_behavior,rapidTurn_events_journey,by = "journeyId")
save(driving_behavior ,file = paste0("//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/","driving_behavior_2017_03_p2.RData"))
#------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------plot_trip------------------------------------------------------
# -------------------------------------------------作图---------------------------------------------------------------------
# max(jourcount_UUID$jourcount)
# jourcount_UUID[jourcount_UUID$jourcount == max(jourcount_UUID$jourcount),]
# 输出最大行程段数的UUID是25420
# ----------------------------------------------导入包-----------------------------------------------------------
library("ggplot2")
library("gridExtra")
library("ggmap")
library('dplyr')
# ----------------------------------------------导入数据---------------------------------------------------------
location <- "C:/Users/ny42068/work/UBI/journey/"
fileNames <- dir(location)
for (i in fileNames){
  load(paste0(location,i))
}

# ----------------------------------------------行程轨迹图-------------------------------------------------------
# # get map
# get_map('Hong kong')
# ggmap(map, extent = 'device') + 
#   geom_point(data = lon_lat, aes(x = lon, y = lat ),
#              color = 'red3', size = .1)
# register_google(key = 'AIzaSyAuqG1xy8XjPN5GpjHbZgpHeI2KogO5ef8 ')
journey_analysis <- raw_data_test %>% select(lat,log,journeyId,speed,heading,tg_acce,rd_acce,dtime,callState) %>% filter(journeyId == 111248)    
journey_analysis$angular_velocity <- journey_analysis$heading/journey_analysis$dtime *pi/180
journey_analysis$index <- as.numeric(row.names(journey_analysis))

rapidAcce_index <- rapidAcce_begin_identify(journey_analysis)
rapidDece_index <- rapidDece_begin_identify(journey_analysis)
rapidTurn_index <- rapidTurn_begin_identify(journey_analysis)
onPhone_index <- onPhone_begin_identify(journey_analysis)

journey_analysis$label1 <- ""
journey_analysis[rapidTurn_index,]$label1 <- "○"

journey_analysis$label2 <- ""
journey_analysis[rapidAcce_index,]$label2 <- "□"

journey_analysis$label3 <- ""
journey_analysis[rapidDece_index,]$label3 <- "△"

journey_analysis$journeyId
journey_analysis$label4 <- ""
journey_analysis[onPhone_index,]$label4 <- "☆"
#journey_analysis$label4 <- ""
#journey_analysis[onPhone_index,]$label4 <- "◇"
# sum(journey_analysis$dtime)
accerlation_plot <- ggplot() + 
  geom_point(data = journey_analysis, aes(x = index,y = speed,color = "speed")) +
  geom_path(data = journey_analysis, aes(x = index,y = speed,color = "speed")) + 
  geom_point(data = journey_analysis, aes(x = index, y = tg_acce, color = "tg_acce"))  + 
  geom_path(data = journey_analysis, aes(x = index , y = tg_acce, color = "tg_acce"))+ 
  geom_text(aes(x = index, y= tg_acce, label=label2),data= journey_analysis, size = 10) + 
  geom_text(aes(x = index, y= tg_acce, label=label3),data= journey_analysis, size = 8) + 
  labs(x = "" , y = "", title = "Harsh braking and Acceleration") +
  scale_x_continuous(breaks = seq(0,150,10)) + 
  scale_y_continuous(breaks = seq(-4,max(journey_analysis$speed),2)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual("",values = c("speed" = "blue", "tg_acce" = "red")) + 
  geom_hline(aes(yintercept = 2), linetype = "dashed") + 
  geom_hline(aes(yintercept = -2), linetype = "dashed")

#head(journey_analysis)
turn_plot <-  ggplot() + 
  geom_point(data = journey_analysis, aes(x = index,y = speed,color = "speed")) +
  geom_path(data = journey_analysis, aes(x = index,y = speed,color = "speed")) + 
  geom_point(data = journey_analysis, aes(x = index, y = rd_acce, color = "rd_acce"))  + 
  geom_path(data = journey_analysis, aes(x = index , y = rd_acce, color = "rd_acce"))+ 
  geom_point(data = journey_analysis, aes(x = index, y = angular_velocity*4, color = "angular_velocity"))  + 
  geom_path(data = journey_analysis, aes(x = index , y = angular_velocity*4, color = "angular_velocity"))+
  geom_text(aes(x = index, y= rd_acce, label=label1),data= journey_analysis, size = 10) + 
  geom_text(aes(x = index, y= rd_acce, label=label4),data= journey_analysis, size = 10) + 
  labs(x = "" , y = "", title = "Harsh Cornering and on phone") +
  scale_x_continuous(breaks = seq(0,150,10)) + 
  scale_y_continuous(breaks = seq(-4,max(journey_analysis$speed),1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual("",values = c("speed" = "blue", "rd_acce" = "red","angular_velocity" = "olivedrab")) + 
  #geom_hline(aes(yintercept = 2), linetype = "dashed") + 
  #geom_hline(aes(yintercept = -2), linetype = "dashed") + 
  geom_hline(aes(yintercept = 3), linetype = "dashed") + 
  geom_hline(aes(yintercept = 2.5), linetype = "dashed")



journey_analysis$index1 <- ifelse(journey_analysis$index%%5 == 0, journey_analysis$index, "")

#lon_lat <- data.frame(journey_analysis$log,journey_analysis$lat,journey_analysis$index1)
#colnames(lon_lat) <- c('log','lat','index1')
journey_analysis[60:100,] %>% select(dtime, rd_acce, tg_acce,speed,heading ) 
location_plot <- ggplot() + 
  geom_point(data = journey_analysis, aes(x = log,y = lat), color = 'red3', size = 1) +
  geom_text(aes(x =log, y = lat, label=index1),data=journey_analysis,hjust=2, vjust=2, size = 3) + 
  geom_text(aes(x =log, y = lat, label=label1),data=journey_analysis,size = 10) + 
  geom_text(aes(x =log, y = lat, label=label2),data=journey_analysis,size = 10) + 
  geom_text(aes(x =log, y = lat, label=label3),data=journey_analysis,size = 10) + 
  geom_text(aes(x =log, y = lat, label=label4),data=journey_analysis,size = 12) +
  labs(x = "longitude", y = "latitude", title = "The track of the car")+
  theme(plot.title = element_text(hjust = 0.5))

# journey_analysis[journey_analysis$dtime >120, ]
grid.arrange(location_plot,accerlation_plot,turn_plot, layout_matrix = rbind(c(1,2,2),c(1,3,3)))
#  nrow(journey_analysis)
#rbind(c(1,2,2),c(1,3,3))
journey_analysis[140:150,]
# 在地图上打散点图
library("devtools")
install_github('lchiffon/REmap')


# ----------------------------------------------加速度分布--------------------------------------------------------

# 径向加速度
# 概述
summary(raw_data_full$lat_acce)
# 密度图
ggplot(raw_data_full,aes(x = lat_acce)) + 
  geom_density()
# 直方图
ggplot(raw_data_full[raw_data_full$lat_acce < 50 & raw_data_full$lat_acce >-50,],aes(x = lat_acce)) + 
  geom_histogram()
# 求分位数
raw_data_full[raw_data_full$lat_acce>400,]
raw_data_full[307400:307420,]
length(raw_data_full$lat_acce[raw_data_full$lat_acce > 2 ])/length(raw_data_full$lat_acce[raw_data_full$lat_acce > 0])
# 6%的横向加速度在1以上
# 2.5%的横向加速度在2以上
# 1.24%的横向加速度在3以上 
# 1%的横向加速度在3.5以上
raw_data_full$lat_acce
raw_data_full$heading

library("dplyr")

library("lubridate")
library("data.table")

trip_data <- fread("C:/Users/ny42068/work/UBI/TripData-20161001-20180226.csv",
                   sep = ",",stringsAsFactors = F, header = T)

colnames(trip_data)

trip_data <- trip_data[!is.na(trip_data$total_journey_actual_score),]
# 434378 obs. of 20 variables

trip_data_cmplt <- trip_data %>% select(total_journey_score,client_user_id,journey_id,length,total_journey_actual_score, valid_journey, Speeding,
                                        Braking, Cornering, Acceleration, OnPhone, start_date, end_date)

trip_data_cmplt <- trip_data_cmplt[substr(trip_data_cmplt$start_date,1,4) == "2017"& 
                                     complete.cases(trip_data_cmplt)&
                                     trip_data_cmplt$valid_journey=="validJourney"&trip_data_cmplt$total_journey_actual_score > 0 &
                                     trip_data_cmplt$length > 0,]
# 231372 obs. of 13 variables

head(trip_data_cmplt)
tail(trip_data_cmplt)
colnames(trip_data_cmplt)
last_score_in2017 <- ddply(trip_data_cmplt, .(client_user_id),function(x){
  x[nrow(x),]$total_journey_score
})
names(last_score_in2017)[2] <- "last_score"


length(unique(trip_data_cmplt$client_user_id))
length(unique(trip_data_cmplt$journey_id))
head(ymd_hms(trip_data_cmplt$start_date))
head(ymd_hms(trip_data_cmplt$end_date))
as.numeric(mean(ymd_hms(trip_data_cmplt$end_date) - ymd_hms(trip_data_cmplt$start_date) ))
mean(trip_data_cmplt$length)

opar <- par(no.readonly = T)
par(mfrow = c(1,1))

hist(last_score_in2017$last_score, freq = FALSE,
     xlab = "User Score in 2017", main = "Histogram for User Score in 2017",
     breaks = 20)
rug(jitter(last_score_in2017$last_score), amount = 0.01)
lines(density(last_score_in2017$last_score), lwd = 2)

#mean(trip_data_cmplt$length)
hist(trip_data_cmplt$total_journey_actual_score, freq = FALSE, xlim = c(20,100), breaks = 20, 
     xlab = "Trip Score in 2017", main = "Histogram for Trip Score in 2017")

hist(trip_data_cmplt$total_journey_score, freq = FALSE, xlim = c(20,100), breaks = 20, 
     xlab = "User Score", main = "Histogram for Trip Score")


hist(trip_data_cmplt$Speeding/trip_data_cmplt$length,freq = F, xlab = "Speeding per KM", 
     main = "Histogram of Speeding per KM ",breaks = 500)
#dim(trip_data_cmplt %>% select(Speeding) %>% filter(Speeding == 0) )[1]/nrow(trip_data_cmplt)
#dim(trip_data_cmplt %>% select(Braking) %>% filter(Braking == 0) )[1]/nrow(trip_data_cmplt)

#dim(trip_data_cmplt %>% select(Acceleration) %>% filter(Acceleration == 0) )[1]/nrow(trip_da#ta_cmplt)
#dim(trip_data_cmplt %>% select(Cornering) %>% filter(Cornering == 0) )[1]/nrow(trip_data_cmplt)

hist(trip_data_cmplt$Braking/trip_data_cmplt$length, freq = F, xlab = "Braking per KM", 
     main = "Histogram of Braking per KM ", xlim = c(0,0.6), breaks = 1000)

hist(trip_data_cmplt$Acceleration/trip_data_cmplt$length, freq = F, xlab = "Acceleration per KM", 
     main = "Histogram of Acceleration per KM ",xlim = c(0,1.5)  ,breaks = 1000)

hist(trip_data_cmplt$Cornering/trip_data_cmplt$length, freq = F, xlab = "Cornering per KM", 
     main = "Histogram of Cornering per KM ",xlim = c(0,1.5),ylim = c(0,10),breaks = 500)

hist(trip_data_cmplt$OnPhone/trip_data_cmplt$length, freq = F, xlab = "OnPhone per KM", 
     main = "Histogram of OnPhone per KM " ,xlim = c(0,2),breaks = 100)

hist(trip_data_cmplt$length, freq = F, xlab = "Trip Length (KM)", 
     main = "Histogram of OnPhone per KM " ,xlim = c(0,100),breaks = 100)

#hist(trip_data_cmplt$Texting/trip_data_cmplt$length, freq = F, xlab = "Text per KM", 
#    main = "Histogram of Text per KM ")

#boxplot(trip_data_cmplt$total_journey_actual_score)


options(digits = 3)

cutpoint <- unname(quantile(trip_data_cmplt$Speeding/trip_data_cmplt$length,seq(0,1,0.05), na.rm = TRUE))
#score_1 <- as.factor(cutpoint[findInterval(score,cutpoint)])
#data.frame(score_1, socre)
Speeding_perLength_cut <- cut(trip_data_cmplt$Speeding/trip_data_cmplt$length, breaks = cutpoint, right = F)



cutpoint <- unname(quantile(modeldata_ctpl_regroup[,"车辆实际价值_level"],seq(0,1,0.05), na.rm = TRUE))
modeldata_ctpl_regroup[,"车辆实际价值_level"] <- cutpoint[findInterval(modeldata_ctpl_regroup[,"车辆实际价值_level"],cutpoint)]


plot_oneway <- function(df){
  p <- ggplot(df, aes(x=var_band))+
    scale_x_discrete(limits=df$var_band)+
    # weight
    geom_bar(aes(y=Total_prem_base, fill=var_band), stat = 'identity', fill = gray(0.7))+
    # actual
    geom_line(aes(y=loss_ratio_base* 3*max(df$Total_prem_base), group=1), colour = "steelblue", size = 1)+ 
    geom_point(aes(y=loss_ratio_base* 3*max(df$Total_prem_base), group=1), colour = "steelblue", size = 2)+ 
    geom_line(aes(y=(avg_base_lr)* 3*max(df$Total_prem_base),  group=2), colour = "steelblue", size = 0.6)+ 
    scale_y_continuous(limits=c(0, 3*max(df$Total_prem_base)), sec.axis = sec_axis(~./ (3*max(df$Total_prem_base)), name = 'loss_ratio'))+
    theme(axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.1))+
    labs(title=df$var_name)
  print(p)
}



#install.packages("ggmap")
library("ggmap")
library(data.table)
register_google(key = 'AIzaSyBLYA2oGVs6J-zW8hrKWtGKHNY5v9tjd34')

dirDAT  <- "~/Projects/UBI/" 
#dirRslt  <- "~/Projects/YZ/result/" 
target_file <- paste0(dirDAT,'map-mapping.csv')


temp <- fread(target_file,header = TRUE,sep = ",")
temp<-temp[1:200,]
temp$index <- seq(1:200)
temp$index1 <- rep("",200)
temp$index1 <- ifelse(temp$index%%5 == 0, temp$index, "")


lon_lat <- data.frame(temp$log,temp$lat,temp$index1)
colnames(lon_lat) <- c('log','lat','timestamp')


map <- get_googlemap(center = c(lon =mean(lon_lat$log), lat =mean(lon_lat$lat)),zoom = 16,
                     color = 'bw',
                     #size = c(2600, 1400),
                     scale = 4,maptype = 'terrain'
)

ggmap(map,extent = 'device') +  # remove axes(extent = 'device')
  geom_point(data = lon_lat, aes(x = log,y = lat), color = 'red3', size = 0.5, alpha = 1) +
  geom_text(aes(x =log, y = lat, label=timestamp),data=lon_lat,hjust=0, vjust=2, size = 3)

# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------feature selection--------------------------------------------------------
# ------------------------------------------feature select---------------------------------------------
rm(list = ls())
library("xlsx")
load(paste0("//as.munichre.com/apac/PEK/PEK-General/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/","driving_behavior_2017_01_p1.RData"))
select_var <- read.xlsx(paste0("G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/","selected_variable.xlsx"),
                        sheetIndex = 1)
select_var <- filter(select_var,selet.by.corr == 1)
driving_behavior_select <- driving_behavior[which(colnames(driving_behavior) %in% select_var$name)]
# colnames(driving_behavior_select)
# -----------------------------------------transform -------------------------------------------------

# -----------------------------------------normalization ---------------------------------------------
log_norm <- function(x){
  log <- log(x+1)
  log_x <- (log - mean(log))/sd(log)
  return(log_x)
}

norm <- function(x){
  norm_x <- (x - mean(x))/sd(x)
  return(norm_x)
}

driving_behavior_select_full <-  driving_behavior_select
sequence <- 1: ncol(driving_behavior_select)
sequence <- sequence[-which(colnames(driving_behavior_select) %in% "label")]
for (i in sequence){
  driving_behavior_select_full[ncol(driving_behavior_select_full) + 1] <- log_norm(driving_behavior_select_full[[i]])
  names(driving_behavior_select_full)[ncol(driving_behavior_select_full)] <- paste0(colnames(driving_behavior_select_full[i]),"_log")
}


for (i in sequence){
  driving_behavior_select_full[ncol(driving_behavior_select_full) + 1] <- norm(driving_behavior_select_full[[i]])
  names(driving_behavior_select_full)[ncol(driving_behavior_select_full)] <- paste0(colnames(driving_behavior_select_full[i]),"_normal")
}


# pdf(file = "G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/histogram.pdf",width = 5,height = 4)
# for (i in sequence) {
#   hist(driving_behavior_select_full[[i]], main = colnames(driving_behavior_select_full)[i])
# }
# dev.off()

pick_var <- read.xlsx(paste0("G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/","selected_variable.xlsx"),
                      sheetName = "log_norm")
pick_var <- filter(pick_var, choose == 1 )
driving_behavior_pick <- driving_behavior_select_full[which(colnames(driving_behavior_select_full) %in% pick_var$name)]

# ----------------------------------data type transform -----------------------------------------------------
str(driving_behavior_pick)
driving_behavior_pick$label <- factor(driving_behavior_pick$label)
# driving_behavior_pick$onPhone <- factor(driving_behavior_pick$onPhone)
# driving_behavior_pick$acce_times <- factor(driving_behavior_pick$acce_times)
# driving_behavior_pick$dece_times <- factor(driving_behavior_pick$dece_times)
# driving_behavior_pick$turn_times <- factor(driving_behavior_pick$turn_times)
# driving_behavior_pick$harshDriving_events_times <- factor(driving_behavior_pick$harshDriving_events_times)
# driving_behavior_pick$rapidAcce_events_times <- factor(driving_behavior_pick$rapidAcce_events_times)
# driving_behavior_pick$rapidDece_events_times <- factor(driving_behavior_pick$rapidDece_events_times)
# driving_behavior_pick$harshDriving_seconds <- factor(driving_behavior_pick$harshDriving_seconds)
# driving_behavior_pick$rapidTurn_events_times <- factor(driving_behavior_pick$rapidTurn_events_times)
save(driving_behavior_pick, file =paste0("G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/","driving_behavior_pick_integer_2017_01_p1.RData"))
# str(driving_behavior_pick)

#--------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------modeling------------------------------------------------------------
# ----------------------------------- import data------------------------------------------------------------------------------
# rm(list = ls())
# fileName <- dir("G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/pick/")
# loc <- "G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/"
# location <- paste0(loc,"pick/",fileName)
# driving_behavior_all <- NULL
# for (j in location ){
#   load(j)
#   driving_behavior_all <- rbind(driving_behavior_all,driving_behavior_pick)
# }
# sum(is.na(driving_behavior_all))

# summary(driving_behavior_all)
str(driving_behavior_all)
driving_behavior_all <- driving_behavior_all[,-which(colnames(driving_behavior_all) == "min_rd_normal")]

#----------------------------------
str(driving_behavior_all)
<- as.numeric(driving_behavior_all$rapidAcce_events_times)

#-----------------------------------xgboost regression tree--------------------------------------------------------------------
library("Matrix")
library("caret")
library("xgboost")
library("dplyr")
# str(mtcars)
set.seed(1234)
train_index <- sample(nrow(driving_behavior_all), 0.7*nrow(driving_behavior_all))
train <- driving_behavior_all[train_index,]
test <- driving_behavior_all[-train_index,]
# nrow(train) + nrow(test)
ytrain <- log(train$total_journey_actual_score + 1)
ytest <- log(test$total_journey_actual_score + 1)

xtrain <- sparse.model.matrix(total_journey_actual_score~-1+., data = train)
xtest <- sparse.model.matrix(total_journey_actual_score~-1+., data = test)

t1 <- Sys.time()
xgb_grid_tree <- expand.grid(nrounds = c(50,200,500,900,1200),
                             max_depth = c(4,5,6),
                             eta = c(0.01,0.03,0.09),
                             gamma =0,
                             colsample_bytree = c(0.6,0.7,0.8),
                             min_child_weight = 1,
                             subsample = c(0.6,0.7,0.8))

# xgb_grid_tree <- expand.grid(nrounds = 600, 
#                              max_depth = 6, 
#                              eta = 0.01, 
#                              gamma =0,
#                              colsample_bytree = 0.8, 
#                              min_child_weight = 1, 
#                              subsample = 0.8)

xgb_trcontrol_cv = trainControl(method = "cv",number = 5, 
                                allowParallel = TRUE,
                                summaryFunction =  defaultSummary)
xgb_tree_model <- train(x = xtrain, 
                        y = ytrain, 
                        trControl = xgb_trcontrol_cv,
                        tuneGrid = xgb_grid_tree,
                        method = "xgbTree",
                        metric = "RMSE")  
t2 <- Sys.time()
t2 - t1
# varImp(xgb_tree_model)
xgb_tree_pred <- exp(predict(xgb_tree_model, xtest)) -1 
#install.packages('ModelMetrics')
pp <- predict(xgb_tree_model, xtest)

ModelMetrics::mae(log(test$total_journey_actual_score+1),pp)

ModelMetrics::mae(test$total_journey_actual_score, xgb_tree_pred)

MAE_tree <- sum(abs(xgb_tree_pred - test$total_journey_actual_score))/length(xgb_tree_pred)
MSE_tree <- sum((xgb_tree_pred - test$total_journey_actual_score)^2)/length(xgb_tree_pred)
#xgb.importance(model =xgb_tree_model )
#xgb_tree_imp <- xgb.importance(feature_names = xgb_tree_model$finalModel$feature_names,
#   model = xgb_tree_model$finalModel)



# 开始xgboost linear regression
# 一般来说tree booster的表现总是好于linear booster
# alpha是L1正则化的权重术语，相当于Lasso regression
# lamda是L2正则化的权重术语，同Ridge regression
# eta是学习速率，应该也是指代着对新的线性模型的学习能力
set.seed(1234)
train_index <- sample(nrow(driving_behavior_all), 0.7*nrow(driving_behavior_all))
train <- driving_behavior_all[train_index,]
test <- driving_behavior_all[-train_index,]
# nrow(train) + nrow(test)
ytrain <- log(train$total_journey_actual_score + 1)
ytest <- log(test$total_journey_actual_score + 1)
xtrain <- sparse.model.matrix(total_journey_actual_score~-1+., data = train)
xtest <- sparse.model.matrix(total_journey_actual_score~-1+., data = test)
# nrounds <- c(50,100,200)
# eta = c(0.01,0.05,0.1)
nrounds <- c(50,100,200)
eta <- c(0.01,0.05,0.1)
for (i in nrounds){
  for (j in eta){
    t1 <- Sys.time()
    set.seed(4321)
    xgb_grid_linear <- expand.grid(
      nrounds = i,
      lambda = c(0.6,0.7,0.8),
      alpha = c(0.3,0.4,0.5),
      eta = j)
    
    xgb_trcontrol_cv = trainControl(method = "cv",number = 5, 
                                    allowParallel = TRUE,
                                    summaryFunction =  defaultSummary)
    
    xgb_linear_model <- train(x = xtrain,
                              y = ytrain,
                              trControl = xgb_trcontrol_cv,
                              tuneGrid = xgb_grid_linear,
                              method = "xgbLinear",
                              metric =  "MAE" )
    t2 <- Sys.time()
    t2 - t1
    save(xgb_linear_model,file = paste0("G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/modeling_data/nrounds_MAE",i,"_eta_",j, ".RData"))
  }
}


# xgb_linear_pred <- exp(predict(xgb_linear_model, xtest)) -1 
# MAE_linear <- sum(abs(xgb_linear_pred - test$total_journey_actual_score))/length(xgb_linear_pred)
# MSE_linear <- sum((xgb_linear_pred - test$total_journey_actual_score)^2)/length(xgb_linear_pred)


# -----------------------------------------random forest--------------------------------------------------------
set.seed(3)
t1 <- Sys.time()
# mtry is number of variables randomly sampled as candidates at each split. Default is the (rounded down) square root of number variables
# min.node.size is the minimum size of terminal nodes. Setting this number larger causes smaller tree to be grown(and take less time), 
# in regression tree, default value is 5
# splitrule is splitting rule. For regression, default value is variance( we want to get a low variance in each node)
ranger_grid <- expand.grid(mtry = 6,
                           splitrule = "variance",
                           min.node.size = 5)
ranger_trcontrol = trainControl(method = "cv",
                                number = 5, 
                                allowParallel = TRUE, 
                                summaryFunction = defaultSummary)

# colnames(train)
# num.trees is the number of CART tree in this forest
# max.depth is the maximum depth of a tree. 从根节点到叶子节点一次经过的节点(含根与叶)
# 行程的最长路径，包括的节点个数即为树的深度
# RMSE Root Mean square error(sqrt(MSE))
# importance is variable imprtance mode, for regression, importance = impurity mreasure the variance
# of the responses for regression
# range is a fast implementation of random forests or recursive partitioning, it is particularly
# suited for high dimensional data.
rftrain <- train
rftrain$total_journey_actual_score <- log(rftrain$total_journey_actual_score + 1)
#rftest <- test
#summary(rftest)
#rfxtest <- rftest %>% select(-total_journey_actual_score)
# summary(rfxtest)
ranger_model = train(total_journey_actual_score ~. ,
                     data = rftrain,
                     trControl = ranger_trcontrol,
                     tuneGrid = ranger_grid,
                     num.trees = 500,
                     max.depth = 6,
                     metric = "RMSE",
                     importance = "impurity", 
                     method = "ranger")

t2 <- Sys.time()
t2 - t1
# summary(test)
rf_pred <- exp(predict(ranger_model, test)) -1 
MAE_rf <- sum(abs(rf_pred - test$total_journey_actual_score))/length(rf_pred)
MSE_rf <- sum((rf_pred - test$total_journey_actual_score)^2)/length(rf_pred)


# ------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------partial dependence--------------------------------------------------------------
library("pdp")
library("ggplot2")
library("dplyr")

rm(list = ls())
load("G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/driving_behavior/driving_behavior_org.RData")
# ----------------------one_way -----------------------------------------
hist(driving_behavior_org$max_speed*3.6,breaks = 100)
cut_point <- cut(driving_behavior_org$max_speed * 3.6,breaks = c(0,40,seq(45,120,5),
                                                                 max(driving_behavior_org$max_speed)*3.6+1),right = F)
df <- data.frame(driving_behavior_org$max_speed, driving_behavior_org$total_journey_actual_score,cut_point)
names(df) <- c("max_speed","score","cut_point")
# head(df)
# str(df)
# summary(df)

df_group <- group_by(df, cut_point)
df_sum <- summarise(df_group, 
                    mean_score  = mean(score),
                    weight  = length(score)
)
df_sum <- as.data.frame(df_sum)
colnames(df_sum) <- c("var_band","loss_ratio_base","Total_prem_base")
df_sum$var_band <- as.character(df_sum$var_band)
str(df_sum)
for (i in 1:nrow(df_sum)){
  # loc1 <- gregexpr("[",df_sum$var_band[1])[[1]][1]
  loc2 <- gregexpr(",",df_sum$var_band[i])[[1]][1]
  loc3 <- gregexpr(")",df_sum$var_band[i])[[1]][1]
  df_sum$var_band[i] <- paste0(substr(df_sum$var_band[i],2,loc2-1),"-",substr(df_sum$var_band[i],loc2+1,loc3-1))
}
df_sum$var_band[18] <- '> 120'
library(ggplot2)

#   apply(df_sum[,"var_band"], 1, function(x) grep(",",x))
# str(df_sum)
# 
# gsub(","," - ",df_sum$var_band[4])
# gsub('['," ",df_sum$var_band[4])

# df_test <- aggregate(data = df,  max_speed + score ~  )
# max_speed_num <- ddply(max_speed, .(cut_point),mean_speed = mean(x),,
# max_speed_meanScore <- aggregate(data = max_speed, driving_behavior_org.total_journey_actual_score ~ cut_point, FUN = count)
# head(max_speed_meanScore)
# head(max_speed_num)
# ggplot() + 
#   geom_bar(data = max_speed_meanScore,aes(x = cut_point,y = driving_behavior_org.total_journey_actual_score), stat = "identity") + 
#   geom_point(data = max_speed_num, aes(x = cut_point, y = V1))
# max(driving_behavior_org$max_speed)
# quantile(driving_behavior_org$max_speed,0.99)
# ggplot(data = driving_behavior_org,a)

df = df_sum  
p <- ggplot(df, aes(x=var_band))+
  scale_x_discrete(limits=df$var_band)+
  # weight
  geom_bar(aes(y=Total_prem_base, fill=var_band), stat = 'identity', fill = gray(0.7))+
  # actual
  geom_line(aes(y=loss_ratio_base* 0.02*max(df$Total_prem_base), group=1), colour = "steelblue", size = 1)+
  geom_point(aes(y=loss_ratio_base* 0.02*max(df$Total_prem_base), group=1), colour = "steelblue", size = 2)+
  
  # geom_line(aes(y=(avg_base_lr)* 3*max(df$Total_prem_base),  group=2), colour = "steelblue", size = 0.6)+
  scale_y_continuous(limits=c(0, 2.5*max(df$Total_prem_base)), 
                     sec.axis = sec_axis(~. *50/(max(df$Total_prem_base)), name = 'Trip Score'))+
  theme(axis.text.x=element_text(angle=45,hjust=0.2,vjust=0.1))+
  labs(title="One-Way Analysis on Max Speed (KM/H)", y = "Number of Trips", x = "Max Speed") + 
  
  theme(plot.title = element_text(hjust = 0.5))  
print(p)
#ggsave(paste(resultdir,df$var_no, '.jpg'), p, device = 'jpeg')


hist(driving_behavior_org$rapidTurn_events_times,breaks = 100)
# table(driving_behavior_org$rapidTurn_events_times)
# sum(driving_behavior_org$rapidTurn_events_times>100)
# sum(driving_behavior_org$rapidTurn_events_times<=100 & driving_behavior_org$rapidTurn_events_times>50)
# sum(driving_behavior_org$rapidTurn_events_times<=100 & driving_behavior_org$rapidTurn_events_times>50)

driving_behavior_org$rapidTurn_events_times <- driving_behavior_org$rapidTurn_events_times / driving_behavior_org$mileage
driving_behavior_org$rapidTurn_events_times <- driving_behavior_org$rapidTurn_events_times * 10 

### times per KM 

quantile(driving_behavior_org$rapidTurn_events_times, seq(0.1,1,0.1)) 
sum(driving_behavior_org$rapidTurn_events_times>8)




cut_point <- cut(driving_behavior_org$rapidTurn_events_times,breaks = c(seq(0,75,5),100,
                                                                        max(driving_behavior_org$rapidTurn_events_times)+0.001),right = F)
df <- data.frame(driving_behavior_org$rapidTurn_events_times, driving_behavior_org$total_journey_actual_score,cut_point)
names(df) <- c("rapidTurn_events_times","score","cut_point")
# head(df)
# str(df)
# summary(df)

df_group <- group_by(df, cut_point)
df_sum <- summarise(df_group, 
                    mean_score  = mean(score),
                    weight  = length(score)
)
df_sum <- as.data.frame(df_sum)
colnames(df_sum) <- c("var_band","loss_ratio_base","Total_prem_base")
df_sum$var_band <- as.character(df_sum$var_band)
# str(df_sum)
for (i in 1:nrow(df_sum)){
  # loc1 <- gregexpr("[",df_sum$var_band[1])[[1]][1]
  loc2 <- gregexpr(",",df_sum$var_band[i])[[1]][1]
  loc3 <- gregexpr(")",df_sum$var_band[i])[[1]][1]
  df_sum$var_band[i] <- paste0(substr(df_sum$var_band[i],2,loc2-1),"-",substr(df_sum$var_band[i],loc2+1,loc3-1))
}
df_sum$var_band[17] <- '> 100'

df = df_sum  
p <- ggplot(df, aes(x=var_band))+
  scale_x_discrete(limits=df$var_band)+
  # weight
  geom_bar(aes(y=Total_prem_base, fill=var_band), stat = 'identity', fill = gray(0.7))+
  # actual
  geom_line(aes(y=loss_ratio_base* 0.02*max(df$Total_prem_base), group=1), colour = "steelblue", size = 1)+
  geom_point(aes(y=loss_ratio_base* 0.02*max(df$Total_prem_base), group=1), colour = "steelblue", size = 2)+
  # geom_line(aes(y=(avg_base_lr)* 3*max(df$Total_prem_base),  group=2), colour = "steelblue", size = 0.6)+
  scale_y_continuous(limits=c(0, 2.5*max(df$Total_prem_base)), 
                     sec.axis = sec_axis(~. *50/(max(df$Total_prem_base)), name = 'Trip Score'))+
  theme(axis.text.x=element_text(angle=45,hjust=0.2,vjust=0.1))+
  labs(title="One-Way Analysis on Harsh Cornering Count", y = "Number of Trips", x = "") + 
  theme(plot.title = element_text(hjust = 0.5))
print(p)


str(driving_behavior_org)

# ----------------------importance----------------------------------------
fileName <- dir("G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/modeling_data/model_permile/RMSE_linear/")
loc <- "G:/_Temp_Files/Weiran/Images of Oneway Analysis/old/temp/modeling_data/model_permile//RMSE_linear/"
location <- paste0(loc,fileName)
importance <- data.frame(ID = 1:36)
for ( j in location ){
  load(j)
  importance_temp <- varImp(xgb_linear_model)
  importance_temp <- importance_temp$importance
  importance_temp$rownames <- rownames(importance_temp)
  importance <- cbind(importance,importance_temp)
}
row.names(importance) <- importance$ID

# ---------------------partial_dependencde--------------------------------
# grid.resolution: integer giving the number of equallly spaced points to use for the continuos variables listed in pred.var when pred.grid is not supplied
# chull: Logical indicating whether or not to restrict the values of the first two variables in pred.var to lie within the convex hull of their training walues
# levelPlot: Logical indicating whether or not to use the false color level plot(T), or a 3-D surface(F), default is T
load()
pd_prch_price <- partial(object=xgb_tree_model, pred.var='rapidTurn_events_times_log', train=xtrain,  grid.resolution = 40, chull = TRUE)
pd_prch_price$rapidTurn_events_times_log <- exp((pd_prch_price$rapidTurn_events_times_log * sd()))-1
pd_prch_price$yhat <- exp(pd_prch_price$yhat) -1
plotPartial(pd_prch_price[(1:15),], levelplot = FALSE, drape = T, colorkey = F, main = 'XGBoost Regression Tree(RMSE) Harsh Cornering')

# set.seed(101)
# seedlist=unique(as.integer(runif(100)*(100*1000)))