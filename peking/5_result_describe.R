rm(list = ls())
load("C:/Users/lenovo/Desktop/在线实习任务/index_classification.RData")
load("C:/Users/lenovo/Desktop/在线实习任务/val_trip_summary.RData")
load("C:/Users/lenovo/Desktop/在线实习任务/decri15.RData")
# ------------------------------------------------得到每辆车的风险指标、驾驶路线类型和出险次数的数据框val_SimNum_summary-------------------------------------
val_trip_summary <- as.data.frame(val_trip_summary)
# head(val_trip_summary)
# summary(val_trip_summary)
# head(decri15)
val_trip_summary <- merge(val_trip_summary, decri15[c("span.distance","rest.mins","span.hours","trip_id")], by.x = "trip_id" , by.y = "trip_id",all.x = T,all.y = F)
val_SimNum_group <- dplyr::group_by(val_trip_summary,SimNum)
val_SimNum_summary <- dplyr::summarise(val_SimNum_group,
                                       sum_Mileage = sum(span.distance),
                                       sum_drivingHour = sum(drivingHour),
                                       ratio_fatigue_drivingHour = sum(fatigue_drivingHour)/sum(drivingHour),
                                       mean_Morning = sum(Morning*span.distance/sum(span.distance)),
                                       mean_Evening = sum(Evening*span.distance/sum(span.distance)),
                                       mean_Midnight = sum(Midnight*span.distance/sum(span.distance)),
                                       mean_meanSpeed = sum(meanSpeed*span.distance/sum(span.distance)),
                                       mean_sdSpeed = sum(sdSpeed*span.distance/sum(span.distance)),
                                       mean_jam_obs =  sum(jam_obs*span.distance/sum(span.distance)),
                                       mean_fast_obs = sum(fast_obs*span.distance/sum(span.distance)),
                                       mean_instability = sum((instability_coef/mean(instability_coef,na.rm = T))*span.distance/sum(span.distance),na.rm = T),
                                       max_maxSpeed = max(maxSpeed)
)
val_SimNum_summary <- as.data.frame(val_SimNum_summary)
val_SimNum_summary$SimNum <- as.character(val_SimNum_summary$SimNum)
length(unique(val_SimNum_summary$SimNum))
length(unique(index$SimNum))
val_SimNum_summary <- merge(val_SimNum_summary,index,by.x = "SimNum",by.y = "SimNum",all = T)
# -----------------------------------------------绘制描述性图表------------------------------------------------------------------------------------------------
str(val_SimNum_summary)
val_SimNum_summary$risk <- factor(val_SimNum_summary$risk,levels = c(0,1),labels = c("未出险","出险"))
table(val_SimNum_summary$risk)
# 平均速度
par(mfrow=c(4,2))
hist(val_SimNum_summary$mean_meanSpeed,main = "Histogram of Mean Speed",
     xlab = "Mean Speed")
boxplot(mean_meanSpeed~risk,data = val_SimNum_summary,
        main = "Boxplot of Mean Speed in different levels",
        xlab = "Risk Level",ylab = "Mean Speed")
# 交通拥堵情况
boxplot(mean_jam_obs~risk,data = val_SimNum_summary)
# 最大速度 (pick)
# par(mfrow=c(1,2))
# png(filename ="C:/Users/lenovo/Desktop/在线实习任务/graphs/Maximum Speed.png")
hist(val_SimNum_summary$mean_meanSpeed,main = "Histogram of Maximum Speed",
     xlab = "Maximum Speed")
boxplot(max_maxSpeed~risk,data = val_SimNum_summary,
        main = "Boxplot of Maximum Speed in different levels",
        xlab = "Risk Level",ylab = "Maximum Speed")
# dev.off()
# boxplot(max_maxSpeed~risk,data = val_SimNum_summary)
# 疲劳驾驶比例 (pick)
# par(mfrow=c(1,2))
# png(filename ="C:/Users/lenovo/Desktop/在线实习任务/graphs/Drowsy Driving.png")
hist(val_SimNum_summary$ratio_fatigue_drivingHour,main = "Histogram of Drowsy Driving ratio",
     xlab = "Drowsy Driving")
boxplot(ratio_fatigue_drivingHour~risk,data = val_SimNum_summary,
        main = "Boxplot of Drowsy Driving ratio in different levels",
        xlab = "Risk Level",ylab = "Drowsy Driving")
# dev.off()
boxplot(ratio_fatigue_drivingHour~risk,data = val_SimNum_summary)
# 夜晚出行占比 (pick)
hist(val_SimNum_summary$mean_Evening,main = "Histogram of Evening Driving Ratio",
     xlab = "Evening Driving")
boxplot(mean_Evening~risk,data = val_SimNum_summary,
        main = "Boxplot of Evening Driving Ratio in Different Levels",
        xlab = "Risk Level",ylab = "Evening Driving")
boxplot(mean_Evening~risk,data = val_SimNum_summary)
# 深夜出行占比 (pick)
hist(val_SimNum_summary$mean_Midnight,main = "Histogram of Midnight Driving Ratio",
     xlab = "Midnight Driving")
boxplot(mean_Midnight~risk,data = val_SimNum_summary,
        main = "Boxplot of Midnight Driving Ratio in Different Levels",
        xlab = "Risk Level",ylab = "Midnight Driving")
boxplot(mean_Midnight~risk,data = val_SimNum_summary)
# 白天出行占比
boxplot(mean_Morning~risk,data = val_SimNum_summary)
# 驾驶平稳性
boxplot(mean_sdSpeed~risk,data = val_SimNum_summary)
boxplot(mean_instability~risk,data = val_SimNum_summary)
# 行驶里程数目
boxplot(sum_Mileage~risk,data = val_SimNum_summary)
# 驾驶时长(pick)
hist(val_SimNum_summary$sum_drivingHour,main = "Histogram of Driving Hours",
     xlab = "Driving Hours")
boxplot(sum_drivingHour~risk,data = val_SimNum_summary,
        main = "Boxplot of Driving Hours in Different Levels",
        xlab = "Risk Level",ylab = "Driving Hours")
boxplot(sum_drivingHour~risk,data = val_SimNum_summary)
# 高速行驶占比
boxplot(mean_fast_obs~risk,data = val_SimNum_summary)


# ------------------------------------------------组合情况---------------------------------------------------
par(mfrow=c(1,2))
classification
boxplot(Tpye1~risk,data = val_SimNum_summary)
boxplot(Tpye2~risk,data = index)
boxplot(Tpye3~risk,data = index)
boxplot(Tpye4~risk,data = val_SimNum_summary,xlab = "Risk Level", ylab = "Ratio in Type 4",
        main = "不同风险水平下，行程数目被归入Type 4的比例") # pick
boxplot(Tpye5~risk,data = index)
boxplot(Tpye6~risk,data = val_SimNum_summary,xlab = "Risk Level", ylab = "Ratio in Type 6",
        main = "不同风险水平下，行程数目被归入Type 6的比例") # pick
# boxplot(Tpye6~risk,data = index) # pick
boxplot(Tpye7~risk,data = index)
boxplot(Tpye8~risk,data = index)
boxplot(Tpye9~risk,data = index)
# summary(val_SimNum_summary)
