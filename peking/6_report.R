rm(list = ls())
# devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
library('plyr')
library(bit64)
library(mvstats)
# library(factoextra)
library(ggplot2)
library(RColorBrewer)
# devtools::install_github("ricardo-bion/ggradar")   #雷达图
# install.packages("knitr")
library(ggradar)
library(knitr)


# --------------------------------------------------处理speed数据---------------------------------------------
#导入数据
fileNames <- dir("C:/Users/lenovo/Desktop/RData")
loc <- paste0("C:/Users/lenovo/Desktop/RData/",fileNames)
raw_data_full <- NULL
for (j in loc){
  load(j)
  raw_data_full <- rbind(raw_data_full, raw_data)
}

#删除速度等于0的所有样本
raw_data_test <- raw_data_full
raw_data_test <- na.omit(raw_data_test)
raw_data_test$SimNum <- as.numeric(raw_data_test$SimNum)
raw_data_test=raw_data_test[-which(raw_data_test$Speed==0),]

# -----------------------------------------------------划分行程----------------------------------------------------
#间隔时间为 15 分(我又理解了一遍，这个函数太棒了)
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
#rest.mins这句其实我一直不同懂，以后问问你
decri$rest.mins=as.numeric(c("NA",difftime(decri$start.time[-1],decri$stop.time[-length(decri[,1])],units="mins")))  
decri$del <- c(1,diff(decri$SimNum,1))
str(raw_data_test)
decri[decri$del != 0, ]$rest.mins <- "NA"

#删除span.hours等于0&span.hours极端值&distance等于0的trip
spanhours_99pre=quantile(decri$span.hours,0.99)
decri15 <-decri[which(decri$span.hours > 0 & decri$span.hours <= spanhours_99pre & decri$span.distance > 0),]
decri_temp=split(decri15,decri15$SimNum)
trip_number15=lapply(decri_temp,function(x){tripnum(x)})
trip_number15=unlist(trip_number15)
trip_number15=data.frame(SimNum=unique(decri15$SimNum),number=trip_number15)
sum(is.na(tripnum15_compare))

# ------------------------------------------------生成unique的指标trip_id----------------------------------------------------
#####黎韬注，merge出来的结果是混乱的，它改变了raw_data_test中各个行驶时点的排序，一个可行的办法是改为使用
# raw_data_val <- raw_data_test[raw_data_test$trip_id %in% decri15$trip_id , ], 其中decri15$trip_id中全都是valid trip，
# 我们只取raw_data_test中含有decri15$trip_id的trip_id
raw_data_test$trip_id <- paste(raw_data_test$SimNum,raw_data_test$trip,sep = "")
decri15$trip_id <- paste(decri15$SimNum,decri15$trip,sep = "")
raw_data_val <- raw_data_test[raw_data_test$trip_id %in% decri15$trip_id , ]

# ------------------------------------------------生成初始指标---------------------------------------------------
instability <- function(x){
  Speed_next <- x[-1]
  Speed_now <- x[-length(x)]
  fit <- lm(Speed_next ~ Speed_now)
  fit <- summary(fit)
  return(fit$r.squared)
}

raw_data_val$trip_id <- as.numeric(raw_data_val$trip_id)
val_trip_group <- dplyr::group_by(raw_data_val,trip_id)
val_trip_summary_temp <- dplyr::summarise(val_trip_group,
                                          SimNum = SimNum[1],
                                          trip = trip[1],
                                          obs.num = length(trip_id),
                                          # drivingHour = sum(dtime,na.rm = T)/3600,
                                          # fatigue_drivingHour = (max(0,sum(dtime,na.rm = T) - 7200))/3600,
                                          meanSpeed = mean(Speed),
                                          maxSpeed = max(Speed),
                                          sdSpeed = sd(Speed),
                                          jam_obs = length(Speed[Speed < 12])/length(Speed),
                                          fast_obs = length(Speed[Speed > 90])/length(Speed),
                                          instability_coef = instability(Speed),
                                          Morning = length(hour[hour >=7 & hour <9])/length(hour),
                                          Evening = length(hour[hour >=17 & hour < 20 ])/length(hour),
                                          Midnight = length(hour[hour == 23 | (hour >=0 & hour < 3)])/length(hour)
)
val_trip_summary_temp <- as.data.frame(val_trip_summary_temp)
val_trip_summary=merge(val_trip_summary_temp,decri15,by.x = c('SimNum','trip'),by.y=c('SimNum','trip'))
val_trip_summary=val_trip_summary[c(1:13,18)]
colnames(val_trip_summary)=c('SimNum','trip','trip_id','obs.num','meanSpeed','maxSpeed','sdSpeed','jam_obs','fast_obs','instability_coef','Morning','Evening','Midnight','drivingHour')
val_trip_summary$fatigue_drivingHour = ifelse(val_trip_summary$drivingHour - 2 >0,val_trip_summary$drivingHour - 2,0)
sum(!complete.cases(val_trip_summary))
#检查出有56个缺失值
#val_trip_summary$trip_id <- as.character(val_trip_summary$trip_id)
#val_trip_summary[!complete.cases(val_trip_summary),]$trip_id
#由于该段trip_id只有3个记录(去除一个速度观测后只剩两个)，所以没法做linear regression，R报了NA。
#这段trip非常短，可以认为没有意义，直接将其删去即可
#na.omit返回一个不含有任何缺失值的对象
val_trip_summary <- na.omit(val_trip_summary)
# colnames(val_trip_summary)
# 删去trip_id,SimNum4个与风险无关的变量
# 变量标准化(采用scale函数中默认标准化的方法)
val_trip_index=val_trip_summary[,5:15]
val_trip_index <- scale(val_trip_index,center = T,scale = T)
summary(val_trip_index)
# summary(val_trip_index)
# 检查有没有缺失值
val_trip_index <- na.omit(val_trip_index)


# --------------------------------------------------出险情况----------------------------------------------
insuance_data=read.csv('C:/Users/lenovo/Desktop/insurance_data.csv',header = T,sep=',')
company_data=read.csv('C:/Users/lenovo/Desktop/company_data.csv',header = T,sep=',')
length(unique(insuance_data$SimNum))
# 保险公司出险一次及以上的车辆有16个
# 给出险的车辆打上出险的标记
insuance_data=data.frame(SimNum=as.character(insuance_data$SimNum),risk=rep(1,length(insuance_data$SimNum)))
# 检查重复值，确定没有duplicated的行
insuance_data=unique(insuance_data)
length(unique(company_data$SimNum))
# 物流公司显示出险的车辆有28个，在这28个里面，有的既没有记录报案、也没有记录事故责任和事故损失
# 我们只选取已经报案的车辆，如果没有报案，我们认为只是小剐蹭，不足以体现其风险，所以不认为出事故
company_data=company_data[which(company_data$report=='是'),]
company_data=data.frame(SimNum=as.character(company_data$SimNum),risk=rep(1,length(company_data$SimNum)))
nrow(company_data)
# 已经报案的车辆有16个，物流公司的数据与保险公司的数据在数量上相吻合(但是在车辆编号上并不吻合)
# 检查重复值，确定没有duplicated的行
company_data=unique(company_data)
# company_data[which(!company_data$SimNum %in% insuance_data$SimNum),]
# # 以上代码生成四个结果，这说明物流公司有一部分已经报案的损失没有得到保险公司赔付
# insuance_data[which(!insuance_data$SimNum %in% company_data$SimNum),]
# # 以上代码生成4个结果，这说明保险公司有一部分已经赔付的损失，没有被物流公司记录在已经报案的数据内
# SimNum <- merge(insuance_data,company_data,by = "SimNum", all = T)
# SimNum <- SimNum[complete.cases(SimNum),]
# nrow(SimNum)
# # 以上代码告诉我们仅有12辆车是已经报案(记录在物流公司已报案数据中)，且获得赔付的(记录在保险公司已赔付的数据中)
# # so, 概率论公式告诉我们, 并集的结果是16 + 16 -12 = 20个 
risk_data=rbind(company_data,insuance_data)
length(unique(risk_data$SimNum))
# 保险公司与物流公司的出事故并集为20个
# 我们也最终选定这20个出险的车
# risk_data记录了出险的车辆编号SimNum
risk_data=unique(risk_data)


# --------------------------------------------------因子分析------------------------------------------------------------
# 先做相关系数矩阵
cor(val_trip_index)
# 有一些变量之间存在着严重的多重共线性，有必要降维处理
# # 使用主成分分析，得到各个变量的标准差(平方后得到方差)
# # 画出各个主成分的方差图
screeplot(princomp(val_trip_index),type="lines")
# 因子分析
FA_select=data.frame(FA_num=0,Vars_Cum=0)
# factpc用于调用因子分析，传入的i是因子个数，rotataion代表着因子旋转的方式(差异化variance)
# 导入一个FA_select，用来储存不同因子数目下(FA_num)，的累积方差贡献率FA_select[i,2] = FA$Vars[i,3]

for (i in 2:6){
  FA_select[i,1]=i
  FA=factpc(val_trip_index,i,rotation="varimax")
  FA_select[i,2]=FA$Vars[i,3]
}
FA_select
# 选定因子数目为6
FA=factpc(val_trip_index,6,rotation="varimax")
FA$Vars
# 此时方差贡献率为87.46%

#得到旋转因子载荷矩阵矩阵FA$loadings
z=round(x=FA$loadings,digits=3)
# 创建一个长度为11的空向量
commonality=vector(length=11)
# 用commonality记录因子载荷矩阵每一行对应五个因子载荷的平方和
for(i in 1:11){
  commonality[i]=round(sum(z[i,1:5]^2),3)
} 


# 将共同度commonality与因子载荷矩阵合并,并按照共同度最大的变量对数据框的行重新排序
z <- cbind(z[,1:6],commonality)
z <- as.data.frame(z)
row.names(z) <- colnames(val_trip_index)
z <- z[order(z$commonality,decreasing = T),]
# 从commonality的大小来看，factpc应该已经自动地将变量标准化了(一般的标准化方法是正态标准化)

#得到因子得分
FA=factpc(val_trip_index,6,rotation="varimax")
FA_score=FA$scores
summary(FA_score)
colnames(FA_score)=c(paste0("Factor",1:6))
index_temp=cbind(val_trip_summary,FA_score)

# -----------------------------------------------车辆的驾驶风险因子-------------------------------------------------------
#对每个行程的初始因子和因子得分求平均，得到车辆的驾驶风险因子
index_group <- dplyr::group_by(index_temp,SimNum)
# index <- dplyr::summarise(index_group,
#                           mean_drivingHour = sum(drivingHour/sum(obs.num)),
#                           ratio_fatigue_drivingHour = sum(fatigue_drivingHour)/sum(drivingHour),
#                           mean_Morning = sum(Morning*obs.num/sum(obs.num)),
#                           mean_Evening = sum(Evening*obs.num/sum(obs.num)),
#                           mean_Midnight = sum(Midnight*obs.num/sum(obs.num)),
#                           mean_meanSpeed = sum(meanSpeed*obs.num/sum(obs.num)),
#                           mean_sdSpeed = sum(sdSpeed*obs.num/sum(obs.num)),
#                           mean_jam_obs =  sum(jam_obs*obs.num/sum(obs.num)),
#                           mean_fast_obs = sum(fast_obs*obs.num/sum(obs.num)),
#                           mean_instability = sum((instability_coef/mean(instability_coef,na.rm = T))*obs.num/sum(obs.num),na.rm = T),
#                           max_maxSpeed = max(maxSpeed),
#                           mean_Factor1= sum(Factor1*obs.num/sum(obs.num)),
#                           mean_Factor2 = sum(Factor2*obs.num/sum(obs.num)),
#                           mean_Factor3 = sum(Factor3*obs.num/sum(obs.num)),
#                           mean_Factor4 = sum(Factor4*obs.num/sum(obs.num)),
#                           mean_Factor5 = sum(Factor5*obs.num/sum(obs.num)),
#                           mean_Factor6 = sum(Factor6*obs.num/sum(obs.num))
# )
index <- dplyr::summarise(index_group,
                          mean_drivingHour = mean(drivingHour),
                          ratio_fatigue_drivingHour = sum(fatigue_drivingHour)/sum(drivingHour),
                          mean_Morning = mean(Morning),
                          mean_Evening = mean(Evening),
                          mean_Midnight = mean(Midnight),
                          mean_meanSpeed = mean(meanSpeed),
                          mean_sdSpeed = mean(sdSpeed),
                          mean_jam_obs =  mean(jam_obs),
                          mean_fast_obs = mean(fast_obs),
                          mean_instability = mean(instability_coef),
                          max_maxSpeed = max(maxSpeed),
                          mean_Factor1= mean(Factor1),
                          mean_Factor2= mean(Factor2),
                          mean_Factor3= mean(Factor3),
                          mean_Factor4= mean(Factor4),
                          mean_Factor5= mean(Factor5),
                          mean_Factor6= mean(Factor6)
)
head(index)
#将车辆风险指标与车辆驾驶行为风险数据进行拼接
index=data.frame(index)
simu=c(as.character(index$SimNum))
index$SimNum=simu
index <- merge(risk_data,index,by.x = 'SimNum',by.y='SimNum',all=T)
index[is.na(index$risk),]$risk <- 0

# -----------------------------------------------逻辑回归-------------------------------------------------
index <- read.csv(file = "C:/Users/lenovo/Desktop/在线实习任务/index.csv",header = T, sep = ",")
index <- select(index,-X,-pre)
# summary(index)
# 所有初始指标回归
logitfit=glm(formula=risk~ mean_meanSpeed+max_maxSpeed+
               mean_sdSpeed+mean_jam_obs+mean_fast_obs+mean_instability+
               mean_Morning+mean_Evening+mean_Midnight+
               mean_drivingHour,data=index,family = binomial(link=logit))
# summary(logitfit)    #仅mean_Morning显著

#所有因子得分回归
# logitfit=glm(formula=risk~ mean_Factor1+mean_Factor2+mean_Factor3+mean_Factor4+
#                mean_Factor5+mean_Factor6,data=index,family = binomial(link=logit))
# summary(logitfit)   #mean_Factor3和mean_Factor5显著

#逐步回归
fit=glm(formula=risk~ mean_meanSpeed+mean_instability+mean_Morning+mean_Evening+mean_Midnight,data=index,family = 'binomial')
summary(fit)
#0.1置信水平下显著

#逻辑回归预测值
index$pre=predict.glm(fit,data=index,type = 'response')
# index$pre=predict.glm(fit,data=index,type = 'terms')
# write.csv(index,"C:/Users/lenovo/Desktop/index.csv")
# index=read.csv("C:/Users/lenovo/Desktop/index.csv")

#按分位点将预测值分成五组，并求每组平均值
labels = c("低风险", "偏低","中等", "偏高","高风险")
break1=quantile(index$pre,probs = c(0.2,0.4,0.6,0.8))
# cut中可以传入参数label，以给各个分组命名。order_result = T 意味着这是一个有序因子(默认是无序因子))
level<-cut(index$pre,c(0,break1,1),labels,ordered_result = T)
table(level)
leveltable=data.frame(level=level,pre=index$pre)

rank=data.frame(ddply(leveltable,.(level),function(x){mean(x$pre)}))
colnames(rank)=c("level","mean_pre")
rank$mean_pre=round(rank$mean_pre,digits = 4)
# signif(rank, digits = 6)
str(rank)
rank$labels <- paste0(rank$mean_pre*100,"%")
rank$level <- factor(rank$level,levels = c("高风险","偏高","中等","偏低","低风险"))
ggplot(rank,aes(x=level,y=mean_pre,fill=level))+
  geom_bar(position = "dodge",stat = "identity",width = 0.8,alpha = 0.5)+
  geom_text(aes(x=level,y=mean_pre,label=labels),vjust = -0.5)+
  # geom_text(x= rank$level[5],y=mean(index$pre),label=as.character(round(mean(index$pre)),2))+
  labs(x='风险等级',y='平均出险率',cex = 1.2)+
  # scale_x_discrete(breaks = c("高风险","偏高风险","中等风险","偏低风险","低风险"))
  geom_hline(aes(yintercept=mean(index$pre)),size=1,linetype="dashed") + 
  scale_y_continuous(breaks = c(0,0.25,mean(index$pre),0.5,0.75))

#这个位置我没能把五个柱状图按从大到小的顺序排序，并在虚线上加“平均出险”的标签
#计算预测准确率，如果预测值高于60%分位点(0.581158)，认为风险高车辆
correct=index[which((index$risk==1 & index$pre>=0.581158)|(index$risk==0 & index$pre<0.581158)),]
#预测正确42个，准确率84%


# -----------------------------------------------雷达图-------------------------------------------------
#每个指标的分数
indexscore = function(x){
  score = (x-min(x))/(max(x)-min(x))
  return(score)
}

index_ggradar=data.frame(
  meanSpeed=indexscore(index$mean_meanSpeed),
  Morning=indexscore(index$mean_Morning),
  Evening=indexscore(index$mean_Evening),
  Midnight=indexscore(index$mean_Midnight),
  stability=indexscore(index$mean_instability))
summary(index_ggradar)
simu=c(as.character(index$SimNum))
is.character(simu)
# 表示在200位数以内不使用科学计数法
options(scipen=200)
simu1=c(as.numeric(as.character(index$SimNum)))
is.character(simu1)
index_ggradar=data.frame(cbind(simu,simu1,index_ggradar))

#车辆选取
#pre max 对应的车64610283841
as.character(index[which(index$pre==max(index$pre)),]$SimNum)
ggradar(index_ggradar[14,2:7],group.point.size = 6,group.colours ='dark blue')

#pre min 对应的车64610283811
as.character(index[which(index$pre==min(index$pre)),]$SimNum)
#64610283811
ggradar(index_ggradar[34,2:7],group.point.size = 6)

#64610283826随机取得
ggradar(index_ggradar[6,2:7],group.point.size = 6,group.colours ='dark green')

index_ggradar_temp <- index_ggradar[c(14,34,6),2:7]
colnames(index_ggradar_temp) <- c("simu1","平均速度","早高峰出行占比","晚高峰出行占比","深夜出行占比",
                                  "驾驶平稳性")
index_ggradar_temp$simu1 <- c("预测风险最大","预测风险最小","预测风险中等")

#三张放在一起
ggradar(index_ggradar_temp,group.point.size = 6)
