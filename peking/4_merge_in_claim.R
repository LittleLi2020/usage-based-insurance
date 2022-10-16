# --------------------------------------------------导入数据----------------------------------------------------------
library(bit64)
library(mvstats)
# library(factoextra)
library(ggplot2)
library(RColorBrewer)
# library(xlsx)
# insuance_data=read.csv('C:/Users/lenovo/Desktop/insurance_data.csv',header = T,sep=',')
# company_data=read.csv('C:/Users/lenovo/Desktop/company_data.csv',header = T,sep=',')
rm(list = ls())
insuance_data=read.csv('C:/Users/lenovo/Desktop/insurance_data.csv',header = T,sep=',')
company_data=read.csv('C:/Users/lenovo/Desktop/company_data.csv',header = T,sep=',')
load("C:/Users/lenovo/Desktop/val_trip_summary.RData")
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
# --------------------------------------------------------
# load("C:/Users/lenovo/Desktop/raw_data_val.RData")
# load("C:/Users/lenovo/Desktop/decri15.RData")
# load("C:/Users/lenovo/Desktop/在线实习任务/raw_data_val.RData")
# load("C:/Users/lenovo/Desktop/在线实习任务/decri15.RData")
# 
# instability <- function(x){
#   Speed_next <- x[-1]
#   Speed_now <- x[-length(x)]
#   fit <- lm(Speed_next ~ Speed_now)
#   fit <- summary(fit)
#   return(fit$r.squared)
# }
# 
# #改了一下指标的顺序
# raw_data_val$trip_id <- as.numeric(raw_data_val$trip_id)
# raw_data_val$dtime <- c(diff(raw_data_val$GpsTime,1),"NA")
# raw_data_val$del <- c(diff(raw_data_val$trip_id,1),1)
# raw_data_val[raw_data_val$del != 0,]$dtime <- "NA"
# raw_data_val$dtime <- as.numeric(raw_data_val$dtime)
# val_trip_group <- dplyr::group_by(raw_data_val,trip_id)
# val_trip_summary <- dplyr::summarise(val_trip_group,
#                                      SimNum = SimNum[1],
#                                      trip = trip[1],
#                                      obs.num = length(trip_id),
#                                      drivingHour = sum(dtime,na.rm = T)/3600,
#                                      fatigue_drivingHour = (max(0,sum(dtime,na.rm = T) - 7200))/3600,
#                                      meanSpeed = mean(Speed),
#                                      maxSpeed = max(Speed),
#                                      sdSpeed = sd(Speed),
#                                      jam_obs = length(Speed[Speed < 12])/length(Speed),
#                                      fast_obs = length(Speed[Speed > 90])/length(Speed),
#                                      instability_coef = instability(Speed),
#                                      Morning = length(hour[hour >=7 & hour <9])/length(hour),
#                                      Evening = length(hour[hour >=17 & hour < 20 ])/length(hour),
#                                      Midnight = length(hour[hour == 23 | (hour >=0 & hour < 3)])/length(hour)
# )
# val_trip_summary <- as.data.frame(val_trip_summary)
# sum(!complete.cases(val_trip_summary))
# 检查出有一个缺失值
# val_trip_summary$trip_id <- as.character(val_trip_summary$trip_id)
# val_trip_summary[!complete.cases(val_trip_summary),]$trip_id
# 由于该段trip_id只有3个记录(去除一个速度观测后只剩两个)，所以没法做linear regression，R报了NA。
# 这段trip非常短，可以认为没有意义，直接将其删去即可
# raw_data_val[raw_data_val$trip_id == val_trip_summary[!complete.cases(val_trip_summary),]$trip_id,]
# na.omit返回一个不含有任何缺失值的对象
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
# # 使用主成分分析，得到各个变量的标准差(平方后得到方差)
# # 画出各个主成分的方差图
# screeplot(princomp(val_trip_index),type="lines")
# --------------------------------------------------因子分析------------------------------------------------------------
# 先做相关系数矩阵
cor(val_trip_index)
# 有一些变量之间存在着严重的多重共线性，有必要降维处理
# 因子分析
FA_select=data.frame(FA_num=0,Vars_Cum=0)
# factpc用于调用因子分析，传入的i是因子个数，rotataion代表着因子旋转的方式(差异化variance)
# 导入一个FA_select，用来储存不同因子数目下(FA_num)，的累积方差贡献率FA_select[i,2] = FA$Vars[i,3]

for (i in 2:5){
  FA_select[i,1]=i
  FA=factpc(val_trip_index,i,rotation="varimax")
  FA_select[i,2]=FA$Vars[i,3]
}
FA_select
# 选定因子数目为5,
FA=factpc(val_trip_index,5,rotation="varimax")
FA$Vars
# 此时方差贡献率为86.73%
# FA_select=data.frame(FA_num=0,Vars_Cum=0)
# i =2 
#   FA_select[i,1]=i
#   FA=factpc(val_trip_index,i,rotation="varimax")
#   FA_select[i,2]=FA$Vars[i,3]
# # FA_select
# FA=factpc(val_trip_index,5,rotation="varimax")
# FA$Vars
#得到旋转因子载荷矩阵矩阵FA$loadings
z=round(x=FA$loadings,digits=3)
# 创建一个长度为11的空向量
commonality=vector(length=11)
# 用commonality记录因子载荷矩阵每一行对应五个因子载荷的平方和
for(i in 1:11){
  commonality[i]=round(sum(z[i,1:5]^2),3)
} 


# 将共同度commonality与因子载荷矩阵合并,并按照共同度最大的变量对数据框的行重新排序
z <- cbind(z[,1:5],commonality)
z <- as.data.frame(z)
row.names(z) <- colnames(val_trip_index)
z <- z[order(z$commonality,decreasing = T),]
# 从commonality的大小来看，factpc应该已经自动地将变量标准化了(一般的标准化方法是正态标准化)

#对聚类指标做聚类碎石图，碎石图中的横轴为各类的距离，纵轴为类数目
#### 对上一句注释有疑惑，横轴应该是聚类数目，纵轴应该是组内
# 计算val_trip_index各个变量的方差，然后乘以(n-1)得到其平方和
# 当只选取一个聚类时，该平方和就等于一个聚类的组内平方和
# 有多少个聚类，就有多少个组内平方和，将这个所有组的组内平方和加总，就得到了总的平方和
# df <- scale(val_trip_index,center = T, scale = T)
# wss=(nrow(df)-1)*sum(apply(df,2,var))
# for (i in 1:14){
#   wss[i]=sum(kmeans(df,centers=i)$withinss,iter.max = 25)
# }
# tpye = "b"可以将线和点同时画出
# plot(1:14, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
# 根据图像结果，将trip划分成4类
# i = 2
#   wss[i]=sum(kmeans(val_trip_index,centers=i)$withinss)
# plot(1:14, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#获得每段路程的因子得分，每行是一个行程，每列是该行程在相应因子上的得分
FA=factpc(val_trip_index,5,rotation="varimax")
FA_score=FA$scores
summary(FA_score)
colnames(FA_score)=c("车速状况","驾驶时长","安全驾驶","夜晚出行","深夜出行")
# ----------------------------------------------聚类分析----------------------------------------------------------
summary(FA_score)
df <- scale(FA_score,center = T, scale = T)
summary(df)
# scale之后df与FA_socre同，说名FA_score的量纲已经统一
wss=(nrow(df)-1)*sum(apply(df,2,var))
for (i in 1:14){
  wss[i]=sum(kmeans(df,centers=i)$withinss,iter.max = 25)
}
# tpye = "b"可以将线和点同时画出
plot(1:14, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
# 由elbow图可知，应该将各类行程分成九类
set.seed(1234)
clur=kmeans(FA_score,9)
centers <- as.data.frame(clur$centers)
# row.names(centers) <- c("低速 疲劳 安全","高速 安全 深夜","")
centers_order <-  NULL
for (i in 1:ncol(centers)){
  centers_order <- cbind(centers_order,order(centers[i]))
}
centers_order <- as.data.frame(centers_order)
colnames(centers_order) <- colnames(FA_score)
centers_tpye <- centers_order
centers_tpye[centers_order$车速状况 > nrow(centers_order)/2,]$车速状况 <- "低速驾驶"
centers_tpye[centers_order$车速状况 < nrow(centers_order)/2,]$车速状况 <- "高速驾驶"
centers_tpye[centers_order$驾驶时长 > nrow(centers_order)/2,]$驾驶时长 <- "精神驾驶"
centers_tpye[centers_order$驾驶时长 < nrow(centers_order)/2,]$驾驶时长 <- "疲劳驾驶"
centers_tpye[centers_order$安全驾驶 > nrow(centers_order)/2,]$安全驾驶 <- "危险驾驶"
centers_tpye[centers_order$安全驾驶 < nrow(centers_order)/2,]$安全驾驶 <- "安全驾驶"
centers_tpye[centers_order$夜晚出行 > nrow(centers_order)/2,]$夜晚出行 <- "白天出行"
centers_tpye[centers_order$夜晚出行 < nrow(centers_order)/2,]$夜晚出行 <- "夜晚出行"
centers_tpye[centers_order$深夜出行 > nrow(centers_order)/2,]$深夜出行 <- "非深夜出行"
centers_tpye[centers_order$深夜出行 < nrow(centers_order)/2,]$深夜出行 <- "深夜出行"

# head(val_trip_summary)
temp=table(val_trip_summary$SimNum,clur$cluster) 
temp.scale=apply(temp,1,function(x) x/sum(x)) 
temp.scale
n <- 9
colors <- brewer.pal(n, "Set3")
# colors=c("lightgreen","lightblue","lightgrey","orange")
classification <- NULL
# paste0(centers_tpye[1,1:ncol(centers_tpye)])
for (i in 1:nrow(centers_tpye)){
  classification=rbind(classification,paste0(centers_tpye[i,1:ncol(centers_tpye)]))
}
# classification <- paste0(classification[1,])
barplot(temp.scale,main = "",xlab = "比例",ylab = "车号",col = colors,horiz=T,legend.text=paste("类",c(1:9),sep=""),args.legend=list(x="top",bty="n",inset=-0.45)) 
# t(temp.scale)
index=data.frame(t(temp.scale))
simu=c(as.character(unique(val_trip_summary$SimNum)))
# risk_data=risk_data[which(risk_data$SimNum %in% simu ),]
# risk_data=data.frame(SimNum=simu,risk=rep(0,5))
# t代表转置
index$SimNum=simu
colnames(index)=c(paste0("Tpye",1:9),"SimNum")
index <- merge(risk_data,index,by.x = 'SimNum',by.y='SimNum',all.y = T,all.x = F)
index[is.na(index$risk),]$risk <- 0
save(index,classification,file = "C:/Users/lenovo/Desktop/在线实习任务/index_classification.RData")
