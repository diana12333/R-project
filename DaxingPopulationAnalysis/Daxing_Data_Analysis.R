##=============
#载入依赖包
#绘图
library(networkD3)
library("ggplot2")
library("ggthemes")
library(ggthemr)
library(RColorBrewer)
library(colorspace)
library(gapminder)
library(animation)
library(gganimate)
library("plyr")
#数据整理
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
#相关性
library(ggcorrplot)
library(corrgram)
library(Rmisc)
library(psych)
#时间序列
library(dynlm)
library(xts)
library("forecast")
library(dtwclust)
library(keras)
library("randomForest")
library(VIM)
library(pacman)
p_load(mice,mi)



##=================
#devtools::install_github('cttobin/ggthemr')
#公安平台流管数
data_1<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2018_3_2017_4.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_2<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2018_5_2017_5.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_3<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2018_5_2018_4.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_4<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2018_7_2018_6.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_5<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_7.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_12<-merge(data_1, data_2, by="镇街名称", all=TRUE)
data_34<-merge(data_3, data_4, by="镇街名称", all=TRUE)
data_1234<-merge(data_12,data_34,by="镇街名称", all=TRUE)
data_12345<-merge(data_1234,data_5,by="镇街名称", all=TRUE)
rm(data_1)
rm(data_2)
rm(data_3)
rm(data_4)
rm(data_5)
rm(data_12)
rm(data_34)
rm(data_1234)
data_12345<-data_12345[-c(6,24,25),-6]
colnames(data_12345)<-
  c("name","2018-03-31","2017-04-30","2018-05-31","2017-05-31","2018-04-30","2018-07-31","2018-06-30","2017-07-31")
data_12345_<-data_12345[,c(1,3,5,9,2,6,4,8,7)]
data_12345_reshape<-melt(data_12345,id = "name")
data_12345_reshape$variable<-as.Date(data_12345_reshape$variable)

ggplot(data= data_12345_reshape, mapping = aes(x = variable, y = value,color=name)) + 
  geom_line()+geom_point() +theme(text=element_text(family="STKaiti",size=14))


#20181022新数据
##============================
data_neo_1<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_full_2017_4_2017_9.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_neo_1<-data_neo_1[,-c(1,5,6,7)]
colnames(data_neo_1)<-c('name','2017-04-23','2017-09-30')
data_neo_1$name<-mapvalues(data_neo_1$name, from = c("生物医药", "新媒体",""), to = c("生物医药基地", "新媒体产业","总计"))
#等待连接到总的数据上去
data_neo_2<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_full_2017_4_2017_8.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_neo_2<-data_neo_2[,-c(1,5:10)]
colnames(data_neo_2)<-c('name','2017-04-23','2017-08-28')
data_neo_2$name<-mapvalues(data_neo_2$name, from = c("生物医药", "新媒体",""), to = c("生物医药基地", "新媒体产业","总计"))
#等待连接到总的数据上去
data_neo_3_xihongmen<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_xihongmen_18_8_23.txt",header = T,sep="\t",fileEncoding="UTF-16")
colnames(data_neo_3_xihongmen)<-c('name',"local_people_2018-08-23",'2018-08-23')
#等待和西红门flow链接
data_neo_4<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_2_28_2017_1_1.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_neo_4<-data_neo_4[-24,-c(1,5:6)]
colnames(data_neo_4)<-c('name','2017-02-28','2017-01-01')
data_neo_4$name<-revalue(data_neo_4$name, c("合计"="总计"))
#等待连接到总的数据上去
data_neo_5<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_3.txt",header = F,sep="\t",fileEncoding="UTF-16")
data_neo_5<-data_neo_5[-21,-1]
colnames(data_neo_5)<-c('name','2018-03-31')
data_neo_5$name<-mapvalues(data_neo_5$name, from ="", to="总计")
#等待连接到总的数据上去
data_neo_6<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_2.txt",header = F,sep="\t",fileEncoding="UTF-16")
data_neo_6<-data_neo_6[-21,-1]
colnames(data_neo_6)<-c('name','2018-02-28')
data_neo_6$name<-mapvalues(data_neo_6$name, from ="", to="总计")

data_neo_7<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_1.txt",header = F,sep="\t",fileEncoding="UTF-16")
data_neo_7<-data_neo_7[-21,-1]
colnames(data_neo_7)<-c('name','2018-01-31')
data_neo_7$name<-mapvalues(data_neo_7$name, from ="", to="总计")

data_neo_8<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_6.txt",header = F,sep="\t",fileEncoding="UTF-16")
data_neo_8<-data_neo_8[-21,-1]
colnames(data_neo_8)<-c('name','2017-06-30')
data_neo_8$name<-mapvalues(data_neo_8$name, from ="", to="总计")

data_neo_9<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_xihongmen_2018_05.txt",header = T,sep="\t",fileEncoding="UTF-16")
colnames(data_neo_9)[2]<-"2018-05-31"
##连接总的数据
data_12345_neo<-data_12345
data_12345_neo<-Reduce(function(x,y) merge(x,y,by="name",all=TRUE) ,list(data_12345_neo,data_neo_1,data_neo_2,data_neo_4,data_neo_5,data_neo_6,data_neo_7,data_neo_8))
data_12345_neo<-data_12345_neo[ , order(names(data_12345_neo))]
colnames(data_xihongmen_flow)[1]<-"name"
data_xihongmen_flow_neo<-Reduce(function(x,y) merge(x,y,by="name",all=TRUE) ,list(data_xihongmen_flow,data_neo_3_xihongmen,data_neo_9))
data_xihongmen_flow_neo<-data_xihongmen_flow_neo[-c(12:19),c(1,8,2,7,3)]
data_xihongmen_flow_neo$`2018-06-30`<-ceiling((data_xihongmen_flow_neo[,2]+data_xihongmen_flow_neo[,3])/2)
data_xihongmen_flow_neo<-data_xihongmen_flow_neo[ , order(names(data_xihongmen_flow_neo))]
rm(data_neo_1);rm(data_neo_2);rm(data_neo_3_xihongmen);rm(data_neo_4);rm(data_neo_5);rm(data_neo_6);rm(data_neo_7);rm(data_neo_8);rm(data_neo_9)
data_423<-data_12345_neo$`2017-04-23.x`
colnames(data_12345_neo)[13]<-"2018-03-31"
data_12345_neo<-data_12345_neo[,-c(3:4,14)]


##2018—10-26 newdata
data_neo_10<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_03.txt",header = F,sep="\t",fileEncoding="UTF-16")
data_neo_10<-data_neo_10[-21,]
colnames(data_neo_10)<-c('name','2017-03-31')
data_neo_11<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_10.txt",header = F,sep="\t",fileEncoding="UTF-16")
data_neo_11<-data_neo_11[-21,]
colnames(data_neo_11)<-c('name','2017-10-31')
data_neo_12<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_11.txt",header = F,sep="\t",fileEncoding="UTF-16")
data_neo_12<-data_neo_12[-21,]
colnames(data_neo_12)<-c('name','2017-11-30')
data_neo_13<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//people_flow_2017_03.txt",header = T,sep="\t",fileEncoding="UTF-16")
data_neo_13<-data_neo_13[-21,]
colnames(data_neo_13)<-c('name','2017-12-31')
data_12345_neo2<-Reduce(function(x,y) merge(x,y,by="name",all=TRUE) ,list(data_12345_neo,data_neo_10,data_neo_11,data_neo_12,data_neo_13))
data_12345_neo2<-data_12345_neo2[ , order(names(data_12345_neo2))]

##===================
#西红门各村数据
##===================
data_xihongmen<-read.csv("20180914.csv")
data_xihongmen<-data_xihongmen[1:11,]
data_xihongmen_flow<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//xihongmen_village_2018_7_8_9.txt",header = T,sep="\t",fileEncoding="UTF-16")
colnames(data_xihongmen_flow)[2:3]<-c("2018-07-31","2018-09-27")
colnames(data_xihongmen)<-c("name","household-2017-06","power-2017-06","household-2018-06","power-2018-06","household-2017-07","power-2017-07","household-2018-07","power-2018-07","household-2017-08","power-2017-08","household-2018-08","power-2018-08")
data_xihongmen_full<-merge(data_xihongmen,data_xihongmen_flow,by = "name",all = T)


#街镇的个数
data_village_no<-read.table("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//people_flow_data//data_village_no.txt",header = T,sep="\t",fileEncoding="UTF-16")

##水电总数据
##===================
data_Daxing_<-read.csv("//Users//dianshen//Desktop//大兴区-开发区经济社会发展月报//大兴区用水用电总体情况-大兴区用水用电总体情况.csv")
str(data_Daxing_)
data_Daxing<-data.frame(t(as.matrix(data_Daxing_)))
data_daxing_2<-data_Daxing
data_daxing_2[is.na(data_daxing_2)]<-0
dimension<-dim(data_daxing_2)[1]
data_Daxing_diff<-data_daxing_2[2:dimension,]-data_daxing_2[1:(dimension-1),]
data_Daxing_diff[c(11,22,34),]<-data_daxing_2[c(12,23,34),]
multiple_line(data_reshape(data_Daxing_diff))
#theme_economist	类似经济类图表
#theme_economist_white	
data_Daxing_diff_what<-data_Daxing_diff[,c(1,4,2,3,7,5,6,8,11,10,9,12,13,18,16,14,15,17,19:22)]

data_Daxing_diff_<-data_reshape(data_Daxing_diff_what)
data_Daxing_diff_$time<-data_Daxing_diff_$V3
data_Daxing_diff_<-data_Daxing_diff_%>%separate(.,col = V3,into = c('year','month','day'),sep = '-')
ggplot(data= data_Daxing_diff_, mapping = aes(x = time, y = values,color = ind)) +facet_wrap(~ind)+ geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=14))

##=================
data_Daxing_diff[5,9]<-5284
data_Daxing_diff[35,3]<-1000
data_Daxing_diff[12,6]<-980

##条形图
ggthemr('dust')
data_bar<-data_12345_reshape

#画折线图
for(i in 1:23){
  name<-i%%3
  if(name==1){
    p1<-ggplot(data= data_Daxing_diff_[(37*(i-1)+1):(37*i),], mapping = aes(x = time, y = values)) + geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=18))+xlab(title[i])
    if (i<11){
      p1<-p1+ylab("万千瓦时")
    }
    else{
      p1<-p1+ylab("万立方米")
    }
  }
  else if(name==2){
    p2<-ggplot(data= data_Daxing_diff_[(37*(i-1)+1):(37*i),], mapping = aes(x = time, y = values)) + geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=18))+xlab(title[i])
    if (i<11){
      p2<-p2+ylab("万千瓦时")
    }
    else{
      p2<-p2+ylab("万立方米")
    }
  }
  else if(name==0){
    p3<-ggplot(data= data_Daxing_diff_[(37*(i-1)+1):(37*i),], mapping = aes(x = time, y = values)) + geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=18))+xlab(title[i])
    if (i<11){
      p3<-p3+ylab("万千瓦时")
    }
    else{
      p3<-p3+ylab("万立方米")
    }
    jpeg(filename = paste("Myplot",floor(i/3),".jpeg"), pointsize =12, quality = 200, bg = "white", res = NA)
    multiplot(p1,p2,p3)
    dev.off()
  }
  print(paste("SAVEING..",i))
}
#画分年折线图
Draw_line<-function(data){
  title<-c("全社会用电量","分行业用电","第一产业用电","第二产业用电","工业用电","建筑业用电","第三产业用电",
           "城乡居民用电","城镇居民用电","乡村居民用电","全社会用水量","农业用水","工业用水","建筑业用水",
           "家庭居民生活用水","城镇居民用水","农村居民用水","公共服务用水","环境卫生用水","园林绿化用水",
           "农村生态用水","河湖补水")
  for(i in 1:22){
    tmp=data[(37*(i-1)+1):(37*i),]
    print(37*(i-1)+1)
    print(title[i])
    if(i<11){
      ggplot(data= tmp, mapping = aes(x = as.numeric(month), y = values,color = year)) +
        geom_line()+geom_point()+scale_x_continuous(breaks=as.numeric(tmp$month), labels = tmp$month) +
        xlab("month")+ylab("万千瓦时")+theme(text=element_text(family="STKaiti",size=9))+ggtitle(title[i])
    }
    else{
      ggplot(data= tmp, mapping = aes(x = as.numeric(month), y = values,color = year)) +
        geom_line()+geom_point()+scale_x_continuous(breaks=as.numeric(tmp$month), labels = tmp$month) +
        xlab("month")+ylab("万立方米")+theme(text=element_text(family="STKaiti",size=9))+ggtitle(title[i])
    }
    ggsave(str_c(title[i],"_",".jpg"),width = 13,height = 3.5)
    
  }
}
Draw_line(data_Daxing_diff_)

data_Daxing_reshape_<-data_reshape(data_daxing_2)


ggplot(data= data_Daxing_diff_, mapping = aes(x = time, y = values,color = ind)) +
  geom_line()+geom_point()
+scale_x_continuous(breaks=as.numeric(data_Daxing_diff_$time), labels = data_Daxing_diff_$time)

#为大兴的数据加上时间
data_reshape<-function(data_daxing_2){
  xx<-stack(data_daxing_2)
  date<-seq(as.Date("2015-02-01"), length=40, by="month")
  date<-date[-c(12,24,36)]
  xx[,3]<-rep(date,22)
  return(xx)
}
#多条线的折线图
multiple_line<-function(xx){
  ugly <- define_palette(
    swatch = rainbow_hcl(dim(data_daxing_2)[2]+1),
    gradient = c(lower = random_colours[1L], upper = random_colours[2L])
  )
  ggthemr(ugly)
  ggplot(data= xx, mapping = aes(x = V3, y = values,color=ind)) + geom_line()+geom_point() 
}

##pie图
##=====================
pie_data = data_Daxing_diff[,c(2,3,4,7)]
pie_data[,1]=pie_data$X3+pie_data$X4+pie_data$X7
pie_data[,c(2,3,4)]<-pie_data[,c(2,3,4)]/pie_data[,1]
pie_data$time<-date
pie_data<-melt(pie_data,id=c("X2","time"))

ggthemr('flat')
ggplot(pie_data, aes(x=factor(1), y=value,fill=factor(variable))) +
  geom_bar(width = 1,stat = "identity",position = "stack")+
  coord_polar(theta = "y",start = 0)+
  # Here comes the gganimate specific bits
  labs(title = 'time: {frame_time}', x = 'production', y = '') +
  transition_time(time) +
  ease_aes('linear')


#保存动图
p<-ggplot(pie_data, aes(x=factor(variable), y=value,fill=factor(variable))) +
  geom_bar(width = 1,stat = "identity",position = "stack")+
  # Here comes the gganimate specific bits
  labs(title = 'time: {frame_time}', x = 'production', y = '') +
  transition_time(time) +
  ease_aes('linear')
save_animation(p,"version1.gif")
anim_save("version1.gif",p)

#拆分时间(流管平台)
data_12345_reshape$time<-data_12345_reshape$variable
data_12345_reshape<-data_12345_reshape%>%
  separate(time,into = c("year","month","day"),sep = "-")%>%
  select(.,-6)
save(data_12345_reshape,file = "liuguan_cleaned.RData")  


##=====================
#等待运营商数据
##人口数字和水电数字的合并
par(family='STKaiti')
data_correlation<-data_Daxing_diff[c(24,25,27,35:37),-c(14,19,21)]
data_sum_tmp<-colSums(data_12345_[,2:7])
data_correlation[,20]<-data_sum_tmp
cor(data_correlation)[,20]
corrgram(data_correlation[,-c(17,18,19)],diag.panel=panel.density,upper.panel=panel.cor,main = "水电变化与人口相关性")
#变量映射图
print(rbind(colnames(data_Daxing_diff),title))
#相关性检验
corr.test(data_correlation[,-c(17,18,19)])
##====================
#描述性分析
#季节性分析
barplot(apply(data_Daxing_diff,2,sd),cex.names = 0.4)
#======+=========
#村镇聚类1.无差别聚类

hc <- hclust(dist(data_12345_[,2:9]), "ave")
plot(hc)
plot(hc, hang = -1)
#村镇聚类2.时间序列聚类
pc.l2 <- tsclust(data_12345_[,5:9], k = 4L,
                 distance = "L2", centroid = "pam",
                 seed = 3247, trace = TRUE,
                 control = partitional_control(nrep = 10L))
hc.l2 <- tsclust(data_12345_[,5:9], type = "hierarchical",
                 k = 4L, trace = TRUE,
                 control = hierarchical_control(method = "all",
                                                distmat = pc.l2[[1L]]@distmat))
plot(hc.l2[[1]])
#What type of clustering method to use: "partitional", "hierarchical", "tadpole" or "fuzzy".
hc.l2 <- tsclust(data_12345_[,5:9], type = "fuzzy",
                 k = 4L, trace = TRUE,
                 control = fuzzy_control(fuzziness = 2, iter.max = 100L, delta = 0.001,
                                         packages = character(0L), symmetric = FALSE, version = 2L,
                                         distmat = NULL))
#可视化模糊聚类
##===============
rc <- rainbow(nrow(hc.l2@fcluster), start = 0, end = 1)
cc <- rainbow(ncol(hc.l2@fcluster), start = 0, end = 1)
hv <- heatmap(hc.l2@fcluster, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc,
              xlab = "", ylab =  "",
              main = "")
utils::str(hv) # the two re-ordering index vectors


#partional 聚类
##===============
hc.l2 <- tsclust(data_12345_[,5:9], type = "partitional",
                 k = 4L, trace = TRUE,
                 control = partitional_control(pam.precompute = TRUE, iter.max = 100L,
                                               nrep = 1L, symmetric = FALSE, packages = character(0L),
                                               distmat = NULL, pam.sparse = FALSE, version = 2L))
tmp<-data.frame(t(rbind(hc.l2@cluster,rownames(data_12345_))))
colnames(tmp)<-c("cluster","name")
tmp<-arrange(tmp,cluster)

#
##===============
#主成分分析
p1<-prcomp(data_Daxing_diff)
p1<-prcomp(data_Daxing_diff[,1:11])
screeplot(p1,type = "lines")
par(mfrow = c(1,1))
pc1<-p1$rotation[,1]
names(pc1)<-title[1:11]
barplot(pc1,main="第一主成分",horiz=TRUE,las=1,cex.names=0.7)

pc2<-p1$rotation[,2]
names(pc2)<-title[1:11]
barplot(pc2,main="第二主成分",horiz=TRUE,las=1,cex.names=0.7)

pc3<-p1$rotation[,3]
names(pc3)<-title[1:11]
barplot(pc3,main="第三主成分",horiz=TRUE,las=1,cex.names=0.7)

##==============
#趋势预测
predict_<-data_Daxing_diff[,1]
lag_setting<-1
y_train_vec <- as.matrix(predict_[1:36])
y_train_arr <- array(data = (y_train_vec), dim = c(nrow(y_train_vec), ncol(y_train_vec)))

x_train_vec <-lag(predict_,n=lag_setting)[2:37]
x_train_vec<-as.matrix(x_train_vec)
x_train_arr <- array(data = x_train_vec, dim = c(nrow(x_train_vec), ncol(x_train_vec), 1))

model <- keras_model_sequential() 
model %>%
  layer_lstm(units            = 10, 
             input_shape      = c(1,1), 
             batch_size       = 1) %>% 
  layer_dense(units = 1)

model %>% 
  compile(loss = 'mae', optimizer = 'adam',metrics = c("logloss"))

history<-model %>% fit(x          = x_train_arr, 
                      y          = y_train_arr, 
                      batch_size = 1,
                      epochs     = 100, 
                      verbose    = 1, 
                      shuffle    = FALSE)
history <- model %>% fit(
  x_train_arr, y_train_arr, 
  epochs = 300, batch_size = 1, 
  validation_split = 0.2
)
##随机森林
##==============
pred_out <- model %>% 
  predict(x_train_arr, batch_size = 1)
library(smooth)
fit = auto.ces(data_Daxing_diff[,2])
pred = forecast(fit, 10)
plot(pred)

data_Daxing_diff_tmp<-data_Daxing_diff[1:34,]
date<-seq(as.Date("2015-02-01"), length=40, by="month")
date<-date[-c(12,13,24)]
data_Daxing_diff_tmp$time<-date[1:34]
data_Daxing_diff_tmp$date<-as.numeric(date[1:34])
data_Daxing_diff_tmp<-data_Daxing_diff_tmp%>%
  separate(time,into = c("year","month","day"),sep = "-")%>%
  select(.,-25)
data_Daxing_diff_tmp<-data_Daxing_diff_tmp[-11,]

data_Daxing_diff_tmp<-data_Daxing_diff[1:34,]
data_Daxing_diff_tmp<-data_Daxing_diff_tmp[-11,]
date<-seq(as.Date("2015-02-01"), length=40, by="month")
date<-date[-c(1,12,13,24,36)]
data_Daxing_diff_tmp$time<-date[1:33]
data_Daxing_diff_tmp$date<-as.numeric(date[1:33])
data_Daxing_diff_tmp<-data_Daxing_diff_tmp%>%
  separate(time,into = c("year","month","day"),sep = "-")%>%
  select(.,-25)



predict_rf<-function(n){
  name<-title[n]
  print(paste("modeling the",n,"th variable",name))
  
  diff.rf<-randomForest(data_Daxing_diff_tmp[,c(23,24,25)],data_Daxing_diff_tmp[,n])
  date_pred<-seq(as.Date("2018-07-01"), length=20, by="month")
  date_pred_<-data.frame(time =date_pred )
  date_pred_$time<-date_pred
  date_pred_$date<-as.numeric(date_pred)
  date_pred_<-date_pred_%>%
    separate(time,into = c("year","month","day"),sep = "-")%>%
    select(.,-3)
  diff.rf.pred<-predict(diff.rf,date_pred_)
  ggthemr('dust')
  diff.pred.all<-as.data.frame(
    rbind(cbind(c(diff.rf$predicted,diff.rf.pred),"predict"),cbind(diff.rf$y,"y")))
  diff.pred.all$V1<-c(diff.rf$predicted,diff.rf.pred,diff.rf$y)
  diff.pred.all$date<-c(date[c(1:11,13:34)],date_pred,date[c(1:11,13:34)])
  #预测值真实值
  p<-ggplot(diff.pred.all,aes(date,V1, colour=V2,group = V2))+geom_point()+geom_line()+ylab("")+
    xlab("时间")+labs(color="预测值和真实值")+ggtitle(title[n])+
    theme(text=element_text(family="STKaiti",size=18),legend.text = element_text(size = 15));p
  ggsave(paste(title[n],"pred_rd.png",sep=""),width = 13,height = 3.5)
  return(diff.rf.pred)
}

for(i in 1:22){
  predict_rf(i)
}
plot(c(date,date_pred),c(diff.rf$predicted,diff.rf.pred),col="red",type="l")
lines(date,diff.rf$y)
points(date,diff.rf$y)
points(c(date,date_pred),c(diff.rf$predicted,diff.rf.pred),col="red")

##数据异常值处理

#带总计
##==========================
data_add<-as.data.frame(cbind(colSums(data_12345_[,-1]),"总计"))
colnames(data_add)<-c("value","name")
data_add$variable<-rownames(data_add)
rownames(data_add)<-NULL
#original
data_bar<-rbind(data_12345_reshape[,c(3,1,2)],data_add)
data_bar$value<-as.numeric(data_bar$value)
#changed
data_bar<-data_bar[intersect(which(data_bar$name!="生物医药基地"),which(data_bar$name!="新媒体产业")),]
data_bar<-data_bar[which(data_bar$name!="总计"),]

ggsave('总计_分镇街流管平台人口的可视化.png',width = 12,height = 4)


##=============================
ggplot(data_bar,aes(name,value,fill=as.factor(variable)))+
  geom_bar(stat="identity",position="dodge")+xlab("镇街名字")+
  ylab('流管平台流动人口统计数(人)')+labs(fill='时间')+
  theme(text=element_text(family="STKaiti",size=15))
ggsave('分镇街流管平台人口的可视化.png',width = 12,height = 4)
##============================
data_12345_group<-data_12345_reshape%>%group_by(name)%>%
  dplyr::summarise(std=sd(value),mean=mean(value))%>%
  mutate(ci=std/mean)
ggplot(data_12345_group,aes(name,ci))+
  geom_bar(stat="identity",position="dodge")+xlab("镇街名字")+
  ylab('流动人口波动性')+theme(text=element_text(family="STKaiti",size=9))
ggsave('分镇街流管平台人口波动性.png',width = 12,height = 4)

##聚类可视化
Mycluster<-function(hc,string,num){
  labelColors = c("#CDB380", "#036564", "#EB6841", "#EDC951","#01a2d9")
  # cut dendrogram in 4 clusters
  clusMember = cutree(hc, num)
  # function to get color labels
  colLab <- function(n) {
    if (is.leaf(n)) {
      a <- attributes(n)
      labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
      attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
    }
    n
  }
  clusDendro = dendrapply(as.dendrogram(hc), colLab)
  par(family='STKaiti')
  pp<-plot(clusDendro, main = string, type = "triangle");pp
  save_plot(paste(string,".jpg"),pp)
}
corrgram(t(rbind(data_12345_[,-1],rowSums(data_12345_[,-1]))),upper.panel = panel.cor)

#线性模型
attach(data_correlation)
#有用的行 3 4 5 7 9 13 14 15 17
#有用的行 5 13 17 
#19 20 21 31:33
data_Daxing_diff_tmp
pred<-data.frame(X5=predict_rf(5),X13=predict_rf(13),X17=predict_rf(17))
pred<-pred[-c(1:4)]
X5<-data_Daxing_diff$X5[c(19:21,31:33)]
X13<-data_Daxing_diff$X13[c(19:21,31:33)]
X17<-data_Daxing_diff$X17[c(19:21,31:33)]
V20<-data_correlation$V20
attach(data_correlation)
model.lm<-lm(V20~X5+X13+X17)

pred<-data.frame(X5=data_Daxing_diff[33:37,5],X13=data_Daxing_diff[33:37,13],X17=data_Daxing_diff[33:37,17])
attach(data_correlation)
model.lm<-lm(V20~X5+X13+X17)
predict_model_lm<-predict(model.lm,pred)
predict_<-as.data.frame(predict_model_lm)
predict_$time<-seq(as.Date("2018-07-01"),length = 5,by = "month")
ggplot(predict_,aes(time,predict_model_lm))+geom_line()+geom_point()+ggtitle("下半年流动人口变化预测")+
  xlab("2018年月份")+ylab("人口数(人)")+theme(text=element_text(family="STKaiti",size=14))
ggsave(paste("1019","pred_2018.png",sep=""),width = 10,height = 4)




#聚类（新）
##==================
data_xihongmen_all<-data_xihongmen_all[,c(12,1:11,13:18)]
data_xihongmen_all<-data_xihongmen_all[,-c(7:12)]
rownames(data_xihongmen_all)<-data_xihongmen_all$name
hc <- hclust(dist(data_xihongmen_all[,-1]), "ave")
Mycluster(hc,"西红门村庄聚类情况",3)




clusMember = cutree(hc, 3)
data_xihongmen_all_hc_people<-data_xihongmen_all[,1:6]
data_xihongmen_all_hc_people$label<-clusMember
data_xihongmen_all_hc_power<-data_xihongmen_all[,c(1,7:12)]
data_xihongmen_all_hc_power$label<-clusMember


Mycluster_label<-function(data,ylab_){
  data<-melt(data,id=c("name","label"));
  p<-ggplot(data,aes(variable,value,color=as.factor(label),group=name))+geom_point()+geom_line()+xlab("时间")+
    theme(text=element_text(family="STKaiti",size=9))+ylab(ylab_);p
}




c.l2 <- tsclust(data_xihongmen_all[,7:12], k = 3L,
                distance = "L2", centroid = "pam",
                seed = 3247, trace = TRUE,
                control = partitional_control(nrep = 10L))


hc.l2 <- tsclust(data_12345_tmp, type = "hierarchical",
                 k = 4L, trace = TRUE,
                 control = hierarchical_control(method = "all",
                                                distmat = pc.l2[[1L]]@distmat))
plot(hc.l2[[1]])
#What type of clustering method to use: "partitional", "hierarchical", "tadpole" or "fuzzy".
hc.l2 <- tsclust(data_12345_[,5:9], type = "fuzzy",
                 k = 4L, trace = TRUE,
                 control = fuzzy_control(fuzziness = 2, iter.max = 100L, delta = 0.001,
                                         packages = character(0L), symmetric = FALSE, version = 2L,
                                         distmat = NULL))
##西红门数据分析
##相关数据 data_xihongmen_flow;data_xihongmen
data_xihongmen_all<-merge(data_xihongmen_flow_neo,data_xihongmen,by="name",all = T)
data_xihongmen_all<-data_xihongmen_all[,order(names(data_xihongmen_all))]

data_add<-as.data.frame(cbind(colSums(data_12345_[,-1]),"总计"))
colnames(data_add)<-c("value","name")
data_add$variable<-rownames(data_add)
rownames(data_add)<-NULL
data_bar<-rbind(data_12345_reshape[,c(3,1,2)],data_add)
data_bar$value<-as.numeric(data_bar$value)

##西红门总数据可视化
##==================
data_xihongmen_people_melt<-melt(data_xihongmen_all[,1:6],"name")
ggthemr('dust')
ggplot(data_xihongmen_people_melt,aes(name,value,fill=as.factor(variable)))+
  geom_bar(stat="identity",position="dodge")+xlab("村庄名称")+
  ylab('流动人口数（人）')+labs(fill='')+ggtitle("西红门11个重点村流动人口")+
  theme(text=element_text(family="STKaiti",size=9))
ggsave('11个村流动人口.png',width = 12,height = 4)

data_xihongmen_power_melt<-melt(data_xihongmen_all[,c(1,7:12)],"name")
data_xihongmen_power_melt$value<-data_xihongmen_power_melt$value/10000
ggthemr('dust')
ggplot(data_xihongmen_power_melt,aes(name,value,fill=as.factor(variable)))+
  geom_bar(stat="identity",position="dodge")+xlab("村庄名称")+
  ylab('售电数（万千瓦时）')+labs(fill='')+ggtitle("西红门11个重点村售电情况")+
  theme(text=element_text(family="STKaiti",size=9))
ggsave('11个村用电数.png',width = 12,height = 4)
##数据整合 前三列环比增长 后三列同比增长 并可视化
data_xihongmen_power_rate<-cbind(data_xihongmen_all[,10:12]/data_xihongmen_all[,7:9],data_xihongmen_all[,8:9]/data_xihongmen_all[7:8],data_xihongmen_all[11:12]/data_xihongmen_all[10:11])
data_xihongmen_people_rate<-data_xihongmen_all[,3:6]/data_xihongmen_all[,2:5]
data_xihongmen_people_rate<-data_xihongmen_people_rate[,2:4]
data_xihongmen_power_rate_<-data_xihongmen_power_rate-1
data_xihongmen_power_rate_$name<-rownames(data_xihongmen_power_rate_)
colnames(data_xihongmen_power_rate_)<-c("同比增长-6月","同比增长-7月","同比增长-8月","较上月增长-2017-07","较上月增长-2017-08","较上月增长-2018-07","较上月增长-2018-08","name")
data_xihongmen_power_rate_<-melt(data_xihongmen_power_rate_,"name")
df <- transform(data_xihongmen_power_rate_, judge = ifelse(value>0, 'Yes', 'No'))
ggplot(data = df, mapping = aes(x = name, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'dodge') + xlab('村庄名称')+
  ylab('增长率')+labs(fill='')+theme(text=element_text(family="STKaiti",size=9))
ggsave('11个村用电各种增长.png',width = 12,height = 4)  

##========
#相关图
data_xihongmen_all_melt<-melt(data_xihongmen_all,"name")
data_xihongmen_all_melt<-
data_xihongmen_all_melt<-data_xihongmen_all_melt%>%
  mutate(attri=)

#流动人口村镇聚类（新）&相关性分析
#新增人口
rownames(data_12345_neo)<-data_12345_neo[,16]
data_12345_neo_cl<-hclust(dist(data_12345_neo[-23,-16]))
Mycluster(data_12345_neo_cl,"按村镇流动人口变化聚类",4)





data_Daxing_diff_<-data_Daxing_diff[-11,]
date[36]<-as.Date("2018-06-01")
date<-date[-37]
data_Daxing_diff_$time<-date
data_Daxing_diff_<-data_Daxing_diff_%>%
  separate(time,c("year","month","day"),sep="-")%>%
  unite_(.,"time",c("year","month"))
data_Daxing_diff_<-data_Daxing_diff_[,-24]

data_12345_cor<-data.frame(time=colnames((data_12345_neo[23,-16])),all=t((data_12345_neo[23,-16])))
data_12345_cor<-data_12345_cor%>%
  separate(time,c("year","month","day"),sep="-")%>%
  unite_(.,"time",c("year","month"))
data_12345_cor<-data_12345_cor[,c(1,3)]

#回归分析new
#1-10 12 15 肥餐6 8 9不显著 123
##==============================
data_correlation_neo<-left_join(data_12345_cor,data_Daxing_diff_,by="time")
data_correlation_neo<-data_correlation_neo[-c(1,9,15),-2]
corrgram(data_correlation_neo[,-c(1,20:24)],diag.panel=panel.density,upper.panel=panel.cor,main = "水电变化与人口相关性")
correaltion_update<-corr.test(data_correlation_neo[,-c(1,20:24)])
correaltion_update$r
print(rbind(colnames(data_Daxing_diff),title))
View(data_correlation_neo)
data_correlation_neo_tmp<-data_correlation_neo
data_correlation_neo_tmp[c(1,8),3:24]<-data_correlation_neo_tmp[c(1,8),3:24]/2
data_cor_2017_03<-data.frame(`time`="2017_03",`总计`=675305)
data_cor_2017_03[,3:24]<-data_Daxing_diff[23,]
data_correlation_neo_tmp[13,]<-data_cor_2017_03
data_correlation_neo_tmp$time[13]<-"2017_03"
data_correlation_neo_tmp<-data_correlation_neo_tmp[order(data_correlation_neo_tmp$time),]
data_correlation_neo_tmp<-data_correlation_neo_tmp%>%
  mutate(.,yearmon=time)%>%
  separate(.,time,into = c("year","month"),sep = "_")
data_correlation_neo_tmp<-mutate(data_correlation_neo_tmp,year=as.numeric(year),month=as.numeric(month))
diff.rf.neo<-randomForest(data_correlation_neo_tmp[,c(1,2,4,15,18,26)],data_correlation_neo_tmp[,3])

#2017三月675305

##===============================
##
data_12345_neo_melt<-melt(data_12345_neo,"name")
data_12345_neo_melt$variable<-as.Date(data_12345_neo_melt$variable)
My_barPlusline<-function(i){
  data_12345_all<-data.frame(time = as.Date(colnames(data_12345_neo2)[-16]),value = as.numeric(t(data_12345_neo2[i,-16])))
  data_12345_all<-data.frame(time = as.Date(colnames(data_12345_neo2)),value = as.numeric(t(data_12345_neo2[i,])))
  ggplot(data_12345_all,aes(time,value))+geom_bar(stat = 'identity', position = 'identity')+
  geom_line(color="#EFA86E")+geom_point(color="#EFA86E")+
  ylab("流动人口数（人）")+xlab("时间轴")+ggtitle(data_12345_neo2$name[i])+
  theme(text=element_text(family="STKaiti",size=14))  
  ggsave(paste(data_12345_neo$name[i],".jpg",sep = ""),width = 5,height = 5)
  print(paste("saving the",i,"th.jpg",sep = ""))
}
for(i in 1:23){
  My_barPlusline(i)
}

model.lm_neo<-lm(总计~X1+X12+X15,data = data_correlation_neo)

date_pred<-seq(as.Date("2018-02-01"), length=20, by="month")
predict_x1<-predict_rf(1)
predict_x12<-predict_rf(12)
predict_x15<-predict_rf(15)
pred_input<-data.frame(X1=predict_x1,X12=predict_x12,X15 = predict_x15)
predict.lm_neo<-predict(model.lm_neo,pred_input)

##西红门预测
ggplot(data_xihongmen_visual_,aes(value,power,color=name,group=name))+
  geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=9))
ggplot(data_xihongmen_visual,aes(value,power,color=name,group=name))+
  geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=9))

#power 同比增长率vs log人口 比较明显的有寿保庄 金星庄 *小白楼 职院庄 *建新庄
ggplot(data_xihongmen_power_rate_2,aes(value,log(people),color=name,group=name))+
  geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=9))
#power vs people/log(people) 比较不明显：团河南和小白楼；比较明显的是大白楼和寿保庄（正）；振亚庄 建新庄 新三余 金星庄
ggplot(data_xihongmen_visual,aes(value,log(power),color=name,group=name))+
  geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=9))

#比较各村用电趋势和全社会用电 2017年 基本是强正相关 除了振亚庄 金星庄
data_xihongmen_visual_2017<-data_xihongmen_all_melt[56:88,]
data_xihongmen_visual_2017$power<-c(rep(data_Daxing_diff$X1[25],11),rep(data_Daxing_diff$X1[26],11),rep(data_Daxing_diff$X1[27],11))
ggplot(data_xihongmen_visual_2017,aes(value/10000,(power),color=name,group=name))+
  geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=9))


#2月份数据处理
data_Daxing_diff_2<-data_Daxing_diff
date[37]<-"2018-06-01"
data_Daxing_diff_2$time<-date
data_Daxing_diff_2[c(21,32),-23]<-data_Daxing_diff_2[c(21,32),-23]/2
data_Daxing_diff_2<-melt(data_Daxing_diff_2,"time")
for(i in 1:23){
  name<-i%%3
  if(name==1){
    p1<-ggplot(data= data_Daxing_diff_2[(37*(i-1)+1):(37*i),], mapping = aes(x = time, y = value)) + geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=14))+xlab(title[i])
    if (i<11){
      p1<-p1+ylab("万千瓦时")
    }
    else{
      p1<-p1+ylab("万立方米")
    }
  }
  else if(name==2){
    p2<-ggplot(data= data_Daxing_diff_2[(37*(i-1)+1):(37*i),], mapping = aes(x = time, y = value)) + geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=14))+xlab(title[i])
    if (i<11){
      p2<-p2+ylab("万千瓦时")
    }
    else{
      p2<-p2+ylab("万立方米")
    }
  }
  else if(name==0){
    p3<-ggplot(data= data_Daxing_diff_2[(37*(i-1)+1):(37*i),], mapping = aes(x = time, y = value)) + geom_line()+geom_point()+theme(text=element_text(family="STKaiti",size=14))+xlab(title[i])
    if (i<11){
      p3<-p3+ylab("万千瓦时")
    }
    else{
      p3<-p3+ylab("万立方米")
    }
    jpeg(filename = paste("Myplot",floor(i/3),".jpeg"), pointsize =12, quality = 200, bg = "white", res = NA)
    multiplot(p1,p2,p3)
    dev.off()
  }
  print(paste("SAVEING..",i))
}



predict_rf<-function(n){
  name<-title(n)
  print(paste("modeling the",n,"th variable",name))
  data_Daxing_diff_tmp_<-data_Daxing_diff_tmp
  data_Daxing_diff_tmp_[c(21,32),-c(23:25)]<-data_Daxing_diff_tmp_[c(21,32),-c(23:25)]/2
  diff.rf<-randomForest(data_Daxing_diff_tmp_[,c(23,24,25)],data_Daxing_diff_tmp_[,n])
  date_pred<-seq(as.Date("2018-07-01"), length=20, by="month")
  date_pred_<-data.frame(time =date_pred )
  date_pred_$time<-date_pred
  date_pred_$date<-as.numeric(date_pred)
  date_pred_<-date_pred_%>%
    separate(time,into = c("year","month","day"),sep = "-")%>%
    select(.,-3)
  diff.rf.pred<-predict(diff.rf,date_pred_)
  ggthemr('dust')
  diff.pred.all<-as.data.frame(
    rbind(cbind(c(diff.rf$predicted,diff.rf.pred),"predict"),cbind(diff.rf$y,"y")))
  diff.pred.all$V1<-c(diff.rf$predicted,diff.rf.pred,diff.rf$y)
  diff.pred.all$date<-c(date[c(1:11,13:34)],date_pred,date[c(1:11,13:34)])
  #预测值真实值
  p<-ggplot(diff.pred.all,aes(date,V1, colour=V2,group = V2))+geom_point()+geom_line()+ylab("")+
    xlab("时间")+labs(color="预测值和真实值")+ggtitle(title[n])+
    theme(text=element_text(family="STKaiti",size=9));p
  ggsave(paste(title[n],"pred_rd.png",sep=""),width = 13,height = 3.5)
  return(diff.rf.pred)
}
predict_x1<-predict_rf(1)
predict_x12<-predict_rf(12)
predict_x15<-predict_rf(15)
attach(data_Daxing_diff_tmp)
diff.rf.neo<-randomForest(data_correlation_neo_tmp[,c(1,2,4,15,18)],data_correlation_neo_tmp[,3])
#26
date_pred<-seq(as.Date("2018-07-01"), length=20, by="month")
data_Daxing_diff_half<-data_Daxing_diff_
data_Daxing_diff_half[c(21,32),-23]<-data_Daxing_diff_half[c(21,32),-23]/2

pred_input_half<-data.frame(time=date_pred,X1=predict_x1+rnorm(1,0,sd(data_Daxing_diff_half$X1)),X12=predict_x12+rnorm(1,0,sd(data_Daxing_diff_half$X12)),X15 = predict_x15+rnorm(1,0,sd(data_Daxing_diff_half$X15)))
pred_input_half<-pred_input_half%>%
  separate(.,time,c("year_","month_"),sep = "-")%>%
  mutate(.,year=as.numeric(year_),month=as.numeric(month_))%>%
  unite(.,"yearmon",c(year_,month_),sep = "_")
pred_input_half$yearmon<-as.factor(pred_input_half$yearmon)



pred_input<-data.frame(time=date_pred,X1=predict_x1+rnorm(1,0,sd(data_Daxing_diff_$X1)),X12=predict_x12+rnorm(1,0,sd(data_Daxing_diff_$X12)),X15 = predict_x15+rnorm(1,0,sd(data_Daxing_diff_$X15)))
pred_input<-pred_input%>%
  separate(.,time,c("year_","month_"),sep = "-")%>%
  mutate(.,year=as.numeric(year_),month=as.numeric(month_))%>%
  unite(.,"yearmon",c(year_,month_),sep = "_")
pred_input$yearmon<-as.factor(yearmon)

pred_input2<-data.frame(time=date_pred,X1=predict_x1,X12=predict_x12,X15 = predict_x15)
pred_input2<-pred_input2%>%
  separate(.,time,c("year_","month_"),sep = "-")%>%
  mutate(.,year=as.numeric(year_),month=as.numeric(month_))%>%
  unite(.,"yearmon",c(year_,month_),sep = "_")
pred_input2$yearmon<-as.factor(pred_input2$yearmon)

pred<-predict(diff.rf.neo,pred_input[,c(5:6,2:4)])
pred_neo<-predict(diff.rf.neo,pred_input[,c(5:6,2:4)])
pred_neo_half<-predict(diff.rf.neo,pred_input_half[,c(5:6,2:4)])#将2月份变成一半
pred_neo_2<-predict(diff.rf.neo,pred_input2[,c(5:6,2:4)])#没有干扰值

pred_df_2_neo<-data.frame(date=date_pred,pred=ceiling(pred_neo_2))
ggplot(pred_df_2_neo,aes(date,pred))+geom_line()+geom_point()+xlab("时间")+
  ylab("人口数")+ggtitle("预测结果")+theme(text=element_text(family="STKaiti",size=14))


pred_df_2_neo<-data.frame(date=date_pred,pred=ceiling(pred_neo_2))
ggplot(pred_df_2_neo,aes(date,pred))+geom_line()+geom_point()+xlab("时间")+
  ylab("人口数")+ggtitle("预测结果")+theme(text=element_text(family="STKaiti",size=14))


pred_df_half<-data.frame(date=date_pred,pred=ceiling(pred_neo_half))
ggplot(pred_df_half,aes(date,pred))+geom_line()+geom_point()+xlab("时间")+
  ylab("人口数")+ggtitle("预测结果")+theme(text=element_text(family="STKaiti",size=14))
