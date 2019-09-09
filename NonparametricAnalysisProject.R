devtools::install_github('cttobin/ggthemr')
library(ggplot2)
library(gridExtra)
library(ggthemr)
library(RColorBrewer)
library(ggthemes)
#install_github("kassambara/easyGgplot2")
library(easyGgplot2)
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(data.table)
library(rpart)
library(rpart.plot)
library(np)
library(corrgram)
library(randomForest)
library(GGally)

#the same as read.csv
mydata  <- read_csv("//Users//dianshen//Desktop//dataset_Facebook.csv")
#**********************
#descrition
#NA_col return columns that have missing value
#group_compare group and caculate the stastistics seperately（Type/Category/Paid)
#              data preparation for pie plot和linechart
#mydensity function for ploting density curve 
#piePlot pieplot
#all_box box_plot
#line_chart 
#qqline_data qq-norm plot
#myforest calculate of the random forest mse和r^2
#**********************

#**********************
# overview of the data and pre processing 
#**********************
str(as.data.frame(mydata))
mydata$Type <- as.factor(mydata$Type)
mydata$Category <- as.factor(mydata$Category)
mydata$`Post Month` <- as.factor(mydata$`Post Month`)
mydata$`Post Weekday` <- as.factor(mydata$`Post Weekday`)
# levels(as.factor(mydata$Category))
# [1] "1" "2" "3"
# levels(as.factor(mydata$Type))
# [1] "Link"   "Photo"  "Status" "Video" 

#missing daya 
NA_col <- function(data){
  na <- which(apply(data,2,anyNA));
  name <- names(na);
  data_temp <- data[,na];
  result <- apply(data_temp,2,FUN = function(x)length(which(is.na(x)==T)));
  result
}
NA_col(mydata)
# Post Hour      Paid      like     share 
# 1         1         1         4 
#  which(is.na(mydata[,6]))
# [1] 500
# which(is.na(mydata[,7]))
# [1] 500
# which(is.na(mydata[,17]))
# [1] 112
# which(is.na(mydata[,18]))
# [1] 112 121 125 165

#**********************************************
#fill missing data with mode for factors variable or mean with continuous variables
#**********************************************

mydata[500,6] <- names(table(mydata$`Post Hour`))[1]
mydata[500,7] <- names(table(mydata$Paid))[1]
mydata[112,17] <- mean(mydata$like,na.rm = T)
c <- which(is.na(mydata[,18]))
mydata[c,18] <- mean(mydata$share,na.rm = T)

#**********************************************
#calculate the statistics and count each category 
#**********************************************
group_compare <- function(group){
  temp <- group_by_(mydata,group)
  temp1 <- dplyr::summarise(
    temp,
    mean_ptl = mean(`Page total likes`),
    #sd_ptl=sd(`Page total likes`),
    mean_lptr = mean(`Lifetime Post Total Reach`),
    #sd_lptr=sd(`Lifetime Post Total Reach`),
    mean_lpti = mean( `Lifetime Post Total Impressions`),
    #sd_lpti = sd(`Lifetime Post Total Impressions`),
    mean_leu = mean(`Lifetime Engaged Users`),
    #sd_leu=mean(`Lifetime Engaged Users`),
    mean_lpcer = mean(`Lifetime Post Consumers`),
    #sd_lpcer=sd(`Lifetime Post Consumers`),
    mean_lpcion = mean(`Lifetime Post Consumptions`),
    #sd_lpcion=sd(`Lifetime Post Consumptions`),
    mean_lpi_who = mean(`Lifetime Post Impressions by people who have liked your Page`),
    #sd_lpi_who=sd(`Lifetime Post Impressions by people who have liked your Page`),
    mean_lpr_who = mean(`Lifetime Post reach by people who like your Page`),
    #sd_lpr_who=sd(`Lifetime Post reach by people who like your Page`),
    mean_who_like_engage = mean(`Lifetime People who have liked your Page and engaged with your post`),
    #sd_who_like_engage=sd(`Lifetime People who have liked your Page and engaged with your post`),
    mean_comment = mean(comment),
    #sd_comment=sd(comment),
    mean_like = mean(like),
    #sd_like=sd(like),
    mean_share = mean(share),
    #sd_share=sd(share),
    mean_interaction = mean(`Total Interactions`),
    #sd_interaction=sd(`Total Interactions`),
    n = n()
  )
  temp1 <- as.data.frame(temp1)
  temp1[,1] <- as.factor(temp1[, 1])
  temp1
}
stat1 <- group_compare("Type")
stat2 <- group_compare("Category")
stat3 <- group_compare("`Post Month`")
stat4 <- group_compare("`Post Weekday`" )
stat5 <- group_compare("`Post Hour`")
stat6 <- group_compare("Paid")

#******************
#data visualization
#******************
ggthemr("fresh")
#pieplot
pie_plot <- function(stat,fill){
  pie2 <- ggplot(stat, aes_string(x = 1, y="n", fill = fill))+
    geom_bar(width = 1, stat = "identity")+ xlab("")+ylab("")+
    labs(fill=fill)+theme(legend.position = 'right')+coord_polar("y")
  pie2
}
pie1<- pie_plot(stat1,"Type")
pie2<- pie_plot(stat2, "Category")
pie3<- pie_plot(stat3, "`Post Month`")
pie4<- pie_plot(stat4, "`Post Weekday`")
pie5<- pie_plot(stat5,"`Post Hour`")
pie6<- pie_plot(stat6, "`Paid`")
grid.arrange(pie1,pie2,pie4,pie6)
ggthemr_reset()
ggplot2.multiplot(pie3,pie5,nol=1)
ggthemr("fresh")

#**********
#line plot 
#**********
line_chart <- function(stat,x){
            new<-melt(as.data.frame(stat),id=1)
            new[,1] <- as.numeric(new[,1])
            p <- ggplot(as.data.frame(new), aes_string(x = x,y = "value",
                                                       group = "variable",color = "variable"))+
              geom_line()+ geom_point()+ xlab(x);p
              }
linechart1 <- line_chart(stat3,"`Post Month`")
linechart2 <- line_chart(stat4,"`Post Weekday`")
linechart3 <- line_chart(stat5,"`Post Hour`")
#**************
#boxplot
#**************
#rename variables
names(mydata)[1] <- "page_total_like"
names(mydata)[4] <- "Post_Month"
names(mydata)[5] <- "Post_Weekday"
names(mydata)[6] <- "Post_Hour"
names(mydata)[8:15] <- c("Lifetime_Post_Total_Reach","Lifetime_Post_Total_Impressions",
                       "Lifetime_Engaged_Users","Lifetime_Post_Consumers",
                       "Lifetime_Post_Consumptions",
                       "Lifetime_Post_Impressions_by_people_who_have_liked_your_Page",
                       "Lifetime_Post_reach_by_people_who_like_your_Page",
                       "Lifetime_People_who_have_liked_your_Page_and_engaged_with_your_post")
mydata$Post_Hour <- factor(mydata$Post_Hour)
mydata$Paid <- factor(mydata$Paid)
box_ggplot <- function(x,y,z = NULL, title, xlab, ylab, zlab = NULL){
  if(is.null(z))
  {z <- x;zlab <- xlab}
  p <- ggplot(mydata,aes_string(y = y, x = x, fill = z)) + geom_boxplot()+
    ggtitle(sprintf("%s _boxplot",title)) + xlab(sprintf("%s",xlab)) + ylab(sprintf("%s",ylab))+
    labs(fill=sprintf("%s",zlab))
  ggsave(paste(title,"_boxplot.png"))
  p
}
myoutlier <- function(varname){
  ii <- which(names(mydata) == varname)
  temp <- boxplot(mydata[,ii])
  temp_sort <- sort(temp$out,decreasing = T)[1:5]
  for(i in 1:5)
    print(which(mydata[,ii]==temp_sort[i])) 
}
all_box <- function(var){
  pbox1 <- box_ggplot("Type",var,"Paid",
                    title = paste0("Type_",var,"Paid"),
                    xlab = "Type",
                    ylab = var,
                    zlab = "Paid")
  pbox2 <- box_ggplot("Type",var,"Category",
                    title = paste0("Type",var,"Category"),
                    xlab = "Type",
                    ylab =var,
                    zlab = "Category")
  pbox3 <- box_ggplot("Category",var,"Paid",
                    title = paste0("Category",var,"Paid"),
                    xlab = "Category",
                    ylab =var,
                    zlab = "Paid")
  myoutlier(var)
  grid.arrange(pbox1,pbox2,pbox3,ncol = 3)
  ggsave(paste0("all_",var,".png"))
  list(pbox1,pbox2,pbox3)
}
box <- all_box("comment")
box2 <- all_box("share")
box3 <- all_box("like")
box4 <- all_box("Lifetime_Post_Total_Reach")
box5 <- all_box("Lifetime_Post_Total_Impressions")
box6 <- all_box("Lifetime_Engaged_Users")
box7 <- all_box("Lifetime_Post_Consumers")
box8 <- all_box("Lifetime_Post_Consumptions")
box9 <- all_box("Lifetime_Post_Impressions_by_people_who_have_liked_your_Page")
box10 <- all_box("Lifetime_Post_reach_by_people_who_like_your_Page")
box12 <- all_box("Lifetime_People_who_have_liked_your_Page_and_engaged_with_your_post")
#extreme value
# temp<-boxplot(mydata$like)
# temp$out
# which(mydata$like==5172)
# [1] 245
# which(mydata$share==790)
# [1] 245
# which(mydata$Lifetime_Post_Total_Reach==180480)
# [1] 245
mydataold <- mydata
mydata <- mydata[-245, ]

#***********
#density plot
#***********
mydensity <- function(x,xlab,f=Type){
  p <- ggplot(mydata,aes(x = x))+
    geom_histogram(aes(x = x,..density..,fill = Category),position = "identity",alpha = 0.2,bins = 30)+
    geom_density(aes(x = x,..density..,colour = Category),size=1)+xlab(xlab)
  p
}

p1 <- mydensity(mydata[,1],xlab = names(mydata)[1])
p8 <- mydensity(mydata[,8],xlab = names(mydata)[8])
p9 <- mydensity(mydata[,9],xlab = names(mydata)[9])
p10 <- mydensity(mydata[,10],xlab = names(mydata)[10])
p11 <- mydensity(mydata[,11],xlab = names(mydata)[11])
p12 <- mydensity(mydata[,12],xlab = names(mydata)[12])
p13 <- mydensity(mydata[,13],xlab = names(mydata)[13])
p14 <- mydensity(mydata[,14],xlab = names(mydata)[14])
p15 <- mydensity(mydata[,15],xlab = names(mydata)[15])
p16 <- mydensity(mydata[,16],xlab = names(mydata)[16])
p17 <- mydensity(mydata[,17],xlab = names(mydata)[17])
p18 <- mydensity(mydata[,18],xlab = names(mydata)[18])
p19 <- mydensity(mydata[,19],xlab = names(mydata)[19])
grid.arrange(p19,p8,p9,ncol = 1)
grid.arrange(p10,p11,p12,ncol = 1)
grid.arrange(p13,p14,p15,ncol = 1)
grid.arrange(p16,p17,p18,ncol = 1)


#*************
#correlation analysis
#*************
#
mydata0 <- as.data.frame(mydata[,c(3,8:19)])
names(mydata0)
ggscatmat(mydata0,color = "Category",alpha = 0.4,corMethod = "kendall",columns = 2:13)
correlation <- cor(mydata0[,2:13],method = "kendall")
corplot1 <- corrgram(mydata0[,2:13],order = T,lower.panel = panel.shade,upper.panel = panel.pts,
                   text.panel =panel.txt,diag.panel = panel.density,cor.method = "kendall")


#************
#facet qq plot
#************
qqline_data <- function(var,group,n = 2){
  intsl <- mydata %>% group_by_(group) %>% 
    summarize(q25    = quantile(var,0.25),
              q75    = quantile(var,0.75),
              norm25 = qnorm( 0.25),
              norm75 = qnorm( 0.75),
              slope  = (q25 - q75) / (norm25 - norm75),
              int    = q25 - slope * norm25)
  p <- ggplot(data = mydata,aes(sample = var))+stat_qq(distribution = "qnorm")+
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="blue")+
    facet_wrap(~Category,nrow=as.integer(n))
}
p1 <- qqline_data(`Lifetime People who have liked your Page and engaged with your post`,"Category")
qq2 <- qqline_data(share,"Type")
qq3 <- qqline_data(comment,"Type");qq3
qq4 <- qqline_data(like,"Type");qq4

saveRDS(mydata,"MYDATA.RData")
mydat<-readRDS("MYDATA.RData")
#********************
#kernel function
#********************

kernel_regression <- function(var,subset=0){
  if(subset==0){
    i <- which(names(mydata)==var)
    modelbw <- npregbw(formula = unlist(mydata[,i])~factor(Post_Hour)+
                       factor(Post_Weekday)+
                       factor(Post_Month)+
                       Type+Paid+Category+page_total_like,
                     data = mydata,
                     regtype="ll",
                     bwmethod = "cv.aic")
    model.np <- npreg(bws=modelbw)
    fit.np <- predict(model.np,data=mydata,newdata=mydata)
    plot(model.np,plot.errors.method="bootstrap",plot.error.boot.sum = 200)
    pse <- mean(unlist(mydata[,i])-fit.np)^2
    my<-unlist(mydata[,i])
    r2 <- (1-(sum((my-fit.np)^2))/(sum((my-mean(my))^2)))
    r21 <- (1-(sum((my-fit.np)^2))/(sum((my-median(my))^2)))
  }
  list(var,pse,r2,r21,model.np)
}
#********************
#kernel regression on different variables
#********************
par(mar=c(1,1,1,1))
kere <- kernel_regression("comment")
kere2 <- kernel_regression("share")
kere3 <- kernel_regression("like")
kere4 <- kernel_regression("Lifetime_Post_Total_Reach")
kere6 <- kernel_regression("Lifetime_Post_Total_Impressions")
kere7 <- kernel_regression("Lifetime_Engaged_Users")
kere8 <- kernel_regression("Lifetime_Post_Consumers")
kere9 <- kernel_regression("Lifetime_Post_Consumptions")
kere10 <- kernel_regression("Lifetime_Post_Impressions_by_people_who_have_liked_your_Page")
kere11 <- kernel_regression("Lifetime_Post_reach_by_people_who_like_your_Page")
kere12 <- kernel_regression("Lifetime_People_who_have_liked_your_Page_and_engaged_with_your_post")

plot(kere8[[5]],plot.errors.method="bootstrap",plot.error.boot.sum = 200,col=2)
npsigtest(kere8[[5]])

#************************
#prediction
#************************
c <- sample(1:499,400)
modelbw <- npregbw(formula = unlist(mydata[c,11])~factor(Post_Hour)+
                   factor(Post_Weekday)+
                   factor(Post_Month)+
                   Type+Paid+Category+page_total_like,
                 data = mydata[c,],
                 regtype = "ll",
                 bwmethod = "cv.aic")
model.np <- npreg(bws=modelbw)
fit.np <- predict(model.np,data=mydata,newdata=unlist(mydata[-c,11]))
predict.np <- as.data.frame(cbind(fit.np,orignal=mydata[-c,11]))
predict.plot <- ggplot(data = predict.np,aes(x=fit.np,y=orignal))+
  geom_point()+geom_abline(slope = 1,intercept = 0,colour="red")+
  ggtitle("evaluate the prodiction")
ggsave("predict.plot1.png")
predict.plot2 <- ggplot(data = predict.np)+
  geom_point(aes(x=1:99,y= orignal,color= "orignal value"))+
  geom_point(aes(x=1:99,y= fit.np,color= "predict value"))
ggtitle("evaluate the prodiction")+xlab("index")
ggsave("predict.plot2.png")

pp <- plot.variable.rfsrc(forest,partial = T)
gg_partial(pp)

