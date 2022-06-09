install.packages("forestplot")
install.packages("ggpubr")
install.packages("caret")
install.packages("pROC")
library("pROC")
library("caret")
library("ggpubr")
library("forestplot")
library("ggplot2")
# Add global the p-value 
install.packages("ggstatsplot")
library("ggstatsplot")
australian <- read.csv("C:\\Users\\xps13\\Desktop\\GO\\0515\\survival.csv",as.is = T,sep=",",header=TRUE)
str(australian$HDL)
for (i in names(australian)[c(5:13)]){australian[,i] <- as.factor(australian[,i])}
str(australian)
#设置随机种子，使数据分割可重复
set.seed(1)
#多次K折交叉验证,如5折400次交叉验证
folds <-createMultiFolds(y=australian$status,k=5,times=400)
#folds会产生5*400=2000个数据组合
#取fold 1数据为训练集，
train <-australian[folds[[1]],]
#其余为验证集
test <- australian[-folds[[1]],]
model<-glm(status~age+TG
           ,
           family = binomial(link=logit), 
           data=australian )
#验证队列做预测
model_pre<-predict(model,
                   type='response',
                   newdata=test)
#查看AUC值、敏感性、特异性
roc1<-roc((test$status),model_pre)
round(auc(roc1),3)
roc1$sensitivities
round(roc1$specificities,3)
#ROC可视化
plot(roc1, 
     print.auc=T, 
     auc.polygon=T, 
     auc.polygon.col="skyblue",
     grid=c(0.1, 0.2),
     grid.col=c("green", "red"), 
     max.auc.polygon=T,
     print.thres=T)
