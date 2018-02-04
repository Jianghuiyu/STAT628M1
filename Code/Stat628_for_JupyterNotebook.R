rm(list=ls())
require("ggplot2")
require("GGally")
require("glmnet")
require("DAAG")
data_body<-read.csv("BodyFat.csv")
density=data_body$DENSITY
data_body=data_body[,-c(1,3)]#delete the ID and density
#### Step0:Select the model
ggplot(data = data_body) + 
  geom_point(mapping = aes(x = BODYFAT, y = AGE))+
  geom_smooth(mapping = aes(x = BODYFAT, y = AGE),se=FALSE)+ theme_bw()

ggplot(data = data_body) + 
  geom_point(mapping = aes(x = BODYFAT, y = HEIGHT))+
  geom_smooth(mapping = aes(x = BODYFAT, y = HEIGHT),se=FALSE)+ theme_bw()

ggplot(data = data_body) + 
  geom_point(mapping = aes(x = BODYFAT, y = ABDOMEN))+
  geom_smooth(mapping = aes(x = BODYFAT, y = ABDOMEN),se=FALSE)+ theme_bw()
#Interpretation: Based on these plots, we think that the BODYFAT has the linear relationship with the other given variables except the HEIGHT,
#thus, we choose the linear regression model
#### Step1:Pro-precess the data
#(1) delete the "ridiculous" point
data_body[182,]
values=495/density[182]-450
cat(sep="","body fat(according to the formula)=",values)
#Interpretation: we notice that the 182nd has bodyfat=0;and we calculate the body fat with the density formula and the result is negative, 
#which is meaning less, thus we delete it.
data_body=data_body[-which(data_body$BODYFAT==0),]#exclude 182nd
#(2) check the outliers
#Delete the records with Cook's distance greater than 2p/n step by step
model_all=lm(BODYFAT~.,data=data_body)
ggplot(model_all, aes(seq_along(.cooksd), .cooksd))+
  geom_bar(stat="identity", position="identity")+
  xlab("Obs. Number")+ylab("Cook's distance")+
  ggtitle("Cook's distance")+theme_bw()
data_body[which(cooks.distance(model_all)==max(cooks.distance(model_all))),]
#Interpretation: the 42nd observation has the biggest cook distance value and he is really short, thus we delete this record.
data_body=data_body[-which(cooks.distance(model_all)==max(cooks.distance(model_all))),]
model_all=lm(BODYFAT~.,data=data_body)
ggplot(model_all, aes(seq_along(.cooksd), .cooksd))+
  geom_bar(stat="identity", position="identity")+
  xlab("Obs. Number")+ylab("Cook's distance")+
  ggtitle("Cook's distance")+theme_bw()
data_body[which(cooks.distance(model_all)==max(cooks.distance(model_all))),]
#Interpretation: the 39th observation has the biggest cook distance value and he is really heavy, thus we delete this record.
data_body=data_body[-which(cooks.distance(model_all)==max(cooks.distance(model_all))),]
#### Step2: Select the variables
#(1) First we use forward AIC standard
null=lm(BODYFAT~1, data=data_body)
select = step(null, scope=list(lower=null, upper=model_all), direction="forward",trace=0)
summary(select)
ggpairs(data_body[c("AGE","ABDOMEN","WEIGHT","WRIST","BICEPS")])+theme_bw()
