
library(readxl)
library(readxl)
df_cococola <- read_excel("D://Study Material//DataScience//Forecasting//CocaCola_Sales_Rawdata.xlsx")
df_cococola = readxl::read_excel(file.choose())
str(df_cococola)
df_cococola$Quarter
plot(df_cococola$Sales) # its increased on the basis of quarters
boxplot(df_cococola$Sales) # No outliers
nrow(df_cococola)
# __________________Pre processing my data__________________________

year <- rep(1986:1996,c(rep(4,length(1986:1996))))[1:42]
Quarters<- data.frame(outer(rep(c("Q1","Q2","Q3","Q4"),length = nrow(df_cococola)),c("Q1","Q2","Q3","Q4"),"==") + 0 )
colnames(Quarters) <- c("Q1","Q2","Q3","Q4")

df_cococola1 <-data.frame(year,Quarters,rn=1:nrow(df_cococola),Sales=df_cococola$Sales)
head(df_cococola1)
colnames(df_cococola1)[2]<-"Sales"
colnames(df_cococola1)
df_cococola1["t"]<-c(1:42)
View(df_cococola1)
df_cococola1["log_rider"]<-log(df_cococola1["Sales"])
df_cococola1["t_square"]<-df_cococola1["t"]*df_cococola1["t"]
attach(df_cococola1)
# Train Test splitting
df_train <-df_cococola1[1:30,]
df_test <- df_cococola1[31:42,]


View(df_cococola1)
attach(df_cococola1)

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=df_train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =df_test))
rmse_linear<-sqrt(mean((df_test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model<-lm(Sales~t,data=df_train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=df_test))
rmse_expo<-sqrt(mean((df_test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model<-lm(Sales~t,data=df_train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=df_test))
rmse_Quad<-sqrt(mean((df_test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q2+Q3+Q4,data=df_train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=df_test,interval='predict'))
rmse_sea_add<-sqrt(mean((df_test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~Q2+Q3+Q4,data=df_train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=df_test))
rmse_Add_sea_Quad<-sqrt(mean((df_test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(Sales~Q2+Q3+Q4,data=df_train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=df_test,interval='predict'))
rmse_multi_sea<-sqrt(mean((df_test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Additive Seasonality ##########################

multi_add_sea_model<-lm(Sales~Q2+Q3+Q4,data=df_train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=df_test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((df_test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)



