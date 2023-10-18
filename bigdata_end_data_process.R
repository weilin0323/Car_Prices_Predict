pacman::p_load(ISLR,ggplot2,gridExtra,sqldf,caret,readr,tidyverse,car,corrplot,plotly,xlsx,DT,Hmisc,rpart,rpart.plot,CatEncoders  )
setwd("~/Desktop/巨量/巨量報告/Group_08")
load("car_prices.rdata")
df<-df %>% filter(df$year<df$saleyear)
df$condition<-as.numeric(df$condition)
df$sellingprice<-as.numeric(df$sellingprice)
#character轉factor
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
#取出要訓練的資料
df2 <-df[c("make","model","body", "state", "condition", "odometer", "color", "interior", "vehicleage", "sellingprice")]

#df2 <-df[c("make","model","body", "state", "condition", "odometer", "color", "interior", "vehicleage", "mmr","sellingprice")]
#MMR is the premier tool for wholesale vehicle valuations, searching millions of transactions for industry-average pricing.
A = df2 %>% group_by(model) %>% summarise(n= n()) %>% filter(n < 6500)
df2 = filter(df2, !(model %in% A$model))
#先進行one hot
#model.matrix(~cut-1,df) %>% as.data.frame() #cut為要改的變數
#make_onehot<-model.matrix(~make-1,df2) %>% as.data.frame()
#model_onehot<-model.matrix(~model-1,df2) %>% as.data.frame()
#state_onehot<-model.matrix(~state-1,df2) %>% as.data.frame()
#color_onehot<-model.matrix(~color-1,df2) %>% as.data.frame()
#interior_onehot<-model.matrix(~interior-1,df2) %>% as.data.frame()
#df3<-cbind(select_if(df2,is.numeric), make_onehot, model_onehot, state_onehot,color_onehot,interior_onehot)

# 正規化
preProcessPlan <- preProcess(df2[,-10], method = c('range')) #3要改成sellingprice所在欄位
df3<-predict(preProcessPlan, df2)

#切模型
set.seed(1)
train_idx <- createDataPartition(df3$sellingprice, p = 0.7, list = F)
train_d <- df3[train_idx,]
test_d <- df3[-train_idx,]


MAE <- function(predicted, actual) return(mean(abs(predicted - actual)))

#car_purchase_lm <- lm(sellingprice ~ ., data = train_d)
#(summary(car_purchase_lm)$coefficients[,4])>0.005
#mae_train_s <- lapply(c(sellingprice ~ make,
#                        sellingprice ~ make+ model,
#                        sellingprice ~ make+ model+ state,
#                        sellingprice ~ make+ model+ state+ condition,
#                        sellingprice ~ make+ model+ state+ condition+ odometer,
#                        sellingprice ~ make+ model+ state+ condition+ odometer+ color,
#                        sellingprice ~ make+ model+ state+ condition+ odometer+ color+ interior,
#                        sellingprice ~ .
#                       ), function(f) {
#                         response <- all.vars(f)[1]
#                          mae <- MAE(train_d[, response], predict(lm(f, train_d)))
#                        })%>% unlist
#sellingprice ~ . 2483.793較低
#library(Metrics)
#rmse(data$actual, data$predicted)
dt<-rpart(sellingprice ~ ., data = train_d, control=rpart.control(maxdepth=4))
rpart.plot(dt)
rpart.rules(dt)

# Predicted values on training data
dt_train_pred <- predict(dt,newdata = train_d)
# Predicted values on testing data
dt_test_pred <- predict(dt,newdata = test_d)

# training MAE: 
round(MAE(dt_train_pred, train_d$sellingprice), 4)
# testing MAE: 
round(MAE(dt_test_pred, test_d$sellingprice), 4)

dt_importance<-data.frame(importance=dt$variable.importance)


