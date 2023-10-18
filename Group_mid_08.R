pacman::p_load(dplyr,tidyverse,lubridate,stringr,rtrim,nortest,
               ISLR,ggplot2,gridExtra,sqldf,caret,readr,car,corrplot,plotly,readxl,DT,Hmisc)

# 資料前處理
car <- read.csv("car_prices.csv")
car[car==""] <- NA #把空白格轉NA
df <- na.omit(car) # drop the row with Na

# 日期切割與轉換
D = str_split(df$saledate, pattern = " ", n = Inf, simplify = TRUE) %>% as.data.frame()
D = D %>% select(V4) 
colnames(D)[1] = "saleyear"
df = select(df,-saledate) 
df = cbind(df,D)
#write.csv(df, "df.csv", row.names=FALSE,fileEncoding = "UTF-8")
df$saleyear = as.integer(df$saleyear)
df$year = as.integer(df$year)

# 計算車齡
df = df %>% mutate(vehicleage = saleyear - year+1)

df = df %>% filter(vehicleage > 0) 

TT <- df %>%
  # cleanliness_lower: cleanliness converted to lowercase
  mutate(body_A = str_to_lower(body))

save(df,file="car_prices.rdata",compress = T)

#資料分析
str(df)
options(scipen=999)

# 轉成factor
df_a<-df
df_a$year<-as.factor(df_a$year)
df_a$saleyear<-as.factor(df_a$saleyear)
df_a$vehicleage<-as.integer(df_a$vehicleage)
df_a$condition<-as.integer(df_a$condition)
df_a$vehicleage<-as.factor(df_a$vehicleage)

#####univariate analysis
lapply(names(df_a[,-2:-4]), function(x){
  if(is.character(df_a[, x] )){
    
    qplot(df_a[, x], geom = "bar",fill =df_a[, x]) + xlab(x)+
      scale_fill_discrete(name=x)
    
  }else if(is.factor(df_a[, x] )){
    qplot(df_a[, x], geom = "bar",fill = df_a[, x]) + xlab(x)+
      scale_fill_discrete(name=x)
  }
  else{
    qplot(df_a[, x], geom = "density",fill = x) + xlab(x)+
      scale_fill_discrete(name=x)+ geom_vline(aes(xintercept=mean(df_a[, x])),
                                              color="black", linetype="dashed", size=1)
  }
})

# 整理出銷售量前15名的製造商
make<-df_a %>% group_by(make) %>% summarise(n=n())
make_15<-make[order(-make$n),] %>% head(15)
make_15<-df_a %>% filter(make %in% make_15$make)
qplot(make_15$make, geom = "bar",fill = make_15$make) + xlab("make")+
  scale_fill_discrete(name="make")

# 整理出銷售量前15名的model
model<-df_a %>% group_by(model) %>% summarise(n=n())
model_15<-model[order(-model$n),] %>% head(15)
model_15<-df_a %>% filter(model %in% model_15$model)
qplot(model_15$model, geom = "bar",fill = model_15$model) + xlab("model")+
  scale_fill_discrete(name="model")

####normal distribution test
Map(function(x){
  
  if(is.integer(df_a[, x] )){
    lillie.test(df_a[, x] %>% sample(5000))}
}, colnames(df_a))

####sales best 5 make's sales best 5 model
make_5<-df_a %>% group_by(make) %>% summarise(n=n())
make_5<-make_5[order(-make_5$n),] %>% head(5)
make_model<-df_a %>% group_by(make,model)%>% summarise(n=n())%>% filter(make %in% make_5$make)
make_model<-make_model[order(make_model$make,-make_model$n),]
make_model$make<-as.factor(make_model$make)

lapply(levels(make_model$make), function(x){
  {
    
    a<-as.data.frame(make_model[make_model$make==x,] %>% head(5))
    print(a)
    df_a_5<-df_a%>% filter(make %in% a[,"make"]) %>% filter(model %in% a[,"model"]) %>% select(make,model)
    qplot(df_a_5[, "model"], geom = "bar",fill = df_a_5[, "model"]) + xlab(x)+
      scale_fill_discrete(name="model")
  }})
###condition

df_a$condition = cut(x = df_a$condition, breaks = c(-Inf, 1.4, 2.4, 3.4, 4.4,5.4),
                   labels = c('1','2','3','4','5'))
qplot(df_a$condition, geom = "bar",fill = df_a$condition) + xlab("condition")+
  scale_fill_discrete(name="condition")

#非連續
#make  model body transmission(t-test)  state color interior
df$interior <- data.frame(name = c("yellow","white","tan","gray","red","purple","orange","gray","green","gray","gold","gray","brown","blue","black","beige"))

ggplot(df, aes(x=sellingprice,y=state,fill=state)) + geom_bar(stat = "identity") + theme_minimal()
ggplot(df, aes(x=sellingprice,y=make,fill=make)) + geom_bar(stat = "identity") + theme_minimal()
plot <- ggplot(df, aes(x=sellingprice,y=interior,fill=interior)) + geom_bar(stat = "identity") + theme_minimal()
plot + scale_fill_manual(values = c("beige","black","blue","brown","gray","gold","gray","green","gray","orange","purple","red","gray","tan","white","yellow","gray"))


#相關矩陣(熱圖)
df$condition = as.numeric(df$condition)
matrix <-df%>%select(year,condition,odometer,vehicleage,sellingprice)%>%cor()
corrplot(matrix, method="color", tl.cex = 1, tl.col = "black", number.cex = 0.8,
         p.mat = matrix, sig.level = c(.001, .01, .05), insig = 'label_sig', 
         pch = 10, pch.cex = 1, pch.col = "red", type = "lower", tl.srt = 45, 
         addCoef.col = "black", addgrid.col = "white", cl.pos = "n",
         fn_left=135, fn_up = 20,
         cl.lim=c(-1, 1))

#製造年份與售出金額
lm_year = lm( sellingprice ~ year, data = df )
summary(lm_year)
cor.test(df$year, df$sellingprice, method = "pearson")
qplot(x=year, y=sellingprice, data=df, geom = c("point", "smooth")) 

#車況與售出金額
lm_condition = lm( sellingprice ~ condition, data = df )
summary(lm_condition)
cor.test(df$condition, df$sellingprice, method = "pearson")
qplot(x=condition, y=sellingprice, data=df, geom = c("point", "smooth")) 

#里程數與售出金額
lm_odometer = lm( sellingprice ~ odometer, data = df )
summary(lm_odometer)
cor.test(df$odometer, df$sellingprice, method = "pearson")
qplot(x=odometer, y=sellingprice, data=df, geom = c("point", "smooth")) 

#車齡與售出金額
lm_vehicleage = lm( sellingprice ~ vehicleage, data = df )
summary(lm_vehicleage)
cor.test(df$vehicleage, df$sellingprice, method = "pearson")
qplot(x=vehicleage, y=sellingprice, data=df, geom = c("point", "smooth")) 


#各品牌 平均車況與平均價格
df2<-df%>%group_by(make) %>% summarise(n = n())%>%top_n(15)
gg=df %>% filter(make %in% df2$make)%>%group_by(make)%>% summarise(condition=mean(condition),sellingprice=mean(sellingprice),n = n())%>%ggplot(aes(x=condition,y=sellingprice)) + geom_point(aes(col=make, size=n), alpha=0.8)+geom_text(aes(label=paste(make,sep="\n")), size=3)
ggplotly(gg)

#各州平均銷售額
df3<-df%>%group_by(state) %>% summarise(n = n())
df3<-df %>% filter(state %in% df3$state)%>%group_by(state)%>% summarise(sellingprice=mean(sellingprice),n=n())
df3$hover <- with(df3, paste("地區:",state, '<br>', "銷售量:",n,'<br>',"平均銷售額:", round(sellingprice, digits = 2) )) 
df3$state<-toupper(df3$state)
fig <- plot_geo(df3, locationmode = 'USA-states') #將表格資料與美國地圖關聯
fig <- fig %>% add_trace(
  locations = ~state,
  type='choropleth',
  z= ~n,
  text = ~hover,
  colorscale="Reds"
)
fig

#車齡 平均車況與平均價格
g=df %>% group_by(vehicleage)%>% summarise(condition=mean(condition),sellingprice=mean(sellingprice))%>%ggplot(aes(x=condition,y=sellingprice)) + geom_point(aes(col=vehicleage, size=vehicleage), alpha=0.8)
ggplotly(g)



