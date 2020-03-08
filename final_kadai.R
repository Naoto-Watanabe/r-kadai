
setwd("~/Dropbox/KBS/M1_3/business_statistics/final_kadai")

dat = read.csv("super_market.csv")
attach(dat)
library(ggplot2)

g <- ggplot(dat, aes(x = Quantity))
g <- g + geom_histogram()
plot(g)

g <- ggplot(dat, aes(x = Branch))
g <- g + geom_bar()
plot(g)

g <- ggplot(dat, aes(x = Rating))
g <- g + geom_histogram()
plot(g)

# Ratingを三段階に変換しRating2に
rate = c()
for(i in dat$Rating){
  if(i >= 8){
    rate = append(rate,"satisfied")
  }else if(i >= 6){
    rate = append(rate,"well")
  }else{
    rate = append(rate,"unsatisfied")
  }
}
dat <- transform(dat,Rating2 = rate)
head(dat)

# Rating2を数値に変換
rate = c()
for(i in dat$Rating){
  if(i >= 8){
    rate = append(rate,0)
  }else if(i >= 6){
    rate = append(rate,1)
  }else{
    rate = append(rate,2)
  }
}
dat <- transform(dat,Rating3 = rate)

time1 = as.character(dat$Time)
new_time = c()
for(i in time1){
  minute = substring(i,1,2)
  minute = as.numeric(minute)
  second = substring(i,4,5)
  second = as.numeric(second)
  new_time <- append(new_time,minute*60 + second)
}
dat <- transform(dat,Time2 = new_time)
head(dat)
attach(dat)

# 標準化
z_income1 = c()
for(i in gross.income){
  z <- (i-mean(gross.income))/sd(gross.income)
  z_income1<- append(z_income1,z)
}
z_income = scale(dat$gross.income)
z_time = scale(dat$Time2)
z_unitp = scale(dat$Unit.price)
z_quan = scale(dat$Quantity)
dat <- transform(dat,z_income = z_income)
dat <- transform(dat,z_time = z_time)
dat <- transform(dat,z_unitp = z_unitp)
dat <- transform(dat,z_quan = z_quan)
head(dat)

# plot
ggplot(dat,aes(x = z_income,y = z_time,color=factor(Rating2),group=Rating2))+geom_point()
ggplot(dat,aes(x = z_income,y = z_quan,color=factor(Rating2),group=Rating2))+geom_point()
ggplot(dat,aes(x = z_income,y = factor(Customer.type),color=factor(Rating2),group=Rating2))
        +geom_point()


# plot（レートに関するもの）
ggplot(dat,aes(x = factor(Customer.type),color=factor(Rating2),group=Rating2))+geom_bar()
ggplot(dat,aes(x = factor(Gender),color=factor(Rating2),group=Rating2))+geom_bar()
ggplot(dat,aes(x = factor(City),color=factor(Rating2),group=Rating2))+geom_bar()

# 学習データと検証データに分ける
library(dplyr) # データ操作一般
library(assertr) # データのチェック
library(rsample)
df_split <- initial_split(dat, prop = 0.8)
dat.train <- training(df_split)
dat.test <- testing(df_split)

# 満足度を説明する線形モデルを考える
head(dat)
res.lm <- lm(Rating ~ factor(City)+factor(Customer.type)+factor(Gender)+factor(Product.line)+Unit.price+Quantity+Time2
             +factor(Payment)+gross.income,data = dat)
summary(res.lm)
best.lm <- step(res.lm)
summary(best.lm)

# 満足度を説明する分類モデルを考える
# 線形モデルでの分類
res.lm <- lm(Rating3 ~ City+Customer.type+Gender+Product.line+Unit.price+Quantity+Time2+Payment
             +gross.income,data = dat.train)
summary(res.lm)
best.lm <- step(res.lm)
summary(best.lm)
lmp <- predict(best.lm,dat.test)
lmp.tab = table(lmp, dat.test$Rating3)
confusionMatrix(lmp.tab, mode="prec_recall") 

# 線形判別での分類
library(MASS)
library(caret)
dat.lda <- lda(Rating2 ~ City+Customer.type+Gender+Product.line+Unit.price+Quantity+Time2+Payment
                +gross.income,data=dat.train) 
dat.lda
lda.tab <- table(predict(dat.lda, dat.test)$class, dat.test[,"Rating2"])
confusionMatrix(lda.tab, mode="prec_recall")

# 決定木
library(rpart)
library(rpart.plot)
dt = rpart(Rating2 ~ City+Customer.type+Gender+Product.line+Unit.price+Quantity+Time2+Payment
           +gross.income,data=dat.train)
rpart.plot(dt)
dt_pre = predict(dt,dat.test,type = "class")
dp.tab = table(dt_pre,dat.test$Rating2)
confusionMatrix(dp.tab, mode="prec_recall")

# ランダムフォレストでの分類
library(randomForest)
rf <- randomForest(factor(Rating2) ~ City+Customer.type+Gender+Product.line+Unit.price+Quantity+Time2+Payment
                   +gross.income,data=dat.train)
rfp <- predict(rf,dat.test,type = "class")
rfp.tab = table(rfp, dat.test$Rating2)
confusionMatrix(rfp.tab, mode="prec_recall") 
varImpPlot(rf)
# SVMでの分類
library(e1071)
d.svm <- svm(factor(Rating2) ~ City+Customer.type+Gender+Product.line+Unit.price+Quantity+Time2+Payment
             +gross.income,data=dat.train)
d.svm
sp <- predict(d.svm,dat.test)
sp.tab = table(sp, dat.test$Rating2)
confusionMatrix(sp.tab, mode="prec_recall") 




