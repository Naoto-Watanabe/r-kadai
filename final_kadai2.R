

setwd("~/Dropbox/KBS/M1_3/business_statistics/final_kadai")

data1 = read.csv("2019_nCoV_data.csv")
data2 = read.csv("covid_19_data.csv")
data3 = read.csv("COVID19_line_list_data.csv")
data4 = read.csv("COVID19_open_line_list.csv")
data5 = read.csv("time_series_covid_19_confirmed.csv")
data6 = read.csv("time_series_covid_19_deaths.csv")
data7 = read.csv("time_series_covid_19_recovered.csv")

head(data4)
dat <- data4$ID
dat <- transform(dat,age = data4$age)
dat <- transform(dat,sex = data4$sex)
dat <- transform(dat,outcome = data4$outcome)
head(dat)

library(dplyr)
dat2 <- filter(dat,dat$age != "",dat$age != "N/A",dat$sex != "",dat$sex != "N/A")

d = c()
for(i in dat2$outcome){
  if(i == "death" || i == "died"){
    d = append(d,1)
  }else{
    d = append(d,0)
  }
}
dat2 <- transform(dat2, death = d)
age2 = c()
for(i in dat2$age){
  if(i >= 60){
    age2 = append(age2,"old")
  }else{
    age2 = append(age2,"young")
  }
}
dat2 <- transform(dat2, age2 = age2)
head(dat2)
group1 <- filter(dat2, dat2$age2=="young")
group1 <- group1$death
group2 <- filter(dat2, dat2$age2=="old")
group2<- group2$death

group3 <- filter(dat2, dat2$sex=="male")
group3 <- group3$death
group4 <- filter(dat2, dat2$sex=="female")
group4<- group4$death

t.test(group1,group2)
t.test(group3,group4)

# 時系列分析とヒートマップのためのデータ整形
head(data2)
mode(data2$ObservationDate)

time1 = as.character(data2$ObservationDate)
n = 0
num <- c()
pre = "俺は海賊王になる！！！"
for(i in time1){
  month = substring(i,2,2)
  day = substring(i,4,5)
  md = paste(month,day,sep = "")
  md = as.numeric(md)
  if(md == pre){
    num <- append(num,n)
  }else{
    pre = md
    n = n +1
    num <- append(num,n)
  }
}

data2 <- transform(data2,date = num)
data2

library(dplyr)
library(tidyr)

tdata <-  data2 %>%
    dplyr::group_by(ObservationDate) %>% #ObservationDate
    dplyr::summarise(Confirmed.Total = sum(Confirmed))
tdata <- as.data.frame(tdata)

t2data <-  data2 %>%
  dplyr::group_by(ObservationDate) %>% #ObservationDate
  dplyr::summarise(Deaths.Total = sum(Deaths))
t2data <- as.data.frame(t2data)

geodata <-  data2 %>%
  dplyr::group_by(Country.Region,ObservationDate) %>%
  dplyr::summarise(Confirmed.Total = sum(Confirmed))
geodata <- as.data.frame(geodata)
geodata <-  geodata %>%
  dplyr::group_by(Country.Region) %>%
  dplyr::summarise(Max.Total = max(Confirmed.Total))
geodata <- as.data.frame(geodata2)
geodata["Country.Region"] <- lapply(geodata["Country.Region"], gsub, pattern="Mainland China", replacement = "China")
geodata <- filter(geodata,geodata$Country.Region!="Colombia")


# コロナの感染状況を世界地図のヒートマップで（結果は画像ファイルがこのプログラムがあるフォルダに追加される）
library(rworldmap)
sVisit <- joinCountryData2Map(geodata, joinCode="NAME", nameJoinColumn="Country.Region")
png("world.png", width=960, height=540)
par(family="ヒラギノ明朝 ProN W3")
#joinCountryData2Mapによって作成した国別のデータを描画し、色をつけます
mapCountryData(sVisit, nameColumnToPlot="Max.Total", catMethod="logFixedWidth", mapTitle = "コロナウイルス感染状況", addLegend = TRUE)
dev.off()

# 現在の状況
par(family="ヒラギノ明朝 ProN W3")
plot(tdata,type="l",main="コロナ感染者数")
t1 <- as.ts(tdata$Confirmed.Total)
t2 <- as.ts(t2data$Deaths.Total)
ts.plot(t1,t2,col=c(4,2),main="コロナ感染者数と死亡者数")
legend("topleft",legend=c("感染者数", "死亡者数"),col=c(4,2),lwd=c(1,1))
# 時系列分析（かなり発展的な内容で僕も教授から突っ込まれたら答えられなのでおまけ的なノリで）
tsdata <- as.ts(tdata$Confirmed.Total)
library(forecast)
nn_cor <- nnetar(tsdata, maxit=1000)
arima_cor <- auto.arima(tsdata, ic = "aic")
# nnet
f_nn_cor <- forecast(nn_cor, h = 7)$mean
# arima
f_arima_cor <- forecast(arima_cor, h=7)$mean
ts.plot(
  tsdata, 
  f_nn_cor,
  f_arima_cor,
  col=c(1,2,4),
  lwd = c(1,2,2),
  main="ARIMAとNNの予測"
  
)
legend(
  "topleft",
  legend=c("過去データ", "ニューラルネット","ARIMA"),
  col=c(1,2,4),
  lwd=c(1,2,2)
)





