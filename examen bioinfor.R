library (nycflights13)
library (weather)
View(weather)
tiempo<-weather
ncol(weather)
nrow (weather)
library(tidyverse)
media_speed = weather %>%
  group_by(month)%>%
  summarise(Media= mean(wind_speed, na.rm=T))
View (media_speed)
media_gust = weather %>%
  group_by(month)%>%
  summarise(Media= mean(wind_gust, na.rm=T))


filter (weather, month==1, wind_speed>0)
filter (weather, month==2, wind_speed>0)
filter (weather, month==3, wind_speed>0)
filter (weather, month==4, wind_speed>0)
filter (weather, month==5, wind_speed>0)
filter (weather, month==6, wind_speed>0)
filter (weather, month==7, wind_speed>0)
filter (weather, month==8, wind_speed>0)
filter (weather, month==9, wind_speed>0)
filter (weather, month==10, wind_speed>0)
filter (weather, month==11, wind_speed>0)
filter (weather, month==12, wind_speed>0)

filter (weather, month==1, wind_gust>0)
filter (weather, month==2, wind_gust>0)
filter (weather, month==3, wind_gust>0)
filter (weather, month==4, wind_gust>0)
filter (weather, month==5, wind_gust>0)
filter (weather, month==6, wind_gust>0)
filter (weather, month==7, wind_gust>0)
filter (weather, month==8, wind_gust>0)
filter (weather, month==9, wind_gust>0)
filter (weather, month==10, wind_gust>0)
filter (weather, month==11, wind_gust>0)
filter (weather, month==12, wind_gust>0)

boxplot(tiempo$temp[tiempo$origin=="EWR"]~tiempo$month[tiempo$origin=="EWR"], xlab="Months", ylab= "ºC", main="EWR", col="red")
boxplot(tiempo$temp[tiempo$origin=="LGA"]~tiempo$month[tiempo$origin=="LGA"], xlab="Months", ylab= "ºC", main="LGA", col="blue")
boxplot(tiempo$temp[tiempo$origin=="JFK"]~tiempo$month[tiempo$origin=="JFK"], xlab="Months", ylab= "ºC", main="JFK", col="green")

cor(weather$temp[weather$month==10 & weather$day==15 & weather$origin=="LGA"], weather$humid[weather$month==10 & weather$day==15 & weather$origin=="LGA"])
ggplot(data = weather) + 
  geom_point(mapping = aes(x = humid[month==10 & day==15 & origin=="LGA"], y = temp[month==10 & day==15 & origin=="LGA"]))
cumpLGA<-filter( weather, month==10, day==15, origin=="LGA")
ggplot(data = cumpLGA) + 
  geom_point(mapping = aes (x=humid, y=temp))
cor(weather$temp[weather$month==10 & weather$day==15 & weather$origin=="EWR"], weather$humid[weather$month==10 & weather$day==15 & weather$origin=="EWR"])
cumpEWR<-filter( weather, month==10, day==15, origin=="EWR")
ggplot(data = cumpEWR) + 
  geom_point(mapping = aes (x=humid, y=temp))
cor(weather$temp[weather$month==10 & weather$day==15 & weather$origin=="JFK"], weather$humid[weather$month==10 & weather$day==15 & weather$origin=="JFK"])
cumpJFK<-filter( weather, month==10, day==15, origin=="JFK")
ggplot(data = cumpJFK) + 
  geom_point(mapping = aes (x=humid, y=temp))
JFK<-filter(weather, origin=="JFK")
LGA<-filter(weather, origin=="LGA")
cor(tempJFK$temp, tempLGA$temp)
sum(cor(tempJFK$temp, tempLGA$temp))
t.test(tempJFK$temp, tempLGA$temp)
pvalue(t.test(tempJFK$temp, tempLGA$temp))
t.test(tempJFK$temp, tempLGA$temp)
boxplot (JFK$temp, LGA$temp, ylab= "ºF", main="Diferencia de temperatura", col="68", xlab="JFK                                   LGA")

plot_meteo <- function(dat, meteo, titulo, unidades)
{
  dat <- data.frame(dat)
  (x)<-weather$meteo
  boxplot((x)~tiempo$month, xlab="Months", ylab= (unidades), main=(titulo))
}
plot_meteo(weather,humid,"Humedad","Relative humidity")
  
sessionInfo()
