#start
dev.off()
rm(list = ls())
setwd("C:/Users/Kyubong/Desktop/weather-analysis/codes")

memory.size()

data00 <- read.csv(file = '../sheets/sk_master_table_with_datepoint_0721_01.csv')
data00 <- data00[, 2:ncol(data00)]
data00$date <- as.Date(data00$date)

library(dplyr)
library(lubridate)

# yc <- data00 %>% filter(point == 168)
# yc <- yc %>% filter(date > '2016-12-31')
# yc_sum <- yc %>% group_by(month=floor_date(date, "month")) %>% summarise(sol = sum(solar_amount),
#                                                                          temp = mean(mean_temp))
# 
# yc_elec <- read.csv(file = '../sheets/yc_2017.csv')
# colnames(yc_elec)[1] <- 'p'
# yc_sum$energy <- yc_elec$energy
# plot(yc_sum[, 1:2])
# plot(yc_sum[, c(1, 3)])
# plot(yc_sum[, 2:3])
# cor(yc_sum$suum, yc_sum$energy)
# 
# lm_fit_yc <- lm(energy ~ sol + temp, data = yc_sum)
# summary(lm_fit_yc)
# par(mfrow = c(1, 2))
# plot(lm_fit_yc)

# 
# plot(yc_sum$sol, yc_sum$energy)
# plot(yc_sum$temp, yc_sum$energy)
# cor(yc_sum$temp, yc_sum$energy)
# 
# lm_yc_pred <- predict(lm_fit_yc, newdata = test)
# test <- yc_sum[, 2:3]
# 
# plot(lm_yc_pred)
# plot(yc_sum$energy)



########## 세종 133  // 15-16

sj_points <- data00 %>% filter(point == 133) # 10년치
rm(sj_16)
sj_15_w <- sj_points %>% filter(date > '2014-12-31' & date < '2016-01-01')

sj_15_w %>% group_by(month=floor_date(date, "month")) %>% tally() #5월 하루 결ㅊ


sj_16_w <- sj_points %>% filter(date > '2015-12-31' & date < '2017-01-01') #윤년


########### 세종의 출력량 가져오기 
sj_outputs <- read.csv(file = '../sheets/sj1516.csv')
colnames(sj_outputs)[1] <- 'year'
colnames(sj_outputs)
summary(sj_outputs)

?data.frame()

colnames(sj_outputs)




# library(lubridate) # for hms and hour functions
# sj_outputs$time <- hms(sj_outputs$time) 
# sj_outputs$hour <- factor(hour(sj_outputs$time))
# library(dplyr)
# test <- sj_outputs %>%
#   select(-time) %>% # dplyr doesn't like this column for some reason
#   group_by(month, day) %>%
#   summarise(size=sum(output))



sj_outputs$month <- as.character(sj_outputs$month)
sj_outputs$day <- as.character(sj_outputs$day)

length(sj_outputs$month[15612])
nchar('12')
# head(sj_outputs)
# paste0(substr(sj_outputs$year, 1, 4),'3')

for (i in 1:length(sj_outputs$month)){
  if (nchar(sj_outputs$month[i]) == 1){
    sj_outputs$month[i] <- paste0('0', sj_outputs$month[i])
    print(0)
  }
}

for (i in 1:length(sj_outputs$day)){
  if (nchar(sj_outputs$day[i]) == 1){
    sj_outputs$day[i] <- paste0('0', sj_outputs$day[i])
    print(0)
  }
}

length(sj_outputs$month[4])

sj_outputs$date <- 0
sj_outputs$date <- paste0(as.character(sj_outputs$year), sj_outputs$month, sj_outputs$day)
#sj_outputs$date <- as.integer(sj_outputs$date)
sj_outputs$date <- as.Date(sj_outputs$date, format = '%Y%m%d')
#write.csv(sj_outputs, 'sj_outputs_w_date.csv')

library(dplyr)
library(lubridate)

summary(sj_outputs)
class(sj_outputs$output)

library(dplyr)
library(lubridate)
sj_op_sum <- sj_outputs %>% group_by(month=floor_date(date, "day")) %>% summarise(sol = sum(output))
sj_op_sum_15 <- sj_op_sum[1:365, ]
sj_op_sum_16 <- sj_op_sum[366:nrow(sj_op_sum), ]

sj_outputs %>% filter(year == 2016) %>% group_by(month, day) %>% summarise(ssum = sum(output))

memory.size()
#write.csv(sj_outputs, 'sj_outputs1516.csv')




#k_master_table_with_datepoint_0721_01 과 날짜로 매핑
#이 마스터 테이블도 대전 133에 15년 16년 따로 떼내어 테이블 2개 만들기
colnames(data00)



colnames(sj_op_sum_15)[1] <- 'date'
colnames(sj_op_sum_15)[2] <- 'output'
colnames(sj_op_sum_15)
colnames(sj_op_sum_16)[1] <- 'date'
colnames(sj_op_sum_16)[2] <- 'output'
colnames(sj_op_sum_16)
sj_15_final <- merge(sj_15_w, sj_op_sum_15, all.x = T)
sj_15_final %>% group_by(month=floor_date(date, "month")) %>% tally()


sj_16_final <- merge(sj_16_w, sj_op_sum_16, all.x = T) #output이 NA omit과 0 제거 필요
#> save.image("C:/Users/Kyubong/Desktop/weather-analysis/codes/elec_weather_sj_0723.RData")
# write.csv(sj_15_final, 'sj_15_final.csv')
# write.csv(sj_16_final, 'sj_16_final.csv')

memory.size()

#회귀 랜포 부스팅으로 15년 모델 yhat(output)을 기상요소들로 학습
#16년 자료로 어느정도인지 검정