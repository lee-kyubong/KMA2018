getwd()
dev.off()
rm(list = ls())
memory.size()
setwd("C:/Users/Kyubong/Desktop/weather-analysis/codes")
#우리나라로 랜포 리모델링한거 불러오ㄱ


data0_nk <- read.csv(file = '../data/north_10years_var.csv')
colnames(data0_nk)
colnames(data0_nk)[1] <- 'point'

summary(data0_nk)
#일단 강수량 NA 값에 따라 rain factor 변수 넣기
idx <- !is.na(data0_nk$sum_rainfall)
data0_nk$rain_factor <- '0' #비가 안왔다
data0_nk$rain_factor[idx] <- '1' #비가 왔다
head(data0_nk)
data0_nk$rain_factor <- as.factor(data0_nk$rain_factor)

#강수량 NA 값에 0 삽입
idx <- is.na(data0_nk$sum_rainfall)
data0_nk$sum_rainfall[idx] <- 0

summary(data0_nk)

data0_nk_valid_var <- data0_nk[, c(3, 7, 6, 10, 8, 14)]
summary(data0_nk_valid_var)
# > summary(data0_nk_valid_var)
# mean_temp        sum_rainfall       mean_wind      mean_humidity  mean_cloud_amount rain_factor
# Min.   :-30.700   Min.   :  0.000   Min.   : 0.000   Min.   : 3.0   Min.   : 0.000    0:66893    
# 1st Qu.: -0.300   1st Qu.:  0.000   1st Qu.: 0.400   1st Qu.:64.0   1st Qu.: 2.500    1:31428    
# Median : 10.400   Median :  0.000   Median : 1.000   Median :75.0   Median : 6.000               
# Mean   :  8.879   Mean   :  2.742   Mean   : 1.386   Mean   :72.8   Mean   : 5.603               
# 3rd Qu.: 19.100   3rd Qu.:  0.000   3rd Qu.: 1.900   3rd Qu.:83.0   3rd Qu.: 8.800               
# Max.   : 31.900   Max.   :440.000   Max.   :20.800   Max.   :99.0   Max.   :10.000               
# NA's   :185                         NA's   :189      NA's   :356    NA's   :1539 

dim(data0_nk_valid_var)
# > dim(data0_nk_valid_var)
# [1] 98321     6

idx <- !is.na(data0_nk_valid_var$mean_cloud_amount)
data0_nk_valid_omit <- data0_nk_valid_var[idx, ]
summary(data0_nk_valid_omit)
dim(data0_nk_valid_omit)
# > dim(data0_nk_valid_omit)
# [1] 96782     6

idx <- !is.na(data0_nk_valid_omit$mean_humidity)
data0_nk_valid_omit <- data0_nk_valid_omit[idx, ]
summary(data0_nk_valid_omit)
dim(data0_nk_valid_omit)

########## RF 모델로 예측 ##############
library(dplyr)
library(ggplot2)
library(randomForest)
yhat_rf_nk <- predict(rf_fit, newdata = data0_nk_valid_omit)
summary(yhat_rf_nk)
summary(data_sk_valid_var_naomit$solar_amount)

data0_nk_predicted <- data0_nk_valid_omit
summary(data0_nk_predicted)
data0_nk_predicted$solar_amount <- yhat_rf_nk
head(data0_nk_predicted)
head(data_sk_valid_var_naomit)
write.csv(data0_nk_predicted, 'nk_solar_predicted_remodeling_0724.csv')


summary(data0_nk_predicted)

#날짜와 지점 결합 
memory.size()
test <- data0_nk_predicted
test$index <- rownames(test)


head(data0_nk)
test_origin <- data0_nk
test_origin$index <- rownames(test_origin)
colnames(test_origin)
test_origin <- test_origin[, c(15, 1, 2)]
test_final <- merge(test, test_origin)
write.csv(test_final, 'nk_solar_predicted_remodeling_with_datapoint_0724.csv')

summary(test_final)


##########################################
##########################################
####### 북한지점별 일사량 ################
##########################################
##########################################


test_final$date <- as.Date(test_final$date, format = '%y-%m-%d')
?as.Date
summary(test_final)
dim(test_final)

point17 <- as.data.frame(test_final %>% group_by(point) %>%
  filter(date > '2016-12-31') %>% summarise(SOLAR = sum(solar_amount)))
#write.csv(point17_half, 'point17_half_remodeling.csv') 


# 농촌진흥청 보고서에서 북한 1 ~ 6월 일사량이 2,800 대
point17_half <- test_final %>% group_by(point) %>%
  filter(date > '2014-12-31' & date < '2015-07-01') %>% summarise(SOLAR = sum(solar_amount))

summary(point17_half$SOLAR)



########################################
############우리나라 북한 EDA ##########
########################################

summary(data_sk_valid_var_naomit)
cor(data_sk_valid_var_naomit[, c(1, 2, 3, 4, 5, 7)])

cor(data0_nk_predicted[, c(1, 2, 3, 4, 5, 7)])

test_0725 <- data0_nk_predicted %>% group_by(point) %>%
  filter(date > '2014-12-31' & date < '2015-07-01') %>%
  summary(ss = sum(solar_amount))

test_0725_sk <- test_sk_final %>% group_by(point) %>%
  tally()

sum(test_0725_sk$n) #[1] 78200

library(ggplot2)
library(dplyr)


tr_set %>%
  ggplot(aes(mean_cloud_amount, fill = rain_factor)) +
  geom_density(alpha = .5)

tr_set %>%
  ggplot(aes(solar_amount, fill = rain_factor)) +
  scale_fill_manual( values = c("yellow","grey")) +
  geom_density(alpha = .5)


tr_set %>%
  ggplot(aes(mean_cloud_amount, fill = rain_factor)) +
  scale_fill_manual( values = c("yellow","grey")) +
  geom_density(alpha = .5)


data0_nk_predicted %>%
  ggplot(aes(mean_cloud_amount, fill = rain_factor)) +
  scale_fill_manual( values = c("yellow","grey")) +
  geom_density(alpha = .5)


summary(gbm_fit)

summary(point17_half$SOLAR)
