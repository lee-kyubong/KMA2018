# 전력량은 15년도 + 16년도 일부를 randint로 학습하고, 30%로 검정.
# > load("C:/Users/Kyubong/Desktop/weather-analysis/codes/until_nk_prediction_using_sk_model_MASTER_0721_02.RData")

#sk analysis
getwd()
dev.off()
rm(list = ls())
memory.size()
setwd("C:/Users/Kyubong/Desktop/weather-analysis/codes")
dim(data_sk_valid_point)
summary(data_sk_valid_point)
################################################
################# 0724 #######################
############## test_sk_final ##################
summary(test_sk_final)
library(dplyr)
point_ea <- test_sk_final %>% group_by(point) %>% tally()
dim(point_ea)


# > summary(data_sk_valid_point)

# point               date          mean_temp         min_temp          max_temp     
# Min.   : 93.0   2016-10-01:    42   Min.   :-18.40   Min.   :-26.800   Min.   :-14.00  
# 1st Qu.:129.0   2016-10-02:    42   1st Qu.:  5.20   1st Qu.:  0.600   1st Qu.: 10.20  
# Median :159.0   2016-10-03:    42   Median : 14.50   Median :  9.700   Median : 19.90  
# Mean   :174.4   2016-10-04:    42   Mean   : 13.41   Mean   :  9.126   Mean   : 18.39  
# 3rd Qu.:252.0   2016-10-05:    42   3rd Qu.: 21.70   3rd Qu.: 18.000   3rd Qu.: 26.60  
# Max.   :283.0   2016-10-06:    42   Max.   : 33.50   Max.   : 30.900   Max.   : 39.70  
#                 (Other)   :139117   NA's   :100      NA's   :11        NA's   :10     


# sum_rainfall       mean_wind         mean_dew      mean_humidity    mean_ground_hpa 
# Min.   :  0.000   Min.   : 0.000   Min.   :-30.00   Min.   :  8.50   Min.   : 900.6  
# 1st Qu.:  0.000   1st Qu.: 1.400   1st Qu.: -2.30   1st Qu.: 56.80   1st Qu.:1000.7  
# Median :  0.000   Median : 2.000   Median :  7.60   Median : 69.40   Median :1008.0  
# Mean   :  3.457   Mean   : 2.427   Mean   :  6.78   Mean   : 67.82   Mean   :1005.6  
# 3rd Qu.:  0.500   3rd Qu.: 3.000   3rd Qu.: 16.70   3rd Qu.: 80.10   3rd Qu.:1015.0  
# Max.   :357.500   Max.   :21.800   Max.   : 28.50   Max.   :100.00   Max.   :1035.5  
#                   NA's   :51       NA's   :177      NA's   :187      NA's   :129  


# mean_ocean_hpa   solar_duration    solar_amount   mean_cloud_amount    ml_cloud    
# Min.   : 985.1   Min.   : 0.000   Min.   : 0.00   Min.   : 0.00     Min.   : 0.00  
# 1st Qu.:1010.1   1st Qu.: 2.300   1st Qu.: 8.24   1st Qu.: 2.50     1st Qu.: 0.90  
# Median :1016.3   Median : 6.900   Median :13.02   Median : 5.30     Median : 3.10  
# Mean   :1016.2   Mean   : 6.109   Mean   :13.73   Mean   : 5.18     Mean   : 3.31  
# 3rd Qu.:1022.3   3rd Qu.: 9.400   3rd Qu.:19.19   3rd Qu.: 7.90     3rd Qu.: 5.50  
# Max.   :1038.7   Max.   :14.300   Max.   :42.86   Max.   :10.00     Max.   :10.00  
# NA's   :131      NA's   :123      NA's   :9581    NA's   :51763     NA's   :52455  

#61140

#  rain_factor
#  0:86742    
#  1:52627    


# 일사량은 장비교체 후 일정날짜 이후 수집이 된 경우가 몇 지점 존재


library(dplyr)
library(ggplot2)

natest_sk_valid_point <- data_sk_valid_point %>%
  group_by(point) %>%
  summarise(mean_temp1= sum(is.na(mean_temp)), mean_dew1= sum(is.na(mean_dew)),
            mean_ground_hpa1= sum(is.na(mean_ground_hpa)), mean_wind1= sum(is.na(mean_wind)),
            #sum_rainfall1= sum(is.na(sum_rainfall)), TOTAL_EA = length(point), ZERO_rainfall = sum(sum_rainfall == 0, na.rm = T),
    
            # 0718 강수량 문제는 해결
            mean_cloud_amount1= sum(is.na(mean_cloud_amount)), TOTAL_EA = length(point),
            solar_amount1 = sum(is.na(solar_amount)),
            solar_duration1 = sum(is.na(solar_duration)),
            max_temp1= sum(is.na(max_temp)), mean_humidity1= sum(is.na(mean_humidity)),
            mean_ocean_hpa1= sum(is.na(mean_ocean_hpa)), ml_cloud1= sum(is.na(ml_cloud)),
            min_temp1= sum(is.na(min_temp)))

table(data_sk_valid_point$rain_factor, is.na(data_sk_valid_point$mean_cloud_amount))
# FALSE  TRUE
# 0 53713 33029
# 1 33893 18734

table(data_sk_valid_point$point, is.na(data_sk_valid_point$mean_cloud_amount))
#
#####
#### 일사량, 전운량의 결측치 포함행 모두 제거: data_sk_valid_naomit
####

idx1 <- !is.na(data_sk_valid_point$solar_amount)
idx2 <- !is.na(data_sk_valid_point$mean_cloud_amount)

data_sk_valid_naomit <- data_sk_valid_point[idx1 & idx2, ]





summary(data_sk_valid_naomit)
memory.size()
# point             date         mean_temp         min_temp         max_temp     
# Min.   : 93   2008-07-30:   23   Min.   :-18.30   Min.   :-26.80   Min.   :-14.00  
# 1st Qu.:114   2008-07-31:   23   1st Qu.:  5.10   1st Qu.:  0.70   1st Qu.:  9.70  
# Median :136   2008-10-28:   23   Median : 14.40   Median :  9.70   Median : 19.50  
# Mean   :141   2008-10-29:   23   Mean   : 13.26   Mean   :  9.12   Mean   : 18.04  
# 3rd Qu.:165   2008-10-30:   23   3rd Qu.: 21.80   3rd Qu.: 18.10   3rd Qu.: 26.40  
# Max.   :251   2008-10-31:   23   Max.   : 33.50   Max.   : 29.40   Max.   : 39.30  
#               (Other)   :78091   NA's   :4        NA's   :5        NA's   :2       


# sum_rainfall       mean_wind         mean_dew       mean_humidity   mean_ground_hpa 
# Min.   :  0.000   Min.   : 0.000   Min.   :-27.200   Min.   : 11.3   Min.   : 905.6  
# 1st Qu.:  0.000   1st Qu.: 1.400   1st Qu.: -2.200   1st Qu.: 56.9   1st Qu.:1001.3  
# Median :  0.000   Median : 2.000   Median :  7.400   Median : 69.1   Median :1008.0  
# Mean   :  3.399   Mean   : 2.529   Mean   :  6.702   Mean   : 68.0   Mean   :1005.4  
# 3rd Qu.:  0.500   3rd Qu.: 3.000   3rd Qu.: 16.600   3rd Qu.: 80.3   3rd Qu.:1014.7  
# Max.   :318.000   Max.   :21.800   Max.   : 28.500   Max.   :100.0   Max.   :1034.8  
#                       NA's   :16       NA's   :6         NA's   :12      NA's   :8      


# mean_ocean_hpa   solar_duration    solar_amount   mean_cloud_amount    ml_cloud     
# Min.   : 985.1   Min.   : 0.000   Min.   : 0.00   Min.   : 0.000    Min.   : 0.000  
# 1st Qu.:1010.0   1st Qu.: 2.200   1st Qu.: 8.10   1st Qu.: 2.500    1st Qu.: 0.900  
# Median :1016.4   Median : 6.900   Median :12.89   Median : 5.300    Median : 3.100  
# Mean   :1016.2   Mean   : 6.089   Mean   :13.61   Mean   : 5.156    Mean   : 3.296  
# 3rd Qu.:1022.4   3rd Qu.: 9.400   3rd Qu.:19.09   3rd Qu.: 7.900    3rd Qu.: 5.400  
# Max.   :1038.7   Max.   :14.300   Max.   :33.16   Max.   :10.000    Max.   :10.000  
# NA's   :8        NA's   :19                                         NA's   :695 


#  rain_factor
#  0:47866    
#  1:30363    


###############################
#### 주요변수로 분석 진행 ####
#############################


hist(log(sqrt(data_sk_valid_naomit$mean_cloud_amount)))
qqnorm(data_sk_valid_naomit$mean_cloud_amount)
hist(log(na.omit(data_sk_valid_naomit$sum_rainfall)))
hist(log10(data_sk_valid_naomit$sum_rainfall + 1))
hist(sqrt(na.omit(data_sk_valid_naomit$mean_cloud_amount)))


# install.packages('nortest')
# library(nortest)
# ad.test(log(na.omit(data_sk_valid_naomit$mean_cloud_amount)))

summary(sqrt(na.omit(data_sk_valid_naomit$sum_rainfall)))
 
colnames(data_sk_valid_naomit)
cor(data_sk_valid_naomit[, c(3, 6, 7, 9, 13, 14)], use = 'pairwise.complete.obs')
plot(sample_n(data_sk_valid_naomit[, c(3, 6, 7, 9, 13, 14)], 10000))
#                    mean_temp  sum_rainfall   mean_wind mean_humidity solar_amount
# mean_temp          1.0000000   0.15013514 -0.10187324    0.36328917   0.38357837
# sum_rainfall       0.1501351   1.00000000  0.03664286    0.31875147  -0.28117125
# mean_wind         -0.1018732   0.03664286  1.00000000   -0.04436736  -0.05405113
# mean_humidity      0.3632892   0.31875147 -0.04436736    1.00000000  -0.31626063
# solar_amount       0.3835784  -0.28117125 -0.05405113   -0.31626063   1.00000000
# mean_cloud_amount  0.2606357   0.33299205  0.06873617    0.58827812***-0.54655737***


#                      mean_cloud_amount
# mean_temp                0.26063570
# sum_rainfall             0.33299205
# mean_wind                0.06873617
# mean_humidity            0.58827812
# solar_amount            -0.54655737
# mean_cloud_amount        1.00000000


###########################
#### rmse 함수 만들기 ####
#########################

rmse <- function(yi, yhat_i){
  sqrt(mean((yi - yhat_i)^2))
  
}

#################################
#### tr, val, test set 분할 ####
###############################

# library(dplyr)
# set.seed(726)
# 
# n <- nrow(data_sk_valid_naomit)
# idx <- 1 : n
# tr_idx <- sample(idx, n * .60)
# idx <- setdiff(idx, tr_idx)
# val_idx <- sample(idx, n * .20)
# test_idx <- setdiff(idx, val_idx)
# 
# tr_set <- data_sk_valid_naomit[tr_idx, ]
# val_set <- data_sk_valid_naomit[val_idx, ]
# test_set <- data_sk_valid_naomit[test_idx, ]


##########################################################################
#### 주요변수 c(3, 6, 7, 9, 13, 14, +15(rain_factor)) 모두 활용한 lm #### 
########################################################################
colnames(data_sk_valid_naomit)
lm_full_fit <- lm(solar_amount ~ mean_temp + sum_rainfall + mean_wind + mean_humidity +
                    mean_cloud_amount + rain_factor, data = tr_set)
summary(lm_full_fit)
par(mfrow = c(1, 2))
plot(lm_full_fit)

###########################
#### 강수량 로그 변환 ####
#########################
cor(tr_set$solar_amount, log(tr_set$sum_rainfall + 1), use = 'pairwise.complete.obs')
cor(tr_set$solar_amount, tr_set$sum_rainfall, use = 'pairwise.complete.obs')
plot(tr_set$solar_amount, tr_set$sum_rainfall)
plot(tr_set$solar_amount, log(tr_set$sum_rainfall + 1))  


hist(sqrt(tr_set$mean_cloud_amount + 1))
cor(tr_set$solar_amount, tr_set$mean_cloud_amount)

plot(tr_set$solar_amount, tr_set$mean_cloud_amount)



library(ggplot2)
tr_set %>%
  ggplot(aes(mean_cloud_amount, fill = rain_factor)) +
  geom_density(alpha = .5)

tr_set %>%
  ggplot(aes(solar_amount, fill = rain_factor)) +
  geom_bar()

tr_set %>%
  ggplot(aes(solar_amount, fill = rain_factor)) +
  scale_fill_manual( values = c("yellow","grey")) +
  geom_density(alpha = .5)

summary(data_sk_valid_naomit[, c(3, 6, 7, 9, 13, 14, 16)])

# > summary(data_sk_valid_naomit[, c(3, 6, 7, 9, 13, 14, 16)])
# mean_temp       sum_rainfall       mean_wind      mean_humidity    solar_amount   mean_cloud_amount
# Min.   :-18.30   Min.   :  0.000   Min.   : 0.000   Min.   : 11.3   Min.   : 0.00   Min.   : 0.000   
# 1st Qu.:  5.10   1st Qu.:  0.000   1st Qu.: 1.400   1st Qu.: 56.9   1st Qu.: 8.10   1st Qu.: 2.500   
# Median : 14.40   Median :  0.000   Median : 2.000   Median : 69.1   Median :12.89   Median : 5.300   
# Mean   : 13.26   Mean   :  3.399   Mean   : 2.529   Mean   : 68.0   Mean   :13.61   Mean   : 5.156   
# 3rd Qu.: 21.80   3rd Qu.:  0.500   3rd Qu.: 3.000   3rd Qu.: 80.3   3rd Qu.:19.09   3rd Qu.: 7.900   
# Max.   : 33.50   Max.   :318.000   Max.   :21.800   Max.   :100.0   Max.   :33.16   Max.   :10.000   
# NA's   :4                          NA's   :16       NA's   :12                                       

#  rain_factor
#  0:47866    
#  1:30363


##########################################################################
#### 주요변수 c(3, 6, 7, 9, 13, 14, +16(rain_factor)) 모두 활용한 lm #### 
########################################################################

##########################################################################
#### 이를 위해, 주요변수들만 택하고 결측치 제거한 master table1 준비 ####
########################################################################

data_sk_valid_var <- data_sk_valid_naomit[, c(3, 6, 7, 9, 14, 16, 13)]
head(data_sk_valid_var)

summary(data_sk_valid_var)

idx1 <- !is.na(data_sk_valid_var$mean_temp)
idx2 <- !is.na(data_sk_valid_var$mean_wind)
idx3 <- !is.na(data_sk_valid_var$mean_humidity)

data_sk_valid_var_naomit <- data_sk_valid_var[idx1 & idx2 & idx3, ]
dim(data_sk_valid_var_naomit) #78200 
dim(data_sk_valid_var) #78229

summary(data_sk_valid_var_naomit)
# > summary(data_sk_valid_var_naomit)
# mean_temp       sum_rainfall       mean_wind      mean_humidity  
# Min.   :-18.30   Min.   :  0.000   Min.   : 0.000   Min.   : 11.3  
# 1st Qu.:  5.10   1st Qu.:  0.000   1st Qu.: 1.400   1st Qu.: 56.9  
# Median : 14.40   Median :  0.000   Median : 2.000   Median : 69.1  
# Mean   : 13.26   Mean   :  3.398   Mean   : 2.529   Mean   : 68.0  
# 3rd Qu.: 21.80   3rd Qu.:  0.500   3rd Qu.: 3.000   3rd Qu.: 80.3  
# Max.   : 33.50   Max.   :318.000   Max.   :21.800   Max.   :100.0

# mean_cloud_amount rain_factor  solar_amount  
# Min.   : 0.000    0:47851     Min.   : 0.00  
# 1st Qu.: 2.500    1:30349     1st Qu.: 8.10  
# Median : 5.300                Median :12.89  
# Mean   : 5.156                Mean   :13.61  
# 3rd Qu.: 7.900                3rd Qu.:19.09  
# Max.   :10.000                Max.   :33.16  



################################## 0724
#data_sk_valid_var_namoit 서 index 제거
colnames(data_sk_valid_var_naomit)
data_sk_valid_var_naomit <- data_sk_valid_var_naomit[, 1:7]

summary(data_sk_valid_var_naomit)
cor(data_sk_valid_var_naomit[, c(1, 2, 3, 4, 5, 7)])


set.seed(726)

n <- nrow(data_sk_valid_var_naomit)
idx <- 1 : n
tr_idx <- sample(idx, n * .70) #0724 
idx <- setdiff(idx, tr_idx)
val_idx <- sample(idx, n * .30)
#test_idx <- setdiff(idx, val_idx)

tr_set <- data_sk_valid_var_naomit[tr_idx, ]
val_set <- data_sk_valid_var_naomit[val_idx, ]
#test_set <- data_sk_valid_var_naomit[test_idx, ]
#rm(test_set)
#
summary(tr_set)
#colnames(data_sk_valid_naomit)
lm_full_fit <- lm(solar_amount ~ mean_temp + sum_rainfall + mean_wind + mean_humidity +
                    mean_cloud_amount + rain_factor, data = tr_set)

summary(lm_full_fit)
par(mfrow = c(2, 2))
plot(lm_full_fit)

summary(lm(solar_amount ~ mean_temp + sum_rainfall + mean_wind + mean_humidity +
             mean_cloud_amount + factor(rain_factor), data = tr_set))

summary(lm_full_fit)
memory.size()
# after lm diagnosis, let's see outlier rows

lm_full_fit_2 <- lm(solar_amount ~., data = tr_set)
summary(lm_full_fit_2)
par(mfrow = c(2, 2))
plot(lm_full_fit_2)

y_obs <- val_set$solar_amount
yhat_lm <- predict(lm_full_fit_2, newdata = val_set)

rmse(y_obs, yhat_lm)
# > rmse(y_obs, yhat_lm)
# [1] 4.221481

#0724
# > rmse(y_obs, yhat_lm)
# [1] 4.215511


# write.csv(tr_set, 'diagnosis.csv')
# install.packages(c('glmnet', 'randomForest', 'gbm'))

levels(tr_set$rain_factor)
# [1] "0" "1"
tr_set$rain_factor[1:5]
# [1] 0 0 1 0 1
# Levels: 0 1
# test <- test_set
# x <- model.matrix( ~ rain_factor, test)
# glimpse(x)
# colnames(x)
# View(x)
# 
# 
# head(x)
# head(test_set$rain_factor)


library(glmnet)


############################
#### BASELINE (mean(y)) ####
############################

####################
#### GLM ####
############


lasso_x <- model.matrix(solar_amount ~ .-1, data_sk_valid_var_naomit)
dim(lasso_x) #[1] 15640     7
# summary(lasso_x)
# head(lasso_x)
class(lasso_x)

x <- lasso_x[tr_idx, ]
y <- tr_set$solar_amount

data_cvfit <- cv.glmnet(x, y)

par(mfrow = c(1, 1))
plot(data_cvfit)
 
coef(data_cvfit, s = c('lambda.1se'))
coef(data_cvfit, s = c('lambda.min'))

yhat_glmnet <- predict(data_cvfit, s = 'lambda.min', newx = lasso_x[val_idx, ])
yhat_glmnet <- yhat_glmnet[, 1]

rmse(y_obs, yhat_glmnet)
# > rmse(y_obs, yhat_glmnet)
# [1] 4.215985

plot_glmnet(lasso_fit)

lasso_fit
#여기서 나오는 DF: 변수의 개수

coef(lasso_fit, s = c(3.512, 2.206))
# > coef(lasso_fit, s = c(3.512, 2.206))
# 8 x 2 sparse Matrix of class "dgCMatrix"
# 1          2
# (Intercept)       14.149683 15.3561520
# mean_temp          .         0.1045407
# sum_rainfall       .         .        
# mean_wind          .         .        
# mean_humidity      .         .        
# mean_cloud_amount -0.109496 -0.6108912
# rain_factor0       .         .        
# rain_factor1       .         . 
##########################################################################
#### 라쏘를 통해서는, 전운량과 평균기온이 가장 주요한 변수로 나왔다! ####
########################################################################

lasso_cv <- cv.glmnet(lasso_x, lasso_y)
plot(lasso_cv)

log(lasso_cv$lambda.min) # 교차검증 오차의 평균값을 최소화(왼쪽 점선)한 [최적 예측력]
log(lasso_cv$lambda.1se) # 교차검증 오차의 평균값이 최소값으로부터 1 - 표준편차
# 이상 떨어지지 않은 가장 간단하(오른쪽 점선) [해석 가능한 모형을 위해]

coef(lasso_cv, s = lasso_cv$lambda.min)

# > coef(lasso_cv, s = lasso_cv$lambda.min)
# 8 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)        1.723165e+01
# mean_temp          4.291981e-01
# sum_rainfall      -6.832858e-02
# mean_wind          1.953754e-01
# mean_humidity     -6.179820e-02
# mean_cloud_amount -1.188920e+00
# rain_factor0       1.209617e+00
# rain_factor1      -6.758074e-11


# lasso_x_val <- model.matrix(solar_amount ~.-1, data = val_set)
# yhat_lasso <- predict(lasso_cv, s = 'lambda.min', newx = lasso_x_val)
# yhat_lasso <- yhat_lasso[, 1]
# rmse(y_obs, yhat_lasso)

###############
#### Tree ####
#############
library(rpart)
tree_fit <- rpart(solar_amount ~., data = tr_set)
summary(tree_fit)


plot(tree_fit)
text(tree_fit, use.n = T)
par(mfrow = c(1, 1), xpd = NA)
yhat_tree <- predict(tree_fit, val_set)
rmse(y_obs, yhat_tree)


############
#### RF ####
############
set.seed(726)
library(randomForest)
rf_fit <- randomForest(solar_amount ~., tr_set, do.trace = 50)
par(mfrow = c(1, 2))
plot(rf_fit)
varImpPlot(rf_fit)
#16:30
#16:59
yhat_rf <- predict(rf_fit, newdata = val_set)
rmse(y_obs, yhat_rf)
# > rmse(y_obs, yhat_rf)
# [1] 3.918732
# > rmse(y_obs, yhat_lm)
# [1] 4.221481

# > rmse(y_obs, yhat_rf)
# [1] 3.899719

############
#### GBM #### 
############
memory.size()
library(gbm)
set.seed(726)
gbm_fit <- gbm(solar_amount ~., data = tr_set,
               n.trees = 60000, cv.folds = 3, verbose = T)
memory.size()
par(mfrow = c(1, 1))
(best_iter = gbm.perf(gbm_fit, method = 'cv'))


yhat_gbm <- predict(gbm_fit, n.tree = best_iter, newdata = val_set)
rmse(y_obs, yhat_gbm)

rmse(y_obs, yhat_gbm)
# [1] 4.039775

rmse(y_obs, yhat_rf)
# [1] 3.918732
rmse(y_obs, yhat_lm)
# [1] 4.221481




###########################################
##########################################
#### 우리나라 지점별 파악위해 ###########
#### 마스터 테이블에 지점과 날짜 맵핑 ####
#########################################
########################################

summary(data_sk_valid_naomit)
test_sk <- data_sk_valid_naomit
test_sk$index <- rownames(test_sk)

colnames(test_sk)
test_sk_origin <- test_sk[, c(17, 1, 2)]

#data_sk_valid_var_naomit이 우리나라의 마스터 테이블
data_sk_valid_var_naomit$index <- rownames(data_sk_valid_var_naomit)
test_sk_final <- merge(data_sk_valid_var_naomit, test_sk_origin)
summary(test_sk_final)

write.csv(test_sk_final, 'sk_master_table_with_datepoint.csv')


test_sk_final$date <- as.Date(test_sk_final$date)

summary(test_sk_final)
dim(test_sk_final)

# > memory.size()
# [1] 864.53
#
# point17_sk <- as.data.frame(test_sk_final %>% group_by(point) %>%
#                            filter(date > '2016-12-31') %>% summarise(SOLAR = sum(solar_amount)))
# 
# point_165_sk <- test_sk_final %>% filter(point == 119) %>%
#   filter(date > '2014-12-31' & date < '2016-01-01') 
# write.csv(point_ys_168, 'point_ys_168.csv')
# 
# 
# point_ys_168 <- test_sk_final %>% filter(point == 168) %>%
#   filter(date > '2016-12-31' & date < '2018-01-01')
