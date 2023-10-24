getwd()# 지금현재 데이터 

setwd("C:/Users/82102/Desktop/임데마/중간고사")# 데이터 가져오기

dir()# 디렉토리 확인

#csv 파일 불러오기 
h1<-read.csv("midterm1.csv")
h2<-read.csv("midterm2.csv")
h3<-read.csv("midterm3.csv")

install.packages("dplyr")
library(dplyr)

# 파일 m2로 합치기
m1<-left_join(h1,h2,by="T_ID") 
m2<-left_join(m1,h3)

# 빈도확인
install.packages("descr")
library(descr) # freq에필요한 패키지
freq(m2$T_HTN) # N=빈도수(퍼센트) 채우기

#결측확인
summary(m2$T_GLO0)
table(m2$T_INCOME) #9999만있어서 99999만 결측처리

#결측처리(일괄)
m2[m2 == 66666 | m2 == 77777 | m2 == 99999] <- NA


#AGE
shapiro.test(m2$T_AGE) # p-value = 0.0007784로 0.05보다 작게나옴 > 윌콕슨
hist(m2$T_AGE) 
m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_AGE, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_AGE, na.rm = T))
wilcox.test(T_AGE ~ T_HTN, data=m2) #  p-value = 0.03472

#AGE group
m2 <- m2 %>% mutate(age_gr3=ifelse(T_AGE < 60, 1, 
                                          ifelse(T_AGE >= 60, 2, NA))) 
table(m2$age_gr3)

library(descr) # freq에필요한 패키지
library(gmodels) # CrossTable에 필요한 패키지
CrossTable(m2$age_gr3, m2$T_HTN)
chisq.test(m2$age_gr3, m2$T_HTN) # X-squared = 1.0838 / p-value = 0.2978

#ICOME group
m2<-m2 %>% mutate(inc_gr3=ifelse(T_INCOME %in% c(1,2,3,4),1,
                                   ifelse(T_INCOME  %in% c(5,6,7,8), 2, NA)))
table(m2$inc_gr3)

CrossTable(m2$inc_gr3, m2$T_HTN)
chisq.test(m2$inc_gr3, m2$T_HTN) #X-squared = 2.4553/ p-value = 0.1171

#BMI
shapiro.test(m2$T_BMI)# p-value = 3.092e-05 로 0.05보다 작게나옴 
hist(m2$T_BMI) 
m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_BMI, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_BMI, na.rm = T))
wilcox.test(T_BMI ~ T_HTN, data=m2) # p-value = 3.333e-05

#BMI group
m2<-m2 %>% mutate(bmi_gr3=ifelse(T_BMI <23, 1,
                                 ifelse(T_BMI >= 23 & T_BMI < 25, 2,
                                        ifelse(T_BMI >= 25, 3, NA))))

table(m2$bmi_gr3)
CrossTable(m2$bmi_gr3, m2$T_HTN)
chisq.test(m2$bmi_gr3, m2$T_HTN) # X-squared = 13.216 / p-value = 0.00135

# Weight
shapiro.test(m2$T_WEIGHT) # p-value = 0.0005192 로 0.05보다 작게나옴 > 윌콕슨검사 
hist(m2$T_WEIGHT) 

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_WEIGHT, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_WEIGHT, na.rm = T))
wilcox.test(T_WEIGHT ~ T_HTN, data=m2) # p-value = 5.95e-05 

m2 <- m2 %>% mutate(weight_gr3=ifelse(T_WEIGHT < 90, 1, 
                                   ifelse(T_WEIGHT >= 90, 2, NA))) 
table(m2$weight_gr3)

CrossTable(m2$weight_gr3, m2$T_HTN)
chisq.test(m2$weight_gr3, m2$T_HTN) #X-squared = 1.2047 / p-value = 0.2724


#Exercise
CrossTable(m2$T_EXER, m2$T_HTN) 
chisq.test(m2$T_EXER, m2$T_HTN) # X-squared = 0.093051 / p-value = 0.7603

#Drink
m2<-m2 %>% mutate(drink_gr3=ifelse(T_DRINK == 2,1,
                               ifelse(T_DRINK %in% c(2,3), 2, NA)))
table(m2$drink_gr3)
CrossTable(m2$drink_gr3, m2$T_HTN)
chisq.test(m2$drink_gr3, m2$T_HTN) # X-squared = 5.19 / p-value = 0.02272


#SBP
shapiro.test(m2$T_SBP)# p-value = 0.1287
hist(m2$T_SBP) 
library(psych)
describeBy(m2$T_SBP, m2$T_HTN)
var.test(T_SBP ~ T_HTN, data = m2 ) # p-value = 0.8277
t.test(T_SBP ~ T_HTN, data = m2, var.equal=T ) # p-value = 0.0001655

#DBP
shapiro.test(m2$T_DBP)# p-value = 0.001137
hist(m2$T_DBP) 

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_DBP, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_DBP, na.rm = T))
wilcox.test(T_DBP ~ T_HTN, data=m2) # p-value = 0.00162 

#Pulse
shapiro.test(m2$T_PULSE) # p-value = 0.04517 
hist(m2$T_PULSE) 
m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_PULSE, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_PULSE, na.rm = T))
wilcox.test(T_PULSE ~ T_HTN, data=m2) # p-value = 0.3359

#Waist 
shapiro.test(m2$T_WAIST) #  p-value = 0.101
hist(m2$T_WAIST) 
describeBy(m2$T_WAIST, m2$T_HTN) # 자료요약 하면서 mean,sd 찾아서 표에 넣어라 
var.test(T_WAIST ~ T_HTN, data = m2 ) # p-value = 0.4621 라 0.05보다 커서 var.equal= T로 작성 
t.test(T_WAIST ~ T_HTN, data = m2, var.equal=T ) # p-value = 1.808e-06




#Hip
shapiro.test(m2$T_HIP) # p-value = 0.02564로 0.05보다 작게나옴 > 윌콕슨
hist(m2$T_HIP)

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_HIP, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_HIP, na.rm = T))
wilcox.test(T_HIP ~ T_HTN, data=m2) # p-value = 8.296e-06

#Glucose
shapiro.test(m2$T_GLU0) # p-value < 2.2e-16 로 0.05보다 작게나옴 > 윌콕슨
hist(m2$T_GLU0) 

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_GLU0, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_GLU0, na.rm = T))
wilcox.test(T_GLU0 ~ T_HTN, data=m2) #p-value = 0.07174


#Triglyceride
shapiro.test(m2$T_TG) # p-value < 2.2e-16 로 0.05보다 작게나옴 > 윌콕슨
hist(m2$T_TG) 

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_TG, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_TG, na.rm = T))
wilcox.test(T_TG ~ T_HTN, data=m2) # p-value = 0.2802

