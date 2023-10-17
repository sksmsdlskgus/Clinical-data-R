getwd()# 지금현재 데이터 

setwd("C:/Users/82102/Desktop/임데마/임데마 실습")# 데이터 가져오기

dir()# 디렉토리 확인

#csv 파일 불러오기 
h1<-read.csv("htnw1.csv")
h2<-read.csv("htnw2.csv")
h3<-read.csv("htnw3.csv")

install.packages("dplyr")
library(dplyr)

# 파일 m2로 합치기
m1<-left_join(h1,h2,by="T_ID") 
m2<-left_join(m1,h3)

# 빈도확인
install.packages("descr")
library(descr) # freq에필요한 패키지
freq(m2$T_HTN) # N=빈도수(퍼센트) 채우기

#성별확인
freq(m2$T_SEX)

#결측확인
summary(m2$T_GLO0)
table(m2$T_INCOME) #9999만있어서 99999만 결측처리

#결측처리(일괄)
m2[m2 == 99999] <- NA

########################## 필수 

# 정규성 검사 후 크면 t테스트 작으면 윌콕슨
# 자, 이제 검사 시작 


#AGE
#나이 정규분포 확인 (대상자가 많을떼 shapiro를써서 정규분포로 씀)
shapiro.test(m2$T_AGE) 
# p-value = 0.05205 로 0.05보다 크게나옴 > 정규분포
hist(m2$T_AGE) 
library(psych)
describeBy(m2$T_AGE, m2$T_HTN) # 자료요약 하면서 mean,sd 찾아서 표에 넣어라 

#나이 비교하기
var.test(T_AGE ~ T_HTN, data = m2 ) # p-value = 0.09973 라 0.05보다 커서 var.equal= T로 작성 
t.test(T_AGE ~ T_HTN, data = m2, var.equal=T )
#나이가 yes가 많으니 고혈압 증간여부에 따라서 나이의 평균 차이가 통계적으로 유의미하다
# p-value = 3.626e-07로 0.05보다 작으니 0.0001로 표에 작성

# 연령그룹 3개
m2 <- m2 %>% mutate(age_gr3=ifelse(T_AGE < 50, 1,
                                ifelse(T_AGE >= 50 & T_AGE < 60, 2, 
                                       ifelse(T_AGE >= 60, 3, NA))))  
table(m2$age_gr3)

library(descr) # freq에필요한 패키지
library(gmodels) # CrossTable에 필요한 패키지
CrossTable(m2$age_gr3, m2$T_HTN)
chisq.test(m2$age_gr3, m2$T_HTN)

#ICOME
m2<-m2 %>% mutate(inc_gr3=ifelse(T_INCOME < 5, 1,
                                ifelse(T_INCOME == 5 | T_INCOME == 6, 2, 
                                       ifelse(7<= T_INCOME & T_INCOME <= 8, 3, NA)))) 
table(m2$inc_gr3)

CrossTable(m2$inc_gr3, m2$T_HTN)
chisq.test(m2$inc_gr3, m2$T_HTN) # p-value = 0.01695


#BMI
#나이 정규분포 확인 (대상자가 많을떼 shapiro를써서 정규분포로 씀)
shapiro.test(m2$T_BMI) # 정규성검사 ks.test 대신에 사용 왜? 자료가 커서 
# p-value = 3.505e-05 로 0.05보다 작게나옴 
hist(m2$T_BMI) 
describeBy(m2$T_BMI, m2$T_HTN) # 자료요약 하면서 mean,sd 찾아서 표에 넣어라 

# var.test(T_BMI ~ T_HTN, data = m2 ) # p-value = 0.3327 라 0.05보다 커서 var.equal= T로 작성 
# t.test(T_BMI ~ T_HTN, data = m2, var.equal=T )
# p-value = 0.003692로 0.05보다 작으니 0.0001로 표에 작성

# 그룹별 사분범위 확인
m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_BMI, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_BMI, na.rm = T))
wilcox.test(T_BMI ~ T_HTN, data=m2)# < p-value = 0.001281 읽고, 작으니 < 0.0001작성


m2<-m2 %>% mutate(bmi_gr3=ifelse(T_BMI <23, 1,
                                       ifelse(T_BMI >= 23 & T_BMI < 25, 2,
                                              ifelse(T_BMI >= 25, 3, NA))))

table(m2$bmi_gr3)


CrossTable(m2$bmi_gr3, m2$T_HTN)
chisq.test(m2$bmi_gr3, m2$T_HTN) # 범주형 시작시 적는 p-value = 0.001625

# Weight
shapiro.test(m2$T_WEIGHT) # 정규성검사 ks.test 대신에 사용 왜? 자료가 커서 
# p-value = 0.001469 로 0.05보다 작게나옴 > 윌콕슨검사 
hist(m2$T_WEIGHT) 

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_WEIGHT, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_WEIGHT, na.rm = T))
wilcox.test(T_WEIGHT ~ T_HTN, data=m2)
# p-value = 0.06119 이므로 0.05보다 큼


#Exercise
CrossTable(m2$T_EXER, m2$T_HTN) # 범위확인
chisq.test(m2$T_EXER, m2$T_HTN) # p-value = 0.2418

#SBP
shapiro.test(m2$T_SBP)# p-value = 0.0001145
hist(m2$T_SBP) 
m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_SBP, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_SBP, na.rm = T))
wilcox.test(T_SBP ~ T_HTN, data=m2) # p-value = 5.933e-11


m2<-m2 %>% mutate(sbp_gr3=ifelse(T_SBP <130, 1,
                                 ifelse(T_SBP >= 130,2, NA)))
CrossTable(m2$sbp_gr3, m2$T_HTN)
chisq.test(m2$sbp_gr3, m2$T_HTN)
                                                                                                                                                                                                                                                                                        

#DBP
shapiro.test(m2$T_DBP)# p-value = 4.611e-08
hist(m2$T_DBP) 
m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_DBP, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_DBP, na.rm = T))
wilcox.test(T_DBP ~ T_HTN, data=m2) # p-value = 1.371e-05

m2<-m2 %>% mutate(dbp_gr3=ifelse(T_DBP <90, 1,
                                 ifelse(T_DBP >= 90,2, NA)))

table(m2$dbp_gr3)

CrossTable(m2$dbp_gr3, m2$T_HTN)
chisq.test(m2$dbp_gr3, m2$T_HTN) # 범주형 시작시 적는 p-value = 0.03038

#Pulse
shapiro.test(m2$T_PULSE) # p-value = 0.1324 로 0.05보다 크게나옴 > 정규분포
hist(m2$T_PULSE) 

describeBy(m2$T_PULSE, m2$T_HTN) # 자료요약 하면서 mean,sd 찾아서 표에 넣어라 
var.test(T_PULSE ~ T_HTN, data = m2 ) # p-value = 0.124 라 0.05보다 커서 var.equal= T로 작성 
t.test(T_PULSE ~ T_HTN, data = m2, var.equal=T ) # p-value = 0.3776


#Waist 
shapiro.test(m2$T_WAIST) # p-value = 0.05538 로 0.05보다 크게나옴 > 정규분포 > t
hist(m2$T_WAIST) 

describeBy(m2$T_WAIST, m2$T_HTN) # 자료요약 하면서 mean,sd 찾아서 표에 넣어라 
var.test(T_WAIST ~ T_HTN, data = m2 ) # p-value = 0.4621 라 0.05보다 커서 var.equal= T로 작성 
t.test(T_WAIST ~ T_HTN, data = m2, var.equal=T ) # p-value = 2.168e-05


#Hip
shapiro.test(m2$T_HIP) # p-value = 0.003769 로 0.05보다 작게나옴 > 윌콕슨
hist(m2$T_HIP) 


m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_HIP, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_HIP, na.rm = T))
wilcox.test(T_HIP ~ T_HTN, data=m2) # p-value = 0.0522

#Glucose
shapiro.test(m2$T_GLU0) # 1.686e-12 로 0.05보다 작게나옴 > 윌콕슨
hist(m2$T_GLU0) 

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_GLU0, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_GLU0, na.rm = T))
wilcox.test(T_GLU0 ~ T_HTN, data=m2) # p-value = 0.01194 


#Triglyceride
shapiro.test(m2$T_TG) # p-value < 2.2e-16 로 0.05보다 작게나옴 > 윌콕슨
hist(m2$T_TG) 

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_TG, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_TG, na.rm = T))
wilcox.test(T_TG ~ T_HTN, data=m2) # p-value = 0.5738


#Age at menarche T_MNSAG
shapiro.test(m2$T_MNSAG) # p-value = 2.071e-09 로 0.05보다 작게나옴 > 윌콕슨
hist(m2$T_MNSAG) 

m2 %>% filter(T_HTN==1) %>% summarize(quantile(T_MNSAG, na.rm = T)) 
m2 %>% filter(T_HTN==2) %>% summarize(quantile(T_MNSAG, na.rm = T))
wilcox.test(T_MNSAG ~ T_HTN, data=m2) # p-value = 0.09224

# Menopause T_PMYN
CrossTable(m2$T_PMYN, m2$T_HTN)
chisq.test(m2$T_PMYN, m2$T_HTN) # p-value = 0.001931


# 0. freq(m2$비교하는 열 변수) 하여 N = 빈도수 (퍼센트) 값 채우기
# 1. 정규성 검사 하기 ks테스트가 있지만 shapiro 로 진행 바람
# 2. 정규성 검사에서 0.05 보다 p-value 값이 크면 t테스트 작으면 윌콕슨 ( 연속형 )
# 3. t테스트면 describeBy의 평균과 표준편차 +_ 범위 작성
# 4. 윌콕슨 테스트면 사분위성 검사 해서 중앙값(위-아래)로 범위 작성 
# 5. t테스트나 윌콕슨테스트 한 p-value 값을 작성
# 6. 범주형이면 소수에는 왜 shapiro 정규성검사하고, t랑 윌콕슨 하는지 아직 모름 ??
# 6-1. 근데 내가볼때 비교 열 변수가 범주형이면 안하고 나머지 그냥 하는듯함
# 7. 범주형이면 그룹 변수 생성해서 CrossTable하여 값과 퍼센트 작성
# 8. CrossTable 한다음 카이제곱 검정 하여 p-value 값 작성 
# 9. 모든 p-value 값이 0.05보다 작으면 <0.0001로 표시 아니면 그 값만 소수점 4자리 남기고 작성
# 10. p-value 값 말고 범위 작성에는 소수점 1자리만 남기고 작성 !! 모두 반올림 조심

# library(descr) -> freq에 필요한 패키지
# library(gmodels) -> CrossTable에 필요한 패키지
# library(psych) -> 시각화에 필요한 패키지
# library(dplyr) -> 파이프 연산자에 필요한 패키지
