getwd()# 지금현재 데이터 

setwd("C:/Users/82102/Desktop/임데마/실습용 기반데이터")# 데이터 가져오기

dir()# 디렉토리 확인

#csv 파일 불러오기 
d1<-read.csv("BASE_DATA1.csv")
d2<-read.csv("BASE_DATA2.csv")
d3<-read.csv("BASE_DATA3.csv")


str(d1) #데이터 세트 구조 확인
str(d2) #데이터 세트 구조 확인
str(d3) #데이터 세트 구조 확인
dim(d1) #관측값 수 (행), 변수의 수 (열) 확인
ls(d1) #변수명 확인
head(d1) #데이터셋 앞부분 6행 출력 
head(d1,10) #데이터셋 앞부분 10행 출력
tail(d3) #데이터셋 뒷부분 6행 출력

#자료 결합 (merge) 
install.packages("dplyr")
library(dplyr)

m1<-left_join(d1,d2,by="T_ID")
m2<-left_join(m1,d3)

write.csv(m2, "fin.csv")

#연속형 자료 요약
m2$T_HEIGHT
summary(m2$T_HEIGHT)
str(m2$T_DRINK)
summary(m2$T_DRINK)

mean(m2$T_HEIGHT)#평균
sd(m2$T_HEIGHT)#표준편차
median(m2$T_HEIGHT)#중앙값
quantile(m2$T_HEIGHT)#사분위수
min(m2$T_HEIGHT)
max(m2$T_HEIGHT)

#psych 패키지 이용
install.packages("psych")
library(psych)
describe(m2$T_AGE) # 연속형만 가능 
describe(m2$T_HEIGHT)
describe(m2$T_DRINK)

#그래프
par(mfrow=c(1,1))
par(mfrow=c(1,2))
hist(m2$T_AGE,main="Histogram",xlab = "T_AGE")#히스토그램
boxplot(m2$T_AGE,main="Box plot",xlab="T_AGE")#boxplot


#그룹별 기술통계량
describeBy(m2$T_BMI, m2$T_SEX)
boxplot(T_BMI ~ T_SEX, data=m2)
win.graph() #표를 새로운창에 띄워줌

#범주형 자료 요약
install.packages("descr")
library(descr)
str(m2$T_DRINK)# str(m2$T_SEX)
m2$T_SEX<-as.factor(m2$T_SEX)
freq(m2$T_SEX)
freq(m2$T_SMOKE)
freq(m2$T_DRINK)


# 기본코드 결측치 처리 (일괄)---
m2[m2 == 66666 | m2 == 77777 | m2 == 99999] <- NA


#정규분포 만들기.. > 정규분포로 확 다 바꿔버리는 코드 
m2$T_SBP_n<-qqnorm(m2$T_SBP,plot.it=F)$x
m2$T_DBP_n<-qqnorm(m2$T_DBP , plot.it=F)$x
m2$T_PULSE_n<-qqnorm(m2$T_PULSE , plot.it=F)$x
m2$T_WAIST_n<-qqnorm(m2$T_WAIST , plot.it=F)$x
m2$T_HIP_n<-qqnorm(m2$T_HIP , plot.it=F)$x
m2$T_HEIGHT_n<-qqnorm(m2$T_HEIGHT , plot.it=F)$x
m2$T_WEIGHT_n<-qqnorm(m2$T_WEIGHT , plot.it=F)$x
m2$T_BMI_n<-qqnorm(m2$T_BMI , plot.it=F)$x
m2$T_HBA1C_n<-qqnorm(m2$T_HBA1C , plot.it=F)$x
m2$T_GLU0_n<-qqnorm(m2$T_GLU0 , plot.it=F)$x
m2$T_CREATINE_n<-qqnorm(m2$T_CREATINE , plot.it=F)$x
m2$T_AST_n<-qqnorm(m2$T_AST , plot.it=F)$x
m2$T_ALT_n<-qqnorm(m2$T_ALT , plot.it=F)$x
m2$T_TCHL_n<-qqnorm(m2$T_TCHL , plot.it=F)$x
m2$T_HDL_n<-qqnorm(m2$T_HDL , plot.it=F)$x
m2$T_LDL_n<-qqnorm(m2$T_LDL , plot.it=F)$x
m2$T_TG_n<-qqnorm(m2$T_TG , plot.it=F)$x


freq(m2$T_DM)
sum(is.na(m2$T_DM))

# 당뇨 진단 여부 결측 제외 
dm <- m2 %>% filter(!(is.na(T_DM)))

freq(dm$T_INCOME)


#변수 생성 / 표보고 1-4, 5-6, 7-8 묶어서 범위 조절
#Age group  
dm<-dm %>% mutate(age_gr=ifelse(T_AGE < 50, 1,
                                ifelse(T_AGE >= 50 & T_AGE < 60, 2, 
                                       ifelse(T_AGE >= 60, 3, NA)))) 


freq(dm$age_gr)
table(dm$T_AGE, dm$age_gr)

#Income group
dm<-dm %>% mutate(inc_gr=ifelse(T_INCOME < 5, 1,
                                ifelse(T_INCOME == 5 | T_INCOME == 6, 2, 
                                       ifelse(7<= T_INCOME & T_INCOME <= 8, 3, NA)))) 

table(dm$T_INCOME, dm$inc_gr)
str(dm$inc_gr)
freq(dm$inc_gr)
describe(dm$inc_gr)

#marital status
dm<-dm %>% mutate(marry=ifelse(T_MARRY == 2,1,
                               ifelse(T_MARRY %in% c(1,3,4,5,6), 2, NA)))

table(dm$marry, dm$T_MARRY)
freq(dm$marry)


#BMI
dm<-dm %>% mutate(bmi_gr=ifelse(T_BMI < 18.5, 1,
                                ifelse(T_BMI >= 18.5 & T_BMI <23, 2,
                                       ifelse(T_BMI >= 23 & T_BMI < 25, 3,
                                              ifelse(T_BMI >= 25, 4, NA)))))

freq(dm$bmi_gr)

#나머지는 애초에 범주형이라 그냥..
freq(dm$T_EXER)
freq(dm$T_SMOKE)
freq(dm$T_DRINK)


# 정규성 검정
ks.test(dm$T_AGE, pnorm) # p-value < 2.2e-16 읽기 0.05보다 작네 > 정규분포가 아니다.

#그룹별 연령 요약 
describeBy(dm$T_AGE, dm$T_DM) # 1번은 당뇨없는사람, 2번은 당뇨있는사람
# Age 행에 mean +_ sd 값 읽어서 작성 ex) 58.7+_7.93 / 53.3+-8.66

dm %>% filter(T_DM==1) %>% summarize(quantile(T_AGE)) 
# 당뇨없는사람에서만 age의 사분위 수를 요약을 해서 보여줘
dm %>% filter(T_DM==2) %>% summarize(quantile(T_AGE)) 
# 당뇨있는사람에서만 age의 사분위 수를 요약을 해서 보여줘


# 나이는 평균 비교로 갑시다. 
var.test(T_AGE ~ T_DM, data = dm) 
# 등분산 비교 > p-value = 0.001849 읽기 > 0.05보다 작네 > 등분산 만족 안한다.
t.test(T_AGE ~ T_DM, data = dm, var.equal=F) # var.equal=F 등분산 만족안해서 F입력
# p-value < 2.2e-16 읽고 , 너무 작으니 표에 < 0.0001 작성 
# ex) AGE / 58.7+_7.93 / 53.3+-8.66 / < 0.0001


# var > t 검정 실행 방법
################################## 
# wilcox 검정 실행 방법


#BMI 정규성 검정 
ks.test(dm$T_BMI, pnorm) #  p-value < 2.2e-16 정규분포 아님 > 근데 인척 예시
describeBy(dm$T_BMI, dm$T_DM) # median 값 읽기 ex) 25 / 24
sum(is.na(dm$T_BMI))
# dm %>% filter(T_DM==1) %>% summarize(quantile(T_BMI)) 결측값이 있으면 안돌아감
dm %>% filter(T_DM==1) %>% summarize(quantile(T_BMI, na.rm = T))
# 2번째와 4번째를 읽어와서 22,26 > ex) 25 / 24(22-26)
dm %>% filter(T_DM==2) %>% summarize(quantile(T_BMI, na.rm = T))
# 2번째와 4번째를 읽어와서 23,27 > ex) 25(23-27) / 24(22-26) > 왼쪽이 더 크네 

wilcox.test(T_BMI ~ T_DM, data = dm)
# p-value < 2.2e-16 값 읽고 , 너무 작으니 표에 < 0.0001 작성 
# ex) 25(23-27) / 24(22-26) / < 0.0001


#glucose 정규성 검정 
ks.test(dm$T_GLU0, pnorm)  
hist(dm$T_GLU0) # 히스토그램 그리기 > 왼쪽으로 치우쳐져있음
describeBy(dm$T_GLU0, dm$T_DM) # kurtosis값을 읽으면 크다 정규분포라고 말 못함

# 그럴때 윌콕슨

dm %>% filter(T_DM==1) %>% summarize(quantile(T_GLU0, na.rm = T)) 
# 그룹별 사분범위 확인 49, 84, 90, 98, 296 에서 중앙값 90을 두고, 90(84-98)작성 
dm %>% filter(T_DM==2) %>% summarize(quantile(T_GLU0, na.rm = T))
# 그룹별 사분범위 확인 53, 106, 124, 150, 394 에서 중앙값 124을 두고, 124(106-150)작성 

wilcox.test(T_GLU0 ~ T_DM, data=dm)# p-value < 2.2e-16 읽고, 작으니 < 0.0001작성

# 즉, glucose / 124(106-150) / 90(84-98) / < 0.0001 작성하면 됨 

#HbA1c 정규성 검정 
ks.test(dm$T_HBA1C, pnorm) #정규성검정 
hist(dm$T_HBA1C) # 히스토그램 그리기
describeBy(dm$T_HBA1C, dm$T_DM)# 그룹별 연속형 자료 요약

wilcox.test(T_HBA1C ~ T_DM, data=dm)# 비모수 검정 (윌콕슨)
dm %>% filter(T_DM==1) %>% summarize(quantile(T_HBA1C, na.rm = T)) # 그룹별 사분범위 확인
dm %>% filter(T_DM==2) %>% summarize(quantile(T_HBA1C, na.rm = T))


#choalesterol들 분석 > 위에 만든 n 을 가지고 사용할거임 > p-value = 1 나올거임
ks.test(dm$T_TCHL_n, pnorm) # p-value = 1 읽기
hist(dm$T_TCHL_n) # 이쁜 그래프 출력
describeBy(dm$T_TCHL_n, dm$T_DM) # mean , sd 값 읽기

var.test(T_TCHL_n ~ T_DM, data = dm) 
# p-value = 5.877e-10 읽기 > 엄청 작네 > 등분산 만족 안함
t.test(T_TCHL_n ~ T_DM, data = dm, var.equal=F) 
# p-value = 6.858e-08 읽기 > 차이 있음


####
ks.test(dm$T_HDL_n, pnorm) # p-value = 1 읽기
hist(dm$T_HDL_n) # 이쁜 그래프 출력
describeBy(dm$T_HDL_n, dm$T_DM) # mean(평균) , sd(표준편차) 값 읽고, 입력

var.test(T_HDL_n ~ T_DM, data = dm)
# p-value = 0.9848 읽기 > 0.05보다 커서 > 등분산 만족함
t.test(T_HDL_n ~ T_DM, data = dm, var.equal=T) 
# 그래서 T로 입력 > p-value < 2.2e-16 > 유의미한 차이가 난다.
# var.equal=T > 등분산 만족 할 때 
# var.equal=F > 등분산 만족 안할 때

####
ks.test(dm$T_LDL_n, pnorm) # p-value = 1 읽기
hist(dm$T_LDL_n) # 이쁜 그래프 출력
describeBy(dm$T_LDL_n, dm$T_DM) # mean(평균) , sd(표준편차) 값 읽고, 입력

var.test(T_LDL_n ~ T_DM, data = dm)
# p-value = 4.384e-13 읽기 > 0.05보다 작아서 > 등분산 만족안함
t.test(T_LDL_n ~ T_DM, data = dm, var.equal=F) 
# p-value = 1.765e-08 읽고, 차이 남


####
ks.test(dm$T_TG_n, pnorm) # p-value = 1 읽기
hist(dm$T_TG_n) # 이쁜 그래프 출력
describeBy(dm$T_TG_n, dm$T_DM) # mean(평균) , sd(표준편차) 값 읽고, 입력

var.test(T_TG_n ~ T_DM, data = dm)
# p-value = 0.2475 읽기 > 0.05보다 커서 > 등분산 만족함
t.test(T_TG_n ~ T_DM, data = dm, var.equal=T)
# p-value < 2.2e-16 읽고 > 차이가 남


#Height 정규성 검정 
ks.test(dm$T_HEIGHT, pnorm) #정규성검정  p-value < 2.2e-16 읽기 > 정규분포는 아님
hist(dm$T_HEIGHT) # 정규분포는 아닌데 히스토그램은 근데 좀 괜찮게 나와서 t테스트 해볼게
describeBy(dm$T_HEIGHT, dm$T_DM) # mean(평균) , sd(표준편차) 값 읽고, 조금 차이나네

var.test(T_HEIGHT ~ T_DM, data = dm) # p-value = 0.02636 읽기 > 0.05보다 작아서 F
t.test(T_HEIGHT ~ T_DM, data = dm, var.equal=F) 
# p-value = 0.3577 이고,  160.0146 / 160.3249 평균비교로 차이가 없다고 출력됨

#pv<- wilcox.test(T_Height ~ T_DM, data=dm)# 비모수 검정 (윌콕슨)
#pv$p.value

#wilcox.test(T_Height ~ T_DM, data=dm)
#dm %>% filter(T_DM==1) %>% summarize(quantile(T_HEIGHT, na.rm = T)) # 그룹별 사분범위 확인
#dm %>% filter(T_DM==2) %>% summarize(quantile(T_HEIGHT, na.rm = T)) # F인지 T인지 ..


###########################################################
# 연속형 은 +_ 
# 범주형은 퍼센트 작성 

# 범주형 시작 > 교차표 가져오기 앞 행, 뒤 열 > 교차표 그리기

#교차표 및 chi-square
install.packages("gmodels")
library(gmodels)

# 이 두줄만 실행하면 댐 간단

CrossTable(dm$T_SEX, dm$T_DM)# 교차표 
# 319(44.8) / 3134(33.8) 남자 
# 393(55.2) / 6135(66.2) 여자
chisq.test(dm$T_SEX, dm$T_DM) 
# chi-square 카이제곱 검정 ----> p-value = 3.614e-09 읽기 > < 0.0001작성

# 즉, 표채우기
# 319(44.8) / 3134(33.8) 남자 < 0.0001
# 393(55.2) / 6135(66.2) 여자 

# 근데p-value < 2.2e-16일경우 가장 적은 경우인데, 이때 p-value 자세히 보고싶다.
ct<-chisq.test(dm$T_SEX, dm$T_DM)
ct$p.value # [1] 3.613688e-09 이렇게 자세히 나옴



CrossTable(dm$age_gr, dm$T_DM)
# 96(13.5) / 3443(37.1) 1 
# 274(38.5) / 3413(36.8) 2 
# 342(48.0) / 2413(26.0) 3  작성
chisq.test(dm$age_gr, dm$T_DM) # p-value < 2.2e-16읽기
ct<-chisq.test(dm$age_gr, dm$T_DM)
ct$p.value # [1] 1.102924e-48 정확한 값 읽을 수 있음

# 즉, 표 작성하기 
# 96(13.5) / 3443(37.1) 1 / < 0.0001
# 274(38.5) / 3413(36.8) 2 
# 342(48.0) / 2413(26.0) 3 

# AGE표는 즉,
# AGE / 58.7+_7.93 / 53.3+-8.66 / < 0.0001
# 96(13.5) / 3443(37.1) 1 / < 0.0001
# 274(38.5) / 3413(36.8) 2 
# 342(48.0) / 2413(26.0) 3 

CrossTable(dm$marry, dm$T_DM) # 교차표 
# 607(85.7) / 8061(87.4) 동거 함
# 101(14.3) / 1157(23.6) 혼자 삼
chisq.test(dm$marry, dm$T_DM) # p-value = 0.2068 읽기 > 통계적으로 유의미한 차이가 없다.

# 즉, Marital status 표채우기
# 607(85.7) / 8061(87.4) 동거 함 / 0.2068
# 101(14.3) / 1157(23.6) 혼자 삼


### 나머지 카이제곱 만해도,  p-value 값들은 0.05보다 작아서 유의미한 차이 있음을 확인 가능함

CrossTable(dm$T_EXER, dm$T_DM)# 교차표 > 퍼센트는 밑에서 2번째꺼 읽기
chisq.test(dm$T_EXER, dm$T_DM) # chi-square > 두분표가 차이가 나냐 확인하기 위해 피벨류 읽기


CrossTable(dm$T_SMOKE, dm$T_DM)# 교차표 
chisq.test(dm$T_SMOKE, dm$T_DM) # chi-square 카이제곱 검정 

chisq.test(dm$bmi_gr, dm$T_DM)

chisq.test(dm$T_DRINK, dm$T_DM)

