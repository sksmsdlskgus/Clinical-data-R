#현재 디렉토리 확인 차이가 나며 어디가크다라는 해석해달라
getwd() 

#디렉토리 설정
setwd("C:/Users/82102/Desktop/임데마/임상데이터마이닝/실습용 기반데이터")

#현재 디렉토리 안에 있는 파일 확인
dir()

#데이터 저장
write.csv(m2, "merge.csv", row.names = F)
m2<-read.csv ("merge.csv")
w<-subset(m2,T_SEX ==1 & T_HTN==1)

#BMI는 혈압과 통계적으로 유의미한 연관성이 있다.
# X = BMI, Y = T_SBP, T_DBP

#주요 변수들 위주로 기술 통계 확인
win.graph()
summary(w$T_BMI)
hist(w$T_BMI)
library(psych)
describe(w$T_BMI) #n값을 Raw data n= 칸에 채워넣기

summary(w$T_SBP)
hist(w$T_SBP)
describe(w$T_SBP)

summary(w$T_DBP)
hist(w$T_DBP)
describe(w$T_DBP)


#결측처리
w[w == 66666 | w == 77777 | w == 99999] <- NA


#보정변수선정 ############################## 플로우 차트 시작 
#income
#exercise, smoke, drink

#결측수 확인
sum(is.na(w$T_DBP)) #11
sum(is.na(w$T_SBP)) #11  # DBP 또는 SBP에 결측 있는 대상자 수 = 11 칸에 채워넣기 
sum(is.na(w$T_BMI)) #19 # BMI에결측있는대상자수 = 14 칸에 채워넣기
sum(is.na(w$T_INCOME)) #461 # Income, exercise, drink, smoke 변수에 결측 있는 대상자수 = 461 칸에 채워넣기
sum(is.na(w$T_EXER)) #8 
sum(is.na(w$T_DRINK)) #8
sum(is.na(w$T_SMOKE)) #8

r1 <- w %>% filter(!(is.na(T_SBP))) #2712
r2 <- r1 %>% filter(!(is.na(T_DBP))) #2712 # N값 채워넣기 2723-"11" = 2712
r2 <- w %>% filter(!(is.na(T_DBP))) #2712
r3 <- r2 %>% filter(!(is.na(T_BMI))) #2698 # N값 채워넣기 2723-"14" = 2698
r4 <- r3 %>% filter(!(is.na(T_INCOME))) #2247
r5 <- r4 %>% filter(!(is.na(T_EXER))) #2244
r6 <- r5 %>% filter(!(is.na(T_DRINK))) #2240
r7 <- r6 %>% filter(!(is.na(T_SMOKE))) #2237 # Final data N= 2723-"461" = 2237

###############################플로우차트 마무리 


# 2. 변수 만들고 빈도 표 작성하기
# 비만도 변수 만들기
# BMI -> 저체중 (BMI <18.5), 
# 정상 (BMI 18.5~23), 
# 과체중 (BMI 23 ~ 25), 비만(>25)

# 1번 Income group 표 채우기 
r7 <- r7 %>% mutate(inc_gr = ifelse(T_INCOME < 5, 1, 
                                    ifelse(T_INCOME == 5 | T_INCOME == 6, 2,
                                           ifelse(T_INCOME == 7 | T_INCOME == 8, 3, NA))))

library(descr)
table(r7$T_INCOME, r7$inc_gr) # 한번씩 새로만들면 꼭 해달라
freq(r7$inc_gr) # Frequency (Percent) 값 작성 



# 2번 음주 경험 여부 표 채우기
r7 <- r7 %>% mutate(drink = ifelse(T_DRINK == 1, 1,
                                   ifelse(T_DRINK == 2 | T_DRINK == 3, 2, NA)))

table(r7$drink ,r7$T_DRINK)
freq(r7$drink) # Frequency (Percent) 값 작성 



# 3번 흡연 경험 여부 표 채우기
r7 <- r7 %>% mutate(smoke = ifelse(T_SMOKE == 1, 1,
                                   ifelse(T_SMOKE == 2 | T_SMOKE == 3, 2, NA)))
table(r7$smoke ,r7$T_SMOKE)
freq(r7$smoke) # Frequency (Percent) 값 작성 



# 4번 BMI group 표 채우기
r7<-r7 %>% mutate(bmi_gr=ifelse(T_BMI < 18.5, 1,
                                ifelse(T_BMI >= 18.5 & T_BMI <23, 2,
                                       ifelse(T_BMI >= 23 & T_BMI < 25, 3,
                                              ifelse(T_BMI >= 25, 4, NA)))))
table(r7$bmi_gr ,r7$T_BMI)
freq(r7$bmi_gr) # Frequency (Percent) 값 작성 



# 5번 고혈압 여부 (SBP≥140 또는 DBP≥90) 표 채우기
r7 <- r7 %>% mutate(htn = ifelse(T_SBP>=140 | T_DBP >= 90, 1, 0)) #클린코드 - 결측치 없을 때 가능

table(r7$htn, r7$T_SBP, r7$T_DBP)
freq(r7$htn) # Frequency (Percent) 값 작성 

#####################################################2번 끝

win.graph()
hist(r7$T_BMI)
library(psych)
describe(r7$T_BMI)


hist(r7$T_SBP)
describe(r7$T_SBP)

hist(r7$T_DBP)
describe(r7$T_DBP)


plot(r7$T_BMI,r7$T_SBP) #확인차
plot(r7$T_BMI,r7$T_DBP)
plot(r7$T_SBP,r7$T_DBP)

cor.test(r7$T_BMI,r7$T_SBP)
cor.test(r7$T_BMI,r7$T_DBP)
pv<- cor.test(r7$T_BMI,r7$T_DBP)
pv$p.value


# 변수 속성 확인 
str(r7$inc_gr)
str(r7$T_EXER)
str(r7$drink)
str(r7$smoke)

#선형 회귀분석 > 3번 표
rg7<-lm(T_SBP ~ T_BMI, data=r7)
summary(rg7) # 해석 잘해줭

rg7<-lm(T_SBP ~ T_BMI + T_AGE + inc_gr, data=r7)
summary(rg7) #인컴은 통계적으로 유의한 연관성이 없음

rg7<-lm(T_SBP ~ T_BMI + T_AGE + inc_gr + T_EXER, data=r7)
summary(rg7)

rg7<-lm(T_SBP ~ T_BMI + T_AGE + inc_gr + T_EXER + smoke, data=r7)
summary(rg7)

rg7<-lm(T_SBP ~ T_BMI + T_AGE + inc_gr + T_EXER + smoke + drink, data=r7)
summary(rg7)  # SBP 종속변수 표 채우기 > coefficients 부분과 Pr(>|t|)부분 읽고 작성 
# 소수 4자리까지만

# BMI는 나머지를 고려해도 여전히 통계적으로 유의미한 양의 연관성을 가지고 있다. (BMI해석)
# 나이가 한살씩 증가할때마다 혈압이 올라가는 경향을 보임 으음.. 


#선형 회귀분석
rg8<-lm(T_DBP ~ T_BMI, data=r7)
summary(rg8) # 해석 잘해줭

rg8<-lm(T_DBP ~ T_BMI + T_AGE + inc_gr, data=r7)
summary(rg8) #인컴은 통계적으로 유의한 연관성이 없음

rg8<-lm(T_DBP ~ T_BMI + T_AGE + inc_gr + T_EXER, data=r7)
summary(rg8)

rg8<-lm(T_DBP ~ T_BMI + T_AGE + inc_gr + T_EXER + smoke, data=r7)
summary(rg8)

rg8<-lm(T_DBP ~ T_BMI + T_AGE + inc_gr + T_EXER + smoke + drink, data=r7)
summary(rg8)  # DBP 종속변수 표 채우기 > coefficients의 Estimate 부분과 Pr(>|t|)부분 읽고 작성 


###################################### 3번 끝 

#로지스틱 > y변수는 0,1이여야만 함


r7$bmi_gr<-relevel(as.factor(r7$bmi_gr), ref = "2")
r7$inc_gr <- as.factor(r7$inc_gr)
r7$T_EXER <- as.factor(r7$T_EXER)
r7$smoke <- as.factor(r7$smoke)
r7$drink <- as.factor(r7$drink)


rg7 <- glm(htn ~ bmi_gr + T_AGE + inc_gr + T_EXER + smoke + drink, data = r7, family = "binomial")
summary(rg7) 
table(r7$bmi_gr)
str(r7$bmi_gr)
exp(coef(rg7))  #오즈비 > 4번 시작 
# bmi_gr2만 안뜨면 그게 기준이 됨
# 2번 그룹을 기준으로 하여 2번 오즈비 칸엔 1.00 작성하고 신뢰구간에는 ref라고 적는다.

exp(confint.default(rg7)) #95% CI구간 1보다 작으면 음의 연관성 크면 양의 연관성
# (2.5 % - 97.5 %) 순서대로 작성 ex.(0.52-0.88)

# 오즈비과 신뢰구간은 소수점 두자리 까지 작성 

# summary(rg7)를 돌려 비교대상인 기준이 존재한다면 p-value 가 0.05보다 크다.= 유의미하다. (해석?)
################################################### 4번 끝 
