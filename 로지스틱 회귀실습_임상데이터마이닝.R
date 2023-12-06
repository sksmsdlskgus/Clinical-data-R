#현재 디렉토리 확인
getwd() 

#디렉토리 설정
setwd("C:\\Users\\박601-49\\Documents\\카카오워크 받은 파일")

#현재 디렉토리 안에 있는 파일 확인
dir()

#파일 불러오기
m2<-read.csv ("merge.csv")

#비만정도에 따라 당뇨유병 위험에 차이가 있는가?
# X = 비만도, Y= 당뇨 여부

#BMI 요약
summary(m2$T_BMI)
#혈당 요약
summary(m2$T_GLU0)
#결측 처리
m2[m2 == 66666 | m2 == 77777 | m2 == 99999] <- NA

#결측수 확인
sum(is.na(m2$T_BMI)) #51
sum(is.na(m2$T_GLU0)) #236
sum(is.na(m2$T_DM)) #19
library(dplyr)
r<-m2 %>% filter(!(is.na(T_BMI))) # n=9949
r1<-r %>% filter(!(is.na(T_GLU0))) # n=9720
r2<-r1 %>% filter(!(is.na(T_DM))) # n=9709

#비만도 변수 만들기
# BMI -> 저체중 (BMI <18.5), 
#정상 (BMI 18.5~23), 
#과체중 (BMI 23 ~ 25), 비만(>25)
r3<-r2 %>% mutate(bmi_gr=ifelse(T_BMI<18.5,1,
   ifelse(T_BMI>=18.5 & T_BMI<23,2,
   ifelse(T_BMI>=23&T_BMI<25,3,
   ifelse(T_BMI >= 25, 4, NA)))))

#당뇨 유병 여부 확인
r4 <- r3 %>% mutate
(dm = ifelse(T_DM == 2 | T_GLU0 >= 126, 1, 0))
r4 <- r4 %>% mutate
(dm_n = ifelse(T_DM   T_GLU0 , 0, 1))
table(r4$dm,r4$dm_n)

sum(is.na(r4$dm)) 
table(r4$dm)
table(r4$dm,r4$T_DM)

library(descr)
library(gmodels) # CrossTable() 에 필요한 패키지
CrossTable(r4$T_DM, r4$dm)


CrossTable(r4$bmi_gr, r4$dm)
chisq.test(r4$bmi_gr, r4$dm)

glm(dm ~ bmi_gr, data = r4, family = "binomial")
lg<-glm(dm ~ bmi_gr, data = r4, family = "binomial")
summary(lg)

lg1<-glm(dm ~ T_BMI, data = r4, family = "binomial")
summary(lg1)

exp(coef(lg1)) # 오즈비
exp(confint.default(lg1)) # 95% 신뢰구간

#범주형 변수로 변경해주기
str(r4$dm)
r4$dm<-as.factor(r4$dm)
str(r4$bmi_gr)
r4$bmi_gr<-as.factor(r4$bmi_gr)

lg2 <- glm(dm ~ bmi_gr, data = r4, family = "binomial")
summary(lg2)

exp(coef(lg2)) # 오즈비
exp(confint.default(lg2)) # 95% 신뢰구간

# 기준 범주 (reference) 변경
r4$bmi_gr <- relevel(r4$bmi_gr, ref = "2")
lg3 <- glm(dm ~ bmi_gr, data = r4, family = "binomial")
summary(lg3)

exp(coef(lg3)) # 오즈비
exp(confint.default(lg3)) # 95% 신뢰구간
