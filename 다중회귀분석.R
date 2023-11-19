#현재 디렉토리 확인
getwd() 

#디렉토리 설정
setwd("C:/Users/82102/Desktop/임데마/임상데이터마이닝/실습용 기반데이터")

#현재 디렉토리 안에 있는 파일 확인
dir()

#csv 파일 불러오기
d1<-read.csv("BASE_DATA1.csv")
d2<-read.csv("BASE_DATA2.csv")
d3<-read.csv("BASE_DATA3.csv")

#자료 결합 (merge) #
install.packages("dplyr")
library(dplyr)

m1<-left_join(d1,d2,by="T_ID")
m2<-left_join(m1,d3)

#데이터 저장
write.csv(m2, "merge.csv", row.names = F)
m2<-read.csv ("merge.csv")


#체질량 지수가 혈당에 미치는 영향
# X= BMI, Y= Glucose


#BMI 요약
summary(m2$T_BMI)

#혈당 요약
summary(m2$T_GLU0)

# 기본코드 결측치 처리 (일괄) ----
m2[m2 == 66666 | m2 == 77777 | m2 == 99999] <- NA




#결측수 확인
sum(is.na(m2$T_BMI)) #51
sum(is.na(m2$T_GLU0)) #236


library(dplyr)
#결측있는 대상자 제거
r<-m2 %>% filter(!(is.na(T_BMI))) #n=9949
r1<-r %>% filter(!(is.na(T_GLUO))) #n=9720




#단순 선형 회귀
lm(T_GLUO ~ T_BMI, data = r1)
regl<- lm(T_GLUO~T_BMI, data = r1)
summary(regl)


#산점도 및 회귀식
plot(T_GLUO~T_BMI, data = r1)
abline(coef(regl))

#다중회귀분석
mreg<-lm(T_GLUO~T_BMI+T_AGE+T_SEX, data = r1)
summary(mreg)

#다중공선성 진단
install.packages("car")
options(digits = 3)
library(car)
car::vif(mreg) #vif>10 이면 다중공선성 의심
vif(mreg)

#다중공선성 예시
mreg2<-lm(T_GLUO~T_BMI+T_AGE+T_SEX+T_HEIGHT+T_WEIGHT, data = r1)
summary(mreg2)
vif(mreg2)


mreg3<-lm(T_GLUO~T_BMI+T_AGE+T_SEX+T_HEIGHT+T_WEIGHT+T_MARRY+T_INCOME+T_SMOKE+T_DRINK+T_EXER, data = r1)
summary(mreg3)
v_sel3<-step(mreg3, direction = "both")
#전진=forward 후진=backward 단계적=both

#결측없는 dataset만들기
test1<-r1 %>% select(T_ID,T_GLUO,T_BMI,T_AGE,T_SEX,T_HEIGHT,T_WEIGHT,T_INCOME,T_MARRY,T_SMOKE,T_DRINK,T_EXER)
dc1<-na.omit(test1)
#변수들 중 Na가 1개라도 있는 대상자 모두 제거

v_sel1<-step(lm(T_GLUO~T_BMI+T_AGE+T_SEX+T_HEIGHT+T_WEIGHT+T_MARRY+T_INCOME+T_SMOKE+T_DRINK+T_EXER, data = dc1),direction = "forward")
# 전진 forward, 후진 backward, 단계적 both
summary(v_sel1)

#범주형 변수 입력 코드에 따른 회귀분석 결과 
table(r1$T_SMOKE)
ss<-lm(T_GLU0~T_SMOKE,data = r1)
summary(ss)

r1<-r1 %>% mutate(smoke=T_SMOKE-1)
table(r1$smoke)


ss1<-lm(T_GLU0~smoke, data = r1)
summary(ss1)

ss2<-lm(T_GLU0~smoke + T_SEX + T_AGE, data = r1)
summary(ss2)


#더미변수 만들기
dd<- transform(r1, drink1= as.factor(ifelse(T_DRINK=="1",1,0)),  drink2= as.factor(ifelse(T_DRINK=="2",1,0)), drink3= as.factor(ifelse(T_DRINK=="3",1,0)))
table(dd$drink1)

dr2<-lm(T_GLU0~ drink1 + drink2 + drink3 + T_SEX + T_AGE, data=dd)
dr2<-lm(T_GLU0~ drink1 + drink3 + T_SEX + T_AGE, data=dd)
dr2<-lm(T_GLU0~ drink3 + T_SEX + T_AGE, data=dd)
dr2<-lm(T_GLU0~ drink1 + T_SEX + T_AGE, data=dd)

summary(dr2)
vif(dr2)

#산점도 그리기
plot(m2$T_TCHL,m2$T_LDL)
plot(m2$T_GLU0,m2$T_BMI)
plot(m2$T_GLU0,m2$T_HBA1C)


# 히스토그램, 왜도와 첨도 확인
library(psych)
hist(m2$T_TCHL)
describe(m2$T_TCHL)
hist(m2$T_LDL)
describe(m2$T_LDL)

hist(m2$T_GLU0)
describe(m2$T_GLU0)
hist(m2$T_BMI)
describe(m2$T_BMI)

hist(m2$T_HBA1C)
describe(m2$T_HBA1C)


cor.test(m2$T_TCHL,m2$T_LDL)

cor.test(m2$T_TCHL,m2$T_LDL, method="pearson")



cor.test(m2$T_GLU0,m2$T_HBA1C)

cor.test(m2$T_GLU0,m2$T_HBA1C, method="pearson") 

win.graph()
plot(m2$T_GLU0,m2$T_BMI)
cor.test(m2$T_GLU0,m2$T_BMI)
cor.test(m2$T_GLU0,m2$T_BMI, method="spearman")

c<-cor.test(m2$T_GLU0,m2$T_BMI)
cp<-cor.test(m2$T_GLU0,m2$T_BMI, method="spearman")

c$estimate
cp$estimate
c$p.value
cp$p.value






which(colnames(m2)=="T_SBP") 
#해당 변수가 몇번째인지
which(colnames(m2)=="T_TG") 
#해당 변수가 몇번째인지
str(m2[,47:63])
cor(m2[,47:63])
cor(m2[,47:63], use="complete.obs", method = "pearson")
cc<-cor(m2[,47:63], use="complete.obs", method = "pearson")
cor(m2[,47:63], use="pairwise.complete.obs", method = "pearson")
pcc<-cor(m2[,47:63], use="pairwise.complete.obs", method = "pearson")
cor.test(m2$T_TCHL,m2$T_LDL)

#차이가 무엇일지 확인해봅시다
dc<-d3 #임상검사 수치만 있는 data d3을 새로운 이름으로 저장합니다
dc[dc == 66666 | dc == 77777 | dc == 99999] <- NA #결측처리합니다.

which(colnames(dc)=="T_SBP") #해당 변수가 몇번째인지
which(colnames(dc)=="T_TG") #해당 변수가 몇번째인지
cc<-cor(dc[,3:19], use="complete.obs", method = "pearson")
pcc<-cor(dc[,3:19], use="pairwise.complete.obs", method = "pearson")


cor.test(dc$T_SBP,m2$T_TG) 
sum(is.na(dc$T_SBP)|is.na(dc$T_TG))


dc1<-na.omit(dc) #dc에서 17개 임상검사 수치중 하나라도 결측이 있는 대상자는 제외
cor.test(dc1$T_SBP,dc1$T_TG)
sum(is.na(dc1$T_SBP))
sum(is.na(dc1$T_TG))




win.graph()
library(corrplot)
corrplot(cc)
corrplot(cc, method="number")

pairs(cc)

install.packages("corrgram")
library(corrgram)
corrgram(cc)
corrgram(cc, upper.panel=panel.conf)

install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cc)

