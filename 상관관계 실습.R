#현재 디렉토리 확인
getwd() 

#디렉토리 설정
setwd("D:\\강의자료\\임상데이터마이닝\\실습용 기반데이터")

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

#BMI 요약
summary(m2$T_BMI)

# 기본코드 결측치 처리 (일괄) ----
m2[m2 == 66666 | m2 == 77777 | m2 == 99999] <- NA


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
