score4<-c(3,3,6,7,7,10,10,10,11,13,30)
(n<-length(x))
min(score4)
max(score4)
range(score4)
diff(range(score4))
#평균
mean(score4)
#중위수
median(score4)
#분산
#분산을 구하는 이유 : 평균의 질적 판단, PCA(주성분 분석)을 위해(분산이 크면 중요한 데이터)
var(score4)
#표준편차 : 질적 상태 판단, 정규분포의 기준값
#표준편차 1배수 : 68%, 2배수 : 95%, 3배수 : 99.7%
sd(score4) 
#표준오차 : 신뢰구간 설정
#변동계수 : 이질적 집단 비교
#정규분포 : 모집단은 정규분포를 가짐

#install.packages("moments")
library(moments)
#set.seed : 시드값을 설정하면 랜덤으로 설정해도 계속 같은 값을 얻을 수 있음
set.seed(1234) #컴퓨터의 난수 : 의사난수, 그래서 시드값으로 랜덤하게 선택
n.sample<-rnorm(n=10000, mean=55, sd=4.5) #sampling
skewness(n.sample) #왜도 : 기준값은 0, 좌우대칭
kurtosis(n.sample) #첨도
#1차 원점에대한 적률 : 평균
#2차 원점에대한 적률 : 분산
#3차 원점에대한 적률 : 왜도
#4차 원점에대한 적률 : 첨도

#정규분포가 아닌 데이터 : log,sqrt를 이용해 정규분포로 변환해야 계산 가능
#install.packages("UsingR")
library(UsingR)
data(cfb)
head(cfb)
summary(cfb$INCOME)
hist(cfb$INCOME, breaks=500, freq=TRUE)

cfb<-transform(cfb,INCOME_log=log(INCOME+1)) #log0은 무한대로 가기때문에 전체적으로 +1을 해줌
hist(cfb$INCOME_log, breaks=500, freq=TRUE)

cfb<-transform(cfb,INCOME_sqrt=sqrt(INCOME+1))
hist(cfb$INCOME_sqrt, breaks=500, freq=TRUE) #정규분포화

par(mfrow=c(1,3))
qqnorm(cfb$INCOME, main="INCOME")
qqline(cfb$INCOME)

qqnorm(cfb$INCOME_log, main="INCOME_log")
qqline(cfb$INCOME_log)

qqnorm(cfb$INCOME_sqrt, main="INCOME_sqrt")
qqline(cfb$INCOME_sqrt)

par(mfrow=c(1,1))

skewness(cfb$INCOME)
kurtosis(cfb$INCOME)
skewness(cfb$INCOME_log)
kurtosis(cfb$INCOME_log)
skewness(cfb$INCOME_sqrt)
kurtosis(cfb$INCOME_sqrt)

#변동계수
data(trees)
dim(trees)
head(trees)
summary(trees)
sd(trees$Height) #높이 6.37
sd(trees$Girth)  #둘레 3.13
sd(trees$Volume)  #체적 16.4(표준편차가 가장 크므로 셋 중 데이터가 가장 분산되있을 것이라 예상)
sd(trees$Height); sd(trees$Girth); sd(trees$Volume)
#변동계수로 이질적 집단의 표준편차를 비교
sd(trees$Volume)/mean(trees$Volume)
sd(trees$Girth)/mean(trees$Girth)
sd(trees$Height)/mean(trees$Height)

#표준오차
x=c(0,1,2,3,4,5,6,7,8,9,10,11)
se<-sd(x)/sqrt(length(x))
c(mean(x)-(2*se),mean(x)+(2*se)) #95% 신뢰구간에대한 하한값, 상한값
c(mean(x)-(3*se),mean(x)+(3*se)) #99% 신뢰구간에대한 하한값, 상한값

#문제 : iris에서 Sepal.Length에 대해 표준오차를 구하고 95% 신뢰구간 값을 구해보시오
data(iris)
sd(iris$Sepal.Length)
(iris_se<-(sd(iris$Sepal.Length))/sqrt(length(iris$Sepal.Length)))
c(mean(iris$Sepal.Length)-(2*iris_se),mean(iris$Sepal.Length)+(2*iris_se))
###선생님 풀이
stderr<-function(x) sd(x,na.rm = T)/sqrt(length(na.omit(x)))
se<-stderr(iris$Sepal.Length)
c(mean(iris$Sepal.Length)-(2*se),mean(iris$Sepal.Length)+(2*se))

#문제 : 평균이 100이고 표준편차가 10인 정규분포에서 50이 나올 정규분포 함수 값을 구하시오
(z1=(50-100)/10)
dnorm(z1,mean=0,sd=1)
dnorm(50,mean=100,sd=10)

#문제1 : 비행기의 평균 비행시간은 120시간이라고 하자 비행시간이 정규분포이고 표준편차가 30시간이라고 가정하고,
#9대 표본으로 추출할 때와 36대의 표본으로 추출할때 표본평균과 분산을 비교해보시오
t9=rnorm(9,120,30)
t36=rnorm(36,120,30)

t100=rnorm(100,120,30)
t1000=rnorm(1000,120,30)

mean(t9)  #112.4985
var(t9)   #1180.337
mean(t36) #118.8513
var(t36)  #1165.575

mean(t100)  #117.3194
var(t100)   #881.7493
mean(t1000) #120.8123
var(t1000)  #884.1899
#표준오차
stderr(t9)    #11.45201
stderr(t36)   #5.690087
stderr(t100)  #2.969426
stderr(t1000) #0.9403137
#모집단 : 표본이 많아지면 점점 정규분포에 가까워지고, 표본이 많아지면 표준오차는 점점 줄어든다

#문제2 : x~(300,50^2)(=평균,분산)인 정규분포에서 P(X>=370)일 확률을 구하여라(Z점수 사용)
zpoint=(370-300)/50 #Z점수 = (구하고 싶은 값 - 평균)/표준편차
pnorm(zpoint)
(1-pnorm(zpoint))*100
#시각적으로 표현하시오
(x<-rnorm(1000,0,1)) #표준정규분포에서 방정식은 무한대 입력 가능 : 일정한 범위
curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)") #dnorm이 1000개의 대해서 높이 값을 알려줌 -4~4까지
z=seq(((370-300)/50),4,0.01)
lines(z,dnorm(z),type="h",col="gray")
points(((370-300)/50),dnorm(((370-300)/50)))

# 문제3 브랜드의 백열전구의 수명이 1500시간의 평균값과 75시간의 표준편차로 정규적으로 분포되어 있다.
# #  1) 백열 전구가 1410 시간보다 덜 오래 갈 확률은 얼마인가?
(x<-rnorm(1000,0,1)) 
curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)") #표준정규분포 그래프 그리기
z=seq(-4,(1410-1500)/75,0.01) #-4부터 (1410-1500)/75까지 
lines(z,dnorm(z),type="h",color="gray")
(Z=(1410-1500)/75)
pnorm(Z)
# #  2) 백열전구가 1563과 1648시간 사이의 오래갈 확률은 얼마인가?
# 계산해서 할 때는 -값이 나와서 그냥 진행이 되고 실제 값을 넣을 때는 자기 수를 그냥 넣어주면 된다.
# 1563과 1648시간 사이의 오래갈 확률 구간을 그래프 상에서 시각적으로 표현해 보시요
# +이면 누적확률이고 -이면 거기 까지 인가.
# 3) 위의 구간을 정규분포 곡선으로 그려 그래프 위에 표시해 보시요
Z1=(1648-1500)/75
Z2=(1563-1500)/75
pnorm(Z1)-pnorm(Z2)
curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)")
z=seq((1563-1500)/75,(1648-1500)/75,0.01)
lines(z,dnorm(z),type="h",col="gray")

#문제 4
#우리나라에서 사육하고 있는 생후 18개월 이상된 황소 무게는 평균 500kg, 표준편차 50kg인 정규분포
#우량한우를 집중 육성 관리하기위해 무게가 무거운 순서대로 5%에 해당하는 황소를 선발하고자 한다
#그렇다면 무게가 몇 kg인 황소를 선발해야하는가
ox=rnorm(10000,500,50) #랜덤하게 황소 생성
(x<-rnorm(1000,0,1))
curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)")
start<-qnorm(1-0.05,500,50) #5%를 얻어야해서 qnorm에 대입
z=seq((start-500)/50,4, 0.01)
lines(z,dnorm(z),type="h",col="gray")

#카이제곱분석(범주형 데이터)
#기대빈도 : 이상적인 데이터,전체 데이터/변수의 개수
#카이제곱 값 = (관측빈도-기대빈도)^2/기대빈도
#카이제곱 분포 : 카이제곱이 이루는 분포
#자유도(n-1)에 따라서 확률표가 달라짐
#dchisq-밀도함수의 위치(확률밀도함수), pchisq-누적분포함수, qchisq-확률에 대응하는 분위수(누적분포함수의 역함수), rchisq-임의 수들을 확률에 기준해서 선택

x<-seq(1,10,.1)
par(mfrow=c(2,3))

plot(x,dchisq(x,6),type = "l")
plot(x,dchisq(x,5),type = "l")
plot(x,dchisq(x,4),type = "l")
plot(x,dchisq(x,3),type = "l")
plot(x,dchisq(x,2),type = "l")
plot(x,dchisq(x,1),type = "l")

(d<-data.frame(x=c("1","2","2","1"),y=c("A","B","A","B"),num=c(3,5,8,7)))
class(d)
str(d)
table(d) #num값에 따라서 여러 테이블 생성
(xt=xtabs(num~x+y,data=d))
margin.table(xt,1) #행방향으로
margin.table(xt,2) #열방향로
margin.table(xt)   #전체 총합
prop.table(xt,1) #분포

#install.packages("gmodels")
library(gmodels)
str(mtcars)
#데이터 타입 == 범주형
#범주형 데이터의 기본 통계 : count
CrossTable(mtcars$vs,mtcars$am,expected=T)
14*13/32
#18,6 실제값, 0은 18개
#0과 1의 비율로
#18개에대한 비율로
#14개에대한 비율로
##
#실제값 : 
#카이제곱 : (관측도수-기대도수)^2/기대도수
#d.f. (2-1)*(2-1)=1
#카이제곱분포는 자유도에 따라서 분포함수가 다름
#p-value가 0.05보다 큰 0.34xx 나왔으니 있을 수 있음
#cell contents에서 n은 실제도수, expected n은 기대도수

library(MASS)
CrossTable(infert$education, infert$induced, expected = T)
fisher.test(infert$education, infert$induced, alternative = "two.sided")

#선호도에 차이가 있는지 검정하시오
#귀무가설 : 사이다에대한 선호도 차이가 없다
#대립가설 : 사이다에대한 선호도 차이가 있다
data<-textConnection(
  "사이다종류 관측도수
  1 14
  2 32
  3 17
  4 9
  5 18")
(x<-read.table(data,header=T))
(14+32+17+9+18)/5 #기대도수 : 18
res<-((14-18)^2+(32-18)^2+(17-18)^2+(9-18)^2+(18-18)^2)/18
dchisq(res,4)
pchisq(res,4)
1-pchisq(res,4)
chisq.test(x$관측도수) #자유도 : 4, p-value : 0.002603
#x-squared=16.333, df=4, p-value=0.002603
#귀무가설을 기각하고 대립가설을 채택한다 -> 사이다 제품 간 유의미한 선호도  차이가 있음

#문제 : 독립성 검정
data(survey)
str(survey)
#가설설정 : 성별과 운동량에는 별 차이가 있는지 알고싶다
#귀무가설 : 성별과 운동량은 관계가 없다.
#대립가설 : 성별과 운동량은 관계가 있다.
sum(is.na(survey))
survey<-na.omit(survey)
head(survey[c("Sex","Exer")])
(xt<-xtabs(~Sex+Exer,data=survey))
#카이제곱분석 : 독립성 검정, 동질성 검정
#기준 : 기대도수
CrossTable(survey$Sex, survey$Exer, expected = T)
chisq.test(survey$sex, survey$Exer)
#p-value가 0.05보다 크므로 귀무가설 기각 실패, =성별과 운동량은 관계가 없다


###
data<-read.csv(file.choose(),fileEncoding = "utf-8", encoding="cp949")
str(data)
data1<-data
#문제 : level, pass 간 관계를 알고 싶다
#부모의 학력과 자녀의 대학 진학 간 관계가 있는지 분석해보시오
#귀무가설 : 관계가 없다
#대립가설 : 관계가 있다
sum(is.na(data1))
data1<-na.omit(data)
head(data1[c("level","pass")])
CrossTable(data1$level, data1$pass, expected = T)
chisq.test(data1$level, data1$pass)
fisher.test(data1$level, data1$pass, alternative = "two.sided")

result<-data%>%
  select("level","pass")
sapply(result,function(x) sum(is.na(x)))
result<-na.omit(result)
CrossTable(x=result$level, y=result$pass, expected=T,)
chisq.test(x=result$level, y=result$pass)
#귀무가설 기각 실패. 부모의 학력과 자녀의 대학 진학간 관계는 없다

#평균검정
#단일 표본 평균검정
#귀무가설은 평균이 동일하다
#대립가설은 평균이 동일하지않다
x=c(65,78,88,55,48,95,66,57,79,81)
t.test(x,mu=75) #0.4537
mean(x) #71.2, 71.2==75(유의미한 차이가 나지 않으면 동일하다고 간주)


sleep
attach(sleep)
#수면제 약효
plot(extra~group,data=sleep)

#var.test(분산비 테스트)
var.test(extra[group==1],extra[group==2], data=sleep)
#평균비교
#var.equal(등분산)
#p-value : 0.07919
#두 집단은 유의미한 차이가 없다(귀무가설 기각 실패)
with(sleep,t.test(extra[group==1],extra[group==2],var.equal = T))
t.test(extra~group, data=sleep)
detach(sleep)


shapiro.test(rnorm(1000))
#정규분포 검정
#귀무가설 : 정규분포이다.
#대립가설 : 정규분포가 아니다.

with(iris,var.test(Sepal.Width, Sepal.Length))
#p-value : 3.595e-14
#귀무가설을 기각하고 대립가설을 채택, 고로 두 데이터는 등분산이다.


# 새로운 당뇨병 치료제를 개발한 제약사의 예
# 치료에 지대한 영향을 주는 외부요인을 통제하기 위해 10명의 당뇨병 환자를 선별하여
# 1달 동안 '위약(placebo)'을 투여한 기간의  혈당(x1)과
#동일한 사람에 대해 '신약(new medicine)'을 투여한 1달 기간 동안의 혈당 수치(x2)를 측정하여 짝을 이루어 혈당 차이를 유의수준 5%에서 비교하시요
x1 <- c(51.4, 58.0, 45.5, 55.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)

#평균 비교 : t.test(정규분포인 경우), wilcox.test(비모수인 경우)
#정규분포인지 테스트 : shapiro.test
#t.test에서 고려해야할 것
  #짝 데이터인지(pairs)
  #등분산인지(vat.test)
shapiro.test(x1) #p-value = 0.7659  #정규분포
shapiro.test(x2) #p-value = 0.04861 #정규분포가 아님
##정규분산이 아니라 원래는 이거로 가면 안됨
var.test(x1,x2)  #p-value = 0.2073  #등분산이다
t.test(x1,x2,alternative = c("greater"),
       paired=TRUE, #짝 데이터인 경우
       var.equal=TRUE, #등분산인 경우
       conf.level=0.95) #유의수준이 0.05인 경우
#p-value = 0.02987 유의미한 차이가 있음
##
wilcox.test(x1,x2, #p-value = 0.0124
            paired=TRUE,
            var.equal=TRUE,
            conf.level=0.95)
wilcox.test(x1,x2, #p-value = 0.0124
            paired=TRUE,
            var.equal=TRUE,
            exact=FALSE,
            conf.level=0.95)
#귀무가설을 기각하고 대립가설을 채택 : 유의미한 차이가 있음

#anova 분석(3개 이상의 변수인 경우) : F분포(평균비)
#bartlett.test(동질성 테스트) : 귀무가설 : 분산이 동질적이다
#스프레이 기긱에대한 효과 분석
#귀무가설 : 스프레이별로 효과 차이가 없다.
#대립가설 : 스프레이별로 효과 차이가 있다.
data(InsectSprays)
attach(InsectSprays)
str(InsectSprays) #6개의 스프레이 개수, count : 벌레를 죽인 횟수
#6개의 스프레이에 대해서 분산분석
detach(InsectSprays)
aov.out=aov(count~spray,data=InsectSprays)
summary(aov.out) #귀무가설을 기각하고 대립가설을 채택, 스프레이 간 효과차이가 있다
TukeyHSD(aov.out)
#F분포도 자유도에 영향을 받아서 자유도에 따라서 밀도함수의 모양이 바뀜

#문제 : Cars93 데이터를 이용하여 생산국이 USA vs non-USA(Origin) 두 그룹간의 price 평균 차이가 있는지 테스트하고 95%신뢰구간을 구하시오
#Origin변수와 price 변수간의 관계
#가설설정
#귀무가설 : USA와 non-USA 간 가격차이가 없다
#대립가설 : USA와 non-USA 간 가격차이가 있다
#연구환경
#유의분석(p-value)
#분석방법 : 평균비교 또는 비모수 평균 검정
#결과해석
Cars93
head(Cars93)
attach(Cars93)
usa_prices <- Cars93$Price[Cars93$Origin == "USA"]
non_usa_prices <- Cars93$Price[Cars93$Origin == "non-USA"]
var.test(usa_prices,non_usa_prices) #p-value = 0.01387
shapiro.test(usa_prices) #p-value = 0.7659
shapiro.test(non_usa_prices) #p-value = 0.04861
wilcox.test(usa_prices,non_usa_prices, #p-value = 0.0124
            paired=FALSE,
            var.equal=FALSE,
            exact=FALSE,
            conf.level=0.95)
##선생님
#shapiro.test 역할 : t.test로 할 것인지, wilcox.test로 할 것인지 결정
data("Cars93")
table(Cars93$Origin)
with(Cars93, tapply(Price,Origin,summary))
with(Cars93, tapply(Price,Origin,shapiro.test))
var.test(Price~Origin, data=Cars93)
wilcox.test(Price~Origin,
             data=Cars93,
             alternatice=c("two.sided"), #양측 검증/단측 검증 0.025 벗어나면 단측검증일때는 왼쪽이던지 오른쪽이던지 0.05를 벗어나면 기각역이됨
             var.equal=FALSE,
             exact=FALSE,
             conf.level=0.95)
