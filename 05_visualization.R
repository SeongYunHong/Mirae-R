#EDA(Exploratory Data Analysis)
#시각화의 목적 : 비교, 구성, 분포, 관계, 추세, 변동
#시각화 함수의 목적
#이산적 데이터 출력(barplot, stem) : 범주형 데이터 카운트
#연속적 데이터 출력(plot,boxplot,histogram) : 산포도
#모델의 결과 출력(qqnorm,paris,curve) 

data(cars)
str(cars) #차량속도, 제동거리 50x2 데이터
head(cars)

plot(cars$speed, type="l") #line(축을 표현하는 값 : 1,x축 index, y축 speed)
plot(cars$dist, type="l")

plot(cars, type="p") #point
plot(cars, type="b") #both(line+point)
plot(cars, type="b",pch=20)
plot(cars, type="l",pch="+",cex=2,col="red",xlab="speed",ylab="distance",xlim=c(10,20),ylim=c(10,40),lty="dotdash")
#보조 시각화 함수
identify(cars$speed, cars$dist)
text(cars$speed, cars$dist, pos=1)

coin<-c(2,2,0,1,1,1,2,3,1,2,2,3,3,4,1,3,2,3,3,1,2,2,1,2)
frequency<-coin
(frequency<-table(coin)) #범주->카운트(도수분포표)
#상대도수분포표
(relative<-frequency/length(coin))
sum(relative)
#누적도수분포표
(cumulative<-cumsum(relative))
#화면 분할
opar<-par(mfrow=c(1,4)) #1개의 행 4개의 열로 분할, 리턴값은 나누기전의 화면구조
plot(frequency,xlab="값",ylab="도수",type="b",col="red",main="도수",sub="순수도수",frame.plot=F,panel.first=T)
plot(1:5,frequency,xlab="값",ylab="도수",type="b",col="red",frame.plot=F)
plot(round(relative,2),type="b",pch=23,col="red")
plot(round(cumulative,2),type="b",col="red",axes=F) #축만 제외
par(opar)

opar<-par(mfrow=c(1,3))
barplot(frequency,xlab="값",ylab="도수",col="blue",border=TRUE,density=TRUE,horiz=TRUE,main="도수분포표") #density : 빗금, horiz : 옆으로 출력
barplot(round(relative,2),xlab="값",ylab="도수",col="red",main="상대도수분포표")
barplot(round(cumulative,2),xlab="값",ylab="도수",col="red",main="누적도수분포표")
par(opar)

par(mfrow=c(1,1))
x=1:720
library(NISTunits)
x<-NISTdegTOradian(x)
plot(x,sin(x), main="사인 함수", ylab="sin(x)", type="l", col="blue") #주파수별로 출력
plot(x,cos(X), main="코사인 함수", ylab="COS(x)", type="l", col="blue")
plot(x,tan(x), main="탄젠트 함수", ylab="tan(x)", type="l", col="blue")

library(MASS)
str(Boston)
Boston$tax
plot(density(Boston$tax, bw=5)) #보간법(interpolation)
#보간법 : 없는 값은 추정 spline(연속적인 선으로 표현)
#그래프 중복을 표현할 방법이 없음
#jitter 잡음 : 밀도 확인 표현
#정규분포
rug(Boston$tax+rnorm(length(Boston$tax),sd=5),col=2,lwd=3.5)

#패키지 간에 함수 이름이 중복되는 경우
x<-stats::runif(12); #균등분포
y<-stats::rnorm(12)
i<-order(x,y);x<-x[i];y<-y[i]
plot(x,y,main="arrows(.) and segments(.)")
(s<-seq(length(x)-1)) #맨 마지막은 화살표 그리지 않아서 -1
arrows(x[s],y[s],x[s+1],y[s+1],col=1:3) #x[s]에서 x[s+1]로 화살표
(s<-s[-length(s)])
segments(x[s],y[s],x[s+2],y[s+2],col='pink') #x[s]에서 x[s+2]로 선을 그음


(x=(0:100)*pi/10)
amp.1<-2
amp.2<-2
amp.3<-5
amp.4<-5
wav.1<-1
wav.2<-2
wav.3<-3
wav.4<-7
signal.1<-amp.1*sin(wav.1*x) #sin = -1~1 사이, 앞에 곱해지는것 :진폭, 주기가 빨라짐 : 고주파(분석 어려움)
signal.2<-amp.2*sin(wav.2*x) #amp를 곱하면 진폭이 커지게 됨
signal.3<-amp.3*sin(wav.3*x)
signal.4<-amp.4*sin(wav.4*x)

par(mfrow=c(1,4))
plot(x,signal.1,type="l",ylim = c(-5,5));abline(h=0,v=0,lty=3)
plot(x,signal.2,type="l",ylim = c(-5,5));abline(h=0,lty=3)
plot(x,signal.3,type="l",ylim = c(-5,5));abline(h=0,lty=3)
plot(x,signal.4,type="l",ylim = c(-5,5));abline(h=0,lty=3)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
str(cars)
plot(cars$dist, type="o",cex=0.5, xlab="speed", ylab="dist")
tapply(cars$dist, cars$speed, mean) #x축에 여러번 나타는 경우 평균값으로 표현
plot(tapply(cars$dist, cars$speed, mean), type="o", cex=0.5, xlab="speed",ylab="dist")
par(mfrow=c(1,1))

#install.packages("mlbench")
library(mlbench)
data(Ozone)
str(Ozone)
head(Ozone)
opar<-par(mfrow=c(1,2))
plot(Ozone$V6, Ozone$V7, xlab="Windspeed", ylab="Humidity",main="OZone")
plot(jitter(Ozone$V6), jitter(Ozone$V7), xlab="Windspeed", ylab="Humidity",main="OZone")
par(opar)

#선형회귀 그래프
#데이터 예측 선을 구하는것
#실제값과 예측값의 차이 발생
#+-상계를 방지하기 위해 제곱을 구한 다음 더한 값이 최소가 되는(최소제곱법) 직선을 구한는 것(기울기 + 절편)
#선형회귀의 결과는 점추정-> 구간 추정(표준오차)
#평균 +- 표준오차*2(95% 신뢰구간)

(m<-lm(dist~speed,data=cars)) #formula(~) dist : 종속, speed : 독립
#절편, 기울기
#dist=3.932 * speed - 17.579(y=ax+b)
#선형회귀는 예측을 하기 위해 일반화 진행
plot(cars)
abline(m) #abline : 직선이나 수직선 그리기
#구간 추정(점추정보다 정확)
(p<-predict(m,interval = "confidence")) #95% 신뢰구간으로 예측하여라 #predict하면 구간추정
head(p)

x<-c(cars$speed, tail(cars$speed,1), rev(cars$speed), cars$speed[1]) #rev : reverse
y<-c(p[,"lwr"],tail(p[,"upr"],1),rev(p[,"upr"]),p[,"lwr"][1])
polygon(x,y,col=rgb(.7,.7,.7,.5)) #투명도 조절
opar<-par(mfrow=c(1,1))
plot(cars)
lines(lowess(cars)) #비선형회귀

#문제 : women 데이터의 weight와 height 데이터 간의 선형회귀 선을 그리시오
str(women)
head(women)
(m<-lm(weight~height,data=women))
#weight=3.45*height-87.52
plot(women)
abline(m)

#3개의 변수를 가진 데이터(다중회귀)
str(trees)
summary(trees)
Height2<-trees$Height^2 #파생변수
trees2<-cbind(trees,Height2)
attach(trees2) #데이터를 마치 패키지처럼 메모리에 상주($안써도 됨)
(test2<-lm(Girth~Height+Height2))
#Girth=-0.133717*Height+0.002602*Height2+8.276598
plot(Girth~Height)
fitted(test2)
lines(sort(Height), fitted(test2)[order(Height)],col='red',type="l")
detach(trees2)

###
#norm : 벡터의 크기(선형대수), 정규분포(통계학)
x<-rnorm(1000,mean=100,sd=1) #rnorm(random+norm) : 정규분포
qqnorm(x) #정규분포
qqline(x,lty=2)

x<-runif(1000)
qqnorm(x)
qqline(x,lty=2)

#pairs : 상관계수(정방행렬/대칭행렬=>고유값 분해(eigen))를 확인하는 그림
#고유값(크기값) + 고유벡터(직교행렬) 방향값을 나타냄
#주성분 분석(PCA)은 상관 계수로부터 나옴
pairs(mtcars,main="mtcars 데이터")

data(iris)
head(iris)
x<-iris$Sepal.Length
hist(x,prob=TRUE, col="skyblue",border="white") #히스토그램(연속적인 수치 : 구간 범주화 후 카운트)
lines(density(x)) #없는 데이터는 보간법으로 추정 #보간법 종류 : 선형 보간법, spline 보간법
boxplot(x)

#벡터에 행 이름 부여여
honeymoon<-c(12,14,8,9)
names(honeymoon)<-c("하와이","동남아","유럽","기타")
pie(honeymoon, col=rainbow(4))
per<-round(100*honeymoon/sum(honeymoon),1)
lab<-paste(names(honeymoon),"\n",per,"%")
pie(honeymoon,labels=lab, col=rainbow(4))

text(-0.5,0,"32.6%",col="black")
text(0.3,0.3,"27.9%",col="black")
text(-0.3,-0.5,"18.6%",col="black")
text(0.5,-0.3,"20.9%",col="black")

library(ggplot2)
par(mfrow=c(1,1))
plot(mtcars$wt, mtcars$mpg) #차 무게, 연비 #역상관관계 #plot(산포도)

ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()

BOD
barplot(BOD$demand, names.arg = BOD$Time)

table(mtcars$cyl)
barplot(table(mtcars$cyl))

ggplot(BOD,aes(x=Time,y=demand))+geom_col()

curve(x^3-5*x, from=-4, to=4)

#배기량과 연비의 관계
qplot(displ,hwy,data = mpg)
#drv 4륜구동, 전륜구동, 후륜구동
qplot(displ,hwy,data = mpg,color=drv)

qplot(hwy,data = mpg,fill=drv)
#구간 범위 : 구간 개수가 줄어듬 binwidth
qplot(hwy,data = mpg,fill=drv,binwidth=2)
#facets(화면 분할)
qplot(hwy,data = mpg,fill=drv,binwidth=2,facets = .~drv,)
#qsec 가속력
qplot(wt,mpg,data=mtcars,size=qsec,color=factor(carb),shape=factor(cyl))

diamonds
qplot(clarity,data=diamonds) #도수분포
qplot(clarity,data=diamonds,fill=cut,geom="bar")

ggplot(mpg,aes(x=hwy, fill=drv))+geom_bar()

g<-ggplot(mpg,aes(class)) #변수에 저장
g+geom_bar()
g+geom_bar(aes(weight=displ))
g+geom_bar(aes(fill=drv))

g+geom_bar(aes(fill=drv),position=position_stack(reverse=FALSE))+
  coord_flip()+ #좌표계 전환
  theme(legend.position = "top") #범례 위치조정
#ggtitle("plot of length \nby dose")
labs(title="연비",x="mpg",y="테스트")
ggplot(mpg,aes(x=hwy,y=cty))+geom_point()+ylab('cty')+xlab('hwy')
ggplot(mpg,aes(x=hwy,y=cty))+geom_point(position=position_jitter(w=0.2,h=0.2))+geom_smooth(method="lm")

ggplot(data=diamonds, aes(x=price))+geom_histogram(binwidth = 3000)+facet_wrap(~cut,nrow=3) #화면분할

x_mean<-1.5
x_sd<-1.2
N<-500
#substitute : 대체제, paste : 문자열을 결합, mu : 모집단의 평균, sigma : 표준편차, bar(x) : 표본 평균, s^2 : 표본 분산
#통계학 : 모집단(비용, 시간)/표본
#모집단의 평균 : 추정, 분산, 표준편차
#문자열 내에서 표현되는 수학기호 expression
n<-ggplot(data.frame(x<-rnorm(N,x_mean,x_sd)),aes(x=x))+
  geom_bar()+stat_bin(binwidth = 0.5)+
  labs(title=substitute(paste("Random 데이터 출력",
                              mu,"=",m,",",
                              sigma^2,"=",s2,",",
                              "draws=",numdraws,",",
                              bar(x),"=",xbar,",",
                              s^2,"=",sde),
                        list(m=x_mean,xbar=mean(x),s2=x_sd^2,sde=var(x),numdraws=N)))
print(n)

ggplot(mtcars, aes(mpg,wt))+geom_point()+xlim(15,20)

x<-seq(-4,4,len=101)
y<-cbind(sin(x),cos(x))
matplot(x,y,type="l",xaxt="n",
        main=expression(paste(plain(sin)*phi," and ",
                              plain(cos)*phi)),
        ylab=expression("sin"*phi,"cos"*phi),
        xlab=expression(paste("Phase Angle "),phi),
        col.main="blue")

#install.packages("maps")
library(maps)

asia<-map_data("world",region = c("China","Japen","North Korea","South Korea","India"))
str(asia)
asia<-map_data("world",region=c("North Korea","South Korea"))
ggplot(asia,aes(x=long,y=lat,group=group,fill=region))+geom_polygon(color="black")+scale_fill_brewer(palette="Set1")


#문제 : coin 데이터를 도수 분포표, 상대도수 분포표, 누적도수 분포표를 하나의 그래픽으로 그리시오
library(ggplot2)
coin<-c(2,2,0,1,1,1,2,2,3,4,1,3,2,3,3,1,2,2,1,2,2)

(frequency<-table(coin)) #범주->카운트(도수분포표)
frequency
#상대도수분포표
(relative<-frequency/length(coin))
sum(relative)
#누적도수분포표
(cumulative<-cumsum(relative))
#coin<-data.frame(frequency,relative,cumulative)
ggplot()+geom_line(aes(x=coin),y=frequency, color="blue")+
  geom_line(aes(x=round(relative,2)),y=frequency,color="green")+
  geom_line(aes(x=round(cumulative,2)),y=frequency,color="red")


#문제
# 후원자와 후원금에 대한 정리
# 후원자의 직업군, 후원금 규모, 고액 후원자, 후원자수의 비교
# 다양한 비교및 처리를 통해서 정보를 추출하는 것이 중요하다.
# 고액 후원자의 직업 성향으로 당의 특징을 파악해 보시요 ,

# 문제 election_2012.csv 파일을 로딩한 다음 다음 문제를 해결하시요
election<-read.csv(file.choose())
str(election)
# 문제1) 다음 데이터만 선택하여 작업하도록 data.frame을 준비하시요
#1. cand_id : 2. cand_nm : 대선 후보자 이름 , 3 . contbr_nm : 후원자 이름 4. contbr_occupation : 후원자 직업군 , 10. contb_receipt_amt: 후원금
cand<-data.frame(election$cand_id,election$cand_nm,election$contbr_nm,election$contbr_occupation,election$contb_receipt_amt)

# 문제2) 'Romney, Mitt'와 'Obama, Barack' 대령통 후보자 별로 서브셋(subset)을 생성하시요
#조건1) romney와 obama으로 저장
romney<-subset(cand,election.cand_nm=="Romney, Mitt")
obama<-subset(cand,election.cand_nm=="Obama, Barack")
#조건2) 각 후보자의 데이터 수를 확인하고 그 내용을 head로 확인하시요
NROW(romney)
NROW(obama)
head(romney)
head(obama)

# 문제3) romney와 obama 변수를 대상으로 후원금이 600달러 이상인 후원자를 추출하고 다음을 처리하시요
# <조건1> 추출된 결과를 다음 변수에 저장하시요. romney_6000over, obama_6000over
romney%>%
  filter(election.contb_receipt_amt>=600)
obama%>%
  filter(election.contb_receipt_amt>=600)
# <조건2> 각 후보자별로 후원자 수를 파악하시요  ( 중복한 경우는 제거하고 )
NROW(distinct(romney,election.contbr_nm))
NROW(distinct(obama,election.contbr_nm))
# <조건3> 각 후보자별로 가장 많은 후원금의 기부자의 이름과 직업군은 무엇입니까
select(subset(romney,election.contb_receipt_amt==max(election.contb_receipt_amt)),election.contbr_nm,election.contbr_occupation)
select(subset(obama,election.contb_receipt_amt==max(election.contb_receipt_amt)),election.contbr_nm,election.contbr_occupation)

# 문제4) romney와 obama 변수를 대상으로 직업군이 공백인 관측치를 제거하여 서브셋을 생성하시요
# <조건1> romney2, obama2 변수 저장
romney2<-romney
obama2<-obama
# <조건2> 공백 제거 전과 후 관측치 차이 계산
Romney2<-romney2%>%
  group_by(election.contbr_occupation)%>%
  filter(election.contbr_occupation!="")%>%
  summarise( 
    n=n())

Obama2<-obama2%>%
  group_by(election.contbr_occupation)%>%
  filter(election.contbr_occupation!="")%>%
  summarise( 
    count=n())
#romney2=ifelse(romney2$election.contbr_occupation=="",NA,romney2$election.contbr_occupation)
#obama2=ifelse(obama2$election.contbr_occupation=="",NA,romney2$election.contbr_occupation)
#romney2<-na.omit(romney2)
#obama2<-na.omit(obama2)

NROW(romney2)
NROW(Romney2)
NROW(obama2)
NROW(Obama2)
# <조건3> romney2와 obama2 변수를 romney.csv와 obama.csv 파일 저장(행번호 제외)
write.table(romney2,file = "romney.csv",sep = ",")
write.table(obama2,file = "obama.csv",sep = ",")

# 문제 5) romney.csv, obama.csv 파일을 읽어와서 다음과 같이 처리하시오.
# <조건1> 저장할 변수명 : romney3, obama3
(romney3<-read.table("romney.csv",header = TRUE,sep=","))
(obama3<-read.table("obama.csv",header = TRUE,sep=","))
# <조건2> 후보자별 직업군이 'RETIRED'인 후원금만 추출하여 합계 계산
romney3%>%
  filter(election.contbr_occupation=="RETIRED")%>%
  mutate(total=sum(election.contb_receipt_amt))%>%
  summarise(sum(election.contb_receipt_amt))

obama3%>%
  filter(election.contbr_occupation=="RETIRED")%>%
  mutate(total=sum(election.contb_receipt_amt))%>%
  summarise(sum(election.contb_receipt_amt))


# 문제 6) romney3, obama3 변수를 대상으로 각 후보자별 가장 많은 후원자의 직업군 3개씩 확인하시오
romney3%>%
  group_by(election.contbr_occupation)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(3)

obama3%>%
  group_by(election.contbr_occupation)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(3)
