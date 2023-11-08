(x<-1:10)
rnorm(100,mean=5,sd=3) #rnrom : random + norm(정규분포) #랜덤으로 정규분포 100개를 만들어라 
(x=rnorm(100)) #표준정규분포(평균 : 0, 표준편차 : 1) 무한대까지 생성
(su<-sum(x))
#평균
mean(x) #=su/100
#분산
var(x) #=sum((x-mean(x))^2)/(100-1)
varresult=sum((x-mean(x))^2)/(100-1) #100-1 : 자유도
#표준편차
sd(x) #=sqrt(varresult)
sdresult=sd(x)
#표준오차
sdresult/sqrt(100)
#변동계수
sdresult/mean(x)
#범위
range(x)
diff(range(x))
#IQR(Inter Quantile Range)
IQR(x) #3사분위수-1사분위수
quantile(x,0.75)-quantile(x,0.25)
#요약
summary(x) #min 1사분위수 중위값 3사분위수 max
boxplot(x)

#균등분포(uniform) : 확률이 동일
uni<-runif(100)
max(uni)
min(uni)
prod(uni)
order(uni) #인덱스 순서
table(uni) #도수분포표 #uni는 균등분포라 하나씩만 있음
table(x) #소수점이 있어서 하나씩만 출력됨, 소수점을 없애고 정수로 출력하려면 구간값을 설정해주면 됨

abs(-5)
log(10) #자연로그
log10(10)
#올림 버림 반올림
(x<-c(1.5,2.5,-1.3,2.5))
ceiling(mean(x))
floor(mean(x))
round(mean(x))
#범주화
x<--5:5
cut(x,breaks=2) #cut : 구간 범주화 #2개로 나누라해서 0,1 두개로 나눔(개수로 나눔(정분))
cut(x,breaks=c(-6,2,5)) #범위로 나눔(-6에서 2까지,2에서 5까지)
x<-c(12,1,25,12,65,2,6,17)
result<-cut(x,breaks=c(0,3,12,15,20,80),labels=c("First","Second","Third","Fourth","Fifth"))
table(result)

library(MASS)
str(Cars93) #93 obs. of  27 variables(93x27의 dataframe)
#히스토그램(자동으로 구간 범주화+table(도수분포표))->시각화=히스토그램
#연속된 수에서만 사용가능
hist(Cars93$MPG.highway) #MPG.highway : 고속도로에서의 연비
disc_1<-Cars93[,c("Model","MPG.highway")] #두개만 따로 빼서 저장
head(disc_1)

within(Cars93,{MPG.highway>=20&MPG.highway<25}) #within(a,{b,c,d}) : a$b a$c a$d와 같은 의미를 가짐
range(disc_1["MPG.highway"])
disc_1<-within(disc_1,{
  MPG.highway_cd = character(0) #변수생성
  MPG.highway_cd[MPG.highway>=20 & MPG.highway<25] = "20~25"
  MPG.highway_cd[MPG.highway>=25 & MPG.highway<30] = "25~30"
  MPG.highway_cd[MPG.highway>=30 & MPG.highway<35] = "30~35"
  MPG.highway_cd[MPG.highway>=35 & MPG.highway<40] = "35~40"
  MPG.highway_cd[MPG.highway>=40 & MPG.highway<45] = "40~45"
  MPG.highway_cd[MPG.highway>=45 & MPG.highway<50] = "45~50"
  MPG.highway_cd=factor(MPG.highway_cd,level=c("20~25","25~30","30~35","35~40","40~45","45~50"))})
str(disc_1)
head(disc_1)
attributes(disc_1$MPG.highway_cd)
table(disc_1$MPG.highway_cd)
#apply(a,b,c)  a : 데이터, b : 1(행),2(열), c : 함수
#tapply(a,b,c) a : 데이터, b : , c : 함수
tapply(disc_1$MPG.highway,disc_1$MPG.highway_cd,sum) #6개의 범주를 기준으로 MPG.highway의 합계 출력
tapply(disc_1$MPG.highway,disc_1$MPG.highway_cd,mean)
tapply(disc_1$MPG.highway,disc_1$MPG.highway_cd,sd)

#install.packages("ggplot2")
library(ggplot2) #시각화 패키지(layer로 나눠져있음)
#dev.off()
#ggplot으로 기본 구성 세팅,geom_dotplot(점 모양)으로 모양 결정
ggplot(disc_1,aes(x=MPG.highway_cd, fill=MPG.highway_cd))+geom_dotplot(binwidth = 0.3) #binwidth : 간격
#ggplot으로 기본 구성 세팅,geom_bar으로 모양 결정
ggplot(disc_1,aes(x=MPG.highway_cd, fill=MPG.highway_cd))+geom_bar()

#4분위수 범주화
disc_1 <- within(disc_1,{
  MPG.highway_cd2 = character(0)
  MPG.highway_cd2[ MPG.highway <  quantile(MPG.highway, 0.25) ] = "1Q"
  MPG.highway_cd2[ MPG.highway >= quantile(MPG.highway, 0.25) & MPG.highway < quantile(MPG.highway, 0.50) ] = "2Q"
  MPG.highway_cd2[ MPG.highway >= quantile(MPG.highway, 0.50) & MPG.highway < quantile(MPG.highway, 0.75) ] = "3Q"
  MPG.highway_cd2[ MPG.highway >= quantile(MPG.highway, 0.75) ] = "4Q"
  MPG.highway_cd2 = factor(MPG.highway_cd2,level = c("1Q", "2Q", "3Q", "4Q"))})
head(disc_1)
(table(disc_1$MPG.highway_cd2))
tapply(disc_1$MPG.highway,disc_1$MPG.highway_cd2,mean)

#개수로 범주화 : 연비 데이터를 기준으로 정렬하고 순서대로 번호를 매긴 다음 4구간으로 범주화하시오
(disc_1<-disc_1[order(disc_1$MPG.highway), ])
disc_1$N<-seq(1:length(disc_1$MPG.highway))

disc_1<-within(disc_1,{
  MPG.highway_cd3 = character(0)
  MPG.highway_cd3[N<=23] = "1st_Freq"
  MPG.highway_cd3[N>=24 & N<=46] = "2nd_Freq"
  MPG.highway_cd3[N>=47 & N<=69] = "3rd_Freq"
  MPG.highway_cd3[N>=70] = "4th_Freq"
  MPG.highway_cd3 = factor(MPG.highway_cd3,level=c("1st_Freq","2nd_Freq","3rd_Freq","4th_Freq"))
})
head(disc_1)

#one-hot-encoding 범주형의 정규화(신경만 반드시 실행)
#정규화 : 종속변수를 동일한 영향을 가진 독립변수로 변화
#범주형 : 범주 5개일때 0.2 0.4 0.6 0.8 1.0
Quter<-c("E1","E2","E3","E4","E1","E2","E3","E4")
Productcount<-c(30,80,40,10,28,75,39,6)
TS<-data.frame(Quter,Productcount,stringsAsFactors = F)
TS<-transform(TS,
              Quter1 = ifelse(Quter=="E1",1,0), #3항 연산자 ifelse(a,b,c) a : 조건 b : 참일 경우 c : 거짓일 경우
              Quter2 = ifelse(Quter=="E2",1,0),
              Quter3 = ifelse(Quter=="E3",1,0),
              Quter4 = ifelse(Quter=="E4",1,0))
TS

#정규화
min_max_norm<-function(x){ #vector
  (x-min(x))/(max(x)-min(x))
}
#나팔꽃, 꽃잎, 꽃받침을 이용해서 종을 분류
head(iris)  
#3가지 종(범주형)
str(iris) #종마다 50개
lapply(iris[1:4],min_max_norm) #자동 열방향 처리
#객체 지향 프로그래밍이지만 클래스를 함수형 프로그래밍으로 바꿔서 클래스 없음(객체 지향 프로그래밍을 구조적 프로그래밍처럼 사용)
iris[1:4]
iris_norm<-as.data.frame(lapply(iris[1:4],min_max_norm)) #데이터 프래임으로 바꿈
head(iris_norm)

#Z점수 정규화(z=(x-mu(평균))/sigma(표준편차)) (표준정규분포에서 찾으면됨)
head(iris)
iris$Sepal.Length<-(iris$Sepal.Length-mean(iris$Sepal.Length))/sd(iris$Sepal.Length)

z_normal<-function(x){ #기본적으로 vector
  (x-mean(x))/sd(x)
}
iris_z_norm<-as.data.frame(lapply(iris[1:4],z_normal))
iris_z_norm

iris_standardize<-as.data.frame(scale(iris[1:4]))

summary(iris)
str(iris)
head(iris)
tail(iris)
dim(iris)
length(iris) #length(벡터) : 행, length(데이터프레임) : 열
NROW(iris)
names(iris)
class(iris)
sapply(iris,class) #열별로 character(문자) 벡터로 출력
mode(sapply(iris,class))
boxplot(iris)

ir<-iris
head(ir)
IQR(ir$Sepal.Width)
mean(ir$Sepal.Width)
(ir$Sepal.Width=ifelse(ir$Sepal.Width>1,ir$Sepal.Width,NA))
is.na(ir[,2]) #결측치 확인함수

#install.packages("dplyr")
library(dplyr)

#ir%>%filter(!is.na(ir))
summary(iris[,1])
fivenum(iris[,1],na.rm = TRUE) #minimum lower-hinge median upper-hinge maximum

(outdata<-iris[1:4])
for(i in 1:(ncol(outdata)-1)){ #자바의 for in 문과 동일
  uppercut=fivenum(outdata[,i],na.rm = T)[4]+1.5*IQR(outdata[,i],na.rm = T) #상한치
  lowercut=fivenum(outdata[,i],na.rm = T)[2]-1.5*IQR(outdata[,i],na.rm = T) #하한치
  out<-filter(outdata,outdata[,i]<=uppercut, outdata[,i]>=lowercut)
}
str(out)
#install.packages("forecast")
library(forecast)

head(mtcars)
str(mtcars) #32x11(변수 개수 : 11)
(exdata<-mtcars[1:10,1:3])
class(exdata)

#문제
#apply를 이용해서 mtcars에서 추출된 exdata의 열별 합계를 구하시오
apply(exdata,2,sum)
#tapply를 이용해서 exdata의 실린더별 연비의 평균 분산 표준편차를 구하시오
(datamean<-tapply(exdata$mpg, exdata$cyl,mean))
(datavar<-tapply(exdata$mpg, exdata$cyl,var))
(datasd<-tapply(exdata$mpg, exdata$cyl,sd))
(result<-rbind(datamean,datavar,datasd))
rownames(result)<-c("평균","분산","표준편차")
result
#각 열의 클래스 타입을 확인하시오
apply(exdata,2,class)
lapply(exdata,class)
sapply(exdata,class)
class(apply(exdata,2,class))
class(lapply(exdata,class))
class(sapply(exdata,class))

(mat<-matrix(rnorm(20),nrow=5,ncol=4))

#문제 : 행으로 1사분위수와 중위수 3사분위수를 출력하시오
apply(mat,1,quantile, probs=c(0.25,0.5,0.75))

########################################################
state_table <-
  data.frame( key=c("SE", "DJ", "DG", "SH", "QD"),
              name=c(" 서울", "대전", "대구", "상해", "칭따오"),
              country=c("한국", "한국", "한국", "중국", "중국"))
state_table
# 년도
month_table <-
  data.frame(key=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
month_table
# 상품 테이블
prod_table <-data.frame(key=c("Printer", "Tablet", "Laptop"), price=c(225, 570, 1120))
prod_table
(prod_table1 <- data.frame(Printer=225,Tablet=570, Laptop=1120 ))

(prod_table1 <-t(prod_table1)) #t() : 전치행렬

prod_table
names(prod_table)
row.names(prod_table)

row.names(prod_table)<-c("Printer", "Tablet", "Laptop")
prod_table["Tablet",]
prod_table[prod_table["key"]=="Tablet"][2]
prod <- sample(prod_table$key, 5, replace=T, prob=c(1, 3, 2))
prod
class(prod)

gen_sales <- function(no_of_recs) {
  loc <- sample(state_table$key, no_of_recs, replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2012, 2013), no_of_recs, replace=T)
  prod <- sample(prod_table$key, no_of_recs, replace=T, prob=c(1, 3, 2))
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3))
  
  amount <- unit*prod_table1[prod,1]
  
  
  
  sales <- data.frame(month=time_month,
                      year=time_year,
                      loc=loc,
                      prod=prod,
                      unit=unit,
                      amount=amount)
  
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

sales_fact<-gen_sales(500)
sales_fact
str(sales_fact)
head(sales_fact)
tail(sales_fact)

(revenue_cube<-
    tapply(sales_fact$amount,sales_fact[,c("prod","month","year","loc")],
           FUN=function(x){return(sum(x))}))

tapply(sales_fact$amount,sales_fact[,c("prod")],
      FUN=function(x){return(sum(x))})
tapply(sales_fact$amount,sales_fact[,c("month")],
      FUN=function(x){return(sum(x))})
tapply(sales_fact$amount,sales_fact[,c("year")],
      FUN=function(x){return(sum(x))})
tapply(sales_fact$amount,sales_fact[,c("loc")],
      FUN=function(x){return(sum(x))})

scube<-revenue_cube[c("Tablet","laptop",' ' )]
scube
revenue_cube["Tablet","1","2012",]#dicing
revenue_cube[,"3","2013",] #slicing
revenue_cube
#roll-up
apply(revenue_cube,c("year","prod"),FUN=function(x){return(sum(x,na.rm = TRUE))})
apply(revenue_cube,c("prod","year"),FUN=function(x){return(sum(x,na.rm = TRUE))})

#문제
#제품별로 년도별, 월별 매출액을 확인하시오
apply(revenue_cube,c("year","month","prod"),FUN=function(x){return(sum(x,na.rm = TRUE))})
#매장별 제품별로 월별 매출액을 확인하시오
apply(revenue_cube,c("loc","prod","month"),FUN=function(x){return(sum(x,na.rm = TRUE))})
