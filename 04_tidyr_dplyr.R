#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
#tibble : modern한 문법을 적용하기 위한 데이터 타입
df<-as_tibble(mtcars) #데이터 프레임 -> as_tibble
df
str(df)
class(df)
head(df)

#테이블, 사각형에 정의된 데이터
starwars #영화 출연자들 정보
class(starwars) #tbl(tibble)
glimpse(starwars) #str을 변경

#filter : 행선택(subset(조건에 맞는 것들 남김)과 유사)
starwars%>%
  filter(species=="Droid")

#select : 열선택
starwars%>%
  select(name,ends_with("color")) #ends_with(정규표현식 함수)

#mutate : 변수추가
starwars%>%
  mutate(name,bmi = mass/((height/100)^2))%>% #bmi라는 변수 추가
  select(name:mass,bmi)

#arrange : 정렬(default : 오름차순)
starwars%>%
  arrange((desc(mass))) 

#group_by, summarise(데이터요약)
#데이터 분석의 1단계(데이터 요약)
starwars%>%
  group_by(species)%>% #결과 : group_by한 변수, 집계함수만 나타남
  summarise( 
    n=n(), #개수 카운트
    mass=mean(mass,na.rm=TRUE)
  )%>%
  filter(
    n>1,
    mass>50
  )

#
data<-data.frame(x1=1:6,
                 x2=c(1,2,2,3,1,2),
                 x3=c("F","B","C","E","A","D"))
#문제
#x1, x2변수만 선택하시오
select(data,x1,x2)
#x1변수만 제외하고 출력하시오
select(data,-x1)
#x3를 중심으로 정렬해보시오
arrange(data,x3)
#x2가 2인 데이터만 출력하시오
filter(data,x2==2)
#x1과 x2를 더해서 x4변수를 만드시오
data<-mutate(data,x4=x1+x2)
#x4변수를 100으로 나눠서 x5변수를 만드시오
mutate(data,x5=x4/100)
#x4와 x2를 곱해서 x6변수를 만드시오
mutate(data,x6=x4*x2)
#x2그룹별로 x1의 평균을 구하고 구성원이 몇사람인지 알아보시오
data%>%
  group_by(x2)%>%
  summarise( 
    people=n(), #개수 카운트
    average_height=mean(x1,na.rm=TRUE))
#
grouped_data=group_by(data,x2)
summarise(grouped_data,average_height=mean(x1),people=n())

##########
str(mpg)
distinct(mpg,class)
best_in_class<-mpg%>%
  group_by(class)%>%
  filter(row_number(desc(hwy))==1) #데이터에 행번호 지정정
best_in_class
test<-mpg
test%<>% #입력과 출력 데이터 표현
  group_by(class)%>%
  filter(row_number(desc(hwy))==1) #row_number : 행번호 지정,row_number(desc(hwy)) : hwy를 내림차순하고 차례대로 행번호 지정
test

test2<-mpg #백업
#class 그룹별로 displ의 중위수와 hwy 고속도로 연비의 중위수를 구하시오
class_avg<-test2%>%
  group_by(class) %>%
  summarise(
    displ=median(displ),
    hwy=median(hwy)
  )
class_avg

mpg%>%
  filter(displ>=5, displ<=7, hwy>=10, hwy<=30) %>%
  ggplot(aes(displ,hwy))+ #미적요소 (x,y좌표) : 레이어어
  geom_point(aes(color=class))+ #클래스별로 컬러 지정
  geom_smooth() #비선형회귀
#비선형 회귀선 : 
#선형회귀 : 직선, 모든 데이터를 예측하는 y=ax+b(기울기, 절편)
#회귀분석 : 예측과 실제값의 차이를 가장 적게하는 직선의 방정식을 구하는 것
#차의 제곱의 합을 구해서 가장 적게하는 직선의 방정식을 구하는 것(더 나은 예측하기 위해)
#최소제곱법 : 제곱한 값의 합을 최소로하는 기울기와 절편
#비선형(과적합이 일어날 수 있음) : 선형(일반화)
#대표적인 비선형 회귀 : 딥러닝 
#점추정 : 틀릴 확률이 높아짐
#구간추정의 기준 : 표준오차
#95% 구간 추정 : 평균+-표준오차*2

#install.packages("hflights")
library(hflights)
head(hflights)
dim(hflights)
library(data.table)#dataframe을 사용->data.table(데이터베이스처럼 key가지고 있음)
hflights_df<-as_tibble(hflights)
arrange(hflights_df,Month,DayofMonth,desc(AirTime))
#도착지연 - 출발지연
mutate(hflights_df,gain=ArrDelay - DepDelay, gain_per_hour = gain/(AirTime/60))

dest_planes_flights<-hflights_df%>%
  group_by(Dest)%>% #목적지
  summarise(planes=n_distinct(TailNum),flights=n()) #비행기 종류별 수량, 비행 횟수

#년, 월, 일로 그룹핑해서 비행횟수 출력
hflights_df%>%
  group_by(Year,Month,DayofMonth)%>%
  summarize(number_flights=n()) %>% #비행횟수
  summarize(number_flights=sum(number_flights)) #비행횟수 집계

#문제
#1. Year, Month, DayofMonth 순서대로 그룹화하시오
hflights_df%>%
  group_by(Year,Month,DayofMonth)
#2. Year, DayofMonth, ArrDelay, DepDelay열 선택하여 출력하시오
select(hflights_df,Year, DayofMonth, ArrDelay, DepDelay)
#3. 평균 연착시간과 평균 출발 지연시간을 구하시오
mean(hflights_df$ArrDelay,na.rm=TRUE)
mean(hflights_df$DepDelay,na.rm=TRUE)
#4. 평균 연착시간과 평균 출발 지연시간이 30분이상인 데이터를 추출하시오
hflights_df%>%
  group_by(Year,Month,DayofMonth)%>%
  select(Year:DayofMonth, ArrDelay, DepDelay)%>%
  summarise(Arr=mean(ArrDelay,na.rm=TRUE),Dep=mean(DepDelay,na.rm=TRUE))%>%
  filter(Arr>30 | Dep>30)

#몇년간의 데이터인지 확인
unique(hflights_df$Year)
n_distinct(hflights_df$Year)
#몇개의 항공사(FlightNum) 의 몇개 비행기(TailNum)인가
unique(hflights_df$FlightNum)
n_distinct(hflights_df$FlightNum)
unique(hflights_df$TailNum)
n_distinct(hflights_df$TailNum)

#문제 : 평균 비행시간을 구하시오
summarise(hflights_df,AT=mean(AirTime,na.rm=TRUE))
mutate(hflights_df,cnt=n(),sum=sum(AirTime,na.rm = T),avg=sum/cnt)

#문제
#1. 다음 데이터를 데이터프레임으로 생성하시오
x<-c(101,102,103,104,105)
y<-c(350,280,200,300,1000)
z<-c(0.10,0.10,0.12,0.15,0.00)
(pay201801<-data.frame(empno=x,pay=y,bonus=z))
#2. total 열을 추가해 급여 + (급여*보너스)
(total=pay201801$pay+(pay201801$pay*pay201801$bonus))
pay201801=cbind(pay201801,total)
#3. dplyr을 이용해 총급여 300이상인 사원번호와 총급여를 출력하시오
pay201801%>%
  filter(total>=300)%>%
  select(empno,total)
#4. 다음 출력처럼 부서번호(deptno)를 추가하시오
deptno<-c(1,2,1,2,2)
pay201801=cbind(pay201801,deptno)
#5. 아래의 출력 내용처럼 부서별 급여 평균을 출력하시오
pay201801%>%
  group_by(deptno)%>%
  dplyr::summarise(
    mean_total=mean(total),sum_total=sum(total), count=n()
  )
#formula : 종속변수와 독립변수를 묶는 식으로 표현
aggregate(total~deptno,data=pay201801,sum) #deptno로 total을 집계하라
aggregate(total~deptno,data=pay201801,mean)
#tapply
tapply(pay201801$pay, pay201801$deptno,mean)
#6. x축을 empno y축을 pay로해서 geom_bar 그래프를 그리시오
#geom_bar(도수), geom_histogram(구간 범주화 후 도수), geom_col(x,y를 지정해서 출력력)
pay201801%>%
  ggplot(pay201801,aes(empno,pay,fill=empno))+geom_col()
 #ggplot(pay201801,aes(deptno))+geom_bar()
  #ggplot(pay201801,aes(pay))+geom_histogram(bins=30)

#데이터베이스
library(data.table)
#install.packages("RMySQL")
library(RMySQL)

con<-dbConnect(MySQL(),user="root",password="",dbname="mirae",host="localhost",port=3306)
dbListTables(con)

data("iris")    
IRIS<-data.table(iris)
dbWriteTable(con,"IRIS",IRIS,overwrite=TRUE)
(IRIS=dbReadTable(con,"IRIS"))
class(IRIS)
IRIS<-data.table(IRIS) #key부여할 수 있음(정렬)

setkey(IRIS,Species) #key부여할 수 있음(정렬)
tables() #data.table로 구성된 데이터가 몇개인가
IRIS[J("Setosa")]
IRIS[,sum(Sepal.Width),by="Species"] #종별로 그룹핑한 sepa.width의 합계계

#install.packages("PASWR")
library(PASWR)
data("titanic3")
titanic<-titanic3
class(titanic)
str(titanic)
titanic.dt<-data.table(data)
str(titanic.dt)
head(titanic.dt)

setkey(titanic.dt$pclass)
tables()
str(titanic)
titanic[pclass=1,]

titanic3
str(titanic3)

dbWriteTable(con,"mtcars",mtcars)
dbListTables(con)
str(mtcars)
res<-dbSendQuery(con,"SELECT*FROM mtcars where am = 0")
dbFetch(res)
dbClearResult(res)
#문제 : 실린더(cyl)가 4개 또는 6개인 데이터를 쿼리하시오
res<-dbSendQuery(con,"SELECT*FROM mtcars where cyl = 4 or cyl = 6")
dbFetch(res)
dbClearResult(res)

library(PASWR)
data("titanic3")
(titanic <- titanic3[, !names(titanic3) %in% c("home.dest", "boat", "body")] )
str(titanic)
head(titanic)
View(titanic)


#titanic$pclass <- as.factor(titanic$pclass)
titanic$name <- as.character(titanic$name)
titanic$ticket <- as.character(titanic$ticket)
titanic$cabin <- as.character(titanic$cabin)

#titanic$survived <- factor(titanic$survived, levels=c(0, 1), labels=c("dead", "survived"))
#titanic$pclass <- factor(titanic$pclass, levels=c("1","2","3"), labels=c("1st", "2nd", "3th"))
titanic
dbWriteTable(con,"titanic",titanic)

myData<-con%>%
  tbl("titanic")%>%
  select(pclass,sex,age,fare,survived,parch)%>%
  filter(survived==0)
myData

myData<-dbReadTable(con,"titanic")%>%
  select(pclass,sex,age,fare,survived,parch)%>%
  filter(survived==0)

#문제 : titanic 테이블에서 선실별(pclass) 남여(sex)로 그룹핑하고 survived의 평균을 출력하시오
myData<-titanic%>%
  select(pclass,sex,survived)%>%
  group_by(pclass,sex)%>%
  summarise(survival_ratio=mean(survived,rm.na=TRUE))
myData

ggplot(myData,aes(pclass,survival_ratio,color=sex,group=sex))+geom_point(size=3)+geom_line()

#formula
xtabs(~sex+pclass,data=titanic) #교차표

#문제
#mtcars 데이터 프레임을 mysql에 저장하고 다음 질문에 답하시오
dbWriteTable(con,"mtcars",mtcars)
dbListTables(con)
#1. 자동변속기인 자동차 중에 mpg,cyl,am을 쿼리하시오(am==0)
res<-dbSendQuery(con,"SELECT mpg,cyl,am FROM mtcars where am = 0")
res_am<-dbFetch(res)
dbClearResult(res)
res_am
#2. 연비(mpg)가 20보다 큰 자동차를 선택하고 mpg,cyl,am을 출력하시오
res<-dbSendQuery(con,"SELECT mpg,cyl,am FROM mtcars where mpg > 20")
res_mpg<-dbFetch(res)
dbClearResult(res)
res_mpg
#3. 실린더(cyl)가 4또는 6개인 자동차즐 중 mpg,cyl,am을 출력하시오
res<-dbSendQuery(con,"SELECT mpg,cyl,am FROM mtcars where cyl = 4 or cyl=6")
res_cyl<-dbFetch(res)
dbClearResult(res)
res_cyl
#4. 작업이 끝나면 mtcars테이블을 삭제하시오(dbRemoveTable)
dbWriteTable(con,"trial",res_cyl)
dbRemoveTable(con,"trial")
dbDisconnect(con) #DB 종료
