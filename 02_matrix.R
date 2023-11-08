#행렬의 종류
#정방행렬(행=열), 비정방행렬
#정방행렬 : 공분산행렬, 상관계수행렬, 거리행렬
#대각행렬 : 대각으로 값을 가지고 있는 행렬(크기값을 결정)
#대칭행렬 : 대각을 기준으로 상하의 값이 동일한 행렬(정방행렬일때만 발생)
#전치행렬 : 행과 열의 순서를 바꾼 행렬 #행렬의 거듭제곱(3x5행렬 %*% 3x5)(X)하고 싶을 때 전치행렬 사용
#단위행렬 : 행렬의 행렬곱 연산의 항등원
#역행렬   : 행렬의 나눗셈 역할(행렬곱을 하면 )
#직교행렬 : 행렬을 이루는 벡터간의 내적을 구하면 0이되는 행렬
#희소행렬 : 대다수의 데이터가 0인 행렬 -> 특징을 추출 후 연산해야함
#정방행렬&&대칭행렬인 경우 전치행렬은 역행렬
#정방행렬&&대칭행렬인 경우 고유값분해를 하면 고유치와 고유벡터 출력
#고유치 : 벡터방향으로의 크기를 의미
#고유벡터는 직교벡터로 이 직교벡터로 데이터를 재표현하면 noise가 없는 데이터로 변환(주성분 분석)

#행렬곱 연산
#행렬곱은 내적의 연속
#내적은 다차원을 차원축소해서 거리값과 방향을 고려한 하나의 값으로 차원축소하여 표현하는 것
#행렬곱은 이를 이용해서 행렬의 차원축소와 특징을 춫ㄹ하는 용도로 사용

(a=matrix(1:9, nrow=3, ncol=3, byrow=TRUE)) #byrow=TRUE 행 우선배치
matrix(1:9,nrow=3) #열이 부족하면 자동 계산, byrow=FALSE가 디폴트 값(열 우선배치가 디폴트) 
class(a)
#데이터가 부족하면 경고가 뜨지만 recycling됨
#(a=matrix(1:8, nrow=3, ncol=3, byrow=TRUE))
attributes(a)
dim(a)

x=matrix(1:9,nrow=3,byrow=TRUE)
colnames(x)
colnames(x)<-c("C1","C2","C3")
rownames(x)<-c("R1","R2","R3")
#x["C1"]
#x["R1"]

#list 사용 이유 : 행과 열의 개수가 다를 수 있기때문
(x<-matrix(1:9, nrow=3, dimnames=list(c("X","Y","Z"),c("A","B","C"))))

y<-matrix(nrow=2,ncol=2)
y[1,2]
y[1,1]<-1
y[1,2]<-2
y[2,1]<-3
y[2,2]<-4
mode(y)
class(y)
y
#matrix 결합
cbind(c(1,2,3),c(4,5,6)) #column
rbind(c(1,2,3),c(4,5,6)) #row

(x<-matrix(1:9, nrow=3, byrow=TRUE))
(y<-matrix(11:19, nrow=3, byrow=TRUE))
(c<-rbind(x,y))
(d<-cbind(x,y))

#indexing
(x=matrix(1:9,nrow=3,byrow=TRUE))
x[c(1,2),c(2,3)] # 1,2행출력 2,3열출력
x[c(2,3),] #2,3열 출력
x[,] #지정하지 않으면 전부 다 출력
x[-1,]
a=x[1,]
class(a)
mode(a)

(x=matrix(1:9,nrow=3,byrow=TRUE))
x[1:4] #1차원 인덱스로 입력되면 메트릭스를 1차원으로 보고 출력(열우선)
x[c(3,5,7)]
x
#boolean indexing
x[c(TRUE,FALSE,FALSE),c(TRUE,TRUE,FALSE)] #
x[c(TRUE,FALSE),c(2,3)]
x[c(TRUE,FALSE)] #1차원으로 보고 출력
x[x>5]
x[x%%2==0]


(x=matrix(1:9,nrow=3,ncol=3))
(x[2,2]<-10)
(x[x<5]<-0) #1~4까지 0으로 변경
print(x)
t(x)
x
(xt=t(x))
x%*%xt #행렬곱 : 앞의 행렬의 열수와 뒤의 행렬의 행수가 일치
       #행렬곱의 결과 : 앞의 행렬의 행수와 뒤의 행렬의 열수가 결정  
x%*%x

(x=matrix(1:12,nrow=3,ncol=4))
x*x #요소 연산
x%*%x #3x4 3x4(행렬 거듭제곱) 앞의 행과 뒤의 열이 달라서 계산불가능
x%*%t(x) #전치행렬을 이용하여 계산
xt<-t(x)
(result=x%*%xt)
#문제 : 결과값이 result[1,1] 값을 결정하는 수식을 작성하시오
x[1,1]*xt[1,1]+x[1,2]*xt[2,1]+x[1,3]*xt[3,1]+x[1,4]*xt[4,1]
sum(x[1,1:4]*xt[1:4,1])
sum(x[1,]*xt[,1])
#166 188 210
colSums(x[1,]*xt[,c(1,2,3)]) #1행
colSums(x[2,]*xt[,c(1,2,3)]) #2행
colSums(x[3,]*xt[,c(1,2,3)]) #3행

(mdat<-matrix(seq(20,4,-2),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("a","b","c"),c("x","y","z"))))
nrow(mdat)
ncol(mdat)
dim(mdat)

rowMeans(mdat)
rowSums(mdat)
colMeans(mdat)
colSums(mdat)

#대각에 있는 요소들만 추출
diag(mdat)
diag(diag(mdat)) #벡터를 주면 행렬로 출력 : 대각행렬 출력

#거듭제곱
(mdat2<-mdat%*%t(mdat)) #거듭제곱의 특징 : 정방행렬이면서 대칭행렬
#고유값 분해(eigenvalue decomposition)
(mdatEigen<-eigen(mdat2)) #values(고유치), vectors(고유벡터)
mode(eigen(mdat2))
mdatEigen$values  #고유치
mdatEigen$vectors #고유벡터

sqrt(sum(mdatEigen$vectors))

mdatEigen$vectors[1,]
mdatEigen$vectors[2,]
mdatEigen$vectors[3,]
#고유벡터는 직교벡터이다
#두 벡터는 내적을 구해서 0이되면 직교이다
mdatEigen$vectors[1,]%*%mdatEigen$vectors[2,]
mdatEigen$vectors[1,]%*%mdatEigen$vectors[3,]
mdatEigen$vectors[2,]%*%mdatEigen$vectors[3,]

mdatEigen$vectors[,]%*%mdatEigen$vectors[2,]
mdatEigen$vectors[,]%*%mdatEigen$vectors[3,]
mdatEigen$vectors[,]%*%mdatEigen$vectors[3,]

#특이행렬 분해(singular value decomposition:SVD
svd(mdat) # $d(차수 : 행중심), $u(고유치와 같은의미), $v(차수가 열중심)

#역행렬(solve(mat)) solve안에 변수가 두개 이상이면 역행렬이 아님?
solve(mdat)

#y=10x에서 y값이 20일때 x값은?
#solve(a,b) ax=b에서 x를 구하여 반환
solve(20,10)
solve(10,20)
#연립방정식의 해
#2x+3y=5  2,3 x 5
#3x+5y=6  3,5 y 6
(mdat<-matrix(c(2,3,3,5),nrow=2,ncol=2,byrow=TRUE))
(c=c(5,6))
solve(mdat,c) #mdat를 역행렬로 만들어서 c와 행렬곱

#문제
#2x+y+z=1       2,1,1 x  1 
#4x+3y+4z=2     4,3,4 y  2
#-4x+2y+2z=-6  -4,2,2 z -6
(mat<-matrix(c(2,1,1,4,3,4,-4,2,2),nrow=3,ncol=3,byrow=TRUE))
(y=c(1,2,-6))
solve(mat,y)

#문제 : 역행렬을 구해서 행렬곱으로 위의 연립방정식의 해를 구해보시오
(inv_m<-solve(mat))
mat%*%inv_m #결과는 단위행렬
inv_m%*%y

#영행렬(행렬의 덧셈의 항등원)
matrix(0,3,5) #3x5
(one<-matrix(1,5,5)) #일행렬
dia<-vec<-c(1,1,2,1,1) #scale 2배  
(dia2<-diag(dia)) #대각행렬
one%*%dia2
one*dia2

#MRS(rotate)
library(NISTunits)
vec<-c(1,1)
ang=NISTdegTOradian(720)
rot<-c(cos(ang),-sin(ang),sin(ang),cos(ang))
(mat=matrix(rot,nrow=2,ncol=2,byrow=T))

vec%*%mat

#문제 : z축으로 180도 회전시키시오
vec<-c(1,1,1)
ang=NISTdegTOradian(720)
rot<-c(cos(ang),-sin(ang),0,sin(ang),cos(ang),0,0,0,0)
(mat=matrix(rot,nrow=3,ncol=3,byrow=T))
vec%*%mat

#문제 
#1-4까지의 수를 2x2행렬로 만들고(열우선)
(x<-matrix(1:4,2,2,T))
#5-8까지의 수를 2x2행렬로 만든 다음(행우선)
(y<-matrix(5:8,2,2))
#두 행렬의 요소 연산(+-*/)를 시행하시오
(x+y)
(x-y)
(x*y)
(x/y)
(x^y)
#두 배열을 행과 열로 결합하시오
rbind(x,y) #4x2
cbind(x,y) #2x4
#x행렬에 대해 행합, 열합, 행평균, 열평균을 구하시오
rowSums(x)
colSums(x)
rowMeans(x)
colMeans(x)

apply(x,1,sum) #x에대해서 1(:행방향)으로 sum(행합)
apply(x,2,sum)
apply(x,1,mean)
apply(x,2,mean)
#sum은 함수지만 주소를 전달해주는 역할이라 sum()로 쓰면 안됨
#x행렬에 거듭제곱을 구하시오
x%*%t(x)

#data.frame : 열 내 동질적/열 간 이질적(열=vector)
#계산하지 않고 데이터만 다룰때는 data.frame을 가장 많이 사용
#vector,matrix에서는 데이터가 부족하면 recycling되지만 data.frame은 안됨
x<-c(10,20,30,40)
y<-c(6,7,8,9)
#열이름=데이터
data<-data.frame(width=x,height=y)
data
str(data) #data.frame 정보출력
data$width #$접근 가능(자바에서 멤버 변수나 멤버 함수를 참조할때 쓰는 .이랑 같은 의미=data.width)
data[,1] #인덱싱
head(data) #전체 데이터 값이 아닌 일부 값만 출력
data$deliner=c(30,40,50,60) #실시간 추가 가능

d.f<-data.frame()
d.f<-edit(d.f)
d.f

L3<-LETTTERS[1:3]
d<-data.frame(cbind(x=1,y=1:10),fac=sample('L3',10,replace = T))
d
d$fac
names(d)
(d$yhat<-d$fac) 
str(d)
head(d)
d$fac=factor(3$fac) #문자라서 안됨
d$fac=factor(d$fac) #범주형 데이터 : 양적 데이터/ 질적 데이터(크다, 작다, 아릅답다, 밉다)
                    #범주화
rownames(d)<-c("일","이","삼","사","오","","","","","","",)

x<-data.frame("SN"=1:2,"Age"=c(21,15),"Name"=c("에어컨","삼성SDS"))
str(x)
x<-data.frame("SN"=1:2,"Age"=c(21,15),"Name"=c("에어컨","삼성SDS"),stringAsFactors=FALSE)
x
x["Name"]
x$Name
class(x)
class(x$Name) #"character" #character vector
class(x["Name"]) #"character" 요소 하나로
x[["Name"]]
class(x[["Name"]])
colnames(x)<-c("SS","AA","CC")
ncol(x)
nrow(x)
length(x)
str(x)
x
x["Age"]
x["Age",1]<-20
x
x[1,"Age"]<-20
x
x$Age=NULL
x

data() #데이터들 모아져있음
str(trees) #나무가 성장할 때의 둘레(Girth), 높이(height), 부피(volume) 3가지 정보를 관찰
head(trees)
head(trees,n=3)
tail(trees,n=3)
trees[2:3,]
trees[trees$Height>82,]
trees[10:12,2]
trees[10:12,2,drop=FALSE]
class(trees[trre$Height>82,])
class(trees[10:12,2]) #numeric vector로 출력
class(trees[10:12,2,drop=FALSE]) #drop = FALSE 데이터 프래임 속성을 유지
summary(trees) #최솟값(0), 1사분위수(25), 중위수(50), 평균, 3사분위수(75), 최대값(100)
#사분위수 : 데이터를 정렬해서 25%씩 끊은 수(1사분위수 : 가운데 값을 기준으로 왼쪽의 절반)

boxplot(trees) #이상치 찾는것
#inter quantile range : 3사분위수에서 1사분위수
#이상치는 평균에 지대한 영향을 미침, 통계학은 평균 중심, 이상치가 없는게 중요
pairs(trees) #상관 관계를 알아보는 것
getwd() #저장소 위치 찾기
data<-read.table("input.csv",header=T,encoding="UTF-8",fill=TRUE,sep=",",quote="") #sep : ,로 데이터를 끊어라 / fill : 데이터가 비어있으면 NA로 채워라 / quote : ""안에 있으면 문자열
data
?read.table
colnames(data)
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))
(sal<-max(data$salary))

#데이터 필터링(subset)
#install.packages("stringr",dependencies = TRUE)
#library(stringr) #설치가 안되어있고 로딩도 되지 않은 라이브러리(함수나 클래스를 모아놓은 것)
retval<-subset(data,salary==max(salary)) #salary가 max인 사람
print(retval)
retval<-subset(data,str_trim(dept)=="HR") #str_trim : 앞 뒤 공백제거
retval<-subset(data,str_trim(dept)=="IT")
print(retval)

#문제
#salary가 600이상이고 dept가 finace인 것만 출력하시오
retval<-subset(data,salary>=600 & str_trim(dept)=="Finance")
retval

#날짜 : 입출력(문자열)
#날짜로 변경할때는 casting필요(as.Date)
retval<-subset(data,as.Date(start_date)>as.Date("2014-01-01"))
retval
write.csv(retval,"output.csv") #csv파일로 저장
(newdata<-read.csv("output.csv"))

#특정 행 제거(인덱스를 사용하여 특정 데이터빼고 출력 후 저장)
retval<-newdata[-2,] #열 변수 : NULL 제거

#문제 : X 제거하시오
retval$X=NULL
retval

#문제 : 다음 데이터를 데이터 프레임에 입력하시오
eng<-c(67,92,89)
grade<-c("C","A","B")
name<-c("퀴즈","중간","기말")
#열이름=데이터
(data<-data.frame("영어"=eng,"등급"=grade,row.names = name))
#문제2)수학점수를 50,100,80점으로 입력하시오(열로 추가) 실시간추가
data$"수학"=c(50,100,80)
data
#문제3)보충이라는 이름으로 dataframe을 만들어 rbind시키시오(행 추가) rbind
보충<-data.frame("영어점수"=90,"등급"="A","수학점수"=100,row.names = "보충")
(data=rbind(data,보충))
#문제4)열별합계를 계산해서 마지막 행에 추가하시오
hab<-colSums(as.matrix(cbind(data$영어,data$수학)))
da=data.frame("영어"=hab[1],"등급"=0,"수학"=hab[2],row.names = "합계")
(data<-rbind(data,da))
#저장
#write.csv로 저장하고 읽을때 row.names = 1을 안해주면 첫번째 행을 데이터로 취급
write.csv(data,file = "sungjuk.csv",sep = ",")
(data2<-read.table("sungjuk2.csv",header = TRUE,sep=","))
(data3<-read.table("sungjuk.csv",header = TRUE,sep=",",row.names = 1))
#write.table은 행이름이 정상적으로 저장
write.table(data,file = "sungjuk2.csv",sep = ",")
(data2<-read.table("sungjuk.csv",header = TRUE,sep=","))
