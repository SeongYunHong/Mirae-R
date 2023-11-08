(a <-2);
(b <-5);
ls() #linux에서 파일리스트 출력 #R에서는 변수 리스트 출력
Sys.getenv("JAVA_HOME"); #자바도 기본저긍로 설치하고 실행할 것
print(a);

x<-5;
typeof(x) # double로 강제 형변환
mode(x) #문자, 숫자, 불린 구분
class(x) #만들어진 클래스 종류

#데이터 타입을 변경하고 싶다(as.integer)
x = as.integer(x); #자바에서는 클래스 내부 참조할 때 .을 씀
                   #R은 .이 그냥 함수 이름  
mode(x)
typeof(x)
#객체 지향 프로그램 -> 함수형 프로그래밍 구조적 프로그래밍으로 생성
y<-x; #값에 의해서 대입
y

x+y
x*y
x-y
x/y
y%%x #나머지 연산자
y%/%x #몫 연산자자
y^x #y의 x 제곱

x<-5;
y<-16;
x>y;
x<y;
x<=5
y==16
x!=5

#R에서는 TRUE,FALSE 둘다 대문자
#R은 대소문자 구분함
x<-c(TRUE,FALSE,0,6) #c=combination
x

#문자 -> 숫자 -> boolean
x<-5
x+1 #R에서는 대입으로 값을 변경(x+1해도 값이 안변함)
x

(x<-c(TRUE,FALSE,0,6))
(y<-c(FALSE,TRUE,FALSE,TRUE))

!x #부정(!) (0=FALSE,1보다 큰 수 TRUE)
x&y #각 요소별로 참 거짓 판단
x&&y #전체가 참일때 참

t<-1:10 #range연산자(:)
t
v1<-8
v2<-15
v1 %in% t #포함연산자(%in%)
v2 %in% t

#벡터 연산은 벡터화 연산을 수행 : core가 6개 부동소수점 연산기
#matrix도 벡터 연산 수행
#dataframe : 벡터가 열로 적용되어 있음
#multi core를 사용하여 동시에 연산() : 병렬연산
c(1,3,5,7,9) * 2 #각각 요소별로 적용됨(굳이 for문 필요없음)
c(1,3,5,7,9,11) * c(2,4) #짝이 맞아야 연산 가능#(1*2),(3*4),(5*2)....
c(2,4) * c(1,3,5,7,9,11)

#내장 함수
factorial(1:5) #팩토리얼
exp(2) #지수함수
exp(2:10)
cos(c(0,pi/4)) #cos0,cosㅠ/4
sqrt(c(1,4,9,16))

#Inf,NA,NaN
sum(c(1,2,4,3))
sum(c(1,2,NA,3)) #NA : 결측치
1/0 #Inf : 무한대(정의되지 않은)
0/0 #NaN : 숫자가 아니다
Inf/NaN #NaN
Inf/Inf #NaN
log(Inf) #Inf
Inf+NaN #NaN

vec<-c(0,Inf,NaN,NA)
typeof(vec) #double
mode(vec) #numeric
class(vec) #numeric

#체크(is)
is.finite(vec) #유한한지 확인
is.nan(vec)
is.na(vec)
is.infinite(vec)
sum(vec)

#연산자 정의
#데이터 타입을 지정하지 않기 때문에 함수의 리턴 타입이 없음
#매개변수의 데이터타입도 없음
#R의 함수는 1급함수 
`%divisible%`<-function(x,y) #나머지가 0이면 true반환하는 함수
{
  if(x%%y==0) return(TRUE)
  else        return(FALSE)
}
10%divisible%3
10%divisible%2
`%divisible%`(10,5)

#벡터 연산
#중요한 연산 (벡터 : 내적, 행렬 : 행렬곱)
#내적 : 두 벡터의 요소끼리 곱한 다음에 더한 결과값
#내적의 결과값의 의미 : 한 벡터의 크기*한 벡터의 크기*cosΘ의 의미
#행렬곱 : 내적 연산의 연속

#cosΘ는 AB내적/|A||B|
#|A| : A벡터의 크기
#Θ는 acos(AB내적|A||B|)
#내적 자체는 각도가 아니고 내적의 크기 값이 고려된 사이값
#벡터의 거리 : 원점으로부터의 거리값

x<-c(5,6,7)
y<-c(1,2,3)
x%%y #나머지 연산자
x%*%y #벡터의 내적 (5*1)+(6*2)+(7*3)
sum(x*y) #= x%*%y(내적)
x%/%y #몫 연산자

#R에서 사용하는 상수
LETTERS #대문자 알파벳
letters #소문자 알파벳
month.name #달 이름
month.abb #약어(add)
pi
class(month.name) #문자열

#벡터 : 열 중심(검색보다 통계량(합계, 평균, 분산, 표준편차, 표준오차)이 중요함)
a<-c('apple','orange','banana')
mode(a)
class(a)
length(a)
NROW(a) #행의 개수
NCOL(a) #열의 개수

#배열의 인덱스는 1부터 시작(추가 가능)
x<-c(1,5.4,TRUE,"hello") #문자열이 하나 있어서 전부 문자열로 강제 형변환
x[5]=123
x
x[200]=100 #중간 값들을 전부 NA로 처리
x

x<-c(1,5.4,TRUE) #boolean보다 숫자가 더 큰 범위
x
(x=vector("list",10))
(x=vector("numeric",10))
rm(x) #메모리에서 변수 삭제(공간 확보가 중요)
x

#연속적인 숫자 표현
(x<-1:7) #범위는 정수로만 지정해줘야함
(y<-2:-2)

#sequence
seq(1,3.2,by=0.2) #by=n : n만큼 증가
seq(1,5,length.out=4) #length.out=n : n개만 출력
seq(1,6,length.out=4)

#repeatation
(b<-rep(1:4,2)) #
(d<-rep(1:3, each=3)) 
(d<-rep(1:3,2, each=3)) #each가 먼저 작용

#indexing
x<-c(1:9)
x
x[3]
x[c(2,4)]
x[-1] #제외하고의 의미(-)

x[c(2,-4)] #음수와 양수 인덱스는 같이 사용 불가
x[c(2.4,3.54)] #절사 인덱스를 사용(소수점 다 버림)
x[c(TRUE,FALSE,FALSE,TRUE)] #boolean indexing
#R에서는 연산의 개수가 일치해야함(부족하면 recycling)
# 1 2 3 4 /5 6 7 8 /9
# T F F T /T F F T /T
X[X<3]
X[X>3]

#벡터 행 이름 부여-이름으로 indexing
(x<-c("first"=3,"second"=0,"third"=9))
x
x["second"]
x[c("first","third")]


x=c(-3,-2,-1,0,1,2)
#rm(x[1])
#x[1]=NULL

#문제
#x에 1,2,3을 대입하고 y에 (2,3,-4)를 대입한 다음 사칙연산을 해보시오
x<-c(1,2,3)
y<-c(2,3,-4)
x+y
x-y
x*y
x/y
x%*%y

(x<-1:5)
all(x>2) #모두 조건을 만족할 시 TRUE
any(x>2) #하나라도 조건을 만족할 시TRUE

#데이터 정렬(sort,order)
#sort : 한 행에서 정렬
#order : 데이터가 2개일때 다른 열을 중심하고 정렬,인덱스로 값을 찾을 때
x<-c(3,2,6,4,5,8,1)
x

sort(x) #기본 내림차순
sort(x,decreasing = FALSE) #내림차순
sort(x,decreasing = TRUE) #오름차순
#매개변수가 상이해도 작동하는 이유 : 함수는 디폴트 매개변수를 가짐

#정렬된 인덱스
order(x) #정렬된 데이터의 인덱스값을 출력
order(x,decreasing = TRUE) #오름차순
x[order(x)] #정렬된 데이터를 출력

#NA 연산
x=c(2,NA,3,1,4)
sum(x)
sum(x,na.rm =TRUE) #na.rm : na를 remove하라
mean(x,na.rm =TRUE) #평균
median(x,na.rm =TRUE) #중위수 : 정렬해놓고 중간 값을 출력
#통계학에서는 이상치때문에 평균보단 중위수를 더 중요하게 생각
prod(x,na.rm =TRUE) #product

#행 이름 부여(names)
vectorA<-c(1,2,3,4)
names(vectorA)<-c("국어","영어","수학","과학")
vectorA
vectorA["국어"]
vectorA[2]
vectorA[-1]
vectorA[c(1,3)]
vectorA[vectorA>5]
vectorA[c(FALSE,FALSE,TRUE)]
#append : 원래 데이터에 영향을 안줌(대입을 해줘야 영향을 미침)
append(vectorA,c(3,4,5)) #데이터 추가도 가능
vectorA
(vectorB=append(vectorA,c(3,4,5)))

#CRUD
#C : 공간확보(vector)
#인덱스([],vector, 인덱스, 음수 인덱스, boolean indexing(=if))
#U : 인덱스로 수정
#D : 값 삭제는 불가능(NA)
#변수 삭제 : rm(변수)

#집합 연산
x<-c(2,NA,3,1,4)
y<-c(2,1,0,-1,-2)
union(x,y) #합집함
intersect(x,y) #교집합
setdiff(x,y) #차집합(x-y)
setdiff(y,x) #차집합(y-x)
setequal(x,y) #같은지 비교

#subset
(x<-c(3,2,6,4,5,8,1))
subset(x,x>3)
which(x*x>8) #인덱스 출력

#sample, replace, prob
#sample(1:3,3) : 1-3까지 중에서 3개 선택
#replace = T : 복원 추출
#prob : 확률
(x<-c(sort(sample(1:3,3,replace = T,prob=c(0.1,0.3,0.2)))))


nums<-c(5,8,10,NA,3,11)
nums
which.min(nums) #가장 작은 데이터가 있는 인덱스
which.max(nums) #가장 큰 데이터가 있는 인덱스
nums[which.min(nums)]
nums[which.max(nums)]

#문제
#1) 다음과 같은 벡터 객체를 생성하시오
# 1. Vector1 벡터 변수를 만들고 "R"문자가 5회 반복되도록 하시오
(Vector1<-rep("R",5))
# 2. Vector2 벡터 변수에 1~20까지 3간격으로 연속된 정수를 만드시오
(Vector2<-seq(1,20,by=3))
# 3. Vector3에는 1~10까지 3간격으로 연속된 정수가 3회 반복되게 하시오
(Vector3<-rep(seq(1,10,by=3),3))
# 4. Vector4에는 Vector2~Vector3을 모두 포함하는 벡터를 만드시오
(Vector4<-union(Vector2,Vector3))
(Vector4<-append(Vector2,Vector3))
# 5. 25~-15까지 5간격으로 벡터를 생성하시오(seq함수를 이용)
seq(25,-15,by=-5)
# 6. Vector4에서 홀수번째 값들만 선택하여 Vector5에 할당하시오(첨자 이용)
(Vector5<-Vector4[Vector4%%2==1])
Vector5
#(Vector5<-Vector4[seq(1,length(Vector4),2)])
#(Vector5<-Vector4[seq(1,length(Vector4))%%2==1])
#Vector5=NULL

#Package
#Package 종류(기본 패키지, 설치는 돼있지만 로딩이 안된 패키지, 설치 로딩 둘다 안된 패키지)
#install.packages("NISTunits",dependencies = TRUE) #설치해주고 주석처리해줘야함(가끔 두번 설치하면 오류나는 것들이 있음)
#remove.packages : 패키지 제거
library(NISTunits)
NISTdegTOradian(180) #3.141592
NISTradianTOdeg(pi)
ang<-45
(a<-NISTdegTOradian(ang))
(b<-NISTradianTOdeg(a))

(ratio=sin(a))#길이의 비 : 대각선과 수직선의 비
#만약 대각선이 10이라면 수직선의 길이는 얼마인가
(result = 10*ratio)
#길이의 비를 라디안으로 변환
(ang_rad = asin(ratio))
NISTradianTOdeg(ang_rad)

#문제 : 각도가 30도이고 대각선의 길이가 5일때 수직하는 변의 길이를 구하시오
radian30=NISTdegTOradian(30)
ratio=sin(radian30) #대각선이 1일때
5*ratio
#문제 : 각도가 30도이고 대각선의 길이가 5일때 밑변의 길이를 구하시오
radian30=NISTdegTOradian(30)
ratio=cos(radian30) #대각선이 1일때
5*ratio
#문제 : 직각을 이루는 두변의 길이가 밑변 10, 높이 5라면 각도를 구하시오
ratio=5/10
result=atan(ratio) #길이의 비를 각도로 변경하는 함수(atan)
NISTradianTOdeg(result)

#두 벡터의 사이각은 내적을 이용
x<-c(1,2,3)
y<-c(2,3,-4)
sum(x*y) #=x%*%y(내적 값)
dotresult=x%*%y
#|A||B|cosΘ
(xnorm=sqrt(1^2 + 2^2 + 3^2))
(ynorm=sqrt(2^2 + 3^2 + (-4)^2))

costheta = dotresult/(xnorm*ynorm)
radianangle=acos(costheta)
NISTradianTOdeg(radianangle)

#두 벡터의 사이값을 구하시오
x<-c(1,0,0)
y<-c(0,1,0)
(xnorm=sqrt(1^2))
(ynorm=sqrt(1^2))
costheta = dotresult/(xnorm*ynorm)
radianangle=acos(costheta)
NISTradianTOdeg(radianangle)

#두 벡터의 사이값을 구하시오
a<-c(1,2,4,1,2,4,3,6,7,2,0)
b<-c(1,3,2,3,2,2,1,2,1,1,1)
dotresult=a%*%b
anorm=sqrt((sum(a^2))) #anorm=sqrt(1^2+2^2+4^2+1^2+2^2+4^2+3^2+6^2+7^2+2^2)
bnormsqrt((sum(b^2))) #bnorm=sqrt(1^2+3^2+2^2+3^2+2^2+2^2+1^2+2^2+1^2+1^2+1^2)
costheta = dotresult/(anorm*bnorm)
radianangle=acos(costheta)
NISTradianTOdeg(radianangle)


