library(lsa)
#문제 : 내적을 이용해서 cosine 유사도를 구하시오
vec1<-c(1,1,1,0,0,0,0,0,0,0)
vec2<-c(0,0,1,1,1,1,1,0,0,0)
cosine(vec1,vec2) #1
(vec1%*%vec2)/(sqrt(sum(vec1^2))*sqrt(sum(vec2^2))) #2
(vec1%*%vec2)/(norm(vec1,type="2")*norm(vec2,type="2")) #3
#1=2=3 다 똑같은 식

install.packages("recommenderlab")
library(recommenderlab)
m<-matrix(sample(c(NA,0:5),100,replace = T, prob = c(.7,rep(.3/6,6))),
          nrow=10,ncol=10,dimnames=list(
            user=paste('u',1:10,sep=''),
            item=paste('i',1:10,sep='')
          ))
m
#행렬 표현 방법 : 인접행렬/그래프

#인접행렬 : item
    #user  i1 i2 i3 i4 i5 i6 i7 i8 i9 i10
    #u1  NA  5 NA NA  5 NA NA  2 NA  NA
    #u2  NA NA NA NA NA  0 NA NA  2  NA
    #u3  NA NA NA NA NA  5 NA  1 NA  NA
    #u4  NA NA  1 NA NA  4  5 NA NA  NA
    #u5  NA NA NA NA NA  0  3 NA NA  NA
    #u6  NA NA NA NA  0 NA  4 NA  5  NA
    #u7  NA NA  5  2  1 NA NA  4 NA  NA
    #u8   3  1  5 NA NA NA NA  4  3   4
    #u9  NA NA NA NA NA  0 NA NA NA  NA
    #u10  5 NA  3  3  0  5 NA NA NA  NA
class(m)
typeof(m) #integer vector로 구성된 matrix

r<-as(m,"realRatingMatrix") #realRatingMatrix : 평점행렬(:희소행렬)
r
class(r)
typeof(r) #R은 모두 클래스로 이루어짐 => 구조적 프로그래밍화
dim(r)
#realRatingMatrix 전용 함수들
rowCounts(r)
colCounts(r)
rowMeans(r)
getRatings(r)
image(r)
show(r)
hist(getRatings(r))
as(r,"list")
as(r,"matrix")

#그래프 방식으로 저장
head(as(r,"data.frame")) #원래 저장하는 모습

#추천 알고리즘
#UBCF : User Based Collaborative Filter : 행
#IBCF : Item Based Collaborative Filter : 열
#척도
#cosine similarity
#pearson similarity
#jaccard similarity (binaryRatingMatrix에서 사용)
#추천 데이터 = 희소행렬
#SVD : 비정방 행렬의 특성을 뽑는 작업
#상품 개수 == 사용자 개수 : 정방행렬
#Matrix Factorizing(MF) : 특징을 추출해서 유사도를 계산
#추천기계 : Recommendr / HybridRecommender

#영화 평점 데이터
data("MovieLense")
typeof(MovieLense)
class(MovieLense) #이미 변환이 된 데이터
#358 x 1664
#영화를 100개 이상 본 사람들
(MovieLense100<-MovieLense[rowCounts(MovieLense)>100,])
#50 x 1664
train<-MovieLense100[1:50]
rec<-Recommender(train,method="POPULAR") #POPULAR : 모델학습(개수)
rec #모델에 101과 102번을 넣어줌
#291,292번번
pre<-predict(rec,MovieLense100[101:102],n=10) #10개 추천
pre #‘topNList’ with n = 10 for 2 users. 
as(pre,"list") #id와 영화 제목
as(pre,"matrix")

(MovieLense100<-MovieLense[rowCounts(MovieLense)>100,])
train<-MovieLense100[1:100]
test<-MovieLense100[101:103]

recom<-HybridRecommender(
  Recommender(train, method = "POPULAR"),
  Recommender(train,method="RANDOM"),
  Recommender(train,method="RERECOMMEND"),
  weights=c(.6,.1,.3) #가중치 : 모델 중요
)
recom
getModel(recom)
as(predict(recom,test),"list")

#영화 추천 시스템(상관 계수(열 중심 : 유사한 사람)로 추천) 
#movie4, movie5 중에 어떤 영화를 추천할지
movie<-read.csv("C:/Users/401-17/Downloads/movie.csv",header=T)
movie
str(movie)
movie_t<-t(movie[,-c(1,5,6)]) #사람에대한 상관계수가 필요(대상을 열로 표현)
movie_t
colnames(movie_t)<-c("a","b","c","d","e")
cor(movie_t) #a에게 movie5번을 추천

#유클리디안 거리를 이용한 추천
recomm_w<-read.csv("C:/Users/401-17/Downloads/sf_recomm.csv",header=T)
dim(recomm_w) #2091 13
head(recomm_w)
recomm_df<-recomm_w[c(1:8)]
head(recomm_df)
#코드, 투자금액, 나이, 집마련 월 적립금, 결혼자금 월 적립금, 교육자금, 은퇴자금, 기타목돈
#CODE, INVEST_TOT, NOWAGE, REQ_H, REQ_M, REQ_EDU, REQ_R, REQ_ETC
#과제 : 나와 가장 비슷한 패턴(나이, 집마련 월 적립금, 결혼자금 월 적립금, 교육자금, 은퇴자금, 기타목돈)을 보이는 사람의 연금상품 추천
inputdata<-c(0,208,55,60,35,0,25,183) #추천데이터
#Dataframe 생성
recomm_df2<-rbind(inputdata,recomm_df)
#code 컬럼 제외
recomm_data<-recomm_df2[2:8]
#거리값 계산
test_dist<-dist(head(recomm_data),method="euclidean",by_row=T)
test_dist
(test_dist_mt<-as.matrix(test_dist))
(test_dist_mt_sort<-sort(test_dist_mt[,1])) #거리값이 가장 가까운 순서로 정렬
result_index<-names(test_dist_mt_sort[2]) #거리값이 가장 가까운 인덱스 추출
real_index<-as.numeric(result_index) #5번을 숫자 인덱스로 변환
recomm_df2[real_index,1]
recomm_w[real_index,9:13]
#구성비를 추천 FO_H      FO_M    FO_EDU       FO_R FO_ETC
#모델은 유사도에 의해 추천 : 거리에 의한 추천(MF, SVD)

#문제 real_data를 가진 사람의 어떤 구성비를 추천할 것인지 결정하시오
real_data<-c(0,250,45,10,30,10,0,10)
recomm_df3<-rbind(real_data,recomm_df)
recomm_data<-recomm_df3[2:8]
test_dist<-dist(head(recomm_data),method="euclidean",by_row=T)
test_dist
class(test_dist) #dist 크기행렬
#거리값 행렬은 matrix로 변환해야 정확한 값을 얻을 수 있음
(test_dist_mt<-as.matrix(test_dist))
(test_dist_mt_sort<-sort(test_dist_mt[,1]))
result_index<-names(test_dist_mt_sort[2])
real_index<-as.numeric(result_index)
recomm_df3[real_index,1]
recomm_w[real_index,9:13]

#movie 데이터를 realRatingMatrix로 변환 후 모델 적용
movie<-read.csv("C:/Users/401-17/Downloads/movie.csv",header=T)
movie
library(reshape2)
#long, wide
movie_long<-melt(id=1,movie) #id를 1열을 중심으로
movie_long 
names(movie_long)<-c('user','movie','rating')
movie_long
#rating이 0인 것들 제외(행 선택)
movie_long<-subset(movie_long,rating != 0)
length(movie_long$rating)
#추천 모델의 전용행렬 변환
movie_real<-as(movie_long, "realRatingMatrix")
dim(movie_real)
as(movie_real,"matrix")
#학습 데이터 생성
trainSet<-movie_real[c(2:5),]
recommTarget<-movie_real[1,]
#추천 모델 구성 : 유저 기반, 유사도 척도 : 피어슨 상관계수
recom_model<-Recommender(trainSet,method="UBCF", parameter="Pearson")
recomm_list<-predict(recom_model, recommTarget,n=3)
(recom_result<-as(recomm_list,"list"))
as(recomm_list,'matrix')

#음식 추천 데이터
gloseri<-read.csv("C:/Users/401-17/Downloads/gloseri.csv",header=T,encoding = 'UTF-8', fileEncoding = "cp949")
head(gloseri)
str(gloseri) #주문 내역
realData<-as(gloseri,'realRatingMatrix')
as(realData,"matrix")
rowCounts(realData) #사람별 카운트
colCounts(realData) #제품별 카운트
rowMeans(realData)
getRatings(realData)
getRatingMatrix(realData)
image(realData)
nratings(realData)
sample(realData)
show(realData)
#훈련데이터 생성
trainData<-sample(1:7,6) #6명 선택
(trainSet<-realData[trainData])
as(trainSet,'matrix') #6명x5개제품
trainSet2<-trainSet[rowCounts(trainSet)>=3]
as(trainSet2,"matrix") #5명(1명이 학습에서 제외)
recomm_model_gl<-Recommender(trainSet2,method="UBCF",parameter="Cosine") #사용자 기반 추천 모델, 코사인 유사도
recomm_model_gl
recomm_target_gl<-realData[-trainData] #1명
recommendList<-predict(recomm_model_gl,recomm_target_gl,n=2) #음식 2개 추천
(recomm_result<-as(recommendList,'list')) #우유 : NA인 데이터가 있어야 추천이 가능함

#IBCF 기반 추천 : 열 기반 추천 : true/false binarize를 이용해서 데이터 변환
as(realData,'matrix')
realData_b<-binarize(realData,minRating=1) #minRating=1 : Rating이 1이상인 값 : 참
as(realData_b,'matrix')
trainData<-sample(1:7,6)
(trainSet<-realData_b[trainData]) #데이터 추출
as(trainSet,'matrix')
recomm_target_gl<-realData_b[-trainData] #예측 데이터
recomm_model_gl<-Recommender(trainSet,method="IBCF",parameter="Jaccard") #공통(교집합)/전체(합집합)
recommendList<-predict(recomm_model_gl,recomm_target_gl,n=2) #데이터 2개를 추천하라
recomm_result<-as(recommendList,'list')

#문제
#MovieLense 데이터에 대하여 아이템 별 영화 평가 점수 데이터셋으로 추천 모델을 생성하시오
data("MovieLense")
MovieLense #943x1663
#아이템 별 영화 평가 점수 데이터셋으로 binary포멧으로 생성하시오
#영화를 50회이상 보고 영화별 100회 이상 본 사용자가 있는 경우를 대상으로 하시오
ratings_movies=MovieLense[rowCounts(MovieLense)>=50, colCounts(MovieLense)>=100]
ratings_movies #565x336

(m<-as(ratings_movies,'matrix'))
sum(table(m[1,]))
sum(table(m[,1]))
#사용자는 90%를 훈련 참여하고 10%를 추천으로 데이터를 분할하시오
#10%의 데이터에 대하여 영화 5개씩 추천하시오
ratings_movies_b<-binarize(ratings_movies,minRating=1)
as(ratings_movies_b,'matrix')
#데이터 분할
idx<-sample(1:nrow(ratings_movies),nrow(ratings_movies)*0.9)
trainSet<-ratings_movies_b[idx,]
recomm_target<-ratings_movies_b[-idx,]
dim(trainSet)
dim(recomm_target)
recomm_model<-Recommender(trainSet, method="IBCF",parameter="JACCARD")
recomm_list<-predict(recomm_model,recomm_target,n=5)
recomm_list
head(as(recomm_list,'list'))

#PCA
a <- c(4.0, 4.2, 3.9, 4.3, 4.1 )
b <- c(2.0, 2.1, 2.0, 2.1, 2.2 )
c <- c(0.60, 0.59, 0.58, 0.62, 0.63)
(mat <- matrix(c(a,b,c ), nrow=5,byrow=F ))
#공분산 행렬, 상관계수 행렬
eigvector<-eigen(cor(mat))
names(eigvector) #values : 고유치, vectors : 직교벡터
eigvector$values
eigvector$vectors[,1]%*%eigvector$vectors[,2]
eigvector$vectors[,2]%*%eigvector$vectors[,3]
eigvector$vectors[1,]%*%eigvector$vectors[2,]
#정방행렬이면서 대칭행렬에 대한 역행렬은 전치행렬이다
teigvector=t(eigvector$vectors)
eigvector$vectors%*%teigvector

#PCA 주성분 분석
str(mtcars) #32x11
head(mtcars,10)
fit<-princomp(mtcars,cor=T) #princomp : 상관 계수를 이용해서 주성분 분석 
summary(fit)
#Standard deviation : 표준편차
#Proportion of Variance : 분산에서 차지하는 비율율
#Cumulative Proportion  : 누적비율(Proportion of Variance을 다 더한 값)
#요인 분석, 주성분 분석, kmeans 명명식 : 새롭게 이름을 명명
#주성분분석의 각 축을 결정할 때는 데이터의 모든 열 요소가 참여해서 새로운 축을 생성
#참여한 변수들의 구성비를 보고 축을 재명명함
#domain knowledge : 
#분산이 크면 종속변수를 설명하는데 크게 기여한다.
plot(fit,type="lines")
fit$scores
fit$scores[,1:3] #1~3축까지 영향을 미치는 변수의 값
fit$scores[,1:4]
biplot(fit)

data<-fit$scores[,1:4] # 원래 데이터 %*% 고유벡터(rotate())=score
class(data)
(data<-cbind(mtcars$mpg, data))
colnames(data)<-c("mpg","PC1","PC2","PC3","PC4")
head(data)
data<-as.data.frame(data)
regression_model<-lm(mpg~.,data=data) #주성분 분석한 데이터 4개만 사용 / 주성분 분석을 통해 새롭게 만들어진 변수가 더 정확  
summary(regression_model) #Adjusted R-squared:  0.8867 

regression_model2<-lm(mpg~.,data=mtcars) #전체 11개의 데이터 사용
summary(regression_model2) #Adjusted R-squared:  0.8066

#
(x<-1:10)
(y<-x+runif(10,min=-.5,max=.5))
(z<-x+y+runif(10,min=-10,max=10))
data<-data.frame(x,y,z)
(pr<-prcomp(data,scale=T)) #주성분 분석(구현방법만 다름)
#고유벡터 : 데이터로부터 생성된 새로운 축
#정직교하는 축 : 다중공선성(두 변수간에 상관관계가 있는 경우)
#다중공선성이 있으면 종속변수에 더블의 영향을 미침
#정직교축에 데이터를 재표현하면 다중공선성이 사라짐(noise 제거)
#original data : 10x3
#10x3(original data) 3x3(정직교축(rotation data)) => 10x3 => 이거를 분석하면 다중공선성이 없는 분석 가능
#10x3 3x2 => 10x2
#독립변수 변환을 하더라도 종속변수는 변함이 없음
#뒤의 열 수만큼 특성 추출
summary(pr)
screeplot(pr,type=c("barplot","lines"),max(10,length(pr)))
names(pr)
pr$sdev
pr$rotation
pr$center
pr$scale
pr$x
biplot(pr)

#행간분석 UBCF realRatingMatrix
#열간분석 IBCF binarize