#문제 : 데이터의 열간거리와 행간거리를 구하시오
#        var1  var2
#person1  20    80
#person2  30    44
#person3  90    40

#Minkowski 거리 : sqrt((20-90)^n+(80-40)^n)^(1/n)
#cos유사도 : 내적/|A||B|
#cos비유사도(거리) : 1 - 내적/|A||B|
#Mahalanobis 거리 (object detection -> object tracking)

sqrt((20-80)^2+(30-44)^2+(90-40)^2) #열간거리
sqrt((20-30)^2+(80-44)^2)+sqrt((20-90)^2+(80-40)^2)+sqrt((30-90)^2+(44-40)^2) #행간거리

##########
#군집분석(kmeans-clustering)
vec<-c(1,2,3,4)
sqrt(sum(vec^2))

#install.packages("lsa")
library(lsa)
#cosine 유사도 사용 이유 : cosine 0일때 1이라서
#0도 : 두 데이터가 같은 방향을 봄(유사도가 큼)
#숫자가 크면 더 유사함
vec1 = c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
vec2 = c( 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0 )
vec3 = c( 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0 )
cosine(vec1,vec2)
matrix=cbind(vec1,vec2,vec3)
cosine(matrix) #정방행렬, 대칭행렬 : 공분산 행렬, 상관계수 행렬, 거리값 행렬, 유사도 행렬

vec1%*%vec2 #내적 : 벡터의 크기가 고려된 사이각

(x<-matrix(1:16, nrow=4))
sqrt(sum((x[1,]-x[4,])^2))
(dist<-dist(x,method="euclidean")) #4x4 => 3x3
(dist<-dist(x,method="minkowski",p=2))
(dist<-dist(x,method="manhattan")) #맨하튼 : 직선거리로 측정

#Hierachical clustering(탐색적 클러스터링) : 군집 수를 결정하기 위해서
idx=sample(1:dim(iris)[1],40) #데이터 40개 선택
irisSample=iris[idx,]
Species<-irisSample$Species #백업
irisSample$Species=NULL #열삭제(범주형 데이터)
#dist(irisSample) : 거리값으로 표현된 행렬(정방, 대칭)
hc=hclust(dist(irisSample),method="ave") #계층적 첫번째 매개변수 dist()
#일반화 함수
plot(hc,hang=-1,lables=iris$Species[idx])
rect.hclust(hc,k=3) 
(groups=cutree(hc,k=3)) #40개 데이터에 대한 그룹번호 출력, 대각선이 아닌것들은 정분류
table(groups,Species)

#kmeans
#k개의 군집들과 k개의 중심값을 결정
#모든 요소의 중심값과의 거리값을 계산
#가장 가까운 군집에 할당
#집단별 중심값을 재계산
#반복과정 속에서 WSS(within sum of square)값을 최소화해가는 알고리즘
#군집분석을 할 때는 열별 중요도를 같게하기 위해서 scaling 실시
set.seed(1)
iris_back<-iris
iris_back$Species<-NULL #150
iris.scaled<-scale(iris_back[,-5])
(kmeans.result<-kmeans(iris.scaled,3,trace=T)) #iris.scaled 대신 거리값이 들어갈 수 있음
table(iris$Species, kmeans.result$cluster) #결과 비교
plot(iris.scaled[,c("Sepal.Length","Sepal.Width")], xlim=c(-2,2), ylim=c(-2,2), col=kmeans.result$cluster)
#센터 포인트
points(kmeans.result$centers[,c("Sepal.Length","Sepal.Width")], col=1:3, pch=10, cex=10)

names(kmeans.result)
iris$Species
kmeans.result$cluster #결정된 그룹 : setosa : 3 3->2
iris_back$pred<-kmeans.result$cluster
sum(iris_back$Species == iris_back$pred)
kmeans.result$centers #중심이 3개라 3개 #중심값 == 군집을 대표하는 특성(평균으로 집단을 이해한다)
kmeans.result$iter #중심재계산횟수
kmeans.result$size #군집별 개수
kmeans.result$totss #total sum of square 차의 제곱의 합
#거리 : 군집내 거리(각 데이터 + center)(withinss), 군집간 거리(betweenss)
sum(kmeans.result$withinss)+sum(kmeans.result$betweenss)
install.packages("factoextra")
library(factoextra)
km.res<-kmeans(iris.scaled,3,nstart=10)
fviz_cluster(km.res,iris[,-5],ellipse.type="norm")

#caret 패키지 : 자동화 도구
library(caret)
set.seed(123)
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
#createDataPartition : bootstrap 데이터
#cross validation : 데이터 분할(train/test) #ex)10 그룹으로 분할하고 9개로 훈련하고 1개를 테스트(훈련 : 1~9/ 테스트 : 10, 훈련 : 1~8,10/ 테스트 : 9)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
training.data<-scale(training[-5])
summary(training.data)
#중심재계산의 횟수를 10000번으로 제한
iris.kmeans<-kmeans(training.data[,-5],center=3,iter.max = 10000)
training$cluster<-as.factor(iris.kmeans$cluster)
qplot(Petal.Width,Petal.Length,color=cluster,data=training)
#k개수 결정
wssplot<-function(data,nc=15,seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,center=i)$withinss)
  }
  plot(1:nc,wss,type="b",xlab="클러스터 수",ylab="군집 내 거리의 합") #군집 내 거리의 합이 커지면 응집도가 커짐
}
wssplot
wssplot(training.data) #elbow

#문제 : hclustering
#중학교 1학년 신체검사 결과
bodycheck<-read.csv("C:/Users/401-17/Downloads/bodycheck.csv")
idist<-dist(bodycheck[,-1])
hc<-hclust(idist)
#일반화 함수
plot(hc,hang=-1)
res<-rect.hclust(hc,k=3,border="red")
attach(bodycheck)
(g1<-subset(bodycheck,번호==10|번호==4|번호==8|번호==1|번호==15))
(g2<-subset(bodycheck,번호==11|번호==3|번호==5|번호==6|번호==14))
(g3<-subset(bodycheck,번호==2|번호==9|번호==13|번호==7|번호==12))
detach(bodycheck)

#snsdata : sns계정을 가진 30000명의 청소년을 대상으로 4개의 기본 데이터
#36개의 관심분야에대해 수진된 정보
#성향분석 : 5개의 그룹-범죄성향, 운동성향, 외모지향, 무기력, 브레인 clustering
teens<-read.csv("C:/Users/401-17/Downloads/snsdata.csv")
seed(100)
str(teens)
head(teens)
sum(is.na(teens)) #7810
colSums(apply(teens,2,is.na)) #gender : 2724/age : 5086
sum(is.na(teens$friends))
table(teens$gender) #F : 22054/M : 5222
summary(teens$age) #5086
teens$age<-ifelse(teens$age>=13 & teens$age<20, teens$age, NA)
summary(teens$age) #5523
#졸업년도를 기준으로 평균 설정
(avg<-ave(teens$age, teens$gradyear, FUN=function(x) mean(x,na.rm=T)))
teens$age<-ifelse(is.na(teens$age),avg,teens$age) #na면 평균나이로 입력
teens$age[1:6]
summary(teens$age)
#4개의 기본정보
interests<-teens[5:40]
summary(interests)
interests_n<-data.frame(lapply(interests,scale))
teen_clusters<-kmeans(interests_n,5)
names(teen_clusters)
teen_clusters$cluster[1:5]
teen_clusters$size
#군집별 특성
round(teen_clusters$centers*100)
#성향분석 : 5개의 그룹-범죄성향, 운동성향, 외모지향, 무기력, 브레인 clustering
#1번 그룹 : 브레인
#2번 그룹 : 운동성향
#3번 그룹 : 무기력력
#4번 그룹 : 범죄지향
#5번 그룹 : 외모지향

(medians<-apply(teen_clusters$cluster,2,median))
teens$cluster<-teen_clusters$cluster
aggregate(data=teens,age~cluster,mean)
qplot(cluster,age,colour=gender,data=teens)
(res<-aggregate(data=teens,gender=='F'~cluster,mean))
#여성이 소프트볼, 발리볼, 헤어스타일, 드레스에 대한 관심이 높다
aggregate(data=teens,softball+volleyball+hair+dress~gender=='F',mean)

aggregate(data=teens,sex+die+drunk~bible>0,mean)
aggregate(data=teens,shopping~cluster,mean)

#문제 : 다음 3가지를 스스로 찾아서 분석해보시오(가설->확인인)
#남자는 음악과 드레스에 관심이 많은가
aggregate(data=music,dress+~gender=='M',mean)
#남자는 음주와 마약에 관심이 많은가
aggregate(data=teens,drunk+drugs~gender=='M',mean)
#여자는 야구+스포츠에 관심이 많은가
aggregate(data=teens,baseball+sports~gender=='F',mean)

###
aggregate(data=teens,basketball+soccer+swimming~gender=='M',mean)
aggregate(data=teens,church+jesus+god~gender=='M',mean)
aggregate(data=teens,church+jesus+god~gender=='F',mean)
###

#DTW : dynamic time wraping
#1:1 대응관계로 비교하면 차이가 많이 발생
#그래프 비교(센서 신호) : 시차지연이 발생
#음악 프로젝트 : 파형분석 - 같은 유형의 음악장르를 구별
install.packages("dtwclust")
library(dtwclust)
data(uciCT)
str(CharTraj)
#보간법(중간에 데이터가 없는 경우)
#데이터 사이즈가 일치(NA)
series<-reinterpolate(CharTraj, new.length = max(lengths(CharTraj)))
(series<-zscore(series)) #z-정규화(NaN->0)
#파형인경우엔 NA를 0으로 채움
#time series
#거리값 : 정규화(변수별로 같은 영향력을 가지게 하기 위해)
pc.dtwlb<-tsclust(series,k=20L,
                  distance="dtw_lb",centroid="pam", #L1 절대값, L2 차의 제곱
                  seed=3247, trace=T,
                  control=partitional_control(pam.precompute=FALSE),
                  args=tsclust_args(dist=list(window.size=20L)))
plot(pc.dtwlb)

#kmeans의 단점 : k 값 설정, center 결정, 이상치가 있을 경우(k median model)
#eps : epsilon, 최소 data point(minPts) =>DBSCAN
#minPts : 그룹이되려면 있어야하는 data point의 개수
install.packages("dbscan")
library(dbscan)

iris2=iris[-5]
ds=dbscan(iris2,eps=0.42,minPts=5)
#plot(ds,iris2[c(1,4)])

set.seed(2)
n<-400
x<-cbind(
  x=runif(4,0,1)+rnorm(n,sd=0.1),
  y=runif(4,0,1)+rnorm(n,sd=0.1)
)
plot(x,col=rep(1:4,time=100))
#DBSCAN clustering : 자동으로 k 결정
#Ordering Points to Identifiy the Clustering Structure
res<-optics(x,eps=10,minPts = 10)
res

res<-extractDBSCAN(res,eps_cl=0.65) #threshold 문지방

#vexhull
hullplot(x,res)
plot(res) #검정색 = 노이즈

#kmeans와 dbscan을 비교하는 문제 출제

#knn(k nearest neighbor) : k개의 근접한 이웃의 추천에 따라 결정/ 학습 불가/ instance based model/
#거리값 : 정규화해야함/ caret패키지를 사용하여 모델 생성
#grape, fish, carrot, orange, celery, cheese
train_df=data.frame(x1=c(8,2,7,7,3,1), #단맛
                    x2=c(5,3,10,3,8,1), #아삭거림
                    y=factor(c(1,2,3,1,3,2))) #과일(1), 단백질(2), 채소(3)로 구분
train_df
plot(train_df$x1,train_df$x2, col=train_df$y, xlab='단맛',ylab='아삭거림')
#새로운 항목 : 토마토, 땅콩, 사과
test_df=data.frame(x1=c(6,3,10),x2=c(4,8,9))
test_df
#토마토-grape
sqrt((8-6)^2 + (5-4)^2)
sqrt((2-6)^2 + (3-4)^2)

library(class)
knn(train_df[,-3],test_df, train_df$y,k=3)
sqrt(length(train_df$x1)) #sqrt(n) #knn의 k값을 구하는 식

#문제 : iris 데이터에 대하여 knn모델을 구현해보시오
set.seed(415)
result<-sample(1:nrow(iris),nrow(iris)*0.7)
train<-iris[result,]
test<-iris[-result,]

sqrt(length(train$Sepal.Length))
res<-knn(train[,-5],test[,-5], train$Species,k=11,prob=T)
res<-table(res,test$Species)
sum(diag(res))/sum(res)

#caret 패키지 : 자동화패키지
library(caret)
modelLookup(model="knn")
data(iris)
TrainData<-iris[,1:4]
TrainClasses<-iris[,5]
featurePlot(TrainData, TrainClasses, plot="density", auto.key=list(columns=3))
knnFit1<-train(TrainData,TrainClasses,
               method="knn", #신경망, tree, 확률, 유사도 등등
               preProcess=c("center","scale"), #센터를 기준으로 스케일 실시(전처리)
               tuneLength=20,
               trControl=trainControl(method="cv")) #cross validation : 전체 데이터를 일정한 덩어리로 분할(돌아가면서 test 데이터가 됨)
plot(knnFit1)
#knn : instance based model
knnPredict<-predict(knnFit1, newdata=TrainData)
confusionMatrix(knnPredict,TrainClasses) #accuracy
