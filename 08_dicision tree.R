#install.packages("party")
library(party) #ctree
#문제의 발견(가설 설정) : 꽃잎의 킉와 넓이 그리고 꽃받침의 크기와 넓이로 종을 구분할 수 있을것
#대상을 관찰 : 결과 데이터를 검색
dim(iris) #150 5 : 범주(3개)
#훈련 데이터와 테스트 데이터를 분리하는 이유 : 과적합 방지
#평가
#
result<-sample(1:nrow(iris),nrow(iris)*0.7) #70% : train/ 30% : test
table(result)
train<-iris[result,]
test<-iris[-result,]
head(train)
#변수 4개를 연결(+)해서 Species에 어떻게 영향을 주는 지 계산
formula<-Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width #Species : 종속변수
iris_ctree<-ctree(formula,data=train)
iris_ctree
#시각화
plot(iris_ctree,type="simple")
plot(iris_ctree)
#예측
pred<-predict(iris_ctree,test)
pred
(tr<-table(pred,test$Species))
paste(round(sum(diag(tr))/nrow(test)*100),'%') #diag(tr) : 대각선의 값 추출

#문제 : node7의 값 출력(행을 선택하는 문제)(subset,filter)
##
result_<-subset(train,train$Petal.Length>1.9 & train$Petal.Width>1.6)
nrow(result_)
##
library(dplyr)
result_<-train%>%
  filter(Petal.Length>1.9 & Petal.Width>1.6)
nrow(result_)

#install.packages("arules")
library(arules)

data("AdultUCI")
str(AdultUCI)
names(AdultUCI) #종속변수 : income, 독립변수 : 14개
class(AdultUCI)
choice<-sample(1:nrow(AdultUCI),10000) #48842개 중 10000개 추출
?AdultUCI

choice
adult.df<-AdultUCI[choice,]
adult.test<-AdultUCI[choice,]
str(adult.df)
(adult_df<-AdultUCI[,c('capital-gain','income','education-num','hours-per-week','race','age')])
colnames(adult_df)<-c('capital','income','education','hours','race','age')
str(adult_df)
#자본이득을 기준으로 잡음(소득이 있는 사람인지지)
formula<-capital~income+education+hours+race+age
adult_ctree<-ctree(formula,data=adult_df)
adult_ctree
plot(adult_ctree)
#res=resolution(해상도),print하기 위해서 dot per inch(인치당 얼마나 점을 찍을 것인가)(클 수록 선명한 그림이 나옴)
png("adult_ctree.png",res=80,height=800,width=1600) 
plot(adult_ctree)
dev.off()

#tree 분석의 문제점
#데이터의 열을 변경하면 결과도 달라짐
#과적합(훈련데이터에 모델이 과도하게 적합)->일반화가 어려워짐
#검정을 할 수없음
#Random Forest 모델을 이용해서 여러개의 모델(500~1000) 만든 후 이산적(투표), 연속적(모델 결과를 평균) 예측
#Ensemble 학습(여러개의 모델이 협력해서 학습)
#여러개의 모델에서는 많은 데이터가 필요 : bootstrap(여러 데이터를 조합해서 더 만드는 것)
#Bagging : bootstrap+aggregation
#Boosting : 증폭하다, 강력하게 한다
#AdaBoost : 약한 모델을 더 강력하게 한다(5~6모델)
  #잘못 분류된 데이터를 윟 ㅐ모델을 만들고 이전 모델과 결합해서 문제를 해결하는 모델
#Gradient Boost : 오류가 난 부분을 점진저긍로 개선 -> 좋은모델
#XGboot = Gradientboost + GPU사용 + 언어호환성
#Python sklearn패키지하고 잘 어울릴 수 있도록 패키지 준비

#엔트로피 지수(Entropy-Index)
#동전의 앞면 : 0.4, 뒷면 : 0.6
x1<-0.4
x2<-0.6
-x1*log(x1)-x2*log(x2) # 0.6730117
#정보이득(information gain(IG))

#install.packages("rpart.plot")
#install.packages("rattle")
library(rpart)
library(rpart.plot)
library('rattle')
result<-sample(1:nrow(iris),nrow(iris)*0.7)
?rpart
train<-iris[result,]
test<-iris[-result,]
dim(train);dim(test) #105 5, 45 5
table(train$Species) #34 35 36
formula<-Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
model<-rpart(formula=formula,data=train)
model
#plot(model,type="simple")
#모델 선택 기준 : accuracy(안정성성)
prp(model)
rpart.plot(model)
fancyRpartPlot(model)
(pred2<-predict(model,test,type="class"))
pred<-predict(model,test)
pred
#0.5를 기준으로하고 크면 선택
cpred<-ifelse(pred[,1]>=0.5,"setosa",
             ifelse(pred[,2]>=0.5,"versicolor","virginica"))
cpred

#문제 : 정분류율, 오분류율을 계산하시오(평가)
(tb=table(cpred,test$Species))

ch<-sum(diag(tb))/nrow(test) #정분류율
1-ch #오분류율
sum(tb-diag(diag(tb)))/nrow(test) #오분류율

#유방암 데이터
wdbc<-read.csv(file.choose())
str(wdbc)
unique(wdbc$diagnosis) #diagnosis : 진단(B->양성, m->악성)
head(wdbc)
wdbc<-wdbc[-1]
wdbc$diagnosis<-factor(wdbc$diagnosis, levels=c("B","M")) #문자를 factor형태로 변환
#rpart로 모델 구현
#정규화(변수간에 종속변수에대한 동등한 영향력을 가질 수 있도록 하는 역할)
normalize<-function(x){
  return(x-min(x)/max(x)-min(x))
}
#디시전트리에서 범주형/연속형 다 사용가능(사이즈가 동일한 상태에서 분산의 크기를 비교하는 것이 더 정확)
wdbc_x<-as.data.frame(lapply(wdbc[2:31],normalize))
summary(wdbc_x)
wdbc_df<-data.frame(wdbc$diagnosis,wdbc_x)
dim(wdbc_df)
idx<-sample(nrow(wdbc_df),nrow(wdbc)*0.7)
wdbc_train<-wdbc_df[idx,]
wdbc_test<-wdbc_df[-idx,]
table(wdbc_df$diagnosis)
model2<-rpart(wdbc.diagnosis~.,data=wdbc_df)
model2
rpart.plot(model2)
fancyRpartPlot(model2)

pred2<-predict(model2,wdbc_test,type="class") #type = class 가 없으면 확률값이 예측이됨
(res<-table(pred2,wdbc_test$wdbc.diagnosis))
acc<-sum(diag(diag(res)))/sum(res) #정분류율
1-acc #오분률

#정밀도
paste(round(res[1,1]/sum(res[1,])*100),'%')
#재현률
paste(round(res[1,1]/sum(res[1,])*100),'%')
#특이도
res[2,2]/sum(res[,2])

#
#install.packages("caret")
library(caret)

confusionMatrix(pred2,wdbc_test$wdbc.diagnosis)

#맥너머 테스트
#귀무가설 : 두 범주는 관련이 없다
#대립가설 : 두 범주는 관련이 있다

#RandomForest
#태동 동기 : 디시전트리 열이 달라지면 결과가 달라지는 단점 극복, 디시전트리의 과적합 극복하기 위해서
#ensemble 학습 : 두개 이상의 모델을 학습시켜 그 결과를 이산적인 경우에는 투표, 연속적인 경우에는 평균으로 계산
#bootstrapping : 많은 데이터를 생성하기 위해서 기존 데이터의 조합
#bagging  : bootstrapping을 해서 만든 모델의 결과를 aggregation
#voting : 상이한 모델을 여러개를 학습한 다음 그 결과를 투표, 평균

install.packages("randomForest")
library(randomForest)
library(rpart)
data(iris)
model=randomForest(Species~.,data=iris) #트리 디폴트값 : 500, 2개의 변수(iris에는 4개의 변수가 있지만 2개만 선택)
model
model=randomForest(Species~.,data=iris,ntree=300,mtry=4,na.action=na.omit)
model
#hyper parameter : 기계가 결정하지 않고 분석가가 결정해야하는 변수
#tuning
ntree<-c(400,500,600)
mtry<-c(2:4)
param<-data.frame(n=ntree,m=mtry)
param
for(i in param$n){
  cat('ntree = ',i,'\n')
  for(j in param$m){
    cat('mtry = ',j,'\n')
    model=randomForest(Species~.,data=iris, ntree=i, mtry=j, na.action=na.omit)
    print(model)
  }
}

#OOB estimate of  error rate이 가장 작은 값 #Number of trees: 500 #No. of variables tried at each split: 2
model3 = randomForest(Species~.,data=iris, ntree=500, mtry=2,importance=T,na.action=na.omit)
model3
importance(model3)
#rpart : Gini 지수
#MeanDecreaseAccuracy(정확도의 개선에 기여하는 변수) MeanDecreaseGini(불순도의 개선에 기여하는 변수)
model3$importance
varImpPlot(model3)
pred<-model$predicted
table(pred,iris$Species)

#문제
#weatherAUS : 내일 비가 올것인가 안 올것인가 
#종속변수 : RainTomorrow
#1. 데이터를 로딩하시오
weatherAUS<-read.csv("C:/Users/401-17/Downloads/weatherAUS.csv", stringsAsFactors = TRUE) #string은 factor로 읽어오기
#2. 학습에 부적한 데이터를 제거하시오
str(weatherAUS)
is.vector(unlist(lapply(weatherAUS,class)))
result<-sapply(weatherAUS,class)
is.vector(result)
weatherAUS<-weatherAUS[c(-1,-2,-22,-23)]
sum(is.na(weatherAUS))
weatherAUS<-na.omit(weatherAUS)
dim(weatherAUS)

#3. sampling(7:3)으로 하시오
set.seed(123)
result<-sample(nrow(weatherAUS),nrow(weatherAUS)*0.7)
train_w=weatherAUS[result,]
test_w=weatherAUS[-result,]
dim(train_w);dim(test_w)
#4. randomForest 모델을 이용해서 분류모델을 구축하시오
#   -최적의 tree수와 mtry개수를 구해서 hyper parameter를 tuning하시오
rain=randomForest(RainTomorrow~.,data=train_w,ntree=100,mtry=2,importance=T,na.action = na.omit)
rain
rainp1<-predict(rain,newdata=test_w)
head(rainp1)
table(rainp1,test_w$RainTomorrow)

ntree<-c(200,300,400,500,600)
mtry<-c(6:10)
param<-data.frame(n=ntree,m=mtry)
param
for(i in param$n){
  cat('ntree = ',i,'\n')
  for(j in param$m){
    cat('mtry = ',j,'\n')
    model=randomForest(RainTomorrow~.,data=train_w, ntree=i, mtry=j, na.action=na.omit)
    print(model)
  }
}
##병렬처리(모델 학습 속도 절약)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
getDoParWorkers()
system.time(
  rf_weather<-foreach(ntree=c(200),.combine=combine,
                      .packages='randomForest',.multicombine=T) %dopar%
    randomForest(RainTomorrow~., data=train_w,
                 ntree=ntree, mtry=8, importance=T,na.action=na.omit)
)
rf_weather
##
#5. 모델에 test를 입력해서 예측한 데이터로 평가하시오
pred<-predict(rf_weather,newdata=test_w) #가장 좋은 파라미터의 모델 사용
tb<-table(pred,test_w$RainTomorrow)
library(caret)
(c<-confusionMatrix(pred,test_w$RainTomorrow))
importance(rf_weather)
varImpPlot(rf_weather)