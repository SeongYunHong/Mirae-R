data1=1:5
data2=2:6
sum((data1-mean(data1))*(data2-mean(data2)))/(length(1:5)-1)
cov(1:5,2:6) #상관관계
cov(1:5,c(3,3,3,3,3)) #0 : 서로 상관 없음
cov(1:5,5:1) #역상관관계 #공분산은 절대적 크기의 정도를 알 수 없음

(m<-matrix(c(1:10,(1:10)^2),ncol=2))
cor(m,method="pearson")
cor(m,method="spearman") #순위의 의미가 있는 것
cor(m,method="kendall")  #부합쌍/비부합쌍의 비율을 이용해서 상관계

#var.test : 등분산 테스트
#cor.test : 상관계수를 검정 
#귀무가설 : 서로 상관이 있다, 대립가설 : 서로 상관이 있다
cor.test(c(1,2,3,4,5),c(1,2,3,4,5),method = "pearson") #p-value < 2.2e-16 : 0.05보다 작으므로 귀무가설을 기각하고 대립가설을 채택(상관이 있다)
#1차 원점에대한 적률 : 평균
#2차 원점에대한 적률 : 분산
#3차 원점에대한 적률 : 왜도
#4차 원점에대한 적률 : 첨도

x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor.test(x,y,method="kendall",alternative="greater") #단측검정(큰 경우, 작은 경우)
#양측검정은 0.05/2 : 정규분포는 대칭
#상관없음??

d<-data.frame(x1=rnorm(10),x2=rnorm(10),x3=rnorm(10))
head(d)
(M<-cor(d))
#install.packages("corrplot")
library(corrplot)
corrplot(M,method="number") #shade, circle, ellipse, number
#순서를 변경해도됨(x1과x2의 상관이나 x2와x1의 상관이 같다)

a <- c(4.0, 4.2, 3.9, 4.3, 4.1 )
b <- c(2.0, 2.1, 2.0, 2.1, 2.2 )
c <- c(0.60, 0.59, 0.58, 0.62, 0.63)

(ab<-sum(a+b)/length(a))
(mat<-matrix(c(a,b,c),nrow=5,byrow=F))
avr<-colMeans(mat)
(acha<-a-avr[1])
(bcha<-b-avr[2])
(ccha<-c-avr[3])

#  a   b   c
#a
#b
#c
(aa_var<-sum(acha*acha)/(length(a)-1)) #0.025
(ab_var<-sum(acha*bcha)/(length(a)-1)) #0.0075
(ac_var<-sum(acha*ccha)/(length(a)-1)) #0.00175

(bb_var<-sum(bcha*bcha)/(length(a)-1)) #0.007
(ba_var<-sum(bcha*acha)/(length(a)-1)) #0.0075
(bc_var<-sum(bcha*ccha)/(length(a)-1)) #0.00135

(cc_var<-sum(ccha*ccha)/(length(a)-1)) #0.00043
(ca_var<-sum(ccha*acha)/(length(a)-1)) #0.00175
(cb_var<-sum(ccha*bcha)/(length(a)-1)) #0.00135

# 데이터에 대하여 상관계수를 구하려면 - 공분산을 두변수의 표준편차로 나누어줌
aa_var / (sd(a) * sd(a)) # 1
ab_var / (sd(a) * sd(b)) # 0.5669467
ac_var / (sd(a) * sd(c)) # 0.533745
cor(mat)

# 고유값 분해 - 아이젠 분해
(eigvector<-eigen(cor(mat)))
eigvector # $values 고유치: 정렬되어 있음 : 분산의 크기 
# $vectors 고유벡터
eigvector$vectors[,1] %*% eigvector$vectors[,2]
eigvector$vectors[,1] %*% eigvector$vectors[,3]
eigvector$vectors[,2] %*% eigvector$vectors[,3]
# 정규직교행렬: 축을 방향값으로 표현
sqrt(sum(eigvector$vectors[,1]^2)) #피타고라스정리??
sqrt(sum(eigvector$vectors[,2]^2))
sqrt(sum(eigvector$vectors[,3]^2))
# -> 모두 1이 나온다는 의미는 축을 방향값으로 표현


#MRS(move,rotate,scale)



##############################################################


# 회귀분석 (linear regression)
# y = ax+b , a와b를 구하는거, 더 복잡해지면 ax+by+cz+b 늘어남
# 데이터로부터 방정식의 계수를 구하는거. 왜?
# y값을 예측하기 위해서
y <- c(1,2,3,8,5,3,10,7)
x1 <- c(2,4,5,6,8,10,11,13)
x2 <- c(1,2,2,4,4,4,6,6)
opar=par(mfrow=c(1,3))
plot(x1,y)
plot(x2,y)
plot(x1,x2)
summary(lm(y~x1)) # lm -> linear model : 선형 모델? , ~ -> formula,y - 절편,x1 - 고유변수 
# y = ax
# 잔차(residual) : 실제값(y) - 예측값(yhat)의 차이
# coefficient 계수
# 직선의 방정식 y = ax+b
# 회귀방정식 0.7121 + 0.5645 * x1
# p-value -> 정규분표 t-value-> t분포
# 잔차의 표준오차가 2.551 
# R-squared:  0.449, 결정계수 : 모델의 설명력을 나타낸다.
# Adjusted R-squared:  0.3571 수정된 결정 계수(변수의 개수를 고려함), 왜 있냐? 
# 결정계수가 변수의 숫자가 많아지면 저절로 커지는 성향이 있다
# F-statistic: 4.889 on 1 and 6 DF,  p-value: 0.06904 모델이 유의미하지않다
summary(lm(y~x2))
# y <- -0.5340 + 1.4921 * x2 # y=ax+b
# 회귀식과 평균의 차이가 회귀 - 회귀제곱합
# 결정계수 = 회귀제곱합 / 전체제곱합(회귀제곱합+잔차제곱합)

summary(lm(y~x1+x2))
-0.1041 + -1.1566 * x1 + 3.7267 * x2 # 1.3094 2.7229 1.5663 7.8631 5.5499 3.2367 9.5335 7.2203
# 결정계수 : Multiple R-squared:  0.9532 모델 설명력이 좋아졌다.
# 계수도 유의미하고 모델도 유의미하다.
# accuracy를 높이려면 변수를 추가해야 한다.


# 문제
str(women) # $ height:, $weight
# 각각 왜 구해야 하는지
# 1 회귀분석실행
# 2 회귀방정식 작성
# 3 각데이터 예측 잔차 구하기
plot(women$weight,women$height)
model <- lm(weight ~ height, data=women)
summary(model)
(women.fitted_weight <- -87.51667 + 3.45000 * women$height)
(residuals_w <- women$weight - women.fitted_weight)
#-87.51667 + 3.45000 * women$height -> 회귀방정식?
#[1] 112.5833 116.0333 119.4833 122.9333 126.3833 129.8333 133.2833 136.7333 140.1833 143.6333 147.0833 150.5333
#[13] 153.9833 157.4333 160.8833

# 회귀방정식 weight <- -87.51667 + 3.45000 * height
# 잔차의 표준오차가 1.525
# R-squared:  0.991,	Adjusted R-squared:  0.9903 
# 모형의 설명력

# F-statistic:  1433 on 1 and 13 DF,  p-value: 1.091e-14
# 계수가 유의미하다 계수의 p-value

# 데이터에 적용된 선형회귀가 적절한가 : 상관분석 : 적절함
# accuracy : 연속된 수치의 정확도는 : 상관분석

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.7333 -1.1333 -0.3833  0.7417  3.1167 
# Residuals 검증
min(residuals_w)
quantile(residuals_w,0.25)
median(residuals_w)
quantile(residuals_w,0.75)
max(residuals_w)

plot(weight ~ height, data=women)
abline(model,col="blue")




# cars 데이터를 선형회귀 해보시오
# 모델이 통계적으로 유의미한가? F-statistic p-value: 1.49e-12(지수) 모델 유의미

# 계수가 유의미한가? 
# p-value 0.0123 * 

# 모델의 설명력은? Adjusted R-squared:  0.6438 
# 결정계수 - 얼마나 데이터를 잘 설명하고 있느냐, 데이터를 가장 잘설명하고 있는 것을 취함

# 선형회귀는 정당한가? 80% 정당함
# 상관계수 cor(cars$speed, cars$dist)

# accuracy는? -> 상관분석? ★

# 맞다 아니다 보단 나온 결과값이 무슨의미인지 해석하기

str(cars)
summary(cars)
plot(cars$dist, cars$speed)
res.lm <- lm(dist~speed, data=cars)
summary(res.lm)
abline(res.lm)
cor(cars$speed, cars$dist)
names(res.lm)
(cof=coef(res.lm))
(cof["speed"])
(cof["(Intercept)"])
(dist_pred = cof["(Intercept)"] + cof["speed"] * cars$speed) # 예측값
fitted(res.lm) # 계산된 fitting 값이 출력
residuals(res.lm)[1:4]
sum(fitted(res.lm)-cars$dist)
plot(res.lm)
par(mfrow=c(1,1))
# qqnorm 표준화잔차가 정규분포
# 회귀분석의 가정 : 등분산성, 독립성, 정규성, 선형성
# bptest : 등분산성 테스트
# 귀무가설 : 등분산성이다
# 대립가설 : 이분산성이다
# -> 외우지 않아도 됨 bptest 있다는 것만 알면됨
# 등분산성, 독립성 중요하다

# install.packages("lmtest")
library(lmtest)
bptest(cars$speed ~ cars$dist) # 등분산성
# Durbin-Watson
dwtest(res.lm) # 독립성 검증
# 2이면 자기상관성이 없다 ( 하나의 변수가 주기별로 다른 데이터 영향 )
# 0~2 양의 자기 상관성이 있다 ( 0이면 아주 높은 양의 자기상관성 )
# 2~5 음의 자기 상관성이 있다.
# 시계열분석의 대상
# 모델 예측
# 귀무가설 : 독립적이다
# 대립가설 : 독립적이지 않다

predict(res.lm,newdata = data.frame(speed=10)) # 속도가 10일 경우 제동거리 예측 # 21.7

# 구간추정
predict(res.lm,newdata = data.frame(speed=10),interval = "confidence")
# fit      lwr      upr
# 1 21.74499 15.46192 28.02807

# 실제 상황과 같이 예측 (예외상황도 고려해서)
predict(res.lm,newdata = data.frame(speed=10),interval = "prediction")
predict(res.lm,newdata = data.frame(speed=4.0,25.0,.21),interval = "confidence")

library(MASS)
data("Boston")
help("Boston")
str("Boston")
?Boston
str(Boston)
# 정규화
boston_df <- as.data.frame(scale(Boston))
str(boston_df)
# 506 obs. of 14 variables:(독립변수 13, 종속변수 1)
# 두 개로 분할 : 학습데이터와 평가 데이터 분리
idx <- sample(1:nrow(boston_df),300)
trainDF <- boston_df[idx,]
testDF <- boston_df[-idx,] # 제외하고
dim(trainDF); dim(testDF) # 206
formula <- medv ~ crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+black+lstat
lm_model <- lm(formula = formula, data = trainDF)
summary(lm_model)
# 과적합이 벌어짐 -> 일반화가 안됨
pred <- predict(lm_model,testDF) # 테스트 데이터는 학습에 참여 않음
pred
cor(pred,testDF$medv) # 숫자일 때의 정확도는 상관계수 : 0.87
# 귀무가설 : 자기상관성이 없다.
# 대립가설 : 자기상관이 있다.
dwtest(lm_model) # DW = 1.9962 (수치 계속 다르게 나옴) : 2이면 자기상관성이 없다
# install.packages("car")
library(car)
# vif(lm_model) # 1~10미만이면 다중공선성이 없다고 본다.
sqrt(vif(lm_model)) > 2 #다중공선성이 있다.