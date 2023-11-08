#연관분석 : Rule base model
#left hand set 과 right hand set의 경우의 수 카운트
#지지도(support), 신뢰도(confidence), 향상도(lift) 척도를 기준으로 관계의 중요성을 추출
#confidence(x,y) : x를 샀을 때 y의 값
#apriori package

#original data->transaction matrix->apriori 모델->inspect 확인
library(arules)
data("Adult")
DATAFRAME(head(Adult)) 
str(Adult) #s4 class여서 내부가 복잡함
typeof(Adult) #s4 class(클래스 구성방식)로 만든 transaction matrix형태
rules<-apriori(Adult) #6137 rules(6137개의 조합 생성)
rules.sub<-subset(rules, subset=rhs %in% "marital-status=Never-married" & lift>2)
rules.sub
inspect(rules.sub) #inspect로 조합 확인인
#age : 원핫인코딩->구간 범주화->dummy 변수화

#범주형->dummy 변수화, 연속형일때는 구간 범주화해야한다
#4개의 연속된 수치
data(iris)
dim(iris) #150 5
irisDisc<-discretizeDF(iris) #discretizeDF : 구간 범주화(break3개로 구간 범주화)
head(irisDisc)
trans5<-as(irisDisc, "transactions")
#15 items (columns) 5개가 15개가 된 이유  : 4개의 데이터를 break 3개를 통해서 12개가 만들어졌고 species가 3개여서 4*3+3=15
trans5
inspect(head(trans5))
rule<-apriori(trans5)
# supp=0.3 : 공통으로 나오는 경우가 10번 중에 3번이다,conf=0.1 : 왼쪽이 10번 나올때 1번은 나와야한다
rule<-apriori(trans5,parameter=list(supp=0.3,conf=0.1)) #859 rules 
inspect(rule)

##################### 라면, 맥주, 고기, 우유, 과일
# t1 : 라면,맥주,우유
# t2 : 라면,고기,우유
# t3 : 라면,과일,고기
# t4 : 고기,맥주,우유
# t5 : 라면,고기,우유
# t6 : 과일,우유

a<-c(1,1,1,0,0)
b<-c(1,0,1,1,0)
c<-c(1,0,0,1,1)
d<-c(0,1,1,1,0)
e<-c(1,0,1,1,0)
f<-c(0,0,1,0,1)
(df<-data.frame(a,b,c,d,e,f))
(df<-as.data.frame(t(df)))
colnames(df)<-c('라면','맥주','우유','고기','과일')
df
trans4<-as(df,"transactions")
rule<-apriori(trans4, parameter=list(supp=0.01, conf=0.01))
inspect(rule)
#   lhs           rhs   support   confidence coverage lift count
#{라면, 고기} => {과일} 0.1666667 0.3333333  0.5000000 1.000 1
#라면,고기 : 3 | 과일 : 2 | 라면,고기&과일 : 1 | support(라면,고기&과일/전체) : 1/6 | confidence(라면,고기&과일/라면) 1/3 | ㅣlift((라면,고기&과일/라면,고기)/(과일/전체)) : 1
#{라면}       => {우유} 0.5000000 0.7500000  0.6666667 0.900 3
#라면 : 4 | 우유 : 5 | 라면&우유 : 3 | support(라면&우유/전체) : 3/6 | confidence(라면&우유/라면) 3/4 | ㅣlift((라면&우유/라면)/(우유/전체)) : 9/10

#문제
#데이터 베이스(DB)에 데이터를 저장하고 데이터를 가지고 온 다음 연관분석을 해보시오(DB에 데이터를 분석하는 경우)
library(data.table)
library(RMySQL)

con<-dbConnect(MySQL(),user="root",password="8325338a!",dbname="mirae",host="localhost",port=3306)
dbListTables(con)

df   
df<-data.table(df)
dbWriteTable(con,"df",df,overwrite=TRUE)
(df=dbReadTable(con,"df"))
class(df)
trans3<-as(df,"transactions")
rule<-apriori(trans3, parameter=list(supp=0.01, conf=0.01))
inspect(rule)




library(dplyr)
library(dbplyr)
library(DBI)
conn<-DBI::dbConnect(RMySQL::MySQL(), dbname="Association", host="127.0.0.1", user="root", password="8325338a!")
dbListTables(conn)
dbWriteTable(conn, "tr_table", value=df, append=F, overwrite=T)

res<-dbSendQuery(conn,"select*from tr_table") #전체 테이블 불러오기
(df<-df[,2:length(df)]) #행 이름 제거
df
df_tran<-as(df,"transactions")
rule<-apriori(df_tran, parameter=list(supp=0.6, conf=0.3, minlen=2, maxlen=3))
inspect(rule)
#install.packages("arulesViz")
library(arulesViz)
plot(rule,method="graph")

###
data("Groceries")
typeof(Groceries)
str(Groceries)
Groceries #9835,169 items
summary(Groceries)
inspect(head(Groceries,3))
size(head(Groceries))
LIST(head(Groceries,3))
itemFrequencyPlot(Groceries,topN=15)
gdf<-as(Groceries,'data.frame')
head(gdf)
inspect(subset(Groceries[1:200],size(Groceries[1:200])>4))
wholemilk_rules<-apriori(data=Groceries,parameter=list(supp=0.001, conf=0.08, minlen=2),appearance=list(lhs="whole milk")) #minlen=2 : 한개짜리 제거
inspect(wholemilk_rules)
rules<-apriori(Groceries,parameter=list(support=0.001, conf=0.5))
rules

rules_supp<-sort(rules, by="support", decreasing=T)
inspect(head(rules_supp))
rules_conf<-sort(rules, by="confidence", decreasing=T)
inspect(head(rules_conf))
rules_lift<-sort(rules, by="lift", decreasing=T)
inspect(head(rules_lift))

plot(rules[1:15], method="graph",engine='interactive')

#whole milk와 자주 구매하는 상품을 검색(conf가 높은 것들 고르기)
amilk<-subset(rules,lhs %in% 'whole milk')
amilk
inspect(amilk)
rules_conf<-sort(amilk, by="confidence", decreasing=T)
inspect(rules_conf[1:5]) 

#문제 : 'berries'와 같이 사는 물건 중에 support가 높은 순으로 3개를 출력하시오
aberries<-subset(rules,lhs%in%'berries')
aberries
rules_supp<-sort(aberries, by="support", decreasing=T)
inspect(rules_supp[1:3]) 

#AdultUCI : Census Income Database
data("AdultUCI") 
basket_rules<-apriori(AdultUCI, parameter=list(sup=0.08,conf=0.5,target="rules")) #rules : 40548
min(AdultUCI["age"])
max(AdultUCI["age"])
count(unique(AdultUCI["age"]))
#factor vs ordered(순서가 있음,수치가 의미 있는 경우)
AdultUCI[["age"]]<-ordered(cut(AdultUCI[["age"]],c(15,25,45,65,100)),labels=c("young","middle","senior","old"))

#문제 : hours-per-week에 대하여 유일값의 개수를 확인해보시오
min(AdultUCI["hours-per-week"])
max(AdultUCI["hours-per-week"])
count(unique(AdultUCI["hours-per-week"]))
AdultUCI[["hours-per-week"]]<-ordered(cut(AdultUCI[["hours-per-week"]],c(0,25,40,60,168)),
                                      labels=c("part-time","full-time","over-time","workaholic"))
AdultUCI[["hours-per-week"]]<-ordered(cut(AdultUCI[["hours-per-week"]],c(0,25,40,60,168)),labels=c("part_time","full-time","over-time","workaholic"))
AdultUCI[["capital-gain"]]<-ordered(cut(AdultUCI[["capital-gain"]],c(-Inf,0,median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]]>0]),Inf)),
                                    labels=c("None","Low","High"))
AdultUCI[["capital-loss"]]<-ordered(cut(AdultUCI[["capital-loss"]],c(-Inf,0,median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]]>0]),Inf)),
                                    labels=c("None","Low","High"))
AdultUCI["fnlwgt"]<-NULL
AdultUCI["education-num"]<-NULL
str(AdultUCI)
Adult_new<-as(AdultUCI,"transactions")
basket_rules<-apriori(Adult_new,parameter=list(sup=0.08, conf=0.5, target="rules"))
p<-inspectDT(basket_rules)
htmlwidgets::saveWidget(p,"arules_2.html",selfcontained = F)
browseURL("arules_2.html")

#문제 1: 가족관계 및 교육수준의 소득과의 연관성을 확인하시오
re<-subset(rules,lhs%pin%c("education","relationship") & rhs%pin%"income")
re_lf<-sort(re, by="lift", decreasing=T)
inspect(head(re_lf))

rule_s <- apriori(rules_s, parameter=list(supp=0.01,conf=0.01,minlen=2),appearance=list(
  rhs=c("relationship=Not-in-family","relationship=Husband","relationship=Wife","relationship=Own-child","relationship=Unmarried","relationship=Other-relative",
        "education=Bachelors","education=HS-grad","education=11th","education=Masters","education=9th","education=Some-college Assoc-acdm","education=Assoc-voc",
        "education=7th-8th","education=Doctorate","education=Prof-school","education=5th-6th","education=10th"),
  lhs=c("income=small","income=large")))
ri<-sort(rule_s,by="lift",decreasing = T)
inspect(ri)


#문제 2: 주당 일하는 시간과 소득과의 관계를 확인하시오
rule_s <- apriori(rules_s, parameter=list(supp=0.01,conf=0.01,minlen=2),appearance=list(
  rhs=c("hours-per-week=part-time","hours-per-week=full-time","hours-per-week=over-time","hours-per-week=workaholic"),
  lhs=c("income=small","income=large")))
hi<-sort(rule_s,by="lift",decreasing = T)
inspect(hi)
#문제 3: 기타 위의 데이터로부터 자기가 주장하고자하는 내용을 확인하고 의견을 제시하시오

#주당 일하는 시간과 결혼 상태의 관계
rule_s <- apriori(rules_s, parameter=list(supp=0.01,conf=0.01,minlen=2),appearance=list(
  rhs=c("hours-per-week=part-time","hours-per-week=full-time","hours-per-week=over-time","hours-per-week=workaholic"),
  lhs=c("marital-status=Never-married","marital-status=Married-civ-spouse","marital-status=Divorced","marital-status=Married-spouse-absent","marital-status=Separated","marital-status=Married-AF-spouse","marital-status=Widowed")))
hm<-sort(rule_s,by="lift",decreasing = T)
inspect(hm)

#인종과 소득의 관계
rule_s <- apriori(rules_s, parameter=list(supp=0.01,conf=0.01,minlen=2),appearance=list(
  rhs=c("race","race=White","race=Black","race=Asian-Pac-Islander","race=Amer-Indian-Eskimo"),
  lhs=c("income=small","income=large")))
rai<-sort(rule_s,by="lift",decreasing = T)
inspect(rai)
#파일 제출(2023_1101_이름_연관분석)