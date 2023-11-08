
sub<-read.csv(file.choose())

sub<-read.csv("C:/Users/401-17/Documents/카카오톡 받은 파일/공장등록현황통계_시도별 용도지역별 현황_17.csv", stringsAsFactors = TRUE,header = TRUE,encoding ='UTF-8', fileEncoding = "euc-kr")
str(sub)
sub<-as.data.frame(sub)
library(data.table)
library(RMySQL)

con<-dbConnect(MySQL(),user="root",password="8325338a!",dbname="munji",host="localhost",port=3306)
dbListTables(con)
sub=t(sub)
dbWriteTable(con,"facpop_19",fp19,overwrite=TRUE)
(f18=dbReadTable(con,"fac_17"))
(p18=dbReadTable(con,"pop_17"))
(f20=dbReadTable(con,"fac_19"))
(p20=dbReadTable(con,"pop_19"))
(fp17<-cbind(f18,p18))
(fp19<-cbind(f20,p20))
