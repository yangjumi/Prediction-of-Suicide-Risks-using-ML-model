## K-mean clustering(1. 첫번째 방식)
# 데이터 전처리 및 데이터 파악
# 여기서의 데이터 사용은 " hn_final "
suicide <- read.csv(file="C://Users//User//Desktop//hn_final.csv", sep=",", header=TRUE)
suicide <- na.omit(suicide)
suicide_1 <- transform(suicide, suicide_bin =ifelse(suicide_bin == "yes", 1, 0))
head(suicide_1)
tail(suicide_1)
set.seed(100)

# K-means 알고리즘에 적합한 cluster 개수 찾기
install.packages("NbClust")
library(NbClust)
nc <- NbClust(suicide_1, min.nc = 2, max.nc = 15, method = "kmeans")
par(mfrow = c(1,1))
barplot(table(nc$Best.n[1,]), xlab = "Number of Clusters", 
        ylab = "Number of Criteria", main = "Number of Clusters Chosen")

# 데이터 scaling 하기
scaling.data <- scale(suicide_1)
summary(scaling.data)
str(scaling.data)

# 군집화 모델 생성하기 
suicide_kmeans <- kmeans(scaling.data, centers = 3, iter.max = 10000)
suicide_kmeans$centers
suicide_kmeans$cluster

# 군집 내 sse 구하기
suicide_kmeans$withinss

# 군집 모델 시각화하기(1)_mh_stress, ho_incm으로 나타내기
library(caret)
suicide_1$cluster <- as.factor(suicide_kmeans$cluster)
qplot(ho_incm, mh_stress, colour = cluster, data = suicide_1)

#  군집 모델 시각화하기(2)_전체 attribute에 대해 2차원으로 나타내기
library(cluster)
library(fpc)
plotcluster(scaling.data, suicide_kmeans$cluster)
clusplot(scaling.data, suicide_kmeans$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)





## K-mean clustering(2. 두번째 방식)
# 데이터 전처리 및 데이터 파악
# 여기서의 데이터 사용은 " hn_final "
df <- read.csv(file="C://Users//User//Desktop//hn_final.csv", sep=",", header=TRUE)
df <- na.omit(df)
df_2 <- transform(df, suicide_bin =ifelse(suicide_bin == "yes", 1, 0))
head(df_2)
tail(df_2)
set.seed(100)

# 최적의 cluster 개수
install.packages("NbClust")
library(NbClust)
N <- NbClust(df_2, min.nc = 2, max.nc = 10, method = "kmeans")
par(mfrow = c(1,1))
barplot(table(N$Best.n[1,]), xlab = "Number of Clusters", 
        ylab = "Number of Criteria", main = "Number of Clusters Chosen")

#  test, train data 분할 
install.packages("caret")
library(caret)
inTrain <- createDataPartition(y=df_2$suicide_bin, p=0.7, list=FALSE)
training <- df_2[inTrain,]
testing <- df_2[-inTrain,]
head(training)
head(testing)

# 데이터 scaling
training.data <- scale(training)
summary(training.data)
str(training.data)

# 군집 모델 생성
kmeans_model <- kmeans(training.data, centers = 4, iter.max = 10000)
kmeans_model$centers
kmeans_model$cluster
kmeans_model$withinss #sse 계산

par(mar=c(1,1,1,1))
plot(df[-21], pch = suicide_kmeans$cluster, col = suicide_kmeans$cluster)





## Hierachical clustering
#필요한 패키지 다운로드
requiredPackages=c("shiny","d3heatmap","RColorBrewer","DT","fpc","clValid","RankAggreg")
for(p in requiredPackages){
  if(!require(p,character.only=TRUE)) install.packages(p)
}
requiredPackages=c("flexclust","NbClust","rattle","cluster","fMultivar","ggplot2")
for(p in requiredPackages){
  if(!require(p,character.only=TRUE)) install.packages(p)
}

# 파일 저장
# 여기서의 데이터사용은 " hn_sip2 "
suicide_1 <- read.csv(file="C://Users//User//Desktop//hn_sip2.csv", sep=",", header=TRUE)
head(suicide_1,10)
tail(suicide_1)

#데이터 시각화
plot(suicide_1)

#거리계산 -> 각 페키지 별로 거리 계싼, 거리가 크다는 것은 관측치가 유사하지 않다는 것 의미
d=dist(suicide_1)
as.matrix(d)[1:4,1:4]

#계층적 군집방법(데이터수 150개 이하일때 사용) -> 엑셀 함수를 이용하여 자살위험군 50명과 비자살위험군 
hclust(d, method= "average")
suicide_1
#data(suicide_1,package="flexclust")
rownames(suicide_1)=tolower(rownames(suicide_1))
suicide_1.scaled=scale(suicide_1)

d=dist(suicide_1.scaled)
fit.average=hclust(d, method="average")
plot(fit.average,hang=-1,cex=.8,main="Average Linkage Clustering")

#군집 몇개로 나누어야 하는가 도출
library(NbClust)
devAskNewPage(ask=TRUE)
nc=NbClust(suicide_1.scaled,distance="euclidean",min.nc=2,max.nc=15,
           method="average") 
devAskNewPage(ask=FALSE)

#이미지로 표현(2,3 추천인데)
table(nc$Best.n[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),xlab="Number of Clusters",
        ylab="Number of Criteria",main="Number of Clusters Chosen by 26 criteria")

#3로하면 
clusters<-cutree(fit.average,k=3)
table(clusters)
clusters

# 클러스터가 비대칭적으로 배분됨 -> 스케일링
# scaling 한 값을 통해 중압 값으로 aggregation
aggregate(as.data.frame(suicide_1.scaled),by=list(cluster=clusters),median)

# 분석결과 시각화
plot(fit.average,hang=-1,cex=.8,
     main="Average Linkage Clustering\n3 Cluster Solution")
rect.hclust(fit.average,k=3)




## Association analysis_(1. 비자살위험군관련)
install.packages("arules")
library(arules)
unloadNamespace("arules") 
update.packages("arules") 
library(arules)

# 여기서의 데이터 사용은 " hn_final0 "
suicide_asso <- read.csv(file="C://Users//User//Desktop//hn_final0.csv", sep=",", header=TRUE)
library("ggplot2")
head(suicide_asso)

#age 변수 범주화
suicide_asso$age_20 <-ifelse(suicide_asso$age < 30 ,1,0)
suicide_asso$age_30 <-ifelse(suicide_asso$age >= 30 & suicide_asso$age < 40 ,1,0)
suicide_asso$age_40 <-ifelse(suicide_asso$age >= 40 & suicide_asso$age < 50 ,1,0)
suicide_asso$age_50 <-ifelse(suicide_asso$age >= 50 & suicide_asso$age < 60 ,1,0)
suicide_asso$age_60 <-ifelse(suicide_asso$age >= 60 & suicide_asso$age < 70 ,1,0)
suicide_asso$age_70 <-ifelse(suicide_asso$age >= 70 & suicide_asso$age < 80 ,1,0)
suicide_asso$age_80 <-ifelse(suicide_asso$age >= 80 & suicide_asso$age < 90 ,1,0)
#HE_ht
suicide_asso$HE_ht_50 <- ifelse(suicide_asso$HE_ht <150 ,1,0)
suicide_asso$HE_ht_60 <- ifelse(suicide_asso$HE_ht>=160&suicide_asso$HE_ht<170,1,0)
suicide_asso$HE_ht_70 <- ifelse(suicide_asso$HE_ht>=170&suicide_asso$HE_ht<180,1,0)
suicide_asso$HE_ht_80 <- ifelse(suicide_asso$HE_ht>=180,1,0)
#incm 
suicide_asso$incm_1 <-ifelse(suicide_asso$incm == 1 | suicide_asso$incm == 2 ,1,0)
suicide_asso$incm_2 <-ifelse(suicide_asso$incm == 3 | suicide_asso$incm == 4 ,1,0)
#ho_incm
suicide_asso$ho_incm_1 <-ifelse(suicide_asso$ho_incm == 1 | suicide_asso$ho_incm == 2 ,1,0)
suicide_asso$ho_incm_2 <-ifelse(suicide_asso$ho_incm == 3 | suicide_asso$ho_incm == 4 ,1,0)
#ho_incm5
suicide_asso$ho_incm5_1 <-ifelse(suicide_asso$ho_incm5 == 1 | suicide_asso$ho_incm5 == 2 ,1,0)
suicide_asso$ho_incm5_2 <-ifelse(suicide_asso$ho_incm5 == 3 | suicide_asso$ho_incm5 == 4 ,1,0)
suicide_asso$ho_incm5_3 <-ifelse(suicide_asso$ho_incm5 == 5 ,1,0)
#allownc
replace(suicide_asso$allownc , suicide_asso$allownc==10, 1) -> suicide_asso$allownc
replace(suicide_asso$allownc , suicide_asso$allownc==20, 0) -> suicide_asso$allownc
#BP1
suicide_asso$BP1_1 <-ifelse(suicide_asso$BP1 == 1 | suicide_asso$BP1 == 2 ,1,0)
suicide_asso$BP1_2 <-ifelse(suicide_asso$BP1 == 3 | suicide_asso$BP1 == 4 ,1,0)
#BP7
replace(suicide_asso$BP7 , suicide_asso$BP7==1, 1) -> suicide_asso$BP7
replace(suicide_asso$BP7 , suicide_asso$BP7==2, 0) -> suicide_asso$BP7
#HE_obe
suicide_asso$HE_obe_1 <-ifelse(suicide_asso$HE_obe == 1 ,1,0)
suicide_asso$HE_obe_2 <-ifelse(suicide_asso$HE_obe == 2 ,1,0)
suicide_asso$HE_obe_3 <-ifelse(suicide_asso$HE_obe == 3 ,1,0)
#BM7
suicide_asso$BM7_1 <-ifelse(suicide_asso$BM7 == 1 | suicide_asso$BM7 == 2 ,1,0)
suicide_asso$BM7_2 <-ifelse(suicide_asso$BM7 == 3 | suicide_asso$BM7 == 4 | suicide_asso$BM7 == 5 ,1,0)
#BM8
suicide_asso$BM8_1 <-ifelse(suicide_asso$BM8 == 1 | suicide_asso$BM8 == 2 ,1,0)
suicide_asso$BM8_2 <-ifelse(suicide_asso$BM8 == 3 | suicide_asso$BM8 == 4 | suicide_asso$BM8 == 5 ,1,0)
#BM14
replace(suicide_asso$BM14 , suicide_asso$BM14==1, 1) -> suicide_asso$BM14
replace(suicide_asso$BM14 , suicide_asso$BM14==2 | suicide_asso$BM14 == 3, 0) -> suicide_asso$BM14
#suicide_bin
transform(suicide_asso, suicide_bin =ifelse(suicide_bin == "no", 0, 1)) -> suicide_asso

#새로 만든 변수가 대체하므로 원래 변수 제거
suicide_asso <- subset(suicide_asso, select = -c(age, HE_ht, incm, ho_incm, ho_incm5, BP1, HE_obe, BM7, BM8))
head(suicide_asso)
#wt_itvex, ainc_1, ainc, HE_dbp3, HE_RBC, HE_dbp  범주화
summary(suicide_asso$wt_itvex)
suicide_asso$wt_itvex<-ifelse(suicide_asso$wt_itvex < 6568.211925, 0 ,1)
summary(suicide_asso$ainc)
suicide_asso$ainc<-ifelse(suicide_asso$ainc>=1000 , 1, 0)
summary(suicide_asso$ainc_1)
suicide_asso$ainc_1<-ifelse(suicide_asso$ainc_1 < 200 , 0, 1)
summary(suicide_asso$HE_dbp3)
suicide_asso$HE_dbp3 <-ifelse(suicide_asso$HE_dbp3 < 130, 1, 0)
summary(suicide_asso$HE_dbp)
suicide_asso$HE_dbp<-ifelse(suicide_asso$HE_dbp < 130, 1, 0)
summary(suicide_asso$HE_RBC)
suicide_asso$HE_RBC <-ifelse(suicide_asso$HE_RBC < 4.618 , 0, 1)
head(suicide_asso)

#연관분석에 이용되는 거래 데이터 형태로 변환
suicide_asso$wt_itvex <- as.factor(suicide_asso$wt_itvex)
suicide_asso$allownc <- as.factor(suicide_asso$allownc)
suicide_asso$ainc <- as.factor(suicide_asso$ainc)
suicide_asso$ainc_1 <- as.factor(suicide_asso$ainc_1)
suicide_asso$BP7 <- as.factor(suicide_asso$BP7)
suicide_asso$mh_stress <- as.factor(suicide_asso$mh_stress)
suicide_asso$HE_dbp3 <- as.factor(suicide_asso$HE_dbp3)
suicide_asso$HE_ht_50 <- as.factor(suicide_asso$HE_ht_50)
suicide_asso$HE_ht_60 <- as.factor(suicide_asso$HE_ht_60)
suicide_asso$HE_ht_70 <- as.factor(suicide_asso$HE_ht_70)
suicide_asso$HE_ht_80 <- as.factor(suicide_asso$HE_ht_80)
suicide_asso$O_chew_d <- as.factor(suicide_asso$O_chew_d)
suicide_asso$HE_dbp <- as.factor(suicide_asso$HE_dbp)
suicide_asso$HE_RBC <- as.factor(suicide_asso$HE_RBC)
suicide_asso$BM14 <- as.factor(suicide_asso$BM14)
suicide_asso$suicide_bin <- as.factor(suicide_asso$suicide_bin)
suicide_asso$age_20 <- as.factor(suicide_asso$age_20)
suicide_asso$age_30 <- as.factor(suicide_asso$age_30)
suicide_asso$age_40 <- as.factor(suicide_asso$age_40)
suicide_asso$age_50 <- as.factor(suicide_asso$age_50)
suicide_asso$age_60 <- as.factor(suicide_asso$age_60)
suicide_asso$age_70 <- as.factor(suicide_asso$age_70)
suicide_asso$age_80 <- as.factor(suicide_asso$age_80)
suicide_asso$incm_1 <- as.factor(suicide_asso$incm_1)
suicide_asso$incm_2 <- as.factor(suicide_asso$incm_2)
suicide_asso$ho_incm_1 <- as.factor(suicide_asso$ho_incm_1)
suicide_asso$ho_incm_2 <- as.factor(suicide_asso$ho_incm_2)
suicide_asso$ho_incm5_1 <- as.factor(suicide_asso$ho_incm5_1)
suicide_asso$ho_incm5_2 <- as.factor(suicide_asso$ho_incm5_2)
suicide_asso$ho_incm5_3 <- as.factor(suicide_asso$ho_incm5_3)
suicide_asso$BP1_1 <- as.factor(suicide_asso$BP1_1)
suicide_asso$BP1_2 <- as.factor(suicide_asso$BP1_2)
suicide_asso$HE_obe_1 <- as.factor(suicide_asso$HE_obe_1)
suicide_asso$HE_obe_2 <- as.factor(suicide_asso$HE_obe_2)
suicide_asso$HE_obe_3 <- as.factor(suicide_asso$HE_obe_3)
suicide_asso$BM7_1 <- as.factor(suicide_asso$BM7_1)
suicide_asso$BM7_2 <- as.factor(suicide_asso$BM7_2)
suicide_asso$BM8_1 <- as.factor(suicide_asso$BM8_1)
suicide_asso$BM8_2 <- as.factor(suicide_asso$BM8_2)
suicide_trans <- as(suicide_asso, "transactions")
suicide_trans
summary(suicide_trans)
memory.limit(size = 50000) 
options(max.print=1000)
suicide_rules <- apriori(suicide_trans, parameter = list(support = 0.4, confidence = 0.8))
inspect(suicide_rules)

#support 값이 높은 순서로 규칙 결과 나열하기
inspect(sort(suicide_rules, by = "support")[1:20])

#의사결정 나무에 나왔던 변수들로만 이루어진 연관 규칙만 찾기
rule_interest <- subset(suicide_rules, items %in% c("mh_stress=1", "O_chew_d=1", "suicide_bin=1"))
inspect(rule_interest[1:200])

#오른쪽 결과가 항상 suicide_bin인 규칙찾기
rule_interest_rhs <- subset(suicide_rules, rhs %in% c("suicide_bin=1"))
inspect(rule_interest_rhs[1:200])






## Association analysis_(2. 자살위험군관련)
install.packages("arules")
library(arules)
unloadNamespace("arules") 
update.packages("arules") 
library(arules)

# 여기서의 데이터 사용은 " hn_final1 "
suicide_asso <- read.csv(file="C://Users//Com//Desktop//team//hn_final1.csv", sep=",", header=TRUE)
library("ggplot2")
head(suicide_asso)

#age 변수 범주화
suicide_asso$age_20 <-ifelse(suicide_asso$age < 30 ,1,0)
suicide_asso$age_30 <-ifelse(suicide_asso$age >= 30 & suicide_asso$age < 40 ,1,0)
suicide_asso$age_40 <-ifelse(suicide_asso$age >= 40 & suicide_asso$age < 50 ,1,0)
suicide_asso$age_50 <-ifelse(suicide_asso$age >= 50 & suicide_asso$age < 60 ,1,0)
suicide_asso$age_60 <-ifelse(suicide_asso$age >= 60 & suicide_asso$age < 70 ,1,0)
suicide_asso$age_70 <-ifelse(suicide_asso$age >= 70 & suicide_asso$age < 80 ,1,0)
suicide_asso$age_80 <-ifelse(suicide_asso$age >= 80 & suicide_asso$age < 90 ,1,0)
#HE_ht
suicide_asso$HE_ht_50 <- ifelse(suicide_asso$HE_ht <150 ,1,0)
suicide_asso$HE_ht_60 <- ifelse(suicide_asso$HE_ht>=160&suicide_asso$HE_ht<170,1,0)
suicide_asso$HE_ht_70 <- ifelse(suicide_asso$HE_ht>=170&suicide_asso$HE_ht<180,1,0)
suicide_asso$HE_ht_80 <- ifelse(suicide_asso$HE_ht>=180,1,0)
#incm 
suicide_asso$incm_1 <-ifelse(suicide_asso$incm == 1 | suicide_asso$incm == 2 ,1,0)
suicide_asso$incm_2 <-ifelse(suicide_asso$incm == 3 | suicide_asso$incm == 4 ,1,0)
#ho_incm
suicide_asso$ho_incm_1 <-ifelse(suicide_asso$ho_incm == 1 | suicide_asso$ho_incm == 2 ,1,0)
suicide_asso$ho_incm_2 <-ifelse(suicide_asso$ho_incm == 3 | suicide_asso$ho_incm == 4 ,1,0)
#ho_incm5
suicide_asso$ho_incm5_1 <-ifelse(suicide_asso$ho_incm5 == 1 | suicide_asso$ho_incm5 == 2 ,1,0)
suicide_asso$ho_incm5_2 <-ifelse(suicide_asso$ho_incm5 == 3 | suicide_asso$ho_incm5 == 4 ,1,0)
suicide_asso$ho_incm5_3 <-ifelse(suicide_asso$ho_incm5 == 5 ,1,0)
#allownc
replace(suicide_asso$allownc , suicide_asso$allownc==10, 1) -> suicide_asso$allownc
replace(suicide_asso$allownc , suicide_asso$allownc==20, 0) -> suicide_asso$allownc
#BP1
suicide_asso$BP1_1 <-ifelse(suicide_asso$BP1 == 1 | suicide_asso$BP1 == 2 ,1,0)
suicide_asso$BP1_2 <-ifelse(suicide_asso$BP1 == 3 | suicide_asso$BP1 == 4 ,1,0)
#BP7
replace(suicide_asso$BP7 , suicide_asso$BP7==1, 1) -> suicide_asso$BP7
replace(suicide_asso$BP7 , suicide_asso$BP7==2, 0) -> suicide_asso$BP7
#HE_obe
suicide_asso$HE_obe_1 <-ifelse(suicide_asso$HE_obe == 1 ,1,0)
suicide_asso$HE_obe_2 <-ifelse(suicide_asso$HE_obe == 2 ,1,0)
suicide_asso$HE_obe_3 <-ifelse(suicide_asso$HE_obe == 3 ,1,0)
#BM7
suicide_asso$BM7_1 <-ifelse(suicide_asso$BM7 == 1 | suicide_asso$BM7 == 2 ,1,0)
suicide_asso$BM7_2 <-ifelse(suicide_asso$BM7 == 3 | suicide_asso$BM7 == 4 | suicide_asso$BM7 == 5 ,1,0)
#BM8
suicide_asso$BM8_1 <-ifelse(suicide_asso$BM8 == 1 | suicide_asso$BM8 == 2 ,1,0)
suicide_asso$BM8_2 <-ifelse(suicide_asso$BM8 == 3 | suicide_asso$BM8 == 4 | suicide_asso$BM8 == 5 ,1,0)
#BM14
replace(suicide_asso$BM14 , suicide_asso$BM14==1, 1) -> suicide_asso$BM14
replace(suicide_asso$BM14 , suicide_asso$BM14==2 | suicide_asso$BM14 == 3, 0) -> suicide_asso$BM14
#suicide_bin
transform(suicide_asso, suicide_bin =ifelse(suicide_bin == "no", 0, 1)) -> suicide_asso

#새로 만든 변수가 대체하므로 원래 변수 제거
suicide_asso <- subset(suicide_asso, select = -c(age, HE_ht, incm, ho_incm, ho_incm5, BP1, HE_obe, BM7, BM8))
head(suicide_asso)
#wt_itvex, ainc_1, ainc, HE_dbp3, HE_RBC, HE_dbp  범주화
summary(suicide_asso$wt_itvex)
suicide_asso$wt_itvex<-ifelse(suicide_asso$wt_itvex < 6568.211925, 0 ,1)
summary(suicide_asso$ainc)
suicide_asso$ainc<-ifelse(suicide_asso$ainc>=1000 , 1, 0)
summary(suicide_asso$ainc_1)
suicide_asso$ainc_1<-ifelse(suicide_asso$ainc_1 < 200 , 0, 1)
summary(suicide_asso$HE_dbp3)
suicide_asso$HE_dbp3 <-ifelse(suicide_asso$HE_dbp3 < 130, 1, 0)
summary(suicide_asso$HE_dbp)
suicide_asso$HE_dbp<-ifelse(suicide_asso$HE_dbp < 130, 1, 0)
summary(suicide_asso$HE_RBC)
suicide_asso$HE_RBC <-ifelse(suicide_asso$HE_RBC < 4.618 , 0, 1)
head(suicide_asso)

#연관분석에 이용되는 거래 데이터 형태로 변환
suicide_asso$wt_itvex <- as.factor(suicide_asso$wt_itvex)
suicide_asso$allownc <- as.factor(suicide_asso$allownc)
suicide_asso$ainc <- as.factor(suicide_asso$ainc)
suicide_asso$ainc_1 <- as.factor(suicide_asso$ainc_1)
suicide_asso$BP7 <- as.factor(suicide_asso$BP7)
suicide_asso$mh_stress <- as.factor(suicide_asso$mh_stress)
suicide_asso$HE_dbp3 <- as.factor(suicide_asso$HE_dbp3)
suicide_asso$HE_ht_50 <- as.factor(suicide_asso$HE_ht_50)
suicide_asso$HE_ht_60 <- as.factor(suicide_asso$HE_ht_60)
suicide_asso$HE_ht_70 <- as.factor(suicide_asso$HE_ht_70)
suicide_asso$HE_ht_80 <- as.factor(suicide_asso$HE_ht_80)
suicide_asso$O_chew_d <- as.factor(suicide_asso$O_chew_d)
suicide_asso$HE_dbp <- as.factor(suicide_asso$HE_dbp)
suicide_asso$HE_RBC <- as.factor(suicide_asso$HE_RBC)
suicide_asso$BM14 <- as.factor(suicide_asso$BM14)
suicide_asso$suicide_bin <- as.factor(suicide_asso$suicide_bin)
suicide_asso$age_20 <- as.factor(suicide_asso$age_20)
suicide_asso$age_30 <- as.factor(suicide_asso$age_30)
suicide_asso$age_40 <- as.factor(suicide_asso$age_40)
suicide_asso$age_50 <- as.factor(suicide_asso$age_50)
suicide_asso$age_60 <- as.factor(suicide_asso$age_60)
suicide_asso$age_70 <- as.factor(suicide_asso$age_70)
suicide_asso$age_80 <- as.factor(suicide_asso$age_80)
suicide_asso$incm_1 <- as.factor(suicide_asso$incm_1)
suicide_asso$incm_2 <- as.factor(suicide_asso$incm_2)
suicide_asso$ho_incm_1 <- as.factor(suicide_asso$ho_incm_1)
suicide_asso$ho_incm_2 <- as.factor(suicide_asso$ho_incm_2)
suicide_asso$ho_incm5_1 <- as.factor(suicide_asso$ho_incm5_1)
suicide_asso$ho_incm5_2 <- as.factor(suicide_asso$ho_incm5_2)
suicide_asso$ho_incm5_3 <- as.factor(suicide_asso$ho_incm5_3)
suicide_asso$BP1_1 <- as.factor(suicide_asso$BP1_1)
suicide_asso$BP1_2 <- as.factor(suicide_asso$BP1_2)
suicide_asso$HE_obe_1 <- as.factor(suicide_asso$HE_obe_1)
suicide_asso$HE_obe_2 <- as.factor(suicide_asso$HE_obe_2)
suicide_asso$HE_obe_3 <- as.factor(suicide_asso$HE_obe_3)
suicide_asso$BM7_1 <- as.factor(suicide_asso$BM7_1)
suicide_asso$BM7_2 <- as.factor(suicide_asso$BM7_2)
suicide_asso$BM8_1 <- as.factor(suicide_asso$BM8_1)
suicide_asso$BM8_2 <- as.factor(suicide_asso$BM8_2)
suicide_trans <- as(suicide_asso, "transactions")
suicide_trans
summary(suicide_trans)
memory.limit(size = 50000) 
options(max.print=1000)
suicide_rules <- apriori(suicide_trans, parameter = list(support = 0.4, confidence = 0.8))
inspect(suicide_rules)

#support 값이 높은 순서로 규칙 결과 나열하기
inspect(sort(suicide_rules, by = "support")[1:20])

#의사결정 나무에 나왔던 변수들로만 이루어진 연관 규칙만 찾기
rule_interest <- subset(suicide_rules, items %in% c("mh_stress=1", "O_chew_d=1", "suicide_bin=1"))
inspect(rule_interest[1:200])

#오른쪽 결과가 항상 suicide_bin인 규칙찾기
rule_interest_rhs <- subset(suicide_rules, rhs %in% c("suicide_bin=1"))
inspect(rule_interest_rhs[1:200])