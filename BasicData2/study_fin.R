#install.packages("rlang")
# 파일_1 : 2017~해촉자, 파일_2 : 2017_해촉자 비대면 feature
# 파일_3 : 2021.04~신규 위촉/해촉 - 테스트를 위한 테스트 set
# mydata = read_excel("data/202107_해촉.xlsx")


library(readxl)
library(foreign) # spss파일 로드
library(dplyr) # 전처리 
library(ggplot2) ##ggplot2 로드 : 시각화 
library(readxl) # 엑셀 파일 불러오기
library(rlang)


# 파일 READ
fi_1 <- read.csv("data/202107_DSMS.csv",stringsAsFactors = FALSE)
fi_2 <- read.csv("data/202107_해촉_new_feature.csv",stringsAsFactors = FALSE)
fi_test <- read.csv("data/202107_test_set.csv",stringsAsFactors = FALSE)

#구성 확인 
str(fi_1)
str(fi_2)
str(fi_test)

# om_no 사원번호로 inner join 
# join한 최종 데이터 추출함 
fi_fin <- inner_join(fi_1,fi_2,c('OM_NO'='OM_NO'))

#구성 확인
str(fi_fin)
fi_fin

# fi_fin (최종본) 요약 
summary(fi_fin)

# 첫번쨰 ROW만 출력 
fi_fin[1,]

# 특정 컬럼만 추출하기 
# 사원번호, 성별구분, 위촉일자 ,해촉일자, 신분코드, 해촉당시 나이, 차월
# 3개월평균소득, 3개월평균신계약건수, 오전출근일수, 오후귀사일수, 2-7 통산유지율
# 유지계약건수, 유지고객수, 고객방문대상수, 고객방문수, 전자청약건수, 스마트온(핸드폰)청약건수
# 물품서비스건수, 비대면건수, 모바일건수 
df_fin <- fi_fin %>%
  select(OM_NO,DSMS_YN,SDT_DV_CD,ET_YM,DSMS_YM,COT_SCPS_CD,DISMISS_AGE,COT_ET_EMNM,
         INCM_AVG_3,SIN_AVG_3,AM_LGIN_CNT_1,PPM_LGIN_CNT_1,YUJIYUL_27,
         HLD_CNR_NCN,HLD_CNOR_CNT,GNR_VST_TGT_CNT,GNR_VST_CNT,
         ELEC_SB_NCN_1,MBL_ELEC_SB_NCN_1,WLS_ARTC_SVC_TOT_NCN,WLS_NF_NCN,WLS_MBL_TOT_NCN)

# 열이름 출력 
names(df_fin) 

# BACKUP
df_B <- df_fin

# 열 이름 변경 
names(df_fin) <- c("사원번호","해촉여부","성별","위촉월","해촉월","신분","나이","차월","평균소득",
               "평균신계약건수","오전출근일수","오후귀사일수","통산유지율",
               "유지계약건수","유지고객수","고객방문대상수","고객방문수",
               "태블릿청약건수","스마트폰청약건수","물품서비스건수","비대면건수","모바일건수")

# 열이름 출력 
names(df_fin) 

str(df_fin)

# 데이터 확인
df_fin[1,]

# 테스트 set 가공 
test_df <- fi_test %>%
  select(OM_NO,DSMS_YN,SDT_DV_CD,ET_YM,DSMS_YM,COT_SCPS_CD,DISMISS_AGE,COT_ET_EMNM,
         INCM_AVG_3,SIN_AVG_3,AM_LGIN_CNT_1,PPM_LGIN_CNT_1,YUJIYUL_27,
         HLD_CNR_NCN,HLD_CNOR_CNT,GNR_VST_TGT_CNT,GNR_VST_CNT,
         ELEC_SB_NCN_1,MBL_ELEC_SB_NCN_1,WLS_ARTC_SVC_TOT_NCN,WLS_NF_NCN,WLS_MBL_TOT_NCN)

names(test_df) 

# 열 이름 변경 
names(test_df) <- c("사원번호","해촉여부","성별","위촉월","해촉월","신분","나이","차월","평균소득",
                    "평균신계약건수","오전출근일수","오후귀사일수","통산유지율",
                    "유지계약건수","유지고객수","고객방문대상수","고객방문수",
                    "태블릿청약건수","스마트폰청약건수","물품서비스건수","비대면건수","모바일건수")

# 데이터 컬럼 및 데이터 확인 
names(test_df) 
test_df[1,]

plot(df_fin, col = df_fin$해촉여부)   


# 월도별 해촉자 그래프 출력 
# 월도별 해촉자 추출 
df_dsmsYM <- df_fin %>%
  filter(해촉여부 == 1) %>% 
  group_by(해촉월) %>% 
  summarise(n=n()) %>%  
  arrange(desc(해촉월)) %>% 
  select(해촉월,n) %>% 
  head(12)

df_dsmsYM
names(df_dsmsYM)

##-----------------------------------------------
##해촉자 월별 추이 
# 막대그래프 그리기 
ggplot(data = df_dsmsYM, aes(x = 해촉월 , y = n),fill = value) + geom_col() +
  ggtitle("월별해촉차추이") + 
  geom_text(aes(label = n), vjust = -0.3) +
  geom_bar(stat = 'identity',fill = 'royalblue') +
  theme(plot.title = element_text(hjust = 0.5))


# 선그래프
ggplot(data = df_dsmsYM, aes(x = 해촉월 , y = n, group=1))  +
  ggtitle("월별해촉차추이") + 
  geom_line(color="red", size = 1)+
  geom_point() +
  geom_text(aes(label = n), vjust = -0.8, size = 3)






#########----------------------
# Random forest
# randomForest / caret : 성능을 평가하기 위해
# tidymodels 데이터를 자르기 위해
# randomForest(종속변수 ~ 독립 변수, data = df)
#install.packages("randomForest")
#install.packages("caret")
#install.packages("tidymodels")
#install.packages('xtable')
#install.packages('randomForestExplainer')

library(randomForest) 
library(caret)
library(tidymodels)
library(caret)
library(xtable)
library(randomForestExplainer)

##----------테스트셋 분리 
# 80% train / 20% test
train_tot <- sample(1:nrow(df_fin), size = nrow(df_fin)*0.8)     

train_R <- df_fin[train_tot,]   
test_R <- df_fin[-train_tot,]  

#------------------------------------
#decision tree
#install.packages("rpart")
library(rpart) #rpart()함수 포함 패키지

#install.packages("rattle")
#install.packages("rpart.plot")

library(rpart.plot)
tree <- rpart(해촉여부~성별+나이+차월+평균소득+평균신계약건수
                  +오전출근일수+오후귀사일수+통산유지율
                  +유지고객수+고객방문대상수+고객방문수+태블릿청약건수
                  +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수
                  , data=train_R, method = "class")
rpart.plot(tree)
summary(tree)

printcp(tree)

# 가지치기 기준이 되는 복잡도(Complexity parameter)
pruned_tree <- prune(tree, cp = 0.1) #cp =0.05로 가지치기

rpart.plot(pruned_tree)

t1 <- table(test_R$해촉여부, predict(pruned_tree, test_R, type = "class"))
t1

t2 <- table(test_R$해촉여부, predict(tree, test_R, type = "class"))
t2

# 정오분류표를 간단하게 그래프
test_R$pred <- predict(tree, test_R, type = "class")  #Test셋에 대한 예측분류
library(ggplot2)

ggplot(test_R, aes(해촉여부, pred, color = 해촉여부)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Confusion Matrix", 
       subtitle="Predicted vs Observed from Dsms dataset", 
       y="Predicted", 
       x="Truth")


#####------------------------------------
## RandomForest 
train_R$해촉여부 <- as.factor(train_R$해촉여부)
test_R$해촉여부 <- as.factor(test_R$해촉여부)

# 모델 생성 mtry: 투입할 FEATURE 개수 
randomForest(해촉여부~성별+나이+차월+평균소득+평균신계약건수
                 +오전출근일수+오후귀사일수+통산유지율+유지계약건수
                 +유지고객수+고객방문대상수+고객방문수+태블릿청약건수
                 +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수,
                 data=train_R, mtry = 12,
                 ntree = 500,
                 importance = TRUE  
                 ) -> random_R 
# 모델 결과 
random_R

# 각항목별 중요도 
importance(random_R)
varImpPlot(random_R, type=2, pch=17, col=1, cex=1, main="")

# 내부의 의사결정 트리가 증가함에 따라 추이 
head(random_R$err.rate)
plot(random_R$err.rate[,1],col='red')


# 학습된 모델에 TEST SET 셋팅 
test_R$y_pred = predict(random_R, test_R, type = "class") 
y_pred

library(caret)
library(e1071)

# confusionMatrix (정확도등.. 구하기 )
confusionMatrix(test_R$y_pred,  test_R$해촉여부)

library(ggplot2)


ggplot(test_R, aes(해촉여부, y_pred, color = 해촉여부)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Confusion Matrix", 
       subtitle="Predicted vs Observed from Dsms dataset", 
       y="Predicted", 
       x="Truth")

##---------------------------------------------------
#xgboost
library(tidyverse)
library(xgboost)
x_columns = c("성별","나이","차월","평균소득",
              "평균신계약건수","오전출근일수","오후귀사일수","통산유지율",
              "유지계약건수","유지고객수","고객방문대상수","고객방문수",
              "태블릿청약건수","스마트폰청약건수","물품서비스건수","비대면건수","모바일건수")


train.data  = as.matrix(train_R[, names(train_R) %in% x_columns])
test.data   = as.matrix(test_R[ , names(test_R)  %in% x_columns])
train.label = train_R$해촉여부 # 0,1기반
test.label  = test_R$해촉여부  # 0,1기반

xg_model <- xgboost(data=train.data, label=train.label, max.depth = 1000,
                    eta = 1, nthread = 20, nround = 1000)

xg_model

# xg_boost 테스트 
pred <- predict(xg_model, test.data)
pred <- ifelse(pred >0.2, 1, 0)
table(pred, test.label)
# 예측도 0.617
sum(pred==test.label)/length(pred)

##-----------------------------------------------
# logistic 
logism = glm(해촉여부~성별+나이+차월+평균소득+평균신계약건수 + 유지계약건수
                         +오전출근일수+오후귀사일수+통산유지율
                         +유지고객수+고객방문대상수+고객방문수+태블릿청약건수
                         +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수
                         , train_R
                         , family="binomial") # 이진 분류로 설정 
summary(logism)

# 이탈도 // pr>chi 가 0.05 초과하면 유의하지 않다고 판단할 수 있기 떄문에
# 유의 유지고객수 : 0.612 -> 제거 
anova(logism, test="Chisq")
installed.packages("ROCR")

exp(coef(glm(해촉여부~성별+나이+차월+평균소득+평균신계약건수 + 유지계약건수
                 +오전출근일수+오후귀사일수+통산유지율
                 +고객방문대상수+고객방문수+태블릿청약건수
                 +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수
                 , train_R
                 , family="binomial"))) # 이진 분류로 설정 

exp(confint(glm(해촉여부~성별+나이+차월+평균소득+평균신계약건수 + 유지계약건수
                 +오전출근일수+오후귀사일수+통산유지율
                 +고객방문대상수+고객방문수+태블릿청약건수
                 +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수
                 , train_R
                 , family="binomial"))) # 이진 분류로 설정 

cc_fit_prob_dt <- augment(logism,type.predict = "response") %>% 
  mutate(y_hat = .fitted)

ggplot(cc_fit_prob_dt,aes(x=태블릿청약건수,y=y_hat)) +
  geom_jitter(with=0.5,height = 0.0 , alpha = 0.5 )+
  geom_smooth() +
  scale_y_continuous("태블릿청약건수",limits=c(0,1))


# 입력 데이터를 기반으로 특정 분류로 나뉘기 때문에 분류기법이라고
# 



# 로지스틱 회귀에서 예측 (test set 으로 진행 )
Log_odds = predict(logism, newdata = test_R)
Probability = predict(logism, newdata = test_R, type = 'response')

# 0.5 보다 높으면 1로 그외는 0으로 분류 
PREDICTED_C = ifelse(Probability > 0.5 , 1 , 0)
PREDICTED_C = as.factor(PREDICTED_C)
PREDICTED_C

# 혼동 행렬 구하기 
library(caret)
confusionMatrix(test_R$해촉여부,PREDICTED_C)

exp(coef(logism))



#유의하지 않은 변수 삭제 유지고객수 0.6으로 삭제 
reducedm = step(logism,direction = "backward") # 정확도 94.39 
summary(reducedm)

# 
library(pROC)

ROC = roc(test_R$해촉여부,Probability)

plot.roc(ROC,
         col="royalblue",
         print.auc = TRUE,
         max.auc.polygon=TRUE,
         print.thres = TRUE, print.thres.pch = 19, print.thres.col = "red",
         auc.polygon = TRUE, auc.polygon.col = "#D1F2EB")

# AUC 값이 높을 수록 잘 분류하는 모델 AUC 면적이 높을 수록 좋은 좋은모델 
# 세로축은 민감도를 나타내며 가로축은 특이도를 나타냅니다. 
##------------------------------------------------

# 인공신경망 적용
# 데이터 표준화 필요 
#install.packages('neuralnet')
library(neuralnet)
library(devtools)

#샘플 데이터 정규화 기능 - 결과의 동일여부 
df_ne <- df_fin

# 정규화작엄 
df_ne$나이 = (df_ne$나이 - min(df_ne$나이)) / (max(df_ne$나이)-min(df_ne$나이))
df_ne$차월 = (df_ne$차월 - min(df_ne$차월)) / (max(df_ne$차월)-min(df_ne$차월))
df_ne$평균소득 = (df_ne$평균소득 - min(df_ne$평균소득)) / (max(df_ne$평균소득)-min(df_ne$평균소득))
df_ne$평균신계약건수 = (df_ne$평균신계약건수 - min(df_ne$평균신계약건수)) / (max(df_ne$평균신계약건수)-min(df_ne$평균신계약건수))
df_ne$오전출근일수 = (df_ne$오전출근일수 - min(df_ne$오전출근일수)) / (max(df_ne$오전출근일수)-min(df_ne$오전출근일수))
df_ne$오후귀사일수 = (df_ne$오후귀사일수 - min(df_ne$오후귀사일수)) / (max(df_ne$오후귀사일수)-min(df_ne$오후귀사일수))
df_ne$통산유지율 = (df_ne$통산유지율 - min(df_ne$통산유지율)) / (max(df_ne$통산유지율)-min(df_ne$통산유지율))
df_ne$유지고객수 = (df_ne$유지고객수 - min(df_ne$유지고객수)) / (max(df_ne$유지고객수)-min(df_ne$유지고객수))
df_ne$고객방문대상수 = (df_ne$고객방문대상수 - min(df_ne$고객방문대상수)) / (max(df_ne$고객방문대상수)-min(df_ne$고객방문대상수))
df_ne$고객방문수 = (df_ne$고객방문수 - min(df_ne$고객방문수)) / (max(df_ne$고객방문수)-min(df_ne$고객방문수))
df_ne$태블릿청약건수 = (df_ne$태블릿청약건수 - min(df_ne$태블릿청약건수)) / (max(df_ne$태블릿청약건수)-min(df_ne$태블릿청약건수))
df_ne$스마트폰청약건수 = (df_ne$스마트폰청약건수 - min(df_ne$스마트폰청약건수)) / (max(df_ne$스마트폰청약건수)-min(df_ne$스마트폰청약건수))
df_ne$물품서비스건수 = (df_ne$물품서비스건수 - min(df_ne$물품서비스건수)) / (max(df_ne$물품서비스건수)-min(df_ne$물품서비스건수))
df_ne$비대면건수 = (df_ne$비대면건수 - min(df_ne$비대면건수)) / (max(df_ne$비대면건수)-min(df_ne$비대면건수))
df_ne$모바일건수 = (df_ne$모바일건수 - min(df_ne$모바일건수)) / (max(df_ne$모바일건수)-min(df_ne$모바일건수))

df_ne
# 테스트 셋 나누기 80% 
train_n <- df_ne[train_tot,]   
test_n <- df_ne[-train_tot,]  

table(train_n$해촉여부)

# 모델에는 hidden 이 있다. 
neural_m <- neuralnet(formula = 해촉여부~성별+나이+차월+평균소득+평균신계약건수
                          +오전출근일수+오후귀사일수+통산유지율
                          +유지고객수+고객방문대상수+고객방문수+태블릿청약건수
                          +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수,
                      train_n)


plot(neural_m)

# 분류 건수 확인 
table(train_n$해촉여부)

# hidden 모델개수 설정 
neural_m5 <- neuralnet(formula = 해촉여부~성별+나이+차월+평균소득+평균신계약건수
                      +오전출근일수+오후귀사일수+통산유지율
                      +유지고객수+고객방문대상수+고객방문수+태블릿청약건수
                      +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수,
                      train_n
                      ,hidden = 2 )

plot(neural_m5)

library(caret)
vd_x = model.matrix( 해촉여부~성별+나이+차월+평균소득+평균신계약건수
                    +오전출근일수+오후귀사일수+통산유지율
                    +유지고객수+고객방문대상수+고객방문수+태블릿청약건수
                    +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수
                    ,test_n)

vd1 = data.frame(cbind(vd_x, test_n$해촉여부))

# compute 인공신경망 모델의 테스팅 데이터에 대한 예측 
neural_m.results <- compute(neural_m, vd1)

# neurons: 뉴런정보 , net.result : 분류에 대한 모델 예측값
# 0.5이상을 경우 1로 그외는 0 
predict_y = ifelse(neural_m.results$net.result > 0.5, 1, 0)

# 혼동 행렬 정확도 94.95 
confusionMatrix(as.factor(predict_y), as.factor(vd1$V18))

## model 2 

# compute 인공신경망 모델의 테스팅 데이터에 대한 예측 
neural_m5.results <- compute(neural_m5, vd1)

# neurons: 뉴런정보 , net.result : 분류에 대한 모델 예측값
# 0.5이상을 경우 1로 그외는 0 
predict_y = ifelse(neural_m5.results$net.result > 0.5, 1, 0)

# 혼동 행렬 정확도 94.95 
confusionMatrix(as.factor(predict_y), as.factor(vd1$V18))

##------------------------------
# svm
#install.packages("e1071")
library(e1071)



svm_fit <- svm(해촉여부~성별+나이+차월+평균소득+평균신계약건수
                   +오전출근일수+오후귀사일수+통산유지율
                   +유지고객수+고객방문대상수+고객방문수+태블릿청약건수
                   +스마트폰청약건수+물품서비스건수+비대면건수+모바일건수
                   , data=train_R, kernel='linear',cost=10 , scale = F)


svm_fit
plot(svm_fit, train_R$해촉여부)

# 교차검증
tune.out <-tune(svm, y ~., data = dat, kernel='linear', range=list(cost=c(0.001,0.01,0.1,1.5,10,50))) #
summary(tune.out) # best cost = 1.5  #보면 1.5일때 에러율이 가장 작다.

tune.out <-tune(svm, y ~., data = dat, kernel='linear', range=list(cost=c(0.001,0.01,0.1,1.5,10,50))) #
summary(tune.out) # best cost = 1.5  #보면 1.5일때 에러율이 가장 작다.

bestmod <-tune.out$best.model
bestmod
summary(bestmod)

#예측하기
xtest <- matrix(rnorm(20 * 2),ncol=2) #샘플링
ytest <- sample(c(-1,1),20,rep=T) #샘플링
xtest
ytest

xtest[ytest==1, ] <-xtest[ytest==1, ] +1
testda <- data.frame(x=xtest, y=as.factor(ytest))
testda


ypred <- predict(bestmod,testda)
table(예측값=ypred, 실제값=testda$y)
(5+10)/nrow(testda)


#iris data로 실습해보기
model <- svm(Species ~., data=iris) #Species에 의해서 분류를함
model # Support Vectors 51개 만듬

pred <- predict(model, iris[,-5])
pred
table(pred,iris$Species) #교차분할표
(50 + 48 + 48) / nrow(iris)
