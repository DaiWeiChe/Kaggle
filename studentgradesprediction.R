

getwd()
df = read.csv(file.choose(),sep=";")#"C:/Users/user/Desktop/大數據資料分析/R_practice/class4/student-mat.csv"
summary(df)

NUM.col = sapply(df, is.numeric)
NUM.col

COR.col = cor(df[,NUM.col])
COR.col


library(ggplot2)
library(GGally)

ggpairs(df[,NUM.col])

#install.packages("corrgram")
library(corrgram)
library(corrplot)
cor.df = corrgram(df[,NUM.col])
cor.df
corrplot(cor.df)
corrplot(cor.df,method = "color")

df1 = df[,NUM.col]
plotdf = ggplot(data = df1,aes(x= df1$G3))
plotdf + geom_histogram()
plotdf + geom_bar()
plotdf + geom_boxplot()
ggpairs(df1[,c(14,15,16)])

#install.packages("caTools")
library(caTools)

Sample = sample.split(df$G3, SplitRatio = 0.7)
Train  = subset(df,Sample = T)
Test   = subset(df,Sample = T)

Model.lm = lm(G3~.,data = Train)
summary(Model.lm)

#build full model
modelall = lm(G3~.,data = Train)
full<-formula(modelall)

summary(modelall)# display results
#build a null model with only the intercept term

model0<-lm(G3~1,data = Train)
summary(model0)
#stepwise regression
step(model0,direction = "both",scope = full)
step(modelall,direction = "backward",scope = full)

model1 = lm(formula = G3 ~ school + age + activities + romantic + famrel + 
              Walc + absences + G1 + G2, data = Train)
summary(model1)# display results

########模型診斷 
#install.packages("ggfortify")
library(ggfortify)
autoplot(model1)
shapiro.test(model1$residuals)
###residuals
ggplot(model1,aes(x=model1$residuals))+geom_histogram()
ggplot(model1,aes(sample=model1$residuals)) + stat_qq() + stat_qq_line()

###老師的方法
res = as.data.frame(residuals(model1))
ggplot(res,aes(x=res$`residuals(model1)`))+geom_histogram()

##### 殘差分析
shapiro.test(df$G3)
ggplot(df,aes(x=G3))+geom_density()
ggplot(df,aes(x=G3))+geom_histogram()
ggplot(df,aes(y=G3))+geom_boxplot()

#####預測

G3.prediction = predict(model1,Test)
result = cbind(round(G3.prediction,1) , Test$G3)
colnames(result) = c("pred","real")
result

#####誤差
result = as.data.frame(result)
mse = mean((result$pred - result$real )^2)
rmse= mse^0.5
rmse

sse = sum((result$pred - result$real)^2)
sse
sst = sum((mean(df1$G3) - result$real)^2)
sst
R2 = 1 - sse/sst
R2
#############
############# 刪除G3為0資料
#############
df1 = df[-which(df$G3 == 0),]

#ggpairs(df1)
shapiro.test(df1$G3)
ggplot(df1,aes(x=G3))+geom_density()
ggplot(df1,aes(x=G3))+geom_histogram()
ggplot(df1,aes(y=G3))+geom_boxplot()

####################

Sample = sample.split(df1$G3, SplitRatio = 0.7)
Train  = subset(df1,Sample = T)
Test   = subset(df1,Sample = T)

Model.lm = lm(G3~.,data = Train)
summary(Model.lm)

#build full model
modelall = lm(G3~.,data = Train)
full<-formula(modelall)

summary(modelall)# display results
#build a null model with only the intercept term

model0<-lm(G3~1,data = Train)
summary(model0)
#stepwise regression
step(model0,direction = "both",scope = full)
step(modelall,direction = "backward",scope = full)

model1 = lm(formula = G3 ~ paid + famrel + goout + health + absences + 
     G1 + G2, data = Train)
summary(model1)# display results

########模型診斷 
#install.packages("ggfortify")
library(ggfortify)
autoplot(model1)
shapiro.test(model1$residuals)
###residuals
ggplot(model1,aes(x=model1$residuals))+geom_histogram()
ggplot(model1,aes(sample=model1$residuals)) + stat_qq() + stat_qq_line()


#####預測

G3.prediction = predict(model1,Test)
result = cbind(round(G3.prediction,1) , Test$G3)
colnames(result) = c("pred","real")
result

#####誤差
result = as.data.frame(result)
mse = mean((result$pred - result$real )^2)
rmse= mse^0.5
rmse

sse = sum((result$pred - result$real)^2)
sse
sst = sum((mean(df1$G3) - result$real)^2)
sst
R2 = 1 - sse/sst
R2


#install.packages("randomForest")
library(randomForest)

set.seed (123)
index.train = sample(1:nrow(df), size=ceiling(0.8*nrow(df))) # training dataset index
train = df[index.train, ] # trainind data
test = df[-index.train, ] # test data


#  Bagging trees: All predictors should be considered for each split of the tree.

bag.df <- randomForest (G3~ ., data = df ,
                            subset = index.train ,  
                            mtry = 6, # (33variables-1)開根號 >>>分類樹
                            importance = TRUE)
bag.df


bag.df <- randomForest (G3~ ., data = df ,
                        subset = index.train ,  
                        mtry = 10, # (33variables-1)/3 >>>回歸樹
                        importance = TRUE)
bag.df



## Observe that what is the best number of trees(ntree)
plot(bag.df) # MSE on OOB samples (OOB error) 


# x.train.data= train[,-14]= training data without Y(medv)   
# y.train.data=train[,14]= Y(medv)
tuneRF(x=train[,-33], y=train[,33], 
       ntreeTry= 300) 


fin.df <- randomForest (G3~ ., data = df ,
                        subset = index.train ,  
                        mtry = 20, # (33variables-1)/3 >>>回歸樹
                        importance = TRUE)
fin.df


















#install.packages("randomForest")
library(randomForest)

randomForest(formula = Y~X,
             subset   = training_data_index,# 注意! 這裡不是直接放入training data，而是index值 
             na.action  =‘na.fail’,# (默認值), 不允許資料中出現遺失值/ ‘na.omit’,刪除遺失值。
             importance = TRUE,# 结合importance()函数使用，用來觀察重要變數。
             ntree   = num_tree,# number of trees, RF裡生成的決策樹數目。
             mtry    = m_variables_try, # 每次抽樣時需要抽「多少個變數」，
             # 建議設置 ?/3  (Regression trees);  √?(Classification trees)
             sampsize  = sampsize,#訓練每棵樹模型的樣本數大小。預設是使用63.25%訓練資料集的比例。
             nodesize  = nodesize,# 末梢(葉)節點最小觀察資料個數。
             Maxnode  = Maxnode,# 內部節點最大個數值。
             ... ) 

'''
參數設定	意思	說明
ntree	RF裡生成的決策樹數目。number of trees.	我們希望有足夠的樹來穩定模型的誤差，但過多的樹會是沒效率且沒必要的，特別是遇到大型資料集的時候。
mtry	建每棵樹，抽樣時需要抽「多少個變數」。	建議設置 p/3 (迴樹樹Regression trees); √p(分類樹Classification trees)。
sampsize	訓練每棵樹模型的樣本數大小。	預設是使用63.25%訓練資料集的比例。
nodesize	訓練的每棵樹時，末梢(葉)節點最少要有多少觀察資料個數。	控制模型複雜度的變數。末梢(葉)節點需含有的資料量越大，生成的樹越簡化/越淺。
maxnode	每棵樹的內部節點最多只能有多少節點。	控制模型複雜度的變數。內部節點越多，生成的樹越複雜/越深。
'''



'''
data1 = data %>%　group_by (TKT_BEG,STOP_NAME) %>% summarise(總進站 = sum(進站),總出站 = sum(出站))

'''


#  Bagging trees: All predictors should be considered for each split of the tree.
library(randomForest)
bag.boston <- randomForest (medv ~ ., data = Boston ,
                            subset = index.train ,  
                            mtry = ncol(Boston)-1, # 14variables - 1(medv)=13 
                            importance = TRUE)
bag.boston

# Perform on the test set
test <- Boston[-index.train  , ]
yhat.bag <- predict (bag.boston , newdata = Boston[-index.train , ])
plot (yhat.bag , test$medv)
abline (0, 1)
mean ((yhat.bag - test$medv)^2) #MSE


rf.boston <- randomForest (medv ~ ., data = Boston ,
                           subset = index.train , 
                           mtry = 4,           #13/3
                           importance = TRUE)
rf.boston


## Observe that what is the best number of trees(ntree)
plot(rf.boston) # MSE on OOB samples (OOB error) 


# x.train.data= train[,-14]= training data without Y(medv)   
# y.train.data=train[,14]= Y(medv)
tuneRF(x=train[,-14], y=train[,14], 
       ntreeTry= 200) 


set.seed (123)
tuneRF(x=train[,-14], y=train[,14],ntreeTry= 200)   
'''
mtry = 4  OOB error = 11.3863 
Searching left ...
mtry = 2 	OOB error = 13.24741 
-0.1634515 0.05 
Searching right ...
mtry = 8 	OOB error = 10.73043 
0.05760202 0.05 
mtry = 13 	OOB error = 11.25116 
-0.04852872 0.05 
   mtry OOBError
2     2 13.24741
4     4 11.38630
8     8 10.73043
13   13 11.25116
'''

##最終模型
# Set mtry=4, ntree=200 to build the model.
rf.tune.boston <- randomForest (medv ~ ., data = Boston ,
                                subset = index.train , 
                                mtry = 8, 
                                ntree = 500,
                                importance = TRUE)
rf.tune.boston

## 預測結果
yhat.tune.rf <- predict (rf.tune.boston, newdata = test)
mean ((yhat.tune.rf - test$medv)^2)#MSE


rf.tune.boston

'''
Call:
 randomForest(formula = medv ~ ., data = Boston, mtry = 8, ntree = 500,      importance = TRUE, subset = index.train) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 8

          Mean of squared residuals: 9.486548
                    % Var explained: 88.74
'''



# Importance of each variable
importance (rf.tune.boston)        # %Increase in MSE(); Increase in node purity
importance (rf.tune.boston,type=1) # %IncMSE
varImpPlot (rf.tune.boston)
'''
          %IncMSE
crim    14.370463
zn       3.839625
indus    8.416519
chas     1.892416
nox     17.017921
rm      43.283217
age     10.611680
dis     22.004338
rad      4.665024
tax     10.515458
ptratio 15.319415
black    5.977527
lstat   37.167670
'''

importance (rf.tune.boston)        # %Increase in MSE(); Increase in node purity

'''
          %IncMSE IncNodePurity
crim    14.370463    1550.09612
zn       3.839625      40.37540
indus    8.416519     688.82240
chas     1.892416      64.37207
nox     17.017921    1120.38661
rm      43.283217   12387.14878
age     10.611680     637.21398
dis     22.004338    2157.54871
rad      4.665024     130.91332
tax     10.515458     551.80444
ptratio 15.319415    1450.46264
black    5.977527     321.22973
lstat   37.167670   12684.82875

'''



