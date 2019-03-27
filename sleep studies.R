
#----------------------------------------------------------------------------------------------
#LIBRARIES
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("e1071")
#install.packages("DMwR")
#install.packages("corrplot")
#install.packages("GGally")
#install.packages("Hmisc")
#install.packages("rpart")
#install.packages("tree")


library(dplyr)
library(ggplot2)

#Data Import
dat<- read.csv("C:/Users/User/Desktop/sleep2.csv")
dat
dat2<-read.csv("C:/Users/User/Desktop/sleep3.csv")
dat2

#----------------------------------------------------------------------------------------
head(dat)
head(dat2)

#removing second row (empty) of dat
dat<- dat[-1,]
names(dat)
head(dat)
names(dat2)

#transforming and merging dat and dat2
dat2<-dat2 %>% select(1:3)
dat2<- slice(dat2, 1:32)
str(dat2)

#Same name so we can merge
names(dat2)[names(dat2) == 'Subjects'] <- 'Participant'
str(dat2)


#retain only the ones with matching participant code
dat0<-inner_join(dat,dat2,by="Participant")
str(dat0)
head(dat0)
str(dat0)
#30 participants left
#----------------------------------------------------
#retain all when merged
datfull<- full_join(dat, dat2, by="Participant")
str(datfull)
datfull
#----------------------------------------------------

#filtering if both questionaires are NA
dat0_filt<-dat0 %>% filter(!is.na(dat0$Epworth.sleepiness.Scale) | !is.na(dat0$Pittsburgh.Sleep.Quality.Index) )
#27 participants left
#1 NA in Epworth, no NAs in Pitts
str(dat0_filt)
dat0$Epworth.sleepiness.Scale
dat0$Pittsburgh.Sleep.Quality.Index

dat0_filt$Epworth.sleepiness.Scale
dat0_filt$Pittsburgh.Sleep.Quality.Index

#extract "group" (all NAs)
dat0_filt<- dat0_filt[,-5]

str(dat0_filt)

#---------------------------------------------------------------------
#turning to numeric
#attach(dat0_filt)
#names(dat0_filt)
#lapply(dat0_filt, class)
#dat0_filt$Total.Sleep.Time<-as.numeric(as.character(Total.Sleep.Time))
#oops, faster way

dat0_filt[,5: ncol(dat0_filt)]<-dat0_filt[,5: ncol(dat0_filt)] %>% lapply(function(x) dat0_filt[,5: ncol(dat0_filt)]<-as.numeric(as.character(x)))
lapply(colnames(dat0_filt) ,function(x) c(class(dat0_filt[,x]), x))

str(dat0_filt)
levels(dat0_filt$Sex)
dat0_filt$Movements.No.
#we should probably exclude no. of movements
#removed No of movement 
df<-dat0_filt[,-11]

df[27,2]<- NA

nacol<- sapply(colnames(df), function(x) sum(is.na(df[,x])))
narow<-sapply(rownames(df), function(x) sum(is.na(df[x,])))
narow
nacol

#last participant 3 NAs
#Sex, BMI, Age, N3 latency, REM latency, Epworth 1 NA each


#-------------Missing values imputation by knn (k=10)---------------------------
library(DMwR)
df_clean<-knnImputation(df[,c(-1,-2)], k=10, meth= "weighAvg")
str(df_clean)
df_clean<- cbind(df[,1:2],df_clean)
str(df_clean)

#------------------------------Visualisation-------------------------------------
attach(df_clean)
plot(Sex)
hist(BMI)
plot(BMI~ Age)
hist(df_clean$Total.Sleep.Time)
hist(REM.duration)
hist(df_clean$Pittsburgh.Sleep.Quality.Index,breaks = 5,col = "Royal Blue")
hist(df_clean$Epworth.sleepiness.Scale,breaks = 5,col = "Royal Blue")

plot(Pittsburgh.Sleep.Quality.Index ~ Age)
plot(Pittsburgh.Sleep.Quality.Index ~ BMI)
plot(Epworth.sleepiness.Scale ~ Age)
plot(Epworth.sleepiness.Scale ~ BMI)

ggplot(df_clean, aes(Pittsburgh.Sleep.Quality.Index,Age)) + geom_smooth()
ggplot(df_clean, aes(Pittsburgh.Sleep.Quality.Index)) + geom_histogram(binwidth = 5 )
ggplot(df_clean, aes(Age)) + geom_dotplot()
ggplot(df_clean, aes(Age)) + geom_area(stat = "bin", binwidth=4)


boxplot(Age, horizontal = TRUE)
boxplot(Pittsburgh.Sleep.Quality.Index)
boxplot(Epworth.sleepiness.Scale)

#-------------------------------Correlation------------------------------------
library(corrplot)
#correlation matrix
cor_mat=cor(as.matrix(df_clean[,c(-1,-2)]))
cor_mat1<- cor(as.matrix(df_clean[,c(-1,-2,-3,-4)]))
corrplot::corrplot(cor_mat,method = "circle", tl.col = "black", is.corr = FALSE)
corrplot::corrplot(cor_mat,method="number", tl.col = "black", is.corr = FALSE)
corrplot::corrplot(cor_mat,method="pie", tl.col = "black", is.corr = FALSE)

library(GGally)
ggpairs(df_clean[,c(-1,-2)])
#we need less variables ~4-6

#Pittsburgh correlations
Pittscorr<- cor_mat[nrow(cor_mat),]
View(Pittscorr)
#correlations52-70% with No awakenings age apneas central apneas mixed sleep efficiency
Pittscorr[abs(Pittscorr)>0.5]

Epwcorr<- cor_mat[nrow(cor_mat)-1,]
View(Epwcorr)
#correlations52-70% with No awakenings age apneas central apneas mixed sleep efficiency
Epwcorr[abs(Epwcorr)>0.5]
#all correlations <0.35

#install.packages("Hmisc")
# Pearson Correlation with p-values
library(Hmisc)
mycor <- rcorr(as.matrix(df_clean[,c(-1,-2)]), type="pearson")
View(mycor$r)
View(mycor$P)

# Spearman Correlation with p-values
spear_corr<- rcorr(as.matrix(df_clean[,c(-1,-2)]), type="spearman")
View(spear_corr$r)
View(spear_corr$P)

#-------------------------Descriptive Stats-----------------------------------
install.packages("raster")
library(raster)
datmean <- sapply(df_clean[,3:ncol(df_clean)],"mean" ,na.rm=TRUE)
datsd<-sapply(df_clean[,3:ncol(df_clean)], "sd", na.rm=TRUE)
datvar<- sapply(df_clean[,3:ncol(df_clean)], "var", na.rm=TRUE)
datmin<- sapply(df_clean[,3:ncol(df_clean)], "min", na.rm=TRUE)
datmax<- sapply(df_clean[,3:ncol(df_clean)], "max", na.rm=TRUE)
datmedian<- sapply(df_clean[,3:ncol(df_clean)], "median", na.rm=TRUE)
datrange<- sapply(df_clean[,3:ncol(df_clean)], "range", na.rm=TRUE)
datCV <- sapply(df_clean[,3:ncol(df_clean)], "cv", na.rm=TRUE)

descr<- cbind(datmean,datmedian, datvar, datsd,datmin, datmax, range=datmax-datmin, datCV)
colnames(descr)<-c("mean", "median", "variance", "standard dev", "min", "max", "range", "CV %")
#Descriptive statistics of numeric variables
descr


#--------------------------------------------------------------------------------------------------------------------
str(df_clean)
#---------------------------------------PCA--------------------------------------------
pca<- prcomp(df_clean[,c(-1,-2)] ,center=TRUE, scale=TRUE)
plot(pca, type = "l")
plot(summary(pca)[[6]][3,], type='b', main= "Variance explained vs PCs", xlab="PCs", ylab="Variance explained")        
summary(pca)
biplot(pca)




#-----------------------------factor pittsburgh-----------------------------------------------------------
df_clean$target[df$Pittsburgh.Sleep.Quality.Index<=5]<-"ok"
df_clean$target[df$Pittsburgh.Sleep.Quality.Index>5]<- "poor"
df_clean$target<- factor(df_clean$target, ordered=FALSE)
str(df_clean)
plot(df_clean$target)
hist(df_clean$Pittsburgh.Sleep.Quality.Index)
summary(df_clean$target)
boxplot(df_clean$Pittsburgh.Sleep.Quality.Index~df_clean$target, col=c(3,2))
#---------------------------------------------------------------------------------------------------------
#factor in 3 categories
#df_clean$y[df$Pittsburgh.Sleep.Quality.Index<=5]<-"ok"
#df_clean$y[df$Pittsburgh.Sleep.Quality.Index>5 & df$Pittsburgh.Sleep.Quality.Index<=8]<- "borderline"
#df_clean$y[df$Pittsburgh.Sleep.Quality.Index>8]<-"poor"
#df_clean$y<- factor(df_clean$y, ordered=FALSE)
#plot(df_clean$y)
#---------------------------------------------------------------------------------------------------------

#-------------------------SVM-----------------------------------------------------------------------------
#Leave one out cross validation
library(e1071)
tune_out <- tune.svm(target~Awakenings.No.+Age+Apneas.Central+Apneas.Mixed+Sleep.Efficiency+BMI , data = df_clean[,-c(1,2,24,25)] ,cost=c(0.0001,0.001,0.01, 0.1, 1,10), 
                     gamma=c(0.0001,0.001,0.01,0.1,1,10),kernel="radial", tunecontrol= tune.control(cross=27))
summary(tune_out)
tune_out$best.performance #0.185 error -> 81.5% accuracy
tune_out$best.parameters #gamma=1 cost=10
svmfit <- svm(target~Awakenings.No.+Age+Apneas.Central+Apneas.Mixed+Sleep.Efficiency+BMI, data = df_clean[,-c(1,2,24,25)] , kernel = "radial", cost=10, gamma=1)

#confusion matrix
eval1_rbf <- predict(svmfit, data =df_clean[,-c(1,2,24,25,26)]  , type = "response")
table(eval1_rbf, df_clean$target)

#tune.rpart
#tune.randomForest
#tune.knn


#-----------------Variable removal based on high correlation >.75 and >.90---------------------------------------------------
#install.packages("caret")
library(caret)
str(df_clean)
findCorrelation(cor(df_clean[,-c(1,2,26)]),exact = TRUE, verbose=TRUE, names=TRUE, cutoff = 0.75) #prints columns to remove
#cutoff= 0.75: N2 duration, REM total sleep, Apneas Central, Apneas. Hypoapneas, Light total sleep, Latency, N3 duration, N3 latency
findCorrelation(cor(df_clean[,-c(1,2,26)]),exact = TRUE, verbose=TRUE, names=FALSE, cutoff = 0.9) #prints columns to remove
#cutoff= 0.9 N2 duration, REM total sleep, Apneas central, Light total sleep, Latency
findCorrelation(cor(df_clean[,-c(1,2,26)]),exact = TRUE, verbose=TRUE, names=TRUE, cutoff = 0.95) #prints columns to remove
#cutoff= 0.95 Deep total sleep Apneas central, Latency


#-----------------------------SVM----------------------------------------------------
#Leave one out cross validation
tune_out <- tune.svm(target ~ Age+ Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(0.0001,0.001,0.01, 0.1, 1,10), 
                     gamma=c(0.0001,0.001,0.01,0.1,1,10),kernel="radial", tunecontrol= tune.control(cross=nrow(df_clean)))
summary(tune_out)
tune_out$best.performance #0.222 error -> 77.8% accuracy
tune_out$best.parameters #gamma=0.1 0.01 cost=10

#10-fold
tune_out1 <- tune.svm(target ~ Age+ Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(0.0001,0.001,0.01, 0.1, 1,10), 
                      gamma=c(0.0001,0.001,0.01,0.1,1,10),kernel="radial", tunecontrol= tune.control(cross=10))
summary(tune_out1)
tune_out1$best.performance #0.1833- 0.216 error -> 81.7% accuracy 
tune_out1$best.parameters #gamma=0.1 cost=10 
#---------------------------
#further grid search
tune_out2<-tune.svm(target ~ Age+Latency+ N3.duration  , data = df_clean[,-c(1,2,24,25)] ,cost=c(2,5,10,15,20,40), 
                    gamma=c(0.5,0.3,0.2, 0.1, 0.01,0.05, 0.08),kernel="radial", tunecontrol= tune.control(cross=nrow(df_clean)))
summary(tune_out2)
tune_out2$best.performance #0.111 error -> 88.89% accuracy
#misses 3 in 27
tune_out2$best.parameters #gamma=0.3 cost=10 15
svm_fitt <- svm(target ~ Age+ Latency + N3.duration, data = df_clean[,-c(1,2,24,25)] , kernel = "radial", cost=10, gamma=0.3)
eval2_rbf <- predict(svm_fitt, data =df_clean[,-c(1,2,24,25,26)]  , type = "response")
table(eval2_rbf, df_clean$target)

#-------------------------
tune_out3<-tune.svm(target ~ Age+ Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(8, 10, 12, 14, 15, 16), 
                    gamma=c(0.29, 0.295, 0.3 ,0.305, 0.31, 0.32, 0.325,  0.33 ), kernel="radial",
                    tunecontrol= tune.control(cross=nrow(df_clean)))
summary(tune_out3)
tune_out3$best.performance #0.111 error -> 88.89% accuracy
tune_out3$best.parameters #gamma=0.28-0.31 cost= 15
# me 10-fold error 0.0833 - > acc=91.67% gamma=0.325 0.33 cost=14 15

# 10-fold
tune_out4<-tune.svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(12,14,15,16,17), 
                    gamma=c(0.3 ,0.31, 0.32,0.3225, 0.3250,  0.3275, 0.33,0.335 ),kernel="radial", tunecontrol= tune.control(cross=10))
summary(tune_out4)
tune_out4$best.performance #0.111 error -> 88.89% accuracy
tune_out4$best.parameters #gamma=0.28-0.31 cost= 15
# me 10-fold error 0.0833 - > acc=91.67% gamma=0.325 0.33 cost=14 15


str(df_clean)
scaled<- scale(x= df_clean[,-c(1,2,24,25,26)], center=TRUE, scale = TRUE)
scaled<- cbind(df_clean[,c(1,2)], scaled, df_clean[,c(24:26)])
str(scaled)

tune_out_scaled <- tune.svm(target ~ Age+ Latency + N3.duration , data = scaled[,-c(1,2,24,25)] ,cost=c(0.0001,0.001,0.01, 0.1, 1,10), 
                            gamma=c(0.0001,0.001,0.01,0.1,1,10),kernel="radial", tunecontrol= tune.control(cross=nrow(scaled)))
summary(tune_out_scaled)
tune_out_scaled$best.performance #0.222 error -> 77.8% accuracy
tune_out_scaled$best.parameters #gamma=0.1 0.01 cost=10

tune_out_scaled1 <- tune.svm(target ~ Age+ Latency + N3.duration , data = scaled[,-c(1,2,24,25)] ,cost=c(8,9,10,11,12,15), 
                             gamma=c(0.2,0.25, 0.28, 0.3, 0.32, 0.33, 0.35, 0.4),kernel="radial", tunecontrol= tune.control(cross=nrow(scaled)))
summary(tune_out_scaled1)
tune_out_scaled1$best.performance #0.111 error
tune_out_scaled1$best.parameters 

tune_out_scaled2 <- tune.svm(target ~ Age+ Latency + N3.duration , data = scaled[,-c(1,2,24,25)] ,cost=c(7,7.5,8,8.5,9,10,12.5), 
                             gamma=c( 0.3, 0.31, 0.32, 0.325, 0.33, 0.34,0.345,0.35),kernel="radial", tunecontrol= tune.control(cross=nrow(scaled)))
summary(tune_out_scaled2)
tune_out_scaled2$best.performance #0.111 error -> 88.8% accuracy
tune_out_scaled2$best.parameters #gia oles tis parametrous

tune_out_scaled3 <- tune.svm(target ~ Age+ Latency + N3.duration , data = scaled[,-c(1,2,24,25)] ,cost=c(7,7.5,8,8.5,9,10,12.5,14), 
                             gamma=c( 0.3, 0.31, 0.32, 0.325, 0.33, 0.34,0.345,0.35),kernel="radial", 
                             tunecontrol= tune.control(cross=10))
summary(tune_out_scaled3)
tune_out_scaled3$best.performance #0.1-0.13 error 
tune_out_scaled3$best.parameters 

#best performance at LOOCV 0.11, 24/27

#----------------------------------------------------------------------------------------------------------------------------

#rpart
library(rpart)
tree<- rpart(target~Latency+N2.Latency+Apneas.Mixed+Awakenings.No.+Apneas.Hypopneas, 
             data=df_clean, method="class", control=rpart.control(minsplit=3, cp=0.01))
tree
plotcp(tree)
summary(tree)
plot(tree)
text(tree)
printcp(tree)

tree2<- rpart(target~., data= df_clean[,-c(1,2,24,25)],control=rpart.control(minsplit=3, cp=0.01), method="class")
plot(tree2)
text(tree2)
printcp(tree2) # display the results 
plotcp(tree2) # visualize cross-validation results 


#tree
library(tree)

controltree<- tree.control(nobs=27, mincut=0, minsize = 2, mindev = 0.005 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~. , data=df_clean[,-c(1,2,24,25)], control= controltree  )
#"Age"         "Latency"     "BMI"
summary(treee)
plot(treee)
text(treee, pretty = 0)
#predict(treee, newdata = target, type="class")
cbind(df_clean$Age, df_clean$Latency,df_clean$BMI, df_clean$target)

attach(df_clean)
plot(Age~target)
plot(Latency~target)
plot(N3.duration~target)

#w<-cbind(Age, Latency, N3.duration, target)
#str(df_clean)

controltree<- tree.control(nobs=27, mincut=5, minsize = 10, mindev = 0.1 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~. , data=df_clean[,-c(1,2,24,25)], control= controltree  )
plot(treee)
text(treee)
summary(treee)
#N2 duration BMI 24/27

controltree<- tree.control(nobs=27 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~. , data=df_clean[,-c(1,2,24,25)], control= controltree  )
plot(treee)
text(treee)
summary(treee)
misclass.tree(treee, detail=TRUE)
predict(treee, newdata= target, type = "vector")


#rpart<-tune.rpart(target ~ Age+ Latency + N3.duration+BMI, data = df_clean, minsplit=seq(1:15),cp=c(0.001,0.005,0.01,0.05,0.1,0.05), maxdepth = 1:10)
#plot(rpart)



library(ROCR)     
predictions=as.vector(rf_output$votes[,2])
pred=prediction(predictions,target)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))


#-------------------------------------Trial and error, mostly error----------------------------------
#---------------------Scaling---------------------------------------------------
#center and scale
numpred<-df_clean[,-c(1,2,24,25)]
nearZeroVar(numpred)
scaled <-data.frame(apply(numpred, 2, scale))
nearZeroVar(scaled)

sum(is.na(numpred))
sum(is.na(scaled))



#descr for scaled
datmean1 <- sapply(scaled[,3:ncol(scaled)],"mean" ,na.rm=TRUE)
datsd1<-sapply(scaled[,3:ncol(scaled)], "sd", na.rm=TRUE)
datvar1<- sapply(scaled[,3:ncol(scaled)], "var", na.rm=TRUE)
datmin1<- sapply(scaled[,3:ncol(scaled)], "min", na.rm=TRUE)
datmax1<- sapply(scaled[,3:ncol(scaled)], "max", na.rm=TRUE)
datmedian1<- sapply(scaled[,3:ncol(scaled)], "median", na.rm=TRUE)
datrange1<- sapply(scaled[,3:ncol(scaled)], "range", na.rm=TRUE)

descr1<- cbind(datmean1,datmedian1, datvar1, datsd,datmin1, datmax1, range=datmax1-datmin1)
colnames(descr1)<-c("mean", "median", "variance", "standard dev", "min", "max", "range")
descr1



#-------------SVM feature selection------------------------------------------------------------------

library(caret)
#backwards feature selection
#x <- scale(df_clean[,-c(1,2,24,25)])
#x <- x[, -findCorrelation(cor(x), .8)]
#x <- as.data.frame(x)
#svmProfile <- rfe(x, df_clean[,25],   sizes = c(2, 5, 10),  rfeControl = rfeControl(functions = caretFuncs,                                           number = 50, verbose=TRUE, p=0.7),
## pass options to train()
# method = "svmRadial")
#svmProfile
library(e1071)
testset<-df_clean[, -c(1,2,24)]
tune_out <- tune.svm(Pittsburgh.Sleep.Quality.Index~. , data = t ,cost=c(0.0001,0.001,0.01, 0.1, 1,10), 
                     gamma=c(0.0001,0.001,0.01,0.1,1,10),kernel="radial", tunecontrol= tune.control(cross=5))
summary(tune_out)
tune_out$best.parameters



#center and scale
set.seed(3)
t<- df_clean[,c(3,4,5,11,18,19,25)]
t[,7]<- t[,7]/28
normal<-preProcess(testset)
testn <- predict(normal,testset)
ctrl<- rfeControl(functions = lrFuncs,
                  method = "cv",
                  number=10,
                  verbose = FALSE)
subset<- c(1:10, 12,15,18,20,22)
rfe(x= t[,-7], t[,7] ,sizes= c(1:6), rfeControl = ctrl, method= "svmLinear"   )

rfeCNTL = rfeControl(functions=lrFuncs, method="cv", number=10)
svm.features = rfe(t[,1:6], t[,7],sizes = c(1,2,4,5),
                   rfeControl = rfeCNTL, method = "svmLinear")


library(bestglm)
str(df_clean)
LOOCV(df_clean[,-c(1,2,24,25)], df_clean$Pittsburgh.Sleep.Quality.Index)
#naive bayes needs y categ
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model

names(getModelInfo())
model1<- train(Pittsburgh.Sleep.Quality.Index~ Awakenings.No.+Age+Apneas.Central+Apneas.Mixed+Sleep.Efficiency, data= df_clean, trControl= train_control, method="lm")
summary(model1)



modelNB <- train(target~. , data=df_clean[,-c(1,2,24,25)], trControl=train_control, method="nb")
str(df_clean)
model$results

modelRF <- train(target~. , data=df_clean[,-c(1,2,24,25)], trControl=train_control, method="rf")
modelRF$results

modelSVMradial <- train(target~. , data=df_clean[,-c(1,2,24,25)], trControl=train_control, method="svmRadial")
modelSVMradial


modelSVMlinear <- train(target~. , data=df_clean[,-c(1,2,24,25)], trControl=train_control, method="svmLinear")
modelSVMlinear

modelSVMpoly <- train(target~. , data=df_clean[,-c(1,2,24,25)], trControl=train_control, method="svmPoly")
modelSVMpoly

modelLM <- train(Pittsburgh.Sleep.Quality.Index~. , data=df_clean[,-c(1,2,24,26)], trControl=train_control, method="lm")
modelLM

modelLDA <- train(target~. , data=df_clean[,-c(1,2,24,25)], trControl=train_control, method="lda")
modelLDA

modelrpart <- train(Pittsburgh.Sleep.Quality.Index~. , data=df_clean[,-c(1,2,24,26)], trControl=train_control, method="rpart")
modelrpart

modelrpartclass <- train(target~. , data=df_clean[,-c(1,2,24,25)], trControl=train_control, method="rpart")
modelrpartclass
#--------------------------------
#--------------------------------

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
model1<- train(Pittsburgh.Sleep.Quality.Index~ Awakenings.No.+Age+Apneas.Central+Apneas.Mixed+Sleep.Efficiency, data= df_clean, trControl= train_control, method="lm")
summary(model1)
varImp(model1)
plot(varImp(model1))

model2<- train(Pittsburgh.Sleep.Quality.Index~., data= df_clean[,-c(1,2,24,26)], trControl= train_control, method="lm" )
plot(varImp(model2))
varImp(model2)

model3<- train(Pittsburgh.Sleep.Quality.Index~Latency+N2.Latency+Apneas.Mixed+Awakenings.No.+Apneas.Hypopneas, data= df_clean[,-c(1,2,24,26)], trControl= train_control, method="lm" )
summary(model3)
#adjusted R2= 73



#rfe for random forest
control1 <- rfeControl(functions=rfFuncs, method="cv", number=nrow(df_clean))
results <- rfe(df_clean[,-c(1,2,24,25,26)], df_clean[,26], sizes=c(1:20), rfeControl=control1)
results
predictors(results)
plot(results, type=c("g", "o"))
plot(df_clean$Pittsburgh.Sleep.Quality.Index~df_clean$Age)
plot(df_clean$Age~df_clean$target)
plot(df_clean$Pittsburgh.Sleep.Quality.Index ~ df_clean$Sex)
plot(df_clean$target~ df_clean$Sex)
table(df_clean$target, df_clean$Sex)


results <- rfe(df_clean[,-c(1,2,24,25,26)],df_clean[,26], sizes=c(1:20),
               preProcess = c("scale", "center"),rfeControl=control1)
plot(results, type=c("g", "o"))

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")

set.seed(42)
model_rf_under <- train(df_clean[,-c(1,2,24,25,26)],df_clean[,26] , method="svmRadial",
                        preProcess = c("scale", "center"),
                        trControl = ctrl )
model_rf_under


ctrl2<- trainControl(method = "cv", 
                     number = nrow(df_clean), 
                     verboseIter = FALSE, sampling = "smote",
                     classProbs = TRUE)

model_rf_rose <- train(df_clean[,-c(1,2,8,17,21,15,6,24,25,26)],df_clean[,26] , method="svmRadial",
                       preProcess = c("scale", "center"),
                       trControl = ctrl2 )

svmm<-svm(df_clean[,26] ~. , data = df_clean[,-c(1,2,8,17,21,15,6,24,25)] , method="C-classification", kernel="radial",cost=0.5, gamma=0.05)
summary(svmm)
eval1__rbf <- predict(svmm, newdata = test , type = "response")
#confusion matrix
table(eval1__rbf, test$label)













#-------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
sink("output1.txt")
cat("dat0")
print(dat0)
cat("datfull")
print(datfull)
cat("df_clean")
print(df_clean)
cat("scaled")
print(scaled)
cat("descr stat")
print(descr)
cat("correlation")
print(cormat)
#PCA-----------------------------------------------------------------------------------------------------------
cat("PCA")
pca<- prcomp(scaled)
plot(pca, type = "l")
plot(summary(pca)[[6]][3,], type='b', main= "Variance explained vs PCs", xlab="PCs", ylab="Variance explained")        
summary(pca)
biplot(pca)
#Correlation-----------------------------------------------------------------------------------------------------------
cat("correlation")
corrplot::corrplot(cormat,method = "circle", tl.col = "black", is.corr = FALSE)
corrplot::corrplot(cormat,method="number", tl.col = "black", is.corr = FALSE)
corrplot::corrplot(cormat,method="pie", tl.col = "black", is.corr = FALSE)
#TREE-----------------------------------------------------------------------------------------------------------
cat("TREE")
controltree<- tree.control(nobs=27, mincut=0, minsize = 2, mindev = 0.005 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~. , data=df_clean[,-c(1,24,25)], control= controltree  )
#"Age"         "Latency"     "N3.duration"
summary(treee)
plot(treee)
text(treee, pretty = 0)
predict(treee, newdata = target, type="class")

#SVM-----------------------------------------------------------------------------------------------------------
cat("SVM")
tune_out3<-tune.svm(target ~ Age+ Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(8, 10, 12, 14, 15, 16), 
                    gamma=c(0.29, 0.295, 0.3 ,0.305, 0.31, 0.32, 0.325,  0.33 ), kernel="radial",
                    tunecontrol= tune.control(cross=nrow(df_clean)))
summary(tune_out3)
tune_out3$best.performance #0.111 error -> 88.89% accuracy
tune_out3$best.parameters #gamma=0.28-0.31 cost= 15
# me 10-fold error 0.0833 - > acc=91.67% gamma=0.325 0.33 cost=14 15


tune_out4<-tune.svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(12,14,15,16,17), 
                    gamma=c(0.3 ,0.31, 0.32,0.3225, 0.3250,  0.3275, 0.33,0.335 ),kernel="radial", tunecontrol= tune.control(cross=10))
summary(tune_out4)
tune_out4$best.performance #0.111 error -> 88.89% accuracy
tune_out4$best.parameters #gamma=0.28-0.31 cost= 15
# me 10-fold error 0.0833 - > acc=91.67% gamma=0.325 0.33 cost=14 15
-----------------------------------------------------------------------------------------------------------
  sink()
