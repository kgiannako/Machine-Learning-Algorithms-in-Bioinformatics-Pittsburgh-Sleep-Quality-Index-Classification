
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
View(df)
#last participant 3 NAs
#Sex, BMI, Age, N3 latency, REM latency, Epworth 1 NA each
df$Participant[11:27]

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
hist(df_clean$Epworth.sleepiness.Scale,breaks = 6,col = "tomato")

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

str(df_clean)
library(GGally)
ggpairs(df_clean[,c(3,4,5,6,9,11,18,19,24,25)])
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
#View(mycor$r)
#View(mycor$P)
zz<-abs(mycor$r[,23])>0.45
mycor$r[zz,23]
print(mycor$r[zz,23], digits = 2)
pears<- cbind(mycor$r[zz,23],mycor$P[zz,23])
colnames(pears)= c("pearson", "p-value")
pears= pears[1:5,]
View(round(pears, digits = 2))
print(pears, digits = 2)

# Spearman Correlation with p-values
spear_corr<- rcorr(as.matrix(df_clean[,c(-1,-2)]), type="spearman")
#View(spear_corr$r)
#View(spear_corr$P)
ss<- abs(spear_corr$r[,23])>0.4
spear<- cbind(spear_corr$r[ss,23],spear_corr$P[ss,23])
spear= spear[1:8,]
colnames(spear) = c("Spearman", "p-value")
View(round(spear, digits = 2))
print(spear, digits = 2)
#-------------------------Descriptive Stats-----------------------------------
#install.packages("raster")
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

ds<- cbind(datmin, datmax,datmean, datsd,datmedian, datCV)
colnames(ds)<-c("min", "max","mean", "standard deviation", "median", "CV %")
View(round(ds, digits=2))


#install.packages("gmodels")
library(gmodels)
Age1=ci(df_clean$Age[df_clean$target=="ok"])
Age2=ci(df_clean$Age[df_clean$target=="poor"])
Age=cbind(Age1[2:3],Age2[2:3])

BMI1=ci(df_clean$BMI[df_clean$target=="ok"])
BMI2=ci(df_clean$BMI[df_clean$target=="poor"])
BMI=cbind(BMI1[2:3],BMI2[2:3])

Sleep1=ci(df_clean$Sleep.Efficiency[df_clean$target=="ok"])
Sleep2=ci(df_clean$Sleep.Efficiency[df_clean$target=="poor"])
Sleep=cbind(Sleep1[2:3],Sleep2[2:3])

Late1=ci(df_clean$Latency[df_clean$target=="ok"])
Late2=ci(df_clean$Latency[df_clean$target=="poor"])
Late=cbind(Late1[2:3],Late2[2:3])

Awak1=ci(df_clean$Awakenings.No.[df_clean$target=="ok"])
Awak2=ci(df_clean$Awakenings.No.[df_clean$target=="poor"])
Awak=cbind(Awak1[2:3],Awak2[2:3])

N21=ci(df_clean$N2.duration[df_clean$target=="ok"])
N22=ci(df_clean$N2.duration[df_clean$target=="poor"])
N2=cbind(N21[2:3],N22[2:3])

N31=ci(df_clean$N3.duration[df_clean$target=="ok"])
N32=ci(df_clean$N3.duration[df_clean$target=="poor"])
N3=cbind(N31[2:3],N32[2:3])

q=c("Age lower","Age upper","BMI lower","BMI upper","Sleep Efficiency lower","Sleep Efficiency upper","Latency lower","Latency upper",
  "No. of Awakenings lower","No. of Awakenings upper","N2 Duration lower","N2 Duration upper","N3 Duration lower","N3 Duration upper")

z1=rbind(Age,BMI,Sleep,Late,Awak,N2,N3)
z1=round(z1, digits=2)
z1=cbind(q,z1)
rownames(z1)=c()
rownames(z)=c("Age CI lower","Age CI upper","BMI CI lower","BMI CI upper","Sleep Efficiency CI lower","Sleep Efficiency CI upper","Latency CI lower","Latency CI upper",
              "No. of Awakenings CI lower","No. of Awakenings CI upper","N2 Duration CI lower","N2 Duration CI upper","N3 Duration CI lower","N3 Duration CI upper")
colnames(z1)=c("95% Confidence Interval","OK: PSQI<=5", "Poor: PSQI>5")
library(formattable)
formattable(as.data.frame(z1) , align=c("l","c","c"))


#--------------------------------------------------------------------------------------------------------------------
str(df_clean)
#---------------------------------------PCA--------------------------------------------
pca<- prcomp(df_clean[,c(-1,-2)] ,center=TRUE, scale=TRUE)
plot(pca, type = "l")
plot(summary(pca)[[6]][3,], type='b', main= "Variance explained vs PCs", xlab="PCs", ylab="Variance explained")        
summary(pca)
biplot(pca)


library(grDevices)

#-----------------------------factor pittsburgh-----------------------------------------------------------
df_clean$target[df$Pittsburgh.Sleep.Quality.Index<=5]<-"ok"
df_clean$target[df$Pittsburgh.Sleep.Quality.Index>5]<- "poor"
df_clean$target<- factor(df_clean$target, ordered=FALSE)
str(df_clean)
plot(df_clean$target, main="PSQI factored",xlab="ok: PSQI<=5  poor: PSQI>5", col="palegoldenrod")
hist(df_clean$Pittsburgh.Sleep.Quality.Index, breaks= nclass.FD(df_clean$Pittsburgh.Sleep.Quality.Index), col="darkgoldenrod2"
    ,xlab="PSQI" ,main="Histogram of PSQI")
hist(df_clean$Pittsburgh.Sleep.Quality.Index, breaks= nclass.scott(df_clean$Pittsburgh.Sleep.Quality.Index))

summary(df_clean$target)
boxplot(df_clean$Pittsburgh.Sleep.Quality.Index~df_clean$target, col=c(5,2), main= "Target levels", xlab="Subjects with PSQI lower and greater than 5", ylab="PSQI score")

par(mfrow=c(1,2))
hist(df_clean$Pittsburgh.Sleep.Quality.Index[df_clean$Pittsburgh.Sleep.Quality.Index<=5], col="aquamarine",
     breaks=4, main="People with good sleep quality", xlab="PSQI <= 5")
hist(df_clean$Pittsburgh.Sleep.Quality.Index[df_clean$Pittsburgh.Sleep.Quality.Index>5], col="brown1",
     breaks=5, main="People with bad sleep quality", xlab="PSQI > 5")

par(mfrow=c(1,1))


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



#------------------------Mutual Information----------------------------------------------------------
#install.packages("infotheo")
library(infotheo)
str(df_clean)
data1<- discretize(df_clean[,-c(1,2,26)])
multiinformation(data1, method ="shrink")
mutinformation(data1$Pittsburgh.Sleep.Quality.Index, data1$Latency, method="shrink")
mm<-mutinformation(data1[,c(23,22,1,2,3,4,7,8,16,17)])
View(mm)

nclass.FD(df_clean$Pittsburgh.Sleep.Quality.Index)
nclass.scott(df_clean$Pittsburgh.Sleep.Quality.Index)
nrow(df_clean)^(1/3)*2

devtools::install_github("pohlio/tidyinftheo")
library(tidyinftheo)
mt_tib <- as_tibble(data1) %>% mutate_all(as.character)
mi_matrix<-mutual_info_matrix(mt_tib, c(2,4,6,9,14,16,23), normalized=TRUE) 

mutual_info_heatmap(mi_matrix, title=NULL, font_sizes=c(12,12))

mi_matrix2<-mutual_info_matrix(mt_tib, 23:1, normalized=F) 

View(mi_matrix2[mi_matrix2$V1=="Pittsburgh.Sleep.Quality.Index",])
mi_pits<- mi_matrix2[mi_matrix2$V1=="Pittsburgh.Sleep.Quality.Index",]
mi_pits$V2[mi_pits$MI>0.19]
mi_pits$MI= (natstobits(mi_pits$MI))/log2(3)
colnames(mi_pits)=c("Variable 1", "Variable 2","Mutual Information")

formattable(mi_pits[order(mi_pits$`Mutual Information`, decreasing = T)[1:11],], align=c("l","l","c"), digits=1, list(`Mutual Information` = percent))

mi_pits$MI <- natstobits(mi_pits$MI)
View(mi_pits)
View(MI)


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



#best performance at LOOCV 0.11, 24/27

#----------------------------------------------------------------------------------------------------------------------------

#Linear
#install.packages("DAAG")
library(DAAG)
DAAG::CVlm(data= df_clean, form.lm = formula(Pittsburgh.Sleep.Quality.Index ~ Sleep.Efficiency+Awakenings.No.+Age), m=10, seed=29, plotit=TRUE, printit=TRUE)


CVbinary(reduce.fit3, rand=NULL, nfolds=10, print.details=TRUE)


## LOGISTIC
reduce.fit3 = glm(target~ Latency+Sleep.Efficiency+Awakenings.No.+Deep.Total.Sleep, family=binomial, data=df_clean[,-c(1,2,24,25)])
summary(reduce.fit3)


cv.probs3 = predict(reduce.fit3, type="response")
cv.predict3 = rep("ok", 27)
cv.predict3[cv.probs3>0.6]="poor"
table(cv.predict3, df_clean$target)



#Logistic with CV
library(caret)

# define training control
train_control <- trainControl(method = "cv", number = 10,savePredictions = T)

# train the model on training set
model <- train(target ~ Age+Latency+N2.duration+Sleep.Efficiency,
               data = df_clean,
               trControl = train_control,
               method = "glm",
               family=binomial())

# print cv scores
summary(model)
model$results
glmfit= glm(target ~ Age+Latency+N2.duration+Sleep.Efficiency,data=df_clean, family=binomial)
library(pROC)
roc(df_clean$target, glmfit$fitted.values, plot=T, legacy.axes=T, print.auc=T)


# train the model on training set
model2 <- train(target ~ Age + Latency + N3.duration,
               data = df_clean,
               trControl = train_control,
               method = "glm",
               family=binomial())
glmfit2= glm(target ~ Age + Latency + N3.duration,data=df_clean, family=binomial)
roc(df_clean$target, glmfit2$fitted.values, plot=T, legacy.axes=T, print.auc=T)
# print cv scores
summary(model2)
model2$results #0.75-0.8 accuracy

#install.packages("pROC")
# Compute AUC for predicting Class with the variable CreditHistory.Critical
#roc(target ~ Age+Latency+N2.duration+Sleep.Efficiency, data=df_clean, plot=T, print.auc=T) 


# LDA
lda.fit2 = train(target ~ Age + Latency + N3.duration, data=df_clean, method="lda",
                trControl = trainControl(method = "cv"))
lda.fit2
lda.fit2$results
#0.72-0.80
library(MASS)
ldafit2 = lda(target~Age + Latency + N3.duration, data=df_clean[,-c(1,2,24,25)])
ldafit2

lda.fit = train(target ~ Age+Latency+N2.duration+Sleep.Efficiency, method="lda",
                trControl = trainControl(method = "cv"))
lda.fit
lda.fit$results
#0.78



# K nearest neighbours
y<- as.vector(df_clean[,26])
as.factor(y)
library(Rfast)
knn.cv( nfolds = 10, y=as.factor(y), x=as.matrix(df_clean[,c(4,6,8,18)]),k=5, 
       dist.type = "euclidean", type = "C", freq.option = 0, 
       pred.ret = FALSE, mem.eff = FALSE) 

#62-77%


####--------------------------------------------------------------------------------------------------------------
library(e1071)
tune_out3<-tune.svm(target ~  Age+Latency+N2.duration  , data = df_clean[,-c(1,2,24,25)] ,cost=c(8, 10, 12, 14), 
                    gamma=c(0.1,0.15,0.2, 0.22,0.25,0.28,0.3), kernel="radial",
                    tunecontrol= tune.control(cross=10))
summary(tune_out3)
tune_out3$best.performance #0.15   0.13-0.2 error 
tune_out3$best.parameters #gamma=0.25 cost= 8

tune_out4<-tune.svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(12,14,15,16,17), 
                    gamma=c(0.3 ,0.31, 0.32,0.3225, 0.3250,  0.3275, 0.33,0.335 ),kernel="radial", tunecontrol= tune.control(cross=10))
summary(tune_out4)
tune_out4$best.performance #0.15   0.13-0.2 error 
tune_out4$best.parameters #gamma=0.31 cost= 12
c<-svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)], cost=12, gammas=0.31, probability=T)
predict(c,df_clean$target)



tune_out4<-tune.svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(0.1,0.3,0.6,1,5), 
                    kernel="linear", tunecontrol= tune.control(cross=10))
summary(tune_out4)
tune_out4$best.performance #0.17-0.23 error 
tune_out4$best.parameters # cost= 1





#ROC
library(ROCR)     
pred <- prediction(predictions, labels)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10))

predictions=as.vector(rf_output$votes[,2])
pred=prediction(predictions,target)

perf_AUC=?performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))













#------------------------------------------------Logistic-----------------------------------------------------------------


full.fit = glm(target~., family=binomial, data= df_clean[,-c(1,2,24,25)] )
summary(full.fit)


#install.packages("car")
library(car)
vif(full.fit)


probs = predict(full.fit, type="response")
contrasts(df_clean$target)

pred = rep("ok", 27)
pred[probs>0.5]<-"poor"


table(pred, df_clean$target)


#install.packages("bestglm")
library(bestglm)


temp<- df_clean[,-c(1,2,24,25)]
temp$y<-rep(0,27)
temp$y[temp$target=="poor"]<-1
temp<-temp[,-c(5,8,13,14,18,19,20,21,22)]
bestglm(Xy = temp, IC="CV", CVArgs=list(Method="HTF", K=5,REP=1), family=binomial)

reduce.fit = glm(target~N2.duration, family=binomial, data=df_clean[,-c(1,2,24,25)])
summary(reduce.fit)


cv.probs = predict(reduce.fit, type="response")
cv.predict = rep("ok", 27)
cv.predict[cv.probs>0.5]="poor"
table(cv.predict, df_clean$target)



reduce.fit2 = glm(target~Sleep.Efficiency+Latency, family=binomial, data=df_clean[,-c(1,2,24,25)])
summary(reduce.fit2)


cv.probs2 = predict(reduce.fit2, type="response")
cv.predict2 = rep("ok", 27)
cv.predict2[cv.probs2>0.5]="poor"
table(cv.predict2, df_clean$target)



reduce.fit3 = glm(target~Age+Latency+N2.duration, family=binomial, data=df_clean[,-c(1,2,24,25)])
summary(reduce.fit3)


cv.probs3 = predict(reduce.fit3, type="response")
cv.predict3 = rep("ok", 27)
cv.predict3[cv.probs3>0.6]="poor"
table(cv.predict3, df_clean$target)



#-----------------------------------------LDA------------------------------------------------------------------------
library(MASS)
lda.fit = lda(target~., data=df_clean[,-c(1,2,24,25)])
lda.fit


lda.predict = predict(lda.fit)
lda.predict$class
table(lda.predict$class, df_clean$target)


lda.fit2 = lda(target~Latency+Sleep.Efficiency, data=df_clean[,-c(1,2,24,25)])
lda.fit2


lda.predict2 = predict(lda.fit2)
lda.predict2$class
table(lda.predict2$class, df_clean$target)




qda.fit = qda(target~Latency+Sleep.Efficiency, data=df_clean[,-c(1,2,24,25)])
qda.fit


qda.predict = predict(qda.fit)
qda.predict$class
table(qda.predict$class, df_clean$target)
tune_out4$best.performance #0.1-0.16 error 
tune_out4$best.parameters #gamma=0.28-0.31 cost= 15





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
summary(svm_fitt)
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
fittt<- svm(target ~ Age+ Latency + N3.duration, data = df_clean[,-c(1,2,24,25)] , kernel = "radial", cost=8, gamma=0.295)
summary(fittt)

# 10-fold
tune_out4<-tune.svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(12,14,15,16,17), 
                    gamma=c(0.3 ,0.31, 0.32,0.3225, 0.3250,  0.3275, 0.33,0.335 ),kernel="radial", tunecontrol= tune.control(cross=10))
summary(tune_out4)
tune_out4$best.performance #0.1-0.16 error 
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



library(tree)
controltree<- tree.control(nobs=27, mincut=0, minsize = 1, mindev = 0.1 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~ Sleep.Efficiency+Deep.Total.Sleep+Awakenings.No.+N2.duration+Latency , data=df_clean[,-c(1,2,24,25)], control= controltree  )
plot(treee)
text(treee)
summary(treee)

prune.tree(treee, method = "misclass")

fgl.cv <- cv.tree(treee,, prune.tree)
for(i in 2:5)  fgl.cv$dev <- fgl.cv$dev +
  cv.tree(treee,, prune.tree)$dev
fgl.cv$dev <- fgl.cv$dev/5
plot(fgl.cv)

fgl.cv


prune.tree(treee, method="misclass")
#pruned tree
controltree<- tree.control(nobs=27, mincut=0, minsize = 2, mindev = 0.00001 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~ Sleep.Efficiency+Deep.Total.Sleep+Awakenings.No.+N2.duration+Latency , data=df_clean[,-c(1,2,24,25)], control= controltree  )
plot(treee)
text(treee)
summary(treee)


controltree<- tree.control(nobs=27, mincut=4, minsize = 10, mindev = 0.1 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~ Sleep.Efficiency+Deep.Total.Sleep+Awakenings.No.+N2.duration+Latency , data=df_clean[,-c(1,2,24,25)], control= controltree  )
plot(treee)
text(treee)
summary(treee)




controltree<- tree.control(nobs=27, mincut=2, minsize = 4, mindev = 0.2 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~ Sleep.Efficiency+Awakenings.No.+Latency , data=df_clean[,-c(1,2,24,25)], control= controltree  )
plot(treee)
text(treee)
summary(treee)


plot(df_clean$target, df_clean$Light.Total.Sleep)




prune.tree(treee, method="misclass")
#pruned tree
controltree<- tree.control(nobs=27, mincut=4, minsize =8, mindev = 0.1 )  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~ Awakenings.No.+ Apneas.Hypopneas+REM.Latency, data=df_clean[,-c(1,2,24,25)], control= controltree  )
plot(treee)
text(treee)
summary(treee)



prune.tree(treee, method="misclass")
#pruned tree
controltree<- tree.control(nobs=26, mincut=3, minsize =10, mindev = 0.03)  #mincut=0, minsize = 2 for perfect fit
treee<- tree(target~  Awakenings.No.+Apneas.Hypopneas+REM.Latency, data=df_clean[-6,-c(1,2,24,25)], control= controltree  )
plot(treee)
text(treee)
summary(treee)





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
treee1<- tree(target~. , data=df_clean[,-c(1,2,24,25)], control= controltree  )
#"Age"         "Latency"     "BMI"
summary(treee1)
plot(treee1)
text(treee1, pretty = 0)
#predict(treee1, newdata = target, type="class")
cbind(df_clean$Age, df_clean$Latency,df_clean$BMI, df_clean$target)

prune.tree(treee1, method = "misclass")

fgl.cv1 <- cv.tree(treee1, prune.tree)
for(i in 2:5)  fgl.cv1$dev <- fgl.cv1$dev +
  cv.tree(treee1,, prune.tree)$dev
fgl.cv1$dev <- fgl.cv1$dev/5
plot(fgl.cv1)

fgl.cv1



controltree<- tree.control(nobs=27, mincut=2, minsize = 5, mindev = 0.005 )  #mincut=0, minsize = 2 for perfect fit
treee1<- tree(target~. , data=df_clean[,-c(1,2,24,25)], control= controltree  )
summary(treee1)
plot(treee1)
text(treee1, pretty = 0)


controltree<- tree.control(nobs=27, mincut=4, minsize = 8, mindev = 0.005 )  #mincut=0, minsize = 2 for perfect fit
treee1<- tree(target~. , data=df_clean[,-c(1,2,24,25)], control= controltree  )
summary(treee1)
plot(treee1)
text(treee1, pretty = 0)



#-----------------------------------------------------------------------
attach(df_clean)
plot(Age~target)
plot(Latency~target)
plot(N3.duration~target)


#attach(df_clean)
#w<-cbind(Age, Latency, BMI, target)
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





# Stratification
library(caret)
# Folds are created on the basis of target variable
folds <- createFolds(factor(data$target), k = 10, list = FALSE)



#-------------------------------------Trial and error, mostly error----------------------------------

library(ROCR)     
predictions=as.vector(rf_output$votes[,2])
pred=prediction(predictions,target)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))


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





#Logistic with CV
library(caret)

# define training control
train_control <- trainControl(method = "cv", number = 27 ,savePredictions = T)

# train the model on training set
model <- train(target ~ Age+Latency+N2.duration+Sleep.Efficiency,
               data = df_clean,
               trControl = train_control,
               method = "glm",
               family=binomial())

# print cv scores
summary(model)
model$results
glmfit= glm(target ~ Age+Latency+N2.duration+Sleep.Efficiency,data=df_clean, family=binomial)
library(pROC)
roc(df_clean$target, glmfit$fitted.values, plot=T, legacy.axes=T, print.auc=T)
#74%

# train the model on training set
model2 <- train(target ~ Age + Latency + N3.duration,
                data = df_clean,
                trControl = train_control,
                method = "glm",
                family=binomial())
glmfit2= glm(target ~ Age + Latency + N3.duration,data=df_clean, family=binomial)
roc(df_clean$target, glmfit2$fitted.values, plot=T, legacy.axes=T, print.auc=T)
# print cv scores
summary(model2)
model2$results #0.75-0.8 accuracy
#77.777%

#install.packages("pROC")
# Compute AUC for predicting Class with the variable CreditHistory.Critical
#roc(target ~ Age+Latency+N2.duration+Sleep.Efficiency, data=df_clean, plot=T, print.auc=T) 


# LDA
lda.fit2 = train(target ~ Age + Latency + N3.duration, data=df_clean, method="lda",
                 trControl = trainControl(method = "cv", number = 27))
lda.fit2
lda.fit2$results
#77.777%
#0.72-0.80
library(MASS)
ldafit2 = lda(target~Age + Latency + N3.duration, data=df_clean[,-c(1,2,24,25)])
ldafit2

lda.fit = train(target ~ Age+Latency+N2.duration+Sleep.Efficiency, method="lda",
                trControl = trainControl(method = "cv"))
lda.fit
lda.fit$results
#0.78



# K nearest neighbours
y<- as.vector(df_clean[,26])
as.factor(y)
library(Rfast)
knn.cv( nfolds = 27, y=as.factor(y), x=as.matrix(df_clean[,c(4,6,8,18)]),k=5, 
        dist.type = "euclidean", type = "C", freq.option = 0, 
        pred.ret = FALSE, mem.eff = FALSE) 
#k=3 66.6%
#k=5 70%
#62-77%


####--------------------------------------------------------------------------------------------------------------
library(e1071)
tune_out3<-tune.svm(target ~  Age+Latency+N2.duration  , data = df_clean[,-c(1,2,24,25)] ,cost=c(8, 10, 12, 14), 
                    gamma=c(0.1,0.15,0.2, 0.22,0.25,0.28,0.3), kernel="radial",
                    tunecontrol= tune.control(cross=27))
summary(tune_out3)
tune_out3$best.performance #0.15    
tune_out3$best.parameters #gamma=0.25 cost= 8

tune_out4<-tune.svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(12,14,15,16,17), 
                    gamma=c(0.3 ,0.31, 0.32,0.3225, 0.3250,  0.3275, 0.33,0.335 ),kernel="radial", tunecontrol= tune.control(cross=27))
summary(tune_out4)
tune_out4$best.performance #0.1111   0.13-0.2 error 
tune_out4$best.parameters #gamma=0.31 cost= 12
c<-svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)], cost=12, gammas=0.31, probability=T)
predict(c,df_clean$target)



tune_out4<-tune.svm(target ~ Age + Latency + N3.duration , data = df_clean[,-c(1,2,24,25)] ,cost=c(0.1,0.3,0.6,1,5), 
                    kernel="linear", tunecontrol= tune.control(cross=10))
summary(tune_out4)
tune_out4$best.performance #0.17-0.23 error 
tune_out4$best.parameters # cost= 1


#install.packages("formattable")
library(formattable)

c= c("K-Nearest Neighbours","Linear Discriminant Analysis","Logistic regression"," Linear Support Vector Machine","Radial Support Vector Machine")
a<- c("66-74%","78%", "76.7%","77-83%","80-87%")
b<-c("70.37%", "78.33%","74.07%", "76.66%", "85.18%")
results=cbind(c,a,b)
colnames(results)= c("Algorithm","10-fold CV","LOOCV")
results=as.data.frame(results)
formattable(results, align = c("l","c","c"))











