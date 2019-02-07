library(dplyr)
library(ggplot2)

dat<- read.csv("C:/Users/User/Desktop/sleep2.csv")
dat
dat2<-read.csv("C:/Users/User/Desktop/sleep3.csv")
dat2

head(dat)
head(dat2)
#removing second row of dat
dat<- dat[-1,]
names(dat)
head(dat)
names(dat2)

#transforming and merging dat and dat2
dat2<-dat2 %>% select(1:3)
dat2<- slice(dat2, 1:32)
str(dat2)

names(dat2)[names(dat2) == 'Subjects'] <- 'Participant'
str(dat2)



#retain only the ones with matching participant code
dat0<-inner_join(dat,dat2,by="Participant")
str(dat0)
dat0
#30 participants left


#retain all when merged
datfull<- full_join(dat, dat2, by="Participant")
str(datfull)
datfull

#checking for NAs in bot questionaires
dat0_filt<-dat0 %>% filter(!is.na(dat0$Epworth.sleepiness.Scale) | !is.na(dat0$Pittsburgh.Sleep.Quality.Index) )
#27 participants left
#1 NA in Epworth, no NAs in Pitts
str(dat0_filt)
dat0$Epworth.sleepiness.Scale
dat0$Pittsburgh.Sleep.Quality.Index

#extract group (all NAs)
dat0_filt<- dat0_filt[,-5]

#---------------------------------------------------------------------
attach(dat0_filt)
names(dat0_filt)

lapply(dat0_filt, class)
dat0_filt$Total.Sleep.Time<-as.numeric(as.character(Total.Sleep.Time))

#oops, faster way
dat0_filt[,5: ncol(dat0_filt)]<-dat0_filt[,5: ncol(dat0_filt)] %>% lapply(function(x) dat0_filt[,5: ncol(dat0_filt)]<-as.numeric(as.character(x)))
lapply(colnames(dat0_filt) ,function(x) c(class(dat0_filt[,x]), x))


str(dat0_filt)
levels(dat0_filt$Sex)
#we should probably exclude no. of movements
#---------------------------NAs------------------------------------------------
dat0_filt[dat0_filt=='NA'] <- NA
newdat0_filt <- dat0_filt[rowSums(is.na(dat0_filt)) > 0,]
dat0_filt[27,2]<- NA
#removed No of movement 
df<-dat0_filt[,-11]
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
plot(BMI)
plot(BMI~ Age)
plot(Total.Sleep.Time)
plot(REM.duration)

plot(Pittsburgh.Sleep.Quality.Index ~ Age)
plot(Pittsburgh.Sleep.Quality.Index ~ BMI)
plot(Epworth.sleepiness.Scale ~ Age)
plot(Epworth.sleepiness.Scale ~ BMI)

ggplot(df_clean, aes(Pittsburgh.Sleep.Quality.Index,Age)) + geom_smooth()
ggplot(df_clean, aes(Age)) + geom_histogram(binwidth = 5 )
ggplot(df_clean, aes(Age)) + geom_dotplot()
ggplot(df_clean, aes(Age)) + geom_area(stat = "bin", binwidth=4)

hist(df_clean$Pittsburgh.Sleep.Quality.Index,breaks = 5,col = "Royal Blue")
hist(df_clean$Epworth.sleepiness.Scale,breaks = 5,col = "Royal Blue")

boxplot(Age, horizontal = TRUE)
boxplot(Pittsburgh.Sleep.Quality.Index)
boxplot(Total.Sleep.Time)

#-------------------------------Correlation------------------------------------
install.packages("corrplot")
library(corrplot)
#correlation matrix
cor(df_clean$N3.Latency, df_clean$Total.Sleep.Time)
cormat=cor(as.matrix(df_clean[,c(-1,-2)]))
cormat1<- cor(as.matrix(df_clean[,c(-1,-2,-3,-4)]))
corrplot::corrplot(cormat,method = "circle", tl.col = "black", is.corr = FALSE)
corrplot::corrplot(cormat,method="number", tl.col = "black", is.corr = FALSE)
corrplot::corrplot(cormat,method="pie", tl.col = "black", is.corr = FALSE)

#lapply(df_clean,is.na)

mean(df_clean$Total.Sleep.Time)

#-------------------------Descriptive Stats-----------------------------------

datmean <- sapply(df_clean[,3:ncol(df_clean)],"mean" ,na.rm=TRUE)
datsd<-sapply(df_clean[,3:ncol(df_clean)], "sd", na.rm=TRUE)
datvar<- sapply(df_clean[,3:ncol(df_clean)], "var", na.rm=TRUE)
datmin<- sapply(df_clean[,3:ncol(df_clean)], "min", na.rm=TRUE)
datmax<- sapply(df_clean[,3:ncol(df_clean)], "max", na.rm=TRUE)
datmedian<- sapply(df_clean[,3:ncol(df_clean)], "median", na.rm=TRUE)
datrange<- sapply(df_clean[,3:ncol(df_clean)], "range", na.rm=TRUE)

descr<- cbind(datmean,datmedian, datvar, datsd,datmin, datmax, range=datmax-datmin)
colnames(descr)<-c("mean", "median", "variance", "standard dev", "min", "max", "range")
descr

#---------------------Scaling---------------------------------------------------
df0<- scale(df_clean[,c(-1,-2)], center=TRUE, scale=TRUE)
