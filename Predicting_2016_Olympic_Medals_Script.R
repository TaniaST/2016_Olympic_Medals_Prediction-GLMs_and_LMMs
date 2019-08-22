oldat <- read.csv("rioolympics.csv")
install.packages("pscl")
install.packages("gamlss")
install.packages("forecast")
install.packages("cAIC4")
install.packages("knitr")
library(forecast)
library(dplyr)
library(ggplot2)
library(GGally)
library(MASS)
library(pscl)
library(gamlss)
library(tidyr)
library(lme4)
library(cAIC4)
library(gridExtra)
library(knitr)

head(oldat)
str(oldat)
#1. DEALING WITH MISSING DATA
#checking for NAs in columns bmi and gdp00 and gdp16 columns

#handling BMI column missing values 
oldat[oldat$bmi=="#N/A",]
#preparing and inserting missing values for bmi column:
countr<-oldat[oldat$bmi=="#N/A",c(1)]

j<-c(26.7, 27.4, 28.4, 28.2, 24.7, 24.4, 27, 26.9, 26.7, 27.2, 25.5, 27.2, 26.5, 23.6, 0, 26.2, 26, 21.7, 25.8, 0, 25.2, 25.4,23.2,28.7,22,26.1,23.4)
newbmi<-data.frame("country"=countr,"bmi"=as.numeric(j))

countrbmi<-oldat[oldat$bmi!="#N/A",c(1,37)]
countrbmi$bmi<-as.numeric(as.character(countrbmi$bmi))
countrbmi<-rbind(newbmi,countrbmi)
countrbmi
colnames(countrbmi)<-c("country","newbmi")
oldat<-inner_join(oldat,countrbmi,by="country")
oldat$bmi<-as.numeric(oldat$newbmi)
#we've inserted all the values except Kosovo and Puerto Rico, we temporarily inserted zeros for them. These values are not available,
#so we'll replace them with an overall mean (excluding these two missing values)
mean(oldat[oldat$bmi!=0,37])
oldat$bmi<-ifelse(oldat$bmi==0,mean(oldat[oldat$bmi!=0,37]),oldat$bmi)
#deleting the temporary newbmi column
oldat$newbmi<-NULL


#Handling gdp00 missing data
oldat[oldat$gdp00=="#N/A",]
#only one value is missing - the one for Afghanistan, let's insert it:

oldat$gdp00<-as.numeric(as.character(oldat$gdp00))
oldat[oldat$country=="Afghanistan",3]<-3532
oldat$gdp00
str(oldat)

#Handling gdp16 missing data
oldat[oldat$gdp16=="#N/A",]
oldat$gdp16<-as.numeric(as.character(oldat$gdp16))
oldat[oldat$country=="Cuba",7]<-91370
oldat[oldat$country=="Syrian Arab Republic",7]<-12377


#2.PREPARING DATA
#transforming categorical variables into factors
oldat$soviet<-as.factor(oldat$soviet)
oldat$host<-as.factor(oldat$host)
oldat$comm<-as.factor(oldat$comm)
oldat$muslim<-as.factor(oldat$muslim)
oldat$oneparty<-as.factor(oldat$oneparty)
#LONG FORMAT PREPARATION       
#adding 1996 results for the total number of medals and gold medals
str(oldat)

oldat1996<-data.frame(country.code=c("USA","GER","RUS","CHN","AUS","FRA","ITA","KOR","CUB","UKR","CAN","HUN","ROU","NED","POL","ESP","BUL","BRA","GBR","BLR","JPN","CZE","KAZ","GRE","SWE","KEN","SUI","NOR","DEN","TUR","NZL","BEL","NGR",
                                  "JAM","RSA","PRK","IRL","FIN","INA","YUG","ALG","ETH","IRI","SVK","ARG","AUT","ARM","CRO","POR","THA","NAM","SLO","MAS","MDA","UZB","GEO","MAR","TRI","BDI","CRC","ECU","HKG","SYR","AZE","BAH","TPE",
                                  "LAT","PHI","TGA","ZAM","IND","ISR","LTU","MEX","MGL","MOZ","PUR","TUN","UGA"),
                 gold96=c(44,20,26,16,9,15,13,7,9,9,3,7,4,4,7,5,3,3,1,1,3,4,3,4,2,1,4,2,4,4,3,2,2,1,3,2,3,1,1,1,2,2,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                 tot96=c(101,65,63,50,41,37,35,27,25,23,22,21,20,19,17,17,15,15,15,15,14,11,11,8,8,8,7,7,6,6,6,6,6,6,5,5,4,4,4,4,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

#creating N-1 edition won medals for each year
oldatlong<-left_join(oldat,oldat1996,by="country.code")
oldatlong$tot96<-ifelse(is.na(oldatlong$tot96),0,oldatlong$tot96)
oldatlong$gold96<-ifelse(is.na(oldatlong$gold96),0,oldatlong$gold96)
previoustot<-oldatlong[,c("country.code","tot96","tot00","tot04","tot08","tot12","gold96","gold00","gold04",
                      "gold08","gold12")]
colnames(previoustot)<-c("country.code","prevtot_2000","prevtot_2004","prevtot_2008","prevtot_2012","prevtot_2016",
                         "prevgold_2000","prevgold_2004","prevgold_2008","prevgold_2012","prevgold_2016")        

#renaming columns for the long format transformation
colnames(oldatlong)<-c("country","country.code","gdp_2000","gdp_2004","gdp_2008","gdp_2012","gdp_2016","pop_2000","pop_2004","pop_2008","pop_2012","pop_2016","soviet","comm","muslim","oneparty","gold_2000","gold_2004","gold_2008","gold_2012","gold_2016","tot_2000","tot_2004","tot_2008","tot_2012","tot_2016","totgold_2000","totgold_2004","totgold_2008","totgold_2012",
                   "totgold_2016","totmedals_2000","totmedals_2004","totmedals_2008","totmedals_2012","totmedals_2016","bmi","altitude","athletes_2000","athletes_2004","athletes_2008","athletes_2012","athletes_2016","host","gold_1996","tot_1996")
oldat.long<-left_join(oldatlong,previoustot,by="country.code")

#deleting the temporary 1996 columns
oldat.long$gold_1996<-NULL
oldat.long$tot_1996<-NULL
str(oldat.long)

#transforming data from wide to long format
oldatresh<-oldat.long[,c(1:12,17:36,39:43,45:54)]
oldat.long<-oldatresh%>%gather(key=variables,value=values,-c(country,country.code))%>%
  separate(variables,into=c("variable","year"),sep="_")%>%spread(variable,values)
oldat.long2<-left_join(oldat.long,oldatlong[,c(2,13:16,37:38,44)],by="country.code")
head(oldat.long2)

#creating a column for gdp per capita
oldat.long2<-mutate(oldat.long2,gdp_per_cap=gdp/pop)
head(oldat.long2)
oldat.long2$gdp<-NULL
oldat.long2$year<-as.integer(oldat.long2$year)
str(oldat.long2)
#training data set - long format
long.training<-oldat.long2%>%subset(.,year<=2012)
#test data set - long format
long.test<-oldat.long2%>%subset(.,year==2016)


#WIDE FORMAT PREPARATION

#let's focus on the total number of medals as response variable
#as 2016 data will be used for prediction, we base all our analysis on the data up to and including 2012

#the gdp, population,athletes columns are measuring the same parameters over different years
#so, we can reduce the number of columns we'll use by calculating the means. Instead of using gdp, let's use the scaled gdp_per_cap parameter. 
#in thousands of US dollars for the 2000-2012 years
#Past performance can be a predictor, so let's include the 2000-2008 years mean of total number of medals per country and gold medals per country columns

oldatwide<-mutate(oldat,gdp=rowMeans(oldat[,3:6],na.rm=TRUE))
oldatwide<-mutate(oldatwide,pop=rowMeans(oldat[,8:11],na.rm=TRUE))
oldatwide<-mutate(oldatwide,prevgold=rowMeans(oldat[,17:19],na.rm=TRUE))
oldatwide<-mutate(oldatwide,prevtot=rowMeans(oldat[,22:24],na.rm=TRUE))
oldatwide<-mutate(oldatwide,gdp_per_cap=gdp/pop)
oldatwide<-mutate(oldatwide,athletes=round(rowMeans(oldat[,39:42],na.rm=TRUE),0))
str(oldatwide)


#Regarding the total number of gold medals and total number of medals, as we fit the model for 
#the number of total and gold medals won by each country in 2012, let's keep only 2012 data
#for total number of medals and total number of gold medals distributed.

trainoldat<-oldatwide[,c(1,2,25,13:16,37,38,44,46:50)]
str(trainoldat)
#renaming the 2012 year columns
###names(trainoldat)[names(trainoldat)=="gold12"]<-"gold"
names(trainoldat)[names(trainoldat)=="tot12"]<-"tot"

#preparing test data
#using 2016 data and 2012 as previous performance for gold and total number of medals

testoldat<-mutate(oldat[,c(1,2,26,13:16,37,38,44,7,12,20,25)],
                  gdp_per_cap=gdp16/pop16)
testoldat<-mutate(testoldat,oldat$athletes16)
str(testoldat)

#renaming response variable columns to make them consistent with training set
names(testoldat)<-c("country","country.code","tot","soviet","comm","muslim","oneparty",
                    "bmi","altitude","host","gdp","pop","prevgold","prevtot","gdp_per_cap","athletes")
str(testoldat)

#3. WIDE FORMAT ANALYSIS
#3.1 exploratory analysis
#let's visualise the relationship of continuous variables with the pairs plot.
ggpairs(trainoldat[,c(3,8,9,11:15)],mapping=aes(alpha=0.3))

#we can see from the pairs plot that the total number of medals and number of gold medals are highly correlated,
#the number of athletes is also highly correlated with total number of medal and with number of gold medals.
#the population parameter shows moderate correlation with the number of medals
#We will need to drop some of these variables: as the response variable is total number of medals, let's drop the gold number of medals and number of athletes
#and only include the mean total number of medals won in previous games
# the number of athletes, the total number of medals and the number of gold medals seem to be highly correlated with the response
#the population parameter shows the moderate correlation with the response

#let's now visualise the categorical variables

#soviet covariate
g1<-ggplot(trainoldat, aes(x=as.factor(soviet), y=tot,fill=as.factor(soviet)))+
        geom_boxplot(alpha=0.5, outlier.alpha = 0)+theme(legend.position="right")+
        geom_jitter(aes(colour=as.factor(soviet),alpha=0.3),show.legend=FALSE)+ 
  scale_fill_discrete(name="Soviet variable",labels=c("Non-soviet","Soviet"))+
  labs(x="Soviet / Non-soviet countries",y="Total number of medals")

#muslim covariate
g2<-ggplot(trainoldat, aes(x=as.factor(muslim), y=tot, fill=as.factor(muslim)))+
        geom_boxplot(alpha=0.5, outlier.alpha=0)+theme(legend.position="right")+
        geom_jitter(aes(colour=as.factor(muslim),alpha=0.3),show.legend=FALSE)+ 
  scale_fill_discrete(name="Muslim variable", labels=c("Non-muslim","Muslim"))+
  labs(x="Muslim / Non-muslim countries",y="Total number of medals")

#oneparty covariate
g3<-ggplot(trainoldat, aes(x=as.factor(oneparty), y=tot, fill=as.factor(oneparty)))+
        geom_boxplot(alpha=0.5, outlier.alpha=0)+theme(legend.position="right")+
        geom_jitter(aes(colour=as.factor(oneparty),alpha=0.3),show.legend=FALSE)+
  scale_fill_discrete(name="One party variable", labels=c("Multiple parties","One party"))+
  labs(x="One party / Multiple parties countries",y="Total number of medals")

#comm covariate
g4<-ggplot(trainoldat, aes(x=as.factor(comm), y=tot, fill=as.factor(comm)))+
        geom_boxplot(alpha=0.5, outlier.alpha=0)+
        geom_jitter(aes(colour=as.factor(comm),alpha=0.3),show.legend=FALSE)+
  scale_fill_discrete(name="Communist variable", labels=c("Non-communist","Communist"))+
  labs(x="Communist / Non-communist countries",y="Total number of medals")

#host covariate
g5<-ggplot(trainoldat, aes(x=as.factor(host), y=tot, fill=as.factor(host)))+
        geom_boxplot(alpha=0.5, outlier.alpha=0)+theme(legend.position="right")+
        geom_jitter(aes(colour=as.factor(host),alpha=0.3),show.legend=FALSE)+
  scale_fill_discrete(name="Host variable", labels=c("Non-host","Host"))+
  labs(x="Host/ Non-host countries",y="Total number of medals")

#visualising all the graphs together
grid.arrange(g1,g2,g3,g4,g5,nrow=3)

#let's check whether the categorical variables are independent, applying chi-squared and Fisher tests:
# oneparty vs. comm
chit_one_com<-chisq.test(trainoldat$oneparty,trainoldat$comm)
fish_one_com<-fisher.test(trainoldat$oneparty,trainoldat$comm)

#both chi-squared and Fisher tests confirm that the oneparty and comm variables are independent
#as the p-value is greater than 0.05

#oneparty vs. Soviet
chit_one_sov<-chisq.test(trainoldat$oneparty,trainoldat$soviet)
fish_one_sov<-fisher.test(trainoldat$oneparty,trainoldat$soviet)
#both chi-squared and Fisher tests confirm that the oneparty and soviet variables are independent
#as the p-value is greater than 0.05

#oneparty vs. muslim
chit_one_mus<-chisq.test(trainoldat$oneparty,trainoldat$muslim)
fish_one_mus<-fisher.test(trainoldat$oneparty,trainoldat$muslim)
#both chi-squared and Fisher tests confirm that the oneparty and muslim variables are independent
#as the p-value is greater than 0.05

#oneparty vs. host
chit_one_hos<-chisq.test(trainoldat$oneparty,trainoldat$host)
fish_one_hos<-fisher.test(trainoldat$oneparty,trainoldat$host)
#both chi-squared and Fisher tests confirm that the oneparty and host variables are independent
#as the p-value is greater than 0.05

#soviet vs. muslim
chit_sov_mus<-chisq.test(trainoldat$soviet,trainoldat$muslim)
fish_sov_mus<-fisher.test(trainoldat$soviet,trainoldat$muslim)
#both chi-squared and Fisher tests confirm that the soviet and muslim variables are independent
#as the p-value is greater than 0.05


#soviet vs. comm
chit_sov_com<-chisq.test(trainoldat$soviet,trainoldat$comm)
fish_sov_com<-fisher.test(trainoldat$soviet,trainoldat$comm)
#both chi-squared and Fisher tests confirm that the soviet and comm variables are not independent
#as the p-value for both tests is less than 0.05 - we will need to drop one of them.
#We will drop the soviet variable.

#soviet vs. host
chit_sov_hos<-chisq.test(trainoldat$soviet,trainoldat$host)
fish_sov_hos<-fisher.test(trainoldat$soviet,trainoldat$host)
#both chi-squared and Fisher tests confirm that the soviet and host variables are independent
#as the p-value is greater than 0.05

#comm vs. muslim
chit_com_mus<-chisq.test(trainoldat$comm,trainoldat$muslim)
fish_com_mus<-fisher.test(trainoldat$comm,trainoldat$muslim)
#both chi-squared and Fisher tests confirm that the comm and muslim variables are independent
#as the p-value is greater than 0.05

#comm vs. host
chit_com_hos<-chisq.test(trainoldat$comm,trainoldat$host)
fish_com_hos<-fisher.test(trainoldat$comm,trainoldat$host)
#both chi-squared and Fisher tests confirm that the comm and host variables are independent
#as the p-value is greater than 0.05

#muslim vs. host
chit_mus_hos<-chisq.test(trainoldat$muslim,trainoldat$host)
fish_mus_hos<-fisher.test(trainoldat$muslim,trainoldat$host)
#both chi-squared and Fisher tests confirm that the muslim and host variables are not independent
#for chi-squared test the p-value is on the boundary at 0.0592 and for Fisher test 
#the p-value is less than 0.005 at 0.02217

#creating a table with the results of chi-squared and Fisher tests:
cat_var_tests<-data.frame("Chi-squared test"=round(c(chit_one_com$p.value,chit_one_sov$p.value,chit_one_mus$p.value,
                       chit_one_hos$p.value,chit_sov_mus$p.value,chit_sov_com$p.value,chit_sov_hos$p.value,
                       chit_com_mus$p.value,chit_com_hos$p.value,chit_mus_hos$p.value),2),
                     "Fisher test" = round(c(fish_one_com$p.value,fish_one_sov$p.value,fish_one_mus$p.value,fish_one_hos$p.value,
                       fish_sov_mus$p.value,fish_sov_com$p.value,fish_sov_hos$p.value,fish_com_mus$p.value,
                       fish_com_hos$p.value,fish_mus_hos$p.value),2),"Independence"=c(rep("independent",5),"associated",rep("independent",3),"on the boundary"))

rownames(cat_var_tests)<-c("oneparty vs. comm", "oneparty vs. soviet","oneparty vs. muslim",
                           "oneparty vs. host","soviet vs. muslim", "soviet vs. comm", "soviet vs. host",
                           "comm vs. muslim", "comm vs. host", "muslim vs. host")

cat_var_tests

#As result of the independence check between categorical explanatory variables,
#we've found out that soviet and comm variables are not independent, we will drop the soviet variable.
#The muslim and host variables seem to be on the boundary of independence, we can drop one of them

#For a Poisson model, we assume that the variance and mean of the response variable are the same.
#Let's check this assumption:
#visualising the distribution of the response variable
hist(trainoldat$tot, main="Histogram", xlab="Total number of medals")

#calculating the mean - 8.85
mean(trainoldat$tot)
#calculating the variance - 294.09
var(trainoldat$tot)

#As variance is much greater than mean, our data is overdispersed.
#checking for excess of zeros
nrow(trainoldat[trainoldat$tot==0,])
nrow(trainoldat)
24/108
#22.2% of zeros in tot column

###3.2 FITTING THE MODELS

#let's calculate the dispersion parameter:
#3.2.1.Poisson model
mod1<-glm(tot~comm+oneparty+host+bmi+altitude+pop+gdp_per_cap+prevtot,family=quasipoisson,data=trainoldat)
X2 <- sum(resid(mod1, type = "pearson")^2)
dp <- X2 / mod1$df.res
dp #it's equal to 4.539

drop1(mod1,test="F")
#let's drop oneparty variable, it has the highest p-value at 0.97
mod2<-glm(tot~comm+host+bmi+altitude+pop+gdp_per_cap+prevtot,
              family=quasipoisson,data=trainoldat)
drop1(mod2,test="F")
#let's drop pop variable with p-value at 0.72
mod3<-glm(tot~comm+host+bmi+altitude+gdp_per_cap+prevtot,
          family=quasipoisson,data=trainoldat)

drop1(mod3,test="F")
#dropping altitude variable with p-value at 0.47
mod4<-glm(tot~comm+host+bmi+gdp_per_cap+prevtot,
          family=quasipoisson,data=trainoldat)
drop1(mod4,test="F")
#dropping bmi variable with p-value at 0.35
mod5<-glm(tot~comm+host+gdp_per_cap+prevtot,
          family=quasipoisson,data=trainoldat)
drop1(mod5,test="F")
#dropping gdp_per_cap parameter with p-value at 0.09
mod6<-glm(tot~comm+host+prevtot,
          family=poisson,data=trainoldat)
#let's calculate the dispersion parameter:
X2 <- sum(resid(mod6, type = "pearson")^2)
dp <- X2 / mod6$df.res
dp #it's equal to 4.33

summary(mod6,dp)

# all the parameters are significant in this model, so we keep: comm, host and tot parameters)
#checking goodness of fit:
qchisq(df=104,p=0.95)
# chi-sq. parameter is 128.8 which is less than the residual deviance at 451.79, so the fit of the model is poor
#as the goal is predicting, we will assess this model along with the others to compare its predictive power

#3.2.2 Negative binomial model
mod.nb<-glm.nb(tot~comm+oneparty+host+bmi+altitude+pop+gdp_per_cap+prevtot,
               data=trainoldat)
summary(mod.nb)

#let's drop insignificant parameters from the model
#dropping bmi
mod.nb2<-glm.nb(tot~comm+oneparty+host+altitude+pop+gdp_per_cap+prevtot,
                data=trainoldat)
summary(mod.nb2)
#dropping pop
mod.nb3<-glm.nb(tot~comm+oneparty+host+altitude+gdp_per_cap+prevtot,
                data=trainoldat)
summary(mod.nb3)
#dropping oneparty
mod.nb4<-glm.nb(tot~comm+host+altitude+gdp_per_cap+prevtot,
                data=trainoldat)
summary(mod.nb4)

#dropping gdp_per_cap
mod.nb5<-glm.nb(tot~comm+host+altitude+prevtot,
                data=trainoldat)
summary(mod.nb5)
#dropping altitude
mod.nb6<-glm.nb(tot~comm+host+prevtot,
                data=trainoldat)
summary(mod.nb6)

#we only have three significant parameters: comm, host and tot variables
qchisq(df=104,p=0.95)
#chi-sq. coefficient for 104 degrees of freedom is 128.8 and the residual deviance is 124.55.
#This indicates that there is no lack of fit for the model. 


###3.2.3 Normal linear model
str(trainoldat)
norm.mod<-lm(tot~comm+oneparty+host+bmi+altitude+pop+gdp_per_cap+prevtot,data=trainoldat[,3:15])
norm.mod1<-step(norm.mod,direction="both")
summary(norm.mod1)
#let's drop host variable as it's p-value is 0.09
norm.mod2<-lm(tot ~ pop + prevtot,data=trainoldat)
summary(norm.mod2)
par(mfrow=c(2,2))
plot(norm.mod2)
#there is one outlier, row 103, but we cannot exclude it because it would mean we exclude one of 
#the countries we need to predict the data for. We are not interested in the overall trend, but in predicting the values for 
#each of the countries, so we keep the outlier.
#The normal QQ plot, fitted values vs. standardized residuals and residuals vs. leverage plots indicate the outliers as well.
#Multiple assumptions are violated in this model, but as our goal is prediction,
#we will assess this model along with the others

#3.2.4 Zero-inflated models
#as there are a lot of countries not winning any medals.Let's consider a zero-inflated model

#zero-inflated poisson model

zpois<-zeroinfl(tot~comm+host|prevtot,data=trainoldat)
summary(zpois)
#the zero-inflation coefficient seems to be significant, as Pr(>z) parameter is less than 0.05

#zero-inflated negative binomial model

zeronb<- zeroinfl(tot~comm+host|prevtot,
                       data = trainoldat, dist = "negbin")
summary(zeronb)
#zero-inflation coefficient is not significant for negative binomial model

#let's compare the AIC parameters of the models
cbind("Quasi-poisson"=AIC(mod6),"Neg.Binomial"=c(mod.nb6$aic),
      "Normal linear"=AIC(norm.mod2),
      "Zero-inflated poisson"=AIC(zpois),
      "Zero-inflated negative binomial"=AIC(zeronb))
#the negative binomial model has the best result at 567.2762 indicating the best trade-off between
#goodness of fit and simplicity of the model

###4.LONG FORMAT ANALYSIS
#4.1 Exploratory analysis

#ggpairs plot - to see if any correlation between predictors and between predictors and response variable:
ggpairs(long.training[,c(3,4,6:11,16,17,19)],mapping=aes(alpha=0.3))
#the response variable and prevtot, prevgold, athletes variables show high positive correlation
#the following predictors are strongly positively correlated, we will need to keep this in mind for variable selection:
#atheletes and prevtot, prevtot and prevgold, athletes and prevgold, totmed and tot gold, year and totgold, year and totmed

#4.2 Fitting the models

#linear mixed model with random intercept

lin.mix.mod<-lmer(tot ~ year+gdp_per_cap+pop+prevtot+bmi+altitude+(1|country),data=long.training)
#using stepwise selection based on conditional AIC:
mod.mix<-stepcAIC(lin.mix.mod,groupCandidates=c("comm","muslim","oneparty","host","country","muslim:country","comm:country",
                                               "host:country","oneparty:country"),
                 direction="both",trace=TRUE)
mod.mix$finalModel@call
#the model with 2 random intercepts is the best as per stepcAIC selection: year + gdp_per_cap + pop + prevtot + bmi + altitude + totmedals + totgold + (1 | host) + (1 | oneparty)
mod.intercept<-lmer(tot~ year + gdp_per_cap + pop + prevtot + bmi + altitude + (1 | host) + (1 | oneparty),data=long.training)
summary(mod.intercept)
round(confint(mod.intercept,oldNames=FALSE),2)
#the CI show that pop, gdp_per_cap, bmi, totgold, totmedals and altitude parameters are insignificant and 1|oneparty intercept as well.
#let's exclude these parameters from the model one by one:

#excluding altitude
mod.intercept<-lmer(tot~ year + gdp_per_cap + pop + prevtot + bmi + (1 | host) + (1 | oneparty),data=long.training)
round(confint(mod.intercept,oldNames=FALSE),2)
#excluding bmi
mod.intercept<-lmer(tot~ year + gdp_per_cap + pop + prevtot + (1 | host) + (1 | oneparty),data=long.training)
round(confint(mod.intercept,oldNames=FALSE),2)
#excluding pop
mod.intercept<-lmer(tot~ year + gdp_per_cap + prevtot + (1 | host) + (1 | oneparty),data=long.training)
round(confint(mod.intercept,oldNames=FALSE),2)
#excluding gdp_per_cap
mod.intercept<-lmer(tot~ year + prevtot + (1 | host) + (1 | oneparty),data=long.training)
round(confint(mod.intercept,oldNames=FALSE),2)
#excluding the oneparty random intercept

mod.intercept<-lmer(tot~year + prevtot + (1 | host),data=long.training)
summary(mod.intercept)
round(confint(mod.intercept,oldNames=FALSE),2)
#all the parameters in the model are significant now

#Linear mixed model with correlated random intercept and slope

#let's fit the model with correlated intercept and slope random effects
mod.slope<-lmer(tot~year + prevtot + (1 +prevtot| host),data=long.training)
summary(mod.slope)
round(confint(mod.slope,oldNames=FALSE),2)
#we can see that in the correlated intercept and slope model,
#the slope in terms of host random effect is insignificant

#Linear mixed model with uncorrelated random intercept and slope

#let's fit the uncorrelated model:
mod.slope.uncorr<-lmer(tot~year + prevtot + (1|host)+(0 +prevtot| host),data=long.training)
summary(mod.slope.uncorr)
round(confint(mod.slope.uncorr,oldNames=FALSE),2)
#the uncorrelated model shows the same result, so the random slope parameter is insignificant
#we will keep the random intercept only model


##5.PREDICTION PERFORMANCE ANALYSIS
#calculating 2012 predictions based on training set
predqpoistr<-predict(mod6,newdata=trainoldat,type="response")
prednbtr<-predict(mod.nb6,newdata=trainoldat,type="response")
predzpoistr<-predict(zpois,newdata=trainoldat,type="response")
predznbtr<-predict(zeronb,newdata=trainoldat,type="response")
prednormtr<-predict(norm.mod2,newdata=trainoldat,type="response")
predintercepttr<-predict(mod.intercept,newdata=long.training[long.training$year==2012,],type="response")
length(predintercepttr)

#measuring RMSE and MAE of the models:
comtr<-rbind(Quasipoisson=accuracy(predqpoistr,trainoldat$tot),Neg_binomial=accuracy(prednbtr,trainoldat$tot),
             Zero_infl_Poisson=accuracy(predzpoistr,trainoldat$tot),Zero_infl_nb=accuracy(predznbtr,trainoldat$tot),Normmod=accuracy(prednormtr,trainoldat$tot),
             ML_intercept=accuracy(predintercepttr,trainoldat$tot))

rownames(comtr)<-c("Quasi-poisson","Negative binomial","Zero-inflated poisson","Zero-inflated negative binomial", "Normal linear",
                   "Mixed linear with random intercept")
#visualising the results
kable(round(comtr[,c(2,3)],2), caption = "2012 prediction")
#Mixed linear model with random intercept is the best in terms of predictive power
#it has the lowest RMSE at 3.77 and MAE at 2.41


#let's predict the 2016 values based on the test set:
predqpois<-predict(mod6,newdata=testoldat,type="response")
prednb<-predict(mod.nb6,newdata=testoldat,type="response")
predzpois<-predict(zpois,newdata=testoldat,type="response")
predznb<-predict(zeronb,newdata=testoldat,type="response")
prednorm<-predict(norm.mod2,newdata=testoldat,type="response")
predlmerintercept<-predict(mod.intercept,newdata=long.test[long.test$year==2016,],type="response")

#predictions table
a<-cbind(round(predqpois,0),round(prednb,0),round(predzpois,0),round(predznb,0),round(prednorm,0),round(predlmerintercept,0),testoldat$tot)
colnames(a)<-c("Quasi-poisson","Negative binomial","Zero-inflated quasi-posisson","Zero-inflated negative binomial","Normal linear","LM with random intercept","Actual")
a<-as.data.frame(a)
a<-mutate(a,country=as.character(testoldat$country))

#measuring RMSE and MAE:
com<-rbind(Quasipoisson=accuracy(predqpois,testoldat$tot),Neg_binomial=accuracy(prednb,testoldat$tot),
           Zero_infl_Poisson=accuracy(predzpois,testoldat$tot),Zero_infl_nb=accuracy(predznb,testoldat$tot),Norm_lin=accuracy(prednorm,testoldat$tot),ML_intercept=accuracy(predlmerintercept,testoldat$tot))
rownames(com)<-c("Quasi-poisson","Negative binomial","Zero-inflated poisson","Zero-inflated negative binomial","Normal linear","LM with random intercept")
#visualising results:
com[,c(2,3)]

#preparing data for visualisation of the results, splitting the data into 3 sets:
#countries with actual medals less than 10
a1<-a[a$Actual<=10,]
#countries with actual medals from 10 to 40
a2<-a[a$Actual>=10&a$Actual<=40,]
#countries with medals more than 40
a3<-a[a$Actual>40,]

#reshaping data for the plots
Melted1<-reshape2::melt(a1, id.var='country')
colnames(Melted1)<-c("country","Models","value")
#plotting for countries with <10 medals
gpr1<-ggplot(Melted1,aes(x=country,y=value,col=Models))+
  geom_point(alpha=0.3,show.legend = TRUE)+
  ylab("Number of medals")+
  theme(axis.text.x = element_blank())+
  ggtitle("Countries with less than 10 medals")+theme(plot.title = element_text(size=10))


#reshaping data for the plots
Melted2<-reshape2::melt(a2, id.var='country')
colnames(Melted2)<-c("country","Models","value")
#plotting for countries with >=10 and <=40 medals
gpr2<-ggplot(Melted2,aes(x=country,y=value,col=Models))+
  geom_point(alpha=0.3,show.legend = FALSE)+
  ylab("Number of medals")+
  theme(axis.text.x = element_blank())+
  ggtitle("Countries with 10-40 medals")+theme(plot.title = element_text(size=10))

#reshaping data for the plots
Melted3<-reshape2::melt(a3, id.var='country')
colnames(Melted3)<-c("country","Models","value")
#plotting for countries with more than 40 medals
gpr3<-ggplot(Melted3,aes(x=country,y=value,col=Models))+
  geom_point(alpha=0.3,show.legend = FALSE)+
  ylab("Number of medals")+
  theme(axis.text.x = element_blank())+
  ggtitle("Countries with more than 40 medals")+theme(plot.title = element_text(size=10))

#visualising the plots together
grid.arrange(gpr3,gpr2,gpr1,
  widths = c(1, 1),
  layout_matrix = rbind(c(1, 2),
                        c(3, 3))
)

#showing results only for the best model vs. the actual values
#splitting into 3 sets as above
a4<-a[,c("country","LM with random intercept","Actual")]
a4.1<-a4[a4$Actual<=10,]
a4.2<-a4[a4$Actual>=10&a4$Actual<=40,]
a4.3<-a4[a4$Actual>40,]
#reshaping data for the plots
Melted1.1<-reshape2::melt(a4.1, id.var='country')
colnames(Melted1.1)<-c("country","Models","value")
#plotting for countries with less than 10 medals
gpr1<-ggplot(Melted1.1,aes(x=country,y=value,col=Models))+
  geom_point(alpha=0.3,show.legend = TRUE)+
  ylab("Number of medals")+
  theme(axis.text.x = element_blank())+
  ggtitle("Countries with less than 10 medals")+theme(plot.title = element_text(size=10))

#reshaping data for the plots
Melted2.1<-reshape2::melt(a4.2, id.var='country')
colnames(Melted2.1)<-c("country","Models","value")
#plotting for countries with medals from 10 to 40
gpr2<-ggplot(Melted2.1,aes(x=country,y=value,col=Models))+
  geom_point(alpha=0.3,show.legend = FALSE)+
  ylab("Number of medals")+
  theme(axis.text.x = element_blank())+
  ggtitle("Countries with 10-40 medals")+theme(plot.title = element_text(size=10))

#reshaping data for the plots
Melted3.1<-reshape2::melt(a4.3, id.var='country')
colnames(Melted3.1)<-c("country","Models","value")
#plotting for countries with >40 medals
gpr3<-ggplot(Melted3.1,aes(x=country,y=value,col=Models))+
  geom_point(alpha=0.3,show.legend = FALSE)+
  ylab("Number of medals")+
  theme(axis.text.x = element_blank())+
  ggtitle("Countries with more than 40 medals")+theme(plot.title = element_text(size=10))

#visualising the plots together
grid.arrange(gpr3,gpr2,gpr1,
             widths = c(1, 1),
             layout_matrix = rbind(c(1, 2),
                                   c(3, 3)))

#6.COMPARISON WITH ONLINE PREDICTIONS
#comparing results with online predictions for 6 top performing countries
online.pred<-data.frame(country=c("United States","China","United Kingdom","Russia","France","Germany"),
                        WSJ=c(101,82,52,51,43,51),
                        Goldman_Sachs=c(106,89,59,58,36,46),
                        Gonzales_Tuck_School=c(105,89,67,62,35,48),
                        Forrest=c(99,90,51,42,30,36),
                        Bredtmann=c(100,86,64,53,34,43),
                        Kupper=c(102,74,43,83,37,41))%>%inner_join(a,by="country")

online.pred<-online.pred[,c(1:7,13,14)]
#showing the results
online.pred
#evaluating the performance in terms of RMSE and MAE
accur<-rbind(WSJ=accuracy(online.pred$WSJ,online.pred$Actual),
             Goldman_Sachs=accuracy(online.pred$Goldman_Sachs,online.pred$Actual),
             Gonzales_Tuck_School=accuracy(online.pred$Gonzales_Tuck_School,online.pred$Actual),
             Forrest=accuracy(online.pred$Forrest,online.pred$Actual),
             Bredtmann=accuracy(online.pred$Bredtmann,online.pred$Actual),
             Kupper=accuracy(online.pred$Kupper,online.pred$Actual),
             Final_model=accuracy(online.pred$`LM with random intercept`,online.pred$Actual)
             )
rownames(accur)<-c("WSJ","Goldman_Sachs","Gonzales_Tuck_School","Forrest","Bredtmann","Kupper","Final model")
#showing the results of evaluation: 
accur[,c(2,3)]
accur_vis<-as.data.frame(round(accur[,c(2,3)],2))

kable(accur_vis[order(accur_vis$RMSE),])
#our model is the 5th best performing out of 7