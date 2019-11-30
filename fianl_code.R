library(readxl)
oscar1<-read_xlsx("~/Desktop/Fall 2018/Econ 213/Final/oscar6.xlsx")
options(scipen = 20)
options(digits = 4)
View(oscar1)
str(oscar1)

#deal other variables
attach(oscar1)
oscar1$london_critics_circle_film_winner<-as.numeric(oscar1$london_critics_circle_film_winner)
oscar1$best_picture<-as.factor(oscar1$best_picture)
oscar1$ny_winner<- as.factor(oscar1$ny_winner)
oscar1$bafta_winner<-as.factor(oscar1$bafta_winner)
oscar1$la_winner<-as.factor(oscar1$la_winner)
oscar1$gloden_globe_winner<-as.factor(oscar1$gloden_globe_winner)
oscar1$certificate.r<-factor(ifelse(substr(oscar1$certificate,1,1)=="R",1,0))
oscar1$certificate.pg<-factor(ifelse(substr(oscar1$certificate,1,2)=="PG",1,0))
oscar1$certificate.o<-factor(ifelse(oscar1$certificate.r==1|oscar1$certificate.pg==1,0,1))
oscar1$genre_bio<-factor(ifelse(oscar1$genre1=="Biography"|oscar1$genre2=="Biography"|oscar1$genre3=="Biography",1,0))
oscar1$genre_crime<-factor(ifelse(oscar1$genre1=="Crime"|oscar1$genre2=="Crime"|oscar1$genre3=="Crime",1,0))
oscar1$genre_drama<-factor(ifelse(oscar1$genre1=="Drama"|oscar1$genre2=="Drama"|oscar1$genre3=="Drama",1,0))
oscar1$genre_comedy<-factor(ifelse(oscar1$genre1=="Comedy"|oscar1$genre2=="Comedy"|oscar1$genre3=="Comedy",1,0))
oscar1$certificate.new<-factor(ifelse(oscar1$certificate.r==1,"R",ifelse(oscar1$certificate.pg==1,"PG","O")))
oscar1$metascore<-as.numeric(oscar1$metascore)
oscar1$votes<-as.numeric(oscar1$votes)
oscar1$user_reviews<-as.numeric(oscar1$user_reviews)
oscar1$critic_reviews<-as.numeric(oscar1$critic_reviews)
oscar1$budget<-as.numeric(oscar1$budget)/1000000
oscar1$opening_weekend<-as.numeric(oscar1$opening_weekend)/1000000
oscar1$Box_office_US<-as.numeric(oscar1$Box_office_US)/1000000
oscar1$month<-as.factor(oscar1$month)


#drop redundant variables
oscar1$certificate<-NULL
oscar1$X__1<-NULL
oscar1$genre1<-NULL
oscar1$genre2<-NULL
oscar1$genre3<-NULL


#data description
attach(oscar1)
summary(oscar1)
aggregate(best_picture,FUN = mean)
N=tapply(oscar1,length)
MU=tapply(oscar1,mean)
SD=tapply(oscar1,sd)
MIN=tapply(oscar1,min)
MED=tapply(oscar1,median)
MAX=tapply(oscar1,max)
result=cbind(N,MU,SD,MIN,MED,MAX)
result

library(epiDisplay)
tabpct(best_picture,certificate.pg)
tabpct(best_picture,certificate.o)
tabpct(best_picture,certificate.r)
tabpct(best_picture,certificate.new)
plot(name_length,best_picture)

table(certificate.new)
nl<-cbind(mean(name_length),median(name_length),sd(name_length),
          min(name_length),max(name_length))
stargazer(nl,type = "text",column.labels = c("mean","median",
         "st. dev.","min","max"),no.space=TRUE)

chisq.test(certificate.pg,best_picture)
chisq.test(certificate.r,best_picture)
chisq.test(certificate.o,best_picture)
chisq.test(certificate.new,best_picture)
                  
var.test(name_length,as.numeric(best_picture))
t.test(name_length,as.numeric(best_picture),var.equal=FALSE)

model.1<-glm(best_picture~duration+rate+metascore+votes+user_reviews+critic_reviews+budget+month+london_critics_circle_film_winner
             +awards_nominees+oscars_nominees+gloden_globe_winner+la_winner+bafta_winner+ny_winner+name_length+winning_rate+certificate.r
             +certificate.pg+certificate.o+genre_bio+genre_comedy+genre_crime+genre_drama,family=binomial(link="probit"))
oscar1$month4<-as.factor(ifelse(oscar1$month==4,1,0))
attach(oscar1)
model.2<-glm(best_picture~rate+month4+oscars_nominees+gloden_globe_winner+Box_office_US+budget
             +name_length+winning_rate+genre_drama,family=binomial(link="logit"))

# Test for collinearity
library(car)
vif(model.2)

# Pseudo R-sqaure
library(BaylorEdPsych)
PseudoR2(model.2)

# Robust standard error
coeftest(model.2, vcov=vcovHC(model.2, type="HC1"))

# Margin effect
library(margins)
margins(model.2)

pred=predict(model.2,oscar1,type="response")
yhat=1*(pred>0.5)
table(oscar1$best_picture,yhat)

# In-sample test
train_sub=sample(nrow(oscar1),7/10*nrow(oscar1))
train_data=oscar1[train_sub,]
test_data=oscar1[-train_sub,]
dim(train_data)
dim(test_data)

model.3<-glm(best_picture~rate+month4+oscars_nominees+gloden_globe_winner+Box_office_US+budget
             +name_length+winning_rate+genre_drama,family=binomial(link="logit"),data=train_data)

pred1=predict(model.3,test_data,type="response")
yhat1=1*(pred1>0.5)
table(test_data$best_picture,yhat1)

# Log box_office and budget
model.4<-glm(best_picture~rate+month4+oscars_nominees+gloden_globe_winner+log(Box_office_US)+log(budget)
             +name_length+winning_rate+genre_drama,family=binomial(link="logit"))

vif(model.4)
PseudoR2(model.2)
PseudoR2(model.4)
coeftest(model.4, vcov=vcovHC(model.2, type="HC1"))
margins(model.4)

pred2=predict(model.4,oscar1,type="response")
yhat2=1*(pred2>0.5)
table(oscar1$best_picture,yhat2)

model.5<-glm(best_picture~rate+month4+oscars_nominees+gloden_globe_winner+log(Box_office_US)+log(budget)
             +name_length+winning_rate+genre_drama,family=binomial(link="logit"),data=train_data)

pred3=predict(model.5,test_data,type="response")
yhat3=1*(pred3>0.5)
table(test_data$best_picture,yhat3)

library(plm)
cov.4<-vcovHC(model.4, type = "HC1")
r.se.4<-sqrt(diag(cov.4))

r2_model2<-data.frame(as.list(PseudoR2(model.2)))
r2_model4<-data.frame(as.list(PseudoR2(model.4)))

library(stargazer)
stargazer(model.2,model.4,model.4,se=list(NULL,NULL,r.se.4),type="html",out="~/Desktop/coefficient.html",
          add.lines=list(c("Adj.McFadden",round(r2_model2[,2],3),round(r2_model4[,2],3),round(r2_model4[,2],3)),
                         c("McFadden",round(r2_model2[,1],3),round(r2_model4[,1],3),round(r2_model4[,2],3))),
          omit.stat=c("ll","n"),column.labels=c("Model.1","Model.2","Model.2(robust)"))
          





