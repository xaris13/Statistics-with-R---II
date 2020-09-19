library(caret)
library(aod)
require(dplyr)
require(glmnet)
library(lmtest)
library(corrplot)

setwd("/Users/xarismallios/Desktop")
mydatasource<- read.csv(file="project 12019-2020-2821912.csv", header=TRUE, sep=";")
# My id ends to 2 p2821912
mydatasource<-mydatasource[mydatasource$CODE == 2,]
dft <- as.data.frame(mydatasource)
#set them to the right class
dft[1]<- lapply(dft[1],as.numeric)
dft[2:10]<- lapply(dft[2:10],as.factor)
dft[11:14]<- lapply(dft[11:14],as.numeric)
dft[15]<- lapply(dft[15],as.factor)
dft[16:20]<- lapply(dft[16:20],as.numeric)
dft[21]<- lapply(dft[21],as.factor)
#we dont need it
dft$CODE<-NULL

#new1 from df for transformations - pdays transform to factor
new1<- dft
new1$pdays[new1$pdays != 999] <-1
new1$pdays[new1$pdays == 999] <-0
new1[13]<- lapply(new1[13],as.factor)

#exploratory data
boxplot(new1$pdays~new1$SUBSCRIBED)
ggplot(data = new1) +
  geom_bar(mapping = aes(x = new1$contact))
#full model
full_model<-glm(new1$SUBSCRIBED ~. , data = new1, family = "binomial")
summary(full_model)
#step wise method with AIC 
step_model<-step(full_model,direction = "both")
##exploratory data
y <- as.factor(new1$SUBSCRIBED)
ggplot(data=new1,aes(x = month))+
  aes(x=month, y=..count../nrow(new1))+geom_bar(aes(fill = y))+ 
  theme(axis.text.x = element_text(angle = 90, vjust = .5))+xlab("Month")+
  ylab("Percent")
#multicolinearity
car::vif(step_model)

step_model2<-glm(formula = new1$SUBSCRIBED ~ -1 + age + default + contact + month + 
                   duration + pdays + previous + nr.employed , 
                 family = "binomial", data = new1)
#deviance of vars
anova(step_model, test ="Chisq")

## We can plot the data...
predicted.data <- data.frame(
  probability.of.sb=step_model$fitted.values,
  sub= new1$age)
fit = glm(new1$SUBSCRIBED ~ + age + default + contact + month + 
            duration + pdays + previous + nr.employed, data=new1, family=binomial)
new1$SUBSCRIBED = predict(fit, newdata=new1, type="response")
plot(new1$SUBSCRIBED ~  new1$age + new1$default + new1$contact + new1$month + 
       new1$duration + new1$pdays + new1$previous + new1$nr.employed, col="red4")

ggplot(data=predicted.data, aes(x=sub, y=probability.of.sb)) +
  geom_point(aes(color=sub), size=2) +
  xlab("Age") +
  ylab("Predicted probability of getting subscribded to loan")
#hypothesis testing - if is better than the contant one
wald.test(b = coef(step_model), Sigma = vcov(step_model), Terms = 1)
waldtest(step_model,test="Chisq")
library(lmtest)
lrtest(step_model)
anova(step_model, test ="Chisq")
with(step_model, pchisq(deviance, df.residual, 
                         lower.tail = FALSE))
with(step_model, pchisq(null.deviance - deviance,
                         +                      + df.null - df.residual, lower.tail = FALSE))
#goodness of fit
#pseudo R2 mcfaden
ll.null<-step_model$null.deviance/-2
ll.proposed<- step_model$deviance/-2
R2<-(ll.null - ll.proposed)/ll.null
#prediction - 2 sets - training and test 
Train <- createDataPartition(new1$SUBSCRIBED, p=0.8, list=FALSE)
training <- new1[ Train, ]
testing <- new1[ -Train, ]
pt<-predict(step_model,training, type = "response")
p_classt <- ifelse(pt>.50,"Yes","No")
table(p_classt)
table(testing$SUBSCRIBED)






