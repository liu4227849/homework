x<-c(1, 0, 2, 0, 3, 1, 0, 1, 2, 0)
y<-c(16, 9, 17, 12, 22, 13, 8, 15, 19, 11)
yx<-lm(y~x)
summary(yx)
sum((y[]-mean(y))^2) 
var(y)*(length(x)-1) #these two are calculating
cor(x,y)^2
sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
top=0
bot=0
xbar=mean(x)
ybar=mean(y)
for (i in 1:length(x)){
  top=top+(x[i]-xbar)*(y[i]-ybar)
  bot=bot+(x[i]-xbar)^2
}
top/bot  #these two are the same

yhat=4*x+10.2
sum((yhat-y)^2)



varb1=sum((yhat-y)^2)/sum((x-mean(x))^2)/(length(x)-2)
4+qt(1 - 0.05/2, length(x)-2)*sqrt(varb1)
confint(yx, 'x', 1-0.05)
varb0=(sum((yhat-y)^2)/(length(x)-2))*(1/length(x)+(mean(x)^2/sum((x-mean(x))^2)))

es = resid(yx) 
sum(es^2)/(length(x)-2)
t=1/sqrt(varb1)
p