setwd("C:/Users/mw621/Desktop/À¯ºñ¿Â")
wk<-read.csv('HR_comma_sep.csv')

set.seed(1234)
library(sampling)
plot(x=wk$salary,y=wk$satisfaction_level)

wk1<-strata(c('salary'),size=c(30,30,30),method='srswor',data=wk)
wk2<-getdata(wk,wk1)


plot(x=wk2$time_spend_company,wk2$satisfaction_level)


plot(x=wk2$time_spend_company,wk2$left)

cor(wk$time_spend_company,wk$satisfaction_level)
