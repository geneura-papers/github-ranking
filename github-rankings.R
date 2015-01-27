
## ----setup, cache=FALSE,echo=FALSE---------------------------------------
library("ggplot2")
load(".RData")


## ----kable,results="asis",echo=FALSE-------------------------------------
kable(top20.data)


## ----byfollow, echo=FALSE, out.width='.49\\linewidth',fig.height=6, fig.subcap=c('# Contributions', '#Users'),----
ggplot(top20.data.bycontrib,aes(x=province,y=contributions)) + geom_bar(stat='identity')+xlab("Cities")+ coord_flip()
ggplot(top20.data.byuser,aes(x=province,y=users)) + geom_bar(stat='identity')+xlab("Cities")+ coord_flip()


## ----zipf, echo=FALSE, out.width='.49\\linewidth',fig.height=6, fig.subcap=c('Contributions', 'Users'),----
ggplot()+geom_point(data=barcelona.data, aes(x=log(seq_along(contributions)), y= log10(contributions),color='BCN')) +geom_point(data=zaragoza.data,aes(y=log10(contributions), x=log(seq_along(contributions)),color='ZGZ'))+geom_point(data=madrid.data, aes(x=log(seq_along(contributions)), y= log10(contributions),color='MAD')) +geom_point(data=granada.data,aes(y=log10(contributions), x=log(seq_along(contributions)),color='GRX'))+geom_point(data=sevilla.data, aes(x=log(seq_along(contributions)), y= log10(contributions),color='SVQ')) +geom_point(data=valencia.data,aes(y=log10(contributions), x=log(seq_along(contributions)),color='VLC'))+labs(color="City")+xlab("Rank")
ggplot()+geom_point(data=barcelona.data, aes(x=log(seq_along(followers)), y=log10(sort(followers,decreasing=TRUE)),color='BCN')) +geom_point(data=zaragoza.data,aes(y=log10(sort(followers,decreasing=TRUE)), x=log(seq_along(followers)),color='ZGZ'))+geom_point(data=madrid.data, aes(x=log(seq_along(followers)), y=log10(sort(followers,decreasing=TRUE)),color='MAD')) +geom_point(data=granada.data,aes(y=log10(sort(followers,decreasing=TRUE)), x=log(seq_along(followers)),color='GRX'))+geom_point(data=sevilla.data, aes(x=log(seq_along(followers)), y=log10(sort(followers,decreasing=TRUE)),color='SVQ')) +geom_point(data=valencia.data,aes(y=log10(sort(followers,decreasing=TRUE)), x=log(seq_along(followers)),color='VLC'))+labs(color="City")+ylab('log10(followers)')+xlab("Rank")


## ----zipfexp,echo=FALSE--------------------------------------------------
#Taken from http://stats.stackexchange.com/questions/6780/how-to-calculate-zipfs-law-coefficient-from-a-set-of-top-frequencies

p<-zaragoza.data$contributions/sum(zaragoza.data$contributions)

lzipf <- function(s,N) -s*log(1:N)-log(sum(1/(1:N)^s))

opt.f <- function(s) sum((log(p)-lzipf(s,length(p)))^2)

opt <- optimize(opt.f,c(0.5,10))
zipf.city <- data.frame(city='Zaragoza',exponent=opt$minimum, obj=opt$objective)

p<-sevilla.data$contributions/sum(sevilla.data$contributions)

opt.f <- function(s) sum((log(p)-lzipf(s,length(p)))^2)

opt<- optimize(opt.f,c(0.5,10))
zipf.city <- rbind(zipf.city,data.frame(city='Sevilla',exponent=opt$minimum, obj=opt$objective))

p<-granada.data$contributions/sum(granada.data$contributions)

opt.f <- function(s) sum((log(p)-lzipf(s,length(p)))^2)

opt <- optimize(opt.f,c(0.5,10))
zipf.city <- rbind(zipf.city,data.frame(city='Granada',exponent=opt$minimum, obj=opt$objective))

p<-valencia.data$contributions/sum(valencia.data$contributions)

opt.f <- function(s) sum((log(p)-lzipf(s,length(p)))^2)

opt <- optimize(opt.f,c(0.5,10))
zipf.city <- rbind(zipf.city,data.frame(city='Valencia',exponent=opt$minimum, obj=opt$objective))

p<-madrid.data$contributions/sum(madrid.data$contributions)

opt.f <- function(s) sum((log(p)-lzipf(s,length(p)))^2)

opt <- optimize(opt.f,c(0.5,10))
zipf.city <- rbind(zipf.city,data.frame(city='Madrid',exponent=opt$minimum, obj=opt$objective))


p<-barcelona.data$contributions/sum(barcelona.data$contributions)

opt.f <- function(s) sum((log(p)-lzipf(s,length(p)))^2)

opt <- optimize(opt.f,c(0.5,10))
zipf.city <- rbind(zipf.city,data.frame(city='Barcelona',exponent=opt$minimum, obj=opt$objective))

kable(zipf.city)


## ----lorenz, echo=FALSE, fig.height=4, fig.width=8-----------------------
ggplot()+geom_point(data=barcelona.data, aes(x=seq_along(contributions)/length(contributions), y=cumsum(sort(contributions))/sum(contributions),color='BCN'))+geom_point(data=zaragoza.data, aes(x=seq_along(contributions)/length(contributions), y=cumsum(sort(contributions))/sum(contributions),color='ZGZ'))+geom_point(data=madrid.data, aes(x=seq_along(contributions)/length(contributions), y=cumsum(sort(contributions))/sum(contributions),color='MAD'))+geom_point(data=granada.data, aes(x=seq_along(contributions)/length(contributions), y=cumsum(sort(contributions))/sum(contributions),color='GRX'))+geom_point(data=sevilla.data, aes(x=seq_along(contributions)/length(contributions), y=cumsum(sort(contributions))/sum(contributions),color='SVQ'))+geom_point(data=valencia.data, aes(x=seq_along(contributions)/length(contributions), y=cumsum(sort(contributions))/sum(contributions),color='VLC'))+xlab('rank')+ylab('contributions')+labs(color="City")


## ----gini,echo=FALSE,results='asis'--------------------------------------
library(ineq)
gini.city <- data.frame(city='Madrid', gini= ineq(madrid.data$contributions,type='Gini'))
gini.city <- rbind(gini.city,  data.frame(city='Granada', gini= ineq(granada.data$contributions,type='Gini')))
gini.city <- rbind(gini.city,  data.frame(city='Zaragoza', gini= ineq(zaragoza.data$contributions,type='Gini')))
gini.city <- rbind(gini.city,  data.frame(city='Sevilla', gini= ineq(sevilla.data$contributions,type='Gini')))
gini.city <- rbind(gini.city,  data.frame(city='Barcelona', gini= ineq(barcelona.data$contributions,type='Gini')))
gini.city <- rbind(gini.city,  data.frame(city='Valencia', gini= ineq(valencia.data$contributions,type='Gini')))

knitr::kable(gini.city,format='latex')


## ----avg, echo=FALSE, fig.height=4, fig.width=8--------------------------
ggplot(top20.data.byavgcontrib,aes(x=province,y=avgcontrib)) + geom_bar(stat='identity')+coord_flip()+xlab("Contributions/users")


## ----impact, echo=FALSE, fig.height=4, fig.width=8-----------------------
ggplot() + geom_line(data=malaga.evol.data, aes(x=seq_along(users), y=users,color='AGP')) +  geom_line(data=sevilla.evol.data, aes(x=seq_along(users),y=users,color='SVQ'))+  geom_line(data=granada.evol.data, aes(x=seq_along(users),y=users,color='GRX')) + xlab("Sequence")+labs(color="City")


