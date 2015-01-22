
objects()
#if you want to see if there is any correlation between two variables, you can generate a scatter plot.
plot(top20.data)
#Get the Log value of the contributions.
datosContribLog<-log(top20.data$contributions)
#Plot provinces vs Log(contributions)
plot(top20.data$province, datosContribLog, main="Province vs Log(Contributions)")

#Get the Log value of the contributions.
datosContribLog<-log(top20.data$contributions)
#Plot provinces vs Log(users)
datosUsers<-top20.data$contributions
datosUsersLog<-log(top20.data$users)
plot(top20.data$province, datosUsers, main="Province vs Users")
plot(top20.data$province, datosUsersLog, main="Province vs Log(Users)")
