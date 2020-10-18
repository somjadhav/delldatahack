library(readr)
county <- read_csv("DellDataHack/CountyData_growth.csv", 
                              col_types = cols(Dates = col_date(format = "%m/%d/%Y")))
names(county)[1] <- "Dates"
#county <- county[,-c(seq(from=8,to=87))]

bastrop = county$Bastrop
caldwell = county$Caldwell
hays = county$Hays
travis = county$Travis
williamson = county$Williamson
total = county$Total

dates = county$Dates
dates <- as.factor(dates)
#county$Dates <- dates


plot(seq(1,224),total,type="l",lty=1,xlab="Date",ylab="Number of Cases",axes=FALSE,main="Cumulative Cases by County")
lines(dates,total,col="blue")
lines(dates,bastrop,col="red")
lines(dates,caldwell,col="green")
lines(dates,hays,col="orange")
lines(dates,travis,col="purple")
lines(dates,williamson,col="pink")
legend("topleft",legend=c("Total","Bastrop","Caldwell","Hays","Travis","Williamson"), 
       lwd = rep(1,6),col=c("blue","red","green","orange","purple","pink"),ncol=1,x.intersp=0.5,y.intersp=0.3,cex=0.60,pt.cex = 1,bty="n")
axis(side=1, at=c(1,56,112,168,224),labels=c("03-04","05-01","06-26","08-21","10-16"))
axis(side=2, at=seq(0,50000,10000))

#phase 1
beg = 9
end = 94
n = end-beg + 1
cagr1 = ((county$Total[end]/county$Total[beg])^(1/n))-1
sect1min <- min(county$Growth[beg:end])
sect1max <- max(county$Growth[beg:end])
cagr1_sd <- (sect1max - sect1min)/4

#phase 2
beg = 94
end = 133
n = end-beg + 1
cagr2 = ((county$Total[end]/county$Total[beg])^(1/n))-1
sect2min <- min(county$Growth[beg:end])
sect2max <- max(county$Growth[beg:end])
cagr2_sd <- (sect2max - sect2min)/4

#phase 3
beg = 133
end = 170
n = end-beg + 1
cagr3 = ((county$Total[end]/county$Total[beg])^(1/n))-1
sect3min <- min(county$Growth[beg:end])
sect3max <- max(county$Growth[beg:end])
cagr3_sd <- (sect3max - sect3min)/4

#phase 4
beg = 170
end = 224
n = end-beg + 1
cagr4 = ((county$Total[end]/county$Total[beg])^(1/n))-1
sect4min <- min(county$Growth[beg:end])
sect4max <- max(county$Growth[beg:end])
cagr4_sd <- (sect4max - sect4min)/4

#July 4
beg = 115 #5 days before July 4
end = 135 #15 days after July 4
n = end - beg + 1
cagrPeak = ((county$Total[end]/county$Total[beg])^(1/n))-1
sectPeakmin <- min(county$Growth[beg:end])
sectPeakmax <- max(county$Growth[beg:end])
cagrPeak_sd <- (sectPeakmax - sectPeakmin)/4

set.seed(100)

#best case scenario - COVID cases continue to grow at CAGR4
#from now to Jan 1

results <- replicate(10000, {
  cases <- total[224]
  
  for(day in 1:41){
    spread = rnorm(1,cagr4,cagr4_sd)
    cases <- cases * (1 + spread)
  }
  november26.cases = cases
  
  for(day in 1:36){
    spread = rnorm(1,cagr4,cagr4_sd)
    cases <- cases * (1 + spread)
  }
  return (cases-november26.cases)
})
mean(results)

#middle case scenario - cases grow at cagr3 after
#major holidays (Thanksgiving, Halloween, Christmas)
results2 <- replicate(10000, {
  cases <- total[224]
  
  #October 17-31
  for(day in 1:15){
    spread = rnorm(1,cagr4,cagr4_sd)
    cases <- cases * (1 + spread)
  }
  
  #Post Halloween Spike
  for(day in 1:6){
    spread = rnorm(1,cagr3,cagr3_sd)
    cases <- cases * (1 + spread)
  }
  
  #November 7 to 26
  for (day in 1:20){
    spread = rnorm(1,cagr4,cagr4_sd)
    cases <- cases * (1 + spread)
  }
  
  #
  november26.cases <- cases
  
  #Post Thanksgiving Spike
  for (day in 1:10){
    spread = rnorm(1,cagr3,cagr3_sd)
    cases <- cases * (1 + spread)
  }
  
  #December 6 to 22
  for (day in 1:17){
    spread = rnorm(1,cagr4,cagr4_sd)
    cases <- cases * (1 + spread)
  }
  
  #Christmas to New Year spike
  for (day in 1:9){
    spread = rnorm(1,cagr3,cagr3_sd)
    cases <- cases * (1 + spread)
  }
  
  return (cases-november26.cases)
})
mean(results2)

#worst case - cases grow at cagrPeak after major holidays
results3 <- replicate(10000, {
  cases <- total[224]
  
  #October 17-31
  for(day in 1:15){
    spread = rnorm(1,cagr4,cagr4_sd)
    cases <- cases * (1 + spread)
  }
  
  #Post Halloween Spike
  for(day in 1:6){
    spread = rnorm(1,cagrPeak,cagrPeak_sd)
    cases <- cases * (1 + spread)
  }
  
  #November 7 to 26
  for (day in 1:20){
    spread = rnorm(1,cagr4,cagr4_sd)
    cases <- cases * (1 + spread)
  }
  
  #cases on 11/26
  november26.cases <- cases
  
  #Post Thanksgiving Spike
  for (day in 1:10){
    spread = rnorm(1,cagrPeak,cagrPeak_sd)
    cases <- cases * (1 + spread)
  }
  
  #December 6 to 22
  for (day in 1:17){
    spread = rnorm(1,cagr4,cagr4_sd)
    cases <- cases * (1 + spread)
  }
  
  #Christmas to New Year spike
  for (day in 1:9){
    spread = rnorm(1,cagrPeak,cagrPeak_sd)
    cases <- cases * (1 + spread)
  }
  
  return (cases-november26.cases)
})
mean(results3)

#populate dataframe with theoretical data
case1 = c()
cases <- total[224]
for(day in 1:77){
  spread = rnorm(1,cagr4,cagr4_sd)
  cases <- cases * (1 + spread)
  case1 = append(case1,cases)
}

case2 = c()
cases2 <- total[224]

case3 = c()
cases3 <- total[224]

#October 17-31
for(day in 1:15){
  spread = rnorm(1,cagr4,cagr4_sd)
  cases2 <- cases2 * (1 + spread)
  cases3 <- cases3 * (1 + spread)
  case2 <- append(case2,cases2)
  case3 <- append(case3,cases3)
}

#Post Halloween Spike
for(day in 1:6){
  spread2 = rnorm(1,cagr3,cagr3_sd)
  spread3 = rnorm(1,cagrPeak,cagrPeak_sd)
  cases2 <- cases2 * (1 + spread2)
  cases3 <- cases3 * (1 + spread3)
  case2 <- append(case2,cases2)
  case3 <- append(case3,cases3)
}

#November 7 to 26
for (day in 1:20){
  spread = rnorm(1,cagr4,cagr4_sd)
  cases2 <- cases2 * (1 + spread)
  cases3 <- cases3 * (1 + spread)
  case2 <- append(case2,cases2)
  case3 <- append(case3,cases3)
}

#Post Thanksgiving Spike
for (day in 1:10){
  spread2 = rnorm(1,cagr3,cagr3_sd)
  spread3 = rnorm(1,cagrPeak,cagrPeak_sd)
  cases2 <- cases2 * (1 + spread2)
  cases3 <- cases3 * (1 + spread3)
  case2 <- append(case2,cases2)
  case3 <- append(case3,cases3)
}

#December 6 to 22
for (day in 1:17){
  spread = rnorm(1,cagr4,cagr4_sd)
  cases2 <- cases2 * (1 + spread)
  cases3 <- cases3 * (1 + spread)
  case2 <- append(case2,cases2)
  case3 <- append(case3,cases3)
}

#Christmas to New Year spike
for (day in 1:9){
  spread2 = rnorm(1,cagr3,cagr3_sd)
  spread3 = rnorm(1,cagrPeak,cagrPeak_sd)
  cases2 <- cases2 * (1 + spread2)
  cases3 <- cases3 * (1 + spread3)
  case2 <- append(case2,cases2)
  case3 <- append(case3,cases3)
}

plot(seq(1,77),case3,type="l",lty=1,xlab="Date",ylab="Number of Cases",
     axes=FALSE,main="Cumulative Cases in Greater Austin (Simulated)")
lines(seq(1,77),case3,col="green")
lines(seq(1,77),case2,col="red")
lines(seq(1,77),case1,col="blue")

legend("topleft",legend=c("Best Case","Middle Case","Worst Case"), 
       lwd = rep(1,3),col=c("blue","red","green"),ncol=1,x.intersp=0.5,y.intersp=0.3,cex=0.60,pt.cex = 1,bty="n")
axis(side=1, at=c(1,41,77),labels=c("10-17","11-26","1/1"))
axis(side=2, at=seq(from=50000,to=150000,by=25000))

