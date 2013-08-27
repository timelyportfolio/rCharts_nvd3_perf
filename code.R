library(reshape2)
library(plyr)
library(quantmod)
library(PerformanceAnalytics)
library(rCharts)

data(managers)
managers.melt <- melt(data.frame(index(managers),managers),id.vars=1)
colnames(managers.melt) <- c("date","manager","perf")

managers.melt.calcs <- managers.melt

managers.melt.calcs <- ddply(
  managers.melt.calcs,
  c("manager"),
  transform,
  cumul=cumprod(1+na.fill(perf,0))
)

#gave up on using ddply the right way with rolling
#get sharpe
managers.melt.calcs$sharpe <- ddply(
  managers.melt.calcs,
  c("manager"),
  .fun=function(data){
    mysharpe = rollapply(
      as.xts(data$perf,order.by=data$date),
      width=36,
      na.pad=TRUE,
      FUN=function(x){
        SharpeRatio(x,FUN="StdDev")
      }
    )
    return(mysharpe)
  }
)[,2]

#gave up on using ddply the right way with rolling
#get drawdown
managers.melt.calcs$drawdown <- ddply(
  managers.melt.calcs,
  c("manager"),
  .fun=function(data){
    mydraw = Drawdowns(
      as.xts(data$perf,order.by=data$date)
    )
    return(mydraw)
  }
)[,2]

n1 <- nPlot(
  cumul~date,
  data = managers.melt.calcs,
  group = "manager",
  type = "lineWithFocusChart"
)
#tell nvd3 how to deal with dates
n1$xAxis(tickFormat =
  "#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#"
)
n1$x2Axis(tickFormat =
  "#!function(d) {return d3.time.format('%Y')(new Date( d * 86400000 ));}!#"
)
#add Angular control for y variable
n1$addControls("y", value = "cumul", values = names(managers.melt.calcs)[-(1:2)])

n1

#writeLines(n1$render(cdn=T),"index.html")
