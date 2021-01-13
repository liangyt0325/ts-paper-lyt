library(parallel)
cl = makePSOCKcluster(8)

# 失败天数是实际损失超过所估计的 VaR 值所返回的结果，失败率是失败天数与样本期的比例

Real_Au1 <-xts(read.zoo("./data/Real_Au.csv",header=TRUE,sep=','))

Au_N_RV=ugarchroll(spec=Lst5[[1]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RV'],n.ahead = 1, n.start = 241, refit.every = 1, refit.window = "moving", forecast.length = 126,solver = "hybrid", fit.control = list(),cluster = cl,calculate.VaR = TRUE, VaR.alpha = 0.05, keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Au_N_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_T_RV=ugarchroll(spec=Lst5[[2]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RV'],n.ahead = 1, n.start = 241, refit.every = 1, refit.window = "moving", forecast.length = 126,solver = "hybrid", fit.control = list(),cluster = cl,calculate.VaR = TRUE, VaR.alpha = 0.05, keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Au_T_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_ST_RV=ugarchroll(spec=Lst5[[3]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RV'],n.ahead = 1, n.start = 241, refit.every = 1, refit.window = "moving", forecast.length = 126,solver = "hybrid", fit.control = list(),cluster = cl,calculate.VaR = TRUE, VaR.alpha = 0.05, keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Au_ST_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_GH_RV=ugarchroll(spec=Lst5[[4]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RV'],n.ahead = 1, n.start = 241, refit.every = 1, refit.window = "moving", forecast.length = 126,solver = "hybrid", fit.control = list(),cluster = cl,calculate.VaR = TRUE, VaR.alpha = 0.05, keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Au_GH_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)


## 预测

ghyp_spec.mod= ugarchspec(
  variance.model = list(model = "realGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
  distribution.model = "ghyp") # 广义双曲线分布

AuLst.ghyp.fit = ugarchfit(data=Real_Au[,"rt"], spec = ghyp_spec.mod, solver = "hybrid", realizedVol = Real_Au[,"RV"])

fore = ugarchforecast(AuLst.ghyp.fit,n.ahead=5)


