library(parallel)
cl = makePSOCKcluster(8)
#AuVaR
#Lst5=list(norm_spec.mod,std_spec.mod,sstd_spec.mod,ghyp_spec.mod)
Au_N_RV=ugarchroll(spec=Lst5[[1]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RV'],n.ahead = 1,n.start = 241, refit.every = 1, refit.window = "moving", forecast.length =82,
  solver = "hybrid", fit.control = list(),cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Au_N_RV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_N_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

失败天数是实际损失超过所
估计的 VaR 值所返回的结果，失败率是失败天数与样本期的比例



Au_T_RV=ugarchroll(spec=Lst5[[2]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RV'],n.ahead = 1,n.start = 241, refit.every = 1, refit.window = "moving", forecast.length =82,
  solver = "hybrid", fit.control = list(),cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Au_T_RV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_T_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_ST_RV=ugarchroll(spec=Lst5[[3]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RV'],n.ahead = 1,n.start = 241, refit.every = 1, refit.window = "moving", forecast.length =82,
  solver = "hybrid", fit.control = list(),cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Au_ST_RV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_ST_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_GH_RV=ugarchroll(spec=Lst5[[4]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RV'],n.ahead = 1,n.start = 241, refit.every = 1, refit.window = "moving", forecast.length =82,
  solver = "hybrid", fit.control = list(),cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  #2020.7.1开始向前一步预测

report(Au_GH_RV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_GH_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

# BV
Au_N_BV=ugarchroll(spec=Lst5[[1]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'BV'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_N_BV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_N_BV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_T_BV=ugarchroll(spec=Lst5[[2]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'BV'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_T_BV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_T_BV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_ST_BV=ugarchroll(spec=Lst5[[3]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'BV'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_ST_BV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_ST_BV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_GH_BV=ugarchroll(spec=Lst5[[4]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'BV'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_GH_BV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_GH_BV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

# medRV
Au_N_medRV=ugarchroll(spec=Lst5[[1]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'medRV'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_N_medRV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_N_medRV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_T_medRV=ugarchroll(spec=Lst5[[2]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'medRV'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_T_medRV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_T_medRV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_ST_medRV=ugarchroll(spec=Lst5[[3]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'medRV'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_ST_medRV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_ST_medRV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_GH_medRV=ugarchroll(spec=Lst5[[4]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'medRV'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_GH_medRV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_GH_medRV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

# RK
Au_N_RK=ugarchroll(spec=Lst5[[1]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RK'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_N_RK, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_N_RK, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_T_RK=ugarchroll(spec=Lst5[[2]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RK'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_T_RK, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_T_RK, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_ST_RK=ugarchroll(spec=Lst5[[3]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RK'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_ST_RK, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_ST_RK, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Au_GH_RK=ugarchroll(spec=Lst5[[4]], data = Real_Au1[,"rt"], realizedVol = Real_Au1[,'RK'],n.ahead = 1,n.start =241, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Au_GH_RK, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Au_GH_RK, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)
```

```{r}
#AgVaR
Ag_N_RV=ugarchroll(spec=Lst5[[1]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'RV'],n.ahead = 1,n.start = 237, refit.every = 1, refit.window = "moving", forecast.length =82,
  solver = "hybrid", fit.control = list(),cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Ag_N_RV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_N_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_T_RV=ugarchroll(spec=Lst5[[2]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'RV'],n.ahead = 1,n.start = 237, refit.every = 1, refit.window = "moving", forecast.length =82,
  solver = "hybrid", fit.control = list(),cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Ag_T_RV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_T_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_ST_RV=ugarchroll(spec=Lst5[[3]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'RV'],n.ahead = 1,n.start = 237, refit.every = 1, refit.window = "moving", forecast.length =82,
  solver = "hybrid", fit.control = list(),cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  #2020.7.1开始向前一步预测
report(Ag_ST_RV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_ST_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_GH_RV=ugarchroll(spec=Lst5[[4]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'RV'],n.ahead = 1,n.start = 237, refit.every = 1, refit.window = "moving", forecast.length =82,
  solver = "hybrid", fit.control = list(),cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  #2020.7.1开始向前一步预测

report(Ag_GH_RV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_GH_RV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

# BV
Ag_N_BV=ugarchroll(spec=Lst5[[1]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'BV'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_N_BV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_N_BV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_T_BV=ugarchroll(spec=Lst5[[2]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'BV'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_T_BV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_T_BV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_ST_BV=ugarchroll(spec=Lst5[[3]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'BV'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_ST_BV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_ST_BV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_GH_BV=ugarchroll(spec=Lst5[[4]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'BV'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_GH_BV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_GH_BV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

# medRV
Ag_N_medRV=ugarchroll(spec=Lst5[[1]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'medRV'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_N_medRV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_N_medRV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_T_medRV=ugarchroll(spec=Lst5[[2]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'medRV'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_T_medRV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_T_medRV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_ST_medRV=ugarchroll(spec=Lst5[[3]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'medRV'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_ST_medRV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_ST_medRV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_GH_medRV=ugarchroll(spec=Lst5[[4]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'medRV'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_GH_medRV, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_GH_medRV, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

# RK
Ag_N_RK=ugarchroll(spec=Lst5[[1]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'RK'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_N_RK, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_N_RK, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_T_RK=ugarchroll(spec=Lst5[[2]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'RK'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_T_RK, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_T_RK, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_ST_RK=ugarchroll(spec=Lst5[[3]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'RK'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_ST_RK, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_ST_RK, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

Ag_GH_RK=ugarchroll(spec=Lst5[[4]], data = Real_Ag1[,"rt"], realizedVol = Real_Ag1[,'RK'],n.ahead = 1,n.start =237, refit.every = 1, refit.window = "moving",
  solver = "hybrid", fit.control = list(),forecast.length =82,cluster = cl,
  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  keep.coef = TRUE)  
report(Ag_GH_RK, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)
report(Ag_GH_RK, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)
```