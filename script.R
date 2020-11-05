library(imputeTS)
library(readxl)
TeaEx <- read_excel("C:/Users/Sasha/Desktop/Tea.xlsx")
tsTeaEx3 <- ts( TeaEx [,3], start=2014, frequency = 12 )
plot(tsTeaEx3)
#восстановление
tsTeaEx3mid <- na_mean(tsTeaEx3, option = "mean", maxgap = Inf)
plot(tsTeaEx3mid)
tsTeaEx3sea <- na_seasplit( tsTeaEx3, algorithm = "interpolation", find_frequency = FALSE, maxgap = Inf )
plot(tsTeaEx3sea)
#разложение временного ряда
stl(tsTeaEx3sea [,1], t.window=13, s.window="periodic", robust=TRUE)
perem <- stl(tsTeaEx3sea [,1], t.window=13, s.window="periodic", robust=TRUE) 
plot (perem)
#прогнозирование
ETS <- forecast(ets(tsTeaEx3sea), h=12)
STL <-stlf(tsTeaEx3sea, lambda=0, h=12, biasadj=TRUE)
ARIMA <- forecast(auto.arima(tsTeaEx3sea, lambda=0, biasadj=TRUE),h=12)
NNAR <- forecast(nnetar(tsTeaEx3sea), h=12)
TBATS <- forecast(tbats(tsTeaEx3sea, biasadj=TRUE), h=12)
Combination <-(ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] +
                 +                    NNAR[["mean"]] + TBATS[["mean"]])/5
theme_set(theme_light(base_size = 16))
autoplot(tsTeaEx3sea, linetype = "dashed") +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Рік") +
  ylab("Експорт чаю Україна")
guides(colour=guide_legend(title=""))
scale_x_continuous(expand = c(0, 0))
scale_y_continuous(expand = c(0, 0))
scale_x_continuous(breaks= seq(2008, 2020, 2))