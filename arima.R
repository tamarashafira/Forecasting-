oil=read.csv("D:\\UII (STATISTIKA)\\prak ARW\\6\\oil1.csv", sep = ";")
View(oil)

oil.train=ts(oil$Price[1:49], end = c(2010), frequency = 12)
oil.train
oil.test=ts(oil$Price[117:128], end = c(2017,8), frequency = 12)
oil.test

library(tseries)
library(forecast)
Arima.Model = auto.arima (oil.test,seasonal = F, lambda = T)
Arima.Model

#time series
oil=ts(oil$Price, start=c(2007,1), frequency=12)
oil
#plot dari data
ts.plot(oil, col="brown", main="TS: Oil")

library(tseries)
#Uji ADF utk menguji stasioneritas data bensin
#H0: data tidak stasioner (data mengandung unit root stasioner dalam mean)
#H1: data stasioner (data tidak mengandung unit root stasioner dalam mean)

adf.test(oil)
#selain adf stasioner juga bisa dilihat pada grafik korelogram
library(forecast)
#Utk membagi window menjadi dua dalam satu baris
par(mfrow=c(1,2))

#Uji ACF dan PACF
Acf(oil, lag.max = 24) #jk ACF menurun secara lambat berarti data tdk stasioner 
Pacf(oil, lag.max = 24)

#Oleh karna data tdk stasioner, maka data didiferensi agar stasioner
oil.diff1=diff(oil, diffrence=1) #satu untuk data diferensiasi pertama 
ts.plot(oil.diff1, main="TS: Oil (Diferensi Orde 1)") #data sudah bergerak disekitar rata" stasioner

#Uji ADF dari bensin.diff1
adf.test(oil.diff1)

#Model ARIMA diperoleh melalui kolegram
#Uji ACF dan PACF
par(mfrow=c(1,2))# untuk melihat 2 garfik yang ditampilkan 
Acf(oil.diff1, lag.max = 24) #jika ACF menurun secara lambat berarti data tdk stasioner
Pacf(oil.diff1, lag.max = 24)


#ACF untuk melambangkan model MA 
#PACF untuk model AR
#karena pada grafik pacf yang keluar garis titik" hanya satu sehingga AR nya satu 
#Estimasi parameter utk ke-3 model
model1=Arima(oil, order=c(1,1,2)) #untuk mengetahui model yang ada layak atau tidak layak
model1
model2=Arima(oil, order=c(1,1,0))
model2
model3=Arima(oil, order=c(0,1,2))
model3

#Untuk melihat sig. dari koefisien model
printstatarima <- function (x, digits = 4,se=TRUE,...){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}

#Melihat signifikansi dari koefisien model, tambahkan fungsi printstatarima
printstatarima(model1)
printstatarima(model2)
printstatarima(model3)


#Uji diagnostik utk uji autokorelasi yang diuji yang signifikan
tsdiag(model2)
tsdiag(model3)

#Peramalan dgn model ARIMA(0,1,2)
pred.oil=predict(model3, n.ahead=5)
pred.oil

#Fitting model ARIMA(0,1,2)
fitted(model3)

#syntax Auto ARIMA
Arima.Model = auto.arima (oil, d=1)
Arima.Model

#prediksi 
pred.data= predict(Arima.Model, n.ahead=5)
pred.data

