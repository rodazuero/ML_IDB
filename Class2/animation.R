
library(animation)

saveHTML({
  x<- seq(-3,3,0.1)
  xguess=x*10000
  y<-10000*x
  fx=x^2+2*x+2
  df<-function(x){2*x+2}
  x0=3
  fxe<-function(x){x^2+2*x+2}
  fxeVector<-fx*0
  y[1]=x0
  for (i in 1:20) {
    plot(xguess,y, ylim = c(0.8, 18),xlim=c(-3,3),col="blue")
    lines(x,fx,col='green')
    x0=x0-0.1*df(x0)
    xguess[i+1]=x0
    y[i+1]=fxe(x0)
    ani.pause()
  }
}, img.name = "Grad_Descent", imgdir = "Grad_descent", htmlfile = "Grad_descent.html", 
autobrowse = FALSE, title = "Demo of 20 grad descent")

