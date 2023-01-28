#IE 500 SMLE HW4 Q2 
library(MASS)
data(mcycle)
mcycle_ss <- smooth.spline(mcycle$times, mcycle$accel, df = 5)
mcycle_ss
mcycle_ss1 <- smooth.spline(mcycle$times, mcycle$accel, df = 10)
mcycle_ss1
mcycle_ss2 <- smooth.spline(mcycle$times, mcycle$accel, df = 15)
mcycle_ss2
mcycle_ss3 <- smooth.spline(mcycle$times, mcycle$accel, df = 20)
mcycle_ss3
mcycle_ss4<-smooth.spline(mcycle$times,mcycle$accel,cv = T)
mcycle_ss4
plot(mcycle$times,mcycle$accel)
lines(mcycle_ss4, col = "yellow")
mcycle_ss5<-smooth.spline(mcycle$times,mcycle$accel,df= 10,cv = TRUE)
mcycle_ss5
plot(mcycle$times,mcycle$accel,col = "blue")
lines(mcycle_ss,lwd=2,col = "purple")
lines(mcycle_ss1,lwd=2,col = "green")
lines(mcycle_ss2,lwd=2,col = "red")
lines(mcycle_ss3,lwd=2,col = "black")
legend(1,70,legend = c("Smoothing Spline with 5 df","Smoothing Spline with 10 df",
                  "Smoothing Spline with 15 df","Smoothing Spline with 20 df"),
                col = c("purple","green","red","black"),lty = 1:4,cex = 0.8)
library(npreg)
mod.ss <- with(mcycle,ss(times,accel))
mod.ss
summary(mod.ss)
plot(mod.ss,xlab = 'Time (ms)',ylab = 'Acceleration (g)')

