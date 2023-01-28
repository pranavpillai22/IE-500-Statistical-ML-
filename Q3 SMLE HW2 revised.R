#Part 3(1)

Question_threetwo = read.csv('Wafer+Data.csv')
names(Question_threetwo) <- c('x_coordinate','y_coordinate','T')
# variable derivation

origin_x <- mean(Question_threetwo$x_coordinate)
origin_y <- mean(Question_threetwo$y_coordinate)

Question_threetwo$x_coordinate = Question_threetwo$x_coordinate - 0
Question_threetwo$y_coordinate = Question_threetwo$y_coordinate - 0
Question_threetwo$x_squared = Question_threetwo$x_coordinate^2
Question_threetwo$y_squared = Question_threetwo$y_coordinate^2
Question_threetwo$x_cubed = Question_threetwo$x_coordinate^3
Question_threetwo$y_cubed = Question_threetwo$y_coordinate^3
Question_threetwo$x_y_multiplied = Question_threetwo$x_coordinate * Question_threetwo$y_coordinate

# Linear Regressin Model
model_32 = lm(T~. , data = Question_threetwo)
summary(model_32)

library(MuMIn)
library(car)
library(MASS)
library(hier.part)

Question_threetwo.sqrt <- sqrt(Question_threetwo$x_y_multiplied)
model.lat <- lm(Question_threetwo.sqrt ~ Question_threetwo$x_coordinate , data = Question_threetwo)
model.long <- lm(Question_threetwo.sqrt ~ Question_threetwo$x_coordinate , data = Question_threetwo)
model.latlong <- lm(Question_threetwo.sqrt ~ Question_threetwo$x_coordinate + Question_threetwo$x_coordinate , data = Question_threetwo)

BIC (model.lat)
BIC (model.long)
BIC (model.latlong)


#Part 3(3)

par(mfcol=c(2,2)) 
plot(Question_threetwo$x_squared,residuals(model_32),xlab="X",ylab="Residuals")
plot(fitted(model_32),residuals(model_32),xlab="Fitted",ylab="Residuals")
plot(Question_threetwo$y_squared,fitted(model_32),xlab="Y",ylab="Fitted" )
# normality
qqnorm(residuals(model_32),ylab="Residuals")
qqline(residuals(model_32))
# hist(residuals(lmfit))

shapiro.test(residuals(model_32))
par(mfcol=c(1,1)) 
''
