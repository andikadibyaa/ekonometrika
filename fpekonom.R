library(readxl)
data_ekonom <- read_excel("C:/REGNON/EKONOM SUR DATA FIX.xlsx")
View(EKONOM_SUR_DATA_FIX)
str(EKONOM_SUR_DATA_FIX)
library(car)
library(tseries)
library(lmtest)

#Defining Variables
Y1=data_ekonom$y1
Y2=data_ekonom$y2
Y3=data_ekonom$y3

#a
z1=c(data_ekonom$x11)
z2=c(data_ekonom$x12)
z3=c(data_ekonom$x13)
z4=c(data_ekonom$x14)
z1 <- as.matrix(z1);z1
z2 <- as.matrix(z2)
z3 <- as.matrix(z3)
z4 <- as.matrix(z4)
coba <- cbind(z1, z2, z3,z4);coba

#b
z5=c(data_ekonom$x21)
z6=c(data_ekonom$x22)
z7=c(data_ekonom$x23)
z8=c(data_ekonom$x24)
z5 <- as.matrix(z5)
z6 <- as.matrix(z6)
z7 <- as.matrix(z7)
z8 <- as.matrix(z8)
coba1 <- cbind(z5, z6, z7,z8);coba1

#c
z9=c(data_ekonom$x31)
z10=c(data_ekonom$x32)
z11=c(data_ekonom$x33)
z12=c(data_ekonom$x34)
z9 <- as.matrix(z9)
z10 <- as.matrix(z10)
z11 <- as.matrix(z11)
z12 <- as.matrix(z12)
coba2 <- cbind(z9, z10, z11,z12);coba2

eq1=Y1~coba
eq2=Y2~coba1
eq3=Y3~coba2

#Descriptive Statistics
summary(Y1)
summary(Y2)
summary(Y3)
summary(coba)
summary(coba1)
summary(coba2)

#OLS Regression
olsreg1=lm(Y1~coba)
summary(olsreg1)

#Uji Normalitas
residual<-resid(olsreg1)

#adjust plot margins
par(mar = c(1, 1, 1, 1))

#create scatterplot
plot(1:30)
qqPlot(residual, dist="norm", main="Normal QQ Plot")
jarque.bera.test(residual)
olsreg2=lm(Y2~coba1)
summary(olsreg2)

#Uji Normalitas
residual<-resid(olsreg2)
qqPlot(residual, dist="norm", main="Normal QQ Plot")
jarque.bera.test(residual)
olsreg3=lm(Y3~coba2)
summary(olsreg3)
#Uji Normalitas
residual<-resid(olsreg3)
qqPlot(residual, dist="norm", main="Normal QQ Plot")
jarque.bera.test(residual)
#Alasan pakai SUR
cor(Y1,Y2)
cor(Y2,Y3)
cor(Y1,Y3)
library(systemfit)
system=list(eq1=eq1,eq2=eq2,eq3=eq3)
sur=systemfit(system,method="SUR",data=data_ekonom)
summary(sur)

# Perform OLS regression
olsreg1 <- lm(Y1 ~ coba)

# Extract the residuals
residuals_olsreg1 <- resid(olsreg1)

# Perform the White test on the residuals
white.test(residuals_olsreg1, power = 2)
