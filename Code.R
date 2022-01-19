# UPLOAD E ATTACHMENT-----------------------------------------------------------
dati = read.table("dati_muscl.txt", header = TRUE)
attach(dati)
dim(dati)
head(dati)
dati

# ANALISI UNIVARIATE------------------------------------------------------------
# bm (BODY MASS) quantità di massa corporea espressa (kg)
summary(bm)
sd(bm)

# wl (WORK LEVEL) livello di lavoro (calorie orarie)
summary(wl)
sd(wl)

# ho (HEAT OUTPUT) calorie consumate (calorie)
summary(ho)
sd(ho)

# CURTOSI-----------------------------------------------------------------------
install.packages("moments")
library(moments)
kurtosis(bm)
kurtosis(wl)
kurtosis(ho)
anscombe.test(ho)

# BOXPLOT-----------------------------------------------------------------------
boxplot(bm)
ggplot(data=dati,aes(x="Body Mass", y = bm))+
  geom_boxplot()
ggplot(data=dati,aes(x="Work Level", y = wl))+
  geom_boxplot()
ggplot(data=dati,aes(x="Heat Output", y = ho))+
  geom_boxplot()

# GRAFICI DI DENSITA'-----------------------------------------------------------
library(ggplot2)
#bm
# Basic density
p <- ggplot(dati, aes(x=bm)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(xintercept = median(bm),      # Add line for median
             col = "black",linetype="dashed",lwd = 1)
# Add mean line
p+ geom_vline(aes(xintercept=mean(bm)),
              color="red", linetype="dashed", size=1)
p

#wl
p2 <- ggplot(dati, aes(x=wl)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(xintercept = median(wl),      # Add line for median
             col = "black",linetype="dashed",lwd = 1)
# Add mean line
p2+ geom_vline(aes(xintercept=mean(wl)),
              color="red", linetype="dashed", size=1)
p2

#ho
p3 <- ggplot(dati, aes(x=ho)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(xintercept = median(ho),      # Add line for median
             col = "black",linetype="dashed",lwd = 1)
# Add mean line
p3+ geom_vline(aes(xintercept=mean(ho)),
               color="red", linetype="dashed", size=1)
p3

# COEFFICIENTI DI VARIAZIONE----------------------------------------------------
# bm
(sd(bm)/mean(bm))*100
#wl
(sd(wl)/mean(wl))*100
#ho
(sd(ho)/mean(ho))*100

# INDICI DI ASIMMETRIA----------------------------------------------------------
skew = function(x){
n = length(x)
s3 = sqrt(var(x)*(n-1)/n)^3
mx = mean(x)
sk = sum((x-mx)^3)/s3
sk/n}
skew(bm)
skew(wl)
skew(ho)

# TEST DI NORMALITA'---------------------------------------------------------
# bm (BODY MASS)
qqnorm(bm, main = "Body Mass")
qqline(bm, col="red")
shapiro.test(bm)

# wl (WORK LEVEL)
qqnorm(wl, main = "Work Level")
qqline(wl, col="red")
shapiro.test(wl)

# ho (HEAT OUTPUT)
qqnorm(ho, main = "Heat Output")
qqline(ho, col="red")
shapiro.test(ho)

install.packages("nortest")
library(nortest)
lillie.test(bm)
lillie.test(wl)
lillie.test(ho)

# TRASFORMAZIONI QUADRATICHE DELLE VARIABILI------------------------------------
shapiro.test(bm)
shapiro.test(bm^2)
shapiro.test(wl)
shapiro.test(wl^2)
shapiro.test(ho)
shapiro.test(ho^2)

# ANALISI BIVARIATE-------------------------------------------------------------
plot(dati)

# scatterplot tra wl e ho
plot(wl,ho)
ggplot(dati, aes(x = wl, y = ho)) +
  geom_point()

# scatterplot tra bm e ho
plot(bm, ho)
ggplot(dati, aes(x = bm, y = ho)) +
  geom_point()

# scatterplot tra bm e wl
plot(bm,wl)
ggplot(dati, aes(x = bm, y = wl)) +
  geom_point()

# CORRELAZIONI (PEARSON e SPEARMAN)---------------------------------------------
cor(dati, method = "pearson")
cor.test(wl, ho, method = "pearson")
cor.test(bm,ho,method = "pearson")
cor.test(bm,wl,method="pearson")

cor(dati, method="spearman")
cor.test(wl, ho, method = "spearman")
cor.test(bm,ho,method = "spearman")
cor.test(bm,wl,method="spearman")

# MODELLO LINEARE---------------------------------------------------------------
modello1 = lm(ho ~ bm+wl, data = dati)
summary(modello1)
vif(modello1)
# STIME STANDARDIZZATE C.BETA---------------------------------------------------
install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(modello1)

#RAPPRESENTAZIONE GRAFICA DEL MODELLO LINEARE-----------------------------------
scatterplot3d(bm,wl,ho)
library(scatterplot3d)
s3d <- scatterplot3d(bm, wl, ho, pch=16, highlight.3d = TRUE, type = "h")
fit0 <- lm(ho ~ bm+wl)
s3d$plane3d(fit0)
?scatterplot3d
# ANALISI DELLA MULTICOLLINEARITA'----------------------------------------------
library(car)
vif(modello1)

#INTERVALLI DI CONFIDENZA PER I PARAMETRI DI REGRESSIONE------------------------
confint(modello1)

#ANALISI DEI RESIDUI------------------------------------------------------------
res = rstandard(modello1)
qqnorm(res,main = "Residui del modello")
qqline(res, col="red")
shapiro.test(res)


#RES.V.VAL.ST. E VALORI STIMATI VS VALORI OSSERVATI (OMOSCHEDASTICITA')---------
ysti = fitted(modello1)
ggplot(dati, aes(x = ysti, y = res)) +
  geom_point()

plot(ho,ysti)
abline(0,1,col="red")

ggplot(dati,                                     
       aes(x = ho,
           y = ysti)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 0.8)

# PREVISIONI--------------------------------------------------------------------
install.packages("propagate")
library(propagate)
predict(modello1)
newdata<-data.frame(bm=30, wl=20)
predict(modello1,newdata,interval="prediction")
predict(nl_fit,newdata,interval=c("prediction"))
newdata1<-data.frame(bm=20,wl=0)
predict(nl_fit,newdata=newdata1)
predictNLS(nl_fit,newdata)
predictNLS(nl_fit,newdata1)

# MODELLO NON LINEARE GLAZEBROOK & DYE -----------------------------------------
nl_fit = nls(ho ~ b0+b1*bm+(wl/(b3+b4*bm)),data = dati, start = list(b0=-138,b1=4.5,b3=0.08,b4=0.003))
summary(nl_fit)

# MODELLO NON LINEARE CON COSTANTI SETTATE A 1 ---------------------------------
nl_fit_one = nls(ho ~ b0+b1*bm+(wl/(b3+b4*bm)),data = dati, start = list(b0=-116.593,b1=4.2128,b3=0.03183,b4=0.0039))
summary(nl_fit_one)

AIC(nl_fit_one)
AIC(nl_fit)

#INTERVALLI DI CONFIDENZA PER I PARAMETRI DI REGRESSIONE------------------------
confint(nl_fit_one)

#RAPPRESENTAZIONE GRAFICA MODELLO NON LINEARE-----------------------------------
scatterplot3d(bm,wl,ho)
s3d <- scatterplot3d(bm, wl, ho, pch=16, highlight.3d = TRUE, type = "h")
s3d$plane3d(mod)
#ANALISI DEI RESIDUI NON LIN.---------------------------------------------------
res = residuals(nl_fit)
qqnorm(residuals(nl_fit), main = "Residui del modello")
qqline(residuals(nl_fit), col="red")
shapiro.test(residuals(nl_fit))

# residui v. valori stimati
ysti_nl = fitted(nl_fit)
ggplot(dati, aes(x = ysti_nl, y = residuals(nl_fit))) +
  geom_point()

# valori stimati v. valori osservati
ggplot(dati,                                     
       aes(x = ho,
           y = ysti_nl)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 0.8)

# AIC e BIC MODELLI-------------------------------------------------------------
AIC(modello1)
AIC(nl_fit)
BIC(modello1)
BIC(nl_fit)

#ADDITIONAL GRAPHS (MODELLI)----------------------------------------------------
# grafico 3d lineare
# x, y, z variables
x <- bm 
y <- wl 
z <- ho 
# Compute the linear regression 
fit_nova <- lm(z ~ x + y, data = dati)
# predict values on regular xy grid
grid.lines = 50
x.pred <- seq(floor(min(x)), ceiling(max(x)), length.out = grid.lines)
y.pred <- seq(floor(min(y)), ceiling(max(y)), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit_nova, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit_nova)
# scatter plot with regression plane
scatter3D(x, y, z, pch = 16, cex = 0.5, colvar=FALSE, 
          col="black",theta = 30, phi = 20, bty="b2",
          expand =0.7, 
          xlab = "Body Mass", ylab = "Work Level", zlab = "Heat Output",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA,col ="red",fit = fitpoints ),main = " ")

# PUNTI ANOMALI----------------------------------------------------------------
library(car)
outlierTest(modello1)

# PUNTI LEVA-------------------------------------------------------------------
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(modello1)

# PUNTI INFLUENTI--------------------------------------------------------------
cutoff <- 4/(nrow(dati)-length(modello1$coefficients)-2)
plot(modello1, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

influencePlot(modello1, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook’s distance")

# MODELLO DI REGRESSIONE LINEARE (OSSERVAZIONE 3 ESCLUSA)----------------------
dati_no3 = read.table("dati_no_oss3.txt", header = TRUE)
modello_no3 = lm(ho ~ bm+wl, data = dati_no3)
summary(modello_no3)
confint(modello_no3)
vif(modello_no3)

# STIME STANDARDIZZATE C.BETA---------------------------------------------------
install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(modello_no3)

#RAPPRESENTAZIONE GRAFICA DEL MODELLO LINEARE-----------------------------------
scatterplot3d(bm,wl,ho)
library(scatterplot3d)
s3d <- scatterplot3d(bm, wl, ho, pch=16, highlight.3d = TRUE, type = "h")
fitno3 <- lm(dati_no3$ho ~ dati_no3$bm+dati_no3$wl)
s3d$plane3d(fitno3)
?scatterplot3d
# ANALISI DELLA MULTICOLLINEARITA'----------------------------------------------
library(car)
vif(modello1)

#INTERVALLI DI CONFIDENZA PER I PARAMETRI DI REGRESSIONE------------------------
confint(modello1)

#ANALISI DEI RESIDUI------------------------------------------------------------
res = rstandard(modello1)
qqnorm(res,main = "Residui del modello")
qqline(res, col="red")
shapiro.test(res)

#RES.V.VAL.ST. E VALORI STIMATI VS VALORI OSSERVATI (OMOSCHEDASTICITA')---------
ysti = fitted(modello1)
ggplot(dati, aes(x = ysti, y = res)) +
  geom_point()

plot(ho,ysti)
abline(0,1,col="red")

ggplot(dati,                                     
       aes(x = ho,
           y = ysti)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 0.8)
