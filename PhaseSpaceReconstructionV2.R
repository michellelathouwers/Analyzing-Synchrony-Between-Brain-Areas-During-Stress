#Baseline for phase space reconstruction

#packages if not installed yet:
#install.packages("nonlinearTseries")
#install.packages("crqa")
#install.packages("casnet")
#install.packages("tseriesChaos")
#install.packages("plot3D")
#install.packages("scatterplot3d")
#install.packages("rgl")
#install.packages("ggplot2")
#install.packages("Hmisc")
#install.packages("quantmod")

#packages
require(nonlinearTseries)
require(crqa)
require(casnet)
require(tseriesChaos)
require(plot3D)
require(scatterplot3d)
require(rgl)
require(ggplot2)
require(Hmisc)
options(scipen=999)

#preprocessing steps:
#load data
Relax_data <- read.csv("~/Uni/M Jaar 1/Sem2/ComS/Relax-20231211T181913Z-001/Relax/Relax_sub_1.csv")
head(Relax_data)

#choose your desired complex systems  --> Om het juiste complex system te vinden moet hierbij alleen de "X..." worden aangepast. Welke nodig is, kan terug gevonden worden in de header in de code hiervoor
System1 = Relax_data$X2
System2 = Relax_data$X3

#plot fp1 with f7
plot(System1,type ='l', xlab = "Time", ylab="Value", main="FP1 & F7 Time Series")
lines(System2,type ='l',col ='2')
legend("topright",c("X", "Y"), col=c("Black", "Red"), lty = 1)

#creating lag variables
System1_lag1 <- Lag(System1,1)
System1_lead1 <- Lag(System1,-1)
System1_change <- (System1_lead1 - System1_lag1)/2

System2_lag1 <- Lag(System2,1)
System2_lead1 <- Lag(System2,-1)
System2_change <- (System2_lead1 - System2_lag1)/2

#change time series
plot(System1_change,type='l', xlab="Time", ylab="Change in Position", main = "Fp1 and F7 Change Scores")
lines(System2_change, col='2')
legend("topright",c("X", "Y"), col=c("Black", "Red"), lty = 1)


#finding out the delay of system 1:
#auto correlation check with lag.max of 350
tau.acf <- timeLag(System1, technique = "acf", lag.max = 350, do.plot = T)
#best choice according to the auto correlation function
print(tau.acf)

#auto correlation check with lag.max of 1000
tau.acf <- timeLag(System1, technique = "acf", lag.max = 1000, do.plot = T)
#best choice according to the auto correlation function
print(tau.acf)

#Even with 1000, 140 is still the best choice

# applying avarage mutual Information:
tau.ami <- timeLag(System1, technique = "ami", lag.max = 350, do.plot = T)
print(tau.ami)

#this gives us 26 --> since 26 is way less (this means more usable data, a delay of 26 is our choice)

#finding out the num of embeddings:
# false NN
fnn.out = false.nearest(System1, m = 15, d = tau.ami, t = 50, eps = sd(Relax_data$X2)/10 )
plot(fnn.out)

#Cao's method
emb.dim = estimateEmbeddingDim(System1, time.lag = tau.ami, max.embedding.dim = 15)
print(emb.dim)

#in this example, 6 seems to be in both methods a fine choice, thus we will choose 6 dimensions

#using these choices:
#these are chosen by us!! Thus we need to investigate the methods in order to assign these values!
embeddings1 = 6
delay1 = 26

System1.ps <- buildTakens(System1,embeddings1,delay1)
head(System1.ps, n=12)

#plot this
plot(System1.ps)
#3D plot of 3 dims
lines3D(System1.ps[,1],System1.ps[,2],System1.ps[,3], t="l", col="blue", asp=1)


#finding out the delay of system 2:
#auto correlation check with lag.max of 350
tau.acf <- timeLag(System2, technique = "acf", lag.max = 350, do.plot = T)
#best choice according to the auto correlation function
print(tau.acf)

#auto correlation check with lag.max of 1000
tau.acf <- timeLag(System2, technique = "acf", lag.max = 1000, do.plot = T)
#best choice according to the auto correlation function
print(tau.acf)

# applying avarage mutual Information:
tau.ami <- timeLag(System2, technique = "ami", lag.max = 350, do.plot = T)
print(tau.ami)

#finding out the num of embeddings:
# false NN
fnn.out = false.nearest(System2, m = 15, d = tau.ami, t = 50, eps = sd(Relax_data$X2)/10 )
plot(fnn.out)

#Cao's method
emb.dim = estimateEmbeddingDim(System2, time.lag = tau.ami, max.embedding.dim = 15)
print(emb.dim)

#using the choices:
#these are chosen by us!! Thus we need to investigate the methods in order to assign these values!
embeddings2 = 8
delay2 = 26

System2.ps <- buildTakens(System2,embeddings2,delay2)
head(System2.ps, n=12)

#plot this:
plot(System2.ps)
#3D plot of 3 dims
lines3D(System2.ps[,1],System2.ps[,2],System2.ps[,3], t="l", col="blue", asp=1)

#Now we have the values for system 1, and 2 --> Now we can apply an avarage & maximum to combine these findings and to find the ideal values:
#max value embeddings:
Max_embeddings = max(embeddings1, embeddings2)
Max_delay = max(delay1, delay2)

print(paste("Maximum value for embeddings:", Max_embeddings))
print(paste("Maximum value for delay:", Max_delay))


#avarage value embeddings:
Avg_embeddings <- mean(c(embeddings1, embeddings2))
Avg_delay <- mean(c(delay1, delay2))

#if it is a decimal number, round it up
if (Avg_embeddings %% 1 != 0) {
  Avg_embeddings <- ceiling(Avg_embeddings)
}
if (Avg_delay %% 1 != 0) {
  Avg_delay <- ceiling(Avg_delay)
}
print(paste("Avarage value for embeddings:", Avg_embeddings))
print(paste("Avarage value for delay:", Avg_delay))

