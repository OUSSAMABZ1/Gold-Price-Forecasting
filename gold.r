#install.packages('dygraphs')
#install.packages('quantmod')
#install.packages('tseries')
#install.packages('stats')
#install.packages('forecast')
library(dygraphs)
suppressMessages(library(quantmod))
library(tseries)
library(stats)
library(forecast)

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2022-12-31")

getSymbols("GC=F",
           src = "yahoo", 
           from = start_date, 
           to = end_date,
)
gold <- data.frame(
  Date = index(`GC=F`),
  Price = `GC=F`$`GC=F.Close`
)
head(gold)
plot(x = gold$Date, 
     y = gold$GC.F.Close, 
     type = "l", 
     main = "Daily Price of Gold for the Period between 01-Jan-2020 & 31-Dec-2022 in USD per OUNCE",
     xlab = "Day",
     ylab = "Price of Gold in USD per OUNCE",
     col= "red"
)
points(x = gold$Date, 
       y = gold$GC.F.Close, 
       cex =0.5
)

#Tracé avec sélecteur
close <- Cl(`GC=F`)
dygraph(close,) %>% 
  dyRangeSelector()

summary(gold)
hist(gold$GC.F.Close)
boxplot(gold$GC.F.Close)
gold[match(min(gold$GC.F.Close),gold$GC.F.Close),]

par(mfrow = c(2,3))
plot(x = gold$GC.F.Close[2: length(gold$GC.F.Close)], y = gold$GC.F.Close[1:(length(gold$GC.F.Close)-1)], xlab = "Y[t+1]", ylab="Y[t]")
plot(x = gold$GC.F.Close[3: length(gold$GC.F.Close)], y = gold$GC.F.Close[1:(length(gold$GC.F.Close)-2)], xlab = "Y[t+2]", ylab="Y[t]")
plot(x = gold$GC.F.Close[4: length(gold$GC.F.Close)], y = gold$GC.F.Close[1:(length(gold$GC.F.Close)-3)], xlab = "Y[t+3]", ylab="Y[t]")
plot(x = gold$GC.F.Close[5: length(gold$GC.F.Close)], y = gold$GC.F.Close[1:(length(gold$GC.F.Close)-4)], xlab = "Y[t+4]", ylab="Y[t]")
plot(x = gold$GC.F.Close[6: length(gold$GC.F.Close)], y = gold$GC.F.Close[1:(length(gold$GC.F.Close)-5)], xlab = "Y[t+5]", ylab="Y[t]")
plot(x = gold$GC.F.Close[7: length(gold$GC.F.Close)], y = gold$GC.F.Close[1:(length(gold$GC.F.Close)-6)], xlab = "Y[t+6]", ylab="Y[t]")

gold.training <- gold[gold$Date <= "2022-10-01",]
gold.test <- gold[gold$Date > "2022-10-01",]
data <- ts(gold.training$GC.F.Close, frequency = 12)
data <- na.omit(data)

par(mfrow = c(1,1))
decomposition <- decompose(x = data)
plot(decomposition)

gold.training.amélioré <- gold.training[gold.training$Date >= "2020-03-30",]
plot(x= gold.training.amélioré$Date, y = gold.training.amélioré$GC.F.Close, type= 'l')
data.amélioré <- ts(gold.training.amélioré$GC.F.Close, frequency = 12)
data.amélioré <- na.omit(data.amélioré)

model.amélioré2 <- auto.arima(data.amélioré)
model.amélioré2

Predictions.amélioré2 <-forecast(model.amélioré2,h=length(gold.test$Date), level=c(95,99))
Predictions.amélioré2
min <- min(Predictions.amélioré2$lower[,2])
max <- max(Predictions.amélioré2$upper[,2])
plot(x = gold.test$Date, y = gold.test$GC.F.Close, type='l', ylim = c(min, max), col="black", main = "Prévisions du modèle amélioré 2.0")
lines(x = gold.test$Date, y = Predictions.amélioré2$mean, type = 'l', col="red")
lines(x = gold.test$Date, y = Predictions.amélioré2$lower[,1], lty=2, col="green")
lines(x = gold.test$Date, y = Predictions.amélioré2$upper[,1], lty=2, col="green")
lines(x = gold.test$Date, y = Predictions.amélioré2$lower[,2], lty=2, col="blue")
lines(x = gold.test$Date, y = Predictions.amélioré2$upper[,2], lty=2, col="blue")
legend("bottomleft", legend=c("Real data", "Prediction", "Intervalle de prevision à 80%", "Intervalle de Prévision à 90%"),
       col=c("black", "red", "green", "blue"), lty=c(1,1,2,2),cex = 0.4)
grid(col = "lightgray", lty = "dotted")

error.amélioré2 <- Predictions.amélioré2$mean - gold.test$GC.F.Close
summary(error.amélioré2)
MAE.amélioré2 <- mean(abs(error.amélioré2))
MAE.amélioré2

MSE.amélioré2 <- mean(error.amélioré2^2)
MSE.amélioré2
RMSE.amélioré2 <- sqrt(MSE.amélioré2)
RMSE.amélioré2
