
list.of.packages = c("weathermetrics", "car", "nortest", "goftest", "ggplot2", "labstatR", "boot", "asympTest", "plotrix")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,require,character.only = TRUE)

source("funzioni.R")

chicago_bikes <- read.csv("chicago_bikes.csv")
View(chicago_bikes)
str(chicago_bikes)

### PRELIMINARI ####
# STUDIO LA STRUTTURA DEI DATI
which(is.na(chicago_bikes))
which(chicago_bikes$unknown == 1)

#LE COLONNE unknown, tstorm, cloudy, rain_or_snow, not_clear, clear CONTENGONO VARIABILI DICOTOMICHE E MUTUAMENTE ESCLUSIVE:
for (i in 1:length(chicago_bikes[,1])) {
  if(chicago_bikes$unknown[i]+chicago_bikes$rain_or_snow[i]+chicago_bikes$cloudy[i]+
     chicago_bikes$clear[i]+chicago_bikes$not_clear[i]+chicago_bikes$tstorms[i] != 1)
    print(i)
}
#QUINDI LE ORGANIZZO IN UN UNICA COLONNA "weather"
n0 <- length(chicago_bikes[,1])

chicago_bikes$weather <- vector(mode="character", length=n0)
for (i in 1:n0) {
  if(chicago_bikes$tstorms[i] == 1) {chicago_bikes$weather[i] <- "storm"}
  if(chicago_bikes$unknown[i] == 1) {chicago_bikes$weather[i] <- "NA"}
  if(chicago_bikes$cloudy[i] == 1) {chicago_bikes$weather[i] <- "cloudy"}
  if(chicago_bikes$rain_or_snow[i] == 1) {chicago_bikes$weather[i] <- "rain/snow"}
  if(chicago_bikes$not_clear[i] == 1) {chicago_bikes$weather[i] <- "not clear"}
  if(chicago_bikes$clear[i] == 1) {chicago_bikes$weather[i] <- "clear"}
  
}

# RIEMPIO I DATI CON ANCHE I NOLEGGI 0
which(chicago_bikes$rentals == 0)

for(y in unique(chicago_bikes$year)) {
  for(m in unique(subset(chicago_bikes, year == y)$month)) {
    for(w in unique(subset(chicago_bikes, year == y & month == m)$week)) {
      for(d in unique(subset(chicago_bikes, year == y & month == m & week == w)$day )) {
        for(h in c(0:23)) {
          if(length(which( chicago_bikes$year == y &
                           chicago_bikes$month == m &
                           chicago_bikes$week == w &
                           chicago_bikes$day == d &
                           chicago_bikes$hour == h )) == 0 ) {
            new <- data.frame(y, m, w, d, h, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, NA)
            names(new) <- names(chicago_bikes)
            chicago_bikes <- rbind(chicago_bikes, new)
          }
        }
      }
    }
  }
}

n <- length(chicago_bikes[,1])
# FATTORIZZAZIONE
chicago_bikes$weather <- factor(chicago_bikes$weather, exclude = "NA")
chicago_bikes$year <- factor(chicago_bikes$year, ordered = T, levels = c(2014:2017))
chicago_bikes$month <- factor(chicago_bikes$month, ordered = T, levels = c(1:12))
chicago_bikes$day <- factor(chicago_bikes$day, ordered = T)
chicago_bikes$week <- factor(chicago_bikes$week, ordered = T)
chicago_bikes$hour <- factor(chicago_bikes$hour, ordered = T)
chicago_bikes$is_weekend <- factor(chicago_bikes$is_weekend)
chicago_bikes$tstorms <- factor(chicago_bikes$tstorms)
chicago_bikes$unknown <- factor(chicago_bikes$unknown)
chicago_bikes$cloudy <- factor(chicago_bikes$cloudy)
chicago_bikes$rain_or_snow <- factor(chicago_bikes$rain_or_snow)
chicago_bikes$not_clear <- factor(chicago_bikes$not_clear)
chicago_bikes$clear <- factor(chicago_bikes$clear)

# temperatura mediana uguale a quella media, superflua.
which(chicago_bikes$mean_temperature != chicago_bikes$median_temperature)
chicago_bikes <- chicago_bikes[,-8]

chicago_bikes$mean_temperature <- fahrenheit.to.celsius(chicago_bikes$mean_temperature)

chicago_bikes <- chicago_bikes[order(chicago_bikes$year, chicago_bikes$month,
                                     chicago_bikes$week, chicago_bikes$day,
                                     chicago_bikes$hour),]



### ANALISI 1 ####
# INFERENZA SULLA DISTRIBUZIONE DEGLI NOLEGGI
# densit empirica vs. densità exp
hist(chicago_bikes$rentals, freq=F, xlim = c(0,2000), ylim = c(0, 0.004),
     breaks = seq(0, 2500, 100), col = "lightblue", xlab = "Noleggi",
     ylab = "Densità", main = "Istogramma delle frequenze")
lines(density(chicago_bikes$rentals, from = 0, to = 2000), lwd = 2, col = "red") 
curve(dexp(x, rate = 1/mean(chicago_bikes$rentals)),
      col="green", lty=4, lwd=2, add=TRUE, yaxt="n")
legend("topright", legend = c("Densità empirica", "Densità esp"), 
      lty = 1, col = c("red", "green"))
curve(dgeom(x, prob = 1/n*mean(chicago_bikes$rentals)),
      col="darkblue", lty="dotted", lwd=2, add=TRUE, yaxt="n")

# ecdf vs. cdf exp
plot(ecdf(chicago_bikes$rentals),pch = "",
     xlim=quantile(chicago_bikes$rentals,probs = c(0,.99)))
curve(pexp(x, rate = 1/mean(chicago_bikes$rentals)),
      col="green", lty="dotted", lwd=2, add=TRUE, yaxt="n")

# tests
ad.test(chicago_bikes$rentals, null = "pexp", rate = 1/mean(chicago_bikes$rentals))
ks.test(x = abs(chicago_bikes$rentals + runif(n = n, min = -.5, max = .5)),
        y = "pexp", rate = 1/mean(chicago_bikes$rentals))


# selezione dati "più esponenziali"
getmode(subset(chicago_bikes, rentals < 400)$year)
getmode(subset(chicago_bikes, rentals < 400 & year == 2014)$month)

# most_exp_data <- subset(chicago_bikes, mean_temperature<8 & mean_temperature >7.0)
most_exp_data <- subset(chicago_bikes, month == 3 & year == 2014)
n2 <- length(most_exp_data[,1])
hist(most_exp_data$rentals, freq=F, xlim = c(0,2000), ylim = c(0, 0.01), 
     breaks = seq(0, 2500, 100), col = "orange", xlab = "Noleggi",
     ylab = "Densità", main = "Istogramma delle frequenze - selez. dati")
lines(density(most_exp_data$rentals, from = 0, to = 2000), lwd = 2, col = "red") 
curve(dexp(x, rate = 1/mean(most_exp_data$rentals)),
      col="green", lty="dotted", lwd=2, add=TRUE, yaxt="n")
legend("topright", legend = c("Densità empirica", "Densità esp"), 
       lty = 1, col = c("red", "green"))
curve(dgeom(x, prob = 1/n2*mean(most_exp_data$rentals)),
      col="darkblue", lty="dotted", lwd=2, add=TRUE, yaxt="n")

plot(ecdf(most_exp_data$rentals),pch = "",
     xlim=quantile(most_exp_data$rentals,probs = c(0,.99)))
curve(pexp(x, rate = 1/mean(most_exp_data$rentals)),
      col="green", lty="dotted", lwd=2, add=TRUE, yaxt="n")

ad.test(most_exp_data$rentals, null = "pexp", rate = 1/mean(most_exp_data$rentals))
ks.test(x = abs(most_exp_data$rentals + runif(n = n2, min = -.5, max = .5)),
        y = "pexp", rate = 1/mean(most_exp_data$rentals))

# C'è DIFFERENZA TRA DISTRIBUZIONI NEL WEEKEND E NON?
weekend <- subset(chicago_bikes, is_weekend == 1)
wk <- length(weekend[,1])
nnweekend <- subset(chicago_bikes, is_weekend == 0)
nwk <- length(nnweekend[,1])

ks.test(x = abs(weekend$rentals + runif(n = wk, min = -.5, max = .5)),
                y = abs(nnweekend$rentals + runif(n = nwk, min = -.5, max = .5)))

# NO!

### ANALISI 2 ####
# STUDIO DELLE DISTRIBUZIONI, DELLE MEDIE E DELLE VARIANZE DEI NOLEGGI DIVISI PER ORA
#hour_distr <- rep(0, 24)
hour_distr2 <- rep(0, 24)
hour_distr_wk <- rep(0, 24)
hour_distr_nwk <- rep(0, 24)
tot_rentals <- sum(chicago_bikes$rentals)

for(i in 0:23) {
  #hour_distr[i+1] <- sum(subset(chicago_bikes, hour == i)$rentals)/tot_rentals
  hour_distr2[i+1] <- mean(subset(chicago_bikes, hour == i)$rentals)
  hour_distr_wk[i+1] <- mean(subset(chicago_bikes, hour == i & is_weekend == 1)$rentals)
  hour_distr_nwk[i+1] <- mean(subset(chicago_bikes, hour == i & is_weekend == 0)$rentals)
}

# Osservo che ci sono dei picchi (nelle medie degli affitti/idem negli affitti tot) alle 8 e alle 17
barplot(height = hour_distr2, names.arg = c(0:23),las = 1, xlab = "Ora", 
        ylab = "Noleggi medi", axis.lty = 1, col = "darkblue", main = "Noleggi medi per ora")

# Vedo anche che non c'è evidente differenza tra weekend e non
barplot(height = hour_distr_wk)
barplot(height = hour_distr_nwk)

# studio la media e la varianza nelle ore di picco: aggiungo la colonna delle ore di picco
chicago_bikes$rush_hour <- rep(0, n)
chicago_bikes$rush_hour[which(chicago_bikes$hour == 8 | chicago_bikes$hour == 17)]  <- 1
chicago_bikes$rush_hour <- factor(chicago_bikes$rush_hour)#, levels = c(0,1), labels = c("not rush hour", "rush hour"))

# uso un test asintotico, perché i dati (rentals ~ rush_hour) non sono distribuiti normalmente ma sono numerosi
mean(subset(chicago_bikes, rush_hour == 1)$rentals)
mean(subset(chicago_bikes, rush_hour == 0)$rentals)
asymp.test(rentals ~ rush_hour, data = chicago_bikes, parameter = "mean",
           alternative = "less")

var(subset(chicago_bikes, rush_hour ==1)$rentals)
var(subset(chicago_bikes, rush_hour ==0)$rentals)
asymp.test(rentals ~ rush_hour, data = chicago_bikes, parameter = "dVar",
           alternative = "two.sided")

hours <- c(0:23)
for(i in subset(hours, hours != 8 & hours != 17)) {
  for(j in c(8, 17)) {
    print(asymp.test(subset(chicago_bikes, hour == j)$rentals, 
               subset(chicago_bikes, hour == i)$rentals,
               parameter = "dVar", alternative = "greater"))
  }
}



# calcolo 24 intervalli di confidenza per il numero medio di noleggi di ciascun ora
hourly_conf_int1 <- matrix(vector("double", 48), ncol = 2, byrow = F)
for(h in 0:23) {
  hourly_conf_int1[h+1,] <- asymp.test(subset(chicago_bikes, hour == h)$rentals, parameter = "mean",
                                      alternative = "two.sided", conf.level = 0.95)$conf.int
}


# costruisco anche un IC per la media totale e disegno il tutto
conf_int_tot <- asymp.test(chicago_bikes$rentals, parameter = "mean", alternative = "two.sided",
                           conf.level = 0.95)$conf.int

hour_distr_datafr <- data.frame(Ore = c(0:23), Noleggi = hour_distr2)
ggplot(hour_distr_datafr) +
  geom_bar( aes(x=Ore, y=Noleggi), stat="identity", fill="skyblue", alpha=0.7) +
  geom_pointrange( aes(x=Ore, y=Noleggi, ymin=hourly_conf_int1[,1], ymax=hourly_conf_int1[,2]),
                   colour="orange", alpha=1.0, size=0.3) +
  geom_errorbar( aes(x=Ore, ymin=hourly_conf_int1[,1], ymax=hourly_conf_int1[,2]),
                 width=0.4, colour="orange", alpha=0.9, size=1.0) +
  #discrete_scale(aes(x=Ore, y=Noleggi), "Ora", palette = function(x) c(0:x), limits=paste(c(0:23))) +
  scale_x_discrete("Ora", limits=paste(c(1:23))) +
  geom_errorbar( aes(x=24, ymin = conf_int_tot[1], ymax = conf_int_tot[2]),
                width=0.4, colour="red", alpha=0.9, size=1.0) +
  xlab("Ora") +
  ylab("Noleggi medi") +
  ggtitle("Noleggi medi per ora")


# Verifico che il test asintotico asymp.test usa la statistica AN(0,1), utilizzando la funzione
# per il calcolo dell'IC asintotico da noi creata
# hourly_conf_int2 <- matrix(vector("double", 48), ncol = 2, byrow = F)
# for(h in 0:23) {
#      hourly_conf_int2[h+1,] <- norm.interval(subset(chicago_bikes, hour == h)$rentals)
# }

for(i in 0:23) {
  ora <- subset(chicago_bikes, hour == i)
  hist(ora$rentals, main = paste("Ora ", i), freq = F)
  lines(density(ora$rentals), lwd = 2, col = "red")
}

asymp.test(subset(chicago_bikes, hour == 12)$rentals, 
            subset(chicago_bikes, hour == 13)$rentals, parameter = "mean")



### ANALISI 3 ####


hist(chicago_bikes$mean_temperature, freq = F)
lines(density(chicago_bikes$mean_temperature), lwd = 2, col = "red")
boxplot(chicago_bikes$mean_temperature)


# MODELLO LINEARE
result <- lm(rentals ~ mean_temperature, data = chicago_bikes)
summary(result)
anova(result)

# MODELLO LINEARE CON VARIABILI DICOTOMICHE
step(result, scope = (rentals ~ mean_temperature + tstorms + cloudy + clear + 
                        not_clear + rain_or_snow + rush_hour), direction = 'forward')

result2 <- lm(formula = rentals ~ mean_temperature + tstorms + cloudy + clear + 
                not_clear + rush_hour, data = chicago_bikes)
summary(result2)

chicago_bikes <- subset(chicago_bikes, !is.na(tstorms))
Storms <- which(chicago_bikes$tstorms == 1)
Cloudy <- which(chicago_bikes$cloudy == 1)
Clear <- which(chicago_bikes$clear == 1)
NotClear <- which(chicago_bikes$not_clear == 1)
RushHour <- which(chicago_bikes$rush_hour == 1)

Ycapp <- result2$fitted.values
plot(c(-26, 32),c(42,1100), type = 'n', xlab = "Temperatura", ylab = "Noleggi",
     main = "Rette di regressione")
# plot(c(-26, 32),c(0,20), type = 'n', xlab = "Temperatura", ylab = "Noleggi",
#      main = "Rette di regressione")
grid()

min_storm <- min(chicago_bikes[Storms,]$mean_temperature, na.rm = T)
max_storm <- max(chicago_bikes[Storms,]$mean_temperature, na.rm = T)
segments(x0 = min_storm, x1 = max_storm,
         y0 = result2$coefficients[1]+result2$coefficients[3]+min_storm*result2$coefficients[2],
         y1 = result2$coefficients[1]+result2$coefficients[3]+max_storm*result2$coefficients[2],
         col = "green", lwd = 2)
#abline(a=result2$coefficients[1]+result2$coefficients[3], b = result2$coefficients[2], col = "green", lwd = 2)

min_cloudy <- min(chicago_bikes[Cloudy,]$mean_temperature, na.rm = T)
max_cloudy <- max(chicago_bikes[Cloudy,]$mean_temperature, na.rm = T)
segments(x0 = min_cloudy, x1 = max_cloudy,
         y0 = result2$coefficients[1]+result2$coefficients[4]+min_cloudy*result2$coefficients[2],
         y1 = result2$coefficients[1]+result2$coefficients[4]+max_cloudy*result2$coefficients[2],
         col = "blue", lwd = 2)
#abline(a=result2$coefficients[1]+result2$coefficients[4], b = result2$coefficients[2], col = "blue", lwd = 2)

min_clear <- min(chicago_bikes[Clear,]$mean_temperature, na.rm = T)
max_clear <- max(chicago_bikes[Clear,]$mean_temperature, na.rm = T)
segments(x0 = min_clear, x1 = max_clear,
         y0 = result2$coefficients[1]+result2$coefficients[5]+min_clear*result2$coefficients[2],
         y1 = result2$coefficients[1]+result2$coefficients[5]+max_clear*result2$coefficients[2],
         col = "red", lwd = 2)
#abline(a=result2$coefficients[1]+result2$coefficients[5], b = result2$coefficients[2], col = "red", lwd = 2)

min_notclear <- min(chicago_bikes[NotClear,]$mean_temperature, na.rm = T)
max_notclear <- max(chicago_bikes[NotClear,]$mean_temperature, na.rm = T)
segments(x0 = min_notclear, x1 = max_notclear,
         y0 = result2$coefficients[1]+result2$coefficients[6]+min_notclear*result2$coefficients[2],
         y1 = result2$coefficients[1]+result2$coefficients[6]+max_notclear*result2$coefficients[2],
         col = "orange", lwd = 2)
#abline(a=result2$coefficients[1]+result2$coefficients[6], b = result2$coefficients[2], col = "orange", lwd = 2)

min_rushhour <- min(chicago_bikes[RushHour,]$mean_temperature, na.rm = T)
max_rushhour <- max(chicago_bikes[RushHour,]$mean_temperature, na.rm = T)
segments(x0 = min_rushhour, x1 = max_rushhour,
         y0 = result2$coefficients[1]+result2$coefficients[7]+min_rushhour*result2$coefficients[2],
         y1 = result2$coefficients[1]+result2$coefficients[7]+max_rushhour*result2$coefficients[2],
         col = "purple", lwd = 2)
#abline(a=result2$coefficients[1]+result2$coefficients[7], b = result2$coefficients[2], col = "purple", lwd = 2)

legend("topleft", legend = c("Temporale", "Nuvoloso", "Sereno", "Non sereno", "Ore di punta"),
       col = c("green", "blue", "red", "orange", "purple"), lty = 1, lwd = 2,
       title = "Dummies", bg = "white")

qqPlot(result2, distribution ="norm",main="QQ Plot")


# ripeto tutto trasformando i dati

# MODELLO LINEARE
result <- lm(log(rentals) ~ mean_temperature, data = chicago_bikes)
summary(result)
anova(result)


# MODELLO LINEARE CON VARIABILI DICOTOMICHE
step(result, scope = (log(rentals) ~ mean_temperature + tstorms + cloudy + clear + 
                        not_clear + rain_or_snow + rush_hour), direction = 'forward')

result2 <- lm(formula = log(rentals) ~ mean_temperature + tstorms + cloudy + clear + 
                not_clear + rush_hour, data = chicago_bikes)
summary(result2)


qqPlot(result2, distribution ="norm",main="QQ Plot - con trasformazione")

