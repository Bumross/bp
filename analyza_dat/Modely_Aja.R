# knihovny
library(lubridate)
library(car)
library(dplyr)

library(TTR)
library(zoo)
library(forecast)
library(tseries)
library(dynlm)
library(TSA)

# prehled dat
names(data)
head(data)
summary(data)

# nahled casovych rad
pm <- ts(data$data_pm100)
plot(pm)

# abnormalni vykyvy se tykaji 4 dni: 5.-8.7.2024
data$cas[data$data_pm100 > 100]

no2 <- ts(data$data_no2)
plot(no2)
temp <- ts(data$data_temp1)
plot(temp)
hum <- ts(data$data_hum1)
plot(hum)
pres <- ts(data$data_pressure)
plot(pres)
wind1 <- ts(data$data_windSpeed)
plot(wind1)
wind2 <- ts(data$data_windImpact)
plot(wind2)
vol <- ts(data$data_volumeMm)
plot(vol)
auta <- ts(data$valid_speed_count)
plot(auta)
which.max(auta)
data$cas[which.max(auta)]

##########################
# Model pro auta - jen trend, sezonnost a veco, co jsou predikovat
auta.rm <- rollmean(auta, 169)
lines(auta.rm, col = 2)
auta.rm2 <- rollmean(auta, 168*2+1)
lines(auta.rm2, col = 3)
  # zobrazeni trendu

# nezavisle promenne
den <- data$den_v_tydnu
svatek <- data$statni_svatky
hod <- hour(data$cas)
prazd <- data$letni_prazdniny

# nahled, co vsechno muze mit vliv
lm1 <- lm(auta ~ as.factor(hod) + as.factor(den) + as.factor(svatek) + as.factor(prazd))
Anova(lm1)
  # klasicky linearni model

oldpar <- par(mfrow = c(2,2))
plot(lm1)
par(oldpar)
  # uplne super to neni - mozna by stalo za to uvazovat logaritmus

# a jak vypadaji residua  
res <- ts(residuals(lm1))
plot(res)
  # jsou videt Vanoce, Velikonoce a podzimni prazdniny (asi)

var(res)
var(diff(res, 1))
var(diff(diff(res,1), 1))
var(diff(res, 24))
var(diff(res, 7*24))
var(diff(diff(res,7*24), 1))
  # neni treba uvazovat sezonni diference, pouze klasicke a pouze jednu diferenci
  #   (ma minimalni rozptyl)

acf(res)
  # je videt sezonni zavislost - denni
acf(res, lag.max = 200)
  # je videt sezonni zavislost - i tydeni

auta2 <- ts(data$valid_speed_count, frequency = 24)
  # osetreme denni sezonnost :(
  # ale treba by s tydenni sezonnosti vypadalo lip
reg <- as.matrix(cbind(as.factor(hod), as.factor(den), as.factor(svatek), as.factor(prazd))) 
  # takhle to nejde, potrebuju design matrix :(
  # bud se da ziskat z modelu, nebo je treba pripravit rucne

# a ted arima
ar.a1 <- auto.arima(auta2, xreg = reg, d = 1, D = 0)
  # DODELAT se spravnou reg matici

# a nebo aspon na residua
res.t <- ts(res, frequency = 24)
res.mod1 <- auto.arima(res.t, d = 1, D = 0)
summary(res.mod1)
  # podle me max ma3 staci

# dynamicky linearni model
lm2 <- dynlm(auta ~ as.factor(hod) + as.factor(den) + as.factor(svatek) + as.factor(prazd) +
               L(auta, 1) + L(auta, 24) + L(auta, 168))
Anova(lm2)
  # letni prazdniny nejsou vyznamne
res2 <- residuals(lm2)
res2.t <- ts(res2)
plot(res2.t)
  # letni prazdniny ovlivnuji variabilitu
acf(res2.t)
acf(res2.t, lag.max = 200)
  # to je pekne :)
  # asi vlozit do ARIMY tydenni sezonnost spis nez denni
pacf(res2.t)

###############################
# Model pro teplotu

plot(temp)
temp.rm <- rollmean(temp, 99)
lines(temp.rm, col = 2)
temp.rm2 <- rollmean(temp, 169)
lines(temp.rm2, col = 3)
temp.rm3 <- rollmean(temp, 301)
lines(temp.rm3, col = 4)

# nezavisle promenne
den <- as.numeric(date(data$cas) - date("2024-01-01"))
  summary(den)
den <- ifelse(den > 365, den - 366, den)
  # potrebuji den v roce
hod <- hour(data$cas)
month <- data$month

# kvadraticka zavislost na dni
lm.t1 <- lm(temp ~ den + I(den^2))
summary(lm.t1)
  # klasicky linearni model
plot(temp)
lines(fitted(lm.t1), col = 2)
  # to je pekne

# a ted to tam chce pridat i denni sezonnost, tj. zavislost na hodine,
#   ale ta je v kazdou rocni dobu jina
#   zkusila jsem modelovat zvlast po mesicich
lm.t2 <- lm(temp ~ den + I(den^2) + as.factor(hod)*as.factor(month))
summary(lm.t2)
Anova(lm.t2)
plot(temp)
lines(fitted(lm.t2), col = 2)
  # vim, ze se Vam to nelibi, ale smysl to dava ;)

oldpar <- par(mfrow = c(2,2))
plot(lm.t2)
par(oldpar)
  # predpoklady ok

res <- ts(residuals(lm.t2))
plot(res)
  # to je pekne :)
acf(res)
  # to neni pekne :(

# chci periodogram, ale ten nechce missing value, tak je odstranuju :D
#   urcite vymyslite i inteligentnejsi zpusob, nez "rucni"
#   nahrazovani na 20 radku
# chci nahradit prumerem sousednich hodnot, nebo alespon nejblizsi hodnotou
sum(is.na(temp))
chybi <- data[is.na(data$data_temp1), 1:2]
temp.per <- temp[-c(1:61)]
temp.pom <- cbind(c(temp.per[-c(1,2)],NA, NA), c(temp.per[-1],NA), temp.per, c(NA,temp.per[-length(temp.per)]), c(NA,NA, temp.per[-c(length(temp.per)-1,length(temp.per))]))
temp.pom[,3] <- ifelse(is.na(temp.pom[,3]), (temp.pom[,2] + temp.pom[,4])/2, temp.pom[,3])
temp.pom[,3] <- ifelse(is.na(temp.pom[,3]), temp.pom[,2], temp.pom[,3])
temp.pom[,3] <- ifelse(is.na(temp.pom[,3]), temp.pom[,4], temp.pom[,3])
temp.pom[,3] <- ifelse(is.na(temp.pom[,3]), (temp.pom[,1] + temp.pom[,5])/2, temp.pom[,3])
temp.pom[,3] <- ifelse(is.na(temp.pom[,3]), temp.pom[,1], temp.pom[,3])
temp.pom[,3] <- ifelse(is.na(temp.pom[,3]), temp.pom[,5], temp.pom[,3])
sum(is.na(temp.pom[,3]))
temp.pom[is.na(temp.pom[,3]),]
kde <- which(is.na(temp.pom[,3]))
temp.pom[c((kde-2):(kde + 2)),]
temp.pom[,3] <- ifelse(is.na(temp.pom[,3]), (8.95+12.88)/2, temp.pom[,3])
temp.per <- temp.pom[,3]

#### nahrazeni tohoto kodu pomoci funkce 
temp.per <- na.approx(temp, x = index, rule = 2)
sum(is.na(temp.per))

###

# periodogram - hleda periody :)
periodogram(temp.per)$spec[1:20]
  # tak mam periodogram a nic moc na nem neni :(
(per1 <- 1/periodogram(temp.per)$freq[1])
periodogram(temp.per)$freq[80:100]
(m.spec <- max(periodogram(temp.per)$spec[-c(1:100)]))
(per2 <- 1/periodogram(temp.per)$freq[periodogram(temp.per)$spec == m.spec])
  # je videt jen denni, tj. 24 hodinova perioda

var(res)
var(diff(res, 1))
var(diff(diff(res,1), 1))
var(diff(res, 24))
  # vypada to, ze staci jedna klasicka diference
acf(diff(res, 1))
pacf(diff(res, 1))
  # to uz vypada smysluplne

### ted to chce bud vylepsit model
### nebo aspon vlozit design matrix do auto.arimy s prvnima obyc diferencema
#   bez sezonnich diferenci
temp2 <- ts(data$valid_temp1, frequency = 24)
  # nastaveni denni sezonnosti
reg <- as.matrix() 
  # potrebuju design matrix !

ar.t1 <- auto.arima(temp2, xreg = reg, d = 1, D = 0)
summary(ar.t1)
Anova(ar.t1)

# dynamicky linearni model
den2 <- den*den
lm.t3 <- dynlm(temp ~ den + den2 + as.factor(hod)*as.factor(month) +
               L(temp, 1))
Anova(lm.t3)

# residua
res3 <- residuals(lm.t3)
res3.t <- ts(res3)
plot(res3.t)
  # to jde :)
acf(res3.t)
  # je videt perioda, ale jen trochu
pacf(res3.t)

# pridani zavislosti o nez zpet
lm.t4 <- dynlm(temp ~ den + den2 + as.factor(hod)*as.factor(month) +
                 L(temp, 1) + L(temp, 24))
Anova(lm.t4)

# residua
res4 <- residuals(lm.t4)
res4.t <- ts(res4)
plot(res4.t)
  # to jde :)
acf(res4.t)
  # pridat do modelu hodnotu z minuleho dne moc nepomohlo
var(res3)
var(res4)
  # ale uplne chyba to asi neni

###################
## Model pro prachove castice
plot(pm)

# vynechani nesmyslu v cervenci
kde <- which(data$data_pm100 > 100)
pm2 <- pm
pm2[4400:4700] <- ifelse(pm[4400:4700] > 25, NA, pm[4400:4700])
plot(pm2)
  # takhle to vypada lip

# dlouhodoby trend
pm.rm <- rollmean(pm2, 99)
lines(pm.rm, col = 2)
pm.rm2 <- rollmean(pm2, 169)
lines(pm.rm2, col = 3)
pm.rm3 <- rollmean(pm2, 301)
lines(pm.rm3, col = 4)

# nezavisle promenne
den <- data$den_v_tydnu
svatek <- data$statni_svatky
hod <- hour(data$cas)
prazd <- data$letni_prazdniny
topeni <- data$topna_sezona
month <- month(data$cas)

# linearni model
lm.pm1 <- lm(pm2 ~ as.factor(den) + svatek + as.factor(hod) + 
               as.factor(month))
summary(lm.pm1)
Anova(lm.pm1)
plot(pm2)
lines(fitted(lm.pm1), col = 2)

oldpar <- par(mfrow = c(2,2))
plot(lm.pm1)
par(oldpar)
  # spatne - predpoklady nejsou splneny

# transformace zavisle promenne
sq.pm <- sqrt(pm2)
plot(sq.pm)
ln.pm <- log(pm2)
plot(ln.pm)
  # logaritmus je lepsi
lm.pm2 <- lm(ln.pm ~ as.factor(den) + svatek + as.factor(hod) + 
               as.factor(month))
Anova(lm.pm2)
plot(ln.pm)
lines(fitted(lm.pm2), col = 2)
  # to neni dobry model :(

plot(pm2)
lines(exp(fitted(lm.pm2)), col = 2)

# Ted hledam neco, co mi udela alespon neco trochu pouzitelneho,
#   protoze v datech je videt jen zimni topeni, ale pres
#   topnou sezonnu se to modelovat neda :(
# den v roce
den <- as.numeric(date(data$cas) - date("2024-01-01"))
den <- ifelse(den > 365, den - 366, den)
topeni2 <- case_match(month, c(1, 2, 3, 11, 12) ~ 1, c(4, 5, 6, 7, 8, 9) ~ 3,
                  c(10) ~ 4)

# na cem by to tak mohlo zaviset?
plot(ln.pm ~ den, pch = 19, cex = 0.1)
boxplot(ln.pm ~ as.factor(hod))
boxplot(ln.pm ~ as.factor(month))
boxplot(ln.pm ~ topeni)
boxplot(ln.pm ~ prazd)
  # na topeni

# tak co vylepsena topna sezona
lm.pm3 <- lm(ln.pm ~ den*as.factor(topeni2) + I(den^2)*as.factor(topeni2))
plot(ln.pm)
lines(fitted(lm.pm3), col = 2)
summary(lm.pm3)
  # zas nic :(

plot(ln.pm[month > 2 & month < 11], type = "l")
plot(ln.pm[month < 3], type = "l")
plot(ln.pm[month > 10], type = "l")
plot(ln.pm[month < 3 | month > 10], type = "l")
  # takhle oddelene by to mozna slo
  # treti az desaty mesic parabola
  #  a zbytek konstanta

# tak to takhle zkusim, ale Vy to budete umet lip :)
dat.leto <- data.frame(cbind(den, ln.pm)[month > 2 & month < 11,])
plot(dat.leto$ln.pm, type = "l")
lm.pm4 <- lm(ln.pm ~ den + I(den^2), data = dat.leto)
summary(lm.pm4)
lines(fitted(lm.pm4), col = 2)
fitted(lm.pm4)[c(1, dim(dat.leto)[1])]
max(fitted(lm.pm4))
(fit.mn <- mean(ln.pm[month < 3 | month > 10], na.rm = T))
fit.pm <- ifelse(month < 3 | month > 10, fit.mn, NA)
pr <- predict(lm.pm4, newdata = data.frame("den" = dat.leto$den))
pr <- ifelse(pr > fit.mn, fit.mn, pr)
fit.pm[month > 2 & month < 11] <- pr
  # je to odporne naprogramovano, ale mam z toho to, co chci :)

plot(ln.pm)
lines(fit.pm, col = 2)
  # asi nejlepsi, co umim
plot(pm2)
lines(exp(fit.pm), col = 2)

# a ted arima na residuich z logaritmu
ln.res <- ln.pm - fit.pm
plot(ln.res)
acf(ln.res, na.action = na.pass)
  # diference nutne, periodu prilis nevidet

var(ln.res, na.rm = T)
var(diff(ln.res,1), na.rm = T)
var(diff(diff(ln.res,1),1), na.rm = T)
var(diff(ln.res,24), na.rm = T)
acf(diff(ln.res,1), na.action = na.pass)
pacf(diff(ln.res,1), na.action = na.pass)
acf(diff(ln.res,1), lag.max = 100, na.action = na.pass)
  # potreba 1. diference a sezona

ln.rest <- ts(ln.res, frequency = 24)
ar.pm1 <- auto.arima(ln.rest, d = 1, D = 0, max.p = 3, max.q = 3)
summary(ar.pm1)
plot(ln.rest)
lines(fitted(ar.pm1), col = 2)
  # to vypada slusne :)

##########################
# A ted by to chtelo zavislost pm na teplote a autech