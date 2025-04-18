install.packages("readxl")
library(readxl)
install.packages("tseries")
library(tseries)
#######
production_industrielle <- read_excel(file.choose(), skip = 10)
production_industrielle$observation_date <- as.Date(production_industrielle$observation_date)
head(production_industrielle)
str(production_industrielle)
summary(production_industrielle)
class(production_industrielle)


# Créer une série temporelle avec fréquence mensuelle
production_industrielle_ts <- ts(production_industrielle$IPB53122N,
                      start = c(2009, 1),  # Début en janvier 2004
                      end = c(2024, 3),    # Fin en mars 2024
                      frequency = 12)      # Fréquence mensuelle

# Afficher les premières lignes de la série multivariée
head(production_industrielle_ts)


# Vérifiez la série temporelle
plot(production_industrielle_ts)
class(production_industrielle_ts)

# Sélectionner une sous-série de 2010 à 2012
zoom_production <- window(production_industrielle_ts, start=c(2010, 1), end=c(2013, 12))

# Tracer la sous-série
plot(zoom_production,  ylab="Production", xlab="Année")


# Résumé statistique
summary(production_industrielle_ts)

# Moyenne
mean(production_industrielle_ts)

# Variance
var(production_industrielle_ts)

# Écart-type
sd(production_industrielle_ts)

# Décomposition de la série temporelle
decomp <- decompose(production_industrielle_ts)

# Afficher les composants de la décomposition
decomp

# Afficher la tendance
decomp$trend

# Afficher la saisonnalité
decomp$seasonal

decomp$random

plot(decomp)

lag.plot(production_industrielle_ts , lags=36)

# Calcul de l'autocorrélation (ACF) pour observer la saisonnalité
acf(production_industrielle_ts)
acf(production_industrielle_ts , plot=F )


# Test de la stationnarité avec Augmented Dickey-Fuller Test (ADF)


adf.test(production_industrielle_ts)  # Test de Dickey-

# Appliquer une différenciation d'ordre 1
production_industrielle_diff <- diff(production_industrielle_ts)

# Afficher la série différenciée
plot(production_industrielle_diff, main = "Série différenciée d'ordre 1")
adf.test(production_industrielle_diff)





########## data 2 
data_job_openings <- read_excel(file.choose(), skip = 10)
data_job_openings$observation_date <- as.Date(data_job_openings$observation_date)
head(data_job_openings)
str(data_job_openings)
summary(data_job_openings)
class(data_job_openings)



# Créer une série temporelle avec fréquence mensuelle
job_openings_ts <- ts(data_job_openings$JTU2300JOL,
                      start = c(2004, 1),  # Début en janvier 2004
                      end = c(2024, 3),    # Fin en mars 2024
                      frequency = 12)      # Fréquence mensuelle

# Vérifiez la série temporelle
class(job_openings_ts)


# Afficher les premières lignes de la série multivariée
head(job_openings_ts)


# Vérifiez la série temporelle
plot(job_openings_ts)

class(job_openings_ts)

# Sélectionner une sous-série de 2010 à 2012
zoom_production_job <- window(job_openings_ts, start=c(2010, 1), end=c(2013, 12))

# Tracer la sous-série
plot(zoom_production_job,  ylab="jobs", xlab="Année")


# Résumé statistique
summary(job_openings_ts)

# Moyenne
mean(job_openings_ts)

# Variance
var(job_openings_ts)

# Écart-type
sd(job_openings_ts)

# Décomposition de la série temporelle
decomp_job <- decompose(job_openings_ts )


# Afficher les composants de la décomposition
decomp_job


# Afficher la tendance
decomp_job$trend

# Afficher la saisonnalité
decomp_job$seasonal

decomp_job$random

plot(decomp_job)

lag.plot(job_openings_ts , lags=36)

# Calcul de l'autocorrélation (ACF) pour observer la saisonnalité
acf(job_openings_ts)
acf(job_openings_ts , plot=F )


# Test de la stationnarité avec Augmented Dickey-Fuller Test (ADF)


adf.test(job_openings_ts)  # Test de Dickey-

# Appliquer une différenciation d'ordre 1
job_openings_ts_diff <- diff(job_openings_ts)

# Afficher la série différenciée
plot(job_openings_ts_diff, main = "Série différenciée d'ordre 1")
adf.test(job_openings_ts_diff)



###auto-correlation croiser 
# Calcul de l'autocorrélation croisée
ccf(job_openings_ts, production_industrielle_ts, lag.max = 48, main = "Autocorrélation Croisée entre Serie1 et Serie2")

ccf(job_openings_ts, production_industrielle_ts, lag.max = 48, plot=F)


####phase 2###########
install.packages("forecast")
library(forecast)
####serie 1
# Série différenciée (car non stationnaire)
production_diff <- diff(production_industrielle_ts)

# Ajustement automatique ARIMA
model_prod <- auto.arima(production_industrielle_ts, seasonal = TRUE)
summary(model_prod)

# Prévision sur 12 mois
forecast_prod <- forecast(model_prod, h = 12)
plot(forecast_prod)
forecast_prod


# Holt-Winters (saisonnalité additive)
hw_model <- HoltWinters(production_industrielle_ts)
plot(hw_model)
hw_model
forecast_hw <- forecast(hw_model, h = 12)
plot(forecast_hw)


# Comparaison des résidus
accuracy(model_prod)
accuracy(forecast_hw)


####serie 2: 
# Différenciation
job_diff <- diff(job_openings_ts)

# Modèle ARIMA automatique
model_job <- auto.arima(job_openings_ts, seasonal = TRUE)
summary(model_job)

# Prévision
forecast_job <- forecast(model_job, h = 12)
plot(forecast_job)

# Holt-Winters
hw_job <- HoltWinters(job_openings_ts)
plot(hw_job)
forecast_hw_job <- forecast(hw_job, h = 12)
plot(forecast_hw_job)

# Comparaison
accuracy(model_job)
accuracy(forecast_hw_job)



########phase 3 ##########
#### Résidus du modèle ARIMA pour la série "production_industrielle" : serie 1:
resid_prod <- residuals(model_prod)

# Test de Ljung-Box pour la stationnarité des résidus (si les résidus sont des bruits blancs)
Box.test(resid_prod, lag = 20, type = "Ljung-Box")

# Histogramme des résidus
hist(resid_prod, main = "Histogramme des Résidus - ARIMA", xlab = "Résidus", breaks = 20)

# Test de normalité des résidus (test de Shapiro-Wilk)
shapiro.test(resid_prod)

# Comparaison des résidus avec ceux du modèle Holt-Winters
resid_hw_prod <- residuals(hw_model)

# Test de Ljung-Box pour les résidus du modèle Holt-Winters
Box.test(resid_hw_prod, lag = 20, type = "Ljung-Box")

# Histogramme des résidus du modèle Holt-Winters
hist(resid_hw_prod, main = "Histogramme des Résidus - Holt-Winters", xlab = "Résidus", breaks = 20)

# Test de normalité des résidus du modèle Holt-Winters
shapiro.test(resid_hw_prod)



#### Résidus du modèle ARIMA pour la série "job_openings": serie 2
resid_job <- residuals(model_job)


# Test de Ljung-Box pour la stationnarité des résidus (si les résidus sont des bruits blancs)
Box.test(resid_job, lag = 20, type = "Ljung-Box")

# Histogramme des résidus
hist(resid_job, main = "Histogramme des Résidus - ARIMA", xlab = "Résidus", breaks = 20)

# Test de normalité des résidus (test de Shapiro-Wilk)
shapiro.test(resid_job)

# Comparaison des résidus avec ceux du modèle Holt-Winters
resid_hw_job <- residuals(hw_job)

# Test de Ljung-Box pour les résidus du modèle Holt-Winters
Box.test(resid_hw_job, lag = 20, type = "Ljung-Box")

# Histogramme des résidus du modèle Holt-Winters
hist(resid_hw_job, main = "Histogramme des Résidus - Holt-Winters", xlab = "Résidus", breaks = 20)

# Test de normalité des résidus du modèle Holt-Winters
shapiro.test(resid_hw_job)




