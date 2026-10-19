install.packages('evir')
install.packages('ismev')
install.packages('fExtremes')
install.packages('POT')
install.packages('tidyr')
install.packages('extRemes')
install.packages('graphics')
install.packages('tseries')
install.packages("imputeTS") 
install.packages(c("extRemes", "tseries"))
library(evir)
library(ismev)
library(fExtremes)
library(POT)
library(tidyr)
library(extRemes)
library(graphics)
library(tseries)
library(imputeTS)
library(extRemes)
library(tseries)



############## CODE R n°0 : départ propre ##############
rm(list = ls())          # vide la mémoire
graphics.off()           # ferme les fenêtres graphiques
dossier <- "C:/Users/hp/Desktop/M2/pfof DEM/projet Prof DEM 2026/nouveau"   # adapte si besoin
dir.create(dossier, showWarnings = FALSE)
setwd(dossier)
getwd()                  # doit afficher le chemin du dossier "nouveau"
ls()                     # doit afficher character(0) : aucun objet en mémoire
list.files()             # doit être vide au départ


############## question (2.1 et 2.2)##############

############## CODE R n°1 : chargement et valeurs manquantes (sans imputation) ##############
dir.create("figures_R", showWarnings = FALSE)

#### 1. Chargement de la base (choisir le fichier CSV dans la fenêtre) ####
donnees <- read.table(file.choose(), sep = ";", header = TRUE, dec = ".",
                      na.strings = c("NA", ""), fileEncoding = "UTF-8-BOM")
names(donnees) <- c("Date", "Kedougou", "Mako")          # noms simplifiés
donnees$Date <- as.Date(donnees$Date, format = "%d/%m/%Y")

#### 2. Vérification de la structure ####
dim(donnees)
head(donnees)
tail(donnees)
str(donnees)
range(donnees$Date)

#### 3. Valeurs manquantes ####
colSums(is.na(donnees[, c("Kedougou", "Mako")]))                  # nombre
round(100 * colMeans(is.na(donnees[, c("Kedougou", "Mako")])), 1)  # pourcentage

plus_long_trou <- function(x) { r <- rle(is.na(x)); max(r$lengths[r$values]) }
plus_long_trou(donnees$Kedougou)      # plus long trou consécutif (en mois)
plus_long_trou(donnees$Mako)

mois <- as.integer(format(donnees$Date, "%m"))
table(mois[is.na(donnees$Kedougou)])  # mois concernés par les manquants
table(mois[is.na(donnees$Mako)])

#### 4. Variables utiles pour la suite ####
# année hydrologique : de mai à avril (mai 1971 -> avril 1972 = année 1971)
donnees$annee_hydro <- as.integer(format(donnees$Date, "%Y")) - (mois < 5)
table(donnees$annee_hydro)            # doit donner 12 mois pour chacune des 49 années

#### B1. Mois manquants par année hydrologique ####
manq_K <- tapply(is.na(donnees$Kedougou), donnees$annee_hydro, sum)
manq_M <- tapply(is.na(donnees$Mako),     donnees$annee_hydro, sum)
dev.new(width = 9, height = 4.5, noRStudioGD = TRUE)
par(mfrow = c(1, 2))
barplot(manq_K, ylim = c(0, 12), col = "blue", las = 2, cex.names = 0.6,
        xlab = "Année hydrologique", ylab = "Nombre de mois manquants", main = "Kédougou")
barplot(manq_M, ylim = c(0, 12), col = "red", las = 2, cex.names = 0.6,
        xlab = "Année hydrologique", ylab = "Nombre de mois manquants", main = "Mako")
dev.copy2pdf(file = "figures_R/fig_manquants.pdf", width = 9, height = 4.5)

#### 5. Sauvegarde de la base (valeurs manquantes conservées) ####
write.csv2(donnees, "base_observee.csv", row.names = FALSE)

############## question (2.3 2.4 et 2.5)##############

############## CODE R n°2 : statistiques descriptives et graphiques (valeurs observées) ##############
if (!exists("donnees")) {
  donnees <- read.csv2("base_observee.csv")
  donnees$Date <- as.Date(donnees$Date)
}
dir.create("figures_R", showWarnings = FALSE)
xK <- donnees$Kedougou
xM <- donnees$Mako

#### A. Statistiques descriptives ####
stat_desc <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x); s <- sd(x)
  c(n = length(x), Min = min(x), Q1 = unname(quantile(x, 0.25)),
    Mediane = median(x), Moyenne = m, Q3 = unname(quantile(x, 0.75)),
    Max = max(x), EcartType = s, CV_pct = 100 * s / m,
    Asymetrie     = mean((x - m)^3) / mean((x - m)^2)^(3/2),
    Aplatissement = mean((x - m)^4) / mean((x - m)^2)^2)
}
tab_obs <- rbind(Kedougou = stat_desc(xK), Mako = stat_desc(xM))
round(tab_obs, 2)

# A2. Date du maximum, du minimum et nombre de mois à débit nul
for (st in c("Kedougou", "Mako")) {
  x <- donnees[[st]]
  cat(st, ": max =", max(x, na.rm = TRUE), "en", format(donnees$Date[which.max(x)], "%m/%Y"),
      "| min =", min(x, na.rm = TRUE), "(1re occurrence :", format(donnees$Date[which.min(x)], "%m/%Y"), ")",
      "| mois à débit nul =", sum(x == 0, na.rm = TRUE), "\n")
}

# A3. Débit moyen par mois (année hydrologique)
mois_num <- as.integer(format(donnees$Date, "%m"))
ordre <- c(5:12, 1:4)
etiq  <- c("Mai","Juin","Juil","Aout","Sept","Oct","Nov","Dec","Janv","Fevr","Mars","Avr")
saison <- cbind(Kedougou = tapply(xK, mois_num, mean, na.rm = TRUE)[as.character(ordre)],
                Mako     = tapply(xM, mois_num, mean, na.rm = TRUE)[as.character(ordre)])
rownames(saison) <- etiq
round(saison, 1)

#### B. Graphiques (affichés puis enregistrés dans le dossier figures_R) ####
graphics.off()

# B2. Évolution des deux séries
dev.new(width = 8, height = 4.5, noRStudioGD = TRUE)
plot(donnees$Date, xM, type = "l", col = "red", ylim = range(c(xK, xM), na.rm = TRUE),
     xlab = "Années", ylab = "Débit (m3/s)", main = "Évolution des débits mensuels")
lines(donnees$Date, xK, col = "blue")
legend("topright", legend = c("Kédougou", "Mako"), col = c("blue", "red"), lty = 1, bty = "n")
dev.copy2pdf(file = "figures_R/fig_evolution.pdf", width = 8, height = 4.5)

# B3. Histogrammes et densités
dev.new(width = 9, height = 4.2, noRStudioGD = TRUE)
par(mfrow = c(1, 2))
hist(xK[!is.na(xK)], breaks = 30, probability = TRUE, col = "lightblue",
     xlab = "Débit (m3/s)", main = "Kédougou")
lines(density(xK, na.rm = TRUE), col = "red", lwd = 2)
hist(xM[!is.na(xM)], breaks = 30, probability = TRUE, col = "lightblue",
     xlab = "Débit (m3/s)", main = "Mako")
lines(density(xM, na.rm = TRUE), col = "red", lwd = 2)
dev.copy2pdf(file = "figures_R/fig_hist_densite.pdf", width = 9, height = 4.2)

# B4. Boîtes à moustaches
dev.new(width = 6, height = 4.5, noRStudioGD = TRUE)
boxplot(list("Kédougou" = xK, "Mako" = xM), col = c("lightblue", "salmon"),
        ylab = "Débit (m3/s)", main = "Comparaison des deux stations")
dev.copy2pdf(file = "figures_R/fig_boxplot.pdf", width = 6, height = 4.5)

# B5. Régime saisonnier (débit moyen par mois)
dev.new(width = 8, height = 4.5, noRStudioGD = TRUE)
matplot(1:12, saison, type = "b", pch = c(16, 17), lty = 1, col = c("blue", "red"),
        xaxt = "n", xlab = "Mois (année hydrologique)", ylab = "Débit moyen (m3/s)",
        main = "Débit moyen mensuel")
axis(1, at = 1:12, labels = etiq)
legend("topright", legend = c("Kédougou", "Mako"), col = c("blue", "red"),
       pch = c(16, 17), lty = 1, bty = "n")
dev.copy2pdf(file = "figures_R/fig_saisonnalite.pdf", width = 8, height = 4.5)

list.files("figures_R")     # doit afficher les fichiers PDF



############## question 4.1 ##############

############## CODE R n°3 : maxima annuels (section 4.1), sans imputation ##############
library(tseries)
if (!exists("donnees")) {
  donnees <- read.csv2("base_observee.csv")
  donnees$Date <- as.Date(donnees$Date)
}
dir.create("figures_R", showWarnings = FALSE)
mois  <- as.integer(format(donnees$Date, "%m"))
annee <- donnees$annee_hydro

#### 1. Années dont les hautes eaux (juillet à octobre) sont entièrement observées ####
pic  <- mois %in% 7:10
ok_K <- tapply(!is.na(donnees$Kedougou[pic]), annee[pic], all)
ok_M <- tapply(!is.na(donnees$Mako[pic]),     annee[pic], all)
c(Kedougou = sum(ok_K), Mako = sum(ok_M), les_deux = sum(ok_K & ok_M))
names(ok_K)[!ok_K]            # années exclues à Kédougou
names(ok_M)[!ok_M]            # années exclues à Mako

#### 2. Maxima annuels ####
max_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
BMA_K_all <- tapply(donnees$Kedougou, annee, max_na)   # maximum des mois disponibles
BMA_M_all <- tapply(donnees$Mako,     annee, max_na)
BMA_K <- BMA_K_all[ok_K]      # série principale : années au pic observé
BMA_M <- BMA_M_all[ok_M]
BMA_M_dispo <- BMA_M_all[!is.na(BMA_M_all)]   # comparaison pour Mako : toutes les années
c(length(BMA_K), length(BMA_M), length(BMA_M_dispo))
an_K <- as.integer(names(BMA_K))
range(an_K); all(diff(an_K) == 1)             # Kédougou : années consécutives ?

#### 3. Statistiques descriptives des maxima annuels ####
resume <- function(x) c(n = length(x), Min = min(x), Q1 = unname(quantile(x, 0.25)),
                        Mediane = median(x), Moyenne = mean(x),
                        Q3 = unname(quantile(x, 0.75)), Max = max(x), EcartType = sd(x))
tab_bma <- rbind("Kedougou (pic observe)"     = resume(BMA_K),
                 "Mako (pic observe)"         = resume(BMA_M),
                 "Mako (mois disponibles)"    = resume(BMA_M_dispo))
round(tab_bma, 1)

#### 4. Stationnarité, tendance et indépendance ####
adf.test(as.numeric(BMA_K))                                  # Kédougou : années consécutives
cor.test(an_K, as.numeric(BMA_K), method = "kendall")
acf(as.numeric(BMA_K), lag.max = 3, plot = FALSE)
an_M <- as.integer(names(BMA_M))
cor.test(an_M, as.numeric(BMA_M), method = "kendall")        # Mako : années non consécutives
an_Md <- as.integer(names(BMA_M_dispo))
adf.test(as.numeric(BMA_M_dispo))                            # Mako : toutes les années
cor.test(an_Md, as.numeric(BMA_M_dispo), method = "kendall")
acf(as.numeric(BMA_M_dispo), lag.max = 3, plot = FALSE)

#### B6. Maxima annuels (points pleins : pic observé ; cercles : pic incomplet) ####
dev.new(width = 9, height = 4.5, noRStudioGD = TRUE)
par(mfrow = c(1, 2))
an <- as.integer(names(BMA_K_all))
plot(an, BMA_K_all, type = "p", col = "blue", pch = ifelse(ok_K, 16, 1),
     ylim = c(0, max(BMA_M_all)), xlab = "Année hydrologique",
     ylab = "Maximum annuel (m3/s)", main = "Kédougou")
plot(an, BMA_M_all, type = "p", col = "red", pch = ifelse(ok_M, 16, 1),
     ylim = c(0, max(BMA_M_all)), xlab = "Année hydrologique",
     ylab = "Maximum annuel (m3/s)", main = "Mako")
dev.copy2pdf(file = "figures_R/fig_maxima_annuels.pdf", width = 9, height = 4.5)


############## question 4.2 ##############

############## CODE R n°4 : corrélation entre les deux stations (section 4.2) ##############

#### A. Maxima annuels : années où les deux pics sont entièrement observés ####
ok_both <- ok_K & ok_M
a_K <- as.numeric(BMA_K_all[ok_both])
a_M <- as.numeric(BMA_M_all[ok_both])
length(a_K)                                               # doit donner 33
cor.test(a_K, a_M, method = "pearson")
cor.test(a_K, a_M, method = "spearman", exact = FALSE)
cor.test(a_K, a_M, method = "kendall",  exact = FALSE)
coef(lm(a_M ~ a_K))                                       # droite : Mako en fonction de Kédougou
summary(lm(a_M ~ a_K))$r.squared

#### B. Débits mensuels : mois où les deux stations sont observées ####
commun <- donnees[!is.na(donnees$Kedougou) & !is.na(donnees$Mako), ]
nrow(commun)
cor(commun$Kedougou, commun$Mako)
cor.test(commun$Kedougou, commun$Mako, method = "spearman", exact = FALSE)

#### B7. Maxima annuels : Mako en fonction de Kédougou ####
dev.new(width = 6, height = 5, noRStudioGD = TRUE)
plot(a_K, a_M, pch = 16, xlab = "Maximum annuel à Kédougou (m3/s)",
     ylab = "Maximum annuel à Mako (m3/s)", main = "Années où les deux pics sont observés")
abline(lm(a_M ~ a_K), col = "red", lwd = 2)
abline(0, 1, lty = 2)
dev.copy2pdf(file = "figures_R/fig_correlation.pdf", width = 6, height = 5)


############## question 4.3 et 4.4 ##############
############## CODE R n°5 : ajustement de la GEV avec gevFit (sections 4.3 et 4.4) ##############
library(fExtremes)

# Les trois séries de maxima annuels (construites au code n°3)
series <- list("Kedougou (pic observe)"  = as.numeric(BMA_K),
               "Mako (pic observe)"      = as.numeric(BMA_M),
               "Mako (mois disponibles)" = as.numeric(BMA_M_dispo))

fits_gev    <- lapply(series, function(x) gevFit(x, type = "mle"))      # GEV
fits_gumbel <- lapply(series, function(x) gumbelFit(x, type = "mle"))   # Gumbel

#### A. Résultats de gevFit ####
fits_gev[["Kedougou (pic observe)"]]
fits_gev[["Mako (pic observe)"]]
fits_gev[["Mako (mois disponibles)"]]

#### B. GEV : estimations et erreurs standard (xi = forme, mu = position, beta = échelle) ####
for (nm in names(series)) {
  cat("\n", nm, "\n")
  print(round(data.frame(estimation = fits_gev[[nm]]@fit$par.ests,
                         erreur_std = fits_gev[[nm]]@fit$par.ses), 3))
  cat("convergence (0 = ok) :", fits_gev[[nm]]@fit$converged, "\n")
}

#### C. Gumbel : estimations et erreurs standard ####
for (nm in names(series)) {
  cat("\n", nm, " (Gumbel)\n")
  print(round(data.frame(estimation = fits_gumbel[[nm]]@fit$par.ests,
                         erreur_std = fits_gumbel[[nm]]@fit$par.ses), 3))
}

#### D. Test du rapport de vraisemblance (H0 : Gumbel contre H1 : GEV) ####
lrt <- t(sapply(names(series), function(nm) {
  l1 <- -fits_gev[[nm]]@fit$nllh.final       # log-vraisemblance GEV
  l0 <- -fits_gumbel[[nm]]@fit$nllh.final    # log-vraisemblance Gumbel
  st <- 2 * (l1 - l0)
  c(loglik_GEV = l1, loglik_Gumbel = l0, statistique = st, p_value = 1 - pchisq(st, df = 1))
}))
round(lrt, 4)
qchisq(0.95, df = 1)                         # seuil à 5 %

#### E. Intervalle de confiance à 95 % de xi par vraisemblance profilée ####
ic_profil_xi <- function(x, fit, niveau = 0.95) {
  l_max <- -fit@fit$nllh.final
  p0 <- fit@fit$par.ests
  grille <- seq(-0.9, 0.7, by = 0.005)
  lp <- sapply(grille, function(xi) {
    nll <- function(par) {
      v <- -sum(log(dgev(x, xi = xi, mu = par[1], beta = par[2])))
      if (!is.finite(v) || par[2] <= 0) 1e10 else v
    }
    -optim(c(p0["mu"], p0["beta"]), nll)$value
  })
  dans <- grille[2 * (l_max - lp) <= qchisq(niveau, df = 1)]
  c(xi_estime = unname(p0["xi"]), borne_inf = min(dans), borne_sup = max(dans))
}
ic <- t(sapply(names(series), function(nm) ic_profil_xi(series[[nm]], fits_gev[[nm]])))
round(ic, 3)


############## question 4.5 et 4.6 ##############
############## CODE R n°6 : niveaux de retour et validation (sections 4.5 et 4.6) ##############
library(fExtremes)

# Loi retenue pour chaque série : GEV si p-valeur du test < 0,05, sinon Gumbel
retenu <- ifelse(lrt[, "p_value"] < 0.05, "GEV", "Gumbel")
retenu

# Niveau de retour et intervalle de confiance à 95 % (méthode delta)
niveau_retour <- function(fit, T, gumbel = FALSE, niveau = 0.95) {
  p <- fit@fit$par.ests; V <- fit@fit$varcov
  y <- -log(1 - 1 / T)
  if (gumbel) {
    z    <- p["mu"] - p["beta"] * log(y)
    grad <- c(1, -log(y))                                   # (mu, beta)
  } else {
    xi <- p["xi"]
    z    <- p["mu"] - p["beta"] / xi * (1 - y^(-xi))
    grad <- c(p["beta"] / xi^2 * (1 - y^(-xi)) - p["beta"] / xi * y^(-xi) * log(y),
              1,
              -(1 - y^(-xi)) / xi)                          # (xi, mu, beta)
  }
  se <- sqrt(as.numeric(t(grad) %*% V %*% grad))
  u  <- qnorm(1 - (1 - niveau) / 2)
  c(T = T, niveau_retour = unname(z), erreur_std = se,
    IC_inf = unname(z) - u * se, IC_sup = unname(z) + u * se)
}

#### A. Niveaux de retour du modèle retenu (T = 2, 5, 10, 20, 50 et 100 ans) ####
periodes <- c(2, 5, 10, 20, 50, 100)
for (nm in names(series)) {
  gum <- (retenu[nm] == "Gumbel")
  fit <- if (gum) fits_gumbel[[nm]] else fits_gev[[nm]]
  tab <- t(sapply(periodes, function(T) niveau_retour(fit, T, gumbel = gum)))
  cat("\n", nm, " : modèle retenu =", retenu[nm], "\n")
  print(round(tab, 1))
}

#### B. Comparaison GEV / Gumbel pour T = 10 et 20 ans ####
for (nm in names(series)) {
  cat("\n", nm, "\n")
  print(round(rbind(GEV_10    = niveau_retour(fits_gev[[nm]], 10),
                    Gumbel_10 = niveau_retour(fits_gumbel[[nm]], 10, gumbel = TRUE),
                    GEV_20    = niveau_retour(fits_gev[[nm]], 20),
                    Gumbel_20 = niveau_retour(fits_gumbel[[nm]], 20, gumbel = TRUE)), 1))
}

#### C. Test de Kolmogorov-Smirnov du modèle retenu ####
for (nm in names(series)) {
  gum <- (retenu[nm] == "Gumbel")
  fit <- if (gum) fits_gumbel[[nm]] else fits_gev[[nm]]
  p   <- fit@fit$par.ests
  xi  <- if (gum) 0 else p["xi"]
  cat("\n", nm, "\n")
  print(suppressWarnings(ks.test(series[[nm]], function(q) pgev(q, xi = xi, mu = p["mu"], beta = p["beta"]))))
}

#### B8. Diagnostics du modèle retenu (PP, QQ, niveaux de retour) ####
diagnostics <- function(x, fit, gumbel, titre) {
  p  <- fit@fit$par.ests
  xi <- if (gumbel) 0 else p["xi"]
  x_ord <- sort(x); m <- length(x); i <- 1:m
  pu <- i / (m + 1)
  par(mfrow = c(1, 3))
  plot(pgev(x_ord, xi = xi, mu = p["mu"], beta = p["beta"]), pu, pch = 16,
       xlab = "Probabilité du modèle", ylab = "Probabilité empirique", main = "PP")
  abline(0, 1, col = "red")
  plot(qgev(pu, xi = xi, mu = p["mu"], beta = p["beta"]), x_ord, pch = 16,
       xlab = "Quantiles du modèle", ylab = "Quantiles empiriques", main = "QQ")
  abline(0, 1, col = "red")
  Tg <- exp(seq(log(1.05), log(200), length.out = 100))
  tabg <- t(sapply(Tg, function(T) niveau_retour(fit, T, gumbel = gumbel)))
  plot(Tg, tabg[, "niveau_retour"], type = "l", log = "x", col = "red", lwd = 2,
       ylim = range(c(tabg[, c("IC_inf", "IC_sup")], x)),
       xlab = "Période de retour (années)", ylab = "Niveau de retour (m3/s)", main = "Niveaux de retour")
  lines(Tg, tabg[, "IC_inf"], lty = 2); lines(Tg, tabg[, "IC_sup"], lty = 2)
  points((m + 1) / (m + 1 - i), x_ord, pch = 16)
  mtext(titre, outer = TRUE, line = -1.5)
}
noms_fichiers <- c("kedougou", "mako37", "mako49")
for (k in seq_along(series)) {
  nm  <- names(series)[k]
  gum <- (retenu[nm] == "Gumbel")
  fit <- if (gum) fits_gumbel[[nm]] else fits_gev[[nm]]
  dev.new(width = 12, height = 4.2, noRStudioGD = TRUE)
  par(oma = c(0, 0, 2, 0))
  diagnostics(series[[nm]], fit, gum, paste(nm, "-", retenu[nm]))
  dev.copy2pdf(file = paste0("figures_R/fig_diag_", noms_fichiers[k], ".pdf"), width = 12, height = 4.2)
}
list.files("figures_R")
