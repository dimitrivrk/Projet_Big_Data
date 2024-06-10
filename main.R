data <- read.csv("Patrimoine_Arbore.csv", header = TRUE, sep = ",", dec = ".", quote = "\"", fill = TRUE, comment.char = "", stringsAsFactors = FALSE)
data <- data.frame(data)


# pour chaque colonne modifier l'encodage  passe les en UTF8 car il ne le sont pas de base
for (i in 1:ncol(data)) {
  if (is.character(data[,i])) {
    data[,i] <- iconv(data[,i], to = "UTF-8")
  }
}

# Description du jeu de données
summary(data)

# Affichage des premières lignes du jeu de données
head(data)

# # Conversion des types de données
# data$X <- as.factor(data$X)
# data$Y <- as.factor(data$Y)
# data$OBJECTID <- as.factor(data$OBJECTID)
# data$created_date <- as.Date(data$created_date, format = "%Y-%m-%d")
# data$created_user <- as.factor(data$created_user)
# data$src_geo <- as.factor(data$src_geo)
# data$clc_quartier <- as.factor(data$clc_quartier)
# data$clc_secteur <- as.factor(data$clc_secteur)
# data$id_arbre <- as.factor(data$id_arbre)
# data$haut_tot <- as.numeric(data$haut_tot)
# data$haut_tronc <- as.numeric(data$haut_tronc)
# data$tronc_diam <- as.numeric(data$tronc_diam)
# data$fk_arb_etat <- as.factor(data$fk_arb_etat)
# data$fk_stadedev <- as.factor(data$fk_stadedev)
# data$fk_port <- as.factor(data$fk_port)
# data$fk_pied <- as.factor(data$fk_pied)
# data$fk_situation <- as.factor(data$fk_situation)
# data$fk_revetement <- as.factor(data$fk_revetement)
# data$commentaire_environnement <- as.factor(data$commentaire_environnement)
# data$dte_plantation <- as.Date(data$dte_plantation, format = "%Y-%m-%d")
# data$age_estim <- as.numeric(data$age_estim)
# data$fk_prec_estim <- as.factor(data$fk_prec_estim)
# data$clc_nbr_diag <- as.factor(data$clc_nbr_diag)
# data$dte_abattage <- as.Date(data$dte_abattage, format = "%Y-%m-%d")
# data$fk_nomtech <- as.factor(data$fk_nomtech)
# data$last_edited_user <- as.factor(data$last_edited_user)
# data$last_edited_date <- as.Date(data$last_edited_date, format = "%Y-%m-%d")
# data$villeca <- as.factor(data$villeca)
# data$nomfrancais <- as.factor(data$nomfrancais)
# data$nomlatin <- as.factor(data$nomlatin)
# data$GlobalID <- as.factor(data$GlobalID)
# data$CreationDate <- as.Date(data$CreationDate, format = "%Y-%m-%d")
# data$Creator <- as.factor(data$Creator)
# data$EditDate <- as.Date(data$EditDate, format = "%Y-%m-%d")
# data$Editor <- as.factor(data$Editor)
# data$feuillage <- as.factor(data$feuillage)
# data$remarquable <- as.factor(data$remarquable)






