data <- read.csv("Patrimoine_Arbore.csv", header = TRUE, sep = ",", dec = ".", quote = "\"", fill = TRUE, comment.char = "", stringsAsFactors = FALSE)
data <- data.frame(data)


# pour chaque colonne modifier l'encodage  passe les en UTF8 car il ne le sont pas de base
for (i in 1:ncol(data)) {
    data[,i] <- iconv(data[,i], to = "UTF-8")
}

# Description du jeu de données
#summary(data)

head(data)



# Conversion des types de données
data$X <- as.numeric(data$X)
data$Y <- as.numeric(data$Y)
data$OBJECTID <- as.numeric(data$OBJECTID)
data$created_date <- as.Date(data$created_date, format = "%Y/%m/%d %H:%M:%S%z")
data$created_user <- as.factor(data$created_user)
data$src_geo <- as.factor(data$src_geo)
data$clc_quartier <- as.factor(data$clc_quartier)
data$clc_secteur <- as.factor(data$clc_secteur)
data$id_arbre <- as.numeric(data$id_arbre)
data$haut_tot <- as.numeric(data$haut_tot)
data$haut_tronc <- as.numeric(data$haut_tronc)
data$tronc_diam <- as.numeric(data$tronc_diam)
data$fk_arb_etat <- as.factor(data$fk_arb_etat)
data$fk_stadedev <- as.factor(data$fk_stadedev)
data$fk_port <- as.factor(data$fk_port)
data$fk_pied <- as.factor(data$fk_pied)
data$fk_situation <- as.factor(data$fk_situation)

data$fk_revetement <- data$fk_revetement == "Oui"

data$commentaire_environnement <- as.character(data$commentaire_environnement)
data$dte_plantation <- as.Date(data$dte_plantation, format = "%Y/%m/%d %H:%M:%S%z")
data$age_estim <- as.numeric(data$age_estim)
data$fk_prec_estim <- as.numeric(data$fk_prec_estim)
data$clc_nbr_diag <- as.numeric(data$clc_nbr_diag)
data$dte_abattage <- as.Date(data$dte_abattage, format = "%Y/%m/%d %H:%M:%S%z")
data$fk_nomtech <- as.character(data$fk_nomtech)
data$last_edited_user <- as.factor(data$last_edited_user)
data$last_edited_date <- as.Date(data$last_edited_date, format = "%Y/%m/%d %H:%M:%S%z")
data$villeca <- as.factor(data$villeca)
data$nomfrancais <- as.character(data$nomfrancais)
data$nomlatin <- as.character(data$nomlatin)
data$GlobalID <- as.character(data$GlobalID)
data$CreationDate <- as.Date(data$CreationDate, format = "%Y/%m/%d %H:%M:%S%z")
data$Creator <- as.factor(data$Creator)
data$EditDate <- as.Date(data$EditDate, format = "%Y/%m/%d %H:%M:%S%z")
data$Editor <- as.factor(data$Editor)
data$feuillage <- as.factor(data$feuillage)

data$remarquable <- data$remarquable == "Oui"


summary(data$X)
# Nettoyage des données
# Valeurs manquantes, valeurs aberrantes et Doublons


# met tous les caractères en miniscule
for (i in 1:ncol(data)) {
    data[,i] <- tolower(data[,i])
}
#supprimer toute les lignes avec des valeurs manquantes en X ou Y
data <- data[!is.na(data$X) | !is.na(data$Y),]

summary(data$X)
# Suppression des doublons
data <- unique(data)

# completer les lignes avec des valets manquantes par la mediane des valeurs de la colonne 
for (i in 1:ncol(data)) {
  if (is.numeric(data[,i])) {
    data[,i][is.na(data[,i])] <- median(data[,i], na.rm = TRUE)
  }
}


# Détection des valeurs aberrantes
# Suppression des valeurs aberrantes pour toutes les colonnes
# for (i in 1:ncol(data)) {
#     outliers <- boxplot.stats(data[, i])$out
#     data <- data[!data[, i] %in% outliers, ]
# }


# Affichage des premières lignes du jeu de données après nettoyage
#head(data)


# Histogramme de la hauteur totale
#hist(data$haut_tot, main = "Histogramme de la hauteur totale", xlab = "Hauteur totale", ylab = "Fréquence", col = "lightblue", border = "black")

# Fréquence des variables catégorielles
#cat_vars <- c("X", "Y", "OBJECTID", "created_user", "src_geo", "clc_quartier", "clc_secteur", "id_arbre", "fk_arb_etat", "fk_stadedev", "fk_port", "fk_pied", "fk_situation", "commentaire_environnement", "fk_prec_estim", "clc_nbr_diag", "fk_nomtech", "last_edited_user", "villeca", "nomfrancais", "nomlatin", "GlobalID", "Creator", "Editor", "feuillage", "remarquable")



#Boxplot du diamètre du tronc
#boxplot(data$tronc_diam, main = "Boxplot du diamètre du tronc", xlab = "Diamètre du tronc", col = "lightblue", border = "black")

# Boxplot de la hauteur totale par quartier
#boxplot(data$haut_tot ~ data$clc_quartier, main = "Boxplot de la hauteur totale par quartier", xlab = "Quartier", ylab = "Hauteur totale", col = "lightblue", border = "black")

# Distribution des arbres par quartier
#barplot(table(data$clc_quartier), main = "Distribution des arbres par quartier", xlab = "Quartier", ylab = "Nombre d'arbres", col = "lightblue", border = "black")

#Répartition des types de feuillage
#barplot(table(data$feuillage), main = "Répartition des types de feuillage", xlab = "Type de feuillage", ylab = "Nombre d'arbres", col = "lightblue", border = "black")


# Créer des représentations graphiques
#Exemple: répartition des arbres suivant leur stade de développement
#barplot(table(data$fk_stadedev), main = "Répartition des arbres suivant leur stade de développement", xlab = "Stade de développement", ylab = "Nombre d'arbres", col = "lightblue", border = "black")


#Créer des histogrammes
#Exemple: Quantité d’arbres en fonction du quartier/secteur, de sa situation
#hist(data$haut_tot[data$clc_quartier == "quartier du centre-ville"], main = "Quantité d’arbres en fonction du quartier", xlab = "Hauteur totale", ylab = "Nombre d'arbres", col = "lightblue", border = "black")




