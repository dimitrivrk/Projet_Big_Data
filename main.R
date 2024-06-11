data <- read.csv("Patrimoine_Arbore_SOURCE.csv", 
    header = TRUE,
    sep = ",",
    encoding="latin1")

    
data <- data.frame(data)

data[data == ""] <- NA
data[data == " "] <- NA


# pour chaque colonne modifier l'encodage  passe les en UTF8 car il ne le sont pas de base
for (i in 1:ncol(data)) {
    data[,i] <- iconv(data[,i], from="latin1",to = "UTF-8")
}

# Description du jeu de données
#summary(data)


# Conversion des types de données
data$X <- as.numeric(data$X)
data$Y <- as.numeric(data$Y)
data$OBJECTID <- as.numeric(data$OBJECTID)
data$created_date <- as.Date(data$created_date)
data$created_user <- as.factor(data$created_user)
data$src_geo <- as.factor(data$src_geo)
data$clc_quartier <- as.factor(data$clc_quartier)
data$clc_secteur <- as.factor(data$clc_secteur)
data$id_arbre <- as.factor(data$id_arbre)
data$haut_tot <- as.numeric(data$haut_tot)
data$haut_tronc <- as.numeric(data$haut_tronc)
data$tronc_diam <- as.numeric(data$tronc_diam)
data$fk_arb_etat <- as.factor(data$fk_arb_etat)
data$fk_stadedev <- as.factor(data$fk_stadedev)
data$fk_port <- as.factor(data$fk_port)
data$fk_pied <- as.factor(data$fk_pied)
data$fk_situation <- as.factor(data$fk_situation)
data$fk_revetement <- ifelse(is.na(data$fk_revetement), FALSE, data$fk_revetement == "Oui")
data$commentaire_environnement <- as.character(data$commentaire_environnement)
data$dte_plantation <- as.Date(data$dte_plantation)
data$age_estim <- as.numeric(data$age_estim)
data$fk_prec_estim <- as.numeric(data$fk_prec_estim)
data$clc_nbr_diag <- as.numeric(data$clc_nbr_diag)
data$dte_abattage <- as.Date(data$dte_abattage)
data$fk_nomtech <- as.character(data$fk_nomtech)
data$last_edited_user <- as.factor(data$last_edited_user)
data$last_edited_date <- as.Date(data$last_edited_date)
data$villeca <- as.factor(data$villeca)
data$nomfrancais <- as.character(data$nomfrancais)
data$nomlatin <- as.character(data$nomlatin)
data$GlobalID <- as.character(data$GlobalID)
data$CreationDate <- as.Date(data$CreationDate)
data$Creator <- as.factor(data$Creator)
data$EditDate <- as.Date(data$EditDate)
data$Editor <- as.factor(data$Editor)
data$feuillage <- as.factor(data$feuillage)
data$remarquable <- ifelse(is.na(data$remarquable), FALSE, data$remarquable == "Oui")


#* Nettoyage des données


#* Mettre en minuscule les valeurs des colonnes
#? OK!

for (i in 1:ncol(data)) {
    if(is.factor(data[,i]) | is.character(data[,i])){
        data[,i] <- tolower(data[,i])
    }
}


#* Supprimer toute les lignes avec des valeurs manquantes en X ou Y
#? OK!
data <- data[!is.na(data$X) | !is.na(data$Y),]


#* Suppression des doublons
#? OK! Enfin je pense 
#! A tester 
data <- unique(data)


#* Completer les lignes avec des valeurs manquantes
#* created_date
#* Si pas de date on prend celle du dessus
#? OK!
for (i in 1:nrow(data)) {
    if(is.na(data[i, "created_date"])){
        data[i, "created_date"] <- data[i-1, "created_date"]
    }
}


#* les ligne qui possède plus de 13 valeurs manquantes sont supprimées.
#* Pourquoi 13 ? cf le rapport
#? OK!
nrow(data)
data <- data[rowSums(is.na(data)) < 13,]


#* formatage des noms
#* remplace les . de la colone created_user pas des espaces
#* rq : ils sont déjà en miniscules
#? OK!
data$created_user <- gsub("\\.", " ", data$created_user)
data$last_edited_user <- gsub("\\.", " ", data$last_edited_user)
data$Creator <- gsub("\\.", " ", data$Creator)
data$Editor <- gsub("\\.", " ", data$Editor)


#* Les valeurs manquantes des quartier sont remplacées par la valeur du quartier du meme secteur si il est renseigné
#* il est remplacé par la valeur du quartier de l'arbre le plus proche qui possède un quartier renseigné avec x=data[i, "X"] et y=data[i, "Y"]
#* on a fixé une distance limite de 275 qui n'est pas utilisé dans notre cas
#* mais qui empecherait qu'un arbre vraiment loin soit ajouté à un quartier, il resterait alors non catégorisé

#? OK!
for (i in 1:nrow(data)) {
    if(is.na(data[i, "clc_quartier"])){
        secteur <- data[i, "clc_secteur"]
        quartier <- data[data$clc_secteur == secteur, "clc_quartier"]
        if(!is.na(quartier[1])){
            data[i, "clc_quartier"] <- quartier[1]
        }else{
            min_distance <- Inf
            for (j in 1:nrow(data)) {
                if(!is.na(data[j, "clc_quartier"])){
                    distance <- sqrt((data[i, "X"] - data[j, "X"])**2 + (data[i, "Y"] - data[j, "Y"])**2)
                    if(distance < min_distance && distance < 275){
                        min_distance <- distance
                        closest_quartier <- data[j, "clc_quartier"]
                        data[i, "clc_quartier"] <- closest_quartier
                    }
                }
            }
        }
    }
}


#* les valeurs manquante de tronc_diam sont remplacé par la moyenne des tronc_diam de la meme espece et au même stade de développement
#* Si on ne connait pas l'espèce on se base sur le diamètre des troncs qui ont ont le même age et feuillage (comprenant que les non NA et >0)
#? OK!
for (i in seq_len(nrow(data))) {
  if (is.na(data[i, "tronc_diam"])) {
    espece <- data[i, "nomfrancais"]
    fk_stadedev <- data[i, "fk_stadedev"]
    
    tronc_diam <- data$tronc_diam[data$nomfrancais == espece & data$fk_stadedev == fk_stadedev  & data$tronc_diam > 0]
    if (!all(is.na(tronc_diam))) {
      data[i, "tronc_diam"] <- round(mean(tronc_diam, na.rm = TRUE))
    } else {
      
      age_estim <- data[i, "age_estim"]
      feuillage <- data[i, "feuillage"]
      
      tronc_diam <- data$tronc_diam[data$age_estim == age_estim & data$feuillage== feuillage & data$tronc_diam > 0]
      if (!all(is.na(tronc_diam))) {
        data[i, "tronc_diam"] <- round(mean(tronc_diam, na.rm = TRUE))
      }
    }
  }
}


#* Les valeurs manquantes de fk_stadedev sont remplaces par le  fk_stadedev le plus frequent des arbres de la meme espece et du meme age_estim (comprenant que les non NA et >0)
#* mettre fk_stadedev a "mort" si fk_arb_etat est differente de "en place" ou "non éssouché" ou "remplacé"
#? OK!
#TODO A REVOIR 
for (i in seq_len(nrow(data))) {
  if (is.na(data[i, "fk_stadedev"])) {
    espece <- data[i, "nomfrancais"]
    fk_arb_etat <- data[i, "fk_arb_etat"]
    
    fk_stadedev <- data$fk_stadedev[data$nomfrancais == espece & data$fk_arb_etat == fk_arb_etat]
    if (!all(is.na(fk_stadedev))) {
      data[i, "fk_stadedev"] <- names(sort(table(fk_stadedev), decreasing = TRUE))[1]
    } else {
      data[i, "fk_stadedev"] <- "mort"
    }
  }
}


#* remplace les NA de commentaire_environnement par "RAS"
#? OK!
data$commentaire_environnement[is.na(data$commentaire_environnement)] <- "RAS"

#* remplacer les NA de age_estim a l'aide d'une regression lineaire entre les age_estim connus et leur tronc_diam connus de la meme espece qui determine la valeur de age_estim pour un tronc_diam donné
#? OK!
for (i in seq_len(nrow(data))) {
  if (is.na(data[i, "age_estim"])) {
    espece <- data[i, "nomfrancais"]
    age_estim <- data$age_estim[data$nomfrancais == espece & !is.na(data$age_estim) & data$tronc_diam > 0]
    tronc_diam <- data$tronc_diam[data$nomfrancais == espece & !is.na(data$age_estim) & data$tronc_diam > 0]
    if (!all(is.na(age_estim)) && !all(is.na(tronc_diam)) && !is.na(data[i, "tronc_diam"])) {
      model <- lm(age_estim ~ tronc_diam - 1, data = data.frame(age_estim, tronc_diam))
      data[i, "age_estim"] <- round(predict(model, data.frame(tronc_diam = data[i, "tronc_diam"])))
    }
  }
}

# remplacer tout les NA de clc_nbr_diag sont remplacé par 0
#? OK!
data$clc_nbr_diag[is.na(data$clc_nbr_diag)] <- 0





# #* les valeurs de remaquable sont remplacé a l'aide d'une regression linaire qui prend en compte les valeurs de tronc_diam et de haut_tot fk_stadedev et fk_arb_etat
# #? pas OK
# cat("lala",sum(is.na(data$remarquable)), "\n")
# for (i in seq_len(nrow(data))) {
#     if (is.na(data[i, "remarquable"]) || data[i, "remarquable"] == 0) {
#         # Filtrer les données valides pour le modèle
#         valid_data <- data[!is.na(data$remarquable) & data$remarquable != 0 & !is.na(data$tronc_diam) & !is.na(data$haut_tot) , ]
        
#         # S'assurer qu'il y a suffisamment de données pour créer un modèle
#         if (nrow(valid_data) > 1) {
#             tronc_diam <- valid_data$tronc_diam
#             haut_tot <- valid_data$haut_tot
#             remarquable <- valid_data$remarquable
            
#             model <- lm(remarquable ~ tronc_diam + haut_tot +  + fk_arb_etat, data = valid_data)
#             predicted_value <- predict(model, newdata = data.frame(tronc_diam = data[i, "tronc_diam"], haut_tot = data[i, "haut_tot"]))
            
#             data[i, "remarquable"] <- ifelse(predicted_value > 0.5, 1, 0)
#         }
#     }
# }

# sum(is.na(data$remarquable))


# Détection des valeurs aberrantes
# Suppression des valeurs aberrantes pour toutes les colonnes
# for (i in 1:ncol(data)) {
#     outliers <- boxplot.stats(data[, i])$out
#     data <- data[!data[, i] %in% outliers, ]
# }


# Affichage du jeu de données après nettoyage
# View(data)
