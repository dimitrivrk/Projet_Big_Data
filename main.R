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
data$nomfrancais <- as.factor(data$nomfrancais)
data$nomlatin <- as.character(data$nomlatin)
data$GlobalID <- as.character(data$GlobalID)
data$CreationDate <- as.Date(data$CreationDate)
data$Creator <- as.factor(data$Creator)
data$EditDate <- as.Date(data$EditDate)
data$Editor <- as.factor(data$Editor)
data$feuillage <- as.factor(data$feuillage)
data$remarquable <- ifelse(is.na(data$remarquable), NA, data$remarquable == "Oui")

#* Nettoyage des données
#* Mettre en minuscule les valeurs des colonnes
#*
#* Cette fonction prend en entrée un dataframe et met en minuscule les valeurs des colonnes qui sont de type factor ou character.
#*
#* @param data Le dataframe à nettoyer
#* @return Le dataframe avec les valeurs des colonnes en minuscule
#*
#? OK!
clean_data <- function(data) {
    for (i in 1:ncol(data)) {
        if(is.factor(data[,i]) | is.character(data[,i])){
                data[,i] <- tolower(data[,i])
        }
    }
    return(data)
}

data <- clean_data(data)

#* Supprimer toutes les lignes avec des valeurs manquantes en X ou Y
#*
#* Cette fonction prend en entrée un dataframe et supprime toutes les lignes qui ont des valeurs manquantes en X ou Y.
#*
#* @param data Le dataframe contenant les données
#* @return Le dataframe avec les lignes contenant des valeurs manquantes en X ou Y supprimées
#*
#? OK!
remove_X_Y <- function(data) {
    data <- data[!is.na(data$X) | !is.na(data$Y),]
    return(data)
}

data <- remove_X_Y(data)


#* Fonction pour compléter les dates de création manquantes dans les données
# 
# Cette fonction parcourt les lignes du dataframe 'data' et remplace les valeurs manquantes
# dans la colonne 'created_date' par la valeur de la ligne précédente.
# 
# Args:
#   data: Le dataframe contenant les données à traiter
# 
# Returns:
#   Le dataframe 'data' avec les dates de création complétées
complete_created_date <- function(data) {
    for (i in 1:nrow(data)) {
        if (is.na(data[i, "created_date"])) {
            data[i, "created_date"] <- data[i-1, "created_date"]
        }
    }
    return(data)
}

data <- complete_created_date(data)

remobe_NA_lines <- function(data) {
    #* Supprime les lignes qui ont plus de 13 valeurs manquantes.
    #* Cette fonction est utilisée pour nettoyer les données en éliminant les lignes avec un grand nombre de valeurs manquantes.
    #* Les lignes avec moins de 13 valeurs manquantes sont conservées.
    #* Paramètres:
    #*   - data: Le jeu de données à nettoyer.
    #* Retourne:
    #*   Le jeu de données nettoyé, avec les lignes contenant plus de 13 valeurs manquantes supprimées.
    data <- data[rowSums(is.na(data)) < 13,]
    return(data)
}

data <- remobe_NA_lines(data)

data$src_geo[is.na(data$src_geo)] <- "orthophoto"

#* Fonction pour formater les noms
#'
#' Cette fonction prend en entrée un dataframe et remplace les points dans les colonnes
#' created_user, last_edited_user, Creator et Editor par des espaces.
#' Les noms doivent déjà être en minuscules.
#'
#' @param data Le dataframe contenant les données à formater
#' @return Le dataframe avec les noms formatés
#'
formatage_noms <- function(data) {
    data$created_user <- gsub("\\.", " ", data$created_user)
    data$last_edited_user <- gsub("\\.", " ", data$last_edited_user)
    data$Creator <- gsub("\\.", " ", data$Creator)
    data$Editor <- gsub("\\.", " ", data$Editor)
    return(data)
}

data <- formatage_noms(data)

#' Remplace les valeurs manquantes dans la colonne "clc_quartier" en utilisant les valeurs de la même colonne pour le même secteur ou en utilisant la valeur du quartier le plus proche.
#'
#' @param data Le dataframe contenant les données.
#' @return Le dataframe avec les valeurs manquantes dans la colonne "clc_quartier" remplacées.
#'
#' @details Cette fonction parcourt chaque ligne du dataframe et vérifie si la valeur de la colonne "clc_quartier" est manquante. Si c'est le cas, elle recherche d'abord une valeur non manquante dans la même colonne pour le même secteur. Si une telle valeur est trouvée, elle remplace la valeur manquante par cette valeur. Sinon, elle recherche le quartier le plus proche en calculant la distance euclidienne entre les coordonnées (X, Y) de la ligne actuelle et les coordonnées des autres lignes avec des valeurs non manquantes dans la colonne "clc_quartier". Si la distance est inférieure à 275, elle remplace la valeur manquante par le quartier le plus proche.
#'
replace_missing_quartier <- function(data) {
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
    return(data)
}


# Mettre "orthophoto" dans toutes les valeurs de src_geo qui sont NA
data <- replace_missing_quartier(data)


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

data$fk_nomtech[is.na(data$fk_nomtech)] <- "ras"
data$nomfrancais[is.na(data$nomfrancais)] <- "ras"
data$nomlatin[is.na(data$nomlatin)] <- "ras"
data <- data[data$nomfrancais != "ras" & data$nomfrancais != "indéterminé",]


#faire une regression logistique pour completer les valeurs manquantes de remarquable en fonction de la haut_tot, tronc_diam, hau_tronc et nomfrancais
#? OK!
cat("remarquable :",sum(is.na(data$remarquable)))
# Perform logistic regression
logistic_regression <- glm(remarquable ~ nomfrancais + haut_tot + tronc_diam + haut_tronc + as.factor(fk_stadedev), data = data, family = binomial)

# Predict the values of remarquable using the logistic regression model
for (i in seq_len(nrow(data))) {
  if (is.na(data[i, "remarquable"])) {
    data[i, "remarquable"] <- predict(logistic_regression, newdata = data[i, ], type = "response")
  }
}

# Convert the predicted values to TRUE or FALSE based on a threshold
data$remarquable <- ifelse(data$remarquable > 0.5, TRUE, FALSE)

sum(is.na(data$remarquable))





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


# Function to replace missing age_estim values
replace_missing_age_estim <- function(data) {
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, "age_estim"])) {
      vector <- c()
      for (j in c('tronc_diam', 'haut_tot', 'haut_tronc', 'fk_stadedev')) {
        if (!is.na(data[i, j])) {
          vector <- c(vector, j)
        }
      }
      if (length(vector) > 0) {
        # Vérifier et exclure les prédicteurs colinéaires
        formula <- as.formula(paste("age_estim ~", paste(vector, collapse = " + ")))
        model <- lm(formula, data = data, na.action = na.exclude)
        new_data <- data.frame(
          tronc_diam = data[i, "tronc_diam"],
          haut_tot = data[i, "haut_tot"],
          haut_tronc = data[i, "haut_tronc"],
          fk_stadedev = data[i, "fk_stadedev"]
        )
        # Garder seulement les colonnes présentes dans le modèle
        new_data <- new_data[, vector, drop = FALSE]
        predicted_value <- predict(model, newdata = new_data)
        data[i, "age_estim"] <- max(round(predicted_value), 0)
      }
    }
  }
  return(data)
}
# remove lines if we failed to predict the age
data <- data[!is.na(data$age_estim), ]

#supprimer les lignes avec des valeurs manquantes dans la colonne nomfrancais


impute_feuillage_simple <- function(data) {
  cat("Nombre de valeurs manquantes avant : ", sum(is.na(data$feuillage)), "\n")
  
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, "feuillage"])) {
      espece <- data[i, "nomfrancais"]
      feuillage <- data$feuillage[data$nomfrancais == espece & !is.na(data$feuillage)]
      if (length(feuillage) > 0) {
        data[i, "feuillage"] <- feuillage[1]
      }
    }
  }
  
  cat("Nombre de valeurs manquantes après : ", sum(is.na(data$feuillage)), "\n")
  
  return(data)
}

# Utilisation de la fonction
data <- impute_feuillage_simple(data)
print(data[is.na(data$feuillage), ])




# for (i in seq_len(nrow(data))) {
#     if (is.na(data[i, "feuillage"])){
#         espece <- data[i, "nomfrancais"]
#         feuillage <- data$feuillage[data$nomfrancais == espece]
#         model <- glm(ifelse(feuillage=="feullu",1,0) ~ nomfrancais, data = data, family = binomial)
#         data[i, "feuillage"] <- ifelse(predict(model, type = "response") > 0.5, "feullu", "conifère")
#     }
# }



# # Convertir les valeurs de feuillage en valeurs binaires (0 et 1)
# data$feuillage_bin <- ifelse(data$feuillage == "feullu", 1, 0)

# # Régression logistique pour déterminer le feuillage
# model <- glm(feuillage_bin ~ nomfrancais, data = data, family = binomial)
# data$feuillage_bin <- ifelse(predict(model, type = "response") > 0.5, 1, 0)





# Afficher le nombre de valeurs manquantes avant et après l'exécution de la fonction
cat("Nombre de valeurs manquantes avant : ", sum(is.na(data$age_estim)), "\n")
data <- replace_missing_age_estim(data)
cat("Nombre de valeurs manquantes après : ", sum(is.na(data$age_estim)), "\n")

# remplacer tout les NA de clc_nbr_diag sont remplacé par 0
#? OK!
data$clc_nbr_diag[is.na(data$clc_nbr_diag)] <- 0

nrow(data)




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
# data$fk_revetement <- ifelse(is.na(data$fk_revetement), FALSE, data$fk_revetement == "Oui")
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
# data$remarquable <- ifelse(is.na(data$remarquable), FALSE, data$remarquable == "Oui")

rm(list=setdiff(ls(), "data"))
