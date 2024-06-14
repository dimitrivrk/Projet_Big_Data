data <- read.csv("Patrimoine_Arbore_SOURCE.csv", 
    header = TRUE,
    sep = ",",
    encoding="latin1")

data <- data.frame(data)

# Remplace les valeurs vides par des valeurs manquantes (NA)
data[data == ""] <- NA
data[data == " "] <- NA

# Boucle pour convertir les colonnes du dataframe 'data' de l'encodage latin1 à l'encodage UTF-8
for (i in 1:ncol(data)) {
  data[,i] <- iconv(data[,i], from="latin1",to = "UTF-8")
}



convert_data_types <- function(data) {
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
  
  return(data)
}
data <- convert_data_types(data)


clean_data <- function(data) {
  #* Nettoyage des données
  #* Mettre en minuscule les valeurs des colonnes
  #*
  #* Cette fonction prend en entrée un dataframe et met en minuscule les valeurs des colonnes qui sont de type factor ou character.
  #*
  #* @param data Le dataframe à nettoyer
  #* @return Le dataframe avec les valeurs des colonnes en minuscule
  #*
    for (i in 1:ncol(data)) {
        if(is.factor(data[,i]) | is.character(data[,i])){
                data[,i] <- tolower(data[,i])
        }
    }
    return(data)
}
data <- clean_data(data)


remove_X_Y <- function(data) {
  #* Supprimer toutes les lignes avec des valeurs manquantes en X ou Y
  #*
  #* Cette fonction prend en entrée un dataframe et supprime toutes les lignes qui ont des valeurs manquantes en X ou Y.
  #*
  #* @param data Le dataframe contenant les données
  #* @return Le dataframe avec les lignes contenant des valeurs manquantes en X ou Y supprimées
  #*
    data <- data[!is.na(data$X) | !is.na(data$Y),]
    return(data)
}
cat("Nombre ligne avant suppression X et Y : ", nrow(data)," \n")
data <- remove_X_Y(data)
cat("Nombre ligne après suppression X et Y : ", nrow(data)," \n\n")

complete_created_date <- function(data) {
  #* Fonction pour compléter les dates de création manquantes dans les données
  # 
  #  Cette fonction parcourt les lignes du dataframe 'data' et remplace les valeurs manquantes
  #  dans la colonne 'created_date' par la valeur de la ligne précédente.
  # 
  #  Args:
  #   data: Le dataframe contenant les données à traiter
  # 
  #  Returns:
  #   Le dataframe 'data' avec les dates de création complétées
    for (i in 1:nrow(data)) {
        if (is.na(data[i, "created_date"])) {
            data[i, "created_date"] <- data[i-1, "created_date"]
        }
    }
    return(data)
}
data <- complete_created_date(data)

remove_NA_lines <- function(data) {
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
cat("Nombre ligne avant suppression NA_lines : ", nrow(data)," \n")
data <- remove_NA_lines(data)
cat("Nombre ligne après suppression NA_lines : ", nrow(data)," \n\n")


replace_missing_src_geo <- function(data) {
  #' Remplace les valeurs manquantes dans la colonne "src_geo" par "orthophoto"
  #'
  #' @param data Le dataframe contenant les données.
  #' @return Le dataframe avec les valeurs manquantes dans la colonne "src_geo" remplacées.
  #'
  #' @details Cette fonction remplace les valeurs manquantes dans la colonne "src_geo" du dataframe par la valeur "orthophoto".
  #'
  data$src_geo[is.na(data$src_geo)] <- "orthophoto"
  return(data)
}
cat("Nombre de valeurs manquantes dans la colonne 'src_geo' avant imputation : ", sum(is.na(data$src_geo)), "\n")
data <- replace_missing_src_geo(data)
cat("Nombre de valeurs manquantes dans la colonne 'src_geo' après imputation : ", sum(is.na(data$src_geo)), "\n\n")

formatage_noms <- function(data) {
  #* Fonction pour formater les noms
  #'
  #' Cette fonction prend en entrée un dataframe et remplace les points dans les colonnes
  #' created_user, last_edited_user, Creator et Editor par des espaces.
  #' Les noms doivent déjà être en minuscules.
  #'
  #' @param data Le dataframe contenant les données à formater
  #' @return Le dataframe avec les noms formatés
  #'
    data$created_user <- gsub("\\.", " ", data$created_user)
    data$last_edited_user <- gsub("\\.", " ", data$last_edited_user)
    data$Creator <- gsub("\\.", " ", data$Creator)
    data$Editor <- gsub("\\.", " ", data$Editor)
    return(data)
}
data <- formatage_noms(data)


replace_missing_quartier <- function(data) {
  #' Remplace les valeurs manquantes dans la colonne "clc_quartier" en utilisant les valeurs de la même colonne pour le même secteur ou en utilisant la valeur du quartier le plus proche.
  #'
  #' @param data Le dataframe contenant les données.
  #' @return Le dataframe avec les valeurs manquantes dans la colonne "clc_quartier" remplacées.
  #'
  #' @details Cette fonction parcourt chaque ligne du dataframe et vérifie si la valeur de la colonne "clc_quartier" est manquante. Si c'est le cas, elle recherche d'abord une valeur non manquante dans la même colonne pour le même secteur. Si une telle valeur est trouvée, elle remplace la valeur manquante par cette valeur. Sinon, elle recherche le quartier le plus proche en calculant la distance euclidienne entre les coordonnées (X, Y) de la ligne actuelle et les coordonnées des autres lignes avec des valeurs non manquantes dans la colonne "clc_quartier". Si la distance est inférieure à 275, elle remplace la valeur manquante par le quartier le plus proche.
  #'
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
cat("Nombre de valeurs manquantes dans la colonne 'clc_quartier' avant imputation : ", sum(is.na(data$clc_quartier)), "\n")
data <- replace_missing_quartier(data)
cat("Nombre de valeurs manquantes dans la colonne 'clc_quartier' après imputation : ", sum(is.na(data$clc_quartier)), "\n\n")


remplacer_valeurs_manquantes_tronc_diam <- function(data) {
  #' Remplace les valeurs manquantes dans la colonne 'tronc_diam' en utilisant
  #' les valeurs non manquantes de la même espèce et du même stade de développement
  #' @param data Le jeu de données contenant la colonne 'tronc_diam' avec des valeurs manquantes
  #' @return Le jeu de données modifié avec les valeurs manquantes imputées
  #' 
  
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
  
  return(data)
}
cat("Nombre de valeurs manquantes dans la colonne 'tronc_diam' avant imputation : ", sum(is.na(data$tronc_diam)), "\n")
data <- remplacer_valeurs_manquantes_tronc_diam(data)
cat("Nombre de valeurs manquantes dans la colonne 'tronc_diam' après imputation : ", sum(is.na(data$tronc_diam)), "\n\n")

remplacer_valeurs_manquantes <- function(data) {
  # Fonction pour remplacer les valeurs manquantes dans les colonnes 'fk_nomtech', 'nomfrancais' et 'nomlatin'
  # avec la valeur "ras" et supprimer les lignes avec les valeurs "ras" ou "indéterminé" dans la colonne 'nomfrancais'
  data$fk_nomtech[is.na(data$fk_nomtech)] <- "ras"
  data$nomfrancais[is.na(data$nomfrancais)] <- "ras"
  data$nomlatin[is.na(data$nomlatin)] <- "ras"
  data$commentaire_environnement[is.na(data$commentaire_environnement)] <- "ras"
  data <- data[data$nomfrancais != "ras" & data$nomfrancais != "indéterminé",]
  data$clc_nbr_diag[is.na(data$clc_nbr_diag)] <- 0
  data <- data[, !(names(data) %in% c("created_date", "dte_abattage", "dte_plantation", "last_edited_date", "CreationDate", "EditDate"))]


  return(data)
}
data <- remplacer_valeurs_manquantes(data)


impute_remarquable <- function(data) {
  #' @param data Le jeu de données contenant la colonne 'remarquable' avec des valeurs manquantes
  #' @return Le jeu de données modifié avec les valeurs manquantes imputées
  #' @details Cette fonction utilise la régression logistique pour imputer les valeurs manquantes dans la colonne 'remarquable' 
  #' du jeu de données. Elle calcule d'abord le nombre de valeurs manquantes dans la colonne, puis effectue une régression logistique en utilisant les variables 
  #' 'nomfrancais', 'haut_tot', 'tronc_diam', 'haut_tronc' et 'fk_stadedev' comme prédicteurs. Ensuite, elle itère sur chaque ligne du jeu de
  #'  données et impute les valeurs manquantes en utilisant les prédictions de la régression logistique. Les valeurs prédites sont ensuite converties en TRUE ou FALSE en 
  #' fonction d'un seuil de 0,5. Enfin, le jeu de données modifié est renvoyé.
  # Print the number of missing values in the 'remarquable' column
  
  logistic_regression <- glm(remarquable ~ nomfrancais + haut_tot + tronc_diam + haut_tronc + as.factor(fk_stadedev), data = data, family = binomial)
  
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, "remarquable"])) {
      data[i, "remarquable"] <- predict(logistic_regression, newdata = data[i, ], type = "response")
    }
  }
  data$remarquable <- ifelse(data$remarquable > 0.5, TRUE, FALSE)

  return(data)
}
cat("Nombre de valeurs manquantes dans la colonne 'remarquable' avant imputation : ", sum(is.na(data$remarquable)), "\n")
data <- impute_remarquable(data)
cat("Nombre de valeurs manquantes dans la colonne 'remarquable' après imputation : ", sum(is.na(data$remarquable)), "\n\n")


replace_missing_age_estim <- function(data) {
  #' Remplace les valeurs manquantes de la variable 'age_estim' en utilisant la régression linéaire
  #' @param data Le jeu de données contenant la variable 'age_estim' avec des valeurs manquantes
  #' @return Le jeu de données modifié avec les valeurs manquantes imputées
  #' 
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, "age_estim"])) {
      vector <- c()
      for (j in c('tronc_diam', 'haut_tronc', 'fk_stadedev','feuillage')) {
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
          haut_tronc = data[i, "haut_tronc"],
          fk_stadedev = data[i, "fk_stadedev"],
          feuillage = data[i, "feuillage"]

        )
        # Garder seulement les colonnes présentes dans le modèle
        new_data <- new_data[, vector, drop = FALSE]
        predicted_value <- predict(model, newdata = new_data)
        data[i, "age_estim"] <- max(round(predicted_value), 0)
      }
    }
  }
  data <- data[!is.na(data$age_estim), ]
  return(data)
}
cat("Nombre de valeurs manquantes dans la colonne 'age_estim' avant imputation : ", sum(is.na(data$age_estim)), "\n")
data <- replace_missing_age_estim(data)
cat("Nombre de valeurs manquantes dans la colonne 'age_estim' après imputation : ", sum(is.na(data$age_estim)), "\n\n")


impute_feuillage_simple <- function(data) {
  #' Remplace les valeurs manquantes de la variable 'feuillage' en utilisant
  #' les valeurs non manquantes de la même espèce
  #' @param data Le jeu de données contenant la variable 'feuillage
  #' @return Le jeu de données modifié avec les valeurs manquantes imputées
  #' 
  
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, "feuillage"])) {
      espece <- data[i, "nomfrancais"]
      feuillage <- data$feuillage[data$nomfrancais == espece & !is.na(data$feuillage)]
      if (length(feuillage) > 0) {
        data[i, "feuillage"] <- feuillage[1]
      }
    }
  }
    
  return(data)
}
cat("Nombre de valeurs manquantes dans la colonne 'feuillage' avant imputation : ", sum(is.na(data$feuillage)), "\n")
data <- impute_feuillage_simple(data)
cat("Nombre de valeurs manquantes dans la colonne 'feuillage' après imputation : ", sum(is.na(data$feuillage)), "\n\n")


rm(list=setdiff(ls(), "data"))



