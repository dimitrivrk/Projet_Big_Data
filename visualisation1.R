source("main.R")


plot_tree_distribution <- function(data) {
  #' Fonction qui permet de visualiser la répartition des arbres par quartier
  #' @param data le jeu de données
  #' @return un graphique de répartition des arbres par quartier
  #' 
  barplot(table(data$fk_stadedev), main = "Répartition des arbres suivant leur stade de développement", xlab = "Stade de développement", ylab = "Nombre d'arbres", col = "lightblue", border = "black")
}




plot_types_feuillage <- function(data) {
  #' Fonction qui permet de visualiser la répartition des types de feuillage des arbres
  #' @param data le jeu de données
  #' @return un graphique de répartition des types de feuillage des arbres
  #' 
  barplot(table(data$feuillage), main = "Répartition des types de feuillage", xlab = "Type de feuillage", ylab = "Nombre d'arbres", col = "lightblue", border = "black")
}

#Create a color dictionary for each quartier
color_dict <- c()
unique_quartiers <- unique(data$clc_quartier)
for (quartier in unique_quartiers) {
  color <- sample(colors(), 1)
  color_dict[quartier] <- color
}


plot_boxplot_tronc_diam <- function(data) {
  #' Fonction qui permet de visualiser le boxplot du diamètre du tronc des arbres
  #' @param data le jeu de données
  #' @return un graphique boxplot du diamètre du tronc des arbres
  #' 
  boxplot(data$tronc_diam, main = "Boxplot du diamètre du tronc", xlab = "Diamètre du tronc", col = "lightblue", border = "black")
}


plot_types_feuillage <- function(data) {
  #' Fonction qui permet de visualiser la répartition des types de feuillage des arbres
  #' @param data le jeu de données
  #' @return un graphique de répartition des types de feuillage des arbres
  #' 
  barplot(table(data$feuillage), main = "Répartition des types de feuillage", xlab = "Type de feuillage", ylab = "Nombre d'arbres", col = "lightblue", border = "black")
}



# # Créer des histogrammes
# # Exemple: Quantité d’arbres en fonction du quartier/secteur, de sa situation
create_histogram <- function(data, quartier) {
  #' Fonction qui crée un histogramme de la quantité d'arbres en fonction du quartier
  #' @param data le jeu de données
  #' @param quartier le quartier spécifié
  #' @return un graphique d'histogramme de la quantité d'arbres pour le quartier spécifié
  #' 
  hist(data$haut_tot[data$clc_quartier == quartier], main = "Quantité d'arbres en fonction du quartier", xlab = "Hauteur totale", ylab = "Nombre d'arbres", col = "lightblue", border = "black")
}



plot_arbre_quartier <- function(data, unique_quartiers){
  #' Fonction qui permet de visualiser la répartition des arbres par quartier
  #' @param data le jeu de données
  #' @param unique_quartiers les quartiers uniques
  #' @return un graphique de répartition des arbres par quartier
  #' 

  colors <- rainbow(length(unique_quartiers))
  color_dict <- setNames(colors, unique_quartiers)

  Y <- table(data$clc_quartier)

  colors_vector <- sapply(unique_quartiers, function(x) color_dict[[x]])

  sorted_quartiers <- names(sort(Y, decreasing = TRUE))
  sorted_colors <- sapply(sorted_quartiers, function(x) color_dict[[x]])
  sorted_Y <- sort(Y, decreasing = TRUE)

  max_label_length <- max(nchar(sorted_quartiers))
  par(mar = c(5, max(10, max_label_length * 0.7), 4, 2))

  barplot(sorted_Y, 
      names.arg = sorted_quartiers, 
      main = "Quantité d'arbres pour chaque quartier", 
      xlab = "Nombre d'arbres", 
      col = sorted_colors, 
      border = "black", las = 1, horiz = TRUE, cex.names = 1.8)
}