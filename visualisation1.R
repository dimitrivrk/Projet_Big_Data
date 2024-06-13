source("main.R")


#Exemple: répartition des arbres suivant leur stade de développement
#barplot(table(data$fk_stadedev), main = "Répartition des arbres suivant leur stade de développement", xlab = "Stade de développement", ylab = "Nombre d'arbres", col = "lightblue", border = "black")


#affiche la Répartition des types de feuillage
#barplot(table(data$feuillage), main = "Répartition des types de feuillage", xlab = "Type de feuillage", ylab = "Nombre d'arbres", col = "lightblue", border = "black")

#Create a color dictionary for each quartier
color_dict <- c()
unique_quartiers <- unique(data$clc_quartier)
for (quartier in unique_quartiers) {
  color <- sample(colors(), 1)
  color_dict[quartier] <- color
}

# Fréquence des variables catégorielles
#cat_vars <- c("X", "Y", "OBJECTID", "created_user", "src_geo", "clc_quartier", "clc_secteur", "id_arbre", "fk_arb_etat", "fk_stadedev", "fk_port", "fk_pied", "fk_situation", "commentaire_environnement", "fk_prec_estim", "clc_nbr_diag", "fk_nomtech", "last_edited_user", "villeca", "nomfrancais", "nomlatin", "GlobalID", "Creator", "Editor", "feuillage", "remarquable")
#en x je veux les quartier et en y le nombre de d'arbre dans chaque quartier


#Boxplot du diamètre du tronc
#boxplot(data$tronc_diam, main = "Boxplot du diamètre du tronc", xlab = "Diamètre du tronc", col = "lightblue", border = "black")


#Répartition des types de feuillage
#barplot(table(data$feuillage), main = "Répartition des types de feuillage", xlab = "Type de feuillage", ylab = "Nombre d'arbres", col = "lightblue", border = "black")


# # Créer des représentations graphiques
# # Exemple: répartition des arbres suivant leur stade de développement
# # Calculer les fréquences des niveaux du facteur
# freq_table <- table(data$fk_stadedev)

# # Réorganiser les niveaux du facteur selon les fréquences
# sorted_levels <- names(sort(freq_table, decreasing = TRUE))

# # Réorganiser les données en utilisant les niveaux triés
# data$fk_stadedev <- factor(data$fk_stadedev, levels = sorted_levels)

# # Créer le barplot avec les niveaux réorganisés
# barplot(table(data$fk_stadedev), 
#         main = "Répartition des arbres suivant leur stade de développement", 
#         xlab = "Stade de développement", 
#         ylab = "Nombre d'arbres", 
#         col = "lightblue", 
#         border = "black")

# # Créer des histogrammes
# # Exemple: Quantité d’arbres en fonction du quartier/secteur, de sa situation
# hist(data$haut_tot[data$clc_quartier == "quartier du centre-ville"], main = "Quantité d’arbres en fonction du quartier", xlab = "Hauteur totale", ylab = "Nombre d'arbres", col = "lightblue", border = "black")






# Créer un dictionnaire de couleurs pour chaque quartier avec la palette rainbow
cat("Quartiers uniques :\n", paste(unique_quartiers, collapse = "\n"), "\n")

colors <- rainbow(length(unique_quartiers))
color_dict <- setNames(colors, unique_quartiers)

# Distribution des arbres par quartier
barplot(table(data$clc_quartier), main = "Distribution des arbres par quartier", xlab = "Quartier", ylab = "Nombre d'arbres", col = "lightblue", border = "black")
# Créer un vecteur Y avec le nombre d'arbres pour chaque quartier
Y <- table(data$clc_quartier)

# Répartition des types de feuillage
barplot(table(data$feuillage), main = "Répartition des types de feuillage", xlab = "Type de feuillage", ylab = "Nombre d'arbres", col = "lightblue", border = "black")
# Extraire les couleurs dans l'ordre des quartiers uniques
colors_vector <- sapply(unique_quartiers, function(x) color_dict[[x]])

# Trier les quartiers et les couleurs en fonction de Y
sorted_quartiers <- names(sort(Y, decreasing = TRUE))
sorted_colors <- sapply(sorted_quartiers, function(x) color_dict[[x]])
sorted_Y <- sort(Y, decreasing = TRUE)

# Créer des représentations graphiques
# Exemple: répartition des arbres suivant leur stade de développement
barplot(table(data$fk_stadedev), main = "Répartition des arbres suivant leur stade de développement", xlab = "Stade de développement", ylab = "Nombre d'arbres", col = "lightblue", border = "black")
# Ajuster les marges pour donner plus d'espace aux noms des quartiers
max_label_length <- max(nchar(sorted_quartiers))
par(mar = c(5, max(10, max_label_length * 0.7), 4, 2))

# Afficher un histogramme horizontal avec en x les noms des quartiers et en y le nombre d'arbres
barplot(sorted_Y, 
    names.arg = sorted_quartiers, 
    main = "Quantité d'arbres pour chaque quartier", 
    xlab = "Nombre d'arbres", 
    col = sorted_colors, 
    border = "black", las = 1, horiz = TRUE, cex.names = 1.8)