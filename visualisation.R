#import un autre fichier R
source("main.R")


#Analyse exploratoire 
#Histogramme de la hauteur totale
#hist(data$haut_tot, main = "Histogramme de la hauteur totale", xlab = "Hauteur totale", ylab = "Nombre d'arbres", col = "lightblue", border = "black")


#Fréquence des variables catégorielles
# Fréquence des variables catégorielles
#cat_vars <- c("X", "Y", "OBJECTID", "created_user", "src_geo", "clc_quartier", "clc_secteur", "id_arbre", "fk_arb_etat", "fk_stadedev", "fk_port", "fk_pied", "fk_situation", "commentaire_environnement", "fk_prec_estim", "clc_nbr_diag", "fk_nomtech", "last_edited_user", "villeca", "nomfrancais", "nomlatin", "GlobalID", "Creator", "Editor", "feuillage", "remarquable")


#Boxplot du diamètre du tronc
#boxplot(data$tronc_diam, main = "Boxplot du diamètre du tronc", xlab = "Diamètre du tronc", col = "lightblue", border = "black", horizontal = TRUE, range = 2)
#suprrime les valeur aberrantes a l'aide du boxplot
print(nrow(data[data$tronc_diam > 300, ]))
data <- data[data$tronc_diam < 300, ]


unique_quartiers <- unique(data$clc_quartier)
print(unique_quartiers)
# Boxplot de la hauteur totale par quartier
par(mar = c(5, 15, 4, 2))
boxplot(data$haut_tot ~ data$clc_quartier, 
    main = "Boxplot de la hauteur totale par quartier", 
    xlab = "Quartier", 
    ylab = "Hauteur totale",
    col = "lightblue", 
    border = "black",
    horizontal = TRUE,
    las = 1,
    range = 2)

# Valeurs de haut_tot par quartier

# #Créer des représentations graphiques

# #Exemple: répartition des arbres suivant leur stade de développement
# #barplot(table(data$fk_stadedev), main = "Répartition des arbres suivant leur stade de développement", xlab = "Stade de développement", ylab = "Nombre d'arbres", col = "lightblue", border = "black")



# # Créer un dictionnaire de couleurs pour chaque quartier avec la palette rainbow
# cat("Quartiers uniques :\n", paste(unique_quartiers, collapse = "\n"), "\n")

# colors <- rainbow(length(unique_quartiers))
# color_dict <- setNames(colors, unique_quartiers)

# # Créer un vecteur Y avec le nombre d'arbres pour chaque quartier
# Y <- table(data$clc_quartier)

# # Extraire les couleurs dans l'ordre des quartiers uniques
# colors_vector <- sapply(unique_quartiers, function(x) color_dict[[x]])

# # Trier les quartiers et les couleurs en fonction de Y
# sorted_quartiers <- names(sort(Y, decreasing = TRUE))
# sorted_colors <- sapply(sorted_quartiers, function(x) color_dict[[x]])
# sorted_Y <- sort(Y, decreasing = TRUE)

# # Ajuster les marges pour donner plus d'espace aux noms des quartiers
# max_label_length <- max(nchar(sorted_quartiers))
# par(mar = c(5, max(10, max_label_length * 0.7), 4, 2))

# Afficher un histogramme horizontal avec en x les noms des quartiers et en y le nombre d'arbres
# barplot(sorted_Y, 
#     names.arg = sorted_quartiers, 
#     main = "Quantité d'arbres pour chaque quartier", 
#     xlab = "Nombre d'arbres", 
#     col = sorted_colors, 
#     border = "black", las = 1, horiz = TRUE, cex.names = 1.8)

