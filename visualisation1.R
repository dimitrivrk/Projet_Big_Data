source("main.R")


#Exemple: répartition des arbres suivant leur stade de développement
#barplot(table(data$fk_stadedev), main = "Répartition des arbres suivant leur stade de développement", xlab = "Stade de développement", ylab = "Nombre d'arbres", col = "lightblue", border = "black")


#Créer des histogrammes
# Créer des histogrammes
# Exemple: Quantité d’arbres pour chaque quartier
#liste des quartiers
# Create a single plot for the quantity of trees in each quartier
# Create a color dictionary for each quartier
# Create a color dictionary for each quartier
color_dict <- c()
unique_quartiers <- unique(data$clc_quartier)
for (quartier in unique_quartiers) {
    color <- sample(colors(), 1)
    color_dict[quartier] <- color
}

# Set the color for each quartier in the histogram
par(mfrow = c(1, 1))
hist(, breaks = 10, col = color_dict[data$clc_quartier], border = "black", main = "Quantité d’arbres pour chaque quartier", xlab = "Hauteur totale", ylab = "Nombre d'arbres")
legend("topright", legend = unique(data$clc_quartier), fill = unique(color_dict[data$clc_quartier]), border = "black", bty = "n")

# Fréquence des variables catégorielles
#cat_vars <- c("X", "Y", "OBJECTID", "created_user", "src_geo", "clc_quartier", "clc_secteur", "id_arbre", "fk_arb_etat", "fk_stadedev", "fk_port", "fk_pied", "fk_situation", "commentaire_environnement", "fk_prec_estim", "clc_nbr_diag", "fk_nomtech", "last_edited_user", "villeca", "nomfrancais", "nomlatin", "GlobalID", "Creator", "Editor", "feuillage", "remarquable")
#en x je veux les quartier et en y le nombre de d'arbre dans chaque quartier


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
