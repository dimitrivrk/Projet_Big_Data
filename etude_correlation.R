if (!exists("data", inherits = FALSE)){
    source("main.R")
}


library(corrplot)
library(ggplot2)
theme_set(theme_minimal())

plot_correlations <- function (){
    #' Fonction qui permet de visualiser les corrélations entre les variables numériques
    #' de notre jeu de données
    #' @param data le jeu de données
    #' @return un graphique de corrélation
    #' 
    data_cor <- data[, sapply(data, is.numeric)]
    data_cor <- subset(data_cor, select = -c(fk_prec_estim, OBJECTID))
    corrplot(cor(data_cor), method = 'number')
}

planter_arbre <- function (){
    #' Fonction qui permet de visualiser la répartition des arbres plantés
    #' en fonction de leur année de plantation
    #' @param data le jeu de données
    #' @return un graphique de répartition des arbres plantés
    #' 
    nb_arb_quartier = table(data$clc_quartier)
    df_quartiers = as.data.frame(nb_arb_quartier)
    colnames(df_quartiers) <- c("Quartier", "nb_arb")
    moy <- mean(df_quartiers$nb_arb)
    df_quartiers$harmonise <- ifelse(df_quartiers$nb_arb < moy, 1, 0)

    ggplot(df_quartiers, aes(x=nb_arb, y=harmonise)) + geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

    df_quartiers$harmonise <- ifelse(df_quartiers$nb_arb < moy, 1-(df_quartiers$nb_arb/moy)^2, 0)
    model_quartier_harmonie <- glm(harmonise ~ nb_arb , data = df_quartiers, family = quasibinomial(link = "logit"))

    ggplot(df_quartiers, aes(x=nb_arb, y=harmonise)) + geom_point() + ggtitle("harmonisation en fonction de la distance a la moyenne au carre") +
        geom_smooth(method = "glm", method.args = list(family = quasibinomial(link = "logit")), se = FALSE)
}

find_explicative_var_for_age <- function (){
    #' Fonction qui permet de trouver les variables explicatives pour l'âge estimé
    #' @param data le jeu de données
    #' @return les variables explicatives
    #' 
    model_age <- lm(age_estim ~ tronc_diam + haut_tronc + fk_stadedev + feuillage, data = data)
    summary(model_age)
}

# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)
# (Intercept)            9.787151   0.909309  10.763  < 2e-16 ***
# tronc_diam             0.173112   0.005711  30.311  < 2e-16 ***
# haut_tot              -0.082104   0.056343  -1.457  0.14509  !!!!!!!!!!!!
# haut_tronc             2.280099   0.152954  14.907  < 2e-16 ***
# fk_stadedevjeune     -13.186935   0.582102 -22.654  < 2e-16 ***
# fk_stadedevmort      -14.432354  15.709588  -0.919  0.35827   !!!!!!!!!!!
# fk_stadedevsenescent  30.944702   3.171176   9.758  < 2e-16 ***
# fk_stadedevvieux      10.222794   2.774429   3.685  0.00023 ***
# feuillagefeuillu       4.645203   0.609524   7.621 2.73e-14 ***


find_trees_to_cut <- function (){
    #' Fonction qui permet de trouver les arbres à abattre
    #' @param data le jeu de données
    #' @return les arbres à abattre
    #' 
    model_abattre <- glm(fk_arb_etat != "en place" ~ haut_tot + tronc_diam + fk_stadedev,
                      data = data,
                      family = binomial)
    summary(model_abattre)
    # haut_tot                 5.125e-02  1.331e-02   3.852 0.000117 ***
    # haut_tronc               6.723e-03  3.273e-02   0.205 0.837282
    # tronc_diam               2.308e-03  1.184e-03   1.949 0.051241 .
    # fk_stadedevjeune        -5.237e-01  1.729e-01  -3.028 0.002463 **
    # fk_stadedevmort          3.736e+01  2.130e+03   0.018 0.986009
    # fk_stadedevsenescent     1.289e-01  5.197e-01   0.248 0.804153
    # fk_stadedevvieux         3.477e+00  4.954e-01   7.018 2.25e-12 ***
    # on garde haut_tot, fk_stadedev et éventuellement tronc_diam

    # on fait le choix de ne pas abattre les arbres remarquables
    # et on n'essaie pas d'abattre un arbre déjà abattu :)
    test_data <- data[data$fk_arb_etat == "en place" & !data$remarquable, ]
    results <- predict(model_abattre, newdata = test_data, type = "response")
    plot(results)
    indexes <- which(results > 0.8)
    a_abbattre <- test_data[indexes, "OBJECTID"]
    print("OBJECTID des arbres a abattre")
    print(a_abbattre)   # 1480 1481 1483 6291 6963 6971 7263 7312
}
perform_chi2_tests <- function() {
    #' Fonction qui permet de réaliser des tests du chi2 et d'afficher les résultats sous forme de tableau
    #' @param data le jeu de données
    #' @return les résultats des tests
    #' 
    tab <- table(data$haut_tot, data$haut_tronc)
    print(tab)
    
    chi2_test <- chisq.test(tab)
    print(chi2_test)
    
    table_remarquable_feuillage <- table(data$remarquable, data$feuillage)
    
    chi2_test <- chisq.test(data$remarquable, data$feuillage)
    print(chi2_test)
    
    table_remarquable_fk_situation <- table(data$remarquable, data$fk_situation)
    print(table_remarquable_fk_situation)
    
    chi2_test <- chisq.test(data$remarquable, data$fk_situation)
    print(chi2_test)
    
    mosaicplot(table_remarquable_feuillage, main = "Relation entre remarquable et feuillage", cex.axis = 1.2)
    
    mosaicplot(table_remarquable_fk_situation, main = "Relation entre remaquable et fk_situation", cex.axis = 1.2)
}
