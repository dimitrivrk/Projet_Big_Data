source("main.R")

# model_age <- lm(age_estim ~ tronc_diam + haut_tot + haut_tronc + fk_stadedev + feuillage, data = data)
# summary(model_age)

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