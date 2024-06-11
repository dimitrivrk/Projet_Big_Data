source("main.R")

# green color gradient based on age
color_gradient <- colorRampPalette(c("#CCFFAA", "#115511"))
age_colors <- color_gradient(90)[data$age_estim]

# blue for remarquable trees
point_colors <- ifelse(data$remarquable, "blue", age_colors)

# red + cross symbol for dead trees
point_colors <- ifelse(data$fk_arb_etat == "en place", point_colors, "red")
etat_symbols <- ifelse(data$fk_arb_etat == "en place", 2, 4) # 2 for triangle up, 4 for cross

# remarquable trees are twice bigger
point_sizes <- ifelse(data$remarquable, 1, 0.5)

plot(
    data$X, data$Y,
    col = point_colors, pch = etat_symbols, cex = point_sizes,
    main = "Carte des arbres", xlab = "X", ylab = "Y"
)

legend(
    "topleft",
    legend = c("en place", "remarquable", "abbatu, ..."),
    col = c("green", "blue", "red"),
    pch = c(2, 2, 4),
    cex = 0.8
)
legend(
    "topright",
    legend = c("vieux", "jeune"),
    col = c("#CCFFAA", "#115511"),
    pch = c(2, 2)
)