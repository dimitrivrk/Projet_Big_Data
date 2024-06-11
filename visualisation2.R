source("main.R")

plot_carte <- function(){
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
}

plot_quartiers <- function(){
    # Generate a color vector based on clc_quartier
    unique_quartiers <- unique(data$clc_quartier)
    color_vector <- rainbow(length(unique_quartiers))  # Or use any other color palette

    # Map clc_quartier to colors
    color_map <- setNames(color_vector, unique_quartiers)
    point_colors <- color_map[data$clc_quartier]

    # create a layout with 2 regions
    layout(matrix(c(1, 2), nrow = 1), widths = c(4, 3))

    # Plot the data
    par(mar = c(5, 3, 5, 0))
    plot(
      data$X, data$Y,
      col = point_colors, pch = 2, cex = 0.1,
      main = "arbres par quartier", xlab = "X", ylab = "Y"
    )

    plot.new()
    legend(
      "center",
      legend = paste(unique_quartiers, " : ", as.character(table(data$clc_quartier))),
      col = color_vector,
      pch = 2,
      title = "nombre d'arbres par quartiers",
      cex = 0.8
    )
}

# plot_carte()
plot_quartiers()