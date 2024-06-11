source("main.R")
library(leaflet)
library(sf)

transform_coord <- function(){
    # Create an sf object from the data.frame
    sf_data <- st_as_sf(data, coords = c("X", "Y"))

    # Set the coordinate reference system to EPSG:3949
    st_crs(sf_data) <- 3949

    # Transform coordinates to EPSG:4326
    sf_data_4326 <- st_transform(sf_data, crs = 4326)

    return(sf_data_4326)
}

data_ <- transform_coord()

color_gradient <- colorRampPalette(c("#CCFFAA", "#115511"))
age_colors <- color_gradient(200)[data$age_estim]

map <- leaflet(data_) %>%
    addTiles() %>%
    addCircles(
        color = ~ifelse(
            fk_arb_etat == "en place",
            ifelse(
                data_$remarquable,
                "blue",
                color_gradient(200)[data$age_estim]
                ),
            "red"
        ),
        fillOpacity = 1,
        stroke = FALSE,
        radius = ~tronc_diam *2 / 100 +1,
        popup = ~paste(data_$nomfrancais, "<br>", data_$age_estim, " ans<br>", data$haut_tot, " de haut<br>", data_$tronc_diam, " de diametre")
    )

map