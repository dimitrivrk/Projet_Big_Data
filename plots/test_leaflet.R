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

row_to_html_popup <- function (row){}

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
        radius = ~tronc_diam / 150 + 1,
        popup = ~paste("<b>", data_$OBJECTID, "</b><br><i>",
                       data_$nomfrancais, "</i><br>",
                       data_$age_estim, " ans<br>",
                       data$haut_tot, " de haut<br>",
                       data_$tronc_diam, " de diametre<br><i>",
                       data_$feuillage, "</i><br>",
                       ifelse(data_$fk_arb_etat == "en place", "", data_$fk_arb_etat))
    ) %>%
    addScaleBar("bottomleft")

map