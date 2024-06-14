if (!exists("data", inherits = FALSE)){
    source("main.R")
}

export_csv <- function (){
    summary(data)
    write.csv(data, "patrimoine_arbore_nettoye.csv", row.names = FALSE)
}
