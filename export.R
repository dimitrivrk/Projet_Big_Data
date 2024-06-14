source("main.R")

summary(data)

write.csv(data, "patrimoine_arbore_nettoye.csv", row.names = FALSE)