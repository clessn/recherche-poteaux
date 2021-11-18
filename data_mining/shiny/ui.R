library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- fluidPage(
  fileInput("dataFile", "Sélectionner le fichier de données",
            accept = c(".csv", ".xlsx", ".rds"),
            buttonLabel = "Cliquer ici",
            placeholder = "Aucun fichier sélectionné"),
  varSelectInput("select_vars", "Sélectionner les variables à explorer",
                 data = data.frame(),
                 multiple = T)
)