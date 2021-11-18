library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)

ui <- fluidPage(
  fileInput("dataFile", "Sélectionner le fichier RData (rds)",
            accept = c(".rds"),
            buttonLabel = "Cliquer ici",
            placeholder = "Aucun fichier sélectionné"),
  pickerInput("select_vars", "Sélectionner les variables à explorer",
              choices = NULL,
              multiple = T,
              options = list(`actions-box` = T)),
  pickerInput("select_vars_fill", "Sélectionner les variables à explorer (fill)",
              choices = NULL,
              multiple = T,
              options = list(`actions-box` = T)),
  textOutput("wd"),
  textInput("path", "Path"),
  fluidRow(actionButton("dl_corrs", "Downloader nuages de points"),
           actionButton("dl_histo", "Downloader histogrammes"),
           actionButton("dl_densite", "Downloader densité"),
           actionButton("dl_bars", "Downloader bar-graphs"))
)