library(shiny)
library(tidyverse)

server <- function(session, input, output) {
  observe({
    Data <- data.frame(reactive(input$dataFile))
    updateVarSelectInput(session = session,
                         "select_vars", "Sélectionner les variables à explorer",
                         data = Data)
    })
}
