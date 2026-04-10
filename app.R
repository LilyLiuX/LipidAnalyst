source("global.R")
source("ui.R")
source("server.R")
# full stack call print out lines and file and nested reactive calls
options(shiny.fullstacktrace = TRUE)

shinyApp(ui = ui, server = server)

# runApp(display.mode = "showcase", launch.browser = TRUE)
