shinyUI(fluidPage(
        titlePanel('Pre-Text'),
        sidebarLayout(
                # Sidebar with a slider and selection inputs
                sidebarPanel(
                        textInput("text", label = h3("Text input"), value = "Enter text..."),
                        #actionButton("update", "Submit")
                        hr(),
                        fluidRow(column(10, verbatimTextOutput("value")))
                ),
                #Show word cloud
                mainPanel(
                        plotOutput('plot')
                )
        )
))