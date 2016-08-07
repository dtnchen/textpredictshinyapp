

# Read in data on app start
onegrams <- readRDS("onegrams_mixed0p04.rds")
twograms <- readRDS("twograms_mixed0p04.rds")
threegrams <- readRDS("threegrams_mixed0p04.rds")
fourgrams <- readRDS("fourgrams_mixed0p04.rds")

shinyServer(function(input, output,session) {
        
        #input$update
        # take the text input and return prediction and count estimates
        terms <- reactive({
                m <- textpredict(input$text)
                m <- as.data.frame(m)
                m
        })
        output$value <- renderPrint({ input$text })
        
        # Make the wordcloud drawing predictable during a session
        wordcloud_rep <- repeatable(wordcloud)
        output$plot <- renderPlot({
                v <- terms()
                words <- word(v$String,start=-1L, end=-1L)
                counts <- v$Count
                wordcloud_rep(words, counts, scale=c(3,1.0),
                              min.freq=1,max.words=20,rot.per=.01,
                              colors=brewer.pal(8, "Dark2"))
        })
        
})