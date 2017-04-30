library(shiny)
library(plyr)
library(Hmisc)
library(RColorBrewer)

load("./data/vgsales_rating.rda")
salesData <- sales.rating
salesData$Multi.Platform <- factor(salesData$Multi.Platform, labels = c("Single", "Multi"))
colnames(salesData)[8] <- "North.America"
colnames(salesData)[11] <- "Others"

load("./data/franchise.rda")
franchise <- franchise.list
series <- c("Pokemon", "Sonic", "Mario", "GTA", "Wii")

shinyServer(function(input, output) {

    mainCap <- reactive({
        input$variable2
    })

    franCap <- reactive({
        input$variable4
    })
    
    formulaText1 <- reactive({
        paste(input$variable2,"~", input$variable1)
    })

    formulaText2 <- reactive({
        if (input$variable1 != "Score") {
            paste("Score ~", input$variable1)
        }
        else paste("Score ~", input$variable2)
    })

    formulaText3 <- reactive({
        paste("Sales.millions ~", input$variable3)
    })

    selectData <- reactive({
        switch(input$variable4,
               Pokemon = ldply(franchise[1], data.frame),
               Sonic = ldply(franchise[2], data.frame),
               Mario = ldply(franchise[3], data.frame),
               GTA = ldply(franchise[4], data.frame),
               Wii = ldply(franchise[5], data.frame))
    })

    ## num <- reactive({
    ##     grep(input$variable4, series)
    ## })

    ## datasetInput <- reactive({
    ##     ldply(franchise[num()], data.frame)
    ## })
    
    output$title <- renderText({
        mainCap()
    })

    output$caption1 <- renderText({
        formulaText1()
    })

    output$caption2 <- renderText({
        formulaText2()
    })

    output$caption3 <- renderText({
        formulaText3()
    })

    ## v <- reactiveValues(doPlot = FALSE)
    ## observeEvent(input$do, {
    ##     v$doPlot <- input$do
    ## })
    
    output$salesPlot1 <- renderPlot({        
        boxplot(as.formula(formulaText1()),
                data = salesData,
                ylab = "Unit Sales (Millions)",
                col = brewer.pal(9, "BuGn"),
                outline = input$outliers)
    })

    output$salesPlot2 <- renderPlot({
        boxplot(as.formula(formulaText2()),
                data = salesData,
                ylab = "Ratings (out of 10)",
                col = brewer.pal(11, "Spectral"),
                outline = input$outliers)
    })

    output$summary <- renderPrint({
        describe(salesData)
    })
    
    output$table <- renderDataTable({
        sales.rating[, input$show_vars, drop = FALSE]
    })

    output$salesPlot3 <- renderPlot({
        boxplot(as.formula(formulaText3()),
                data = selectData(),
                ylab = "Unit Sales (Millions)",
                col = brewer.pal(9, "BuGn"))
    })

    output$summary2 <- renderPrint({
        describe(selectData())
    })

    output$table2 <- renderDataTable({
        selectData()
    })

})
