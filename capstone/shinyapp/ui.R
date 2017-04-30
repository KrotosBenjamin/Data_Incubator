library(shiny)

load("./data/vgsales_rating.rda")
salesData <- sales.rating

shinyUI(pageWithSidebar(
    headerPanel("Video Game Trends"),
    ## Variables to plot against Global Sales
    ## If possible, I would also like to separate out North America, Japan, Europe
    sidebarPanel(
        conditionalPanel(selectInput("variable1", "Variable:",
                                     list("Rating" = "Score",
                                          "Genre" = "Genre.y",
                                          "Platform" = "Platform",
                                          "Multi-platform" = "Multi.Platform",
                                          "Developer" = "Developer",
                                          "Generation" = "Generation",
                                          "Year" = "Year")),
                         selectInput("variable2","Region:",
                                     list("North America" = "North.America",
                                          "Europe" = "Europe",
                                          "Japan" = "Japan",
                                          "Others" = "Others",
                                          "World Wide" = "Global")),
                         
                         checkboxInput("outliers", "Show outliers", FALSE),
                         ## actionButton("do","Update View"),
                         helpText("Create interactive plots looking at video game unit sales (top) and ratings (bottom)."),
                         condition="input.conditionedPanels==1"
                         ),
        conditionalPanel(condition="input.conditionedPanels==2",
                         helpText("Descriptive summary of video games looking at 15 variables.")
                         ),
        conditionalPanel(condition="input.conditionedPanels==3",
                         checkboxGroupInput('show_vars', 'Columns:',
                                            names(salesData),
                                            selected = names(salesData))
                         ),
        conditionalPanel(condition="input.conditionedPanels==4",
                         selectInput("variable4", "Game Series:",
                                     list("Pokemon" = "Pokemon",
                                          "Mario" = "Mario",
                                          "Sonic the Hedgehog" = "Sonic",
                                          "Grand Theft Auto" = "GTA",
                                          "Wii Series" = "Wii")),
                         selectInput("variable3","Variable:",
                                     list("Rating" = "Score",
                                          "Genre" = "Genre",
                                          "Platform" = "Platform",
                                          "Series" = "ID",
                                          "Year" = "Year")),
                         helpText("Five select top franchise statistics all greater than 200 million unit sales.")
                         )
        ),
    mainPanel(
        tabsetPanel(
            tabPanel(
                "Overview",
                h2(textOutput("title"), align="center"),
                h3(textOutput("caption1"), align="center"),
                plotOutput("salesPlot1"),
                h3(textOutput("caption2"), align="center"),
                plotOutput("salesPlot2"),
                value=1),
            tabPanel(
                "Summary",
                verbatimTextOutput("summary"),
                value=2),
            tabPanel(
                "Table",
                dataTableOutput("table"),
                value=3),
            tabPanel(
                "Franchise",
                h2(textOutput("franCap"), align="center"),
                h3(textOutput("caption3"), align="center"),
                plotOutput("salesPlot3"),
                verbatimTextOutput("summary2"),
                dataTableOutput("table2"),
                value=4),
            id = "conditionedPanels"
        )
    )
))
    
