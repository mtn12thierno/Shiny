library(shiny)
library(shinydashboard)
shinyApp(
  ui = dashboardPage(
    #l'en tête
    dashboardHeader(),
    #la partie gauche 
    dashboardSidebar(
      sidebarMenu(
        #Définir ce qui sera dans la partie gauche
        menuItem("Preparation des donnèes", tabName = "prepa", icon = icon("database"),
                 menuSubItem("base de données", tabName = "option1"),
                 menuSubItem("caractéristique des données", tabName = "option2")),
        menuItem("Traitement des données", tabName = "apu", icon = icon("dashboard")),
        menuItem("Analyse descriptive", tabName = "ad", icon = icon("pen")),
        menuItem("Graphique", tabName = "graph", icon = icon("book-open"))
      )
    ),
    #la partie droite principale
    dashboardBody(
      tabItems(
        tabItem(
          "option1",
            fileInput("file1", "Choisir un fichier CSV",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            selectInput("variable", "Sélectionner une variable", choices = NULL),
          dataTableOutput("table"),
      ),
      tabItem(
        "option2",
        selectInput("select", h3("Sélectionner votre variable"), 
                    choices = NULL, selected = NULL),
        verbatimTextOutput("summary"),
        plotOutput("boxplot"),
      )
     )
    ),
    title = "EHCVM",
    skin = "red" #Couleur
  ),
  server = function(input, output, session) {
    data <- reactive({
      infile <- input$file1
      if (is.null(infile)) {
        return(NULL)
      }
      read.csv2(infile$datapath)
    })
    
    observeEvent(input$file1, {
      updateSelectInput(session, "variable", choices = colnames(data()))
    })
    
    output$table <- renderDataTable({
      if (!is.null(input$variable) && length(colnames(data())) > 1) {
        data_subset <- data()[, c(1, match(input$variable, colnames(data()))), drop = FALSE]
        data_subset
      } else {
        data()
      }
    })
    observeEvent(input$file1, {
      updateSelectInput(session, "select", choices = colnames(data()), selected = colnames(data())[1])
    })
    output$summary <- renderPrint({
      selected_variable <- input$select
      summary(data()[[selected_variable]])
    })
    
    output$boxplot <- renderPlot({
      selected_variable <- input$select
      boxplot(data()[[selected_variable]], main = "Boxplot", ylab = selected_variable)
    })
  }
)
