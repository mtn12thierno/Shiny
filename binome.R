library(shiny)
library(shinydashboard)
library(psych)
library(ggplot2)
library(haven)
library(dplyr)
library(foreign)
library(shinyFiles)


shinyApp(
  ui = dashboardPage(
    #l'en tête
    dashboardHeader(
      title="Statistique"
    ),
    #la partie gauche 
    dashboardSidebar(
      sidebarMenu(
        #Définir ce qui sera dans la partie gauche
        menuItem("préparation des données", tabName = "prepa", icon = icon("database"),
                 menuSubItem("base de données", tabName = "option1"),
                 menuSubItem("caractéristique des données", tabName = "option2")),
        menuItem("Analyse descriptive", tabName = "ad", icon = icon("pen"),
                 menuSubItem("Statistique univariée", tabName = "Des1"),
                 menuSubItem("Statistique bivariée", tabName = "Des2"),
                 menuSubItem("EHCVM", tabName = "ehcvm")),
        menuItem("Graphique", tabName = "graph", icon = icon("book-open"))
      )
    ),
    #la partie droite principale
    dashboardBody(
      tabItems(
        tabItem(
          "option1",
          div(class = "row",
              div(class = "col-sm-6",
                  fileInput("file1", "Choisir un fichier CSV",
                            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
              ),
              div(class = "col-sm-6",
                  selectInput("variable", "Sélectionner une variable", choices = NULL)
              )
          ),
          selectInput("distribution", "type de visualisation", 
                      choices = c( "toutes les observations", "tableau de distribution"), selected = "toutes les observations"),
          dataTableOutput("table"),
      ),
      tabItem(
        "option2",
        selectInput("select", "Sélectionner votre variable", 
                    choices = NULL, selected = NULL),
        verbatimTextOutput("summary"),
        plotOutput("boxplot"),
        textOutput("text")
      ),
      tabItem(
        "graph",
        div(class = "row",
            div(class = "col-sm-6",
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)),
        div(class="col-sm-6",
            textInput(inputId = "titre", label = "Title:", value = "Histogram"))),
        div(class = "row",
            div(class = "col-sm-6",
            selectInput(inputId = "color", label = "Couleur :",
                        choices = c("Red" = "red", "Green" = "green", "Blue" = "blue"))),
            div(class = "col-sm-6",
            selectInput("var1", "Sélectionner votre variable", 
                            choices = NULL, selected = NULL)),
            div(class = "col-sm-6",
              selectInput("var2", "Sélectionner la deuxième variable",
                            choices = c("rien", colnames(data())), selected = NULL))
                
        ),
        selectInput(inputId = "typeGraph", label = "Type de graphique",
                    choices = c("Histogramme" = "hist", "Diagramme en barres" = "bar", "Nuage de points" = "scatter")),
        plotOutput("distPlot"),
        div(textOutput("n_bins"), align = "center"),
        tags$hr(),
      ),
      tabItem(
        "Des1",
            selectInput("var_ad", "Choisir une variable :", choices = NULL, selected = NULL),
            actionButton("submit", "Analyser"),
        fluidRow(
          box(
         title = "statistiques descriptives ",
         tableOutput("output_ad"),
         width = 20
       ),
        ),
        tabsetPanel(
          tabPanel("Grpahique 1", plotOutput("plot1")),
          tabPanel("Grpahique 2", plotOutput("plot2")),
          tabPanel("Grpahique 3", plotOutput("plot3"))
        )
      ),
      tabItem(
        "Des2",
        div(class="row",
            div(class="col-sm-6",
                selectInput("var1_des2", "Sélectionner votre variable", 
                            choices = NULL, selected = NULL)),
            div(class="col-sm-6",
                selectInput("var2_des2", "Sélectionner votre variable", 
                            choices = NULL, selected = NULL))
       
      ),
      actionButton("submit_1", "Analyser"),
      uiOutput("tabs")
     ),
     tabItem(
       "ehcvm",
       box(
         title = "Revenu non agricole",
         width = 20,
         height = 40
       ),
       div(class = "row",
           div(class = "col-sm-6",
               fileInput("file2", "Sélectionner la base Stata de la section a", accept = ".dta")
           ),
           div(class = "col-sm-6",
           fileInput("file3", "Sélectionner la base Stata de la section b", accept = ".dta")
           )
       ),
       tabsetPanel(
         tabPanel("Apurement et variables d'intêret", 
                          actionButton("submit_ehcvm", "générer le tableau"),
                  div(class = "row",
                      div(class = "col-sm-6",
                          textInput("chemin", label = "chemin d'accés", value="C:/Users/user/Desktop")
                      ),
                      div(class = "col-sm-6",
                          actionButton("export_csv", "Exporter")
                      )
                  ),
                  dataTableOutput("table_ehcvm")
                  ),
         tabPanel("Revenu non agricole",
                  actionButton("submit_ehcvm1", "générer le tableau"),
                  div(class = "row",
                      div(class = "col-sm-6",
                          textInput("chemin1", label = "chemin d'accés", value="C:/Users/user/Desktop")
                      ),
                      div(class = "col-sm-6",
                          actionButton("export_csv1", "Exporter")
                      )
                  ),
                  dataTableOutput("table_ehcvm1")),
       )
      )
    )
    ),
    title = "EHCVM",
    skin = "red" #Couleur
  ),

  server = function(input, output, session) {
      options(shiny.maxRequestSize = 100*1024^2)  
    
    data <- reactive({
      infile <- input$file1
      if (is.null(infile)) {
        return(NULL)
      }
      read.csv2(infile$datapath)
    })
    
    data1 <- reactive({
      infile <- input$file2
      if (is.null(infile)) {
        return(NULL)
      }
      df <- read_dta(infile$datapath)
      return(df)
    })
    
    data_10a_subset <- reactive({
      data_subset <- data1()
      if (!is.null(data_subset)) {
        vars_of_interest <- c('interview__key', 's10q01', 's10q02', 's10q03', 's10q04', 's10q05',
                              's10q06', 's10q07', 's10q08', 's10q09', 's10q10') 
        return(data_subset[, vars_of_interest, drop = FALSE])
      } else {
        return(NULL)
      }
    })
    
    # Chargement complet de la base de données
    data2 <- reactive({
      infile <- input$file3
      if (is.null(infile)) {
        return(NULL)
      }
      df <- read_dta(infile$datapath)
      return(df)
    })
    
    data_10b_subset <- reactive({
      data_subset <- data2()
      if (!is.null(data_subset)) {
        vars_of_interest <- c('interview__key', 's10q12a', 's10q16', 's10q17a',
                              's10q17b', 's10q17c','s10q23', 's10q28', 's10q29', 's10q30', 's10q31', 's10q32',
                              's10q33', 's10q34', 's10q46', 's10q47', 's10q48', 's10q49', 's10q50', 's10q51',
                              's10q52', 's10q53', 's10q54', 's10q55', 's10q56', 's10q57') 
        return(data_subset[, vars_of_interest, drop = FALSE])
      } else {
        return(NULL)
      }
    })
    
    observeEvent(input$submit_ehcvm, {
      
      data_subset_10b <- data_10b_subset()
      
      if (!is.null(data_subset_10b)) {
        ## Renaming the variables
        new_names_10b <- c('interview_key', 'Activity', 'Product', 'Section_Code', 'Branch_Code', 'Activity_Code', 'Activity_local',
                           'Phone_number', 'Accounting', 'Taxation_phone', 'RC_Registered', 'Person_registered', 'Legal_form',
                           'Finance_source', 'Sold_product', 'Buy_product', 'Profit_product', 'Raw_materials_buy', 'Profit_services',
                           'Other_CI', 'Home_spending', 'Services_fees', 'Other_spending', 'Patente', 'Taxes', 'Admin_fees')
        
        colnames(data_subset_10b) <- new_names_10b
        
        variables_avant_sold_product <- c('interview_key', 'Activity', 'Product', 'Section_Code', 'Branch_Code', 'Activity_Code',
                                          'Activity_local', 'Phone_number', 'Accounting', 'Taxation_phone', 'RC_Registered',
                                          'Person_registered', 'Legal_form', 'Finance_source')
        
        variables_apres_sold_product <- c('Sold_product', 'Buy_product', 'Profit_product', 'Raw_materials_buy', 'Profit_services',
                                          'Other_CI', 'Home_spending', 'Services_fees', 'Other_spending', 'Patente', 'Taxes', 'Admin_fees')
        
        # Remplacer les valeurs manquantes par des chaînes vides pour les variables avant 'Sold_product'
        data_subset_10b <- data_subset_10b %>%
          mutate(across(all_of(variables_avant_sold_product), ~ ifelse(is.na(.), "", as.character(.))))
        
        # Remplacer les valeurs manquantes par des zéros pour les variables après 'Sold_product'
        data_subset_10b <- data_subset_10b %>%
          mutate(across(all_of(variables_apres_sold_product), ~ ifelse(is.na(.), 0, as.numeric(.))))
      }
      
      ## Now for the 10a base
      ## Renaming the variables
      new_names_10a <- c('interview_key', 'Demographics', 'Study_level', 'Marital_status', 'Social_status', 'Gender',
                         'Employment_status', 'Residence_type', 'Location_type', 'Location_size', 'Region', 'Year')
      
      # Replace the column names of data_10a_subset() with new_names_10a
      data_subset_10a=data_10a_subset()
      colnames(data_subset_10a) <- new_names_10a
      
      ## Merging the two datasets
      data_10 <- merge(data_subset_10b, data_subset_10a, by = 'interview_key', all.x = TRUE)
      data_10 <- data_10[complete.cases(data_10), ]
      
      output$table_ehcvm <- renderDataTable({
        data_10
      })
    
      observeEvent(input$export_csv, {
        if (!is.null(data_10)) {
           if(input$chemin!= ""){
             write.csv(data_10,paste0(input$chemin,"/EHCVM.csv"), row.names = FALSE)
             showNotification("La base de données a été exportée au format CSV.")
           } else {
             showNotification("veuillez mettre le chemin d'accés")
           }
          
        }
      })
      
    })
    
    observeEvent(input$submit_ehcvm1,{
      c('Sold_product', 'Buy_product', 'Profit_product', 'Raw_materials_buy', 'Profit_services', 'Other_CI', 'Home_spending', 'Services_fees', 'Other_spending', 'Patente', 'Taxes', 'Admin_fees')
      
      data_10$Revenu_Non_agricole = data_10$Sold_product - data_10$Buy_product + data_10$Profit_product - data_10$Raw_materials_buy +
        data_10$Profit_services - data_10$Other_CI - data_10$Home_spending - data_10$Services_fees -data_10$Other_spending - 
        data_10$Patente - data_10$Taxes - data_10$Admin_fees
      Revenu_menage <- aggregate(data_10$Revenu_Non_agricole, by = list(interview_key = data_10$interview_key), FUN = sum)
      colnames(Revenu_menage)=c("Id_ménage", "Revenu non agricole")
      output$table_ehcvm1 <- renderDataTable({
        Revenu_menage
      })
      
      observeEvent(input$export_csv1, {
        if (!is.null(Revenu_menage)) {
          if(input$chemin!= ""){
            write.csv(Revenu_menage,paste0(input$chemin,"/Revenu_non_agricole.csv"), row.names = FALSE)
            showNotification("La base de données a été exportée au format CSV.")
          } else {
            showNotification("veuillez mettre le chemin d'accés")
          }
          
        }
      })
    })
    
    observeEvent(input$file1, {
      updateSelectInput(session, "variable", choices = colnames(data()))
    })
    
    # Filtrage des données en fonction de la variable sélectionnée
    filtered_data <- reactive({
      distribution_selected <- input$distribution
      
      if (distribution_selected == "toutes les observations") {
        variable_selected <- input$variable
        if (!is.null(variable_selected) && variable_selected %in% colnames(data())) {
          data()[, c(1, match(variable_selected, colnames(data()))), drop = FALSE]
        } else {
          NULL
        }
      } else if (distribution_selected == "tableau de distribution") {
        variable_selected <- input$variable
        if (!is.null(variable_selected) && variable_selected %in% colnames(data())) {
          table_data <- table(data()[, variable_selected, drop = FALSE])
          data.frame(Modalités = names(table_data), Fréquence = as.vector(table_data))
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
    
    # Affichage de la table filtrée des données
    output$table <- renderDataTable({
      filtered_data()
    })
      
    observeEvent(input$file1, {
      updateSelectInput(session, "select", choices = colnames(data()), selected = colnames(data())[1])
    })
    output$summary <- renderPrint({
      selected_variable <- input$select
      summary(data()[[selected_variable]])
    })
    
    observeEvent(input$select, {
      selected_variable <- input$select
      if (!is.null(selected_variable) && is.numeric(data()[[selected_variable]])) {
        output$boxplot <- renderPlot({
          boxplot(data()[[selected_variable]], main = "Boxplot", ylab = selected_variable)
        })
      } else {
        output$boxplot <- renderPlot({})
      }
    })
    
    observeEvent(input$file1, {
      updateSelectInput(session, "var1", choices = colnames(data()))
    })
    
    observeEvent(input$file1, {
      updateSelectInput(session, "var2", choices = c("rien", colnames(data())))
    })
    
    observeEvent(c(input$var1, input$var2), {
      observeEvent(input$typeGraph, {
        output$distPlot <- renderPlot({
          selected_variable1 <- input$var1
          selected_variable2 <- input$var2
          if (selected_variable2 == "rien") {
            if (!is.null(selected_variable1) && is.numeric(data()[[selected_variable1]])) {
              if (input$typeGraph == "hist") {
                hist(data()[[selected_variable1]], main = input$titre, xlab = selected_variable1,
                     col = input$color, breaks = input$bins)
              } else if (input$typeGraph == "bar") {
                barplot(table(data()[[selected_variable1]]), main = input$titre, xlab = selected_variable1,
                        col = input$color)
              } else if (input$typeGraph == "scatter") {
                plot(data()[[selected_variable1]], main = input$titre, xlab = selected_variable1, ylab = "Valeur",
                     col = input$color)
              }
            } else {
              output$distPlot <- renderPlot({
                selected_variable <- input$var1
                  freq_table <- table(data()[[selected_variable]])
                  barplot(freq_table,
                          main = input$titre,
                          xlab = selected_variable,
                          ylab = "Fréquence",
                          col = input$color)})
            }
          } else {
            if (!is.null(selected_variable1) && !is.null(selected_variable2)) {
              if (is.numeric(data()[[selected_variable1]]) && is.numeric(data()[[selected_variable2]])) {
                if (input$typeGraph == "hist") {
                  hist(data()[[selected_variable1]], main = input$titre,
                       xlab = selected_variable1, ylab = selected_variable2, col = input$color)
                } else if (input$typeGraph == "bar") {
                  barplot(table(data()[[selected_variable1]], data()[[selected_variable2]]), main = input$titre,
                          xlab = selected_variable1, ylab = selected_variable2, col = input$color)
                } else if (input$typeGraph == "scatter") {
                  plot(data()[[selected_variable1]], data()[[selected_variable2]], main = input$titre,
                       xlab = selected_variable1, ylab = selected_variable2, col = input$color)
                  regression <- lm(data()[[selected_variable2]] ~ data()[[selected_variable1]])
                  regression <- lm(data()[[selected_variable2]] ~ data()[[selected_variable1]])
                  abline(regression, col = "red")
                  
                  # Calcul du coefficient de corrélation
                  correlation <- cor(data()[[selected_variable2]], data()[[selected_variable1]])
                  
                  # Affichage du coefficient de corrélation
                  legend("topright", legend = paste("Corrélation =", round(correlation, 2)), bg = "white")
                }
              } else {
                output$distPlot <- renderPlot({
                  selected_variable <- input$var1
                  freq_table <- table(data()[[selected_variable]])
                  barplot(freq_table,
                          main = input$titre,
                          xlab = selected_variable,
                          ylab = "Fréquence")})
              }
            }
          }
        })
      })
    })
    
    observeEvent(input$file1, {
      updateSelectInput(session, "var_recode", choices = c(colnames(data())))
    })
    
    observeEvent(input$file1, {
      updateSelectInput(session, "var_ad", choices = c(colnames(data())))
    })
    
    # Fonction pour générer le résumé de la variable
    generateSummary <- function(data, x) {
      variable_summary <- NULL
      
      if (is.numeric(data[[x]])) {
        variable_summary <- as.data.frame(psych::describe(data[[x]]))
      } else {
        variable_summary <- table(data[[x]])
      }
      
      return(variable_summary)
    }
    
    observeEvent(input$submit, {
      variable_selected <- input$var_ad
      
      if (!is.null(variable_selected)) {
        variable_summary <- generateSummary(data(), variable_selected)
        
        output$output_ad <- renderTable({
          variable_summary
        })
        if (is.numeric(data()[[variable_selected]])) {
          output$plot1 <- renderPlot({
            hist(data()[[variable_selected]], main = paste("Histogramme de", variable_selected), xlab = variable_selected, col = "lightblue")
          })
          
          output$plot2 <- renderPlot({
            plot(data()[[variable_selected]], main = paste("Nuage de points de", variable_selected), xlab = variable_selected, ylab = "Valeurs")
          })
          
          output$plot3 <- renderPlot({
            boxplot(data()[[variable_selected]], main = "Boxplot", ylab = variable_selected)
          })
          
        } else {
          freq_table <- table(data()[[variable_selected]])
          output$plot1 <- renderPlot({
            barplot(freq_table,
                    xlab = variable_selected,
                    ylab = "Fréquence",
                    col = "red")})
          
          output$plot3<- NULL
          variable_summary <- table(data()[[variable_selected]])
            output$plot2 <- renderPlot({
              proportions <- prop.table(variable_summary)
              pie_chart <- pie(proportions, main = "Répartition de la variable")
              print(pie_chart)
            })
        }
      }
    })
    
   
    observeEvent(input$file1, {
      updateSelectInput(session, "var1_des2", choices = c(colnames(data())))
    })
    observeEvent(input$file1, {
      updateSelectInput(session, "var2_des2", choices = c(colnames(data())))
    })
    
    observeEvent(c(input$var1_des2, input$var2_des2), {
      
      var1 <- data()[[input$var1_des2]]
      var2 <- data()[[input$var2_des2]]
      # Afficher ou cacher les onglets en fonction des conditions
      output$tabs <- renderUI({
        if (is.numeric(var1)==FALSE && is.numeric(var2)==FALSE) {
          # Afficher les onglets pour les variables qualitatives
          tabsetPanel(
            tabPanel("Contingency Table", uiOutput("contingencyTable")),
            tabPanel("Statistical Results", uiOutput("statResults"))
          )
        } else if (is.numeric(var1)==TRUE && is.numeric(var2)==TRUE) {
          # Afficher les onglets pour les variables quantitatives
          tabsetPanel(
            tabPanel("Scatter Plot", plotOutput("scatterPlot")),
            tabPanel("Statistical Results", uiOutput("statResults"))
          )
        } else if((is.numeric(var1)==TRUE && is.numeric(var2)==FALSE)||(is.numeric(var1)==FALSE && is.numeric(var2)==TRUE)) {
          tabsetPanel(
            tabPanel("Histogramme", plotOutput("histogram")),
            tabPanel("Statistical Results", uiOutput("statResults"))
          )
        }
      })
    })
    
    observeEvent(input$submit_1, {
      var1 <- data()[[input$var1_des2]]
      var2 <- data()[[input$var2_des2]]
      
      if (is.numeric(var1)==FALSE && is.numeric(var2)==FALSE) {
        
        contingencyTable <- table(var1, var2)
        correlationTests <- chisq.test(contingencyTable)
        
        correlationTests <- data.frame(
          Khi_deux = correlationTests$statistic,
          Degrés_libérté = correlationTests$parameter,
          pValue = correlationTests$p.value
        )
        
        output$contingencyTable <- renderTable(with(data(), table(get(input$var1_des2),get(input$var2_des2))))
        output$statResults <- renderTable(correlationTests)
        output$scatterPlot <- NULL
        output$histogram <- NULL
      } else if (is.numeric(var1) && is.numeric(var2)) {
        scatterPlot <- ggplot(data = data(), aes(x = var1, y = var2)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE) +
          labs(x = input$var1_des2, y = input$var2_des2)
        
        correlation <- cor(var1, var2)
        
        output$scatterPlot <- renderPlot({ scatterPlot })
        output$statResults <- renderTable(correlation)
        output$contingencyTable <- NULL
        output$histogram <- NULL
      } else if (is.numeric(var1)==FALSE && is.numeric(var2)==TRUE) {
        data <- data.frame(var1 = var1, var2 = var2)
        
        histogram <- ggplot(data = data, aes(x = var2, fill = var1)) +
          geom_histogram(position = "identity", bins = 30, alpha = 0.5) +
          labs(x = input$var2_des2, y = "Count")
        
        descriptives <- data %>%
          group_by(var1) %>%
          summarise(mean = mean(var2), median = median(var2), sd = sd(var2))
        
        output$histogram <- renderPlot({ histogram })
        output$statResults <- renderTable(descriptives)
        output$scatterPlot <- NULL
        output$contingencyTable <- NULL
      }  else if (is.numeric(var1)==TRUE && is.numeric(var2)==FALSE) {
        data <- data.frame(var2 = var2, var1 = var1)
        
        histogram <- ggplot(data = data, aes(x = var1, fill = var2)) +
          geom_histogram(position = "identity", bins = 30, alpha = 0.5) +
          labs(x = input$var1_des2, y = "Count")
        
        descriptives <- data %>%
          group_by(var2) %>%
          summarise(mean = mean(var1), median = median(var1), sd = sd(var1))
        
        output$histogram <- renderPlot({ histogram })
        output$statResults <- renderTable(descriptives)
        output$scatterPlot <- NULL
        output$contingencyTable <- NULL
      }
    })
 
  }
)
