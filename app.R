library(shiny)
library(readr)
library(tidyverse)
library(DT)

df <- read_csv("ObesityDataSet_raw_and_data_sinthetic.csv")
df =
  df |> mutate(NObeyesdad = factor(NObeyesdad,
                                   levels = c("Insufficient_Weight",
                                              "Normal_Weight",
                                              "Overweight_Level_I",
                                              "Overweight_Level_II",
                                              "Obesity_Type_I",
                                              "Obesity_Type_II",
                                              "Obesity_Type_III"),
                                   ordered = TRUE),
               across(c(FCVC,NCP,CH2O,FAF,TUE),round),
               across(c(FCVC,NCP,CH2O,FAF,TUE),as.factor))
                 
variables_factor = names(df)[-(2:4)]
variables_numericas = names(df)[2:4]


ui = navbarPage(
  title = "Explorador de datos - Obesidad",
  
  tabPanel("Inicio",
           h2("Bienvenido/a"),
           p("Esta aplicación permite explorar el dataset de obesidad, cruzando
              variables numéricas y categóricas, y calcular tu propio IMC."),
           p("Usá la pestaña 'Exploración' para explorar el dataset,
              o 'Calculadora IMC' para calcular tu índice de masa corporal.")
  ),
  
  tabPanel("Base de datos",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("NObeyesdad_base", "Categoría",
                                  choices = levels(df$NObeyesdad),
                                  selected = levels(df$NObeyesdad))
             ),
             mainPanel(
               DTOutput("tabla")
             )
           )
  ),
  
  tabPanel("Exploración",
           sidebarLayout(
             sidebarPanel(
               radioButtons("tipo_grafico", "Tipo de análisis:",
                            choices = c("Una variable numérica" = "num",
                                        "Una variable categórica" = "cat",
                                        "Dos númericas" = "2num",
                                        "Dos categóricas" = "2cat",
                                        "Numérica según categoría" = "cruzado"),
                            selected = "num"),
               
               conditionalPanel(
                 condition = "input.tipo_grafico == 'num'",
                 selectInput("var_num", "Variable numérica:", choices = variables_numericas)
               ),
               
               conditionalPanel(
                 condition = "input.tipo_grafico == 'cat'",
                 selectInput("var_cat", "Variable categórica:", choices = variables_factor)
               ),
               
               conditionalPanel(
                 condition = "input.tipo_grafico == '2num'",
                 selectInput("var_num1", "Variable númerica 1:", choices = variables_numericas),
                 selectInput("var_num2", "Variable númerica 2:", choices = variables_numericas)
               ),
               
               conditionalPanel(
                 condition = "input.tipo_grafico == '2cat'",
                 selectInput("var_cat1", "Variable categórica 1:", choices = variables_factor),
                 selectInput("var_cat2", "Variable categórica 2:", choices = variables_factor)
               ),
               
               conditionalPanel(
                 condition = "input.tipo_grafico == 'cruzado'",
                 selectInput("var_num_c", "Variable númerica:", choices = variables_numericas),
                 selectInput("var_cat_c", "Variable categórica:", choices = variables_factor)
               ),

               checkboxGroupInput("NObeyesdad_exp", "Categoría",
                                  choices = levels(df$NObeyesdad),
                                  selected = levels(df$NObeyesdad))
             ),
             
             mainPanel(
               tabsetPanel(
                 id = "tabs",
                 tabPanel("Gráfico",
                          radioButtons("segun_categoria", "Elegir si colorear por categoria",
                                       choices = c("General" = "gral",
                                                   "Coloreado" = "col"),
                                       selected = "gral"),
                          conditionalPanel(
                            condition = "input.tipo_grafico == 'num' && input.segun_categoria == 'col'",
                            sliderInput("alpha", "Transparencia:", min = 0.1, max = 1, value = 0.8)
                          ),
                          conditionalPanel(
                            condition = "input.tipo_grafico == 'cat' && input.segun_categoria == 'col'",
                            radioButtons("eje_y", "Elegir variable del eje y",
                                         choices = c("Cantidad" = "cant",
                                                     "Proporción" = "prop"),
                                         selected = "cant")
                          ),
                          plotOutput("bar")),
                 tabPanel("Resumen", verbatimTextOutput("resumen"))
               )
             )
           )
  ),
  
  tabPanel("Calculadora IMC",
           br(),
           numericInput("peso", "Peso (kg):", value = 70, min = 1, max = 300),
           numericInput("altura", "Altura (m):", value = 1.70, min = 0.5, max = 2.5, step = 0.01),
           h3("Resultado:"),
           textOutput("resultado_imc"),
           textOutput("categoria_imc"))
)

server = function(input, output) {
  
  datos_filtrados_base = reactive({
    req(input$NObeyesdad_base)
    df[df$NObeyesdad %in% input$NObeyesdad_base, ]
  })
  
  datos_filtrados_exp = reactive({
    req(input$NObeyesdad_exp)
    df[df$NObeyesdad %in% input$NObeyesdad_exp, ]
  })
  
  output$bar = renderPlot({
    
    if (input$tipo_grafico == "num") { # Elige variable númerica
      if (input$segun_categoria == "gral") { # y elige colorear general
        
          ggplot(datos_filtrados_exp(),
               aes(x = .data[[input$var_num]])) +
          geom_density() +
          theme_minimal() +
          theme(legend.position = "none")
      } else {                           # o elige colorear por NObeyesdad
          ggplot(datos_filtrados_exp(),
               aes(x = .data[[input$var_num]], fill = NObeyesdad)) +
          geom_density(alpha = input$alpha) +
          theme_minimal()
      }
    } else if (input$tipo_grafico == "cat") { # Elige variable categorica
      
      if (input$segun_categoria == "gral") { # y elige colorear general
        
          ggplot(datos_filtrados_exp(),
               aes(x = .data[[input$var_cat]], fill = .data[[input$var_cat]])) +
          geom_bar() +
          labs(y = "Cantidad") +
          theme_minimal() + 
          theme(legend.position = "none")
      } else {                             # o elige colorar por NObeyesdad
          if (input$eje_y == "cant") {        # y elige se vean las cantidad
              ggplot(datos_filtrados_exp(),
                   aes(x = .data[[input$var_cat]], fill = NObeyesdad)) +
              geom_bar() +
              labs(y = "Cantidad") +
              theme_minimal()
          } else {                            # o que se vean las proporciones
              ggplot(datos_filtrados_exp(),
                   aes(x = .data[[input$var_cat]], fill = NObeyesdad)) +
              geom_bar(position = "fill") +
              labs(y = "Proporción") +
              theme_minimal()
          }
      }
    } else if (input$tipo_grafico == "2num") { # Elige dos variables numericas
        
        ggplot(datos_filtrados_exp(),
               aes(x = .data[[input$var_num1]], y = .data[[input$var_num2]])) +
        geom_point() +
        theme_minimal()
    
    } else if (input$tipo_grafico == "2cat") { # Elige dos variables categoricas
        ggplot(datos_filtrados_exp(),
           aes(x = .data[[input$var_cat1]], fill = .data[[input$var_cat2]])) +
        geom_bar(position = "dodge") +
        theme_minimal()
    
    } else if (input$tipo_grafico == "cruzado") { # Elige una numerica y una categorica
      ggplot(datos_filtrados_exp(),
             aes(x = .data[[input$var_cat_c]], y = .data[[input$var_num_c]])) +
        geom_boxplot() +
        theme_minimal()
    } 
  })
  
  output$resumen = renderPrint({
    datos = datos_filtrados_exp()
    
    if (input$tipo_grafico == "num") {
      summary(datos[[input$var_num]])
    
    } else if (input$tipo_grafico == "cat") {
        addmargins(table(datos$NObeyesdad, datos[[input$var_cat]]))
    
    } else if (input$tipo_grafico == "2num") {
        cov(datos[[input$var_num1]], datos[[input$var_num2]])
        cor(datos[[input$var_num1]], datos[[input$var_num2]])
    
    } else if (input$tipo_grafico == "2cat") {
        table(datos[[input$var_cat1]], datos[[input$var_cat2]]) |> addmargins()
    
    } else if (input$tipo_grafico == "cruzado") {
      tapply(datos[[input$var_num_c]],
             datos[[input$var_cat_c]],
             summary)
    }
  })
  
  output$tabla = renderDT({
    datos_filtrados_base()
  })
  
  imc = reactive({
    req(input$peso, input$altura)
    input$peso / (input$altura^2)
  })
  
  output$resultado_imc = renderText({
    paste("Tu IMC es:", round(imc(), 2))
  })
  
  output$categoria_imc = renderText({
    valor = imc()
    categoria = case_when(
      valor < 18.5 ~ "Insufficient_Weight (Bajo peso)",
      valor < 25.0 ~ "Normal_Weight (Peso normal)",
      valor < 27.0 ~ "Overweight_Level_I (Sobrepeso Nivel 1)",
      valor < 30.0 ~ "Overweight_Level_II (Sobrepeso Nivel 2)",
      valor < 35.0 ~ "Obesity_Type_I (Obesidad Tipo 1)",
      valor < 40.0 ~ "Obesity_Type_II (Obesidad Tipo 2)",
      TRUE ~ "Obesity_Type_III (Obesidad Tipo 3 o mórbida)"
    )
    paste("Categoría:", categoria)
  })
}

shinyApp(ui, server)

