library(shiny)
library(readr)
library(tidyverse)
library(knitr)
library(plotly)
library(DT)

df <- read_csv("ObesityDataSet_raw_and_data_sinthetic.csv")

# Redondeamos variables númericas (que tienen valores con coma por los datos sintéticos) para que sean factores
 # y no haya problema en los gráficos.
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
               across(where(is.character), as.factor),
               across(c(FCVC,NCP,CH2O,FAF,TUE),round),
               across(c(FCVC,NCP,CH2O,FAF,TUE),as.factor)) 
                 
variables_factor = names(df)[-(2:4)]
variables_numericas = names(df)[2:4]

# Random Forest
set.seed(1)
split = initial_split(df, prop = 0.8, strata = NObeyesdad)
train_df = training(split)
test_df = testing(split)
receta_rf2 <- recipe(NObeyesdad ~ ., data = train_df) |> 
  step_rm(Weight, Height, Age, Gender)

rforest2 = 
  rand_forest(trees = 1000) |> 
  set_engine("ranger", importance = "impurity") |> 
  set_mode("classification") 

wf_rforest2 = 
  workflow() |>
  add_recipe(receta_rf2) |> 
  add_model(rforest2) |> 
  fit(data = train_df)

ui = navbarPage(
  title = "Explorador de datos - Obesidad",
  # Formato de la barra superior y de los paneles
  header = tags$head(
    tags$style(HTML("
    .navbar-default { background-color: #AEC6CF !important; }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: #2c3e50 !important; }
    
    .well { 
      background-color: #F0F8FF !important; 
      border: 1px solid #AEC6CF !important; 
    }
  "))
  ),
  # --- Primer pestaña --- #
  tabPanel("Inicio",
           h2("Proyecto final de Ciencia de Datos con R 2026", 
              style = "font-weight: bold; text-decoration: underline;"),
           h4("Franco Purtscher, Emiliano Santestevan, Ignacio Tarigo")
           wellPanel(
             h3("Descripción del proyecto"),
             p("Esta Shiny está basada en una base de datos extraída de UCI Machine Learning,
               con la cual intentaremos explicar el nivel de obesidad de un individuo,
               basándonos en hábitos alimenticios, condición física, historial familiar, y demás variables.",
               style = "font-size: 16px;"),
             p("Creemos que es un fenómeno interesante de abordar, debido a que determinados niveles de obesidad
               pueden llegar a ser perjudiciales para la salud de una persona. A través de este análisis intentaremos
               conocer ciertos motivos o patrones que podrían permitir la detección o la prevención de ese riesgo.",
               style = "font-size: 16px;"),
             
             hr(),
             
             h4("Guía de la aplicación:"),
             tags$ul(style = "font-size: 18px;",
                     tags$li("En la pestaña 'Base de datos' podrás acceder al dataset y a información sobre él."),
                     tags$li("En la pestaña 'Exploración', verás cómo se relacionan cada una de las variables del dataset."),
                     tags$li("En la pestaña 'Calculadora IMC', podrás ingresar tu altura y peso para obtener tu IMC."),
                     tags$li("En la pestaña 'Modelo predictivo', podrás responder preguntas para que el modelo prediga tu categoría.")
             )
           )
  ),
   # --- Segunda pestaña --- #
  tabPanel("Base de datos",
           sidebarLayout(
             sidebarPanel(
               p("La base de datos está basada en encuestas web a individuos de México, Perú y Colombia.
               El 23% de las observaciones se recopilaron directamente desde las encuestas,
               mientras que el 77% fueron creados sinteticamente utilizando la herramienta Woka y el filtro SMOTE,
                 con el fin de equiparar la cantidad de datos en cada categoría y así
                 los distintos algoritmos que se pueden llegar a utilizar con el dataframe funcionen de una mejor manera.
                 Contiene 2111 observaciones y 17 variables",
                 style = "font-size: 16px;"),
               wellPanel(
                 h4("Glosario de variables"),
                 tags$div(style = "font-size: 15px;",
                          p(strong("FAVC:"), " Consumo frecuente de alimentos calóricos."),
                          p(strong("FCVC:"), " Frecuencia de consumo de vegetales."),
                          p(strong("NCP:"), " Número de comidas principales."),
                          p(strong("CAEC:"), " Consumo de alimentos entre comidas."),
                          p(strong("SMOKE:"), " Si fuma."),
                          p(strong("CH2O:"), " Consumo de agua diario."),
                          p(strong("SCC:"), " Monitoreo de calorías consumidas."),
                          p(strong("FAF:"), " Frecuencia de actividad física."),
                          p(strong("TUE:"), " Tiempo de uso de dispositivos tecnológicos."),
                          p(strong("CALC:"), " Consumo de alcohol."),
                          p(strong("MTRANS:"), " Medio de transporte utilizado.")
                 )
               ),
             ),
             mainPanel(
               DTOutput("tabla"),
               h3("La categoría de NObeyesdad, esta basada en el valor del IMC"),
               h3("Puede encontrar toda la información de la base de datos en: https://www.sciencedirect.com/science/article/pii/S2352340919306985?via%3Dihub")
             )
           )
  ),
  
  # --- Tercer pestaña --- #
  tabPanel("Exploración",
           sidebarLayout(
             sidebarPanel(
               # Elige el tipo de variables que quiere observar
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
                 selectInput("var_cat_c", "Variable categórica:", choices = variables_factor[-length(variables_factor)])
               ),

               checkboxGroupInput("NObeyesdad_exp", "Categoría",
                                  choices = levels(df$NObeyesdad),
                                  selected = levels(df$NObeyesdad))
             ),
             
             mainPanel(
               tabsetPanel(
                 id = "tabs",
                 # Primer pestaña dentro de Exploración
                 tabPanel("Gráfico",
                          # Se condicionan distintos aspectos del gráfico según las variables que eligen
                          conditionalPanel(
                            condition = "input.tipo_grafico != '2cat'",
                            radioButtons("coloreado", "Coloreado",
                                       choices = c("Normal" = "gral",
                                                   "Por categoria" = "col"),
                                       selected = "gral")
                            ),
                          conditionalPanel(
                            condition = "(input.tipo_grafico == 'num' || input.tipo_grafico == '2num') && input.coloreado == 'col'",
                            sliderInput("alpha", "Transparencia:", min = 0.1, max = 1, value = 0.8)
                          ),
                          conditionalPanel(
                            condition = "input.tipo_grafico == 'cat' && input.coloreado == 'col'",
                            radioButtons("eje_y", "Variable del eje y",
                                         choices = c("Cantidad" = "cant",
                                                     "Proporción" = "prop"),
                                         selected = "cant")
                          ),
                          plotOutput("grafico")),
                 # Segunda pestaña dentro de Exploración
                 tabPanel("Resumen",
                          uiOutput("titulo_resumen"),
                          verbatimTextOutput("resumen"))
               )
             )
           )
  ),
  # --- Cuarta pestaña --- #
  tabPanel("Calculadora IMC",
           sidebarLayout(
             sidebarPanel(numericInput("peso", "Peso (kg):", value = 70, min = 1, max = 300),
                          numericInput("altura", "Altura (m):", value = 1.70, min = 0.5, max = 2.5, step = 0.01)
               
             ),
             mainPanel(h2("Resultado:"),                # La funciones h2() y h4() refieren al tamaño del texto
                       h4(textOutput("resultado_imc")),
                       h4(textOutput("categoria_imc")),
                       plotlyOutput("plot_imc"))

            )
  ),
  # --- Quinta pestaña --- #
  tabPanel("Modelo predictivo",
           sidebarLayout(
             sidebarPanel(
               width = 8,
               h4("Cuestionario de Hábitos"),
               p("Responde a las siguientes preguntas para que un modelo Random Forest prediga tu categoría:"),

               fluidRow(
                 column(6,
                        selectInput("pred_family", "¿Historial familiar con sobrepeso? (family_history_with_overweight)", choices = unique(df$family_history_with_overweight)),
                        selectInput("pred_FAVC", "¿Consumo frecuente de alimentos ricos en calorías? (FAVC)", choices = unique(df$FAVC)),
                        selectInput("pred_FCVC", "¿Frecuencia de consumo de vegetales? (FCVC)", choices = levels(df$FCVC)),
                        selectInput("pred_NCP", "¿Cantidad de comidas principales? (NCP)", choices = levels(df$NCP)),
                        selectInput("pred_CAEC", "¿Comes entre comidas? (CAEC)", choices = unique(df$CAEC)),
                        selectInput("pred_SMOKE", "¿Fuma usted? (SMOKE)", choices = unique(df$SMOKE)) 
                        ),
                 column(6,
                        selectInput("pred_CH2O", "¿Cuánta agua bebes diariamente? (CH2O)", choices = levels(df$CH2O)),
                        selectInput("pred_SCC", "¿Controlas las calorías que consumes? (SCC)", choices = unique(df$SCC)),
                        selectInput("pred_FAF", "¿Frecuencia de actividad física? (FAF)", choices = levels(df$FAF)),
                        selectInput("pred_TUE", "¿Tiempo de uso de dispositivos tecnológicos? (TUE)", choices = levels(df$TUE)),
                        selectInput("pred_CALC", "¿Frecuencia de consumo de alcohol? (CALC)", choices = unique(df$CALC)),
                        selectInput("pred_MTRANS", "¿Medio de transporte habitual? (MTRANS)", choices = unique(df$MTRANS))
                        )
               ),
               hr(),
               actionButton("btn_predecir", "Predecir categoría", class = "btn-success", width = "100%")
             ),
             mainPanel(
               width = 4,
               h2("Resultado del Modelo:"),
               h3("El modelo predice que perteneces a "),
               h3(textOutput("texto_resultado_rf"), style = "font-weight: bold;"))
           )
  )
)

server = function(input, output) {
  
  # Se usa reactive para que los datos se filtren una única vez.
  # Cada reactive refiere a cada pestaña donde se pueden filtrar los datos
  
  datos_filtrados_exp = reactive({
    req(input$NObeyesdad_exp)
    df[df$NObeyesdad %in% input$NObeyesdad_exp, ]
  })
  
  # Se grafica según que variables se elige
  output$grafico = renderPlot({
    
    if (input$tipo_grafico == "num") { ## Elige variable númerica
      if (input$coloreado == "gral") { # y elige colorear general
        
          ggplot(datos_filtrados_exp(),
               aes(x = .data[[input$var_num]])) +
          geom_density() +
          theme_minimal(base_size = 20) +
          theme(legend.position = "none")
      } else {                           # o elige colorear por NObeyesdad
          ggplot(datos_filtrados_exp(),
               aes(x = .data[[input$var_num]], fill = NObeyesdad)) +
          geom_density(alpha = input$alpha) +
          theme_minimal(base_size = 16)
      }
    } else if (input$tipo_grafico == "cat") { ## Elige variable categorica
      
      if (input$coloreado == "gral") { # y elige colorear general
        
          ggplot(datos_filtrados_exp(),
               aes(x = .data[[input$var_cat]], fill = .data[[input$var_cat]])) +
          geom_bar() +
          labs(y = "Cantidad") +
          theme_minimal(base_size = 20) + 
          theme(legend.position = "none")
      } else {                             # o elige colorar por NObeyesdad
          if (input$eje_y == "cant") {        # y elige se vean las cantidad
              ggplot(datos_filtrados_exp(),
                   aes(x = .data[[input$var_cat]], fill = NObeyesdad)) +
              geom_bar() +
              labs(y = "Cantidad") +
              theme_minimal(base_size = 16)
          } else {                            # o que se vean las proporciones
              ggplot(datos_filtrados_exp(),
                   aes(x = .data[[input$var_cat]], fill = NObeyesdad)) +
              geom_bar(position = "fill") +
              labs(y = "Proporción") +
              theme_minimal(base_size = 16)
          }
      }
    } else if (input$tipo_grafico == "2num") { ## Elige dos variables numericas
          
        if (input$coloreado == "gral") { # y elige colorear general
          ggplot(datos_filtrados_exp(),
                 aes(x = .data[[input$var_num1]], y = .data[[input$var_num2]])) +
            geom_point() +
            theme_minimal(base_size = 16)
      } else {                           # o elige colorear por NObeyesdad
        ggplot(datos_filtrados_exp(),
               aes(x = .data[[input$var_num1]], y = .data[[input$var_num2]])) +
          geom_point(aes(color = NObeyesdad), alpha = input$alpha) +
          theme_minimal(base_size = 16)
      }
    } else if (input$tipo_grafico == "2cat") { ## Elige dos variables categoricas
        ggplot(datos_filtrados_exp(),
           aes(x = .data[[input$var_cat1]], fill = .data[[input$var_cat2]])) +
        geom_bar(position = "dodge") +
        labs(y = "Cantidad") +
        theme_minimal(base_size = 20)
    
    } else if (input$tipo_grafico == "cruzado") { ## Elige una numerica y una categorica
        if (input$coloreado == "gral") {          # y elige colorear normal
            ggplot(datos_filtrados_exp(),
                 aes(x = .data[[input$var_cat_c]], y = .data[[input$var_num_c]])) +
            geom_boxplot(aes(fill = .data[[input$var_cat_c]])) +
            theme_minimal(base_size = 20) +
            theme(legend.position = "none")
        } else {                                   # o elige colorear por NObeyesdad
            ggplot(datos_filtrados_exp(),
                 aes(x = .data[[input$var_cat_c]], y = .data[[input$var_num_c]])) +
            geom_boxplot(aes(fill = NObeyesdad)) +
            theme_minimal(base_size = 16)
        }
    } 
  })
  
  # Titulo dentro de la pestaña resumen. Varia según las variables elegidas y el coloreado
  output$titulo_resumen = renderUI({
    
    texto = if (input$tipo_grafico == "num") {  ## Elige variable númerica
      paste("Medidas de resumen de", input$var_num)
      
    } else if (input$tipo_grafico == "cat") {  ## Elige variable categorica
      if (input$coloreado == "gral") {          # y coloreado normal
        paste("Frecuencias de", input$var_cat)
      } else {                                  # o coloreado según NObeyesdad
        paste("Tabla de contingencia de", input$var_cat, "con Nivel de Obesidad")
      }
      
    } else if (input$tipo_grafico == "2num") { ## Elige dos variables numericas
      paste("Covarianza y correlación entre", input$var_num1, "y", input$var_num2)
      
    } else if (input$tipo_grafico == "2cat") { ## Elige dos variables categoricas
      paste("Tabla de contingencia de", input$var_cat1, "con", input$var_cat2)
      
    } else if (input$tipo_grafico == "cruzado") { ## Elige una variable de cada tipo
      if (input$coloreado == "gral") {             # y coloreado normal
        paste("Resumen de", input$var_num_c, "según", input$var_cat_c)
      } else {                                     # o coloreado según NObeyesdad
        paste("Resumen de", input$var_num_c, "según", input$var_cat_c, "y Nivel de Obesidad")
      }
    }
    
    h4(texto) # Tamaño de dicho titulo
  })
  
  # Tablas o medidas de resumen sobre las variables elegidas.
  output$resumen = renderPrint({
    datos = datos_filtrados_exp()
    
    if (input$tipo_grafico == "num") { ## Elige variable numerica
      x = datos[[input$var_num]]
      if (input$coloreado == "gral") { # y coloreado normal
        data.frame(
          Min = round(min(x), 2),
          Q1 = round(quantile(x, 0.25), 2),
          Mediana = round(median(x), 2),
          Media = round(mean(x), 2),
          Q3 = round(quantile(x, 0.75), 2),
          Max = round(max(x), 2),
          Varianza = round(var(x), 2),
          Desvio = round(sd(x), 2),
          row.names = NULL
        )
      
      } else {                      # o coloreado según NObeyesdad
        datos |> group_by(NObeyesdad) |> 
          summarise(Min = round(min(.data[[input$var_num]]), 2),
                    Q1 = round(quantile(.data[[input$var_num]], 0.25), 2),
                    Mediana = round(median(.data[[input$var_num]]), 2),
                    Media = round(mean(.data[[input$var_num]]), 2),
                    Q3 = round(quantile(.data[[input$var_num]], 0.75), 2),
                    Max = round(max(.data[[input$var_num]]), 2),
                    Varianza = round(var(.data[[input$var_num]]), 2),
                    Desvio = round(sd(.data[[input$var_num]]), 2))
      }
    } else if (input$tipo_grafico == "cat") { ## Elige variable categorica
        if (input$coloreado == "gral") {      # y elige colorear normal
          table(datos[[input$var_cat]]) |> addmargins()
        
        } else if (input$eje_y == "cant") {     # o coloreado según NObeyesdad y la cantidad
          table(datos$NObeyesdad, datos[[input$var_cat]]) |> addmargins()
        } else {                                # o coloreado según NObeyesdad y las proporciones
          table(datos$NObeyesdad, datos[[input$var_cat]]) |> prop.table(margin = 2) |> round(2) 
        } 
    
    } else if (input$tipo_grafico == "2num") { ## Elige dos variables numericas
        data.frame(Covarianza = cov(datos[[input$var_num1]], datos[[input$var_num2]]),
                   Correlacion = cor(datos[[input$var_num1]], datos[[input$var_num2]])) 
        
    
    } else if (input$tipo_grafico == "2cat") { ## Elige dos variables categoricas
        table(datos[[input$var_cat1]], datos[[input$var_cat2]]) |> addmargins()
    
    } else if (input$tipo_grafico == "cruzado") { ## Elige una variable de cada tipo
        if (input$coloreado == "gral") {        # y coloreado normal
          datos |>
            group_by(.data[[input$var_cat_c]]) |>
            summarise(
              Min = round(min(.data[[input$var_num_c]]), 2),
              Q1 = round(quantile(.data[[input$var_num_c]], 0.25), 2),
              Mediana = round(median(.data[[input$var_num_c]]), 2),
              Media = round(mean(.data[[input$var_num_c]]), 2),
              Q3 = round(quantile(.data[[input$var_num_c]], 0.75), 2),
              Max = round(max(.data[[input$var_num_c]]), 2),
              Varianza = round(var(.data[[input$var_num_c]]), 2),
              Desvio = round(sd(.data[[input$var_num_c]]), 2)
            )
      
        } else {                               # o según NObeyesdad
          datos |>
            group_by(.data[[input$var_cat_c]], NObeyesdad) |>
            summarise(
              Min = round(min(.data[[input$var_num_c]]), 2),
              Q1 = round(quantile(.data[[input$var_num_c]], 0.25), 2),
              Mediana = round(median(.data[[input$var_num_c]]), 2),
              Media = round(mean(.data[[input$var_num_c]]), 2),
              Q3 = round(quantile(.data[[input$var_num_c]], 0.75), 2),
              Max = round(max(.data[[input$var_num_c]]), 2),
              Varianza = round(var(.data[[input$var_num_c]]), 2),
              Desvio = round(sd(.data[[input$var_num_c]]), 2),
              .groups = "drop"
            )
        }
    }
  })
  
  # Dataset interactivo en pestaña Base de datos
  output$tabla = renderDT({
    df
  })
  
  # Se usa reactive para hacer el cálculo una unica vez
  imc = reactive({
    req(input$peso, input$altura)
    input$peso / (input$altura^2)
  })
  # Resultados del calculo de IMC 
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
  
  # Gráfico en pestaña IMC que muestra donde se encuentra el peso y la altura en el dataset
  output$plot_imc = renderPlotly({
    imc_grafico = ggplot(datos_filtrados_exp()) +
      geom_point(aes(x = Weight, y = Height, color = NObeyesdad)) +
      geom_vline(xintercept = input$peso) +
      geom_hline(yintercept = input$altura) +
      theme_minimal(base_size = 12) 
    
    ggplotly(imc_grafico)
  })
  # Modelo de predicción
  prediccion_rf <- eventReactive(input$btn_predecir, {
    
    # Creamos un dataframe de 1 fila con las respuestas ingresadas. 
    nuevo_dato <- tibble(
      # Variables que el modelo no usa pero que debemos completar para que funcione
      Gender = factor(levels(df$Gender)[1], levels = levels(df$Gender)),
      Age = 25,
      Height = 1.70,
      Weight = 70,
      # Variables que se deben ingresar porque son las que efectivamente usa el modelo
      family_history_with_overweight = factor(input$pred_family, levels = levels(df$family_history_with_overweight)),
      FAVC = factor(input$pred_FAVC, levels = levels(df$FAVC)),
      FCVC = factor(input$pred_FCVC, levels = levels(df$FCVC)),
      NCP = factor(input$pred_NCP, levels = levels(df$NCP)),
      CAEC = factor(input$pred_CAEC, levels = levels(df$CAEC)),
      SMOKE = factor(input$pred_SMOKE, levels = levels(df$SMOKE)),
      CH2O = factor(input$pred_CH2O, levels = levels(df$CH2O)),
      SCC = factor(input$pred_SCC, levels = levels(df$SCC)),
      FAF = factor(input$pred_FAF, levels = levels(df$FAF)),
      TUE = factor(input$pred_TUE, levels = levels(df$TUE)),
      CALC = factor(input$pred_CALC, levels = levels(df$CALC)),
      MTRANS = factor(input$pred_MTRANS, levels = levels(df$MTRANS))
    )
    
    # Hacemos la predicción con el modelo ya entrenado al inicio (wf_rforest2)
    pred <- predict(wf_rforest2, new_data = nuevo_dato)
    return(pred$.pred_class[1])
  })
  
  output$texto_resultado_rf <- renderText({
    req(prediccion_rf())
    paste(prediccion_rf())
  })
}

shinyApp(ui, server)

