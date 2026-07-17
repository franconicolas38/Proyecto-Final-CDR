library(shiny)
library(ggplot2)
library(DT)

df<- read_csv("ObesityDataSet_raw_and_data_sinthetic.csv")
df =
  df |> mutate(NObeyesdad = factor(NObeyesdad,
                                   levels = c("Insufficient_Weight", 
                                              "Normal_Weight",
                                              "Overweight_Level_I", 
                                              "Overweight_Level_II", 
                                              "Obesity_Type_I", 
                                              "Obesity_Type_II", 
                                              "Obesity_Type_III"),
                                   ordered = TRUE))


columnas_validas <- sapply(df, function(x) is.factor(x) | is.character(x))
variables_categoricas <- names(df)[columnas_validas]
variables_categoricas <- setdiff(variables_categoricas, "NObeyesdad")


ui = fluidPage(
  titlePanel("Explorador de datos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yvar", "Variable categórica:", choices = variables_categoricas),
      checkboxGroupInput("NObeyesdad", "Categoría",
                         choices = unique(df$NObeyesdad),
                         selected = unique(df$NObeyesdad))
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Gráfico", plotOutput("bar")),
        tabPanel("Resumen", verbatimTextOutput("resumen")),
        tabPanel("Tabla", DTOutput("tabla"))
      )
    )
  )
)

server = function(input, output){
  
  datos_filtrados = reactive({
    req(input$NObeyesdad)
    df[df$NObeyesdad %in% input$NObeyesdad, ]
  })
  
  output$bar = renderPlot({
    ggplot(datos_filtrados(), 
           aes(x = .data[[input$yvar]], fill = NObeyesdad )) +
      geom_bar(position = "dodge") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$resumen = renderPrint({
    req(input$yvar)
    datos <- datos_filtrados()
    summary(datos[[input$yvar]])
  })
  
  output$tabla = renderDT({
    datos_filtrados()
  })
}

shinyApp(ui, server)

