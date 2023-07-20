#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


ruta_archivo1 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_BadBunny.csv"
ruta_archivo2 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_Maluma.csv"
ruta_archivo3 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_Shakira.csv"
ruta_archivo4 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_KarolG.csv"
ruta_archivo5 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_JBalvin.csv"
ruta_archivo6 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_Camilo.csv"
ruta_archivo7 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_CarlosVives.csv"
ruta_archivo8 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_DaddyYankee.csv"
ruta_archivo9 <- "C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_MileyCyrus.csv" 
ruta_archivo10 <-"C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_BLACKPINK.csv"
ruta_archivo11 <-"C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_Rihanna.csv"
ruta_archivo12 <-"C:/Proyectos ML/DM-ML/ProyAnaliticaDesc/datos/artista_Adele.csv"


library(dplyr)

data_artistas <- bind_rows(
  read.csv(ruta_archivo1),
  read.csv(ruta_archivo2),
  read.csv(ruta_archivo3),
  read.csv(ruta_archivo4),
  read.csv(ruta_archivo5),
  read.csv(ruta_archivo6),
  read.csv(ruta_archivo7),
  read.csv(ruta_archivo8),
  read.csv(ruta_archivo9),
  read.csv(ruta_archivo10),
  read.csv(ruta_archivo11),
  read.csv(ruta_archivo12)
  
)

datos_filtrados <- subset(data_artistas, !is.na(stilo) & stilo != "")
data_artistas <-datos_filtrados

library(dplyr)

# Vector de nombres de artistas a filtrar
nombres_artistas <- c("Bad Bunny","Maluma", "Shakira", "Karol G","J. Balvin",
                      "Camilo","Carlos Vives","Daddy Yankee","Miley Cyrus",
                      "Hannah Montana","Maroon 5","Morat","Blackpink")
# Filtrar por nombres de artistas utilizando subset()
data_filtro_nombres <- subset(data_artistas, Artista %in% nombres_artistas)

library(ggplot2)
ggplot(data_filtro_nombres, aes(x = Artista)) + 
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Artista", y = "Temas") +
  ggtitle("Audiencia del artista") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

estilo1 <- datos_filtrados %>%group_by(stilo) 
#print(estilo2)
condicion_x <- estilo1 %>% group_by(stilo)%>%
  count() %>%  filter(n > 15) %>%  pull(stilo)
estilo1 <- estilo1 %>% filter(stilo %in% condicion_x)


ggplot(estilo1,aes(x=stilo))+ geom_bar()+
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Estilo", y = "Número de canciones") +
  ggtitle("Géneros musicales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






library(shiny)
# Importar datos y resultados desde el archivo .Rmd
#knitr::purl("gen_music.Rmd", output = "app.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Géneros musicales"),
 
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  # Render the ggplot plot
  output$ggplotPlot <- renderPlot({
    estilo1 <- data.frame(estilo = c("Rock", "Pop", "Hip-hop", "Electrónica", "Jazz"),
                          cantidad = c(10, 20, 15, 12, 8))
    
    ggplot(estilo1, aes(x = estilo)) +
      geom_bar(fill = "lightblue", color = "black") +
      labs(x = "Estilo", y = "Número de canciones") +
      ggtitle("Géneros musicales") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
