#app om bevolkingspyramide te genereren voor provincie of gemeente

library(shiny)
library(tidyverse)
library(quantmod)
library(cbsodataR)


#bestand met provincies en gemeenten genereren (peildatum 2018-01-01)
#bron: CBS tabel id 83958NED

plaatscode.gem.prov <- cbs_get_data(id = "83958NED")
gemeenten <- plaatscode.gem.prov %>%
  select(GEMEENTE = Naam_2, GEM_CODE = Code_3) %>%
  unique() %>%
  mutate(GEMEENTE = trimws(toupper(GEMEENTE)),
         GEM_CODE = trimws(as.character(GEM_CODE))) %>%
  arrange(GEMEENTE)
provincies <- plaatscode.gem.prov %>%
  select(PROVINCIE = Naam_4, PROV_CODE = Code_5) %>%
  unique() %>%
  mutate(PROVINCIE = trimws(toupper(PROVINCIE)),
         PROV_CODE = paste0(trimws(as.character(PROV_CODE)),"  ")) %>%
  arrange(PROV_CODE)
gebieden <- bind_rows(
  mutate(gemeenten, GEBIED = GEMEENTE, GEBIED_CODE = GEM_CODE),
  mutate(provincies, GEBIED = PROVINCIE, GEBIED_CODE = PROV_CODE))

ui <- fluidPage(
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      #Select provincie of gemeente
      radioButtons(inputId = "keuze",
                  label = "Provincie of  gemeente overzicht genereren?",
                  choices = c("Provincie", "Gemeente"),
                  selected = "Gemeente"),
      
      conditionalPanel(
          condition = "input.keuze == 'Provincie'",
      selectInput(inputId = "prov",
                 label = "Selecteer provincie",
                 choices = provincies$PROVINCIE,
                 selected = "ZUID-HOLLAND")),
      
      conditionalPanel(
          condition = "input.keuze == 'Gemeente'",
      selectInput(inputId = "gem", 
                  label = "Selecteer gemeenten",
                  choices = gemeenten$GEMEENTE,
                  selected = c("AMSTERDAM"),
                  multiple = FALSE))
    ),
    
    # Output(s)
    mainPanel(
      #plotOutput(outputId = "ned", height = "200px", width = "50%"),
      conditionalPanel(condition = "input.keuze == 'Gemeente'",
                        textOutput(outputId = "tekstgem")),
      conditionalPanel(condition = "input.keuze == 'Provincie'",
                       textOutput(outputId = "tekstprov")),
      br(), br(),
      plotOutput(outputId = "bevpyr")
)
)
)

      
      #dataTableOutput(outputId = "gemdata")



#bestand met inwoneraantallen gekozen provincie/ gemeente genereren
#peildatum 2018-01-01
#bron CBS tabel 03759ned


# Server

server <- function(input, output) {
  
  gebcode <- reactive({
    if (input$keuze == 'Provincie') {
         provincies$PROV_CODE[provincies$PROVINCIE == input$prov]
    } else {
         gemeenten$GEM_CODE[gemeenten$GEMEENTE == input$gem]
    }
  })
  
  plotdata <- reactive({
    cbs_get_data(id = "03759ned",
                 Perioden = "2018JJ00",
                 RegioS = gebcode(),
                 BurgerlijkeStaat = "T001019")  %>%
      filter(Geslacht != "T001038", Leeftijd > 10000, Leeftijd < 20000) %>%
      cbs_add_label_columns() %>%
      mutate(LEEFTIJD = as.integer(str_remove(Leeftijd_label, "jaar")),
             AANTAL = ifelse(Geslacht_label == "Mannen", -BevolkingOp1Januari_1,
                             BevolkingOp1Januari_1)) %>%
      select(GEB_CODE = RegioS,
             GEBIED = RegioS_label,
             GESLACHT = Geslacht_label,
             LEEFTIJD,
             AANTAL) %>%
      group_by(GEBIED)  %>%  
      mutate(AANTAL_PROC = AANTAL/sum(abs(AANTAL)) * 100)
  })
  
  output$bevpyr <- renderPlot({
    ggplot(data = plotdata(),
           aes(x = LEEFTIJD,
               y = AANTAL_PROC,
               group = GESLACHT,
               fill = GESLACHT)) +
      geom_bar(stat = 'identity', col = 'grey') +
      ylab("AANTAL INWONERS PROCENTUEEL") +
      scale_x_continuous(breaks = seq(0, 100, 10)) +
      scale_y_continuous(labels = abs) +
      facet_wrap(GEBIED~.) +
      coord_flip() +
      theme_light() +
      ggtitle("AANTAL INWONERS NAAR GESLACHT EN LEEFTIJD OP 1 JANUARI 2018")
  })
  
  
  output$tekstgem <- renderText ({
    paste("Gemeente", input$gem, gebcode())
  })
  output$tekstprov <- renderText({
    paste("Provincie", input$prov, gebcode())
  })
}




# Create a Shiny app object
shinyApp(ui = ui, server = server)




         
