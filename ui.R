library(shiny)
library(leaflet)

# Do rysowania interfejsu używane są funkcje pochodzące z pakietu shiny: 
# - shinyUI i fluidPage - do rysowania całego interfejsu strony
# - sidebarLayout - do umiejscawiania elementów strony internetowej w układzie:
#    - pasek boczny (sidebarPanel)
#    - główny kontener (mainPanel)  
shinyUI(
  fluidPage(
    titlePanel("Trams_Data"),
    sidebarLayout(
      sidebarPanel(
        
        # Formularz - przy pomocy rozsuwanej listy użytkownik wybiera jakie linie interesują go w analizie
        selectInput(
          'lines_selected',
          label = 'Wybierz Linie:',
          choices = '',
          selectize = TRUE,
          multiple = TRUE),
        
        # Dane wyjściowe - wykres pokazujący rozkład %%%% tramwajów
        plotOutput('plot'),
        
        # Dane wyjściowe - tabela zawierająca proste statystyki podsumowujące wybrane tramwaje 
        dataTableOutput('table')
      ),
      mainPanel(
        
        # Dane wyjściowe - mapa pokazująca aktualne pozycje tramwajów
        leafletOutput(
          'main_map', 
          height = '800px'
          # height = '100%'
        )
      )
    )
  )
)
