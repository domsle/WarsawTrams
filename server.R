library(shiny)
library(dplyr)
library(jsonlite)
library(lubridate)
library(geosphere)
library(ggplot2)
library(DT)
library(httr)

# Funkcjonalność odpowiedzialna za odczytywanie pliku z kluczem dostępu API
# Celem tego zachowania jest udostępnienie aplikacji w publicznym repozytorium
# bez udostępniania samego klucza 
api_key <- readRDS("api_key.RDS")

shinyServer(function(input, output) {
  
  # Główna zmienna reaktywna, w którym przechowywane są wszystkie dane na temat tramwajów
  # Pierwotnie zmienna jest pusta, natomiast jej wartość zostanie nadpisana w późniejszym czasie
  trams_history <- reactiveVal()
  
  # Zmienna reaktywna zawierająca najaktualniejsze (ostatnio pobrane) wartości danych otrzymanych z API
  # W kodzie generującym zmienną najpierw pobierane są dane z odpowiedniego (predefiniowanego) adresu URL
  # endpointa. Następtnie pobrane dane są poddane prostym sprawdzeniom i zwracane  
  trams <- reactive({
    # Definicja adresu URL endpointa
    api_url <-
      "https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id=f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey="
    
    # Dodanie do adresu URL parametru, będącego kluczem API do pobrania, oraz parametru definiującego 
    # pobranie danych dla tramwajów
    url.api <- paste(api_url, api_key, "&type=2", sep = "")
    
    # Pobranie danych przy pomocy zapytania GET
    result_raw <- httr::GET(url = url.api)
    
    # Ekstrakcja z otrzymanej odpowiedzi dołączonej zawartości, w formacie tekstowym  
    result_text <- httr::content(result_raw, as = 'text')
    
    # Przeformatowanie otrzymanych danych z formatu tekstowego do ramki danych 
    result <- jsonlite::fromJSON(result_text)$result
    
    # Logika sprawdzająca czy pobrane dane są zgodne z oczekiwaniami
    if(!is.data.frame(result)) {
    # Jeżeli pobrane dane nie zostały przekonwertowane do ramki danych, coś poszło nie tak i dane będą pobrane 
    # znowu za 5 sekund 
      invalidateLater(5 * 1000)
      req(FALSE, cancelOutput = FALSE)
    } else if(ncol(result) != 6) {
    # Jeżeli pobrana ramka nie zawiera 6 kolumn, również coś poszło nie tak - najprawdopodobniej dane są
    # wysyłane w formacie innym niż oczekiwany, jednak za 5 sekund zakolejkowana jest kolejna próba pobrania
      invalidateLater(5 * 1000)
      req(FALSE, cancelOutput = FALSE)
    } else {
    # Jeżeli powyższe sprawdzenia nie zachodzą, kolejne zakolejkowanie jest zakolejkowane na za 15 sekund 
      invalidateLater(15 * 1000)
      result <- dplyr::as_tibble(result)
    }
    # Zmiana struktury danych z dataframe do dplyr::tibble
    result
  })
  
  # Observer, śledzący zmiany na zmiennej reaktywnej `trams()`
  # Zadaniem tego observer'a jest aktualizowanie zmiennej reaktywnej `trams_history()`
  # w zależności od zmian zmiennej reaktywnej `trams()`. 
  # Przy pierwszym uruchomieniu zmienna `trams_history()` zawiera dane, które zostały
  # ostatnio pobrane z API.
  # Każde kolejne uruchomienie tworzy ramkę danych zawierającą dane aktualnie znajdujące się 
  # w ramce danych w trams_history(), oraz dane z najnowszego pobrania z API
  # 
  # Otrzymana w ten sposób ramka danych zawiera stan z dwóch ostatnich pobrań z API, 
  # dzięki czemu da się śledzić zmiany zachodzące w 
  observeEvent(
    trams(),
    {
      if (is.null(trams_history())) {
        data <- trams()
        
      } else if (ncol(trams_history()) == 6) {
        data <- trams() %>%
          dplyr::left_join(
            trams_history() %>% dplyr::select(-Lines,-Brigade),
            by = 'VehicleNumber',
            suffix = c('', '_old')
          ) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            Time = lubridate::as_datetime(Time),
            Time_old = lubridate::as_datetime(Time_old),
            Time_d = as.numeric(Time - Time_old),
            Dist = geosphere::distGeo(c(Lon, Lat), c(Lon_old, Lat_old)),
            Speed = Dist / Time_d,
            Speed = if_else(is.nan(Speed) |
                              is.na(Speed), 0, Speed)
          ) %>%
          dplyr::ungroup()
        
      } else {
        data <- trams() %>%
          dplyr::left_join(
            dplyr::select(
              trams_history(),
              Lon = Lon_old,
              VehicleNumber,
              Time = Time_old,
              Lat = Lat_old
            ),
            by = 'VehicleNumber',
            suffix = c('', '_old')
          ) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            Time = lubridate::as_datetime(Time),
            Time_old = lubridate::as_datetime(Time_old),
            Time_d = as.numeric(Time - Time_old),
            Dist = geosphere::distGeo(c(Lon, Lat), c(Lon_old, Lat_old)),
            Speed = Dist / Time_d,
            Speed = if_else(is.nan(Speed) |
                              is.na(Speed), 0, Speed)
          ) %>%
          dplyr::ungroup()
        
      }
      # print(data)
      trams_history(data)
    })
  
  # Zmienna reaktywna, której działanie odfilterowuje z pobranych dane na temat tramwajów, jedynie
  # wybrane przez użytkownika linie (kolumna `Lines`)
  trams_filtered <- reactive({
    trams() %>%
      dplyr::filter(Lines %in% !!input$lines_selected)
  })
  
  # Zmienna reaktywna, z pobranych danych wyłuskuje występujące unikalne wartości kolumny `Lines`, 
  # następnie przekształca kolumnę na wektor. Zmienna jest używana do wybierania interesujących 
  # użytkownika linii tramwajowych
  lines <- reactive({
    trams() %>%
      dplyr::distinct(Lines) %>%
      dplyr::pull(Lines)
  })
  
  # Zmienna reaktywna, do istniejących unikalnych wartości z ramki danych ze zmiennej reaktywnej 
  # `trams_filtered()` dopasowuje kolory z palety kolorów biblioteki viridisLite. 
  # W tym przypadku używane są kolory z palety viridisLite::turbo() 
  # Wynikowa ramka danych umożliwia łatwiejsze identyfikowanie tramwajów na wizualizacjach, 
  # ponieważ te linie tramwajowe mają przypisane konkretne kolory
  colors <- reactive({
    colors <- trams() %>%
      dplyr::distinct(Lines)
    
    colors$Color <- viridisLite::turbo(n = nrow(colors))
    
    colors
  })
  
  # Observer, śledzący zmienną reaktywną `lines()`. 
  # Przy każdej zmianie zmiennej `lines()` (zawierającej wszystkie dostępne unikalne numery linii tramwajowych) 
  # będzie stosownie aktualizować formularz wyjściowy do wybierania interesujących linii tramwajowych 
  # W razie ewentualności, kiedy pobrane dostają inne linie, przed aktualizacją formularza, odczytywane są wybrane 
  # linie. Aktualizowany formularz ma podmienianą listę dostępnych linii, a także wybrane przez użytkownika linie,
  # więc po aktualizacji użytkownik nie spotyka się z sytuacją w której wybrane przez niego linie zostały odznaczone.
  observeEvent(
    lines(),
    {
      selected <- input$lines_selected
      
      updateSelectInput(
        inputId = 'lines_selected',
        choices = lines(),
        selected = selected)
    })
  
  # Zmienna reaktywna zawierająca obiekt używany do rysowania mapy
  # - do ramki danych z właściwościami na temat wybranych tramwajów przy pomocy funkcji 
  # dplyr::left_join dołączane są odpowiednie kolory linii tramwajów
  # - funkcja leaflet::leaflet() tworzy obiekt pozwalający wyświetlać mapy, przekazywana jest 
  # do niego ramka danych zawierająca wszystkie później wizualizowane dane  
  # - funkcja leaflet::addTiles() dodaje do przekazanego jej obiektu warstwę
  # mapy, z domyślnymi ustawienami
  # - leaflet::addCircleMarkers() dodaje do przekazanego obiektu okrągłe znaczniki, zgodnie
  # z danymi przekazanymi w ramce danych na początku. Użyte są następujące kolumny:
  # lng = ~ Lon - kolumna `Lon` używana jako długość geograficzna punktów na mapie
  # lat = ~ Lat - kolumna `Lat` używana jako szerokość geograficzna punktów na mapie
  # group = ~ Lines - kolumna `Lines` używana do oznaczenia oddzielnych grup, będących liniami tramwajów
  # color = ~ Color - kolumna `Color` używana do wybrania koloru którymi poszczególne grupy są oznaczone
  # label = ~ Lines - kolumna `Lines` używana do pokazania etykiety po najechaniu myszką (numer linii)
  main_map <- reactive({
    req(input$lines_selected)
    trams_filtered() %>%
      dplyr::left_join(colors(), by = 'Lines') %>%
      leaflet::leaflet() %>% # Na tym etapie otrzymujemy zainicjowany obiekt bez mapy (z szarym tłem)
      leaflet::addProviderTiles(
        leaflet::providers$Stamen.TonerLite
        # Do przekazanego obiektu, dodajemy warstwę zawierającą mapę
        # Wybór konkretnej mapy jest motywowany względami estetycznymi
      ) %>%
      leaflet::addCircleMarkers( 
        # Do mapy dodajemy punkty korzystając z danych w przekazanej 
        # w pierwszym kroku ramki danych
        lng = ~ Lon,
        lat = ~ Lat,
        group = ~ Lines,
        color = ~ Color,
        label = ~ paste0(Lines, ", nr pojazdu: ", VehicleNumber)
      )
  })
  
  # Odczytanie ze zmiennej reaktywnej `main_map()` zawierającej obiekt służący do rysowania mapy,
  # następnie przekazanie tego obiektu do listy `output`, w celu pokazania tego obiektu w interfejsie aplikacji
  output$main_map <- leaflet::renderLeaflet({
    main_map()
  })
  
  # Odczytanie ze zmiennej reaktywnej `trams_history()` ramki danych z właściwościami 
  # tramwajów. Ramka danych ulega agregacji
  # 
  # Na końcu ramka danych jest przekazana do listy `output` przy pomocy funkcji `renderDataTable()`
  output$table <- renderDataTable({
    req(trams_history())
    req(ncol(trams_history()) > 7)
    trams_history() %>%
      dplyr::filter(Lines %in% !!input$lines_selected) %>%
      dplyr::group_by(Lines) %>%
      dplyr::summarise(Mean = mean(Speed, na.rm = TRUE),
                Max  = max(Speed, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(Mean = round(Mean, 2),
             Max = round(Max, 2))
    
  })
  
  
  output$plot <- renderPlot({
    req(trams_history())
    req(ncol(trams_history()) > 7)
    col <- colors()$Color
    names(col) <- colors()$Lines
    
    trams_history() %>%
      select(Line = Lines, Dist) %>%
      ggplot2::ggplot() +
      ggplot2::geom_histogram(aes(x = Dist, fill = Line)) +
      ggplot2::scale_fill_discrete(type = col) +
      ggplot2::ggtitle('Histogram dystansu pokonanego przez tramwaje od ostatniego pobrania')
  })
  
  
})
