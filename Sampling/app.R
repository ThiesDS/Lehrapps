#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mosaic)
library(nycflights13)
library(shinycssloaders) # Added package for spinner (see below)
library(shinydashboard)  # For a nicer looking layout out of the box
data(flights)


flights <- flights %>%
  filter(origin=="JFK") %>%
  select(arr_delay, distance) %>%
  mutate(Verspätet=factor(ifelse(arr_delay>10, "Ja", "Nein"))) %>%
  rename(Entfernung=distance) %>%
  select(-arr_delay) %>%
  na.omit()


verpop <- prop(~Verspätet, success = "Ja", data = flights)
entpop <- mean(~Entfernung, data = flights)
# Calculate true standard deviation from population data
verpop_sdtrue <- (verpop*(1-verpop))
entpop_sdtrue <- sd(~Entfernung, data = flights)
# Calculate standard error for 50-Obs Sample data
verpop_se50 =  verpop_sdtrue / sqrt(50)
entpop_se50 =  entpop_sdtrue / sqrt(50)


xlims <- c(0,5500)

# Calculate fixed x-limits as 6 times the standard error for 50-Obs Sample data
xlims_ver50 <- c((verpop-6*verpop_se50),(verpop+6*verpop_se50))
xlims_ent50 <- c((entpop-6*entpop_se50),(entpop+6*entpop_se50))

# ui section
ui = dashboardPage(
  dashboardHeader(title = "FOM-Lehrapp: Sampling",titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Population", tabName = "Population",icon = icon('tachometer')),
      menuItem("Sample Verspätungen (Anteil)", tabName = "Sample_prop",icon = icon('tachometer')),
      menuItem("Sample Entfernung (Mittelwert)", tabName = "Sample_mean",icon = icon('tachometer'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Population",
        fluidRow(
          box(
            width = 12,
            status = "info", solidHeader = TRUE,
            title = "Hintergrund",
            p("Im R Paket nycflights13 liegen alle abgehenden Flüge aus New York City innerhalb der USA aus dem Jahr 2013 vor. 
              Der Datensatz wurde eingeschränkt auf den Flughafen JFK, so dass insgesamt N=109079 Beobachtungen vorliegen. Ein 
              Flug wurde als verspätet klassifiziert, wenn er mehr als 10min Verspätung hatte.", br(),
              "Hier sehen Sie die Verteilung der Entfernung des Fluges, so wie ob dieser verspätet ankam.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary", solidHeader = TRUE,
            title = "Verteilung Verspätung",
            plotOutput("PlotOriginalVer") %>% withSpinner(color = '#387F72')  # With spinner while figure is loading
          )
        ),
        fluidRow(
          box(
            width = 12, 
            status = "primary", solidHeader = TRUE,
            title = "Verteilung Entfernung",
            plotOutput("PlotOriginalEnt") %>% withSpinner(color = '#387F72')  # With spinner while figure is loading
          )
        )
      ),
      tabItem("Sample_prop",
      fluidRow(
          box(
            width = 12,
            status = "info", solidHeader = TRUE,
            title = "Hintergrund",
            p("Im R Paket nycflights13 liegen alle abgehenden Flüge aus New York City innerhalb der USA aus dem Jahr 2013 vor. 
              Der Datensatz wurde eingeschränkt auf den Flughafen JFK, so dass insgesamt N=109079 Beobachtungen vorliegen. Ein 
              Flug wurde als verspätet klassifiziert, wenn er mehr als 10min Verspätung hatte.", br(),
              "Hier sehen Sie die Verteilung der Entfernung des Fluges, so wie ob dieser verspätet ankam.")
          )
        ),
        fluidRow(
          box(
            width = 4, height = 250,
            status = "info", solidHeader = TRUE,
            title = "Stichprobenziehung 50 Flüge",
            p("Ziehe eine Stichprobe von 50 Flügen aus der Population und schaue dir an wie hoch der Anteil der Verspätungen in dieser Stichprobe ist:"),
            actionButton("SampleGo50_ver", "Sample!", icon = icon("refresh"))
          ),
          box(
            width = 4, height = 250,
            status = "info", solidHeader = TRUE,
            title = "Stichprobenziehung 500 Flüge",
            p("Was passiert wenn du in deiner Stichprobe nicht 50 sondern 500 Flüge ziehst? Probiere es aus indem du Stichproben mit 500 Flügen ziehst."),
            actionButton("SampleGo500_ver", "Sample!", icon = icon("refresh"))
          ),
          box(
            width = 4, height = 250,
            status = "warning", solidHeader = TRUE,
            title = "Erklärung",
            p("Was passiert hier genau?")
          )
        ),
        fluidRow(
          box(
            width = 6,
            status = "primary", solidHeader = TRUE,
            title = "Verteilung Verspätungen in aktueller Stichprobe",
              plotOutput("PlotSampleVer")
          ),
          box(
            width = 6,
            status = "primary", solidHeader = TRUE,
            title = "Anteil Verspätungen aller Stichproben",
            plotOutput("PlotPropsVer")
          )
        )
      ),
      tabItem("Sample_mean",
        fluidRow(
          box(
            width = 4, height = 250,
            status = "info", solidHeader = TRUE,
            title = "Stichprobenziehung 50 Flüge",
            p("Ziehe eine Stichprobe von 50 Flügen aus der Population und schaue dir an wie hoch der Anteil der Verspätungen in dieser Stichprobe ist:"),
            actionButton("SampleGo50_ent", "Sample!", icon = icon("refresh"))
          ),
          box(
            width = 4, height = 250,
            status = "info", solidHeader = TRUE,
            title = "Stichprobenziehung 500 Flüge",
            p("Was passiert wenn du in deiner Stichprobe nicht 50 sondern 500 Flüge ziehst? Probiere es aus indem du Stichproben mit 500 Flügen ziehst."),
            actionButton("SampleGo500_ent", "Sample!", icon = icon("refresh"))
          ),
          box(
            width = 4, height = 250,
            status = "warning", solidHeader = TRUE,
            title = "Erklärung",
            p("Was passiert hier genau?")
          )
        ),
        fluidRow(
          box(
            width = 6,
            status = "primary", solidHeader = TRUE,
            title = "Verteilung Entfernungen in aktueller Stichprobe",
            plotOutput("PlotSampleEnt")
          ),
          box(
            width = 6,
            status = "primary", solidHeader = TRUE,
            title = "Mittelwert Entfernungen aller Stichproben",
            plotOutput("PlotMeansEnt")
          )
        )
      )
    )
  )
)




# server section
server = function(input, output, session) {
  
  # For debugging
  options(shiny.fullstacktrace = TRUE)
  options(shiny.trace = TRUE)
  
  # Set reactive value objects
  werte_ver <- reactiveValues()
  werte_ent <- reactiveValues()
  
  # PROPORTION: When the Sample 50 button is clicked, sample 50 flights and store values
  observeEvent(input$SampleGo50_ver, {
    full_sample <- sample(flights,50)
      
    isolate({
      werte_ver$sample <- full_sample
      werte_ver$metric <- c(werte_ver$metric, prop(~Verspätet, success = "Ja", data=full_sample))
      werte_ver$size <- c(werte_ver$size, "50")
      })
  })
  
  # PROPORTION: When the Sample 500 button is clicked, sample 500 flights and store values (in same object)
  observeEvent(input$SampleGo500_ver, {
    full_sample <- sample(flights,500)
    
    isolate({
      werte_ver$sample <- full_sample
      werte_ver$metric <- c(werte_ver$metric, prop(~Verspätet, success = "Ja", data=full_sample))
      werte_ver$size <- c(werte_ver$size, "500")
    })
  })
  
  # MEAN: When the Sample 50 button is clicked, sample 50 flights and store values
  observeEvent(input$SampleGo50_ent, {
    full_sample <- sample(flights,50)
    
    isolate({
      werte_ent$sample <- full_sample
      werte_ent$metric <- c(werte_ent$metric, mean(~Entfernung, data=full_sample))
      werte_ent$size <- c(werte_ent$size, "50")
    })
  })
  
  # MEAN: When the Sample 500 button is clicked, sample 500 flights and store values (in same object)
  observeEvent(input$SampleGo500_ent, {
    full_sample <- sample(flights,500)
    
    isolate({
      werte_ent$sample <- full_sample
      werte_ent$metric <- c(werte_ent$metric, mean(~Entfernung, data=full_sample))
      werte_ent$size <- c(werte_ent$size, "500")
    })
  })

  
  
  # PROPORTION: Plot the original (population) distribution of flights delayed
  output$PlotOriginalVer <- renderPlot({
    gf_bar( ~ Verspätet, data = flights,
            title = paste0("Anzahl Flüge: ",nrow(flights), ", Anteil Verspätet: ", round(verpop,2)),
            color = 'white',       # White spaces between bars for better separation of bars
            fill = '#387F72',      # FOM-Blue for corporate design
            alpha = .5) %>%        # Transparancy to better read exact values
      gf_labs(x = 'Verspätung',
              y = 'Anzahl Flüge') %>%
      gf_theme(theme = theme_bw())   # Black-white theme only little distraction in the background 
  })
  
  # MEAN: Plot the original (population) distribution of flight distance
  output$PlotOriginalEnt <- renderPlot({
    gf_histogram( ~ Entfernung, data = flights,
                  title = paste0("Anzahl Flüge: ",nrow(flights), ", Mittelwert Entfernung: ", round(entpop,2)),
                  binwidth = 250,       
                  color = 'white',      # White spaces between bars for better separation of bars
                  fill = '#387F72',     # FOM-Blue for corporate design
                  alpha = .5) %>%       # Transparancy to better read exact values
      gf_lims(x = xlims) %>%
      gf_refine(scale_x_continuous(limits = xlims, breaks = seq(xlims[1],xlims[2],250))) %>%  # Breaks set manually to better read the exact values of the bars
      gf_labs(x = 'Entfernung',
              y = 'Anzahl Flüge') %>%
      gf_theme(theme = theme_bw())   # Black-white theme only little distraction in the background 
  })
  
  
  
  # PROPORTION: Plot proportion in current sample of delayed flights (either 50 or 500 sample)
  output$PlotSampleVer <- renderPlot({
    req(werte_ver$sample)
    
      gf_bar( ~ Verspätet, data = werte_ver$sample,
              title = paste0(" Anteil Verspätet: ", round(tail(werte_ver$metric,1),2)),
              color = 'white',        # White spaces between bars for better separation of bars
              fill = '#387F72',       # FOM-Blue for corporate design
              alpha = .5) %>%         # Transparancy to better read exact values
      gf_labs(x = 'Verspätung',        
              y = 'Anzahl Flüge') %>%  
      gf_theme(theme = theme_bw())    # Black-white theme only little distraction in the background 
  })
  
  # PROPORTION: Plot distribution of proportions of delayed flights (sample 50 and 500 together)
  output$PlotPropsVer <- renderPlot({
    req(werte_ver$metric)
    
    data = data.frame(x=werte_ver$metric,Stichprobenumfang=werte_ver$size)
    
    p <- gf_dotplot( ~ x, fill = ~Stichprobenumfang, data=data,
                     title = paste0("Anzahl Stichproben: ",length(data$x)),
                     color = 'white',
                     alpha = .5) %>%  # FOM-Blue for corporate design
      gf_vline(xintercept = tail(data$x,1), col = "red") %>%
      gf_vline(xintercept = verpop, col = "blue") %>%
      gf_lims(x = xlims_ver50) %>%       # Fixed x-limits to better visualize what happens when new observations join the existing ones
      gf_labs(x = 'Anteil Verspätungen',
              y = 'Häufigkeit') %>%
      gf_theme(theme = theme_bw())       # Black-white theme only little distraction in the background
    
    p <- p + annotate("text", x = verpop, y = .80, label = "Wahrer Anteil (Population)", size = 4)            # Label the vertical line as expectation value from the  population
    p <- p + annotate("text", x = tail(data$x,1), y = .70, label = "Anteil der aktuellen SP", size = 4)  # Lable the vertical line as Sample mean
    p
  })
  
  
  
  # MEAN: Plot distances in current sample of flight distances (either 50 or 500 sample)
  output$PlotSampleEnt <- renderPlot({
    req(werte_ent$sample)
    
    gf_histogram( ~ Entfernung, data = werte_ent$sample,
                  title =  paste0(" Mittelwert Entfernung: ", round(tail(werte_ent$metric,1),2)),
                  binwidth = 250,
                  color = 'white',        # White spaces between bars for better separation of bars
                  fill = '#387F72',       # FOM-Blue for corporate design
                  alpha = .5) %>%         # Transparancy to better read exact values
                  gf_lims(x = xlims) %>%
                  gf_refine(scale_x_continuous(limits = xlims, breaks = seq(xlims[1],xlims[2],250))) %>% # Breaks set manually to better read the exact values of the bars
                  gf_labs(x = 'Entfernung',
                          y = 'Anzahl Flüge') %>%
      gf_theme(theme = theme_bw())    # Black-white theme only little distraction in the background 
  })
  
  # MEAN: Plot distribution of means of flight distances (sample 50 and 500 together)
  output$PlotMeansEnt <- renderPlot({
    req(werte_ent$metric)
    
    data = data.frame(x=werte_ent$metric,Stichprobenumfang=werte_ent$size)
    
    p <- gf_dotplot( ~ x, fill = ~Stichprobenumfang, data=data,
                     title = paste0("Anzahl Stichproben: ",length(data$x)),
                     color = 'white',
                     alpha = .5) %>%  # FOM-Blue for corporate design
      gf_vline(xintercept = tail(data$x,1), col = "red") %>%
      gf_vline(xintercept = entpop, col = "blue") %>%
      gf_lims(x = xlims_ent50) %>%       # Fixed x-limits to better visualize what happens when new observations join the existing ones
      gf_labs(x = 'Anteil Verspätungen',
              y = 'Häufigkeit') %>%
      gf_theme(theme = theme_bw())       # Black-white theme only little distraction in the background
    
    p <- p + annotate("text", x = entpop, y = .80, label = "Wahrer Mittelwert (Population)", size = 4)            # Label the vertical line as expectation value from the  population
    p <- p + annotate("text", x = tail(data$x,1), y = .70, label = "Mittelwert der aktuellen SP", size = 4)  # Lable the vertical line as Sample mean
    p
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)


