#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-------Library Install Section-------#
#---------------#

#---------------manipulation
library(data.table)
library(tidyverse)
library(magrittr)
library(tidyr)
library(skimr)

#---------------viz
library(plotly)
library(formattable)
library(grid)
library(gridExtra)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(waffle)
library(extrafont)

#---------------mapping
library(tmap)
library(sf)
library(spData)
library(leaflet)

#---------------publishing
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)

#for manipulation
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

#-------Library Install Section-------#
#-------end-------#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-------Data Section-------#
#---------------#

#---------------import
setwd("C:/Users/rebec/30 R/ECI 254/DATA/New folder")
killed = fread("killedJournalists.csv", 
               header = TRUE, stringsAsFactors = FALSE)

#---------------munging
killed %>% 
  .[country == "Israel and the Occupied Palestinian Territory", `:=`(country = "Israel")] %>% 
  .[country == "Democratic Republic of the Congo", `:=`(country = "DRC")]

#---------------munging
tmap_mode("view")

kil.tmp = killed %>%
  select(year, country, location, combinedStatus, fullName, gender, type, organizations, jobs, location) %>%
  arrange(desc(-year), country, fullName) %>%
  group_by(country) %>%
  count() %>% 
  as.data.frame()


#---------------munging
# world[world$name_long == "Democratic Republic of the Congo", "name_long"] == "DRC"

world.proj = world %>%
  select(name_long, continent, geom) %>%
  left_join(kil.tmp, by = c("name_long" = "country")) %>%
  na.omit()

colnames(world.proj)[4] = "Count"

map.Wrld = tm_shape(world.proj) + 
  tm_fill(col = "grey", lwd = 1) +
  tm_borders(col = "blue", lwd = 1) +
  tm_shape(filter(world.proj, Count>1)) + 
  tm_fill(col = "Count") +
  tm_borders(col = "blue", lwd = 1) +
  tm_layout(bg.color = "cyan", frame = TRUE, main.title ="Total Journalists Killed 1992 - 2019") +
  tm_text(text = "Count", col = "black")


#-------Data Section-------#
#-------end-------#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-------UI Section-------#
#---------------#

##notes for UI 
#the `fluidPage` function is where everything  goes
#even for all the different columns
#code sequence is super duper important 
#you'll do all of your aesthetics and `web design-y` shit here.

#this proves that you can execute code here 
#and it doesn't need to go into 

ui = fluidPage(theme = shinytheme("cosmo"),
               fluidRow(style = "background: light grey",
                        titlePanel("The Life and Death of a War Correspndant"),
                        strong("Journalists Killed between 1992 and 2019")),
               br(),
               
               #-------First Column-------#
               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
               column(5,
                      h2("Introduction"),
                      "In 2018, crimes committed in retribution for the work of journalists were brought to the forefront of global discussion by the highly publicized killing of Saudi Arabian dissident Jamal Khashoggi, a contributor to the Washington Post. Journalism is widely regarded as a critical component of a well-functioning democracy, because it provides accountability for a country's leaders. Violence against journalists as a factor that increases self-censorship and thus impair journalist ability to hold people in power accountable. In addition to the negative effect of press censorship on the wellbeing of democracies, this pointed violence shows an increase in repression of civil rights often occurs in the 2 years following the killing of a journalist.",
                      br(),
                      wellPanel(h3("100 Jounalists Deaths"),
                                "The plot below depicts the roughly 1,300 journalist deaths since 1992 as recorded CPJ. The vizualization standardizes to a total to 100 deaths.",
                                selectInput("typeInput",
                                             "Parse by...",
                                             choices = c("impunityMurder", "gender", "employedAs", "sourcesOfFire", "localOrForeign", "typeOfDeath")),
                                plotOutput("wafflePlot", width = "100%", height = 200),
                                br(),
                                plotlyOutput("selectionPlot", width = "100%", height = 300))),
               
               #-------Second Column-------#
               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
               column(7, 
                      tabsetPanel(type = "tabs",
                                  tabPanel("Map", 
                                           wellPanel(strong("The map below depicts the total number of journalists killed in each country"),
                                                     br(),
                                                     "The displayed counties can be changed by using `continent` drop down menu below.",
                                                     "The plot can be interacted with by:",
                                                     "zooming in (mouse roller scroll or legend option)",
                                                     "moved (mouse click and drag)",
                                                     "country information (`click` on country)",
                                                     "layer change (click on legned options)",
                                                     selectInput("continentInput", 
                                                                 "Continent",
                                                                 choices = c("Asia", "Africa", "Europe", "North America", "South America"),
                                                                 width = "25%"),
                                                     sliderInput("deathInput", "Deaths Displayed", 0, 200, c(0)), width = "50%"),
                                           leafletOutput("mapSelected", width = "100%", height = 600)),
                                  tabPanel("Time series", 
                                           wellPanel(strong("The map below depicts the total number of journalists killed in each country"),
                                                     br(),
                                                     "The plot below depicts the top 25 deadliest counties by cummlative measure annually.",
                                                     "The plot can be interacted with by:",
                                                     "activating the animation (press the paly button)",
                                                     "year statically changed (drag year slider)"),
                                           plotlyOutput("lollipop", width = "100%", height = 600)),
                                  
                                  tabPanel("Raw Data", DT::dataTableOutput("results"))))
               
)
?DT::dataTableOutput
#-------UI Section-------#
#-------end-------#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-------Server Section-------#
#---------------#

server = function(input, output) {
  
  #-------Waffl Plot-------#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output$wafflePlot <- renderPlot({
    cummWFFL = killed %>% 
      .[,.(.N), by = c("year", eval(input$typeInput[1]))] %>%  
      .[order(year)] %>%  
      .[order(-year)] %>% 
      dcast(paste("year~", input$typeInput[1]) %>%  
              as.formula(env = .GlobalEnv), value.var = "N", fill = 0) %>%  
      .[,-1] %>% 
      map_df(cumsum) %>%  
      data.table(year = killed$year %>%  
                   unique() %>% 
                   sort(),.)
    
    tmpIndex = cummWFFL[year == 2019,] %>% 
      .[, order(-.[which(rownames(.) == '1'), ]) ]
    
    cummWFFL %>% 
      .[,..tmpIndex] %>% 
      .[year == 2019, 2:ifelse(length(cummWFFL)<9, length(cummWFFL), 9)] %>%
      melt() %>%  
      .[,`:=`(value = as.integer(round(100*(value/sum(value)),0)))] %>% 
      dcast(.~variable) %>% 
      .[,-1] %>% 
      waffle(parts = ., 
             rows = 5, 
             reverse = FALSE,
             color = brewer.pal(n = length(.), name = "Paired"), 
             title = paste("If 100 Journalists were killed \nby", input$typeInput[1]),
             xlab=paste("1 square = 13 Journalists"), 
             legend_pos = "bottom")
    
  })

  #-------Selected Map-------#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output$mapSelected <- renderLeaflet({
    sb = subset(world.proj, continent == input$continentInput[1])
    
    tmap_leaflet(tm_shape(sb) + 
                   tm_fill(col = "grey", lwd = 1) +
                   tm_borders(col = "blue", lwd = 1) +
                   tm_shape(filter(sb, Count>input$deathInput[1])) + 
                   tm_fill(col = "Count") +
                   tm_borders(col = "blue", lwd = 1) +
                   tm_layout(bg.color = "cyan", frame = TRUE, main.title ="Asia") +
                   tm_text(text = "Count", col = "black"))
  })
  
  #-------Timeseries Lollopop Plot-------#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output$lollipop <- renderPlotly({
    llPlot = killed %>% 
      .[,.(.N), by = .(year, country)] %>%  
      dcast(year~country, value.var = "N", fill = 0) %>%  
      .[,-1] %>% 
      map_df(cumsum) %>%  
      data.table(year = killed$year %>%  
                   unique() %>% 
                   sort(),.) %>%
      melt(id.vars = "year") %>%
      .[,`:=`(rank = rank(value, "max")), by = .(year)] %>% 
      .[order(year, -rank, variable)] %>% 
      .[,`:=`(increase = value - lag(value, 1, default = 0)), by = .(variable)] %>%
      cbind(., rankSet =  rep(102:1, length(1992:2019))-(102-25)) %>% 
      .[rankSet > 0, ] %>% 
      set_names(c('Year', 'Country', 'Deaths', 'Rank', 'Increase', 'RankSet'))
    
    llPlot = llPlot %>% 
      ggplot() + 
      geom_segment(aes(x = 0, y = RankSet, xend = Deaths, yend = RankSet, frame = Year)) +
      geom_point(aes(x = Deaths, 
                     y = RankSet, 
                     frame = Year), color = "Red", size = 3) +
      geom_text(aes(x = Deaths+10, 
                     y = RankSet, 
                     label = Country, 
                     frame = Year), color = "Black", nudge_x = 4, check_overlap = TRUE) +
      theme(legend.position = "none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      labs(y = "Death Rank", x = "Total Deaths", title = "Top 25 Deadliest Countries for Journalists") +
      theme_classic() 

        ggplotly(llPlot)
  })
  
  #-------Cummlative Plots by Selection-------#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output$selectionPlot <- renderPlotly({
    
    cummWFFL = killed %>% 
      .[,.(.N), by = c("year", eval(input$typeInput[1]))] %>%  
      .[order(year)] %>%  
      .[order(-year)] %>% 
      dcast(paste("year~", input$typeInput[1]) %>%  
              as.formula(env = .GlobalEnv), value.var = "N", fill = 0) %>%  
      .[,-1] %>% 
      map_df(cumsum) %>%  
      data.table(year = killed$year %>%  
                   unique() %>% 
                   sort(),.)
    
    tmpIndex = cummWFFL[year == 2019,] %>% 
      .[, order(-.[which(rownames(.) == '1'), ]) ]
    
    cummWFFL = cummWFFL %>% 
      .[,..tmpIndex] %>% 
      .[,1:ifelse(length(cummWFFL)<9, length(cummWFFL), 9)] %>% 
      melt(id.var = c("year")) 
    
    ppp = cummWFFL %>% 
      ggplot() + 
      geom_line(aes(year, value, color = variable)) +
      scale_colour_manual(values = brewer.pal(n = nrow(unique(cummWFFL[,2])), name = "Paired")) + 
      labs(x = "Year", 
           y = "Death Totals", 
           title = paste("Cummlative Death \nCounts per year by", input$typeInput[1]), 
           color = paste(input$typeInput[1])) + 
      theme_classic()
    ggplotly(ppp)
  })

  output$results <- DT::renderDataTable({
    killed[,-c(2,4,6,7,9,10,12,14,19, 22, 24, 25, 28, 29, 30, 32:43) ] %>%  
      .[order(year, country,typeOfDeath)] 
  })
  
}

shinyApp(ui = ui, server = server)







  


















