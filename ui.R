library(shiny)
library(shinythemes)


navbarPage(
  "Select a Page to Explore!", # Title of the navigation bar
  tabPanel(
    "Home Page", 
    fluidPage(
      div(style = "text-align: center;",
        tags$img(src = "bigfoot-landing-page.jpg", width = "900px"),
        tags$h1("Welcome to Bigfoot Sightings"),
        tags$h2("Have you ever wondered where Bigfoot has been found? 
        Want to know where to look next? Well, you've come to the right place!")), 

      tags$head(
        tags$style(HTML("
      @keyframes slide {
        0% {
          left: -100px);
        }
        100% {
          left: 100%;
        }
      }
      
      #moving-image {
        position: fixed;
        top: 50%;
        left: -100px;
        animation: slide 3.6s ease-in-out forwards;
        z-index: 9999;
      }
    "))
      ),
      
      
      tags$audio(src = "growl-and-roar-102417.mp3", 
                 autoplay = "autoplay",
                 type = "audio/mpeg"),
      
      
      tags$img(id = "moving-image", 
               src = "new-bigfoot-image-removebg-preview.png",
               width = "300px"),
      
      
      # website: https://pixabay.com/sound-effects/search/bigfoot/
      # https://en.wikipedia.org/wiki/Bigfoot
      # https://www.shutterstock.com/search/big-foot-creature
    )
    
    
  ), #tabPanel for the main page,
  # Word cloud for the words most used in the bigfoot sighting reports 
  
  tabPanel("WordCloud", 
           fluidPage(
             h2("WordCloud of Words Used to Describe BigFoot Sightings"),
             theme = shinytheme("darkly"),
             plotOutput("WordCloud", height = "auto"),
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("max_words",
                             "Select Amount of Words Shown:",
                             min = 1,
                             max = 100,
                             value = 50,
                             step = 1)
               ),
               mainPanel(
                 plotOutput("wordcloud")
               ) 
             ), 
             
           )#fluidPage
  ),#tabPanel for the word cloud
  
  tabPanel("Visualizations for Bigfoot Sightings",
           fluidPage(
             h3("Multiple Visualizations of Bigfoot Sightings"), 
             selectInput("plotChoice" , "Choose a Plot:",
                         choices = c("Sightings by Season", "Sightings by State", "Sightings by Temperature")),
             plotOutput("selectedPlot")
           )#fluidpage for visualizations page
           
  )#Tabpanel for visualizations page
  
)#navbarPage
