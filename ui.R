library(shiny)

shinyUI(fluidPage(
  h4("Dispersal governs the reorganization of ecological networks under environmental change - interactive figures"),
  h5("Patrick L. Thompson and Andrew Gonzalez"),
  h6("Nature Ecology & Evolution 2017 https://www.nature.com/articles/s41559-017-0162"),
  fluidRow(
    column(3,
           wellPanel(
             selectInput("response","Network property",choices=list("Link density" = "LD", "Species (nodes)" = "N","Links" = "Ltot","Connectance" = "C", "Compartmentalization" = "Cbar","Trophic levels" = "Trophic_levels"),selected = "LD"),
             selectInput("com_type", "Community",choices = list("Food web" = 3,"Competitive" = 1, "Mixed" = 2),selected = 3),
             selectInput("disp_select","Dispersal rate",choices = list("0.0005" = 1,"0.001" = 2, "0.01" = 3,"0.1" = 4),selected = 2),
             numericInput("i_patch","Initial patch (pixel with black outline)",value = 85,min = 51,max=150),
             radioButtons("contrast","Contrast (pixel with white outline)",choices = list("Same environment" = 50,"Same patch"= 0),selected = 50),
             sliderInput("f_time","Contrast time",min = 2000,max = 7000,step = 200,value = 7000,round=T,animate = F)
           )
    ),
    column(5,
           plotOutput("heatmap",width = "100%"),
           p("This is an interactive version of the manuscript figures 2 and 4. Users can  select an initial patch and then contrast how the network associated with a that environment or patch reorganizes as the environment changes. This contrast differs from the one used in the manuscript in that it does not contrast the most similar networks through time, because this calculation would make the app too slow. However, our reported results are quantitatively similar to the the contrast within a given environmental condition. Users can select different community types, dispersal rates, and network properties. ")
    ),
    column(4,
           plotOutput("netfig", width="100%"),
           plotOutput("legend")
    )
  )
))