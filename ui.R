library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(3,
           wellPanel(
             selectInput("response","Network measure",choices=list("Link density" = "LD", "Species (nodes)" = "N","Links" = "Ltot","Connectance" = "C", "Trophic levels" = "Trophic_levels"),selected = "LD"),
             selectInput("com_type", "Community",choices = list("Food web" = 3,"Competitive" = 1, "Mixed" = 2),selected = 3),
             selectInput("disp_select","Dispersal rate",choices = list("Low - 0.001" = 1,"Intermediate - 0.01" = 2, "High - 0.05" = 3),selected = 2),
             numericInput("i_patch","Initial patch (pixel with white outline)",value = 85,min = 51,max=150),
             radioButtons("contrast","Contrast (pixel with black outline)",choices = list("Same patch"= 0,"Same climate" = 50),selected = 50),
             sliderInput("f_time","Contrast time",min = 2000,max = 7000,step = 200,value = 7000,round=T,animate = T),
             "press play button (above right) to animate through time"
             
           )
  ),
  column(5,
         plotOutput("heatmap",width = "100%")
         ),
  column(4,
         plotOutput("netfig", width="100%"),
         plotOutput("legend")
         )
  )
))