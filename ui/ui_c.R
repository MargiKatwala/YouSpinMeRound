# ....... ui tab C ......

tabPanel( 
  
  strong( 'Find the Unknown' ),
  
  fluidRow(
    
    column( 12, helpText( p( strong( ' ' ) ) ) )    #..... Heading
    
  ),
  fluidRow(
    
    column( width = 6, align="center",
            selectInput( inputId = "C_Time_Format", 
                            label = "Select time format", 
                            choices = c( "12 Hour" = "12_Hour", 
                                         "24 Hour" = "24_Hour" ) ,
                            selected= "12 Hour")),
    column( 6, align = "center",actionButton( 'C_submit_button', strong( 'Submit' )) )
    ),
    fluidRow(
        column(12, align = "center",
            splitLayout(
            checkboxInput(inputId = "show_graphs",
                          label = "Show Total Tornadoes ",
                          value = FALSE
                ),
            checkboxInput(inputId = "show_tables_t",
            label = "Show tables Total Tornadoes",
            value = FALSE
            )),

            splitLayout(
            checkboxInput(inputId = "show_inj",
            label = "Show Injuries/Loss and Fatalities",
            value = FALSE
            )
            ,
            checkboxInput(inputId = "show_tables_I",
            label = "Show tables for Injuries/Loss and Fatalties",
            value = FALSE
            )),

            splitLayout(
            checkboxInput(inputId = "show_count",
            label = "Show Counties",
            value = FALSE
            )
            ,
            checkboxInput(inputId = "show_tables_C",
            label = "Show tables for Counties",
            value = FALSE
            )

            )
)),

  
  fluidRow(

    conditionalPanel(
      condition = "input.show_graphs == true",
    column( 12, plotlyOutput( "Total_Tornadoes_Each_Year_Barplot" ) )
  
    )
  ),
  

  fluidRow(
    
    column( 12, dataTableOutput( "Total_Tornadoes_Each_Year" ))
    
  ),
  

  fluidRow(

    conditionalPanel(
      condition = "input.show_graphs == true",
    column( 12, plotlyOutput( "Total_Tornadoes_Per_Month_Barplot" ) )
    )
    
  ),
  

  
  fluidRow(
    
    column( 12, dataTableOutput( "Total_Tornadoes_Per_Month" ))
    
  ),


  fluidRow(

    conditionalPanel(
      condition = "input.show_graphs == true",
    column( 12, plotlyOutput( "Total_Tornadoes_Per_Hour_Barplot" ) )
    )
  ),

  fluidRow(
    
    column( 12, dataTableOutput( "Total_Tornadoes_Per_Hour" ) )
    
  ),
  

  
  fluidRow(
    conditionalPanel(
      condition = "input.show_graphs == true",
      
    column( 12, plotlyOutput( "Total_Tornadoes_Given_Distance_Plot" ) , height = 6)
    )
  ),
  
  fluidRow(
    
    column( 12, dataTableOutput( "Total_Tornadoes_Given_Distance" ), height = 6 )
    
  ),
  


  fluidRow(

    conditionalPanel(
      condition = "input.show_inj == true",
       strong( ' Injuries/Loss/Fatalities : By Year' ),
    column(2,selectInput(inputId = "ifl_year_plot",label = "Select input:",choices = c("Injuries", "Fatalities", "Loss"), selected = "Injuries")),
                
                
    column( 12, plotlyOutput( "Injuries_Fatalities_Loss_Year_Plot" ) , height = 6)
    )
  ),


  
  fluidRow(
    
    column( 12, dataTableOutput( "Injuries_Fatalities_Loss_Year" ), height = 6 )
    
  ),
  

  
  fluidRow(

    conditionalPanel(
      condition = "input.show_inj == true",
    strong( ' Injuries/Loss/Fatalities : By Month' ),
    column(2,selectInput(inputId = "ifl_month_plot",label = "Select input:",choices = c("Injuries", "Fatalities", "Loss"), selected = "Injuries")),
    column( 12, plotlyOutput( "Injuries_Fatalities_Loss_Month_Plot" ) )
    )
  ),



  fluidRow(
    
    column( 12, dataTableOutput( "Injuries_Fatalities_Loss_Month" ))
    
  ),
  

  
  fluidRow(
    conditionalPanel(
      condition = "input.show_inj == true",
    strong( ' Injuries/Loss/Fatalities : By Hour' ),
   column(2, selectInput(inputId = "ifl_hour_plot",label = "Select input:",choices = c("Injuries", "Fatalities", "Loss"), selected = "Injuries")),
    column( 12, plotlyOutput( "Injuries_Fatalities_Loss_Hour_Plot" ) )
    )
  ),

  
  fluidRow(


    column( 12, dataTableOutput( "Injuries_Fatalities_Loss_Hour" ) )

  ),
  

fluidRow(

  conditionalPanel(
    condition = "input.show_count == true",

  column( 12, plotlyOutput( "Counties_Most_Hit_by_Tornadoes_Plot" ))
  )
),



  fluidRow(

    column( 12, dataTableOutput( "Counties_Most_Hit_by_Tornadoes" ) )
    
  ),


  
  fluidRow(

    conditionalPanel(
      condition = "input.show_count == true",
       strong( ' Map showing the different distance of tornado based on counties' ),
    column( 12, leafletOutput("Prob_9_Leaflet_Plot") , height = 700)
    )
  ) 
  
)
