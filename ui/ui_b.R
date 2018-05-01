# ....... ui tab B ......

tabPanel( 
  
  strong( 'In depth of Illinois' ),
  
  fluidRow(
#  column( 6, align="center", selectInput( "input_magnitude", label = "Magnitude", choices = list( 'Value' = 'Value' ) ) ,height = 10 ),
    
column( 12,  align="center", selectInput( "B_map_background", label = "Map Background", choices = list( 'Background' = 'Background' ) ), height = 10)),
  fluidRow(

    column( width = 12, align ="center",
 splitLayout(
            checkboxInput(inputId = "show_graphsB",
                          label = "Show graphs",
                          value = TRUE
            ),
            checkboxInput(inputId = "show_top10",
            label = "Show Top 10",
            value = FALSE
            )
        ,
            checkboxInput(inputId = "show_tablesB",
            label = "Table for Top 10 ",
            value = FALSE
            )
        )
        

    )
    
  ),
  
  br(),
  
  fluidRow( 

    conditionalPanel(
      condition = "input.show_graphsB == true",
column( 12, align="center", selectInput( "input_magnitude", label = "Magnitude", choices = list( 'Value' = 'Value' ) ) ,height = 10 ),

    column( 12, leafletOutput("mag_leaflet_map") ,height= 600)
    )
  ),
  
  br(),
fluidRow(
conditionalPanel(
    condition ="input.show_graphsB == true",
column( 6,align="center", selectInput( "B_filter_variable", label = "Filtering Variable", choices = list( 'Variable' = 'Variable' ) ),style = "width: 500px;" ),

column( 6,align = "center", sliderInput( "B_variable_value", label = "Variable Value", min = 0, max = 20, value = 10 ,sep=""), style = "width: 500px;" )

)
),
  
  fluidRow(


    conditionalPanel(
      condition = "input.show_graphsB == true",
      
    column( 12, leafletOutput("mymap",height = 600) )
    )
    ),
  
  br(),
  

  fluidRow( 
    
    conditionalPanel(
      condition = "input.show_top10 == true",
      column( 12, helpText( p( strong( 'Map showing the top 10 tornado(filter by it\'s length in miles)  in IL\'s location and view table for more details ' ) ) ) )   , #..... Heading
    column( 12, leafletOutput("B_last_prob_map", height = 700) )
    )
  ),
  
  br(),
  
  fluidRow(
    
    conditionalPanel(
      condition = "input.show_tablesB == true",
      
    column( 12, dataTableOutput( "B_last_prob_table" ,height = 600) )
    )
  )
  
  )
