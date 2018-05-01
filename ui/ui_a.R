# ....... ui tab A ......

tabPanel( 

strong(  ' Compare and Learn' ),

fluidRow(

column( 12, align = "center",selectInput( "A_map_background", label = "Map Background", choices = list( 'Background' = 'Background' ) ) )),
fluidRow(
column( width = 12,align ="center",
splitLayout(
checkboxInput(inputId = "show_graphsA",
label = "Show Graph by Year",
value = FALSE
),
checkboxInput(inputId = "show_tablesA",
label = "Show tables",
value = FALSE
),
checkboxInput(inputId = "show_counties",
label = "Show Details about County",
value = FALSE
)
) #split layout ending
)#col ending

),

br(),

fluidRow(

conditionalPanel(
condition = "input.show_tablesA == true",column( 12, helpText( p( strong( 'Illinois tabular data' ) ) ) ) ,   #..... Heading


column( 12, dataTableOutput( "IL_table_output" ) )
)
),
br(),


fluidRow(

conditionalPanel(
condition = "input.show_tablesA == true",
column( 12, helpText( p( strong( 'Other State tabular data' ) ) ) )  ,  #..... Heading,
column( 12,align="center", selectInput( "A_state", label = "State", choices = list( 'state' = 'state' ) ) ),
column( 12, dataTableOutput( "compare_table_output" ) )
)
),



br(),

fluidRow(

conditionalPanel(
condition = "input.show_counties == true",

strong( ' Map of IL showing different data based on counties' ),
column( 12, leafletOutput( "A_prob_1_leaflet" ) )

)),
br(),

fluidRow(

conditionalPanel(
condition = "input.show_graphsA == true",
column( 2, selectInput( "A_input_year", label = "Year", choices = list( 'year' = 'year' ) ) ),
strong( ' Map and table showing for tornoados in whole U.S' ),
column( 12, leafletOutput("A_mymap") )
)
),

br(),

fluidRow(

conditionalPanel(
condition = "input.show_graphsA == true",

column( 12, dataTableOutput( "selected_year_table" ) )
)
)



)

