tabPanel(
  strong( 'About' ),
  fluidRow(tabName = "info",
          h1("Project 3 - You Spin Me Round"),
          h1("Authors: Aditya Sinha, Margi Katwala and Vignan Thmmu"),
          h1("Libraries used: Shiny, Ggplot2, Ggthemes, Scales, Dplyr,Plotly, Data.table,Ggmap,Leaflet,DT,Shinythemes & Shinyjs")
       
  ),
  br(),
  fluidRow(tabName = "Explore Finding the Unknown",
           h3("   Explore our first tab: Find the Unknown",align = "center"),
           p(""),
           h4("     This tab will explore different aspects of a tornado over past 65 years in the state of Illinois. Depending on the user's wish to see the graph they can check the box and the graph will be displayed accordingly. The first three graph tells us about how many tornados by year, month and by an hour. Next, there is a drop-down menu item to choose injuries, loss, and fatalities which are represented by a scatter plot. Lastly, on this page, there is a scatter plot to represent counties that were most hit and taking the length a map showing most counties that are in danger of getting hit by a tornado.",
              style = "font-family: 'Source Sans Pro';",style = "color:blue")
           
  ),
  br(),
  fluidRow(tabName = "Map of IL",
           h3("   Explore our second tab: In Depth of Illinois",align = "center"),
           p(""),
           h4("   This tab will have three leaflet map separated based on its functionality. The first map filters tornado in IL based on its magnitude while second map filter based on year, length, loss, width and etc. The third map is to find out which are the top 8 counties on the map of IL. The third map is to find out which are the top 8 counties on the map of IL.", style = "font-family: 'Source Sans Pro';",style = "color:blue")
           
  ),
  br(),
  fluidRow(tabName = "Compare and Learn",
           h3("   Explore our third tab: Compare and Learn",align = "center"),
           p(""),
           h4("   This tab will have a way to compare data from state to state. The set data table is of IL and user has an option to choose the other than they would like to compare IL with. The last thing we decided to find out a way to format the whole U.S tornado's data based on its year selected by the user.", style = "font-family: 'Source Sans Pro';",style = "color:blue")
           
           )
)
