source( 'SourceOfSource.R' )    #... Loading necessary libraries and files

ui = navbarPage(
  theme = shinytheme("sandstone"),

  title = 'Project-3',
  
  #..... Include ui for each tab .....
  
  source( file.path( "ui", "ui_c.R" ), local = T )$value,
  
  source( file.path( "ui", "ui_b.R" ), local = T )$value,
  
  source( file.path( "ui", "ui_a.R" ), local = T )$value,
  source( file.path( "ui", "about.R" ), local = T )$value
  
)

server = function( input, output, session ){
  
  #..... Include server logic for each tab .....
  options(warn = -1)
  source( file.path( 'server', 'server_c.R' ), local = T )$value 
  
  source( file.path( 'server', 'server_b.R' ), local = T )$value
 
  source( file.path( 'server', 'server_a.R' ), local = T )$value
  
}

shinyApp( ui = ui, server = server )

