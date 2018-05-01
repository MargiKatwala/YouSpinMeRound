tornadoe_data_IL_A_0 = reactive({ 
  
  tornadoe_data[ which( tornadoe_data$st == "IL" ), ]
  
})

all_selected_state_data = reactive({
  
  tornadoe_data_IL_A = tornadoe_data_IL_A_0()
  
  all_county_1 = unique( tornadoe_data_IL_A$f1 )
  
  all_county_names = unlist( lapply( all_county_1, function( curr_county ){
    
    County = County_IL_data[ which( County_IL_data$`FIPS County` == curr_county ), "County Name" ]
    
    #if( length( County ) != 0 ){ County = County } else{ County = "Not Found" }
    
    return( County )
    
  } ) )
  
  A_prob_1_df = lapply( all_county_names, function( county_name ){
    
    selected_county_code = County_IL_data[ which( County_IL_data$`County Name` == county_name ), "FIPS County" ]
    
    current_county_data = tornadoe_data_IL_A[ which( tornadoe_data_IL_A$f1 == selected_county_code ), c( "date", "time", "st", "inj",
                                                                                                         
                                                                                                         "fat", "loss", "mag" ) ]
    
    county_inj = unique( current_county_data$inj ); county_inj_text = paste0( county_inj, collapse = "," )
    
    county_fat = unique( current_county_data$fat ); county_fat_text = paste0( county_fat, collapse = "," )
    
    county_loss = unique( current_county_data$loss ); county_loss_text = paste0( county_loss, collapse = "," )
    
    county_mag = unique( current_county_data$mag ); county_mag_text = paste0( county_mag, collapse = "," )
    
    require_df = data_frame( County = county_name, State = "IL", Injuries = county_inj_text, Fatalities = county_fat_text, Loss = county_loss_text )
    
    return( require_df )
    
  } )
  
  A_prob_1_leaflet_df = bind_rows( A_prob_1_df )
  A_prob_1_lat_long_df = merge(A_prob_1_leaflet_df, County_coordinates[,.(County, lon = lng, lat)], by = "County", all.x = T)
  A_prob_1_lat_long_df$popup_var = paste0( 'County:', A_prob_1_lat_long_df$County,"::","Injuries:",A_prob_1_lat_long_df$Injuries,
                                           
                                           "Fatalities:",A_prob_1_lat_long_df$Fatalities, "Loss:",A_prob_1_lat_long_df$Loss  )
  
  return( A_prob_1_lat_long_df )

})

observe({
  x<-sort(tornadoe_data$st)
  all_states = unique( x )
 
  all_states_except_IL = all_states[ -which(all_states == "IL" ) ]
  
  updateSelectInput( session, 'A_state', choices = all_states_except_IL, selected = all_states_except_IL[1] )
  
})

selected_state_0 = reactive( { input$A_state } )

IL_nd_compare_table = reactive( { 
  
  selected_state = selected_state_0()
  
  selected_state_table = tornadoe_data[ which( tornadoe_data$st == selected_state ), -c( 1,2,3,4,25,26,27,28,29 ) ]
  
  IL_data_table = tornadoe_data[ which( tornadoe_data$st == "IL" ), -c( 1,2,3,4,25,26,27,28,29 ) ]
  
  list( 'IL_table' = IL_data_table, 'compare_table' = selected_state_table )
  
  } )

observe({
  
  all_years = unique( tornadoe_data$yr )
  
  updateSelectInput( session, 'A_input_year', choices = all_years, selected = all_years[1] )
  
})

all_selected_year_0 = reactive( { input$A_input_year } )

selected_yr_table = reactive( {
  
  all_selected_year = all_selected_year_0()
  
  selected_yr_df = tornadoe_data[ which( tornadoe_data$yr == all_selected_year ), ]
  
  return( selected_yr_df )
  
  } )

yr_leaflet_data = reactive( { 
  
  A_city_state_df = data.frame( state = unique( selected_yr_table()$st ), country = rep( "U.S.", length( unique( selected_yr_table()$st ) ) ) )
  
  
  A_lat_long_df = merge(A_city_state_df, State_coordinates[,.(state = State, lon = lng, lat)], by = "state", all.x = T)
  
  A_lat_long_df$popup_var = paste0( 'State:', A_lat_long_df$state )
  
  return( A_lat_long_df )
  
  } )

observe({
  
  all_type_background = c( "NatGeoWorldMap", "WorldImagery", "OceanBasemap", "WorldPhysical", "WorldStreetMap" )
  
  updateSelectInput( session, 'A_map_background', choices = all_type_background, selected = all_type_background[1] )
  
})

selected_background_0 = reactive( { input$A_map_background } )

# change below 

output$A_prob_1_leaflet = renderLeaflet( {
  
  selected_background = selected_background_0()
  
  A_background_type = switch( selected_background,
                              
                              "NatGeoWorldMap" = "Esri.NatGeoWorldMap",      
                              
                              "WorldImagery" = "Esri.WorldImagery",   
                              
                              "OceanBasemap" = "Esri.OceanBasemap",     
                              
                              "WorldPhysical" = "Esri.WorldPhysical",    
                              
                              "WorldStreetMap" = "Esri.WorldStreetMap"    
                              
  )
  
  tryCatch({
    
    leaflet( data = all_selected_state_data() ) %>% addTiles() %>% addCircleMarkers(~lon, ~lat, popup = ~as.character(popup_var), label = ~as.character(popup_var) ) %>% addProviderTiles( A_background_type )
    
  }, error = function( error ){message(error) } )
  
})

output$IL_table_output = renderDataTable( { 
  
  IL_nd_compare_table()$IL_table
  
},options = list(
  pageLength = 10
) )


output$compare_table_output = renderDataTable( { 
  
  IL_nd_compare_table()$compare_table
  
},options = list(
  pageLength = 10
) )

output$selected_year_table = renderDataTable( { 
  
  selected_yr_table()
  
},options = list(
  pageLength = 10
) )

output$A_mymap <- renderLeaflet( {
  
  selected_background = selected_background_0()
  
  A_background_type = switch( selected_background,
                       
                       "NatGeoWorldMap" = "Esri.NatGeoWorldMap",      
                       
                       "WorldImagery" = "Esri.WorldImagery",   
                       
                       "OceanBasemap" = "Esri.OceanBasemap",     
                       
                       "WorldPhysical" = "Esri.WorldPhysical",    
                       
                       "WorldStreetMap" = "Esri.WorldStreetMap"    
                          
  )
  
  tryCatch({
    
    leaflet( data = yr_leaflet_data() ) %>% addTiles() %>% addCircleMarkers(~lon, ~lat, popup = ~as.character(popup_var), label = ~as.character(popup_var) ) %>% addProviderTiles( A_background_type )

  }, error = function( error ){message(error) } )
  
})


