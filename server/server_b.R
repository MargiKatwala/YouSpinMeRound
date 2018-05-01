
# Updating Filtering Variable Select Button

observe({
    x<- sort(xyz$mag) # sort the magnitude for the display 
  all_magnitude = unique(x)
  
  updateSelectInput(session, 'input_magnitude', choices = all_magnitude, selected = all_magnitude[1] )
  
})

all_magnitude_0 = reactive( {input$input_magnitude } )

observe({
  
  updateSelectInput(session, 'B_filter_variable', choices = c( 'Year', 'Width', 'Length (miles)',
                                                                
                                                                'Injuries', 'Fatalities', 'Loss'), selected = 'Year' )
  
})

selected_variable_0 = reactive({input$B_filter_variable })

observe({
  
  in_selected_variable = selected_variable_0()
  
  tryCatch({
    
    in_parameters = switch( in_selected_variable,
                            
                            "Year" = unique(xyz$yr ),      
                            
                            "Width" = unique(xyz$wid ),     
                            
                            "Length (miles)" = unique(xyz$len ),
                            
                            "Injuries" = unique(xyz$inj ),      
                            
                            "Fatalities" = unique(xyz$fat ),     
                            
                            "Loss" = unique(xyz$loss )
                            
    )
    
    updateSliderInput(session, 'B_variable_value', value = in_parameters, min = min( in_parameters ), max = max( in_parameters ) )

  }, error = function(cond ){ updateSliderInput(session, 'B_variable_value', value = 'd', min = 'd', max = 'd' ) } )
  
})

selected_variable_value_0 = reactive( {input$B_variable_value } )

observe({
  
  B_type_background = c( "NatGeoWorldMap", "WorldImagery", "OceanBasemap", "WorldPhysical", "WorldStreetMap" )
  
  updateSelectInput( session, 'B_map_background', choices = B_type_background, selected = B_type_background[1] )
  
})

B_selected_background_0 = reactive( { input$B_map_background } )


# Preparaing dataframe for leaflet 

leaflet_data_fr_mag = reactive({ 
   #selected_magnitude =  3
  
  selected_magnitude = all_magnitude_0()
  
  county = unique( xyz[ which( xyz$mag == selected_magnitude ), "County" ] )
  
  mag_city_state_df = data.frame( county = county, state = rep( "IL", length( county ) ) )
  
  mag_lat_long_df = merge(mag_city_state_df, xyz[, c("lng", "lat", "County")], by.x = "county", by.y = "County", all.x = TRUE)
  
  mag_lat_long_df$popup_var = paste0( 'County: ', mag_lat_long_df$county, " ", 'Magnitude: ', selected_magnitude )
  
  return( mag_lat_long_df )
  
  })







leaflet_data = reactive( {
  
  selected_variable = selected_variable_0(); selected_variable_value = selected_variable_value_0()
  
  county = switch( selected_variable,
                   
                   "Year" = unique( xyz[ which( xyz$yr == selected_variable_value ), "County" ] ),      
                   
                   "Magnitude" = unique( xyz[ which( xyz$mag == selected_variable_value ), "County" ] ),   
                   
                   "Width" = unique( xyz[ which( xyz$wid == selected_variable_value ), "County" ] ),     
                   
                   "Length (miles)" = unique( xyz[ which( xyz$len == selected_variable_value ), "County" ] ),
                   
                   "Injuries" = unique( xyz[ which( xyz$inj == selected_variable_value ), "County" ] ),      
                   
                   "Fatalities" = unique( xyz[ which( xyz$fat == selected_variable_value ), "County" ] ),     
                   
                   "Loss" = unique( xyz[ which( xyz$loss == selected_variable_value ), "County" ] )
                   
  )
  
  city_state_df = data.frame( county = county, state = "IL" )
  
  lat_long_df = merge(city_state_df, County_coordinates[,.(lon = lng, lat, County)], by.x = "county",by.y = "County", all.x = TRUE)
  
  lat_long_df$popup_var = paste0( 'County: ', lat_long_df$county )
  
  return( lat_long_df )
  
} )


# show data (and path on the leaflet map) for any of the 10 most powerful / destructive Illinois tornadoes (you should pick the appropriate metric and defend it)

B_last_prob_df = reactive( {

  destructive_df <- xyz[, c(-1,-2,-3,-31,-33,-36)] %>% dplyr::filter(stf == 17) %>%
    dplyr::top_n(n = 10, wt = len) %>%
    dplyr::arrange(desc(len))
  #destructive_df = tornadoe_data[ order( tornadoe_data$mag, decreasing = T )[1:10], ]

  destructive_city_state_df = data.frame( county = unique( destructive_df$`County Name` ),
                                          state = rep( "IL", length( unique( destructive_df$`County Name` ) ) ) )

  destructive_lat_long_df = merge(destructive_city_state_df, xyz[, c("lng", "lat", "County")], by.x = "county", by.y = "County", all.x = TRUE)

  destructive_lat_long_df$popup_var = paste0( 'County: ', destructive_lat_long_df$county )

  return( list( 'leaflet_req_df' = destructive_lat_long_df, 'destructive_table' = destructive_df ) )

  } )

output$mag_leaflet_map = renderLeaflet( {
  
  B_selected_background = B_selected_background_0()
  
  B_background_type = switch( B_selected_background,
                              
                              "NatGeoWorldMap" = "Esri.NatGeoWorldMap",      
                              
                              "WorldImagery" = "Esri.WorldImagery",   
                              
                              "OceanBasemap" = "Esri.OceanBasemap",     
                              
                              "WorldPhysical" = "Esri.WorldPhysical",    
                              
                              "WorldStreetMap" = "Esri.WorldStreetMap"    
                              
  )
  
  tryCatch({
    
    leaflet(data = leaflet_data_fr_mag() ) %>% addTiles() %>% addMarkers(~lng, ~lat, popup = ~as.character(popup_var),
                                                                         label = ~as.character(popup_var) ) %>% 
      addProviderTiles( B_background_type ) %>%
      addMeasure() %>% addScaleBar()
    
  }, error = function( error ){ } )
  
})

output$mymap = renderLeaflet( {
  
  B_selected_background = B_selected_background_0()
  
  B_background_type = switch( B_selected_background,
                              
                              "NatGeoWorldMap" = "Esri.NatGeoWorldMap",      
                              
                              "WorldImagery" = "Esri.WorldImagery",   
                              
                              "OceanBasemap" = "Esri.OceanBasemap",     
                              
                              "WorldPhysical" = "Esri.WorldPhysical",    
                              
                              "WorldStreetMap" = "Esri.WorldStreetMap"    
                              
  )
  
  tryCatch({
   ## leaflet_data()$lat_long_df
    
  leaflet(data = leaflet_data() ) %>% addTiles() %>% addMarkers(~lon, ~lat, popup = ~as.character(popup_var), 
                                                                label = ~as.character(popup_var) ) %>% 
      addProviderTiles( B_background_type ) %>%
      addMeasure() %>% addScaleBar()
      
  
  }, error = function( error ){ } )
    
})

output$B_last_prob_map = renderLeaflet( {
  
  B_selected_background_1 = B_selected_background_0()
  
  B_background_type_1 = switch( B_selected_background_1,
                              
                              "NatGeoWorldMap" = "Esri.NatGeoWorldMap",      
                              
                              "WorldImagery" = "Esri.WorldImagery",   
                              
                              "OceanBasemap" = "Esri.OceanBasemap",     
                              
                              "WorldPhysical" = "Esri.WorldPhysical",    
                              
                              "WorldStreetMap" = "Esri.WorldStreetMap"    
                              
  )
  
  tryCatch({
    
    leaflet(data = B_last_prob_df()$leaflet_req_df ) %>% addTiles() %>% addMarkers(~lng, ~lat, popup = ~as.character(popup_var), 
                                                                                   label = ~as.character(popup_var) ) %>% 
      addProviderTiles( B_background_type_1 ) %>%
      addMeasure() %>% addScaleBar()
      
    
  }, error = function( error ){ } )
  
} )

output$B_last_prob_table = renderDataTable( {

  B_last_prob_df()$destructive_table

},options = list(
  pageLength = 10
) )
