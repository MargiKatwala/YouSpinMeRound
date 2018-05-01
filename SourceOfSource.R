suppressWarnings( library( shiny ) )
suppressWarnings( library( ggplot2 ) )
suppressWarnings( library( ggthemes ) )
suppressWarnings( library( scales ) )
suppressWarnings( library( dplyr ) )
suppressWarnings( library( plotly ) )
suppressWarnings( library( data.table ) )
suppressWarnings( library( ggmap ) )
suppressWarnings( library( leaflet ) )
suppressWarnings( library( DT ) )
suppressWarnings( library( shinythemes ) )
suppressWarnings( library( shinyjs ) )
#suppressWarnings( library( plotly.graph_objs ) )


tornadoe_data = read.csv( "1950-2016_all_tornadoes.csv", stringsAsFactors = F )



County_IL_data = read.csv( "County_IL.csv", stringsAsFactors = F)
County_IL_data = County_IL_data[ -1, ]
names( County_IL_data ) = c( "State", "County Name", "FIPS State", "FIPS County" )

County_code_data = read.csv( "US_FIPS_Codes.csv", stringsAsFactors = F )
County_code_data = County_code_data[ -1, ]
names( County_code_data ) = c( "State", "County Name", "FIPS State", "FIPS County" )

County_coordinates = fread("County_Coordinates_IL.csv")
setnames(County_coordinates, c("CountyState","lat","lng"))
County_coordinates[,County := gsub(" Illinois","",CountyState)]

# setting up the required dataframe


  ilinois_tornado_df <- tornadoe_data %>% dplyr::filter(stf == 17)
  xy <- merge(ilinois_tornado_df, County_IL_data, by.x = "f1", by.y = "FIPS County")
  xy$CountyState = paste(xy$`County Name`, "Illinois")
  xyz <- merge(xy, County_coordinates, by.x = "CountyState", by.y = "CountyState" )
  
  
 

State_coordinates = fread("US_state_coordinates.csv")
State_coordinates[,State := gsub("U.S. ","",CountryState)]

c_prob_8_lat_long_df = County_IL_data %>%
  merge(County_coordinates %>%
          select(County, lat, lon = lng),
        by.x = "County Name", by.y = "County") %>%
  data.frame()

tornadoe_data_IL = tornadoe_data[ which( tornadoe_data$st == "IL" ), ]

tornadoe_state_freq_df_arranged = tornadoe_data_IL %>%
  merge(County_IL_data %>%
          select(County = `County Name`, f1 = `FIPS County`) %>%
          mutate(f1 = as.numeric(f1)),
        by = "f1") %>%
  group_by(County) %>%
  summarize(Total_hit_by_Tornadoe = n()) %>%
  data.frame()

c_prob_8_lat_long_df_2 = c_prob_8_lat_long_df %>%
  merge(tornadoe_state_freq_df_arranged %>%
          select(Total_hit_by_Tornadoe, County),
        by.y = "County", by.x = "County.Name") %>%
  data.frame()

c_prob_8_lat_long_df_2 = c_prob_8_lat_long_df_2[, c(1,2,5,6,7)]
ay <- list(
  tickfont = list(color = "steelblue"),
  overlaying = "y",
  side = "right",
  title = "Percentage"
)
# ,marker = list(color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width = 2))
