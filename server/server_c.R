
tornadoe_tables_info_C = eventReactive( input$C_submit_button, {

  selected_time_format = input$C_Time_Format

  tornadoe_data_IL = tornadoe_data[ which( tornadoe_data$st == "IL" ), ]

  # table and chart showing the total number of tornadoes (and # and % in each magnitude, including 'unknown') for each year in the records and the overall totals
  c_prob_1_output_df = tornadoe_data_IL %>%
    mutate(total_tornado_count = n()) %>%
    group_by(yr,total_tornado_count) %>%
    summarize(total_number_of_tornadoes = n()) %>%
    mutate(each_year_percentage = round(total_number_of_tornadoes/total_tornado_count,4)) %>%
    select(-total_tornado_count) %>%
    rename(year = yr) %>%
    data.frame()

  # table and chart showing the total numbers (and # and % in each magnitude) per month summed over all years

  c_prob_2_output_df = tornadoe_data_IL %>%
    mutate(total_tornado_count = n()) %>%
    group_by(mo,total_tornado_count) %>%
    summarize(total_number_of_tornadoes_each_month = n()) %>%
    mutate(percentage_of_tornadoe_each_month = round(total_number_of_tornadoes_each_month/total_tornado_count,4)) %>%
    select(-total_tornado_count) %>%
    rename(Month = mo) %>%
    data.frame()

  # table and chart showing the total numbers (and # and % in each magnitude) per hour of the day summed over all years

  each_hr_totnadoe_freq_percent_df = tornadoe_data_IL %>%
    mutate(hour = as.numeric(substr(time,start = 1,stop = 2)),
           total_tornado_count = n()) %>%
    group_by(hour, total_tornado_count) %>%
    summarize(each_hour_total = n()) %>%
    mutate(each_hour_percentage = round(each_hour_total/total_tornado_count,4),
           hour_next = hour + 1,
           hour_text = sprintf("%02d",hour),
           hour_next_text = sprintf("%02d", hour_next),
           Format_24_Hour = paste0(hour_text,":00-",hour_next_text,":00"),
           hour_text12 = sprintf("%02d",hour%%12),
           hour_next_text12 = sprintf("%02d",hour_next%%12),
           Format_12_Hour = paste0(hour_text12,":00-",hour_next_text12,":00 ", ifelse(hour_next<12|hour_next==24, "AM","PM"))) %>%
    ungroup() %>%
    select(Format_24_Hour, Format_12_Hour, each_hour_total, each_hour_percentage)%>%
    data.frame()

  # table and chart showing the total numbers (and # and % in each magnitude) for a given distance range from Chicago summed over all years

c_prob_4_output_df =  tornadoe_data_IL %>%
   mutate(total_tornado_count = n()) %>%
   group_by(Distance = mag,total_tornado_count) %>%
   summarize(Tornadoe_Total = n()) %>%
   mutate(Tornadoe_Percentage = round(Tornadoe_Total/total_tornado_count,4)) %>%
   select(Distance, Tornadoe_Total, Tornadoe_Percentage) %>%
  data.frame()

  # table and chart showing the injuries, fatalities, loss for each year in the records

total_injuries_fatalities_loss_for_each_year_df = tornadoe_data_IL %>%
  group_by(Year = yr) %>%
  summarise(Total_Injuries = sum(inj, na.rm = T),
            Total_Fatalities = sum(fat, na.rm = T),
            Total_Loss = sum(loss, na.rm =T))%>%
  data.frame()

  # table and chart showing the injuries, fatalities, loss per month summed over all years

total_injuries_fatalities_loss_for_per_month_df = tornadoe_data_IL %>%
  group_by(Month = mo) %>%
  summarise(Total_Injuries = sum(inj, na.rm = T),
            Total_Fatalities = sum(fat, na.rm = T),
            Total_Loss = sum(loss, na.rm =T))%>%
  data.frame()

  # table and chart showing the injuries, fatalities, loss per hour of the day summed over all years

total_injuries_fatalities_loss_for_each_hr_df  = tornadoe_data_IL %>%
  mutate(hour = as.numeric(substr(time,start = 1,stop = 2))) %>%
  group_by(hour) %>%
  summarise(Total_Injuries = sum(inj, na.rm = T),
            Total_Fatalities = sum(fat, na.rm = T),
            Total_Loss = sum(loss, na.rm =T)) %>%
  mutate(hour_next = hour + 1,
         hour_text = sprintf("%02d",hour),
         hour_next_text = sprintf("%02d", hour_next),
         Format_24_Hour = paste0(hour_text,":00-",hour_next_text,":00"),
         hour_text12 = sprintf("%02d",hour%%12),
         hour_next_text12 = sprintf("%02d",hour_next%%12),
         Format_12_Hour = paste0(hour_text12,":00-",hour_next_text12,":00 ", ifelse(hour_next<12|hour_next==24, "AM","PM"))) %>%
  select(starts_with("Format",ignore.case = FALSE),starts_with("Total", ignore.case = FALSE))%>%
  data.frame()


  # table and chart showing which counties were most hit by tornadoes summed over all years
tornadoe_state_freq_df_arranged = tornadoe_data_IL %>%
    merge(County_IL_data %>%
            select(County = `County Name`, f1 = `FIPS County`) %>%
            mutate(f1 = as.numeric(f1)),
          by = "f1") %>%
    group_by(County) %>%
    summarize(Total_hit_by_Tornadoe = n()) %>%
  data.frame()


  # leaflet map showing all of the tornado tracks across Illinois
c_prob_8_lat_long_df = County_IL_data %>%
  merge(County_coordinates %>%
          select(County, lat, lon = lng),
          by.x = "County Name", by.y = "County") %>%
  data.frame()




  c_prob_8_lat_long_df$popup_var = paste0( 'County:', c_prob_8_lat_long_df_2$`County.Name`, 'State:', c_prob_8_lat_long_df_2$State )

  # output list

  output_dataframes = list( "c_prob_1_output_df" = c_prob_1_output_df, "c_prob_2_output_df" = c_prob_2_output_df,

                            "c_prob_3_output_df" = each_hr_totnadoe_freq_percent_df, "c_prob_4_output_df" = c_prob_4_output_df, "c_prob_5_output_df" = total_injuries_fatalities_loss_for_each_year_df,

                            "c_prob_6_output_df" = total_injuries_fatalities_loss_for_per_month_df, "c_prob_7_output_df" = total_injuries_fatalities_loss_for_each_hr_df,

                            "c_prob_8_output_df" = tornadoe_state_freq_df_arranged, "c_prob_9_output_df" = c_prob_8_lat_long_df_2 )

  return( output_dataframes )

} )
# tornadoe_tables_info_C = output_dataframes
# Tables

output$Total_Tornadoes_Each_Year = renderDataTable({
  if(input$show_tables_t)  {
    formatStyle(datatable(tornadoe_tables_info_C()$c_prob_1_output_df,
    options = list(pageLength = 10),
    rownames = FALSE),
    color = "black",columns = T)

  }
}
  )

output$Total_Tornadoes_Per_Month =  renderDataTable({

  if(input$show_tables_t)  {
    formatStyle(datatable(
  tornadoe_tables_info_C()$c_prob_2_output_df,
  options = list(pageLength = 10),
  rownames = FALSE),
  color = "black",columns = T)

  }})

output$Total_Tornadoes_Per_Hour =  renderDataTable({    if(input$show_tables_t)  {     formatStyle(datatable(

   tornadoe_tables_info_C()$c_prob_3_output_df[ , -2 ],
   options = list(pageLength = 10),
   rownames = FALSE),
   color = "black",columns = T)

}})

output$Total_Tornadoes_Given_Distance =  renderDataTable({    if(input$show_tables_t)  {     formatStyle(datatable(

  tornadoe_tables_info_C()$c_prob_4_output_df,
  options = list(pageLength = 10),
  rownames = FALSE),
  color = "black",columns = T)

}})

output$Injuries_Fatalities_Loss_Year =  renderDataTable({    if(input$show_tables_I)  {     formatStyle(datatable(

  tornadoe_tables_info_C()$c_prob_5_output_df,
  options = list(pageLength = 10),
  rownames = FALSE),
  color = "black",columns = T)

}})

output$Injuries_Fatalities_Loss_Month =  renderDataTable({    if(input$show_tables_I)  {     formatStyle(datatable(

  tornadoe_tables_info_C()$c_prob_6_output_df,
  options = list(pageLength = 10),
  rownames = FALSE),
  color = "black",columns = T)

}})

output$Injuries_Fatalities_Loss_Hour =  renderDataTable({    if(input$show_tables_I)  {     formatStyle(datatable(

  tornadoe_tables_info_C()$c_prob_7_output_df[ , -2 ],
  options = list(pageLength = 10),
  rownames = FALSE),
  color = "black",columns = T)

}})

output$Counties_Most_Hit_by_Tornadoes =  renderDataTable({    if(input$show_tables_C)  {     formatStyle(datatable(

  tornadoe_tables_info_C()$c_prob_8_output_df,
  options = list(pageLength = 10),
  rownames = FALSE),
  color = "black",columns = T)

}})

# scatter Plots
output$Total_Tornadoes_Given_Distance_Plot = renderPlotly({

  Total_Tornadoes_Given_Distance_Plot_data = tornadoe_tables_info_C()$c_prob_4_output_df

  Total_Tornadoes_Given_Distance_Plot_data$Distance = factor( Total_Tornadoes_Given_Distance_Plot_data$Distance, levels = Total_Tornadoes_Given_Distance_Plot_data$Distance )

  plot_ly( Total_Tornadoes_Given_Distance_Plot_data, x = ~Distance, y = ~Tornadoe_Total,size = ~Tornadoe_Total, name = 'Total Number of Tornadoes', type = 'scatter', mode = 'markers')%>%

    add_trace(y = ~Tornadoe_Percentage, name = 'Percentage for each Distance') %>%

    layout(title = "Total number of tornadoes by it's magnitude",
           xaxis = list(title = "Magnitude"),
           yaxis = list (title = "Total") )

})

output$Injuries_Fatalities_Loss_Year_Plot = renderPlotly({

  Injuries_Fatalities_Loss_Year_Plot_data = tornadoe_tables_info_C()$c_prob_5_output_df

  Injuries_Fatalities_Loss_Year_Plot_data$Year = factor( Injuries_Fatalities_Loss_Year_Plot_data$Year, levels = Injuries_Fatalities_Loss_Year_Plot_data$Year )

  req(input$ifl_year_plot)
  if(input$ifl_year_plot == "Injuries"){

  plot_ly( Injuries_Fatalities_Loss_Year_Plot_data,
           x = ~Year,
           y = ~Total_Injuries,
           size = ~Total_Injuries,
    name = 'Total Injuries Each Year',
    type = 'scatter',
    mode = 'markers' ,color = "red") %>% #change here

    layout(title = "Total Injuries",
           xaxis = list(title = ""),
           yaxis = list (title = "Total Injuries"))
  }
  else if(input$ifl_year_plot == "Fatalities"){

    plot_ly( Injuries_Fatalities_Loss_Year_Plot_data,
             x = ~Year,
             y = ~Total_Fatalities,
             size = ~Total_Fatalities,
             name = 'Total fatalities each year',
             type = 'scatter',
             mode = 'markers' ,color = '#ff9900') %>%    #change here

      layout(title = "Total Fatalities",
             xaxis = list(title = ""),
             yaxis = list (title = "Total Fatalities"))
  }
  else if(input$ifl_year_plot == "Loss"){

    plot_ly( Injuries_Fatalities_Loss_Year_Plot_data,
             x = ~Year,
             y = ~Total_Loss,
             size = ~Total_Loss,
             name = 'Total loss each year',
             type = 'scatter',
             mode = 'markers', color = '#ff9900') %>% #change here

      layout(title = "Total Loss",
             xaxis = list(title = ""),
             yaxis = list (title = "Total Loss"))
  }

})

output$Injuries_Fatalities_Loss_Month_Plot = renderPlotly({

  Injuries_Fatalities_Loss_Month_Plot_data = tornadoe_tables_info_C()$c_prob_6_output_df

  Injuries_Fatalities_Loss_Month_Plot_data$Month = factor( Injuries_Fatalities_Loss_Month_Plot_data$Month, levels = Injuries_Fatalities_Loss_Month_Plot_data$Month )

  req(input$ifl_month_plot)

  if(input$ifl_month_plot == "Injuries"){

  plot_ly( Injuries_Fatalities_Loss_Month_Plot_data,
           x = ~Month,
           y = ~Total_Injuries,
           size = ~Total_Injuries,
           name = 'Total injuries each month',
           type = 'scatter',
           mode = 'markers',color = "red" ) %>%  #change here

    layout(title = "Total Injuries",
           xaxis = list(title = "Month"),
           yaxis = list (title = "Total Injuries"))
}
else if(input$ifl_month_plot == "Fatalities"){

  plot_ly( Injuries_Fatalities_Loss_Month_Plot_data,
           x = ~Month,
           y = ~Total_Fatalities,
           size = ~Total_Fatalities,
           name = 'Total fatalities each month',
           type = 'scatter',
           mode = 'markers' ,color ="tomato" ) %>% #change here

    layout(title = "Total Fatalities",
           xaxis = list(title = "Month"),
           yaxis = list (title = "Total Fatalities"))
}
else if(input$ifl_month_plot == "Loss"){

  plot_ly( Injuries_Fatalities_Loss_Month_Plot_data,
           x = ~Month,
           y = ~Total_Loss,
           size = ~Total_Loss,
           name = 'Total loss each month',
           type = 'scatter',
           mode = 'markers',color = 'rgba(152, 0, 0, .8)' ) %>% #change here

    layout(title = "Total Loss",
           xaxis = list(title = "Month"),
           yaxis = list (title = "Total Loss"))
}

} )

output$Injuries_Fatalities_Loss_Hour_Plot = renderPlotly({

  time_format_type = input$C_Time_Format

  Injuries_Fatalities_Loss_Hour_Plot_data = tornadoe_tables_info_C()$c_prob_7_output_df

  Injuries_Fatalities_Loss_Hour_Plot_data$Format_24_Hour = factor( Injuries_Fatalities_Loss_Hour_Plot_data$Format_24_Hour, levels = Injuries_Fatalities_Loss_Hour_Plot_data$Format_24_Hour )

  Injuries_Fatalities_Loss_Hour_Plot_data$Format_12_Hour = factor( Injuries_Fatalities_Loss_Hour_Plot_data$Format_12_Hour, levels = Injuries_Fatalities_Loss_Hour_Plot_data$Format_12_Hour )

  req(input$ifl_hour_plot)

  if(time_format_type == "12_Hour") {
    Injuries_Fatalities_Loss_Hour_Plot_data$hour = Injuries_Fatalities_Loss_Hour_Plot_data$Format_12_Hour}
  else if((time_format_type == "24_Hour")) {
    Injuries_Fatalities_Loss_Hour_Plot_data$hour = Injuries_Fatalities_Loss_Hour_Plot_data$Format_24_Hour}
    if(input$ifl_hour_plot == "Injuries"){

      plot_ly( Injuries_Fatalities_Loss_Hour_Plot_data,
               x = ~hour,
               y = ~Total_Injuries,
               size = ~Total_Injuries,
               name = 'Total injuries each hour',
               type = 'scatter',
               mode = 'markers',color = "red" ) %>%  #change here

        layout(title = "Total Injuries",
               xaxis = list(title = "Hour"),
               yaxis = list (title = "Total Injuries"))
    }
    else if(input$ifl_hour_plot == "Fatalities"){

      plot_ly( Injuries_Fatalities_Loss_Hour_Plot_data,
               x = ~hour,
               y = ~Total_Fatalities,
               size = ~Total_Fatalities,
               name = 'Total fatalities each hour',
               type = 'scatter',
               mode = 'markers'  ,color = "green") %>%  #change here

        layout(title = "Total Fatalities",
               xaxis = list(title = "Hour"),
               yaxis = list (title = "Total Fatalities"))
    }
    else if(input$ifl_hour_plot == "Loss"){

      plot_ly( Injuries_Fatalities_Loss_Hour_Plot_data,
               x = ~hour,
               y = ~Total_Loss,
               size = ~Total_Loss,
               name = 'Total loss each hour',
               type = 'scatter',
               mode = 'markers',color = "red" ) %>%   #change here

        layout(title = "Total Loss",
               xaxis = list(title = "Hour"),
               yaxis = list (title = "Total Loss"))
    }


})

output$Counties_Most_Hit_by_Tornadoes_Plot = renderPlotly({

  Counties_Most_Hit_by_Tornadoes_Plot_data = tornadoe_tables_info_C()$c_prob_8_output_df

  Counties_Most_Hit_by_Tornadoes_Plot_data$County = factor( Counties_Most_Hit_by_Tornadoes_Plot_data$County, levels = Counties_Most_Hit_by_Tornadoes_Plot_data$County )

  plot_ly( Counties_Most_Hit_by_Tornadoes_Plot_data, x = ~County, y = ~Total_hit_by_Tornadoe,size = ~Total_hit_by_Tornadoe, name = 'Total hit by Tornadoe', type = 'scatter', mode = 'markers',color = 'rgb(255, 127, 80)')%>%

    layout(title = "Chart showing which counties were most hit by tornadoes summed over all years",
           xaxis = list(title = ""),
           yaxis = list (title = "") )

})

# Bar Plots

output$Total_Tornadoes_Each_Year_Barplot = renderPlotly({

  Total_Tornadoes_Each_Year_Plot_data = tornadoe_tables_info_C()$c_prob_1_output_df

  Total_Tornadoes_Each_Year_Plot_data$year = factor( Total_Tornadoes_Each_Year_Plot_data$year, levels = Total_Tornadoes_Each_Year_Plot_data$year )

  plot_ly( Total_Tornadoes_Each_Year_Plot_data, x = ~year, y = ~total_number_of_tornadoes, name = 'Total Number of Tornadoes', type = 'bar', marker = list(color = 'rgb(255, 127, 80)' ))%>%

    add_trace(y = ~each_year_percentage, name = 'Each Year Percentage', marker = list(color = 'rgb(22, 96, 167)' )) %>%

    layout(title = "Total number of tornadoes & % in each magnitude by year",
           xaxis = list(title = "Year"),
           yaxis = list (title = "Total") ) #added the barmode - change it if needed

})

output$Total_Tornadoes_Per_Month_Barplot = renderPlotly({

  Total_Tornadoes_Per_Month_Plot_data = tornadoe_tables_info_C()$c_prob_2_output_df

  Total_Tornadoes_Per_Month_Plot_data$Month = factor( Total_Tornadoes_Per_Month_Plot_data$Month, levels = Total_Tornadoes_Per_Month_Plot_data$Month )

  plot_ly( Total_Tornadoes_Per_Month_Plot_data, x = ~Month, y = ~total_number_of_tornadoes_each_month, name = 'Total Number of Tornadoes', type = 'bar', marker = list(color = 'rgb(255, 127, 80)' ))%>%

     add_trace(y = ~percentage_of_tornadoe_each_month, name = 'Each Month Percentage', marker = list(color = 'rgb(22, 96, 167)' )) %>%

    layout(title = "Total number of tornadoes & % in each magnitude per month ",
           xaxis = list(title = "Month"),
           yaxis = list (title = "Total") )

})

output$Total_Tornadoes_Per_Hour_Barplot = renderPlotly({

  time_format_type = input$C_Time_Format

  Total_Tornadoes_Per_Hour_Plot_data = tornadoe_tables_info_C()$c_prob_3_output_df

  Total_Tornadoes_Per_Hour_Plot_data$Format_24_Hour = factor( Total_Tornadoes_Per_Hour_Plot_data$Format_24_Hour, levels = Total_Tornadoes_Per_Hour_Plot_data$Format_24_Hour )

  Total_Tornadoes_Per_Hour_Plot_data$Format_12_Hour = factor( Total_Tornadoes_Per_Hour_Plot_data$Format_12_Hour, levels = Total_Tornadoes_Per_Hour_Plot_data$Format_12_Hour )

  switch( time_format_type,

          "24_Hour" = plot_ly( Total_Tornadoes_Per_Hour_Plot_data, x = ~Format_24_Hour, y = ~each_hour_total, size = ~each_hour_total, name = 'Each Hour Total', type = 'bar', marker = list(color = 'rgb(255, 127, 80)')) %>%

             add_trace(y = ~each_hour_percentage, name = 'Each Hour Percentage', marker = list(color = 'rgb(22, 96, 167)')) %>%

            layout(title = "Total numbers & % in each magnitude by hour of the day",
                   xaxis = list(title = ""),
                   yaxis = list ( title = "Total" ) ),

          "12_Hour" = plot_ly( Total_Tornadoes_Per_Hour_Plot_data, size = ~each_hour_total, x = ~Format_12_Hour, y = ~each_hour_total, name = 'Each Hour Total', type = 'bar', marker = list(color = 'rgb(255, 127, 80)')) %>%

            add_trace(y = ~each_hour_percentage, name = 'Each Hour Percentage', marker = list(color = 'rgb(22, 96, 167)')) %>%

            layout(title = "Total numbers & % in each magnitude by hour of the day",
                   xaxis = list(title = ""),
                   yaxis = list ( title = "Total" ) )

  )

})
# Leaflet plot

df.20 <- c_prob_8_lat_long_df_2
getColor <- function(c_prob_8_lat_long_df_2) {
  sapply(c_prob_8_lat_long_df_2$Total_hit_by_Tornadoe, function(Total_hit_by_Tornadoe) {
    if(Total_hit_by_Tornadoe <= 20) {
      "green"
    } else if(Total_hit_by_Tornadoe <= 60) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

output$Prob_9_Leaflet_Plot = renderLeaflet( {

  tryCatch({

leaflet(data = tornadoe_tables_info_C()$c_prob_9_output_df ) %>% addTiles() %>% addAwesomeMarkers(~lon, ~lat, icon = icons, label = ~as.character(Total_hit_by_Tornadoe)) %>% addLegend(
position = 'bottomright',
colors = c("green",  "orange", "red"),
labels = c("<=20", "20-60",">60"), opacity = 1,
title = 'Distance in miles'
)  %>% addScaleBar()

  }, error = function(error ){ } )

})
