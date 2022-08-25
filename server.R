library(shiny)

shinyServer(function(input, output) {
    
  

  output$plotIspuAll <- renderDygraph({
    
    ispuAll <- xts(ispu_complete$ispu_max, order.by = ispu_complete$tanggal)
    
    plot_ispuAll <- dygraph(ispuAll, main = "Indeks Standar Pencemar Udara (ISPU) 2016 - 2021", ylab = NULL) %>% 
      dyAxis("y", label = NULL, valueRange = c(0,315) ) %>%
      dySeries("V1", label = "Nilai ISPU", color = "#014d4d") %>% 
      dyLegend(show = "follow", hideOnMouseOut = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5), hideOnMouseOut = TRUE) %>% 
      dyLimit(0, color = "#019106", label = "BAIK", labelLoc = "right") %>% 
      dyLimit(51, color = "#030191", label = "SEDANG", labelLoc = "right") %>% 
      dyLimit(101, color = "#916601", label = "TIDAK SEHAT", labelLoc = "right") %>% 
      dyLimit(201, color = "#910101", label = "SANGAT TIDAK SEHAT", labelLoc = "right") %>% 
      dyLimit(301, color = "black", label = "BERBAHAYA", labelLoc = "right") %>%
      dyEvent("2021-02-01", label = "PM25 tersedia", labelLoc = "bottom", color = "gray") %>%
      dyShading(from = 0, to = 50, color = "#CCEBD6", axis = "y") %>%
      dyShading(from = 51, to = 100, color = "#cccfeb", axis = "y") %>%
      dyShading(from = 101, to = 200, color = "#f7da79", axis = "y") %>%
      dyShading(from = 201, to = 300, color = "#f79579", axis = "y") %>%
      dyRangeSelector(dateWindow = c("2021-01-01", "2021-12-31"))
    
    plot_ispuAll
    
  })
  
  
  
  output$baik <- renderText({
    
    inputTahun <- ispu_select %>% filter(tahun == input$input_ispuTahun)
    inputTahun$BAIK
    
    })
  
  output$sedang <- renderText({
    
    inputTahun <- ispu_select %>% filter(tahun == input$input_ispuTahun)
    inputTahun$SEDANG
    
  })
  
  output$tidaksehat <- renderText({
    
    inputTahun <- ispu_select %>% filter(tahun == input$input_ispuTahun)
    inputTahun$`TIDAK SEHAT`
    
  })
  
  output$stidaksehat <- renderText({
    
    inputTahun <- ispu_select %>% filter(tahun == input$input_ispuTahun)
    inputTahun$`SANGAT TIDAK SEHAT`
    
  })
  
  output$ranking <- renderPlotly({
    plot_barYearly <- allParam %>% 
      group_by(year(tahun)) %>% 
      mutate(tahun = as.factor(year(tahun)),
             ranking = rank(-Values),
             label = glue("{Parameter} : {scales::comma(Values)}")) %>% 
      ggplot(mapping = aes(x=as.factor(tahun), y=Values,
                           fill=Parameter, 
                           text = label, 
                           group = ranking)) +
      geom_bar(position = "dodge", stat = "identity", na.rm = TRUE) +
      scale_fill_manual(
        values = c(
          PM10 = "yellow",
          PM25 = "red",
          SO2 = "#3f35d4",
          CO = "#6bbdd1",
          O3 = "orange",
          NO2 = "brown")) +
      labs(
        title = "Ranking Parameter ISPU 2016 - 2021",
        x = "Tahun",
        y = "Nilai Parameter (\u03BCg/m\u00B3)",
      ) +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.margin = margin(1,1,1.5,1.2, "cm"),
            legend.position = "none")
    
    ggplotly(plot_barYearly, tooltip = "text")
    
  })
  
  output$plotAnnualParameter <- renderPlotly({
    
    plot_allParam <- allParam %>% 
      ggplot(mapping = aes(x = tahun, y = Values, color = Parameter, group = Parameter)) +
      geom_line() +
      geom_point(aes(tahun, text = label)) +
      scale_color_manual(
        values = c(
          PM10 = "yellow",
          PM25 = "red",
          SO2 = "#3f35d4",
          CO = "#6bbdd1",
          O3 = "orange",
          NO2 = "brown")) +
      scale_y_continuous(label = scales::comma) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(
        title = "Rata-rata Parameter Tahunan",
        x = "Tahun",
        y = "Nilai Parameter (\u03BCg/m\u00B3)"
      ) +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.title.position = "plot",
            plot.margin = margin(1,1,1.5,1.2, "cm"))
    
    ggplotly(plot_allParam %+% subset(allParam, Parameter %in% input$annualParameter), tooltip = "text")
    
  })
  
  output$plotTrenParameter <- renderDygraph({
    
    parameterTren <- xts(ispu_complete[,input$input_parameter], order.by = ispu_complete$tanggal)
    
    
    threshold <- case_when(input$input_parameter == "pm10" ~ 50,
                           input$input_parameter == "pm25" ~ 55.4,
                           input$input_parameter == "so2" ~ 180,
                           input$input_parameter == "o3" ~ 235,
                           input$input_parameter == "no2" ~ 200,
                           TRUE ~ 0)
    
    plot_parameterTren <- dygraph(parameterTren, ylab = NULL) %>% 
      dyAxis("y", label = NULL, valueRange = c(0,250)) %>%
      dySeries("V1", label = input$input_parameter, color = "#014d4d") %>% 
      dyLegend(show = "follow", hideOnMouseOut = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5), hideOnMouseOut = TRUE) %>% 
      dyLimit(threshold, color = "red", label = glue("Ambang Batas Harian = {threshold}"), labelLoc = "right") %>% 
      dyRangeSelector(dateWindow = c("2021-01-01", "2021-12-31"))
    
    plot_parameterTren
    
  })
  
  output$data <- renderDataTable({ 
    
    DT::datatable(ispu_complete %>%  select(-c(ispu_pm10,ispu_o3,ispu_so2,ispu_co,ispu_no2,ispu_pm25)))
    
  })
  

})
