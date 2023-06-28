library(shiny)
library(ggplot2)
library(forecast)
library(smooth)

load("coalfield.Rdata")

ui <- fluidPage(

    titlePanel(span(img(src = "logo.png", height = 55),"Singrauli Coalfield Pollution Dashboard")),

    sidebarLayout(
        sidebarPanel(
          
          
          selectInput(
            "mode",
            "Display",
            c("Raw data"= "raw", "Interpolated data" = "int", "Predictions" ="pred", "Decomposed"= "decomp", "ACF/PACF" = "acf", "AQI" = "aqi"),
            selected = "Predictions",
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          selectInput(
            "plt",
            "Pollutant",
            c("PM10" = "PM10", "PM2.5" = "PM2.5", "NO"= "NO", "NO2"="NO2", "NOx"="NOx", "CO" = "CO", "SO2"="SO2", "NH3"="NH3", "Ozone"="Ozone"),
            selected = "Ozone",
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          uiOutput("tf"),
          uiOutput("date"),
          tags$a(href = "https://arqamrp.github.io", "Arqam Patel"),
          tags$h6("2023")
        ),

        
        mainPanel(

          uiOutput("method"),
          plotOutput("distPlot"),
          verbatimTextOutput("value") 
        )
    )
)


server <- function(input, output) {
    
  output$tf<- renderUI({
  if(input$mode %in% c("raw", "int", "aqi")){
    selectizeInput(
    "timeframe",
    "Time frame",
    c("Weekly" = "wk", "Daily" = "dl")
  )}
    })
  
  output$date<-renderUI({
    if(input$mode %in% c("raw", "int", "aqi")){ dateInput(
    "date",
    "Date",
    value = "2023/02/16",
    min = "2023/02/01",
    max = "2023/05/31",
    format = "dd-mm-yyyy",
    startview = "month",
  )}
    })
  
    output$method <- renderUI({
      if( input$mode == "pred" ){
        selectizeInput("method",
                     "Method",
                     c("ARIMA" = "ARIMA", "Complex exponential Smoothing" = "CES","MSARIMA"="MSARIMA"))
      }
      else if( input$mode == "int" ){
        selectizeInput("method",
                       "Method",
                       c("Cubic spline" = "cubic", "Linear" = "lin"))
      }
      else if(input$mode == "decomp"){
        selectizeInput("method",
                       "Method",
                       c("Loess" = "stl", "Moving averages" = "ma"))
      }
      
    })
    
    
    output$distPlot <- renderPlot({
      
      x = input$plt
      date0 = as.POSIXct(input$date, format = "%a %b %e %H:%M:%S %Y")
      start <- 96*(as.numeric(date0) -   as.numeric(series[1,1]))/(as.numeric(series[97,1]) - as.numeric(series[1,1])) -21
      if(x == "CO") unit <- "(mg/m3)"
      else unit <- "(microgram/m3)"
      
      
      if(input$timeframe == "dl" ){
        range <- start:(start+95)
        db = "6 hours"
        dt <- as.character(input$date)
      }
      else if(input$timeframe == "wk" ){
          range <- start:(start+671)
          db = "1 day"
          dt <- paste("for week starting", as.character(input$date))
      }
      
      if(input$mode == "raw" ){
        ggplot(data = series[range,])+ geom_line(aes(DateTime, series[range,x]),  col = "orange")+
          scale_x_datetime(date_labels = "%d %B %T", date_breaks = db) +
          xlab("") + ylab(paste(x, "levels")) + labs(title = paste("Recorded", x, "levels", dt))
      }
      else if (input$mode == "aqi"){
        ggplot(data = series[range,])+ geom_line(aes(DateTime, aqi_vec[range]),  col = "brown")+
          scale_x_datetime(date_labels = "%d %B %T", date_breaks = db) +
          xlab("") + ylab(paste("AQI")) + labs(title = paste("AQI", dt))
      }
      else if(input$mode == "acf"){
        idx <- which(colnames(series) == input$plt)
        ts = ts_list[[idx]]
        par(mfrow = c(2,1)) #2 graphs in a single device
        acf(ts, 192, main = paste("ACF for", input$plt), panel.first = grid ())
        pacf(ts, 192, main = paste("PACF for", input$plt), panel.first = grid ())
      }
      else if(input$mode == "decomp" & input$method == "ma"){
        idx <- which(colnames(series) == input$plt)
        plot(decomp_list[[idx]], col = "red")
      }
      else if(input$mode == "decomp" & input$method == "stl"){
        idx <- which(colnames(series) == input$plt)
        plot(stl_list[[idx]], col = "red")
      }
      else if(input$mode == "int" & input$method == "lin"){
          ggplot(data = series[range,])+ geom_line(aes(DateTime, series[range,x]), col = "orange")+
            geom_line(aes(DateTime, linterpol[range,x]), col = "blue")+
            scale_x_datetime(date_labels = "%d %B %T", date_breaks = db) +
            xlab("") + ylab(paste(x, "level", unit)) + labs(title = paste("Linearly interpolated",x, "levels", dt))
        }
      else if(input$mode == "int" & input$method == "cubic"){
          ggplot(data = series[range,])+ geom_line(aes(DateTime, series[range,x]), col = "orange")+
            geom_line(aes(DateTime, cinterpol[range,x]), col = "blue")+
            scale_x_datetime(date_labels = "%d %B %T", date_breaks = db) +
            xlab("") + ylab(paste(x, "level", unit)) + labs(title = paste("Cubic spline interpolated", x, "levels", dt))
      }
      else if(input$mode == "pred" & input$method == "ARIMA")
      {
        idx <- which(colnames(series) == input$plt)
        model <- model_list[[idx]]
        next_day <- forecast(model, h= 192)
        filter <- next_day$mean
        ggplot(data = test)+ geom_line(aes(DateTime, test[[idx]]), col = "darkgreen")+
          geom_line(aes(DateTime, filter), col = "red")+
          scale_x_datetime(date_labels = "%d %B %T", date_breaks = "6 hours") +
          xlab("") + ylab(paste(input$plt)) + labs(title = paste( "ARIMA forecast of", x, "levels"))
        
        # plot(test[[idx]], type = "l", col ="darkgreen",  ylab = input$plt, main = "ARIMA") +lines(y= filter, x = 1:192, col ="red")
      }
      else if(input$mode == "pred" & input$method == "CES")
      {
        idx <- which(colnames(series) == input$plt)
        model <- auto_ces[[idx]]
        next_day <- forecast(model, h= 192)
        filter <- next_day$mean
        
        ggplot(data = test)+ geom_line(aes(DateTime, test[[idx]]), col = "darkgreen")+
          geom_line(aes(DateTime, filter), col = "red")+
          scale_x_datetime(date_labels = "%d %B %T", date_breaks = "6 hours") +
          xlab("") + ylab(paste(input$plt)) + labs(title = paste("CES forecast of", x, "levels"))
        
        # plot(test[[idx]], type = "l", col ="darkgreen", ylab = input$plt, main = "CES" ) +lines(y= filter, x = 1:192, col ="red")
      
      }
      else if(input$mode == "pred" & input$method == "MSARIMA")
      {
        idx <- which(colnames(series) == input$plt)
        model <- auto_msarima[[idx]]
        next_day <- forecast(model, h= 192)
        filter <- next_day$mean
        
         ggplot(data = test)+ geom_line(aes(DateTime, test[[idx]]), col = "darkgreen")+
          geom_line(aes(DateTime, filter), col = "red")+
          scale_x_datetime(date_labels = "%d %B %T", date_breaks = "6 hours") +
          xlab("") + ylab(paste(input$plt)) + labs(title = paste( "MSARIMA forecast of", x, "levels"))


        # plot(test[[idx]], type = "l", col ="darkgreen", ylab = input$plt, main = "MSARIMA") +lines(y= filter, x = 1:192, col ="red")
      }
    })
    
    output$value <- renderPrint({
      
      idx <- which(colnames(series) == input$plt)
      if( input$mode != "pred") model = ""
      else if(input$mode == "pred" & input$method == "CES"){
        model <- auto_ces[[idx]]
        print(model)
      }
      else if(input$mode == "pred" & input$method == "ARIMA"){
        model <- model_list[[idx]]
        print(model)
      }
      else if(input$mode == "pred" & input$method == "MSARIMA"){
        model <- auto_msarima[[idx]]
        print(model)
      }

    })
    
    
}


shinyApp(ui = ui, server = server)
