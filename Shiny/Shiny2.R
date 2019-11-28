library(pacman)
p_load(shiny, shinydashboard, DT, here,shinythemes,dplyr,shinythemes,ggthemes,ggthemr,ggplot2,plotly,htmlwidgets,reshape2)

pathname<-"/Users/dianshen/Library/Mobile Documents/com~apple~CloudDocs/19Fall/TimeSeriesinR:Python/Project/"
#fill all missing data with ImputeTS
ts_au <-read_csv(paste(pathname,"ts_au.csv", sep = ""))
lonlat <-read_csv(paste(pathname,"lonlat.csv", sep = ""))
AU <- map_data("world") %>% filter(region=="Australia")
ts_au <- melt(ts_au, id.vars = c("Date","Location"))
ui <-fluidPage(theme = shinytheme("flatly"),
               titlePanel("Australia Weather"),
               sidebarLayout(
                 sidebarPanel(
                   h3("Locatin"),
                   selectInput(inputId = "Location", label = strong("locatoin"),
                               choices = unique(ts_au$Location),
                               selected = "Sydney"),
                   selectInput(inputId = "Variable", label = strong("Variable"),
                               choices = unique(ts_au$variable),
                               selected = "MaxTemp"),
                   dateRangeInput("date", strong("Date"), start = min(ts_au$Date), end = max(ts_au$Date),
                                  min = min(ts_au$Date), max = max(ts_au$Date)),
                   checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                   
                   # Display only if the smoother is checked
                   conditionalPanel(condition = "input.smoother == true",
                                    sliderInput(inputId = "f", label = "Smoother span:",
                                                min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                animate = animationOptions(interval = 100)),
                                    HTML("Higher values give more smoothness.")
                                    
                                    
                   ),
                   dateInput("date_map",strong("Date for interactive map"),value = min(ts_au$Date))
                 ),
                 mainPanel(
                   #plotOutput(outputId = "lineplot", height = "300px"),
                   plotOutput(outputId = "ggplot", height = "300px"),
                   plotlyOutput(outputId = "map",height = "600px")
                 ))
)
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    ts_au %>%mutate(Date = as.Date(Date))%>%
      filter(
        Location == input$Location,
        variable == input$Variable,
        Date > as.Date(input$date[1]) & Date < as.Date(input$date[2]
        ))%>%
      mutate( mytext=paste("Date: ", Date, "\n",
                           paste(input$Variable,": "), value, sep=""))
  })
  ts_aur <- reactive({
    ts_au_temp<-ts_au%>%mutate(Location = paste("Australia",as.character(Location)))%>%
      filter(variable == input$Variable,Date==as.Date(input$date_map))%>%
      dplyr::select(Location,value)
    lonlat%>%left_join(ts_au_temp,by="Location")%>%
      mutate( mytext=paste("City: ", Location, "\n",
                           paste(input$Variable,": "), value, sep="")
      )
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends()$Date, y = selected_trends()$value, type = "l",
         xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$Date), y = selected_trends()$value, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  output$ggplot <- renderPlot({
    ggthemr("fresh")
    p<-ggplot(selected_trends(),aes(selected_trends()$Date, selected_trends()$value))+geom_line()+geom_point()+xlab("Date")+
      ylab(input$Variable)+ggtitle(paste("The",input$Variable, " between", input$date[1],"and",input$date[2]))
    # Display only if smoother is checked
    if(input$smoother){
      p<-p+geom_smooth(method = "loess", span = input$f)
    }
    print(p)
  })
  output$map <- renderPlotly({
    p<-ggplot() +
      geom_polygon(data = AU, aes(x=long, y = lat, fill = subregion, group = as.factor(group)), color ="black",fill="gray", alpha=0.3) +
      geom_point(data=ts_aur(), aes(x=lon, y=lat,color=value,size = value, text=mytext),alpha = .4) +theme_void()+
      coord_map()+theme(legend.position= "right")+scale_fill_gradient(trans = "log10") +labs(color = input$Variable)
    ggplotly(p, tooltip="text")
  })
  
}
shinyApp(ui = ui, server = server)
