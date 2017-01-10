load("dataforShiny2.rda")

require(tidyverse)
require(gridExtra)
require(lubridate)
require(shiny)

ui <- fluidPage(titlePanel("MyLA311 Data Analysis by Team WeLikeData"),
                
                tabsetPanel(
                  tabPanel(title = "Distributions",
                           plotOutput("graph1"),
                           plotOutput("graph2"),
                           plotOutput("graph5"),
                           plotOutput("graph7"),
                           plotOutput("graph22")),
                  
                  tabPanel(title = "Trends",
                           plotOutput("graph6"),
                           actionButton("graph3", "By Percentage"),
                           plotOutput("graph8"),
                           plotOutput("graph21", height = 800)),
                  
                  tabPanel(title = "Distribution - Source & Type",
                           plotOutput("graph15"),
                           plotOutput("graph18"),
                           plotOutput("graph19")),
                  
                  tabPanel(title = "Trends - Source & Type",
                           plotOutput("graph16", height = 850),
                           plotOutput("graph17"),
                           plotOutput("graph20")),
                  
                  tabPanel(title = "Geographic Spread",
                           plotOutput("graph9")),
                  
                  tabPanel(title = "Geographic Analysis",
                           plotOutput("graph23", height = 800),
                           plotOutput("graph12"),
                           plotOutput("graph13"),
                           tableOutput("table1"),
                           plotOutput("graph14"),
                           tableOutput("table2"))
                )
)






########################################




server <- function(input, output) {
  
  output$graph1 <- renderPlot({
    ggplot(data_graph1, aes(x = reorder(Department.Name, perc), y =perc*100)) + geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      xlab("Department Names") +
      ylab("Percentage") +
      ggtitle("Distribution of Percentage of Calls (Top 7 Departments)") +
      theme(axis.text.y = element_text(size = 9, face = "bold")) +
      theme(axis.text.x = element_text(size = 9, face = "bold"))
  })
  
  output$graph2 <- renderPlot({
    ggplot(data_graph2, aes(x = reorder(Call.Resolution, perc), y = perc*100)) + geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      xlab("Call Resolution Type") +
      ylab("Percentage") +
      ggtitle("Distribution of Percentage of Calls (Top 6 Resolution Types)") +
      theme(axis.text.y = element_text(size = 9, face = "bold")) +
      theme(axis.text.x = element_text(size = 9, face = "bold"))
  })
  
  
  output$graph2 <- renderPlot({
    ggplot(data_graph2, aes(x = reorder(Call.Resolution, perc), y = perc*100)) + geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      xlab("Call Resolution Type") +
      ylab("Percentage") +
      ggtitle("Distribution of Percentage of Calls (Top 6 Resolution Types)") +
      theme(axis.text.y = element_text(size = 9, face = "bold")) +
      theme(axis.text.x = element_text(size = 9, face = "bold"))
  })
  
  
  eventReactive(input$graph3, {renderPlot({ggplot(data_graph3, aes(x = year, y =perc, color = Department.Name)) +
      geom_line(size = 1.5) + theme_bw() + ylim(0, 50) +
      xlab("Year") +
      ylab("Percentage of Calls") +
      theme(legend.title = element_blank())
  })})
  
  
  output$graph5 <- renderPlot({
    ggplot(data_graph5, aes(x = reorder(Service.Name, perc), y = perc*100)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      xlab("Type of Request") +
      ylab("Percentage") +
      ggtitle("Distribution of Percentage of Calls by Request Type") +
      theme(axis.text.y = element_text(size = 9, face = "bold")) +
      theme(axis.text.x = element_text(size = 9, face = "bold"))
  })
  
  
  output$graph6 <- renderPlot({
    ggplot(data_graph6, aes(x = factor(month), y = Department.Name, fill = requests)) +
      geom_tile() + scale_fill_gradient(low = "white", high = "darkred") +
      facet_wrap(~year) +
      xlab("Month") +
      ylab("Department Name") +
      ggtitle("Trend of Calls by Department and Year") +
      theme(axis.text.y = element_text(size = 9, face = "bold")) +
      theme(axis.text.x = element_text(size = 9, face = "bold", angle = 40)) +
      theme(legend.title = element_blank())
  })
  
  output$graph7 <- renderPlot({
    ggplot(combined, aes(x = reorder(Department.Name, perc), y = perc*100)) +
      geom_bar(stat="identity", fill = "steelblue") +
      coord_flip() +
      xlab("Department Name") +
      ylab("Percentage") +
      ggtitle("Proportion of Service Requests processed against Total Calls") +
      theme(axis.text.y = element_text(size = 9, face = "bold")) +
      theme(axis.text.x = element_text(size = 9, face = "bold"))
  })
  
  output$graph8 <- renderPlot({
    d1 <- data_graph7 %>%
      ggplot(aes(x = weekday, y = hour, fill = Requests)) +
      geom_tile() + scale_fill_gradient(low = "white", high = "purple") +
      xlab("") + ylab("") +
      scale_y_continuous(limits = c(0, 23), breaks = seq(0, 23, 1)) +
      theme(legend.position = "none")
    d2 <- data_graph8 %>%
      ggplot(aes(x = weekday, y = hour, fill = Requests)) +
      geom_tile() + scale_fill_gradient(low = "white", high = "purple") +
      xlab("") +
      ylab("")+
      #ylab("Hour of Day") +
      scale_y_continuous(limits = c(0, 23), breaks = seq(0, 23, 1)) +
      theme(legend.position = "none")
    d3 <- data_graph9 %>%
      ggplot(aes(x = weekday, y = hour, fill = Requests)) +
      geom_tile() + scale_fill_gradient(low = "white", high = "purple") +
      xlab("") +
      ylab("")+
      #ylab("Hour of Day") +
      scale_y_continuous(limits = c(0, 23), breaks = seq(0, 23, 1))
    d4 <- data_graph10 %>%
      ggplot(aes(x = weekday, y = hour, fill = Requests)) +
      geom_tile() + scale_fill_gradient(low = "white", high = "purple") +
      xlab("") + ylab("") +
      scale_y_continuous(limits = c(0, 23), breaks = seq(0, 23, 1)) +
      theme(legend.position = "none")
    d5 <- data_graph11 %>%
      ggplot(aes(x = weekday, y = hour, fill = Requests)) +
      geom_tile() + scale_fill_gradient(low = "white", high = "purple") +
      xlab("") + 
      ylab("") + #ylab("Hour of Day") +
      scale_y_continuous(limits = c(0, 23), breaks = seq(0, 23, 1)) +
      theme(legend.position = "none")
    
    grid.arrange(d1, d2, d3, d4, d5, nrow = 2, ncol = 3, top = "Log-Scaled Distribution of Calls by Day of Week and Hour")
  })
  
  output$graph9 <- renderPlot({
    LA +
      geom_tile(data = mapdat2, aes(Lon2, Lat2, fill = Requests), alpha = 1) +
      scale_fill_continuous(low = "white", high = "darkred") +
      ggtitle("Geographic Overview of Service Requests")
  })
  
  output$graph12 <- renderPlot({
    ggplot(data_graph12, aes(x = RequestType, y = PolicePrecinct, fill = Requests)) + geom_tile() +
      scale_fill_continuous(low = "white", high = "darkred") +
      xlab("Request Type") +
      ylab("Police Precinct") +
      ggtitle("Call Volume across Police Precinct") +
      theme(axis.text.y = element_text(size = 9, face = "bold")) +
      theme(axis.text.x = element_text(size = 9, face = "bold"))
  })
  
  output$graph13 <- renderPlot({
    ggplot(x, aes(x = cluster, y = perc * 100, fill = cluster)) + geom_boxplot() +
      xlab("Zipcode Cluster") + ylab("Percentage") +
      ggtitle("Percentage of Calls with Service Request Processed") +
      theme_bw()
  })
  
  output$table1 <- renderTable({
    table(x$cluster)
  })
  
  output$graph14 <- renderPlot({
    LA +
      geom_tile(data = zippositioncluster2,
                aes(x = Lon2, y = Lat2, fill = avg)) +
      scale_fill_continuous(low = "darkred", high = "white") +
      ggtitle("Percentage of Calls with Service Request Processed")
  })
  
  
  output$table2 <- renderTable({
    tapply(zippositioncluster$perc, zippositioncluster$cluster, mean)
  })
  
  output$graph15 <- renderPlot({
    xy1 <- ggplot(percent_part, aes(x = RequestType, y = percent, fill = type)) +
      geom_bar(position = "dodge", stat = "identity") +
      ggtitle("Percent of Service Requests for each Request Source") +
      ylab("Frequency") +
      xlab("Request Type") +
      theme_light() + coord_flip() +
      scale_fill_manual(values = c("blue", "darkred"))
    xy2 <- ggplot(percent_whole, aes(x = RequestType, y = percent, fill = type)) +
      geom_bar(position = "dodge", stat = "identity") +
      ggtitle("Percent of Service Requests compared to Total Requests") +
      ylab("Frequency") +
      xlab("Request Type") +
      theme_light() + coord_flip() +
      scale_fill_manual(values = c("blue", "darkred"))
    grid.arrange(xy1, xy2)
  })
  
  
  output$graph16 <- renderPlot({
    appmap <- data_graph13 %>%
      ggplot(aes(x = daycreated, y = factor(timecreated))) +
      geom_tile(aes(fill = count)) +
      scale_fill_gradient(low = "white",
                          high = "steelblue") +
      ggtitle("Heatmap of Service Requests by App") +
      theme_light() +
      ylab("Time of Day") +
      xlab("Day of Week")
    
    phonemap <- data_graph14 %>%
      ggplot(aes(x = daycreated, y = factor(timecreated))) +
      geom_tile(aes(fill = count)) +
      scale_fill_gradient(low = "white",
                          high = "steelblue") +
      ggtitle("Heatmap of Service Requests by Phone") +
      ylab("Time of Day") +
      xlab("Day of Week") +
      theme_light()
    
    appgraffitimap <- data_graph15 %>%
      ggplot(aes(x = daycreated, y = factor(timecreated))) +
      geom_tile(aes(fill = count)) +
      scale_fill_gradient(low = "white",
                          high = "darkred") +
      ggtitle("Heatmap of Graffiti Removal Requests by App") +
      ylab("Time of Day") +
      xlab("Day of Week") +
      theme_light()
    
    phonegraffitimap <- data_graph16 %>%
      ggplot(aes(x = daycreated, y = factor(timecreated))) +
      geom_tile(aes(fill = count)) +
      scale_fill_gradient(low = "white",
                          high = "darkred") +
      ggtitle("Heatmap of Graffiti Removal Requests by Phone") +
      ylab("Time of Day") +
      xlab("Day of Week") +
      theme_light()
    
    appnograffitimap <- data_graph17 %>%
      ggplot(aes(x = daycreated, y = factor(timecreated))) +
      geom_tile(aes(fill = count)) +
      scale_fill_gradient(low = "white",
                          high = "green") +
      ggtitle("Heatmap of Non-Graffiti Removal Requests by App") +
      ylab("Time of Day") +
      xlab("Day of Week") +
      theme_light()
    
    phonenograffitimap <- data_graph18 %>%
      ggplot(aes(x = daycreated, y = factor(timecreated))) +
      geom_tile(aes(fill = count)) +
      scale_fill_gradient(low = "white",
                          high = "green") +
      ggtitle("Heatmap of Non-Graffiti Removal Requests by Phone") +
      ylab("Time of Day") +
      xlab("Day of Week") +
      theme_light()
    
    grid.arrange(appmap, phonemap, appgraffitimap,
                 phonegraffitimap, appnograffitimap,
                 phonenograffitimap, ncol = 2)
  }, height = 800)
  
  
  output$graph17 <- renderPlot({
    ggplot(timetrend_phonevsapp_final, 
           aes(x = factor(month), y = percent*100, fill = type))+
      geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.7)+
      ggtitle("Percent of Requests by Phone vs App for 2016")+
      xlab("Month in 2016")+
      ylab("Percent of Requests")
  })
  
  output$graph18 <- renderPlot({
    graffit_byrequestsource =
      data_graph19 %>%
      ggplot(aes(x=reorder(RequestSource, -count), y = count/1000))+
      geom_bar(stat = "identity", fill = "darkred")+
      xlab("Request Source")+
      ggtitle("Graffiti Removal Requests by Source")+
      ylab("Graffiti Removal Requests (in 1000s)")
    bulkyitem_byrequestsource =
      data_graph20 %>%
      ggplot(aes(x=reorder(RequestSource, -count), y = count/1000))+
      geom_bar(stat = "identity", fill = "darkred")+
      xlab("Request Source")+
      ggtitle("Bulky Item Removal Requests by Source")+
      ylab("Bulky Item Removal Requests (in 1000s)")
    
    grid.arrange(bulkyitem_byrequestsource,graffit_byrequestsource, nrow =1)
  })
  
  
  output$graph19 <- renderPlot({
    a = ggplot(timetrend_requesttype, aes(x = factor(month), y = count, fill = RequestType))+
      geom_bar(stat = "identity", position = "dodge", width = 0.7)+
      ggtitle("Service Requests by Request Type - 2016")+
      xlab("Month")+
      ylab("Requests")
    b = ggplot(timetrend_requesttype_merged, aes(x = factor(month), y = percent, fill = RequestType))+
      geom_bar(stat = "identity", position = "dodge", width = 0.7)+
      ggtitle("Request Type Percentage Breakdown by Month - 2016")+
      xlab("Month")+
      ylab("Percent")
    
    grid.arrange(a,b,nrow=1)
  })
  
  
  output$graph20 <- renderPlot({
    ggplot(data_graph21, aes(x=serviceefficiency, y = n, fill = type))+
      geom_bar(stat= "identity", position = "dodge", width = 0.5)+
      xlim(c(0, 8))+
      xlab("Time to Service Request in Days")+
      ylab("Requests")+
      ggtitle("Service Efficiency by Request Source")
  })
  
  
  output$graph21 <- renderPlot({
    p1 <- ggplot(data_graph22, aes(factor(year), percent*100, group = Call.Resolution, color = Call.Resolution)) +
      geom_line() +
      ylab("Percentage") +
      xlab("Year") +
      ggtitle("Trend of Call Resolution over Years (Jan - May)") +
      theme_bw() 
    
    p2 <- ggplot(data_graph23, aes(factor(year), percent*100, group = Call.Resolution, color = Call.Resolution)) +
      geom_line() +
      ylab("Percentage") +
      xlab("Year") +
      ggtitle("Trend of Call Resolution over Years (June - Dec)") +
      theme_bw()
    
    grid.arrange(p1,p2)
  })
  
  
  output$graph22 <- renderPlot({
    ggplot(data_graph25, aes(month(Date, label = T, abbr = F), fill = result)) +
      geom_bar(stat = "count", position = "dodge", width = 0.7) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("Month") + ylab("Frequency") + ggtitle("Distribution of Results by Month - 2016")
  })
  
  output$graph23 <- renderPlot({
    ggplot(merged_1, aes(x = Department.Name, y = Zip.Code, fill = Percent)) + geom_tile() + 
      scale_fill_gradient(low = "white", high = "darkred") +
      xlab("Department Name") +
      ylab("Zip Code") +
      ggtitle("Percentage Call Volume across Zip Codes and Department Name") +
      theme(axis.text.y = element_text(size = 9, face = "bold")) +
      theme(axis.text.x = element_text(size = 9, face = "bold")) +
      scale_x_discrete(labels = c("Dept of Building & Safety", "Dept of Transportation", "LAPD", "Sanitation", "Street Services"))
  })
  

  
}





shinyApp(server = server, ui = ui)
