load("dataforShiny2.rda")
library(ggplot2)
library(gridExtra)
library(dplyr)

data_graph1 %>%
     ggplot(aes(x = reorder(Department.Name, perc), y =perc*100)) + geom_bar(stat = "identity", fill = "steelblue") +
     coord_flip() +
     xlab("Department Names") +
     ylab("Percentage") +
     ggtitle("Distribution of Percentage of Calls (Top 7 Departments)") +
     theme(axis.text.y = element_text(size = 9, face = "bold")) +
     theme(axis.text.x = element_text(size = 9, face = "bold"))

## More than 90% of requests are from the 10 categories shown with building and safety, and sanitation generating highest call volume with 60% calls.


############## Distribution of call resolutions
data_graph2 %>%
     ggplot(aes(x = reorder(Call.Resolution, perc), y = perc*100)) + geom_bar(stat = "identity", fill = "steelblue") + 
     coord_flip() +
     xlab("Call Resolution Type") +
     ylab("Percentage") +
     ggtitle("Distribution of Percentage of Calls (Top 6 Resolution Types)") +
     theme(axis.text.y = element_text(size = 9, face = "bold")) +
     theme(axis.text.x = element_text(size = 9, face = "bold"))

##Most calls end up being transferred to LA city services (probably city hall or council). Around 34% calls have requests processed while ~17% involve giving caller some information 

############ Distribution of requests by department by year
p1 <- data_graph3 %>%
     ggplot(aes(x = year, y =perc, color = Department.Name)) + 
     geom_line(size = 1.5) + theme_bw() + ylim(0, 50) +
     xlab("Year") +
     ylab("Percentage of Calls") +
     theme(legend.title = element_blank())

## We see that percentage of calls to LAPD and sanitation has declined while percentage of calls to department of building and safety has increased.

p2 <- data_graph4 %>%
     ggplot(aes(x = year, y = n, color = Department.Name)) + 
     geom_line(size = 1.5) + theme_bw() +
     xlab("Year") +
     ylab("Calls (In Thousands)") +
     theme(legend.title = element_blank())

grid.arrange(p1,p2, top = "Trend of Calls by Department")

## We see that number of calls has remained more or less constant for LAPD and Sanitaiton while it has increased for building and safety. 2015 data is incomplete.



############# Distribution for service requests by department
data_graph5 %>%
     ggplot(aes(x = reorder(Service.Name, perc), y = perc*100)) + 
     geom_bar(stat = "identity", fill = "steelblue") + 
     coord_flip() +
     xlab("Type of Request") +
     ylab("Percentage") +
     ggtitle("Distribution of Percentage of Calls by Request Type") +
     theme(axis.text.y = element_text(size = 9, face = "bold")) +
     theme(axis.text.x = element_text(size = 9, face = "bold"))

## Most calls are requests for bulky item pick up or permit inspection



###############
data_graph6 %>%
     ggplot(aes(x = factor(month), y = Department.Name, fill = requests)) + 
     geom_tile() + scale_fill_gradient(low = "white", high = "darkred") +
     facet_wrap(~year) +
     xlab("Month") +
     ylab("Department Name") +
     ggtitle("Trend of Calls by Department and Year") +
     theme(axis.text.y = element_text(size = 9, face = "bold")) +
     theme(axis.text.x = element_text(size = 9, face = "bold", angle = 40)) +
     theme(legend.title = element_blank())

############## Distribution of Service request processed by department name

combined %>% 
     ggplot(aes(x = reorder(Department.Name, perc), y = perc*100)) + 
     geom_bar(stat="identity", fill = "darkred") +
     coord_flip() +
     xlab("Department Name") +
     ylab("Percentage") +
     ggtitle("Proportion of Service Requests processed against Total Calls") +
     theme(axis.text.y = element_text(size = 9, face = "bold")) +
     theme(axis.text.x = element_text(size = 9, face = "bold"))

## PW/Board of public works, lighting and sanitation process the highest percentage of their requests

############# Distribution by time
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



############################################# GEOGRAPHIC ########################

LA +
     geom_tile(data = mapdat2, aes(Lon2, Lat2, fill = Requests), alpha = 1) + 
     scale_fill_continuous(low = "white", high = "darkred") +
     ggtitle("Geographic Overview of Service Requests")

########### By zip code
merged_1 %>% 
     ggplot(aes(x = Department.Name, y = Zip.Code, fill = Percent)) + geom_tile() + 
     scale_fill_gradient(low = "white", high = "darkred") +
     xlab("Department Name") +
     ylab("Zip Code") +
     ggtitle("Percentage Call Volume across Zip Codes and Department Name") +
     theme(axis.text.y = element_text(size = 9, face = "bold")) +
     theme(axis.text.x = element_text(size = 9, face = "bold")) +
     scale_x_discrete(labels = c("Dept of Building & Safety", "Dept of Transportation", "LAPD", "Sanitation", "Street Services"))


########## Police Precinct
data_graph12 %>% 
     ggplot(aes(x = RequestType, y = PolicePrecinct, fill = Requests)) + geom_tile() +
     scale_fill_continuous(low = "white", high = "darkred") +
     xlab("Request Type") +
     ylab("Police Precinct") +
     ggtitle("Call Volume across Police Precinct") +
     theme(axis.text.y = element_text(size = 9, face = "bold")) +
     theme(axis.text.x = element_text(size = 9, face = "bold"))


########### Clustering by Zip Code

x %>% ggplot(aes(x = cluster, y = perc * 100, fill = cluster)) + geom_boxplot() +
     xlab("Zipcode Cluster") + ylab("Percentage") + 
     ggtitle("Percentage of Calls with Service Request Processed") +
     theme_bw()

table(x$cluster)
tapply(zippositioncluster$perc, zippositioncluster$cluster, mean)

LA +
     geom_tile(data = zippositioncluster2,
               aes(x = Lon2, y = Lat2, fill = avg)) +
     scale_fill_continuous(low = "darkred", high = "white") +
     ggtitle("Percentage of Calls with Service Request Processed")

tapply(zippositioncluster$perc, zippositioncluster$cluster, mean)

## There are clusters of zip codes where service request processing percentage is lower.


#################################### Points 2 and 3 ##########################################

## Percent of Service Requests compaired to Same Request Source

xy1 <- ggplot(percent_part, aes(x = RequestType, y = percent, fill = type)) +
     geom_bar(position = "dodge", stat = "identity") +
     ggtitle("Percent of Service Requests for each Request Source") +
     ylab("Frequency") +
     xlab("Request Type") +
     theme_light() + coord_flip() +
     scale_fill_manual(values = c("blue", "darkred"))

## Percent of Service Requests compaired to Total Requests
xy2 <- ggplot(percent_whole, aes(x = RequestType, y = percent, fill = type)) +
     geom_bar(position = "dodge", stat = "identity") +
     ggtitle("Percent of Service Requests compared to Total Requests") +
     ylab("Frequency") +
     xlab("Request Type") +
     theme_light() + coord_flip() +
     scale_fill_manual(values = c("blue", "darkred"))

grid.arrange(xy1, xy2)



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


#May need better arrangement - too much overlap on my laptop
grid.arrange(appmap, phonemap, appgraffitimap,
             phonegraffitimap, appnograffitimap,
             phonenograffitimap, ncol = 2)



#We see that summer months have a slightly higher request count
timetrend_requestcount = ggplot(timetrend_phonevsapp, aes(x = factor(month), y = count))+
     geom_bar(stat = "identity", fill = "steelblue")+
     ggtitle("Service Request Count - 2016")+
     xlab("Month in 2016")+
     ylab("Count of Service Requests")

# Now looking at the percent of requests from app vs phone, over time

phonevsapp_overtime = ggplot(timetrend_phonevsapp_final, 
                             aes(x = factor(month), y = percent*100, fill = type))+
     geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.7)+
     ggtitle("Percent of Requests by Phone vs App for 2016")+
     xlab("Month in 2016")+
     ylab("Percent of Requests")

### in general phone call usage has gone down about 5% in place of app usage 

####################### 

graffit_byrequestsource =
     data_graph19 %>%
     ggplot(aes(x=reorder(RequestSource, -count), y = count/1000))+
     geom_bar(stat = "identity", fill = "darkred")+
     xlab("Request Source")+
     ggtitle("Graffiti Removal Requests by Source")+
     ylab("Graffiti Removal Requests (in 1000s)")

### the above is very different than the trend for Bulky Item removal requests (50% of all requests are for bulky items)

bulkyitem_byrequestsource =
     data_graph20 %>%
     ggplot(aes(x=reorder(RequestSource, -count), y = count/1000))+
     geom_bar(stat = "identity", fill = "darkred")+
     xlab("Request Source")+
     ggtitle("Bulky Item Removal Requests by Source")+
     ylab("Bulky Item Removal Requests (in 1000s)")


##### 2 ######
grid.arrange(bulkyitem_byrequestsource,graffit_byrequestsource, nrow =1)

## 60% of all graffiti removal requests tend to come in from non phone or app requests.

####################


a = ggplot(timetrend_requesttype, aes(x = factor(month), y = count, fill = RequestType))+
     geom_bar(stat = "identity", position = "dodge", width = 0.7)+
     ggtitle("Service Requests by Request Type - 2016")+
     xlab("Month")+
     ylab("Requests")

#### 2 continued

b = ggplot(timetrend_requesttype_merged, aes(x = factor(month), y = percent, fill = RequestType))+
     geom_bar(stat = "identity", position = "dodge", width = 0.7)+
     ggtitle("Request Type Percentage Breakdown by Month - 2016")+
     xlab("Month")+
     ylab("Percent")

grid.arrange(a,b,nrow=1)

#################

#### 4 ###### Needs aesthetic change
data_graph21 %>%
     ggplot(aes(x=serviceefficiency, y = n, fill = type))+
     geom_bar(stat= "identity", position = "dodge", width = 0.5)+
     xlim(c(0, 8))+
     xlab("Time to Service Request in Days")+
     ylab("Requests")+
     ggtitle("Service Efficiency by Request Source")


################################# Points 4 and 6 ###########################################

############# TREND LINE OVER YEARS - (from months jan to may) as 2015 year has data only till may
p1 <- ggplot(data_graph22, aes(factor(year), percent*100, group = Call.Resolution, color = Call.Resolution)) +
     geom_line() +
     ylab("Percentage") +
     xlab("Year") +
     ggtitle("Trend of Call Resolution over Years (Jan - May)") +
     theme_bw() 

### TREND LINE OVER YEARS - (from months june to Dec) for 2011-2014

p2 <- ggplot(data_graph23, aes(factor(year), percent*100, group = Call.Resolution, color = Call.Resolution)) +
     geom_line() +
     ylab("Percentage") +
     xlab("Year") +
     ggtitle("Trend of Call Resolution over Years (June - Dec)") +
     theme_bw()

grid.arrange(p1,p2)


############## SEASONALITY --- TREND LINE OVER MONTHS - (from months june to Dec) for 2011-2014

ggplot(data_graph24, aes(factor(month), percent*100, group = Call.Resolution, color = Call.Resolution)) +
     geom_line() +
     ylab("Percentage") +
     xlab("Month") +
     ggtitle("Trend of Call Resolution (2011-2014)") +
     theme_bw()


data_graph25 %>%
     ggplot(aes(month(Date, label = T, abbr = F), fill = result)) +
     geom_bar(stat = "count", position = "dodge", width = 0.7) + 
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
     xlab("Month") + ylab("Frequency") + ggtitle("Distribution of Results by Month - 2016")



###########################################################################
data_graph26%>%
     ggplot(aes(as.numeric(Time))) +
     geom_line(stat = "density") +
     scale_x_continuous(breaks = c(0, (60*60*24)), labels = c('00', '24'))+
     theme(strip.text.x = element_text(size = 5, angle=90),
           strip.text.y = element_text(size = 5, angle=0)) + 
     facet_grid(Service.Name~DOW) + xlab("Time of Day")

LA +
     geom_point(data = sampletop10, 
                aes(INTPTLONG, INTPTLAT),
                size = 1/10,
                alpha = 1/20, 
                position = position_jitter(0.05,0.05)) +
     theme(strip.text.x = element_text(size = 5, angle=90),
           strip.text.y = element_text(size = 5, angle=0)) +
     facet_grid(Service.Name~DOW)

