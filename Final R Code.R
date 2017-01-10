#datasets to remove from RDA - mapdat, data, data2, requestdeptzip, requestzip, zipcode,
# zipbyresolution, phoneapp
save(data_graph1, data_graph2, data_graph3, data_graph4, data_graph5, data_graph6, 
     data_graph7, data_graph8, data_graph9, data_graph10, data_graph11, data_graph12, 
     data_graph13, data_graph14, data_graph15, data_graph16, data_graph17, data_graph18, 
     data_graph19, data_graph20, data_graph21, data_graph22, data_graph23, data_graph24, 
     data_graph25, data_graph26, combined, combined2, data.servicesummary, department, 
     departmentbyresolution, mapdat2, merged, merged_1,month_total, percent_part, percent_whole, 
     quick, quick2, sampletop10, timetrend_phonevsapp, timetrend_phonevsapp_final, 
     timetrend_requestcount, timetrend_requesttype, timetrend_requesttype_merged, top10, 
     USZipCodeData, x, zippositioncluster, zippositioncluster2, zips, LA,file="dataforShiny2.rda")

# Libraries
library(ggmap)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zipcode)
library(rockchalk)
library(gridExtra)

#Load Real Data
data <- read.csv("311_Call_Center_Tracking_Data.csv")
data2 <- read.csv("MyLA311_Service_Request_Data_2016.csv")

data("zipcode")
USZipCodeData <- zipcode
USZipCodeData$zip <- factor(USZipCodeData$zip)
rm(zipcode)

zips = read.csv("2016_Gaz_zcta_national.txt", sep = "\t")
zips$GEOID = formatC(zips$GEOID, width = 5, flag = "0")

#Remove Unused Columns
data <- select(data, -Department.Abbreviation)
data2 <- select(data2, -ActionTaken, -Owner, -Anonymous, -AssignTo, -ApproximateAddress,
                -Address, -HouseNumber, -Direction, -StreetName, -Suffix, -TBMPage, -TBMColumn,
                -TBMRow, -APC, -CD, - CDMember, -NC)

#Format Data
colnames(data)[1] <- "Date"
data$Date <- mdy(data$Date)
data$Time  <- hms(data$Time)
data$year <- year(data$Date)
data$month <- month(data$Date)
data <- mutate(data, Hour = hour(Time))
data <- mutate(data, Minute = minute(Time))
data$Zip.Code <- factor(data$Zip.Code)

colnames(data2)[1] <- "SRNumber"
data2$CreatedDate <- mdy_hms(data2$CreatedDate)
data2$UpdatedDate <- mdy_hms(data2$UpdatedDate)
data2$ServiceDate <- mdy_hms(data2$ServiceDate)
data2$ClosedDate <- mdy_hms(data2$ClosedDate)
data2$year_created <- year(data2$CreatedDate)
data2$month <- month(data2$CreatedDate)

#New data frames and joins
quick <- data[sample(nrow(data), 100000), ]
quick2 <- merge(quick, zips, by.x = "Zip.Code", by.y = "GEOID", all.x = T)

data <- merge(data, zips, by.x = "Zip.Code", by.y = "GEOID", all.x = T)

#Map
LA <- qmap("Los Angeles", zoom = 10, color = "bw")

#New Columns
refer = unique(data$Call.Resolution)[c(2,13,8,10)]
resolve = unique(data$Call.Resolution)[c(4,9,18)]
transfer = unique(data$Call.Resolution)[c(6,7,11,16,17)]

data$result = data$Call.Resolution
data$result = combineLevels(data$result, c(10:13), newLabel = "Referred")
data$result = combineLevels(data$result, c(1,2,5,7,10), newLabel = "Resolved")
data$result = combineLevels(data$result, c(1:2,7:9), newLabel = "Transfer")

data$result <- factor(data$result, 
                      levels = c("Resolved", "Transfer", "Referred", 
                                 "Got Voicemail (City)", "Line Busy (City)", "N/A", 
                                 "Static/Ghost Call"), ordered = T) 
data$DOW <- wday(data$Date, label = T, abbr = T)


data2$serviceefficiency = (data2$UpdatedDate - data2$CreatedDate)/86400 
data2$serviceefficiency <- round(data2$serviceefficiency,0)

data2$Duration = data2$CreatedDate%--%data2$ClosedDate
data2$Duration = as.period(data2$Duration)

data2 <- data2 %>%
     mutate(type = ifelse(RequestSource %in% c("Call",
                                               "Voicemail",
                                               "Queue Initiated Customer Call"), "Phone",
                          ifelse(RequestSource == "Mobile App", "App", "Other")))

#Data frames for graphs
data_graph1 <- data %>% group_by(Department.Name) %>% summarise(n = n()) %>% 
     filter(Department.Name != "") %>% 
      mutate(perc = n / sum(n)) %>% 
     arrange(-n) %>% mutate(cumul = cumsum(perc)) %>% slice(1: 7) 

data_graph2 <- data %>% group_by(Call.Resolution) %>% summarise(n = n()) %>% mutate(perc = n / sum(n)) %>% 
     arrange(-n) %>% mutate(cumul = cumsum(perc)) %>% slice(1: 6) 

data_graph3 <- data %>% mutate(year = year(Date)) %>% group_by(year, Department.Name) %>% 
     filter(Department.Name != "") %>% 
     summarise(n = n()) %>% mutate(perc = 100*(n / sum(n))) %>% 
     arrange(-n) %>%  
     filter(Department.Name %in% c("Department of Building and Safety", 
                                   "PW/Bureau of Sanitation", 
                                   "Los Angeles Police Department")) 

data_graph4 <- data %>% mutate(year = year(Date)) %>% group_by(year, Department.Name) %>% 
     filter(Department.Name != "") %>% 
     summarise(n = n()/1000) %>% 
     arrange(-n) %>%  
     filter(Department.Name %in% c("Department of Building and Safety", 
                                   "PW/Bureau of Sanitation", 
                                   "Los Angeles Police Department"))

data_graph5 <- data %>% filter(Service.Name != "") %>% mutate(year = year(Date)) %>% 
     group_by(Service.Name) %>% summarise(n = n()) %>%
     arrange(-n) %>% mutate(perc = n / sum(n))%>% mutate(cumul = cumsum(perc)) %>% slice(1:10) 

data_graph6 <- data %>% mutate(month = month(Date, label = T)) %>% 
     filter(Department.Name %in% c("Department of Building and Safety", 
                                   "PW/Bureau of Sanitation", 
                                   "Los Angeles Police Department",
                                   "PW/Board of Public Works", 
                                   "Department of Transportation", 
                                   "PW/Bureau of Street Services")) %>%
     mutate(year = year(Date)) %>%
     filter(!is.na(month)) %>%
     group_by(year, month, Department.Name) %>% summarise(requests = n()) 

department <- data %>% group_by(Department.Name) %>% summarise(calls = n()) 
departmentbyresolution <- data %>% group_by(Department.Name, Call.Resolution) %>% summarise(calls = n()) 
combined <- merge(departmentbyresolution, department, 
                  by.x = "Department.Name", by.y = "Department.Name")
combined <- combined %>% mutate(perc = calls.x / calls.y) %>% 
     filter(Call.Resolution == "Service Request Processed" & Department.Name != "") %>% 
     arrange(-perc)


data_graph7 <- data %>% mutate(weekday = wday(Date, abbr = T, label = T)) %>%
     mutate(hour = hour(Time)) %>% mutate(year = year(Date)) %>%
     filter(!is.na(hour) & !is.na(weekday)) %>%
     filter(year == 2011) %>%
     group_by(year, weekday, hour) %>% summarise(Requests = log(n(),10))

data_graph8 <- data %>% mutate(weekday = wday(Date, abbr = T, label = T)) %>%
     mutate(hour = hour(Time)) %>% mutate(year = year(Date)) %>%
     filter(!is.na(hour) & !is.na(weekday)) %>%
     filter(year == 2012) %>%
     group_by(year, weekday, hour) %>% summarise(Requests = log(n(),10))

data_graph9 <- data %>% mutate(weekday = wday(Date, abbr = T, label = T)) %>%
     mutate(hour = hour(Time)) %>% mutate(year = year(Date)) %>%
     filter(!is.na(hour) & !is.na(weekday)) %>%
     filter(year == 2013) %>%
     group_by(year, weekday, hour) %>% summarise(Requests = log(n(),10))

data_graph10 <- data %>% mutate(weekday = wday(Date, abbr = T, label = T)) %>%
     mutate(hour = hour(Time)) %>% mutate(year = year(Date)) %>%
     filter(!is.na(hour) & !is.na(weekday)) %>%
     filter(year == 2014) %>%
     group_by(year, weekday, hour) %>% summarise(Requests = log(n(),10))

data_graph11 <- data %>% mutate(weekday = wday(Date, abbr = T, label = T)) %>%
     mutate(hour = hour(Time)) %>% mutate(year = year(Date)) %>%
     filter(!is.na(hour) & !is.na(weekday)) %>%
     filter(year == 2015) %>%
     group_by(year, weekday, hour) %>% summarise(Requests = log(n(), 10))


mapdat <- select(data2, Latitude, Longitude) %>% filter(!is.na(Latitude)) 
mapdat$Lat2 <- round(mapdat$Latitude,2)
mapdat$Lon2 <- round(mapdat$Longitude,2)
mapdat2 <- mapdat %>% group_by(Lat2, Lon2) %>% summarise(Requests = n()) 



requestdeptzip <- data %>% group_by(Zip.Code, Department.Name) %>% summarise(requests = n()) %>%
     filter(requests > 10000) %>% arrange(-requests) %>% 
     filter(!is.na(Zip.Code) & Department.Name != "")
requestzip <- data %>% group_by(Zip.Code) %>% summarise(requests = n()) %>%
     filter(requests > 10000) %>% arrange(-requests) %>% 
     filter(!is.na(Zip.Code))
merged <- merge(x = requestdeptzip, y = requestzip, by.x = "Zip.Code", by.y = "Zip.Code")
merged_1 <- merged %>% mutate(Percent = requests.x / requests.y)


data_graph12 <- data2 %>% group_by(PolicePrecinct, RequestType) %>% summarise(Requests = n()) %>% 
     filter(PolicePrecinct != "") %>% filter(Requests > 5000) 


zipcode <- data %>% group_by(Zip.Code) %>% summarise(calls = n()) 
zipbyresolution <- data %>% group_by(Zip.Code, Call.Resolution) %>% summarise(calls = n()) 
combined2 <- merge(zipbyresolution, zipcode, 
                   by.x = "Zip.Code", by.y = "Zip.Code")
x <- combined2 %>% mutate(perc = calls.x / calls.y) %>% 
     filter(Call.Resolution == "Service Request Processed" & calls.y > 10000) %>% 
     arrange(-perc) %>% filter (Zip.Code != 99999 & Zip.Code != 0)
clusterbyzip <- kmeans(x$perc, 4)
x$cluster <- factor(clusterbyzip$cluster)
zippositioncluster <-
     merge(select(x, Zip.Code, perc ,cluster), USZipCodeData, by.x = "Zip.Code", by.y = "zip")
zippositioncluster$Lat2 <- round(zippositioncluster$latitude,2)
zippositioncluster$Lon2 <- round(zippositioncluster$longitude,2)
zippositioncluster2 <- zippositioncluster %>% group_by(Lat2, Lon2) %>% 
     summarise(avg = mean(perc))


phoneapp <- data2 %>% filter(type %in% c("Phone","App")) %>% droplevels()
percent_part <- phoneapp %>%
     group_by(type, RequestType) %>%
     summarise(count = n()) %>%
     mutate(total = sum(count)) %>%
     mutate(percent = count/total)
percent_whole <- phoneapp %>%
     group_by(type, RequestType) %>%
     summarise(count = n()) %>%
     mutate(total = nrow(phoneapp)) %>%
     mutate(percent = count/total)
phoneapp <- phoneapp %>%
     mutate(daycreated = wday(CreatedDate, label = T, abbr = F),
            timecreated = hour(CreatedDate))

data_graph13 <- phoneapp %>%
     filter(type == "App") %>%
     group_by(daycreated, timecreated) %>%
     summarise(count = n()) 

data_graph14 <- phoneapp %>%
     filter(type == "Phone") %>%
     group_by(daycreated, timecreated) %>%
     summarise(count = n()) 

data_graph15 <- phoneapp %>%
     filter(type == "App", RequestType == "Graffiti Removal") %>%
     group_by(daycreated, timecreated) %>%
     summarise(count = n()) 

data_graph16 <- phoneapp %>%
     filter(type == "Phone", RequestType == "Graffiti Removal") %>%
     group_by(daycreated, timecreated) %>%
     summarise(count = n()) 

data_graph17 <- phoneapp %>%
     filter(type == "App", RequestType != "Graffiti Removal") %>%
     group_by(daycreated, timecreated) %>%
     summarise(count = n()) 

data_graph18 <- phoneapp %>%
     filter(type == "Phone", RequestType != "Graffiti Removal") %>%
     group_by(daycreated, timecreated) %>%
     summarise(count = n()) 

timetrend_phonevsapp = data2 %>%
     group_by(year_created, month, type)%>%
     filter(type != "Other") %>%
     summarise(count = n())%>%
     filter(year_created == "2016" & month != "12")
month_total = data2 %>%
     filter(year_created == "2016" & type != "Other") %>%
     group_by(month)%>%
     summarise(count = n())
timetrend_phonevsapp_final = merge(timetrend_phonevsapp,month_total, by = "month") %>%
     mutate(percent = count.x/count.y)



data_graph19 <- data2%>%
     group_by(RequestType,RequestSource )%>%
     filter(RequestType == "Graffiti Removal")%>%
     summarise(count = n())%>%
     arrange(-count) %>%
     slice(1:5)


data_graph20 <- data2%>%
     group_by(RequestType,RequestSource )%>%
     filter(RequestType == "Bulky Items")%>%
     summarise(count = n())%>%
     arrange(-count) %>%
     slice(1:5)

timetrend_requesttype = data2 %>%
     group_by(year_created, month, RequestType)%>%
     summarise(count = n())%>%
     filter(year_created == "2016")%>%
     filter(RequestType %in% c("Bulky Items", "Graffiti Removal"))

month_total = data2 %>%
     filter(year_created == "2016")%>%
     group_by(month)%>%
     filter(RequestType %in% c("Bulky Items", "Graffiti Removal"))%>%
     summarise(total = n())

timetrend_requesttype_merged = merge(timetrend_requesttype,month_total, by = "month")%>%
     mutate(percent = count/total)


data_graph21 <- data2 %>% group_by(serviceefficiency,type) %>% summarise(n = n()) 

data_graph22 <- data %>%
     filter(Call.Resolution != "N/A" & !is.na(year)) %>%
     filter(month(Date) < 6) %>%
     group_by(year, Call.Resolution) %>%
     summarise(count = n()) %>%
     mutate(percent = round(count/sum(count),2)) %>%
     arrange(-percent) %>%
     slice(1:3)

data_graph23 <- data %>%
     filter(Call.Resolution != "N/A" & year != "NA") %>%
     filter(month(Date) > 5 & month(Date) < 13) %>%
     group_by(year, Call.Resolution) %>%
     summarise(count = n()) %>%
     mutate(percent = round(count/sum(count),2)) %>%
     arrange(-percent) %>%
     slice(1:3)


data_graph24 <- data %>%
     filter(Call.Resolution != "N/A" & year != "NA") %>%
     filter(year != "2015") %>%
     group_by(month, Call.Resolution) %>%
     summarise(count = n()) %>%
     mutate(percent = round(count/sum(count),2)) %>%
     arrange(-percent) %>%
     slice(1:3)


data_graph25 <- data %>%
     filter(!is.na(Date))%>% filter(result %in% c("Transfer", "Resolved", "Referred"))


data.servicesummary = data %>% group_by(Service.Name)%>% summarise(count = n())
top10 = data.servicesummary %>% filter(Service.Name!="") %>% arrange(desc(count)) %>% head(10)
data$top10 = data$Service.Name %in% top10$Service.Name
data_graph26 <- data %>% filter(top10==T)


set.seed(123)
sampletop10 = data_graph26[sample(nrow(data), 200000), ]
sampletop10 = sampletop10 %>% filter(!is.na(DOW)) %>% filter(!is.na(Service.Name))




############## Distribution of requests by department
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

