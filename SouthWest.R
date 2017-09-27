
library(xlsx)
library(ggplot2)
library(tidyr)
library(scales)
library(hexbin)
library(data.table)
library(plotly)
library(PerformanceAnalytics)
library(dplyr)


# LOAD DATA and MERGE with GMT
df <- read.csv("Airline_ontime_data (1).csv",
               header = TRUE,
               quote="\"",
               stringsAsFactors= TRUE,
               strip.white = TRUE)
as.data.frame.matrix(df)
tzo <- read.csv("TimeZonesORIGIN.csv",
               header = TRUE,
               quote="\"",
               stringsAsFactors= TRUE,
               strip.white = TRUE)
tzd <- read.csv("TimeZonesDESTINATION.csv",
               header = TRUE,
               quote="\"",
               stringsAsFactors= TRUE,
               strip.white = TRUE)
as.data.frame.matrix(tzo) 
as.data.frame.matrix(tzd) 
df <- merge(x = df, y = tzo, by = "ORIGIN", all.x = TRUE)
df <- merge(x = df, y = tzd, by = "DEST", all.x = TRUE)

# FILTER DATA (for temporal analysis only)
df_subset <- df[ (df$CARRIER != ''), ] # & (df$FL_DATE == '1/01/14' | df$FL_DATE == '2/01/14')   , ]


#SELECT VARIABLES
df_subset <- subset(df_subset, select=c("CARRIER","Carrier_Type","FL_DATE","TAIL_NUM","FL_NUM","ORIGIN_AIRPORT_ID","ORIGIN_CITY_NAME","DEST_AIRPORT_ID","DEST_CITY_NAME",
                                        "CRS_DEP_TIME","DEP_TIME","DEP_TIME_BLK","DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15","DEP_DELAY_GROUP",
                                        "CRS_ARR_TIME","ARR_TIME","ARR_TIME_BLK","ARR_DELAY","ARR_DELAY_NEW","ARR_DEL15","ARR_DELAY_GROUP",
                                        "CANCELLED","DIVERTED",
                                        "CRS_ELAPSED_TIME","ACTUAL_ELAPSED_TIME","AIR_TIME","DISTANCE","DISTANCE_GROUP",
                                        "CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY",
                                        "GMTO","GMTD"))

# REMOVE CANCELED AND DIVERTED
df_subset <- df_subset[ df_subset$CANCELLED == 0, ]
df_subset <- df_subset[ df_subset$DIVERTED == 0, ]

df_subset$CARRIER_DELAY<-ifelse( is.na(df_subset$CARRIER_DELAY) ,0,df_subset$CARRIER_DELAY)
df_subset$WEATHER_DELAY<-ifelse( is.na(df_subset$WEATHER_DELAY) ,0,df_subset$WEATHER_DELAY)
df_subset$NAS_DELAY<-ifelse( is.na(df_subset$NAS_DELAY) ,0,df_subset$NAS_DELAY)
df_subset$SECURITY_DELAY<-ifelse( is.na(df_subset$SECURITY_DELAY) ,0,df_subset$SECURITY_DELAY)
df_subset$LATE_AIRCRAFT_DELAY<-ifelse( is.na(df_subset$LATE_AIRCRAFT_DELAY) ,0,df_subset$LATE_AIRCRAFT_DELAY)
df_subset$DEP_DELAY_NEW<-ifelse( is.na(df_subset$DEP_DELAY_NEW) ,0,df_subset$DEP_DELAY_NEW)


x <- df_subset[ df_subset$CARRIER== 'WN' , ]
x$DEP_DEL15<-ifelse( is.na(x$DEP_DEL15) ,0,x$DEP_DEL15)
ontime_before <- 1-(sum(x$DEP_DEL15))/nrow(x)
ontime_before

# FORMAT FL_DATE
df_subset$Dates <- as.Date(df_subset$FL_DATE, "%d/%m/%Y")


# DEPARTURE TIME INTO GMT ABSOLUTE
df_subset$dep_hours <- 
    ifelse(nchar(df_subset$CRS_DEP_TIME)==4,substr(df_subset$CRS_DEP_TIME,1,2)  ,
           ifelse(nchar(df_subset$CRS_DEP_TIME)==3,substr(df_subset$CRS_DEP_TIME,1,1)  ,0))

df_subset$dep_minutes <- 
    ifelse(nchar(df_subset$CRS_DEP_TIME)==4,substr(df_subset$CRS_DEP_TIME,3,4)  ,
           ifelse(nchar(df_subset$CRS_DEP_TIME)==3,substr(df_subset$CRS_DEP_TIME,2,3),
                  ifelse(nchar(df_subset$CRS_DEP_TIME)==2,substr(df_subset$CRS_DEP_TIME,1,2),
                         ifelse(nchar(df_subset$CRS_DEP_TIME)==1,substr(df_subset$CRS_DEP_TIME,0,1),0))))

df_subset$dep_hours_temp <- 
    ifelse(nchar(df_subset$DEP_TIME)==4,substr(df_subset$DEP_TIME,1,2)  ,
           ifelse(nchar(df_subset$DEP_TIME)==3,substr(df_subset$DEP_TIME,1,1)  ,0))

df_subset$dep_minutes_temp <- 
    ifelse(nchar(df_subset$DEP_TIME)==4,substr(df_subset$DEP_TIME,3,4)  ,
           ifelse(nchar(df_subset$DEP_TIME)==3,substr(df_subset$DEP_TIME,2,3),
                  ifelse(nchar(df_subset$DEP_TIME)==2,substr(df_subset$DEP_TIME,1,2),
                         ifelse(nchar(df_subset$DEP_TIME)==1,substr(df_subset$DEP_TIME,0,1),0))))

df_subset$CRS_DEP <- as.POSIXct(paste(df_subset$Dates, " ", df_subset$dep_hours,":", df_subset$dep_minutes,sep = "", collapse = NULL))
df_subset$DEP <- as.POSIXct(paste(df_subset$Dates, " ", df_subset$dep_hours_temp,":", df_subset$dep_minutes_temp,sep = "", collapse = NULL))

df_subset$CRS_DEP_GMT <- df_subset$CRS_DEP - df_subset$GMTO*60*60
df_subset$DEP_GMT <- df_subset$DEP - df_subset$GMTO*60*60


# ARRIVAL TIME INTO GMT ABSOLUTE
df_subset$arr_hours <- 
    ifelse(nchar(df_subset$CRS_ARR_TIME)==4,substr(df_subset$CRS_ARR_TIME,1,2)  ,
           ifelse(nchar(df_subset$CRS_ARR_TIME)==3,substr(df_subset$CRS_ARR_TIME,1,1)  ,0))

df_subset$arr_minutes <- 
    ifelse(nchar(df_subset$CRS_ARR_TIME)==4,substr(df_subset$CRS_ARR_TIME,3,4)  ,
           ifelse(nchar(df_subset$CRS_ARR_TIME)==3,substr(df_subset$CRS_ARR_TIME,2,3),
                  ifelse(nchar(df_subset$CRS_ARR_TIME)==2,substr(df_subset$CRS_ARR_TIME,1,2),
                         ifelse(nchar(df_subset$CRS_ARR_TIME)==1,substr(df_subset$CRS_ARR_TIME,0,1),0))))

df_subset$CRS_ARR <- as.POSIXct(paste(df_subset$Dates, " ", df_subset$arr_hours,":", df_subset$arr_minutes,sep = "", collapse = NULL))
df_subset$CRS_ARR_GMT <- df_subset$CRS_ARR - df_subset$GMTD*60*60


# DEPARTURE AND ARRIVAL IN MINUTES (used to calculate turn-around time in minutes)
df_subset$CRS_DEPARTURE_MINUTES <- 
    as.numeric(df_subset$Dates - min(df_subset$Dates))*24*60+
    ifelse(nchar(df_subset$CRS_DEP_TIME)==1,0,
           ifelse(nchar(df_subset$CRS_DEP_TIME)==2,0,
                  ifelse(nchar(df_subset$CRS_DEP_TIME)==3,as.numeric(substr(df_subset$CRS_DEP_TIME, 1, 1)),
                         ifelse(nchar(df_subset$CRS_DEP_TIME)==4,as.numeric(substr(df_subset$CRS_DEP_TIME, 1, 2)),0))))*60+
    as.numeric(substr(df_subset$CRS_DEP_TIME, nchar(df_subset$CRS_DEP_TIME)-1, nchar(df_subset$CRS_DEP_TIME)))-
    df_subset$GMTO*60

df_subset$CRS_ARRIVAL_MINUTES <- 
    as.numeric(df_subset$Dates - min(df_subset$Dates))*24*60+
    ifelse(nchar(df_subset$CRS_ARR_TIME)==1,0,
           ifelse(nchar(df_subset$CRS_ARR_TIME)==2,0,
                  ifelse(nchar(df_subset$CRS_ARR_TIME)==3,as.numeric(substr(df_subset$CRS_ARR_TIME, 1, 1)),
                         ifelse(nchar(df_subset$CRS_ARR_TIME)==4,as.numeric(substr(df_subset$CRS_ARR_TIME, 1, 2)),0))))*60+
    as.numeric(substr(df_subset$CRS_ARR_TIME, nchar(df_subset$CRS_ARR_TIME)-1, nchar(df_subset$CRS_ARR_TIME)))-
    df_subset$GMTD*60


df_subset$DEPARTURE_MINUTES <- 
    as.numeric(df_subset$Dates - min(df_subset$Dates))*24*60+
    ifelse(nchar(df_subset$DEP_TIME)==1,0,
           ifelse(nchar(df_subset$DEP_TIME)==2,0,
                  ifelse(nchar(df_subset$DEP_TIME)==3,as.numeric(substr(df_subset$DEP_TIME, 1, 1)),
                         ifelse(nchar(df_subset$DEP_TIME)==4,as.numeric(substr(df_subset$DEP_TIME, 1, 2)),0))))*60+
    as.numeric(substr(df_subset$DEP_TIME, nchar(df_subset$DEP_TIME)-1, nchar(df_subset$DEP_TIME)))-
    df_subset$GMTO*60

df_subset$ARRIVAL_MINUTES <- 
    as.numeric(df_subset$Dates - min(df_subset$Dates))*24*60+
    ifelse(nchar(df_subset$ARR_TIME)==1,0,
           ifelse(nchar(df_subset$ARR_TIME)==2,0,
                  ifelse(nchar(df_subset$ARR_TIME)==3,as.numeric(substr(df_subset$ARR_TIME, 1, 1)),
                         ifelse(nchar(df_subset$ARR_TIME)==4,as.numeric(substr(df_subset$ARR_TIME, 1, 2)),0))))*60+
    as.numeric(substr(df_subset$ARR_TIME, nchar(df_subset$ARR_TIME)-1, nchar(df_subset$ARR_TIME)))-
    df_subset$GMTD*60

df_subset$ELAPSED_DEP_TO_ARR = df_subset$ARRIVAL_MINUTES - df_subset$DEPARTURE_MINUTES

# PREVIOUS FLIGHT ARRIVAL (by lag)

df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM) %>%
    mutate(Previous_ARR = lag(CRS_ARRIVAL_MINUTES, 1))
df_subset$Buffer = df_subset$CRS_DEPARTURE_MINUTES - df_subset$Previous_ARR


# ACTUAL BUFFER
df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM) %>%
    mutate(Previous_ACTUAL_ARR = lag(ARRIVAL_MINUTES, 1))
df_subset$Actual_Buffer = df_subset$DEPARTURE_MINUTES - df_subset$Previous_ACTUAL_ARR


# ADD VARIABLE FOR FIRST FLIGHT OF THE DAY

df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM) %>%
    mutate(Previous_Date = lag(FL_DATE, 1))
df_subset$Day <- 
    ifelse(is.na(df_subset$Previous_Date),"New Day",
           ifelse(df_subset$FL_DATE==df_subset$Previous_Date,"","New Day"))


# VARIABLE TO CHECK IF ITS STILL THE SAME PLANE (or there's an error in scheduling)
df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM) %>%
    mutate(Previous_Arrival_Airport = lag(DEST_CITY_NAME, 1))

df_subset$Flight <- 
    ifelse(is.na(df_subset$Previous_Arrival_Airport),"Not Applicable",
           ifelse(df_subset$Previous_Arrival_Airport==df_subset$ORIGIN_CITY_NAME,"Same Plane","Different Plane"))

# ADD ID FOR THE FLIGHT NUMBER OF THE DAY
df_subset$id <- ave(as.numeric(df_subset$CRS_DEP_GMT), df_subset$TAIL_NUM,df_subset$FL_DATE, FUN=order) 

df_subset[ df_subset$TAIL_NUM == 'N476HA', c("CARRIER", "TAIL_NUM", "FL_DATE","id","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW", "DEP_DELAY_GROUP",  "ARR_TIME") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]




# MAX id by TAIL and DAY
df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM, FL_DATE) %>%
    mutate(Max_Id = max(id))

df_flights <- df_subset %>% group_by(CARRIER, TAIL_NUM, FL_DATE) %>% summarise(Number_Flights = max(Max_Id))
# TO calculate Point-to-Point without Southwest without WN: df_flights <- df_flights[ df_flights$CARRIER != 'WN', c("CARRIER","Carrier_Type", "FL_DATE","TAIL_NUM","Number_Flights" ) ]


df_flights_Carrier <- df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(Number_Flights_per_day = mean(Number_Flights))

df_flights$Carrier_Type<-ifelse(df_flights$CARRIER=="WN",'Southwest',
                                ifelse(df_flights$CARRIER=="OO",'Point-to-Point without Southwest',
                                       ifelse(df_flights$CARRIER=="HA",'Point-to-Point without Southwest',
                                              ifelse(df_flights$CARRIER=="MQ",'Point-to-Point without Southwest',
                                                     ifelse(df_flights$CARRIER=="EV",'Point-to-Point without Southwest',
                                                            ifelse(df_flights$CARRIER=="F9",'Point-to-Point without Southwest',
                                                                   ifelse(df_flights$CARRIER=="FL",'Point-to-Point without Southwest', 'Hub and Spoke')))))))

df_flights_Carrier_Type <- df_flights %>% group_by(Carrier_Type) %>% summarise(Number_Flights_per_day = mean(Number_Flights))

df_flights_Carrier[order(df_flights_Carrier$Number_Flights_per_day), ]
df_flights_Carrier_Type[order(df_flights_Carrier_Type$Number_Flights_per_day), ]


#Into Analysis
df_subset$Carrier_Type<-ifelse(df_subset$CARRIER=="WN",'Southwest',
                                ifelse(df_subset$CARRIER=="OO",'Point-to-Point without Southwest',
                                       ifelse(df_subset$CARRIER=="HA",'Point-to-Point without Southwest',
                                              ifelse(df_subset$CARRIER=="MQ",'Point-to-Point without Southwest',
                                                     ifelse(df_subset$CARRIER=="EV",'Point-to-Point without Southwest',
                                                            ifelse(df_subset$CARRIER=="F9",'Point-to-Point without Southwest',
                                                                   ifelse(df_subset$CARRIER=="FL",'Point-to-Point without Southwest', 'Hub and Spoke')))))))

detach("package:plyr", unload=TRUE) 
x <- df_subset %>% group_by(Carrier_Type, CARRIER) %>% summarise(Number_Flights = n())
x[order(x$Number_Flights), ]



# Avg flight time per flight
df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM, FL_DATE) %>%
    mutate(Actual_Flight_Time = mean(ACTUAL_ELAPSED_TIME))
df_flights <- df_subset %>% group_by(CARRIER, Carrier_Type, TAIL_NUM, FL_DATE) %>% summarise(Actual_Flight_Time = max(Actual_Flight_Time))
df_flights[ df_flights$TAIL_NUM == 'N3GWAA', c("CARRIER","Carrier_Type", "FL_DATE","TAIL_NUM","Actual_Flight_Time" ) ]
df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(Actual_Flight_Time = mean(Actual_Flight_Time))
df_flights %>% group_by(Carrier_Type) %>% summarise(Actual_Flight_Time = mean(Actual_Flight_Time))



# Avg aircrafts per day
df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]

agg1 <- aggregate(data=df_subset, TAIL_NUM ~ FL_DATE + CARRIER, function(TAIL_NUM) length(unique(TAIL_NUM)))
agg2 <- aggregate(data=df_subset, TAIL_NUM ~ FL_DATE + Carrier_Type, function(TAIL_NUM) length(unique(TAIL_NUM)))

mean(agg2[agg2$Carrier_Type == 'Other',]$TAIL_NUM)
mean(agg2[agg2$Carrier_Type == 'Hub and Spoke',]$TAIL_NUM)
mean(agg2[agg2$Carrier_Type == 'Point-to-Point without Southwest',]$TAIL_NUM)
mean(agg2[agg2$Carrier_Type == 'Southwest',]$TAIL_NUM)


# DELAYS CAUSES
df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM, FL_DATE) %>%
    mutate(CARRIER_DELAY_TIME = sum(CARRIER_DELAY))
df_flights <- df_subset %>% group_by(CARRIER, Carrier_Type, TAIL_NUM, FL_DATE) %>% summarise(Carrier_Delay_Time = max(CARRIER_DELAY_TIME))
df_flights[ df_flights$TAIL_NUM == 'N3GWAA', c("CARRIER","Carrier_Type", "FL_DATE","TAIL_NUM","Carrier_Delay_Time" ) ]
df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(Carrier_Delay_Time = sum(Carrier_Delay_Time))
Delay_Cause_Carrier <- df_flights %>% group_by(Carrier_Type) %>% summarise(Carrier_Delay_Time = sum(Carrier_Delay_Time))



df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM, FL_DATE) %>%
    mutate(REAL_DEP_DELAY_NEW = sum(DEP_DELAY_NEW))
df_flights <- df_subset %>% group_by(CARRIER, Carrier_Type, TAIL_NUM, FL_DATE) %>% summarise(Real_Delay_Time = max(REAL_DEP_DELAY_NEW))
df_flights[ df_flights$TAIL_NUM == 'N3GWAA', c("CARRIER","Carrier_Type", "FL_DATE","TAIL_NUM","Real_Delay_Time" ) ]
df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(Real_Delay_Time = sum(Real_Delay_Time))
Delay_Cause_TOTAL <- df_flights %>% group_by(Carrier_Type) %>% summarise(Real_Delay_Time = sum(Real_Delay_Time))


df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM, FL_DATE) %>%
    mutate(LATE_DELAY = sum(LATE_AIRCRAFT_DELAY))
df_flights <- df_subset %>% group_by(CARRIER, Carrier_Type, TAIL_NUM, FL_DATE) %>% summarise(Late_Delay_Time = max(LATE_DELAY))
df_flights[ df_flights$TAIL_NUM == 'N3GWAA', c("CARRIER","Carrier_Type", "FL_DATE","TAIL_NUM","Late_Delay_Time" ) ]
df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(Late_Delay_Time = sum(Late_Delay_Time))
Delay_Cause_Late <- df_flights %>% group_by(Carrier_Type) %>% summarise(Late_Delay_Time = sum(Late_Delay_Time))


df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM, FL_DATE) %>%
    mutate(WEATHER_DELAY_DELAY = sum(WEATHER_DELAY))
df_flights <- df_subset %>% group_by(CARRIER, Carrier_Type, TAIL_NUM, FL_DATE) %>% summarise(Weather_Delay_Time = max(WEATHER_DELAY_DELAY))
df_flights[ df_flights$TAIL_NUM == 'N3GWAA', c("CARRIER","Carrier_Type", "FL_DATE","TAIL_NUM","Weather_Delay_Time" ) ]
df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(Weather_Delay_Time = sum(Weather_Delay_Time))
Delay_Cause_Weather <- df_flights %>% group_by(Carrier_Type) %>% summarise(Weather_Delay_Time = sum(Weather_Delay_Time))



df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM, FL_DATE) %>%
    mutate(NAS_DELAY_DELAY = sum(NAS_DELAY))
df_flights <- df_subset %>% group_by(CARRIER, Carrier_Type, TAIL_NUM, FL_DATE) %>% summarise(NAS_Delay_Time = max(NAS_DELAY_DELAY))
df_flights[ df_flights$TAIL_NUM == 'N3GWAA', c("CARRIER","Carrier_Type", "FL_DATE","TAIL_NUM","NAS_Delay_Time" ) ]
df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(NAS_Delay_Time = sum(NAS_Delay_Time))
Delay_Cause_NAS <- df_flights %>% group_by(Carrier_Type) %>% summarise(NAS_Delay_Time = sum(NAS_Delay_Time))


df_subset <- df_subset[order(df_subset$TAIL_NUM, df_subset$CRS_DEP_GMT), ]
df_subset <- 
    df_subset %>%
    group_by(TAIL_NUM, FL_DATE) %>%
    mutate(SECURITY_DELAY_DELAY = sum(SECURITY_DELAY))
df_flights <- df_subset %>% group_by(CARRIER, Carrier_Type, TAIL_NUM, FL_DATE) %>% summarise(Security_Delay_Time = max(SECURITY_DELAY_DELAY))
df_flights[ df_flights$TAIL_NUM == 'N3GWAA', c("CARRIER","Carrier_Type", "FL_DATE","TAIL_NUM","Security_Delay_Time" ) ]
df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(Security_Delay_Time = sum(Security_Delay_Time))
Delay_Cause_SECURITY <- df_flights %>% group_by(Carrier_Type) %>% summarise(Security_Delay_Time = sum(Security_Delay_Time))




Delay_Cause_TOTAL <- as.data.frame(Delay_Cause_TOTAL)
Delay_Cause_Late <- as.data.frame(Delay_Cause_Late)
Delay_Cause_Carrier <- as.data.frame(Delay_Cause_Carrier)
Delay_Cause_Weather <- as.data.frame(Delay_Cause_Weather)
Delay_Cause_NAS <- as.data.frame(Delay_Cause_NAS)
Delay_Cause_SECURITY <- as.data.frame(Delay_Cause_SECURITY)

x <- merge(Delay_Cause_Late, Delay_Cause_Carrier,by="Carrier_Type")
y <- merge(Delay_Cause_Weather, Delay_Cause_NAS,by="Carrier_Type")
x <- merge(x, y, by="Carrier_Type")
x <- merge(x, Delay_Cause_SECURITY,by="Carrier_Type")

paste("Hub-and-Speak delays by late aircraft", x[1,2]/(x[1,2]+x[1,3]+x[1,4]+x[1,5]+x[1,6]))
paste("Point-to-Point without Southwest delays by late aircraft", x[2,2]/(x[2,2]+x[2,3]+x[2,4]+x[2,5]+x[2,6]))
paste("SouthWest delays by late aircraft", x[3,2]/(x[3,2]+x[3,3]+x[3,4]+x[3,5]+x[3,6]))

paste("Hub-and-Speak delays by Carrier", x[1,3]/(x[1,2]+x[1,3]+x[1,4]+x[1,5]+x[1,6]))
paste("Point-to-Point without Southwest delays by Carrier", x[2,3]/(x[2,2]+x[2,3]+x[2,4]+x[2,5]+x[2,6]))
paste("SouthWest delays by Carrier", x[3,3]/(x[3,2]+x[3,3]+x[3,4]+x[3,5]+x[3,6]))

paste("Hub-and-Speak delays by Others", (x[1,4]+x[1,5]+x[1,6])/(x[1,2]+x[1,3]+x[1,4]+x[1,5]+x[1,6]))
paste("Point-to-Point without Southwest delays by Others", (x[2,4]+x[2,5]+x[2,6])/(x[2,2]+x[2,3]+x[2,4]+x[2,5]+x[2,6]))
paste("SouthWest delays by Others", (x[3,4]+x[3,5]+x[3,6])/(x[3,2]+x[3,3]+x[3,4]+x[3,5]+x[3,6]))

################################################################################################################################################################################
#######################################################    ANALYSIS    #########################################################################################################
################################################################################################################################################################################

# CHECK AMOUNT OF ERRORS FOR BAD SCHEDULING
count(df_subset)
count(df_subset[ df_subset$Flight == 'Different Plane', ])
round((count(df_subset[ df_subset$Flight == 'Different Plane', ])/count(df_subset))*100, digits = 2)
# For example: 
               df_subset[ df_subset$Flight == 'Different Plane', c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY", "ARR_TIME", "ARR_DELAY" ) ]%>% 
                   select(TAIL_NUM) %>%
                   distinct()

# CHECK AMOUNT OF TURN-AROUND TIME NEGATIVE
count(df_subset)
count(df_subset[ df_subset$Buffer <0, ])
round((count(df_subset[ df_subset$Buffer<0, ])/count(df_subset))*100, digits = 2)



# FILTER OUT ERRORS, NEW DAYS AND DELAYS FROM NAS, SECURITY OR WEATHER over 10 minutes.

df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  & df_subset$Day != 'New Day'  &  df_subset$Buffer>0 &  df_subset$Buffer<300 &  df_subset$DEP_DELAY_NEW<300, ]
count(df_subset)
count(df_Analysis)

df_subset[ df_subset$TAIL_NUM == 'N215WN', c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW", "DEP_DELAY_GROUP",  "ARR_TIME") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp <- df_Analysis[ df_Analysis$TAIL_NUM != '', c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW", "DEP_DELAY_GROUP", "ARR_TIME") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_tem
str(df_temp)


# Histogram: TURN-AROUND TIME DISTRIBUTION
grid <- matrix(c(1,1,2,3), 2, 2, byrow = TRUE)
layout(grid)
par(mfrow=c(3,1))
df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$Buffer>0 
                          &  df_subset$Buffer<120   , ]
df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Hub and Spoke', c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp
summary(df_temp)

x <- as.numeric(df_temp$Buffer)
h<-hist(x, breaks=20, col="#1a3260", xlab="Turn-Around Time (min)", ylab="Number of Flights",
        main="Hub-and-Spoke")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

mean(df_temp$Buffer)
kurtosis(df_temp$Buffer)
skewness(df_temp$Buffer)
sd(df_temp$Buffer)


df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$Buffer>0 
                          &  df_subset$Buffer<120   , ]
df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Point-to-Point without Southwest', c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp
summary(df_temp)

x <- as.numeric(df_temp$Buffer)
h<-hist(x, breaks=20, col="#1a3260", xlab="Turn-Around Time (min)", ylab="Number of Flights",
        main="Point-to-Point without Southwest")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

mean(df_temp$Buffer)
kurtosis(df_temp$Buffer)
skewness(df_temp$Buffer)
sd(df_temp$Buffer)


df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$Buffer>0 
                          &  df_subset$Buffer<120   , ]
df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Southwest', c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp
summary(df_temp)

x <- as.numeric(df_temp$Buffer)
h<-hist(x, breaks=20, col="#1a3260", xlab="Turn-Around Time (min)", ylab="Number of Flights",
        main="Southwest")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

mean(df_temp$Buffer)
kurtosis(df_temp$Buffer)
skewness(df_temp$Buffer)
sd(df_temp$Buffer)



# TURN-AROUND TIME DISTRIBUTION BY CARRIER
df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$Buffer>0 
                          &  df_subset$Buffer<120   , ]


df_Analysis %>% group_by(CARRIER) %>% summarise(Mean_TurnAroundTime = mean(Buffer))
df_Analysis %>% group_by(CARRIER) %>% summarise(Mean_DEP_DELAY = mean(DEP_DELAY_NEW))
df_Analysis %>% group_by(CARRIER) %>% summarise(Mean_Actual_TurnAround = mean(Actual_Buffer))













# Histogram: ACTUAL TURN-AROUND TIME DISTRIBUTION
grid <- matrix(c(1,1,2,3), 2, 2, byrow = TRUE)
layout(grid)
par(mfrow=c(3,1))
df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$Buffer>0 
                          &  df_subset$Buffer<120 
                          &  df_subset$Actual_Buffer>0
                          # &  df_subset$Actual_Buffer<40
                          , ]
df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Hub and Spoke', c("CARRIER","DEPARTURE_MINUTES","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Actual_Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp
summary(df_temp)

df_Analysis[ df_Analysis$Carrier_Type == 'Southwest' & df_Analysis$TAIL_NUM=="N209WN" & df_Analysis$DEPARTURE_MINUTES >5000, c("CARRIER","Carrier_Type","DEPARTURE_MINUTES", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Actual_Buffer","Day","Flight", "DEP_TIME", "DEP_DELAY", "ARR_TIME") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]


x <- as.numeric(df_temp$Actual_Buffer)
h<-hist(x, breaks=20, col="#1a3260", xlab="Turn-Around Time (min)", ylab="Number of Flights",
        main="Southwest")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

mean(df_temp$Actual_Buffer)
mean(df_temp$DEP_DELAY)
kurtosis(df_temp$Actual_Buffer)
skewness(df_temp$Actual_Buffer)
sd(df_temp$Actual_Buffer)



df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$Buffer>0 
                          &  df_subset$Buffer<120 
                          &  df_subset$Actual_Buffer>0
                          # &  df_subset$Actual_Buffer<40
                          , ]
df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Point-to-Point without Southwest', c("CARRIER","DEPARTURE_MINUTES","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Actual_Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp
summary(df_temp)

df_Analysis[ df_Analysis$Carrier_Type == 'Southwest' & df_Analysis$TAIL_NUM=="N209WN" & df_Analysis$DEPARTURE_MINUTES >5000, c("CARRIER","Carrier_Type","DEPARTURE_MINUTES", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Actual_Buffer","Day","Flight", "DEP_TIME", "DEP_DELAY", "ARR_TIME") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]


x <- as.numeric(df_temp$Actual_Buffer)
h<-hist(x, breaks=20, col="#1a3260", xlab="Turn-Around Time (min)", ylab="Number of Flights",
        main="Southwest")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

mean(df_temp$Actual_Buffer)
mean(df_temp$DEP_DELAY)
kurtosis(df_temp$Actual_Buffer)
skewness(df_temp$Actual_Buffer)
sd(df_temp$Actual_Buffer)



df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$Buffer>0 
                          &  df_subset$Buffer<120 
                          &  df_subset$Actual_Buffer>0
                         # &  df_subset$Actual_Buffer<40
                         , ]
df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Southwest', c("CARRIER","DEPARTURE_MINUTES","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Actual_Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp
summary(df_temp)

df_Analysis[ df_Analysis$Carrier_Type == 'Southwest' & df_Analysis$TAIL_NUM=="N209WN" & df_Analysis$DEPARTURE_MINUTES >5000, c("CARRIER","Carrier_Type","DEPARTURE_MINUTES", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Actual_Buffer","Day","Flight", "DEP_TIME", "DEP_DELAY", "ARR_TIME") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]


x <- as.numeric(df_temp$Actual_Buffer)
h<-hist(x, breaks=20, col="#1a3260", xlab="Turn-Around Time (min)", ylab="Number of Flights",
        main="Southwest")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

mean(df_temp$Actual_Buffer)
mean(df_temp$DEP_DELAY)
kurtosis(df_temp$Actual_Buffer)
skewness(df_temp$Actual_Buffer)
sd(df_temp$Actual_Buffer)










# Graph: TURN-AROUND TIME vs. LATE_AIRCRAFT_DELAY

df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          #& df_subset$Buffer>0 
                          , ]

df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Southwest' , c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","LATE_AIRCRAFT_DELAY", "DEP_DELAY_NEW", "CARRIER_DELAY" , "DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp

df_temp$Buffer_intervals<-ifelse(df_temp$Buffer<=19,'<20',
                                     ifelse(df_temp$Buffer<=29,'20-29',
                                         ifelse(df_temp$Buffer<=39,'30-39',
                                                ifelse(df_temp$Buffer<=60,'40-60','>1hr'))))


df_temp$Delay_Late<-ifelse(df_temp$LATE_AIRCRAFT_DELAY>15,"Late Aircraf Delay > 15 min","Delay <15 min")

counts1 <- as.matrix(table(df_temp$Delay_Late, df_temp$Buffer_intervals))
counts1 <- counts1[,c("<20","20-29","30-39","40-60", ">1hr")]

counts1 <- cbind(counts1, Total = rowSums(counts1))
counts1 <- rbind(counts1, Total = colSums(counts1))
counts1 <- rbind(counts1, Delay_pct = round((counts1[2,]/counts1[3,]),3)*100)
Total_pct <- c(round(counts1[3,1]/counts1[3,][6],3)*100,
               round(counts1[3,2]/counts1[3,][6],3)*100,
               round(counts1[3,3]/counts1[3,][6],3)*100,
               round(counts1[3,4]/counts1[3,][6],3)*100,
               round(counts1[3,5]/counts1[3,][6],3)*100,
               round(counts1[3,6]/counts1[3,][6],3)*100)
counts1 <- rbind(counts1, Total_pct)
counts1



# SAME AS ABOVE BUT FOR Point-to-Point without Southwest

df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          #& df_subset$Buffer>0 
                          , ]

df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Point-to-Point without Southwest' , c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","LATE_AIRCRAFT_DELAY", "DEP_DELAY_NEW", "CARRIER_DELAY" , "DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp

df_temp$Buffer_intervals<-ifelse(df_temp$Buffer<=19,'<20',
                                 ifelse(df_temp$Buffer<=29,'20-29',
                                        ifelse(df_temp$Buffer<=39,'30-39',
                                               ifelse(df_temp$Buffer<=60,'40-60','>1hr'))))


df_temp$Delay_Late<-ifelse(df_temp$LATE_AIRCRAFT_DELAY>15,"Late Aircraf Delay > 15 min","Delay <15 min")

counts1 <- as.matrix(table(df_temp$Delay_Late, df_temp$Buffer_intervals))
counts1 <- counts1[,c("<20","20-29","30-39","40-60", ">1hr")]

counts1 <- cbind(counts1, Total = rowSums(counts1))
counts1 <- rbind(counts1, Total = colSums(counts1))
counts1 <- rbind(counts1, Delay_pct = round((counts1[2,]/counts1[3,]),3)*100)
Total_pct <- c(round(counts1[3,1]/counts1[3,][6],3)*100,
               round(counts1[3,2]/counts1[3,][6],3)*100,
               round(counts1[3,3]/counts1[3,][6],3)*100,
               round(counts1[3,4]/counts1[3,][6],3)*100,
               round(counts1[3,5]/counts1[3,][6],3)*100,
               round(counts1[3,6]/counts1[3,][6],3)*100)
counts1 <- rbind(counts1, Total_pct)
counts1



#CORRELATION BETWEEN DEPARTURE DELAYS AND LATE AIRCRAFT ARRIVALS
df_temp <- df_temp[ df_temp$Flight == 'Same Plane' 
                      & df_temp$Day != 'New Day' 
                      # & df_temp$Buffer>0 
                      # & df_temp$Buffer<=35
                      # & ( (df_temp$Buffer<=35 & df_temp$id<=4) | (df_temp$Buffer<=100 & df_temp$id>4) )
                      # & df_temp$Carrier_Type == 'Southwest'  
                      # & df_temp$DEP_DELAY_NEW>1
                    & df_temp$DEP_DELAY_NEW<400
                      & df_temp$LATE_AIRCRAFT_DELAY<400
                      ,]
cor(df_temp$DEP_DELAY_NEW,df_temp$LATE_AIRCRAFT_DELAY)
summary(lm(df_temp$DEP_DELAY_NEW ~ df_temp$LATE_AIRCRAFT_DELAY , data = df_temp))
intercept <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$LATE_AIRCRAFT_DELAY, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$LATE_AIRCRAFT_DELAY, data = df_temp))[2],2)
x <- ggplot(df_temp,aes(LATE_AIRCRAFT_DELAY,DEP_DELAY_NEW))
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Late Aircraft Arrival (minutes)") + ylab("Departure Delay") 




# Effect of turnaround into late aircraft 
# MAIN
df_temp <- df_subset[ df_subset$Flight == 'Same Plane' 
                      & df_subset$Day != 'New Day' 
                      & df_subset$Buffer>19 
                      & df_subset$Buffer<=35
                      # & ( (df_subset$Buffer<=35 & df_subset$id<=4) | (df_subset$Buffer<=100 & df_subset$id>4) )
                      # & df_subset$Carrier_Type == 'Southwest'  
                      # & df_subset$DEP_DELAY_NEW>15
                      # & df_subset$LATE_AIRCRAFT_DELAY<50
                      ,]

#color #ffc300

cor(df_temp$Buffer,df_temp$LATE_AIRCRAFT_DELAY)
summary(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$Buffer , data = df_temp))
intercept <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$Buffer, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$Buffer, data = df_temp))[2],2)
x <- ggplot(df_temp,aes(Buffer,LATE_AIRCRAFT_DELAY))
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Turnaround Time (min)") + ylab("Late Aircraft Arrival (minutes)") 


# SAME BUT WITH DEP_DELAY_NEW
df_temp <- df_subset[ df_subset$Flight == 'Same Plane' 
                      & df_subset$Day != 'New Day' 
                      & df_subset$Buffer>0 
                      & df_subset$Buffer<=35
                      # & ( (df_subset$Buffer<=35 & df_subset$id<=4) | (df_subset$Buffer<=100 & df_subset$id>4) )
                      # & df_subset$Carrier_Type == 'Southwest'  
                      # & df_subset$DEP_DELAY_NEW>15
                      # & df_subset$LATE_AIRCRAFT_DELAY<50
                      ,]

cor(df_temp$Buffer,df_temp$DEP_DELAY_NEW)
summary(lm(df_temp$DEP_DELAY_NEW ~ df_temp$Buffer , data = df_temp))
intercept <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$Buffer, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$Buffer, data = df_temp))[2],2)
x <- ggplot(df_temp,aes(Buffer,DEP_DELAY_NEW))
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Turnaround Time (min)") + ylab("Departure Delay (minutes)") 




# SAME BUT WITH ID
df_temp <- df_subset[ df_subset$Flight == 'Same Plane' 
                      #& df_subset$Day != 'New Day' 
                      & df_subset$Buffer>0
                      #& df_subset$Buffer<90 
                      & df_subset$id<8
                      # & df_subset$id>=2
                      # & ( (df_subset$DEP_DELAY_NEW<=3 & df_subset$id<=1) | (df_subset$id>1) )
                       & df_subset$Carrier_Type == 'Southwest'  
                      # & df_subset$DEP_DELAY_NEW<300
                      # & df_subset$LATE_AIRCRAFT_DELAY<50
                      ,]

cor(df_temp$id,df_temp$DEP_DELAY_NEW)
summary(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id , data = df_temp))

intercept <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id, data = df_temp))[2],2)
x <- ggplot(df_temp,aes(id,DEP_DELAY_NEW))
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Flight number of the day") + ylab("Departure Delay (minutes)") 



# SAME BUT WITH ID
df_temp <- df_subset[ df_subset$Flight == 'Same Plane' 
                      #& df_subset$Day != 'New Day' 
                      & df_subset$Buffer>0
                      #& df_subset$Buffer<90 
                      & df_subset$id<8
                      # & df_subset$id>=2
                      # & ( (df_subset$DEP_DELAY_NEW<=3 & df_subset$id<=1) | (df_subset$id>1) )
                      & df_subset$Carrier_Type == 'Point-to-Point without Southwest'  
                      # & df_subset$DEP_DELAY_NEW<300
                      # & df_subset$LATE_AIRCRAFT_DELAY<50
                      ,]

cor(df_temp$id,df_temp$DEP_DELAY_NEW)
summary(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id , data = df_temp))

intercept <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id, data = df_temp))[2],2)
x <- ggplot(df_temp,aes(id,DEP_DELAY_NEW))
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Flight number of the day") + ylab("Departure Delay (minutes)") 







df_temp <- df_subset[ df_subset$Flight == 'Same Plane' 
                      #& df_subset$Day != 'New Day' 
                      & df_subset$Buffer>0 
                      & df_subset$id>=8
                      #& df_subset$id>=1
                      #& df_subset$Buffer<=35
                      & ( (df_subset$DEP_DELAY_NEW<=3 & df_subset$id<=1) | (df_subset$id>1) )
                      # & df_subset$Carrier_Type == 'Southwest'  
                      # & df_subset$DEP_DELAY_NEW>15
                      # & df_subset$LATE_AIRCRAFT_DELAY<50
                      ,]

cor(df_temp$id,df_temp$DEP_DELAY_NEW)
summary(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id , data = df_temp))
intercept <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$DEP_DELAY_NEW ~ df_temp$id, data = df_temp))[2],2)
x <- ggplot(df_temp,aes(id,DEP_DELAY_NEW))
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Flight number of the day") + ylab("Departure Delay (minutes)") 




# summary(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$Buffer + df_temp$id, data = df_temp))


# SAVING FLIGHTS UNDER 30 minutes turnaround

#How many SOuthwest flights have turnaround times under 30 minutes? 
w <- df_subset[ df_subset$Carrier_Type == 'Southwest'
                        &  df_subset$Buffer<35, ]
nrow(w)

df_subset <- df_subset[ df_subset$Carrier_Type == 'Southwest' , ]

df_subset$DEP_DELAY_NEW<-ifelse( is.na(df_subset$DEP_DELAY_NEW) ,0,df_subset$DEP_DELAY_NEW)
df_subset$Buffer <-ifelse( is.na(df_subset$Buffer) ,0,df_subset$Buffer)
df_subset$Buffer_Positive <-ifelse( df_subset$Buffer<0 ,0,df_subset$Buffer)

df_subset$New_Buffer <-ifelse( df_subset$Buffer<30
                            & df_subset$Carrier_Type == 'Southwest' , 30  ,df_subset$Buffer)

df_subset$New_Buffer_v1_2 <-ifelse( df_subset$Buffer<35
                               & df_subset$Carrier_Type == 'Southwest' , 35  ,df_subset$Buffer)

df_subset$New_Buffer_Diff_v1_2 <-ifelse( df_subset$Buffer<35
                                    & df_subset$Carrier_Type == 'Southwest' , df_subset$New_Buffer_v1_2 - df_subset$Buffer_Positive   ,0)



z<- df_subset[ df_subset$Carrier_Type == 'Southwest'
           &  df_subset$Buffer<35, ]
mean(z$New_Buffer_Diff_v1_2)

df_subset$New_Recommended_Delay <-ifelse( df_subset$Buffer<30
                            & df_subset$Carrier_Type == 'Southwest' ,  
                            df_subset$DEP_DELAY_NEW - ((30-df_subset$Buffer)/2)*df_subset$id  ,
                            df_subset$DEP_DELAY_NEW)

df_subset$New_Recommended_Delay <-ifelse( df_subset$New_Recommended_Delay<0 ,0,df_subset$New_Recommended_Delay)


df_subset$New_Recommended_Delay_v2_by_id <-ifelse( df_subset$Buffer<30
                                          & df_subset$Carrier_Type == 'Southwest' ,  
                                          df_subset$DEP_DELAY_NEW - 5*df_subset$id  ,
                                          df_subset$DEP_DELAY_NEW)

df_subset$New_Recommended_Delay_v2_by_id <-ifelse( df_subset$New_Recommended_Delay_v2_by_id<0 ,0,df_subset$New_Recommended_Delay_v2_by_id)



df_subset$New_Recommended_Delay_v1_2 <-ifelse( df_subset$Buffer<35
                                          & df_subset$Carrier_Type == 'Southwest' ,  
                                          df_subset$DEP_DELAY_NEW - ((35-df_subset$Buffer)/2)*df_subset$id  ,
                                          df_subset$DEP_DELAY_NEW)

df_subset$New_Recommended_Delay_v1_2 <-ifelse( df_subset$New_Recommended_Delay_v1_2<0 ,0,df_subset$New_Recommended_Delay_v1_2)



with_new_delay <- df_subset[ df_subset$Carrier_Type == 'Southwest' 
         & df_subset$DEP_DELAY_NEW>=15 
         & df_subset$Buffer<30
         & df_subset$New_Recommended_Delay<15 ,
         c("CARRIER","id","Carrier_Type", "TAIL_NUM", "FL_DATE",
                                                 "Buffer","New_Buffer", "DEP_DELAY_NEW", 
                                                 "New_Recommended_Delay", "DEP_DEL15") ]


with_new_delay_v2 <- df_subset[ df_subset$Carrier_Type == 'Southwest' 
           & df_subset$DEP_DELAY_NEW>=15 
           & df_subset$Buffer<30
           & df_subset$New_Recommended_Delay_v2_by_id<15 ,
           c("CARRIER","id","Carrier_Type", "TAIL_NUM", "FL_DATE",
             "Buffer","New_Buffer", "DEP_DELAY_NEW", 
             "New_Recommended_Delay_v2_by_id", "DEP_DEL15") ]



with_new_delay_v1_2 <- df_subset[ df_subset$Carrier_Type == 'Southwest' 
                                & df_subset$DEP_DELAY_NEW>=15 
                                & df_subset$Buffer<35
                                & df_subset$New_Recommended_Delay_v1_2<15 ,
                                c("CARRIER","id","Carrier_Type", "TAIL_NUM", "FL_DATE",
                                  "Buffer","New_Buffer", "DEP_DELAY_NEW", 
                                  "New_Recommended_Delay_v2_by_id", "DEP_DEL15") ]


x <- df_subset[ df_subset$Carrier_Type == 'Southwest' ,
         c("CARRIER","id","Carrier_Type", "TAIL_NUM", "FL_DATE",
           "Buffer", "DEP_DELAY_NEW",  "DEP_DEL15") ]

ontime_after_v1 <- 1-(sum(x$DEP_DEL15)-nrow(with_new_delay))/nrow(x)
ontime_after_v2 <- 1-(sum(x$DEP_DEL15)-nrow(with_new_delay_v2))/nrow(x)
ontime_after_v1_2 <- 1-(sum(x$DEP_DEL15)-nrow(with_new_delay_v1_2))/nrow(x)
ontime_before <- 1-(sum(x$DEP_DEL15))/nrow(x)

ontime_after_v1 - ontime_before
ontime_after_v2 - ontime_before
ontime_after_v1_2 - ontime_before



df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$New_Buffer>0 
                          &  df_subset$New_Buffer<120   , ]
df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Southwest', c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"New_Buffer","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp
summary(df_temp)

x <- as.numeric(df_temp$New_Buffer)
h<-hist(x, breaks=20, col="#1a3260", xlab="Turn-Around Time (min)", ylab="Number of Flights",
        main="Southwest")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

mean(df_temp$New_Buffer)
kurtosis(df_temp$New_Buffer)
skewness(df_temp$New_Buffer)
sd(df_temp$New_Buffer)






#V1_2
df_Analysis <- df_subset[ df_subset$Flight == 'Same Plane'  
                          & df_subset$Day != 'New Day'  
                          &  df_subset$New_Buffer>0 
                          &  df_subset$New_Buffer<120   , ]
df_temp <- df_Analysis[ df_Analysis$Carrier_Type == 'Southwest', c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","CRS_DEP_TIME","CRS_ARR_TIME" ,"New_Buffer_v1_2","Day","Flight", "GMTO", "GMTD","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DEL15", "DEP_DELAY_GROUP", "ARR_TIME","dep_hours") ] #, "ARR_DELAY","WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]
df_temp
summary(df_temp)

x <- as.numeric(df_temp$New_Buffer_v1_2)
h<-hist(x, breaks=20, col="#1a3260", xlab="Turn-Around Time (min)", ylab="Number of Flights",
        main="Southwest")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

mean(df_temp$New_Buffer_v1_2)
kurtosis(df_temp$New_Buffer_v1_2)
skewness(df_temp$New_Buffer_v1_2)
sd(df_temp$New_Buffer_v1_2)







df_flights <- df_temp %>% group_by(CARRIER, TAIL_NUM, FL_DATE) %>% summarise(Number_Flights = max(Max_Id))
df_flights_Carrier <- df_flights %>% group_by(CARRIER, Carrier_Type) %>% summarise(Number_Flights_per_day = mean(Number_Flights))











# MAIN2
df_temp <- df_subset[ df_subset$Flight == 'Same Plane' 
                      & df_subset$Day != 'New Day' 
                      & df_subset$Buffer>0 
                      # & df_subset$Buffer>120
                      & df_subset$Buffer<=40
                      # & df_subset$Carrier_Type != 'Other'  
                      # & df_subset$DEP_DELAY_NEW>15
                      # & df_subset$LATE_AIRCRAFT_DELAY<10
                      ,]
x <- ggplot(df_temp,aes(Buffer,LATE_AIRCRAFT_DELAY))
intercept <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$Buffer, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$Buffer, data = df_temp))[2],2)
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Turnaround Time (min)") + ylab("Late Aircraft Departure Delay (minutes)") 




# MAIN3
df_temp <- df_subset[ df_subset$Flight == 'Same Plane' 
                      & df_subset$Day != 'New Day' 
                      & df_subset$Buffer>40 
                      # & df_subset$Buffer>120
                      & df_subset$Buffer<=60
                      # & df_subset$Carrier_Type != 'Other'  
                      # & df_subset$DEP_DELAY_NEW>15
                      # & df_subset$LATE_AIRCRAFT_DELAY<10
                      ,]
x <- ggplot(df_temp,aes(Buffer,LATE_AIRCRAFT_DELAY))
intercept <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$Buffer, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$Buffer, data = df_temp))[2],2)
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Turnaround Time (min)") + ylab("Late Aircraft Departure Delay (minutes)") 




c <- 20
df_temp$cut <- ifelse(df_temp$Buffer<20,2,
                      ifelse(df_temp$Buffer<40,4,
                             ifelse(df_temp$Buffer<60,0,
                                    ifelse(df_temp$Buffer<80,8,0))))


x <- ggplot(df_temp,aes(Buffer,LATE_AIRCRAFT_DELAY))
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(round(coef(lm(as.numeric(df_temp$LATE_AIRCRAFT_DELAY) ~ as.numeric(df_temp$Buffer), data = df_temp))[2],2)  )


lm(LATE_AIRCRAFT_DELAY ~ Buffer, data = df_temp)

 y <- df_temp %>% group_by(cut) %>% do(fit = lm(LATE_AIRCRAFT_DELAY ~ Buffer,data=.))
 tidy(y,fit)

x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(round(coef()[2],2)  )


x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + facet_wrap(~cut,scales="free_x")




# Graph Flight number of the day vs. Late Aircraft Delay


df_temp <- df_subset[ df_subset$Flight == 'Same Plane'  
                          #& df_subset$Day != 'New Day'  
                          & df_subset$Buffer>0 
                        & df_subset$id<8
                      & df_subset$DEP_DELAY_NEW>=5
                          , ]
round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$id, data = df_temp))[2],2)


x <- ggplot(df_temp,aes(id,LATE_AIRCRAFT_DELAY))
intercept <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$id, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$id, data = df_temp))[2],2)
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Flight # of the day") + ylab("Late Aircraft Arrival (minutes)") 



df_temp <- df_subset[ df_subset$Flight == 'Same Plane'  
                      #& df_subset$Day != 'New Day'  
                      & df_subset$Buffer>0 
                      & df_subset$id>=8
                      & df_subset$DEP_DELAY_NEW>=5
                      , ]
round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$id, data = df_temp))[2],2)


x <- ggplot(df_temp,aes(id,LATE_AIRCRAFT_DELAY))
intercept <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$id, data = df_temp))[1],2)
slope <- round(coef(lm(df_temp$LATE_AIRCRAFT_DELAY ~ df_temp$id, data = df_temp))[2],2)
x + geom_jitter(alpha=0.5) + geom_smooth(method="lm", colour="#FFC000") + 
    ggtitle(paste("Intercept: " , intercept , " Slope: " , slope )) + xlab("Flight # of the day") + ylab("Late Aircraft Arrival (minutes)") 





df_subset[ df_subset$id>12 & df_subset$CARRIER != "HA", c("CARRIER","Carrier_Type", "TAIL_NUM", "FL_DATE","ORIGIN_CITY_NAME","DEST_CITY_NAME","DEP_DELAY","DEP_DELAY_NEW", "LATE_AIRCRAFT_DELAY","CARRIER_DELAY", "WEATHER_DELAY","NAS_DELAY", "SECURITY_DELAY" ) ]






