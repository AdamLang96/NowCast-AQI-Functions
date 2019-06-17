library('chron')
library('dplyr')



########################################## Script ####################################


url1 <- "https://storage.googleapis.com/slocleanair-org/pages/air-quality/basicdata.CSV"
df <- PullData(url1) %>% CleanDataTable %>% SortByLocation %>% DisplayAQI
  



#################################### Functions ###################################


PullData <- function(siteurl) {
  # Creates CSV file from url
  # Args 
  #  "https://storage.googleapis.com/slocleanair-org/pages/air-quality/basicdata.CSV"
  #
  # Returns:
  # CSV
  RegData <- read.csv(siteurl, colClasses = c("numeric", "numeric", rep("character", 3), "numeric", "numeric"),
                      na.strings = "NA")
  return(RegData)
}

######################################################################

CleanDataTable <- function(df) {
  #Cleans up regulatory data to just consider variables of interest
  #Changes variables to appropriate classes
  #
  #Args:
  # DataTrame with variables: conc, site, variable, date
  #
  #Returns:
  # Cleaned datatable with our variables of interest
  keeps<-c("conc", "site",
           "variable", "date",
           "lat",  "lng")
  df <- df[keeps] 
  DateandTime <- as.POSIXct(df$date,"%d-%b-%Y %H:%M",tz="Etc/GMT+8")
  df$dateandtime <- DateandTime
  df$conc <- as.numeric(df$conc)
  df$site <- as.factor(df$site)
  df$variable <- as.factor(df$variable)
  df$date <- NULL
  return(df)
}

######################################################################

SortByPollutant <- function(df) {
  #Takes in cleaned datatable and creates and outputs a list of 3 dataframes
  #Output list consists of a datatable for Ozone, PM10, and PM2.5
  #
  #Args:
  # Previously cleaned datatable
  #
  #Returns:
  # List consisting of a datatable for Ozone, PM10, and PM2.5
  PollutantDataFrames <- split(df,df$variable)
  OzoneData <- as.data.frame(PollutantDataFrames["Ozone"])
  PM10Data  <- as.data.frame(PollutantDataFrames["PM10"])
  PM2.5Data <- as.data.frame(PollutantDataFrames["PM2.5"])
  newlist   <- list("Ozone" = as.data.frame(OzoneData), 
                    "PM10 " = as.data.frame(PM10Data),
                    "PM2.5" = as.data.frame(PM2.5Data))
  return(newlist)
}


######################################################################


SortByLocation <- function(df) {
  # Uses SortByPollutant to create a nested list of 3 elements
  # 3 elements are Ozone, PM2.5, PM10
  # Each element is a list of datatables by location of sensor
  #
  #Args:
  # Previously defined datatable
  #
  #Returns:
  # Nested list of datables organized first by pollutant and then by location
  Result <- SortByPollutant(df)

  Ozone  <- Result$Ozone
  PM2.5  <- Result$PM2.5
  PM10   <- Result$PM10
  OzoneByLocation <- split(Ozone, Ozone$Ozone.site)
  PM2.5ByLocation <- split(PM2.5, PM2.5$PM2.5.site)
  PM10ByLocation  <- split(PM10,  PM10$PM10.site)
  PollutantAndLocationList <- list("Ozone" = OzoneByLocation, 
                                   "PM10"  = PM10ByLocation,
                                   "PM2.5" = PM2.5ByLocation)
  return(PollutantAndLocationList)
}

######################################################################


PMAQICalc <- function(vect) {
  # Takes a vector of PM10/PM2.5 values and
  # Calculates the 12 hour NowCast AQI
  # according to EPA specifications
  # Assumes PM values ordered with
  # most recent value first and decreasing
  # by time
  #
  # Args:
  #  vector of PM10/PM2.5 values
  #
  # Returns:
  #   NowCast AQI
  #
  num <- 0
  dem <- 0
  PM10 <- vect[1:12]
  if(sum(is.na(PM10[1:3])) >= 2) {
    return(NA)
  }
  WeightFactor <- (min(na.omit(PM10)) / max(na.omit(PM10)))
  #min max function cant handle NA
  #guarenteed at least two values so min/max will work
  if(WeightFactor < 1/2) { # EPA defined rule
    WeightFactor  <- 1/2
  }
  for(i in 1:length(PM10)) {
    if(!is.na(PM10[i])) { #iterates for only non-NA values
      num <- num + (PM10[i] * (WeightFactor ^ (i-1)))
      dem <- dem + (WeightFactor ^ (i-1))
    } else { #do nothing 
      #skips step in the iteration
    }
  }
  AQI <- Converter(num / dem)
  return(AQI)
}

######################################################################


Converter <- function(x, table = AQILookUpTable, pol = "pm10") {
  #Takes in a NowCast concentration, AQIlookuptable, and polutant
  #Returns the NowCast AQI value
  #Used in AQICalc
  #
  #Args:
  # Nowcast AQI concentration for PM10 and AQIlookuptable
  #
  #Returns:
  # NowCastAQI
  stopifnot(is.numeric(x), length(x) == 1)
  if(is.na(x)) return (NA)
  AQILo  <- table$AQILo[max(which(x >= table[, pol]))]
  AQIHi  <- table$AQIHi[max(which(x >= table[, pol]))]
  ConcLo <- table[, pol][max(which(x >= table[, pol]))]
  ConcHi <- table[, pol][max(which(x >= table[, pol])) + 1] - 1
  AQI    <- round((((AQIHi-AQILo) / (ConcHi-ConcLo)) * (x-ConcLo)) + AQILo)
  return(AQI)
}

######################################################################


#DataFrame for comparing NowCast concentrations
#to AQI values
AQILookUpTable <- data.frame(ozone = c(0, 55, 71, 86, 106, 201, NA, NA), 
                             pm2.5 = c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5, 500.4),
                             pm10  = c(0, 55, 155, 255, 355, 425, 505, 604),
                             so2   = c(0, 36, 76, 186, 305, 605, 805, 1004),
                             AQILo = c(0, 51, 101, 151, 201, 301, 401, NA),
                             AQIHi = c(50, 100, 150, 200, 300, 400, 500, NA))



######################################################################


DisplayAQI <- function(pollist, pol = "PM10") {
  # Takes output from SortByLocation and returns a dataframe
  # One row per site with site name, AQI, pollutant, lat, lon, date
  #
  # Args:
  #  SortByLocation output
  #  pol: either "PM10", "PM2.5", "Ozone"
  #
  # Returns:
  # dataframe with one row per site
  # cols: site name, AQI, pollutant, lat, lon, date 
  df <- data.frame("Site" = NA,
                   "AQI"  = NA,
                   "Variable" = pol,
                   "Lat" = NA,
                   "Lon" = NA,
                   "Date" = NA)
  dflist <- pollist[[pol]]
  for(i in 1:length(dflist)) {
    cols  <- colnames(dflist[[i]])
    site  <- names(dflist[i])
    as.df <- as.data.frame(dflist[[i]])
    if(nrow(as.df) == 0 ) {
      vec <- data.frame("AQI"      = NA,
                        "Site"     = site ,
                        "Variable" = pol,
                        "Lat" = NA,
                        "Lon" = NA,
                        "Date" = NA)
    } else {
      
      polname <- grep("conc", cols, value = TRUE)
      latname  <- grep("lat", cols, value = TRUE)
      lonname  <- grep("lng", cols, value = TRUE)
      datename <- grep("dateandtime", cols, value = TRUE)
      if(pol == "Ozone") {
      AQI <-  OzoneAQICalc(as.df[[polname]])
      } else {
      AQI <-  PMAQICalc(as.df[[polname]])
      }
      
      vec <- data.frame("AQI"  = AQI,
                        "Site" =  site,
                        "Variable" = pol,
                        "Lat" = as.df[[latname]][1],
                        "Lon" = as.df[[lonname]][1],
                        "Date" = as.df[[datename]][1])

    }
    df <- rbind(df, vec)
    attributes(df$Date) <- attributes(vec$Date)
  }
    return(df)
}

#####################################

OzoneAQICalc<- function(vect) {
  # Takes a vector of Ozone values and
  # Calculates the 8 hour NowCast AQI
  # according to EPA specifications
  # Assumes Ozone values ordered with
  # most recent value first and decreasing
  # by time
  #
  # Args:
  #  vector of Ozone values
  #
  # Returns:
  #   NowCast AQI
  #
  num <- 0
  dem <- 0
  Ozone <- vect[1:8]
  if(sum(is.na(Ozone[1:3])) >= 2) {
    return(NA)
  }
  WeightFactor <- 1 - ((max(na.omit(Ozone)) - min(na.omit(Ozone))) / max(na.omit(Ozone)))
  if(WeightFactor > 1) {
    WeightFactor <- 1
  }
  for(i in 1:length(Ozone)) {
    if(!is.na(Ozone[i])) {
      num <- num + (Ozone[i] * (WeightFactor ^ (i-1)))
      dem <- dem + (WeightFactor ^ (i-1))
    } else { #do nothing 
      #skips step in the iteration
    }
  }
  AQI <- Converter(num / dem)
}


###################################################


