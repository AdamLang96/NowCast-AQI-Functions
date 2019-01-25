library('chron')
library('dplyr')
##pulling regulatory data
RegulatoryDataUrl <- "https://storage.googleapis.com/slocleanair-org/pages/air-quality/basicdata.CSV"
RegData <- read.csv(RegulatoryDataUrl, colClasses = c("numeric", "numeric", rep("character", 3), "numeric", "numeric"),
                    na.strings = "NA")
keeps<-c("conc","site","variable","date")



CleanDataTable<-function(df) {
  #Cleans up regulatory data to just consider variables of interest
  #Changes variables to appropriate classes
  #
  #Args:
  # DataTrame with variables: conc, site, variable, date
  #
  #Returns:
  # Cleaned datatable with our variables of interest
  df<-df[keeps] 
  DateandTime<-strptime(df$date,"%d-%b-%Y %H:%M",tz="PST")
  df$dateandtime<-DateandTime
  df$conc<-as.numeric(df$conc)
  df$site<-as.factor(df$site)
  df$variable<-as.factor(df$variable)
  df$date<-NULL
  return(df)
}



SortByPollutant<-function(df) {
  #Takes in cleaned datatable and creates and outputs a list of 3 dataframes
  #Output list consists of a datatable for Ozone, PM10, and PM2.5
  #
  #Args:
  # Previously cleaned datatable
  #
  #Returns:
  # List consisting of a datatable for Ozone, PM10, and PM2.5
  PollutantDataFrames<-split(df,df$variable)
  OzoneData<-PollutantDataFrames["Ozone"]
  PM10Data<-PollutantDataFrames["PM10"]
  PM2.5Data<-PollutantDataFrames["PM2.5"] 
  newlist<-list("Ozone"=OzoneData, "PM10"=PM10Data,"PM2.5"=PM2.5Data)
  return(newlist)
}



SortByLocation<-function(df) {
  # Uses SortByPollutant to create a nested list of 3 elements
  # 3 elements are Ozone, PM2.5, PM10
  # Each element is a list of datatables by location of sensor
  #
  #Args:
  # Previously defined datatable
  #
  #Returns:
  # Nested list of datables organized first by pollutant and then by location
  Result<-SortByPollutant(df)
  Ozone<-data.frame(Result[["Ozone"]])
  PM2.5<-data.frame(Result[["PM2.5"]])
  PM10<-data.frame(Result[["PM10"]])
  OzoneByLocation<-split(Ozone,Ozone$Ozone.site)
  PM2.5ByLocation<-split(PM2.5,PM2.5$PM2.5.site)
  PM10ByLocation<-split(PM10,PM10$PM10.site)
  PollutantAndLocationList<-list("Ozone"=OzoneByLocation, "PM10"=PM10ByLocation,
                                 "PM2.5"=PM2.5ByLocation)
  return(PollutantAndLocationList)
}




OzoneConcToAQI<-function(x) {
  #Takes in a NowCast concentration and converts it to AQI based on
  # defined breaking points for Ozone AQIHigh AQILow ConcHigh and ConcLow
  #
  #Args:
  # NowCast Concentration for Ozone
  #
  #Returns:
  # NowCast AQI for Ozone
  if(x>=0&x<=54) {
    ConcLow<-0
    ConcHigh<-54
    AQILow<-0
    AQIHigh<-50
  }
  else if(x>=55&x<=70) {
    ConcLow<-55
    ConcHigh<-70
    AQILow<-51
    AQIHigh<-100
  }
  else if(x>=71&x<=85) {
    ConcLow<-71
    ConcHigh<-86
    AQILow<-101
    AQIHigh<-150
  } 
  else if(x>=86&x<=105) {
    ConcLow<-86
    ConcHigh<-105
    AQILow<-151
    AQIHigh<-200
    
  } 
  else if(x>=106&x<=200) {
    ConcLow<-106
    ConcHigh<-200
    AQILow<-201
    AQIHigh<-300
  }
  else {
    print("AQI temperarily unavailable")
  }
  AQI<-round((((AQIHigh-AQILow)/(ConcHigh-ConcLow))*(x-ConcLow))+AQILow)
  return(AQI)
}




OzoneAQI<-function(df) {
  #Takes a single datatable of Ozone by location of sensor and calculates Ozone NowCast AQI for that site
  #Returns "AQI not available" if 2/3 of most recent hours are NA values
  #Args:
  # List of datables of Ozone concetration by location of sensor
  #
  #Returns:
  # Ozone NowCast AQI and location of sensor
  # "Nowcast not available" if 2/3 of most recent hours are NA values
  LastEightHours<-head(df,8)
  OzoneConcVar<-LastEightHours[["Ozone.conc"]]
  if((is.na(df[["Ozone.conc"]][1]) & is.na(df[["Ozone.conc"]][2]))==TRUE 
     | (is.na(df[["Ozone.conc"]][1]) & is.na(df[["Ozone.conc"]][3]))==TRUE 
     | (is.na(df[["Ozone.conc"]][2]) & is.na(df[["Ozone.conc"]][3]))==TRUE) {
    return("Ozone Nowcast AQI not available")
  } else {
    for(i in 1:8) {
    if(is.na(OzoneConcVar[i]==TRUE))
        OzoneConcVar[i]<-0
    }
    WeightFactor<-min(OzoneConcVar)/max(OzoneConcVar)
    NowCastConc<-floor((OzoneConcVar[1]+(OzoneConcVar[2]*WeightFactor^1)+(OzoneConcVar[3]*WeightFactor^2)+
            (OzoneConcVar[4]*WeightFactor^3)+(OzoneConcVar[5]*WeightFactor^4)+
            +(OzoneConcVar[6]*WeightFactor^5)+(OzoneConcVar[7]*WeightFactor^6)+
            (OzoneConcVar[8]*WeightFactor^7))/(1+WeightFactor^1+WeightFactor^2+WeightFactor^3+WeightFactor^4+WeightFactor^5+WeightFactor^6+WeightFactor^7))
    AQI<-OzoneConcToAQI(NowCastConc)
    return(AQI)
    }
}



OzoneAQIMap<-function(list) {
  #Calculates Ozone Nowcast AQI for all sensor locations
  #
  #Args:
  #Nested list of datables organized first by pollutant and then by sensor location
  #
  #Returns:
  # Ozone Nowcast concentration for each sensor location
  lapply(list[["Ozone"]], OzoneAQI)
}



OzoneNowCastAQI<-function(df){
  #Takes in Regulatory data and assigns each site a NowCast AQI score
  #
  #Args:
  # Regulatory datatable
  #
  #Returns:
  # NowCast concentration for each sensor location
  # "NowCast AQI not available" if AQI conditions are not met
  CleanData<-CleanDataTable(df)
  PollutantLocationNested<-SortByLocation(CleanData)
  FinalResults<-OzoneAQIMap(PollutantLocationNested)
  print(FinalResults)
}


OzoneNowCastAQI(RegData)



