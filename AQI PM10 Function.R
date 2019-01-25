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


PM10ConcToAQI<-function(x) {
  #Takes in a NowCast concentration for PM10 and converts it to AQI based on
  # defined breaking points for PM10 AQIHigh AQILow ConcHigh and ConcLow
  #
  #Args:
  # NowCast Concentration for PM10
  #
  #Returns:
  # NowCast AQI for PM10
  round(x)
  if(x>=0 & x<=54) {
    ConcLow<-0
    ConcHigh<-54
    AQILow<-0
    AQIHigh<-50
  }
  else if(x>=55 & x<=154) {
    ConcLow<-55
    ConcHigh<-154
    AQILow<-51
    AQIHigh<-100
  }
  else if(x>=155 & x<=254) {
    ConcLow<-155
    ConcHigh<-254
    AQILow<-101
    AQIHigh<-150
  }
  else if(x>=255 & x<=354) {
    ConcLow<-255
    ConcHigh<-354
    AQILow<-151
    AQIHigh<-200
  }
  else if(x>=355&x<=424) {
    ConcLow<-355
    ConcHigh<-424
    AQILow<-201
    AQIHigh<-300
  }
  else if(x>=425&x<=604){
    ConcLow<-425
    ConcHigh<-604
    AQILow<-301
    AQIHigh<-500
  }
  else {
    print("AQI temperarily unavailable")
  }
  AQI<-round((((AQIHigh-AQILow)/(ConcHigh-ConcLow))*(x-ConcLow))+AQILow)
  return(AQI)
}



PM10AQI<-function (df) {
  #Takes a single datatable of PM10 by location of sensor and calculates PM10 NowCast AQI for that site
  #Returns "PM10 NowCast AQI not available" if 2/3 of most recent hours are NA values
  #Args:
  # A single datatable of PM10 by location of sensor
  #
  #Returns:
  # PM10 NowCast AQI and location of sensor
  # "Nowcast not available" if 2/3 of most recent hours are NA values
  LastTwelveHours<-head(df,12)
  PM10ConcVar<-LastTwelveHours[["PM10.conc"]]
  if((is.na(df[["PM10.conc"]][1]) & is.na(df[["PM10.conc"]][2]))==TRUE 
     | (is.na(df[["PM10.conc"]][1]) & is.na(df[["PM10.conc"]][3]))==TRUE 
     | (is.na(df[["PM10.conc"]][2]) & is.na(df[["PM10.conc"]][3]))==TRUE) {
    return("PM10 Nowcast AQI not available")
  } else {
    for(i in 1:12) { 
      if( is.na(df[["PM10.conc"]][i]==TRUE)) {
        PM10ConcVar[i]<-0
      }
    }
  }
  WeightFactor<-min(PM10ConcVar)/max(PM10ConcVar)
  if( WeightFactor < 1/2) {
    WeightFactor<-1/2
  }
  NowCastConc<-floor((PM10ConcVar[1]+(PM10ConcVar[2]*WeightFactor^1)+(PM10ConcVar[3]*WeightFactor^2)+
                        (PM10ConcVar[4]*WeightFactor^3)+(PM10ConcVar[5]*WeightFactor^4)+
                        +(PM10ConcVar[6]*WeightFactor^5)+(PM10ConcVar[7]*WeightFactor^6)+
                        (PM10ConcVar[8]*WeightFactor^7)+(PM10ConcVar[9]*WeightFactor^8)+(PM10ConcVar[10]*WeightFactor^9)+
                        (PM10ConcVar[11]*WeightFactor^10)+(PM10ConcVar[12]*WeightFactor^11))
                     /(1+WeightFactor^1+WeightFactor^2+WeightFactor^3+WeightFactor^4
                       +WeightFactor^5+WeightFactor^6+WeightFactor^7+WeightFactor^8+WeightFactor^9+WeightFactor^10+WeightFactor^11))
  AQI<-PM10ConcToAQI(NowCastConc)
  return(AQI)
}

PM10AQIMap<-function(list) {
  #Calculates PM10 Nowcast AQI for all sensor locations
  #
  #Args:
  #Nested list of datables organized first by pollutant and then by sensor location
  #
  #Returns:
  # PM10 Nowcast concentration for each sensor location
  lapply(list[["PM10"]], PM10AQI)
}

PM10NowCastAQI<-function(df){
  #Takes in Regulatory data and assigns each site a  PM10 NowCast AQI score
  #
  #Args:
  # Regulatory datatable
  #
  #Returns:
  # PM10 NowCast AQI  for each sensor location
  # "PM10 NowCast AQI not available" if AQI conditions are not met
  CleanData<-CleanDataTable(df)
  PollutantLocationNested<-SortByLocation(CleanData)
  FinalResults<-PM10AQIMap(PollutantLocationNested)
  print(FinalResults)
}



PM10NowCastAQI(RegData)
