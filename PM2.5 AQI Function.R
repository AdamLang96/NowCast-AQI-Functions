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


PM2.5ConcToAQI<-function(x) {
  #Takes in a NowCast concentration and converts it to AQI based on
  # defined breaking points for PM2.5 AQIHigh AQILow ConcHigh and ConcLow
  #
  #Args:
  # NowCast Concentration for PM2.5
  #
  #Returns:
  # NowCast AQI for PM2.5
  round(x,1)
  if(x>=0 & x<=12) {
    ConcLow<-0
    ConcHigh<-12
    AQILow<-0
    AQIHigh<-50
  }
  else if(x>=12.1 & x<=35.4) {
    ConcLow<-12.1
    ConcHigh<-35.4
    AQILow<-51
    AQIHigh<-100
  }
  else if(x>=35.5 & x<=55.4) {
    ConcLow<-35.5
    ConcHigh<-55.4
    AQILow<-101
    AQIHigh<-150
  }
  else if(x>=55.5 & x<=150.4) {
    ConcLow<-55.5
    ConcHigh<-150.4
    AQILow<-151
    AQIHigh<-200
  }
  else if(x>=150.5&x<=250.4) {
    ConcLow<-150.5
    ConcHigh<-250.4
    AQILow<-201
    AQIHigh<-300
  }
  else if(x>=250.5&x<=500.4){
    ConcLow<-250.5
    ConcHigh<-500.4
    AQILow<-301
    AQIHigh<-500
  }
  else {
    print("AQI temperarily unavailable")
  }
  AQI<-round((((AQIHigh-AQILow)/(ConcHigh-ConcLow))*(x-ConcLow))+AQILow)
  return(AQI)
}



PM2.5AQI<-function (df) {
  #Takes a single datatable of PM2.5 by location of sensor and calculates PM2.5 NowCast AQI for that site
  #Returns "PM2.5 NowCast AQI not available" if 2/3 of most recent hours are NA values
  #Args:
  # List of datables of PM2.5 concetration by location of sensor
  #
  #Returns:
  # PM2.5 NowCast AQI and location of sensor
  # "Nowcast not available" if 2/3 of most recent hours are NA values
  LastTwelveHours<-head(df,12)
  PM2.5ConcVar<-LastTwelveHours[["PM2.5.conc"]]
  if((is.na(df[["PM2.5.conc"]][1]) & is.na(df[["PM2.5.conc"]][2]))==TRUE 
                        | (is.na(df[["PM2.5.conc"]][1]) & is.na(df[["PM2.5.conc"]][3]))==TRUE 
                        | (is.na(df[["PM2.5.conc"]][2]) & is.na(df[["PM2.5.conc"]][3]))==TRUE) {
  return("PM2.5 Nowcast AQI not available")
} else {
  for(i in 1:12) { 
    if( is.na(df[["PM2.5.conc"]][i]==TRUE)) {
      PM2.5ConcVar[i]<-0
    }
  }
}
  WeightFactor<-min(PM2.5ConcVar)/max(PM2.5ConcVar)
  if( WeightFactor < 1/2) {
    WeightFactor<-1/2
  }
  NowCastConc<-floor((PM2.5ConcVar[1]+(PM2.5ConcVar[2]*WeightFactor^1)+(PM2.5ConcVar[3]*WeightFactor^2)+
                        (PM2.5ConcVar[4]*WeightFactor^3)+(PM2.5ConcVar[5]*WeightFactor^4)+
                        +(PM2.5ConcVar[6]*WeightFactor^5)+(PM2.5ConcVar[7]*WeightFactor^6)+
                        (PM2.5ConcVar[8]*WeightFactor^7)+(PM2.5ConcVar[9]*WeightFactor^8)+(PM2.5ConcVar[10]*WeightFactor^9)+
                        (PM2.5ConcVar[11]*WeightFactor^10)+(PM2.5ConcVar[12]*WeightFactor^11))
                     /(1+WeightFactor^1+WeightFactor^2+WeightFactor^3+WeightFactor^4
                       +WeightFactor^5+WeightFactor^6+WeightFactor^7+WeightFactor^8+WeightFactor^9+WeightFactor^10+WeightFactor^11))
  AQI<-PM2.5ConcToAQI(NowCastConc)
return(AQI)
}

PM2.5AQIMap<-function(list) {
  #Calculates PM2.5 Nowcast AQI for all sensor locations
  #
  #Args:
  #Nested list of datables organized first by pollutant and then by sensor location
  #
  #Returns:
  # PM2.5 Nowcast concentration for each sensor location
  lapply(list[["PM2.5"]], PM2.5AQI)
}

PM2.5NowCastAQI<-function(df){
  #Takes in Regulatory data and assigns each site a  PM2.5 NowCast AQI score
  #
  #Args:
  # Regulatory datatable
  #
  #Returns:
  # PM2.5 NowCast AQI  for each sensor location
  # "PM2.5 NowCast AQI not available" if AQI conditions are not met
  CleanData<-CleanDataTable(df)
  PollutantLocationNested<-SortByLocation(CleanData)
  FinalResults<-PM2.5AQIMap(PollutantLocationNested)
  print(FinalResults)
}

PM2.5NowCastAQI(RegData)

