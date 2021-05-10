# e.g. for NEON OSBS (other sites processed the same way)

#### DOWNLOAD & CLEAN DRIVER DATA:
{
  # RH & air temp: https://data.neonscience.org/data-products/DP1.00098.001
  # wind speed: https://data.neonscience.org/data-products/DP1.00001.001
  # incoming long/shortwave radiation: https://data.neonscience.org/data-products/DP1.00023.001
  # precip: https://data.neonscience.org/data-products/DP1.00006.001 
  # LAI (MCD15A3H): https://modis.ornl.gov/sites 
  # NDVI (MOD13Q1): https://modis.ornl.gov/sites
  # albedo (MCD43A3): https://lpdaacsvc.cr.usgs.gov/appeears/

  ### WIND SPEED
  {
    # Download OSBS -> NEON_wind-2d 
    # read meta-data on sensor positions (.csv file); 
    # selected sensor L1 ("010" in file string) -- this is what we used for the other sites 
    # file name string for 30-min observations for sensor L1: NEON.D03.OSBS.DP1.00001.001.000.010.030.2DWSD_30min
    # use string to search NEON_wind-2d folder for ONLY .csv -> select and move (or copy) files into new folder
    # set working director to new folder:
    setwd("~/Downloads/OSBS_wind")
    files1 <- list.files(pattern = "^[N]", all.files = TRUE, full.names = TRUE, recursive = TRUE) # ^[N] -- file name begins w/ N
    df1 <- read.csv(files1[1],header = TRUE)
    df2 <- read.csv(files1[2],header = TRUE)
    df <- rbind(df1,df2)
    for ( i in 3: length(files1)){
      dfi <- read.csv(files1[i],header = TRUE)
      df <- rbind(df,dfi)
    }
    
    # Extract the Date, Year, DOY
    df$date <- paste0(substr(df$startDateTime,1,10))
    # Get Year & DOY
    # a function for get year
    DF <- data.frame(Date = df$date)
    DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
    Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
    DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
    # add columns to df
    df$Year[1:nrow(df)] <- as.numeric(DF_trans$Year)
    df$DOY[1:nrow(df)] <- as.numeric(DF_trans$NumDays)
    df$Hour[1:nrow(df)] <- rep(seq(0,23.5,by=0.5),(nrow(df)/48))
    # unique(df$Year)
    # unique(df$DOY)
    write.csv(df,file = "~/Downloads/OSBS_wind/OSBS.WS.30m.csv",row.names=FALSE)
  }
  # ^ follow the same apporach for RH, air temp, and radiation
  
  ### PRECIP
  {
    # OSBS 30 min primary precip (~17 mi. (27 km) from tower): NEON.D03.OSBS.DP1.00006.001.900.000.030.PRIPRE_30min
    setwd("~/Downloads/OSBS_precip")
    files1 <- list.files(pattern = "^[N]", all.files = TRUE, full.names = TRUE, recursive = TRUE) # ^[N] -- file name begins w/ N
    df1 <- read.csv(files1[1],header = TRUE)
    df2 <- read.csv(files1[2],header = TRUE)
    df <- rbind(df1,df2)
    for ( i in 3: length(files1)){
      dfi <- read.csv(files1[i],header = TRUE)
      df <- rbind(df,dfi)
    }
    
    # Extract the Date, Year, DOY
    df$date <- paste0(substr(df$startDateTime,1,10))
    # Get Year & DOY
    # a function for get year
    DF <- data.frame(Date = df$date)
    DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
    Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
    DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
    # add columns to df
    df$Year[1:nrow(df)] <- as.numeric(DF_trans$Year)
    df$DOY[1:nrow(df)] <- as.numeric(DF_trans$NumDays)
    df$Hour[1:nrow(df)] <- rep(seq(0,23.5,by=0.5),(nrow(df)/48))
    # unique(df$Year)
    # unique(df$DOY)
    write.csv(df,file = "~/Downloads/OSBS_precip/OSBS.precip.30m.ALL.csv",row.names=FALSE)
    
    OSBS.30m <- read.csv("OSBS.precip.30m.ALL.csv")
    OSBS.30m$priPrecipBulk[which(OSBS.30m$priPrecipRangeQF == 1)] <- NA # NA where  QF fails
    daily_precip <- aggregate(OSBS.30m$priPrecipBulk, 
                              by=list(Category=OSBS.30m$date), FUN=sum) # sum daily precip
    colnames(daily_precip)[1]  <- "date"
    colnames(daily_precip)[2]  <- "daily_precip"
    OSBS.30m <- merge(data.frame(OSBS.30m, row.names=NULL), 
                      data.frame(daily_precip, row.names=NULL), 
                      by = "date", all = TRUE) # join daily precip 
    OSBS.30m <- OSBS.30m[,c(1,4,12,14:17)] # subset data (keep, precip, QF, Year, DOY, Hour, daily_precip)
    write.csv(OSBS.30m,file = "~/Downloads/OSBS_precip/OSBS.precip.30m.csv",row.names=FALSE)
    
  }
  
  ### ALBEDO
  {
    # Data source: AppEEARS
    # OSBS lat, long: 29.689282, -81.993431
    albedo <- read.csv("OSBS_albedo.csv")
    # black-sky albedo
    albedo$MCD43A3_006_Albedo_BSA_Band1[which(albedo$MCD43A3_006_BRDF_Albedo_Band_Mandatory_Quality_Band1 != 0)] <- NA # NA where QF =/= 0 data
    albedo$MCD43A3_006_Albedo_BSA_Band2[which(albedo$MCD43A3_006_BRDF_Albedo_Band_Mandatory_Quality_Band2 != 0)] <- NA 
    albedo$MCD43A3_006_Albedo_BSA_Band3[which(albedo$MCD43A3_006_BRDF_Albedo_Band_Mandatory_Quality_Band3 != 0)] <- NA 
    albedo$MCD43A3_006_Albedo_BSA_Band4[which(albedo$MCD43A3_006_BRDF_Albedo_Band_Mandatory_Quality_Band4 != 0)] <- NA 
    # white-sky albedo
    albedo$MCD43A3_006_Albedo_WSA_Band1[which(albedo$MCD43A3_006_BRDF_Albedo_Band_Mandatory_Quality_Band1 != 0)] <- NA # NA where QF =/= 0 data
    albedo$MCD43A3_006_Albedo_WSA_Band2[which(albedo$MCD43A3_006_BRDF_Albedo_Band_Mandatory_Quality_Band2 != 0)] <- NA 
    albedo$MCD43A3_006_Albedo_WSA_Band3[which(albedo$MCD43A3_006_BRDF_Albedo_Band_Mandatory_Quality_Band3 != 0)] <- NA 
    albedo$MCD43A3_006_Albedo_WSA_Band4[which(albedo$MCD43A3_006_BRDF_Albedo_Band_Mandatory_Quality_Band4 != 0)] <- NA 
    
    albedo_bands <- subset(albedo, select = c(4,8:11,15:18)) # keep WSA & BSA Bands 1-4
    
    # get DOY
    # a function for get year
    DF <- data.frame(Date = albedo_bands$Date)
    DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
    Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
    DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
    albedo_bands$DOY[1:nrow(albedo_bands)] <- as.numeric(DF_trans$NumDays) # add DOY
    
    # daily long-term means
    library(plyr)
    Band1_BSA <- ddply(albedo_bands, "DOY", summarise, mean = mean(MCD43A3_006_Albedo_BSA_Band1, na.rm = T))
    names(Band1_BSA)<-c("DOY","Band1_BSA") # rename columns
    Band2_BSA <- ddply(albedo_bands, "DOY", summarise, mean = mean(MCD43A3_006_Albedo_BSA_Band2, na.rm = T))
    names(Band2_BSA)<-c("DOY","Band2_BSA") # rename columns
    Band3_BSA <- ddply(albedo_bands, "DOY", summarise, mean = mean(MCD43A3_006_Albedo_BSA_Band3, na.rm = T))
    names(Band3_BSA)<-c("DOY","Band3_BSA") # rename columns
    Band4_BSA <- ddply(albedo_bands, "DOY", summarise, mean = mean(MCD43A3_006_Albedo_BSA_Band4, na.rm=T))
    names(Band4_BSA)<-c("DOY","Band4_BSA") # rename columns
    Band1_WSA <- ddply(albedo_bands, "DOY", summarise, mean = mean(MCD43A3_006_Albedo_WSA_Band1, na.rm = T))
    names(Band1_WSA)<-c("DOY","Band1_WSA") # rename columns
    Band2_WSA <- ddply(albedo_bands, "DOY", summarise, mean = mean(MCD43A3_006_Albedo_WSA_Band2, na.rm = T))
    names(Band2_WSA)<-c("DOY","Band2_WSA") # rename columns
    Band3_WSA <- ddply(albedo_bands, "DOY", summarise, mean = mean(MCD43A3_006_Albedo_WSA_Band3, na.rm = T))
    names(Band3_WSA)<-c("DOY","Band3_WSA") # rename columns
    Band4_WSA <- ddply(albedo_bands, "DOY", summarise, mean = mean(MCD43A3_006_Albedo_WSA_Band4, na.rm = T))
    names(Band4_WSA)<-c("DOY","Band4_WSA") # rename columns
    
    # merge bands
    albedo_means <- merge(data.frame(Band1_BSA, row.names=NULL),
                          data.frame(Band2_BSA, row.names=NULL), 
                          by = "DOY", all = F)
    albedo_means <- merge(data.frame(albedo_means, row.names=NULL), 
                          data.frame(Band3_BSA, row.names=NULL), 
                          by = "DOY", all = F) 
    albedo_means <- merge(data.frame(albedo_means, row.names=NULL), 
                          data.frame(Band4_BSA, row.names=NULL), 
                          by = "DOY", all = F) 
    albedo_means <- merge(data.frame(albedo_means, row.names=NULL), 
                          data.frame(Band1_WSA, row.names=NULL), 
                          by = "DOY", all = F) 
    albedo_means <- merge(data.frame(albedo_means, row.names=NULL), 
                          data.frame(Band2_WSA, row.names=NULL), 
                          by = "DOY", all = F) 
    albedo_means <- merge(data.frame(albedo_means, row.names=NULL), 
                          data.frame(Band3_WSA, row.names=NULL), 
                          by = "DOY", all = F) 
    albedo_means <- merge(data.frame(albedo_means, row.names=NULL), 
                          data.frame(Band4_WSA, row.names=NULL), 
                          by = "DOY", all = F) 
    
    write.csv(albedo_means,file 
              = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS.albedo.mean.csv",row.names=FALSE)
    
  }
  
  ### LAI
  {
    # Data source: https://modis.ornl.gov/sites/?id=us_florida_neon_osbs
    # manually add DOY column to csv in Excel from modis_date column: =VALUE(RIGHT(C2,LEN(C2)-5))
    LAI.4d <- read.csv("statistics_Lai_500m.csv")
    plot(LAI.4d$DOY,LAI.4d$value_mean)
    # mean LAI by DOY
    library(plyr)
    DOY.LAImean <- ddply(LAI.4d, "DOY", summarise, mean = mean(value_mean))
    plot(DOY.LAImean$DOY,DOY.LAImean$mean)
    df<-data.frame(NA,1:365) # new df w/ 1-365
    names(df)<-c("mean","DOY") # rename columns in df
    DOY365.LAImean <- rbind(DOY.LAImean, df) # bind df w/ DOY estimates
    DOY365.LAImean <- DOY365.LAImean[!duplicated(DOY365.LAImean$DOY),] # remove duplicate days
    
    library(zoo)
    series <- zoo(DOY365.LAImean, order.by = DOY365.LAImean$DOY) # make time-series ordered value list
    index(series) <- series[,1] 
    series_approx <- na.approx(series) # approximate NA's
    LAIseries_approx <- as.data.frame(series_approx) # make list into df
    plot(LAIseries_approx)
    write.csv(LAIseries_approx,
              file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS.LAI.DOY.csv",
              row.names=FALSE)
    
    
  }
  
  ### NDVI 
  {
    # Data source: https://modis.ornl.gov/sites/?id=us_florida_neon_osbs
    # manually add DOY column to csv in Excel from modis_date column: =VALUE(RIGHT(C2,LEN(C2)-5))
    NDVI.16d <- read.csv("statistics_250m_16_days_NDVI.csv")
    # mean NDVI by DOY
    library(plyr)
    DOY.NDVImean <- ddply(NDVI.16d, "DOY", summarise, mean = mean(value_mean))
    plot(DOY.NDVImean$DOY,DOY.NDVImean$mean)
    df<-data.frame(NA,1:365) # new df w/ 1-365
    names(df)<-c("mean","DOY") # rename columns in df
    DOY365.NDVImean <- rbind(DOY.NDVImean, df) # bind df w/ DOY estimates
    DOY365.NDVImean <- DOY365.NDVImean[!duplicated(DOY365.NDVImean$DOY),] # remove duplicate days
    
    library(zoo)
    timeseries <- zoo(DOY365.NDVImean, order.by = DOY365.NDVImean$DOY) # make time-series ordered value list
    index(timeseries) <- timeseries[,1] 
    timeseries_approx <- na.approx(timeseries) # approximate NA's
    NDVItimeseries_approx <- as.data.frame(timeseries_approx) # make list into df
    plot(NDVItimeseries_approx)
    write.csv(NDVItimeseries_approx,
              file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS.NDVI.DOY.csv",
              row.names=FALSE)
    
    
  }
}


#### CLEANING & MERGING DATA
{
  # NEON TARGETS
  {
    targets <- read.csv("terrestrial_30min-targets.csv")
    targets$date <- paste0(substr(targets$time,1,10))
    # Get Year & DOY
    # a function for get year
    DF <- data.frame(Date = targets$date)
    DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
    Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
    DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),
                          TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
    targets$Year[1:nrow(targets)] <- as.numeric(DF_trans$Year)# add yr column
    targets$DOY[1:nrow(targets)] <- as.numeric(DF_trans$NumDays) # add DOY column
    targets$Hour[1:nrow(targets)] <- rep(seq(0,23.5,by=0.5),(nrow(targets)/48)) # add hour column
    # new key column (year_DOY_hour) 
    library(dplyr)
    library(tidyr)
    targets <- targets %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F) 
    targets_OSBS <- targets[!(targets$siteID=="SRER") & 
                              !(targets$siteID=="BART") &
                              !(targets$siteID=="OSBS"),] # remove non-OSBS
    write.csv(targets_OSBS,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/targets_OSBS.csv",row.names=FALSE)
  }
  targets_OSBS <- read.csv("targets_OSBS.csv")
  
  # RADIATION
  radiation_OSBS <- read.csv("OSBS.radiation.30m.csv")
  radiation_OSBS$inSWMean[which(radiation_OSBS$inSWFinalQF == 1)] <- NA # NA where QF = 1 data
  radiation_OSBS$inLWMean[which(radiation_OSBS$inLWFinalQF == 1)] <- NA 
  radiation_OSBS_key <- radiation_OSBS %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F) # add key
  radiation_OSBS_key <- radiation_OSBS_key[,c(3,64, 126)] # subset data
  radiation_OSBS_key <- radiation_OSBS_key[!is.na(radiation_OSBS_key$inSWMean) 
                                           & !is.na(radiation_OSBS_key$inLWMean),]
  
  # PRECIP
  precip_OSBS <- read.csv("OSBS.precip.30m.csv")
  # already NA where QF = 1 (line 162)
  precip_OSBS_key <- precip_OSBS %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F)
  precip_OSBS_key <- precip_OSBS_key[,c(2,4,8) ] 
  
  # AIR TEMP & RH
  airtemp_RH_OSBS <- read.csv("OSBS.RH.30m.csv")
  airtemp_RH_OSBS$tempRHMean[which(airtemp_RH_OSBS$tempRHFinalQF == 1)] <- NA
  airtemp_RH_OSBS$RHMean[which(airtemp_RH_OSBS$RHFinalQF == 1)] <- NA
  airtemp_RH_OSBS_key <- airtemp_RH_OSBS %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F)
  airtemp_RH_OSBS_key <- airtemp_RH_OSBS_key[ ,c(3,32,94) ]
  airtemp_RH_OSBS_key <- airtemp_RH_OSBS_key[!is.na(airtemp_RH_OSBS_key$RHMean) 
                                             & !is.na(airtemp_RH_OSBS_key$tempRHMean),]
  # Calculate VPD from RH and temp
  library(REddyProc)
  airtemp_RH_OSBS_key$VPD <- fCalcVPDfromRHandTair(rH = airtemp_RH_OSBS_key$RHMean, 
                                                   Tair = airtemp_RH_OSBS_key$tempRHMean)
  
  # WIND SPEED
  WS_OSBS <- read.csv("OSBS.WS.30m.csv")
  WS_OSBS$windSpeedMean[which(WS_OSBS$windDirFinalQF == 1)] <- NA
  WS_OSBS_key <- WS_OSBS %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F)
  WS_OSBS_key <- WS_OSBS_key[ ,c(3,62) ]
  WS_OSBS_key <- WS_OSBS_key[!is.na(WS_OSBS_key$windSpeedMean),]
  
  # LAI
  LAI_OSBS <- read.csv("OSBS.LAI.DOY.csv")
  plot(LAI_OSBS)
  colnames(LAI_OSBS)[2]  <- "mean_LAI"
  
  # NDVI
  NDVI_OSBS <- read.csv("OSBS.NDVI.DOY.csv", header= T)
  plot(NDVI_OSBS)
  colnames(NDVI_OSBS)[2]  <- "mean_NDVI"
  
  # ALBEDO
  albedo_OSBS <- read.csv("OSBS.albedo.mean.csv", header= T)
  
  # merge df's
  merged_OSBS <- merge(data.frame(targets_OSBS, row.names=NULL),
                       data.frame(radiation_OSBS_key, row.names=NULL), 
                       by = "key", all = TRUE) # targets + incoming SW/LW radiation
  merged_OSBS <- merge(data.frame(merged_OSBS, row.names=NULL), 
                       data.frame(precip_OSBS_key, row.names=NULL), 
                       by = "key", all = F) # precip
  merged_OSBS <- merge(data.frame(merged_OSBS, row.names=NULL), 
                       data.frame(airtemp_RH_OSBS_key, row.names=NULL), 
                       by = "key", all = F) # air temp & RH
  merged_OSBS <- merge(data.frame(merged_OSBS, row.names=NULL), 
                       data.frame(WS_OSBS_key, row.names=NULL), 
                       by = "key", all = F) # windspeed
  merged_OSBS <- merged_OSBS[!is.na(merged_OSBS$siteID) ,] # remove rows not in target data
  merged_OSBS <- merge(data.frame(merged_OSBS, row.names=NULL), 
                       data.frame(LAI_OSBS, row.names=NULL), 
                       by = "DOY", all = T) # LAI
  merged_OSBS <- merge(data.frame(merged_OSBS, row.names=NULL), 
                       data.frame(NDVI_OSBS, row.names=NULL), 
                       by = "DOY", all = F) # NDVI
  merged_OSBS <- merge(data.frame(merged_OSBS, row.names=NULL), 
                       data.frame(albedo_OSBS, row.names=NULL), 
                       by = "DOY", all = F) # NDVI
  
  # reorder observations by key (i.e., in chronological order)
  merged_OSBS <- merged_OSBS[order(merged_OSBS$key),] 
  write.csv(merged_OSBS,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS_merged.csv",row.names=FALSE)
  
}


#### DATA PARTITIONING
{
  split_OSBS <- read.csv("OSBS_merged.csv")
  split_OSBS_VSWC <- split_OSBS[!is.na(split_OSBS$vswc),] # remove NA -> VSWC subsetted
  plot(split_OSBS_VSWC$vswc~split_OSBS_VSWC$DOY)
  
  ### LE & NEE:
  split_OSBS <- split_OSBS[!is.na(split_OSBS$le) 
                           & !is.na(split_OSBS$nee),]
  # 1-month chunks of data:
  # 5 days * 48 30-min periods/day = 240 observations
  # 4226 total observations / 240 = 17.60833 => 17
  # 80% TRAIN = 13; 20% TEST = 4
  
  library(Hmisc)
  split_OSBS$groups<-as.numeric(cut2(1:nrow(split_OSBS), m=240)) # split into 17 groups
  # random number generator to select 4 groups randomly for testing set
  # Randomly selected for TEST: 5, 22, 25, 18
  
  # rename as TRAIN and TEST
  split_OSBS$groups[which(split_OSBS$groups == 5)] <- "TEST" 
  split_OSBS$groups[which(split_OSBS$groups == 22)] <- "TEST" 
  split_OSBS$groups[which(split_OSBS$groups == 25)] <- "TEST" 
  split_OSBS$groups[which(split_OSBS$groups == 18)] <- "TEST" 
  split_OSBS$groups[which(split_OSBS$groups != "TEST")] <- "TRAIN" 
  write.csv(split_OSBS,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS_partitioned.csv",row.names=FALSE)
  
  
  ### VSWC ONLY:
  # for some sites, where data were limited for one of the targets, partitioned the data seperately
  # 1-week chunks of data:
  # 5 days * 48 30-min periods/day = 240 observations
  # 7508 total observations / 240 = 31.28333 => 31
  # 80% TRAIN = 25; 20% TEST = 6
  
  library(Hmisc)
  split_OSBS_VSWC$groups<-as.numeric(cut2(1:nrow(split_OSBS_VSWC), m=240)) # split into 32 groups
  # Randomly selected for TEST: 1, 5, 24, 22, 10, 15
  split_OSBS_VSWC$groups[which(split_OSBS_VSWC$groups == 1)] <- "TEST" 
  split_OSBS_VSWC$groups[which(split_OSBS_VSWC$groups == 5)] <- "TEST" 
  split_OSBS_VSWC$groups[which(split_OSBS_VSWC$groups == 24)] <- "TEST" 
  split_OSBS_VSWC$groups[which(split_OSBS_VSWC$groups == 22)] <- "TEST" 
  split_OSBS_VSWC$groups[which(split_OSBS_VSWC$groups == 10)] <- "TEST" 
  split_OSBS_VSWC$groups[which(split_OSBS_VSWC$groups == 15)] <- "TEST" 
  split_OSBS_VSWC$groups[which(split_OSBS_VSWC$groups != "TEST")] <- "TRAIN" 
  
  write.csv(split_OSBS_VSWC,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS_partitioned_VSWC.csv",row.names=FALSE)
  
  
}


#### RANDOM FOREST MODEL
{
  ### LE & NEE:
  OSBS_partitioned <- read.csv("OSBS_partitioned.csv")
  OSBS_train <- OSBS_partitioned[!(OSBS_partitioned$groups=="TEST"),] # remove groups = TEST
  OSBS_test <- OSBS_partitioned[!(OSBS_partitioned$groups=="TRAIN"),] # remove groups = TRAIN
  ### VSWC ONLY:
  OSBS_partitioned_VSWC <- read.csv("OSBS_partitioned_VSWC.csv")
  OSBS_VSWC_train <- OSBS_partitioned_VSWC[!(OSBS_partitioned_VSWC$groups=="TEST"),] # remove groups = TEST
  
  library("randomForest")
  library ("ggplot2")
  library("ggRandomForests")
  library("gridExtra")
  library(regclass)
  set.seed(123)
  
  # ALL PREDICTORS:
  # Hour + DOY + inSWMean + inLWMean + priPrecipBulk + daily_precip + RHMean + tempRHMean + 
  # VPD + windSpeedMean + mean_LAI + mean_NDVI + Band1_BSA + Band1_WSA + Band2_BSA + 
  # Band2_WSA +Band3_BSA + Band3_WSA + Band4_BSA + Band4_WSA
  
  
  ### VSWC
  {
    RF_VSWC <- randomForest(vswc ~ DOY + inLWMean + daily_precip + 
                              RHMean + tempRHMean + mean_LAI + mean_NDVI + 
                              Band4_BSA + Band4_WSA,
                            data=OSBS_VSWC_train, importance=TRUE, 
                            ntree = 1000, na.action=na.omit)
    print(RF_VSWC) # model summary
    plot(gg_vimp(RF_VSWC)) # VIMP plot
    summarize_tree(RF_VSWC) # MSE & node purity
    # GLOBAL MODEL:
    # %var. explained = 92.95 
    # mean sq. res. = 0.0001234984
    # BEST MODEL:
    # %var. explained = 92.99
    # mean sq. res. = 0.0001307164
    
  }
  
  
  ### NEE
  {
    RF_NEE <- randomForest(nee ~ Hour + DOY + inSWMean + daily_precip + 
                             RHMean + tempRHMean + VPD + mean_LAI + mean_NDVI ,
                           data=OSBS_train, importance=TRUE, 
                           ntree = 1000, na.action=na.omit)
    print(RF_NEE) # model summary
    plot(gg_vimp(RF_NEE)) # VIMP plot
    summarize_tree(RF_NEE) # MSE & node purity
    # GLOBAL MODEL:
    # %var. explained = 66.5
    # mean sq. res. = 9.123064
    # BEST MODEL:
    # %var. explained = 68.04
    # mean sq. res. = 9.384084
  }
  
  
  ### LE
  {
    RF_LE <- randomForest(le ~ Hour + DOY + inSWMean + inLWMean + priPrecipBulk +
                            RHMean + tempRHMean + VPD + mean_LAI + mean_NDVI + Band2_WSA,
                          data=OSBS_train, importance=TRUE, 
                          ntree = 1000, na.action=na.omit)
    print(RF_LE) # model summary
    plot(gg_vimp(RF_LE)) # VIMP plot
    summarize_tree(RF_LE) # MSE & node purity
    # GLOBAL MODEL:
    # %var. explained = 62.12
    # mean sq. res. = 2527.205
    # BEST MODEL:
    # %var. explained = 63.75
    # mean sq. res. = 2663.943
  }
  
}


#### FORECAST TARGETS
{ 
  ### DATA AGGREGATION
  {
    ### FORECASTED MET DATA
    setwd("~/Downloads/OSBS")
    files1 <- list.files(pattern = "^[N]", all.files = TRUE, full.names = TRUE, recursive = TRUE) # ^[N] -- file name begins w/ N
    df1 <- read.csv(files1[1],header = TRUE)
    df2 <- read.csv(files1[2],header = TRUE)
    df <- rbind(df1,df2)
    for ( i in 3: length(files1)){
      dfi <- read.csv(files1[i],header = TRUE)
      df <- rbind(df,dfi)
    }
    
    ### AVERAGE ACROSS 30 ENSEMABLE FORECASTS
    library(plyr)
    temp_for <- ddply(df, "X", summarise, mean = mean(air_temperature, na.rm = T)) # air temp
    names(temp_for)<-c("obs","tempRHMean") # rename columns
    RH_for <- ddply(df, "X", summarise, mean = mean(relative_humidity, na.rm = T)) # RH
    names(RH_for)<-c("obs","RHMean")
    df$longwave_in <- as.numeric(as.character(df$longwave_in)) # character => numeric
    LW_for <- ddply(df, "X", summarise, mean = mean(longwave_in, na.rm = T)) # LW
    names(LW_for)<-c("obs","inLWMean")
    df$shortwave_in <- as.numeric(as.character(df$shortwave_in)) # character => numeric
    SW_for <- ddply(df, "X", summarise, mean = mean(shortwave_in, na.rm = T)) # SW
    names(SW_for)<-c("obs","inSWMean")
    df$precipitation <- as.numeric(as.character(df$precipitation)) # character => numeric
    precip_for <- ddply(df, "X", summarise, mean = mean(precipitation, na.rm = T)) # precip
    names(precip_for)<-c("obs","priPrecipBulk")
    WS_for <- ddply(df, "X", summarise, mean = mean(wind_speed, na.rm = T)) # WS
    names(WS_for)<-c("obs","windSpeedMean")
    
    ### MERGE AVERAGES
    forecast.df <- merge(data.frame(temp_for, row.names=NULL),
                         data.frame(RH_for, row.names=NULL), 
                         by = "obs", all = F) # temp + RH
    forecast.df <- merge(data.frame(forecast.df, row.names=NULL), 
                         data.frame(LW_for, row.names=NULL), 
                         by = "obs", all = F) # + LW
    forecast.df <- merge(data.frame(forecast.df, row.names=NULL), 
                         data.frame(SW_for, row.names=NULL), 
                         by = "obs", all = F) # + SW
    forecast.df <- merge(data.frame(forecast.df, row.names=NULL), 
                         data.frame(precip_for, row.names=NULL), 
                         by = "obs", all = F) # + precip
    forecast.df <- merge(data.frame(forecast.df, row.names=NULL), 
                         data.frame(WS_for, row.names=NULL), 
                         by = "obs", all = F) # + WS
    
    ### GET DOY & HOUR
    # 2021-04-25T00 -- 2021-05-30T00
    group.vector <- rep(c(1:36),24) # 36 groups w/ 24 obs in each
    group.vector <- sort(group.vector) # sort order
    group.vector <- group.vector[-c((length(group.vector)-22):length(group.vector))] # cut length of day 36
    forecast.df <- cbind(forecast.df, group.vector) # bind to forecast df
    colnames(forecast.df)[8]  <- "DOY"
    forecast.df$DOY<-as.numeric(forecast.df$DOY) + 114 # 4/25/21 = DOY 115 => (+) 114 to days
    forecast.df$Hour[1:nrow(forecast.df)] <- rep(seq(0,23,by=1)) # add HOD 
    # ^ gives a warning, but works so just ignore it!
    
    ### 1 HR => 30 MIN
    OSBS_forecast <- do.call("rbind", replicate(2, forecast.df, simplify = FALSE)) # duplicate rows x2
    OSBS_forecast <- OSBS_forecast[order(OSBS_forecast$obs),] # reorder by "obs"
    OSBS_forecast$Hour[1:nrow(OSBS_forecast)] <- rep(seq(0,23.5,by=0.5)) # add HOD 
    # ^ gives warning, but ignore it!
    
    ### CALCULATE DAILY PRECIP 
    daily_precip <- aggregate(OSBS_forecast$priPrecipBulk, 
                              by=list(Category=OSBS_forecast$DOY), FUN=sum) # sum daily precip
    colnames(daily_precip)  <- c("DOY", "daily_precip")
    OSBS_forecast <- merge(data.frame(OSBS_forecast, row.names=NULL), 
                           data.frame(daily_precip, row.names=NULL), 
                           by = "DOY", all = TRUE) # join daily precip 
    write.csv(OSBS_forecast,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS.forecast.csv",row.names=FALSE)
    
    ### JOIN LONG-TERM AVG. DOY NDVI, LAI & ALBEDO
    setwd("~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS")
    OSBS_forecast <- read.csv("OSBS.forecast.csv", header= T) # met forecast
    albedo_OSBS <- read.csv("OSBS.albedo.mean.csv", header= T) # albedo
    LAI_OSBS <- read.csv("OSBS.LAI.DOY.csv") # LAI 
    colnames(LAI_OSBS)[2]<- "mean_LAI"
    NDVI_OSBS <- read.csv("OSBS.NDVI.DOY.csv", header= T) # NDVI
    colnames(NDVI_OSBS)[2]<- "mean_NDVI"
    # MERGE
    OSBS_forecast <- merge(data.frame(OSBS_forecast, row.names=NULL),
                           data.frame(albedo_OSBS, row.names=NULL), 
                           by = "DOY", all = F) # forecast + albedo
    OSBS_forecast <- merge(data.frame(OSBS_forecast, row.names=NULL), 
                           data.frame(LAI_OSBS, row.names=NULL), 
                           by = "DOY", all = F) # + LAI
    OSBS_forecast <- merge(data.frame(OSBS_forecast, row.names=NULL), 
                           data.frame(NDVI_OSBS, row.names=NULL), 
                           by = "DOY", all = F) # + NDVI
    
    ### CALCULATE VPD
    library(REddyProc)
    OSBS_forecast$VPD <- fCalcVPDfromRHandTair(rH = OSBS_forecast$RHMean, Tair = OSBS_forecast$tempRHMean)
    # ^ gives warning, but ignore it!
    
    write.csv(OSBS_forecast,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS.forecast.csv",row.names=FALSE)
  }
  
  ### RANDOM FOREST PREDICTIONS
  {
    OSBS_forecast <- read.csv("OSBS.forecast.csv", header= T) # ALL predictors for forecast
    OSBS_train <- read.csv("OSBS_partitioned.csv")
    
    library("randomForest")
    library ("ggplot2")
    library("ggRandomForests")
    library("gridExtra")
    library(regclass)
    set.seed(123)
    
    ### VSWC
    # TRAIN MODEL
    RF_VSWC <- randomForest(vswc ~ DOY + inLWMean + daily_precip + 
                              RHMean + tempRHMean + mean_LAI + mean_NDVI + 
                              Band4_BSA + Band4_WSA,
                            data=OSBS_train, importance=TRUE, 
                            ntree = 1000, na.action=na.omit)
    # PREDICT
    OSBS_predict_VSWC1 <- predict(RF_VSWC, newdata = OSBS_forecast,
                                  na.action = na.omit,importance = TRUE) # PREDICT ("actual" prediction)
    OSBS_predict_VSWC2 <- predict(RF_VSWC, newdata = OSBS_forecast,
                                  predict.all=TRUE,na.action = na.omit,importance = TRUE) # PREDICT (for calculating SD)
    OSBS_predict_VSWC_SD <- t(apply(OSBS_predict_VSWC2$individual, 1, function(x){ 
      c(sd(x), quantile(x, na.rm=T, c(0.025,0.975)) )})) # SD & CI's
    colnames(OSBS_predict_VSWC_SD)<- c("vswc_sd", "vswc_Conf_interv_02.5", "vswc_Conf_interv_97.5") # rename col's
    OSBS_target_forecast <- cbind(OSBS_forecast, OSBS_predict_VSWC1, OSBS_predict_VSWC_SD) # bind
    colnames(OSBS_target_forecast)[22]<- c("vswc") # rename prediction col
    
    
    ### NEE
    # TRAIN MODEL
    RF_NEE <- randomForest(nee ~ Hour + DOY + inSWMean + daily_precip + 
                             RHMean + tempRHMean + VPD + mean_LAI + mean_NDVI ,
                           data=OSBS_train, importance=TRUE, 
                           ntree = 1000, na.action=na.omit)
    # PREDICT
    OSBS_predict_NEE1 <- predict(RF_NEE, newdata = OSBS_forecast,
                                 na.action = na.omit,importance = TRUE) # PREDICT ("actual" prediction)
    OSBS_predict_NEE2 <- predict(RF_NEE, newdata = OSBS_forecast,
                                 predict.all=TRUE,na.action = na.omit,importance = TRUE) # PREDICT (for calculating SD)
    OSBS_predict_NEE_SD <- t(apply(OSBS_predict_NEE2$individual, 1, function(x){ 
      c(sd(x), quantile(x, na.rm=T, c(0.025,0.975)) )})) # SD & CI's
    colnames(OSBS_predict_NEE_SD)<- c("nee_sd", "nee_Conf_interv_02.5", "nee_Conf_interv_97.5") # rename col's
    OSBS_target_forecast <- cbind(OSBS_target_forecast, OSBS_predict_NEE1, OSBS_predict_NEE_SD) # bind
    colnames(OSBS_target_forecast)[26]<- c("nee") # rename prediction col
    
    
    ### LE
    # TRAIN MODEL
    RF_LE <- randomForest(le ~ Hour + DOY + inSWMean + inLWMean + priPrecipBulk +
                            RHMean + tempRHMean + VPD + mean_LAI + mean_NDVI + Band2_WSA,
                          data=OSBS_train, importance=TRUE, 
                          ntree = 1000, na.action=na.omit)
    # PREDICT
    OSBS_predict_LE1 <- predict(RF_LE, newdata = OSBS_forecast,
                                na.action = na.omit,importance = TRUE) # PREDICT ("actual" prediction)
    OSBS_predict_LE2 <- predict(RF_NEE, newdata = OSBS_forecast,
                                predict.all=TRUE,na.action = na.omit,importance = TRUE) # PREDICT (for calculating SD)
    OSBS_predict_LE_SD <- t(apply(OSBS_predict_LE2$individual, 1, function(x){ 
      c(sd(x), quantile(x, na.rm=T, c(0.025,0.975)) )})) # SD & CI's
    colnames(OSBS_predict_LE_SD)<- c("le_sd", "le_Conf_interv_02.5", "le_Conf_interv_97.5") # rename col's
    OSBS_target_forecast <- cbind(OSBS_target_forecast, OSBS_predict_LE1, OSBS_predict_LE_SD) # bind
    colnames(OSBS_target_forecast)[30]<- c("le") # rename prediction col
    
    write.csv(OSBS_target_forecast,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/OSBS/OSBS_targets_forecast.csv",row.names=FALSE)
  }
  
}


### REFORMAT FORECAST
{ 
  OSBS_forecast <- read.csv("OSBS_targets_forecast.csv") 
  OSBS_forecast <- OSBS_forecast[,-c(2:8,10:21,24:25,28:29,32:33)] # subset data
  OSBS_forecast$Date <- as.Date(OSBS_forecast$DOY, origin = "2020-12-31") # get day
  OSBS_forecast$time <- strftime(as.POSIXlt(OSBS_forecast$Hour * 60 * 60, origin = "1970-01-01", tz = "UTC"), format = "%H:%M") # get timestamp from hour
  OSBS_forecast$time <- paste(OSBS_forecast$Date,OSBS_forecast$time,sep=" ")
  
  ## VSWC
  OSBS_VSWC_mean <- OSBS_forecast[,c(3,10)] #  mean
  OSBS_VSWC_mean$statistic <- "mean"
  OSBS_VSWC_sd <- OSBS_forecast[,c(4,10)] # SD
  colnames(OSBS_VSWC_sd)[1]<- "vswc"
  OSBS_VSWC_sd$statistic <- "sd"
  ## NEE
  OSBS_NEE_mean <- OSBS_forecast[,c(5,10)] # mean
  OSBS_NEE_mean$statistic <- "mean"
  OSBS_NEE_sd <- OSBS_forecast[,c(6,10)] # SD
  colnames(OSBS_NEE_sd)[1]<- "nee"
  OSBS_NEE_sd$statistic <- "sd"
  ## LE
  OSBS_LE_mean <- OSBS_forecast[,c(7,10)] # mean
  OSBS_LE_mean$statistic <- "mean"
  OSBS_LE_sd <- OSBS_forecast[,c(8,10)] # SD
  colnames(OSBS_LE_sd)[1]<- "le"
  OSBS_LE_sd$statistic <- "sd"
  
  OSBS_forecast1 <- merge(data.frame(OSBS_VSWC_mean, row.names=NULL),
                          data.frame(OSBS_VSWC_sd, row.names=NULL), 
                          by=c("time", "statistic", "vswc"), all = T) 
  OSBS_forecast2 <- merge(data.frame(OSBS_NEE_mean, row.names=NULL),
                          data.frame(OSBS_NEE_sd, row.names=NULL), 
                          by=c("time","statistic", "nee"), all = T) 
  OSBS_forecast3 <- merge(data.frame(OSBS_LE_mean, row.names=NULL),
                          data.frame(OSBS_LE_sd, row.names=NULL), 
                          by=c("time","statistic","le"), all = T) 
  OSBS_forecast4 <- merge(data.frame(OSBS_forecast1, row.names=NULL),
                          data.frame(OSBS_forecast2, row.names=NULL), 
                          by=c("time","statistic"), all = T) 
  OSBS_forecast4 <- merge(data.frame(OSBS_forecast4, row.names=NULL),
                          data.frame(OSBS_forecast3, row.names=NULL), 
                          by=c("time","statistic"), all = T) 
  
  OSBS_forecast4$siteID <- "OSBS"
  OSBS_forecast4$forecast <- "1"
  OSBS_forecast4$data_assimilation <- "0"
  library(tibble)
  OSBS_forecast4 <- OSBS_forecast4[, c(1,6,2,7:8,4,5,3)]
  write.csv(OSBS_forecast4,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/FORECASTS/OSBS_cleaned.csv",row.names=FALSE)
}


#### JOIN FORECASTS
{
  BART_forecast <- read.csv("BART_cleaned.csv") 
  KONZ_forecast <- read.csv("KONZ_cleaned.csv") 
  OSBS_forecast <- read.csv("OSBS_cleaned.csv") 
  SRER_forecast <- read.csv("SRER_cleaned.csv") 
  
  final_forecast <- merge(data.frame(BART_forecast, row.names=NULL),
                          data.frame(KONZ_forecast, row.names=NULL), 
                          by=c("time", "siteID", "statistic", "forecast", 
                               "data_assimilation", "nee","le","vswc"), all = T) 
  final_forecast <- merge(data.frame(final_forecast, row.names=NULL),
                          data.frame(OSBS_forecast, row.names=NULL), 
                          by=c("time", "siteID", "statistic", "forecast", 
                               "data_assimilation", "nee","le","vswc"), all = T) 
  final_forecast <- merge(data.frame(final_forecast, row.names=NULL),
                          data.frame(SRER_forecast, row.names=NULL), 
                          by=c("time", "siteID", "statistic", "forecast", 
                               "data_assimilation", "nee","le","vswc"), all = T) 
  
  write.csv(final_forecast,file = "~/Documents/2020-(IU)/SPEA-E579 (Envi Sci Readings)/FORECASTS/final_forecast.csv",row.names=FALSE)
}
