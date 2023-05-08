# Download SNOTEL data  - Kehan Yang 2023-05-08


# output location
PATH_SNOTEL <- './inputs/snotel'
# snowtel network
PILLOW_NETWORK = 'cdec' #cdec (for CA) or snotel (for other locations)

# the extent of data
EXTENT_NORTH = 41.9935866548747896
EXTENT_EAST = -118.0141343118380632
EXTENT_SOUTH = 35.3781524800723091
EXTENT_WEST = -122.375

# set up the time period for data downloading
simdate <-'2023-04-03'
oldestDate = '2000-10-01'
simdate <- as.Date(simdate, '%Y-%m-%d')
oldestDate <- as.Date(oldestDate, '%Y-%m-%d')

# get station metadata ----
# filter stations by bounding box using latitude and longitude.
station_locations <- get_station_inventory(PILLOW_NETWORK) %>%
  filter(Latitude>=EXTENT_SOUTH, Latitude<=EXTENT_NORTH, Longitude<= EXTENT_EAST, Longitude>= EXTENT_WEST)

print('Get station data----')
station_data=get_stationswe_data(PATH_SNOTEL,station_locations,PILLOW_NETWORK, simdate, oldestDate)




#' setup and clean station inventory
#'
#' @param network which station network are you using. default 'snotel'. currently only option

#' @return tibble with (at least) column names Site_ID, site_name, Latitude, Longitude.
#' @export
#' @details There is a snotel inventory included with the package from Jan 2017. It can be updated from https://wcc.sc.egov.usda.gov/nwcc/inventory or theoretically replaced with that of a different network

get_station_inventory <- function(network){
  if(network=='cdec'){
    pillowlocs.df=read_csv(system.file("extdata","cdec_swe_inventory.csv",package='StationSWERegressionV2'))
  }
  if(network == 'local'){
    pillowlocs.df= read.csv('./inst/extdata/local_swe_inventory.csv')
  }
  if(network=='snotel'){
    nwccinv=read_csv(system.file("extdata","nwcc_snotel_inventory.csv",package='StationSWERegressionV2'))
    nwccinv <- nwccinv %>%
      rename(stationid=`station id`) %>%
      mutate(stationid=as.character(stationid))

    ## ---- read more accurate locations, fix station names and create spatial points dF
    snotellocs=read_csv(system.file("extdata","SNOTEL_MASTER.csv",package='StationSWERegressionV2')) %>%
      mutate(Site_ID=as.character(Site_ID))

    snotellocs=inner_join(snotellocs,nwccinv,by=c("Site_ID" = "stationid"))

    pillowlocs.df=snotellocs %>%
      dplyr::select(Station_ID,Site_ID,site_name,State,Latitude,Longitude,Elevation_m,start_date,end_date) %>%
      mutate(
        site_name=as.character(site_name),
        site_name=gsub('#','no.',site_name,fixed=T),#ifelse(grepl('#',site_name),gsub('#','no.',site_name,fixed=T),site_name),
        site_name=gsub('-',' ',site_name,fixed=T), #site_name=ifelse(grepl('-',site_name),gsub('-',' ',site_name,fixed=T),site_name),
        site_name=gsub('\'','',site_name,fixed=T),# site_name=ifelse(grepl('\'',site_name),gsub('\'','',site_name,fixed=T),site_name),
        Station_ID=gsub(' ','',Station_ID))
  }

  return(pillowlocs.df)
}


#' download and combine station data
#'
#' @param station_locs tbl_df of site metadata
#' @param network default snotel
#' @param PATH_SNOTEL the snotel file location
#' @param simdate the snotel file location
#' @param oldestDate the snotel file location
#' @export
#' @return tibble with station data from nrcs

get_stationswe_data <- function(PATH_SNOTEL,station_locs,network, simdate, oldestDate){
  # PATH_SNOTEL <- file.path('Run100_input','snoteldownloads')

  #define import functions for different networks
  if(network=='snotel'){
    station_locs <- station_locs %>%#some additional filtering possible with snotel network
      filter(start_date<simdate) %>%
      filter(end_date>simdate)

    import_data <- function(fn){
      tryCatch({
        read_csv(fn,
                 comment='#',
                 col_types = list(
                   col_date('%Y-%m-%d'),
                   col_number()
                 ),
                 skip=1,
                 col_names = c('dte','pillowswe')
        )}, error = function(e){
          data_frame()
        })
    }

  } else if(network=='cdec'){

    import_data <- function(fn){
      tryCatch({
        read_csv(fn,quote="\'",
                 na='m',
                 col_types = list(
                   # col_date(format='%Y%m%d'),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   # col_time(format='%H%M'),
                   col_number(),
                   col_character(),
                   col_character()
                 ),
                 col_names = c('Site_ID','DURATION','SENSOR_NUMBER','SENSOR_TYPE','dte','obsdate','pillowswe','datef','units')
        )}, error = function(e){
          data_frame()
        })
    }




  } else {

    stop('the network you specified for downloading pillow data is not supported.')

  }



  # # cycle through stations to download if necessary. will not download the data again if a file already exists with an equal or later date than the simulation date.
  PATH_SNOTEL_out <- paste0(PATH_SNOTEL,'_org')
  dir.create(PATH_SNOTEL_out, showWarnings = F, recursive = T)
  datestr <- fdate2str(as.Date(simdate))
  fns = list.files(path = PATH_SNOTEL_out, pattern = glob2rx(paste0('*',oldestDate,'*.csv')),full.names=T)
  fnnames = list.files(path = PATH_SNOTEL_out, pattern = glob2rx(paste0('*',oldestDate,'*.csv')),full.names=F)

  if(length(fns)>0){
    dat <- NA
    #orgnize the data
    for(i in 1:length(fns)){
      data <- read.csv(fns[i], stringsAsFactors = F)
      # data <- as.data.frame(data)
      dat <- rbind(dat,data)
    }


    print(' - done reading files')
    dat2 <- dat[complete.cases(dat),]
    dat2=full_join(station_locs,
                   dat2,by=c('Site_ID'))
    dat2<- dat2[complete.cases(dat2),]
    dat2$pillowswe <- as.numeric(dat2$pillowswe)
    pillowdata=dat2 %>%
      dplyr::select(Site_ID, Longitude, Latitude, dte, pillowswe) %>%
      mutate(
        pillowswe=replace(pillowswe,pillowswe<0,  NA),
        pillowswe=pillowswe*2.54/100)#convert inches to meters

  }else{
    iloc=1
    for(iloc in 1:nrow(station_locs)){# use loop with index isntead of value so you can access State for the same row later
      site_id <- station_locs$Site_ID[iloc]

      new_file=file.path(PATH_SNOTEL,paste0(network,'_',site_id,'_',simdate,'.csv'))
      old_files=dir(path=PATH_SNOTEL,glob2rx(paste0(network,'_',site_id,'*.csv')),full.names=T)
      if(!file.exists(new_file)){
        if(length(old_files)>0){
          existingDates <- as.Date(sapply(strsplit(x=basename(old_files),split='[._]',fixed=F),'[',3),'%Y%m%d')
          oldestDateind <- which.max(existingDates)
          oldestDate <- existingDates[oldestDateind]

        } else {
          oldestDate <- NA
        }
        # newDate <- as.Date(simdate,format='%Y%m%d')

        if(!isTRUE(oldestDate > simdate)){

          if(network=='snotel'){
            state_id <- station_locs$State[iloc]
            downloadURL <- paste0('https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/',site_id,':',state_id,':SNTL|id=%22%22|name/POR_BEGIN,POR_END/WTEQ::value')

          } else if(network=='cdec'){
            # downloadURL=paste0('http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=',site_id,'&dur_code=D&sensor_num=82&start_date=1/1/1900&end_date=',strftime(simdate,'%m/%d/%Y'))
            downloadURL=paste0('http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=',site_id,'&SensorNums=82&dur_code=D&Start=1984-01-01&End=',strftime(simdate,'%Y-%m-%d'))

          }

          file.remove(old_files)
          dstatus <- download.file(downloadURL,new_file,method="auto")

        }
      }

    }

    # finish downloading


    fns = list.files(path = PATH_SNOTEL, pattern = glob2rx(paste0(network,'*',simdate,'*.csv')),full.names=T)
    fnnames = list.files(path = PATH_SNOTEL, pattern = glob2rx(paste0(network,'*',simdate,'*.csv')),full.names=F)
    dat <- data.frame(Site_ID = NA,Date = NA,pillowswe = NA, dte = NA)

    #orgnize the data
    for(i in 1:length(fns)){
      data <- read.csv(fns[i], stringsAsFactors = F)
      data <- as.data.frame(data)
      wdata <- cbind.data.frame(Station_ID = data$STATION_ID,Date = data$DATE.TIME,Value = data$VALUE)

      wdata$dte <- substr(wdata[,2],1,8)
      names(wdata) <- c('Site_ID','Date','pillowswe','dte')
      outname <- paste0(PATH_SNOTEL_out, '/',fnnames[i])
      write.csv(wdata,outname, row.names = F)
      dat <- rbind.data.frame(dat,wdata)


    }


    print(' - done reading files')

    dat2 <- dat[complete.cases(dat),]
    dat2=full_join(station_locs,
                   dat2,by=c('Site_ID'))
    dat2<- dat2[complete.cases(dat2),]
    dat2$pillowswe <- as.numeric(dat2$pillowswe)
    pillowdata=dat2 %>%
      dplyr::select(Site_ID, Longitude, Latitude, dte, pillowswe) %>%
      mutate(
        pillowswe=replace(pillowswe,pillowswe<0,  NA),
        pillowswe=pillowswe*2.54/100)#convert inches to meters

  }



  return(pillowdata)

}



