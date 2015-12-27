# STATS290 Final Project - Personal Weather Station. William S. Pittard
#
# This package attempts to implement items 1 through 4 described in the Weather
# project PDF file in the project folder on the STATS290 website. I've implemented
# most of the functions/Classes/methods. 
#
# Use the getWeatherData function (described at the bottom of this file) to get
# the ball rolling and create a Weather Object:
#
# >library(PWS)
# >myWeather = getWeatherData("PaloAlto,CA",15)
# 
# >summary(myWeather)
# >plot(myWeather)
# >getLocsByRadius(myWeather,"KCALOSAL3",5)  # Get all stations within 5 miles 
# >dateVec=c("1/13/11","2/21/11")
# >getHist(myWeather,dateVec)	 # Fetch history for all stations in a Weather Object
# >getWobjData(myWeather,c("ID,"ELEV"))  # Access station id and elevation info from Weather object

##### BEGIN FUNCTION, CLASS, AND METHOD DEFINITIONS
#
# TITLE: deg.dist(long1, lat1, long2, lat2)
# INPUT: An originating point and a distal point both specified 
#        in lat/lon degrees
# OUTPUT: A distance in miles between the two points
#
deg.dist <- function (long1, lat1, long2, lat2) 
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 40041.47/(2 * pi)
    d <- R * c
    return(d)
} # END deg.dist

# TITLE: getLatLon(locale, dist, locLimit))
# INPUT: Location in character format e.g. "Atlanta,GA" or a vector 
#        with such information (e.g. c("Palo Alto,CA")
#
# OUTPUT: A data frame with locations within the specified radius
#
# This function takes a city/state/country 
# string (or vector of such strings) and a distance in miles. It then 
# uses this information to query Google's geocoding service, which is pretty
# good about returning sane information. 
#
# It is much better than the free GeoNames online service, which is pretty 
# saturated and limited in what it returns.This is why I chose to implement 
# a local copy of the GeoNames service in a local MySQL database on miller1 
# for use with downstream functions
#
# >getLatLon("Paris,FR",10,locLimit=41)  gets locations within 10 miles of Paris, France - limit 41 locations
# >myLocs = c("Atlanta,GA","Nashville,TN")
# >getLatLon(mylocs,10)  # Gets locations within 10 miles of both Atlanta and Nashville.
 
getLatLon <- function(locale="PaloAlto,CA",dist=10,locLimit=50) {
  library(RCurl)
  library(XML)
  tresta = data.frame
  restat = list()
  latList = list()
  apiURL = "http://maps.googleapis.com/maps/api/geocode/xml?address="

  pStr=paste("Finding lat/lon info for the specified locale(s):",locale,sep=" ")
  print(pStr)

  locale = sub('\\s+', '', locale, perl = TRUE) # trim white space e.g. "Palo Alto" -> "PaloAlto"
  if (length(locale) != 0)  {                   
    for (jj in 1:length(locale) ) {
      apiStr = paste(apiURL,locale[jj],"&sensor=false",sep="")
      txt = getURL(apiStr)
      rc = grep("ZERO_RESULTS",txt)  # Google API returns this if nothing found
      if (length(rc) != 0) { 
          stop("Couldn't find specified location")
      } 
#
# Process results and get latitude / longitude
#
      hold = xmlTreeParse(txt, useInternalNodes=TRUE)
      place = getNodeSet(hold,"//GeocodeResponse/result[1]/geometry/location[1]/*")
      restat[[jj]] = as.numeric(sapply(place,xmlValue))
      myLat = restat[[jj]][1]; myLon = restat[[jj]][2]
      latList[jj]=paste(myLat,myLon,sep=" ")      
      polo = getPlaces(myLat,myLon,dist,locLimit)
       if (length(tresta) == 1) {
          tresta = polo
       }
       else {
          tresta = rbind(tresta,polo)
       }
     }
   }
  pStr=paste("Checking for locations within",dist,"miles (approximate) of",locale,sep=" ")
  print(pStr)
  myLat = as.numeric(unlist(strsplit(latList[[1]]," ")))[1]
  myLon = as.numeric(unlist(strsplit(latList[[1]]," ")))[2]
  return(list(origCity=locale,iradius=dist,mlat=myLat,mlon=myLon,locs=tresta))  # Return a lat/lon for each locale specifed in the locale vector
#  return(list(origCity=locale,iradius=dist,myCoords=latList,locs=tresta))  # Return a lat/lon for each locale specifed in the locale vector
}  # END getLatLon

# TITLE: getPlaces(lat, lon, distance,locLimit)
# INPUT: A latitude and longitude, a radius/distance, and a limit on how many locations to return
# OUTPUT: A dataframe of locations falling with the given radius and the associated lat/lon info
#
# NOTE: In the typical use case this function is not meant to be called directly by the user. 
# It is usually called only by getLatLon()
#
# This function accepts a latitude, longitude, distance (in miles), and a limit on how many
# stations to return. It returns a data frame containing locations of significance as returned by
# a local MySQL copy of the main Geonames service.  

getPlaces <- function(lat, lon, distance, locLimit) {
  library(RCurl)
  library(RMySQL)
  library(XML)

  print("Querying the local Geonames database to see whats nearby")
  con = dbConnect(MySQL(), user="wpittard", password="t41tt",dbname="wpittard")
#
# This query uses some trig functions and a standard formuala to determine distance from the 
# given lat/lon and the lat/lon present in the GeoNames table. It focuses only on neighborhoods and 
# town names and excludes things like restaurants,malls, hotels. I tried using the online GeoNames service
# but it 1) restricts the specified radius and 2) blocks you after a certain number of queries.
#
  str1 = "SELECT geo_latitude, geo_longitude, geo_name, "
  str2 = paste("(3959*acos(cos(radians(",lat,")) * cos(radians(geo_latitude)) * cos(radians(geo_longitude) - radians(",lon,"))",sep="") 
  str3 = paste("+ sin(radians(",lat,")) * sin( radians( geo_latitude)))) AS distance FROM allcountries where ((geo_feature_code like '%ADM%')",sep="")
  str4 = paste("or (geo_feature_code like '%PPL%') )  having distance < ",distance,";",sep="")
  str5 = paste(str1,str2,str3,str4)
#  print(str5)
  rs = dbSendQuery(con, statement=str5)
  data = fetch(rs, n=-1)
  dbDisconnect(con)
# 
# Did we get anything meaningful back from the query ? Probably too much. Let's sample some 
# number of the locations to get a mix since we don't want to wear out our welcome at the 
# Wunderground server with tons of queries.  
#
  if (nrow(data) >= locLimit) {
     newdata=data[sample(nrow(data),size = locLimit), ]
  }
  else {
     newdata=data
  }
  return(newdata)
} # END getPlaces

# TITLE: getStations(lldf, radius, numofStatLimit)
# INPUT: A dataframe ,as returned by getLatLon, which contains lat/lon locations 
#        within the given radius. The number of stations returned can be limited 
#	 to "limit". This exists since for a given radius some areas are far more 
#        dense with PWS than others - "PaloAlto,CA"  vs. "Billings,MT"  
#
# OUTPUT: A list of Personal Weather Station ids for which we wish to retrieve weather information
#
# This function accepts a data frame, extracts the lat/lon information and
# builds a query string suitable for use with the Wunderground weather service
# to return a list of Personal Weather Station Identifiers.
#
# WARNING: This function could take a while to run since it is dependent on 
# how many queries are given to Wunderground as well as the response time of 
# that site - They did not publish access limits on their API Wiki but I'm 
# sure they have them.

getStations <- function(lldf, radius=10, numofStatLimit=150) {
  library(XML)
  library(RCurl)
  library(svMisc)
  
  dflength = length(lldf)
  if (dflength == 0) {
     stop("Hey. You passed an empty data frame. Unable to generate a list of stations !")
  }
  rlist = list()
  wunderString = "http://api.wunderground.com/auto/wui/geo/GeoLookupXML/index.xml?query="
  nrowdf = nrow(lldf)
  pStr=paste("Now checking Wunderground to find stations")
  print(pStr)
  for (ii in 1:nrowdf) {
    progress(ii,nrowdf-1)	# Print a progress bar so users won't get bored
    lat = lldf[ii,c("geo_latitude")]
    lon = lldf[ii,c("geo_longitude")]
    coords=paste(lat,lon,sep=",")
    urlstr = paste(wunderString,coords,sep="")
    if (ii == (nrowdf-1)) cat("Done!\n")
#    print(urlstr)
# 
# Now lets actually do the query @ Wunderground.  Let's hope they 
# don't get mad at rapid programmatic access. For geographically 
# "tight" locations ,(e.g. small radius) there will be overlap 
# between station names which we have to filter out.
#
    if (radius < 10) {
        halfradius = 3 
    }
    else {
        halfradius = radius/6
    }
    qstring = paste("//location/nearby_weather_stations/pws/station[distance_mi<",halfradius,"]/ancestor-or-self::station/id",sep="")
    txt2parse = getURL(urlstr)
    hold = xmlTreeParse(txt2parse, useInternalNodes=TRUE)  
    stationsXml = getNodeSet(hold,qstring)
    stations=sapply(stationsXml,xmlValue)
    rlist[[ii]] = stations 
  }
#
# Let's filter out repeats. This can happen if the radius is small 
# (< 10) and Wunderground returns the same station name for different 
# lat/lon pairs 
#
  finalStationList = unique(unlist(rlist))
  if ( length(finalStationList) == 0 ) {
      stop("No stations found")
  } 
  else {
      if (length(finalStationList) > numofStatLimit) {
         length(finalStationList) = numofStatLimit   # Check to see if number of stations exceeds the limit 
      }
      pStr=paste("Found",length(finalStationList),"Personal Weather Stations within",radius,"miles",sep=" ")
      print(pStr)
      return(finalStationList)
  }
}  # END getStations

# TITLE: getWeather(slist, lat, lon, origRadius,baseStation)
#
# INPUT: A list of personal weather station identifiers and the lat/lon of the reference 
#        city ,(the first city name specified to getLatLon). Also takes the radius specified
#	 in the original called to GetLatLon() so we can trim out any results for stations that
#	 might be over the specified radius. Note this won't be many - just a few outliers.
#
# OUTPUT: A data frame that has weather information for all the PWS identifiers. It also includes the 
#         distance that each station is from the original/reference city
#
# This function's job is to go to the Wunderground site with the station  list and 
# obtain weather statistics for each. We'll load this info into a dataframe, do some data checks,
# and then load up the S4 weather object to hold this data for subsequent interrogation.
#

getWeather <- function(slist, lat, lon, origRadius, origCity, baseStation) {
  library(XML)
  library(RCurl)
  library(Sxslt)
  library(svMisc)

  library(chron)

  # Cleaner: A local utility function to clean up info returned by Sxlst transformation
  cleaner <- function(x) {
   if (x == "") {
       x=NA
    }
    else if ( length(grep(" ft$",x)) > 0 ) {
      x = unlist(strsplit(x," "))[1]
    }
    else if ( length(grep("Last Updated",x)) > 0 ) {
      x = unlist(strsplit(x,"on"))[2]
    }
    else {
      x
   }
   x = sub('^\\s+', '', x, perl = TRUE)
  }   

  print("Now checking Wunderground for the station reports")
  lenslist = length(slist)
  if (lenslist == 0) {
     return("Hey. You passed me an empty station list. I'm unable to generate a list of weather reports")
  }
  rlist = list()
  wunderString = "http://api.wunderground.com/weatherstation/WXCurrentObXML.asp?ID="
  for (ii in 1:lenslist) {
    urlstr = paste(wunderString,slist[[ii]],sep="")
#    print(urlstr)
    progress(ii,lenslist)
    txt4parse = getURL(urlstr)
    hold = xmlTreeParse(txt4parse, useInternalNodes=TRUE)
#    doc = xsltApplyStyleSheet(hold, "KVPC.xsl")		# Apply XSLT transformation to parse PWS report
    xslFile = system.file("KVPC.xsl", package="PWS")
    
    doc = xsltApplyStyleSheet(hold, xslFile)		# Apply XSLT transformation to parse PWS report
    wvec = saveXML(doc)
    jake = unlist(strsplit(wvec,"\\|"))
    myLat = as.double(jake[1]); myLon = as.double(jake[2])  # Let's pull out orig lat/lon for a distance calc
    myDist = deg.dist(lon,lat,myLon,myLat)
    jake = jake[-1:-2]
#
# Do some houskeeping on the returned PWS report
#
    lop = lapply(jake, cleaner)
    if( length(lop) < 13) { lop[13]=NA }
    lop[14] = (myDist * 0.621371192)    # Convert distance from km to miles here and append to list
    rlist[[ii]] = lop 
#
    if (ii == lenslist) cat("Done!\n")
  }
#
# Make a data.frame. This will soon be a S4 object but let's go ahead and do some
# filtering of bad/missing values. 
#
  ndf = data.frame(t(sapply(rlist,c)))
  colnames(ndf)=c("ID","ELEV","TEMPF","DEW","HUMID","W_DIR","W_MPH","PRESS_IN","PRECIP","OBS_TIME","NEIGHBORHOOD","CITY","STATE","DIST")
  tndf = transform(ndf,
                   ELEV=as.numeric(ELEV),
                   TEMPF=as.numeric(TEMPF),
                   DEW=as.numeric(DEW),
                   HUMID=as.numeric(HUMID),
                   W_MPH=as.numeric(W_MPH),
                   PRESS_IN=as.numeric(PRESS_IN),
                   PRECIP=as.numeric(PRECIP),
                   OBS_TIME=as.chron(strptime(OBS_TIME,"%B %d, %I:%M %p")),
                   DIST=as.numeric(sprintf("%6.2f",DIST))
                  )
#
# Let's do some cleanup and rearranging on the data frame. This is a bit messy but it 
# will do for now. The goal is to resemble the typical output for a station on Wunderground
#
  tndf = tndf[with(tndf,order(DIST)),]	# Sort by distance from original/reference city
  tndf = tndf[,c(1,14,2:9,10:13)]	# Rearrange the columns
  tndf = tndf[,c(1:10,13)]		# Remove a column
  row.names(tndf) = NULL		# reset row names to be sequential (counteracts results of "sample" function
  for (kk in 1:nrow(tndf)) {		# Set "strange" values to NA
	for (ii in 2:6) {
            if (tndf[kk,ii] < 0) { tndf[kk,ii] = NA }
        }
        for (ii in 8:10) {
	     if (tndf[kk,ii] < 0) { tndf[kk,ii] = NA }
        } 
  }
  #
  # Setup a class called "myWeatherObj"
  #
  setClass("myWeatherObj",
          representation(origin = "vector",
                         radius = "numeric",
                         baseStat = "vector",
                         dataDf = "data.frame"),
			 contains=c("list","data.frame")
                         )
  #                         
  # Default Method to Show Object
  #
  setMethod("show","myWeatherObj",
                  function(object) {
		     pStr=paste(">> PWS  report for:",object@origin,"Specified radius:",object@radius,"miles",sep=" ")
		     print(pStr)
                     cat("\n")
                     options(width=120)
                     print(object@dataDf)
                     options(width=80)
                  })
  #
  # Validate the class - We've already done a lot of checking leading up to 
  # this but just to make sure we do some more.
  #
  setValidity("myWeatherObj", 
             function(object) {
                retval=TRUE
                if ( (object@origin == "") || !is.numeric(object@radius) ) {
                      print("Origin or radius not valid")
                      retval=FALSE
                }
                if ( length(names(object@dataDf)) != 11 ) {
                      print("Error creating Weather Object: Data frame is not valid")
                      retval=FALSE
                }
                return(TRUE)
             }
  )
  #
  # Setup a method to interrogate the weather object - given a weather object, station name, and
  # a radius (in miles) then this method will return all stations ,(also as a weather object), from 
  # the given station name within that radius
  #
  setGeneric("getLocsByRadius",function(object,stationName,radVal) standardGeneric("getLocsByRadius"))
  setMethod("getLocsByRadius","myWeatherObj", 
        function(object,stationName,radVal) {
          if (stationName == "") {
               print("Usage: getLocsByRadius(weatherObj,\"STATNAME\",4)")
               stop("Empty Station name string")
          }
          if (!is.numeric(radVal)) {
               print("Usage: getLocsByRadius(weatherObj,\"STATNAME\",4)")
               stop("Radius not specified")
          }
          mt=object@dataDf
          DIST=mt[mt$ID==stationName,2]   # Column 2 is the Distance
          myNewSet = mt[abs(mt$DIST-DIST) < radVal,]
          myNewWo = new("myWeatherObj", origin = origCity, radius = radVal, baseStat=stationName,dataDf = myNewSet)
          return(myNewWo)
        }
  )

  #
  # Let's make a summary method for the object.
  #
  setMethod("summary","myWeatherObj", 
        function(object) {
          mt            = object@dataDf
          origLocale    = object@origin
          myRadius      = object@radius
          numOfStations = length(mt$ID)
          myHeader = paste("There are",numOfStations,"Personal Weather Stations within",myRadius,"miles of",origLocale,sep=" ")
	  print(myHeader)
          mySum = summary(mt[,c(2:6,8:10)])
          cat("\n BASIC SUMMARY STATISTICS FOR WEATHER STATION ATTRIBUTES \n")
          print(mySum) }
  )

  #
  # Let's provide a way for users to access data in the object (other than the "@"/slot mechanism)
  # Users should specify dataVec as in this example c("ID","ELEV","PRECIP")
  # > dataVec=c("ID","ELEV","PRECIP")
  # > getWobjData(myWeatherObj,dataVec)
  #
   setGeneric("getWobjData",function(object,dataVec) standardGeneric("getWobjData")) 
   setMethod("getWobjData","myWeatherObj",
          function(object,dataVec) {
            if (length(dataVec) == 0) {
                 pStr='Usage: getWobjData(myWeatherObj,c("ID,"ELEV")) '
                 print(pStr)
            }
            mt=object@dataDf
            retInfo=subset(mt,select=c(dataVec))
            return(retInfo)
	  }
    )

   # TITLE: getHist
   # INPUT: Weather Object and a date vector (begin date / end date)
   # OUTPUT: A data frame for each day for each station in the time frame
   # This method takes a weather object and for each station name retrieves history
   # based on the datavec arguement that specifies a begin and end date.
   #
   # >dataVec=c("1/13/11","2/21/11")
   # >getHist(myWeatherObject, dataVec)
   
   setGeneric("getHist",function(object,dataVec) standardGeneric("getHist"))
   setMethod("getHist","myWeatherObj",
          function(object,dataVec) {
            library(XML)
            library(RCurl)
	    accumDf = data.frame()
            # Check the Dates
             if ( length(dataVec) == 0)  {
                stop("Usage: getHist(weatherObj,dateVec)")
             }
             if (as.Date(dataVec[[1]],"%m/%d/%y") > as.Date(dataVec[[2]],"%m/%d/%y")) {
		stop("You might have your dates switched around")
             }
            wStr1="http://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID="
            wStr2="&graphspan=custom&"
            charDate=paste(dataVec[1],dataVec[2],sep="-")
            start = as.character(strptime(dataVec[1], "%m/%d/%y"))
            start = unlist(strsplit(as.character(start),"\\-"))
            end   = as.character(strptime(dataVec[2], "%m/%d/%y"))
            end   = unlist(strsplit(as.character(end),"\\-")) 
            mt=object@dataDf
            orig=object@origin
            myVec = mt$ID
#            print(charDate)
            print("Hang on. This could take a while. This isn't part of the publicized Wunderground API and its a bit slow")
    
            for (jj in 1:length(myVec)) {
	       qStr=paste(wStr1,myVec[jj],wStr2,"month=",start[2],"&day=",start[3],"&year=",start[1],"&monthend=",end[2],"&dayend=",end[3],"&yearend=",end[1],"&format=1",sep="")
#               print(qStr)
                myCsv = getURL(qStr)
                temp = tempfile()
                con = file(temp,open="w")
                myCsv = gsub('<br>','',myCsv)
                cat(myCsv,file=con)
                close(con)
                myTab=read.csv(temp,sep=",")
		Station=c(rep(myVec[jj],nrow(myTab)))
 		myTab=cbind(Station=unlist(Station),myTab)  # Bind Station name to daily entries
                myTab = myTab[-max(nrow(myTab)),]	    # Get rid of junk at the botom of each report
                accumDf=rbind(accumDf,myTab)		    # Add onto the existing Data frame
#               print(ndf)
            }  
               options(width=120)
               cat("\n")
               print(charDate)
#               print(accumDf)
               colnames(accumDf)=c("Station","Date","TempHi","TempAvg","TempLow","DewPtHi","DewPtAvg","DewPtLow","HumidHi","HumidAvg","HumidLow","PressMax","PressMin","WindSpeedMaxMPH","WindSpeedAvgMPH","GustSpeedMaxMPH","PrecipSum")

  #
  # Let's make a "Weather History Class"
  #

		setClass("myWeatherHist",
          		 representation(timeframe = "vector",
                         baseStat = "vector",
                         dataDf = "data.frame"),
                         contains=c("list","data.frame")
                         )
  #
  # Set up a show method for the Weather History Class
  #
                setMethod("show","myWeatherHist",
                    function(object) {
                     pStr=paste(">> PWS  history report for date range",object@timeframe,sep=" ")
                     print(pStr)
                     cat("\n")
                     options(width=120)
                     print(object@dataDf)
                     options(width=80)
                  })
  #
  # Setup an accessor Method to get at a particular station history
  #
                setGeneric("getHistInfo",function(object,statName) standardGeneric("getHistInfo"))
		setMethod("getHistInfo","myWeatherHist",
          	   function(object,statName) {
            		if (length(statName) == 0) {
                        pStr='Usage: getHistInfo(myWeatherHistObj,"IQCMONTR8") '
                        print(pStr)
                   }
                   mt=object@dataDf
                   retInfo=subset(mt,Station == statName)
                   if (length(retInfo) == 0) {
                      stop("Nothing found for that station")
                   }
                   myHistPerStation = new("myWeatherHist",timeframe=object@timeframe,baseStat=statName,dataDf=retInfo)
#                   return(retInfo)
#                    print(retInfo)
                   return(myHistPerStation)
          }
    )

   #
   # Create a default summary method for a weather history object
   #
   setMethod("summary","myWeatherHist",
        function(object) {
          mt            = object@dataDf
          base          = object@baseStat
          numstats = length(unique(mt$Station))
          if (numstats > 1) {
              myHeader = paste("There are",numstats,"stations represented in this history object",sep=" ") 
              print(myHeader)
              cat("\n")
              print("You might want to use GetHistInfo(WeatherHistObj,\"station\") to drill down")
              hold=summary(mt)
              print(hold)
          }
          else {
              myHeader = paste("There is ",numstats,"station represented in this history object",sep=" ")
              print(myHeader)
              hold=summary(mt)
              cat("\n")
              print(hold)
          }
#          cat("\n BASIC SUMMARY STATISTICS FOR WEATHER STATION ATTRIBUTES \n")
#          print(mySum) 
         }
  )


   #
   # Need to Create a default plot method for a weather history object
   #
   setMethod("plot","myWeatherHist",
        function(x,y,...) {
 	  mt = x@dataDf
          base  = x@baseStat
          numstats = length(unique(mt$Station))
          if (numstats > 1) {
             myHeader = paste("There are",numstats,"stations represented in this history object",sep=" ") 
             print(myHeader)
             cat("\n")
             print("You might want to use getHistInfo to get info on a specific station and then retry the plot")
             stop("Exit")
          }   
          myDates=as.Date(mt$Date) 
          myTempHi = mt$TempHi
          myTempAvg = mt$TempAvg
          myTempLow = mt$TempLow
          myHumidHi = mt$HumidHi
          par(mfrow=c(4,1))
           
          plot(myTempHi ~ myDates,ylab="Degrees F",main="Hi Temp")
          plot(myTempAvg ~ myDates,ylab="Degrees F ",main="Avg Temp")
          plot(myTempLow ~ myDates,,ylab="Degrees F",main="Low Temp")
          plot(myHumidHi ~ myDates,ylab="Degrees F",main="Humid Hi")

        }
  )

                

   #
   # Create a weather history object.
   #
                myHistory = new("myWeatherHist", timeframe=charDate, baseStat=baseStation, dataDf = accumDf)
#               return(accumDf)
#                show(myHistory)
                options(width=80)
                return(myHistory)
       }
   ) 


  # TITLE: plot(myWeatherObj)
  #
  # Let's make a default plot method for the Weather object
  #
   setMethod("plot","myWeatherObj",
        function(x,y,...) {
          hold=x@dataDf
	  par(mfrow=c(2,4))
          boxplot(hold$DIST,ylab="Distance in Miles",main="Distance from locale",col=c("#F7F4F9"))
          boxplot(hold$ELEV,ylab="Height in feet",main="Elevation",col=c("#E7E1EF"))
          boxplot(hold$TEMPF,ylab="Degrees Fahrenheit",main="Temperature",col=c("#D4B9DA"))
          boxplot(hold$DEW,ylab="Degrees Fahrenheit",main="Dew Point",col=c("#C994C7"))
          boxplot(hold$HUMID,ylab="Percent Humidity",main="Humidity",col=c("#DF65B0"))
          boxplot(hold$W_MPH,ylab="Miles Per Hour",main="Wind Speed",col=c("#E7298A"))
          boxplot(hold$PRESS_IN,ylab="Inches Hg",main="Pressure",col=c("#CE1256"))
          boxplot(hold$PRECIP,ylab="Inches",main="Precipitation",col=c("#91003F"))

        }
  )


  #
  # Now lets create this awesome Weather Object
  #
  myWC = new("myWeatherObj", origin = origCity, radius = origRadius, baseStat=baseStation,dataDf = tndf)
#  show(myWC)
  
  return(myWC) 
}

# TITLE: getWeatherData(locale,distance=10,statLimit=150)
#
# INPUT: Location in character format e.g. "Atlanta,GA" or a vector 
#        with such information (e.g. c("Palo Alto,CA")
#
# OUTPUT: A data frame with locations within the specified radius

  getWeatherData = function(locale="Montreal,CA",distance=20,statLimit=150) {
  .onLoad = function(libname, pkgname) {
     .libPaths("~wpittard/Library")
     require("svMisc")
  }
     
       myLocs  = getLatLon(locale,distance)
       myStats = getStations(myLocs$locs, myLocs$iradius, statLimit)
       if (length(myStats) == 0) {
            stop("Sorry. No stations found within the specified radius")
       }
       baseStat=myStats[1] 
       myWeatherObj  = getWeather(myStats, myLocs$mlat, myLocs$mlon, myLocs$iradius, myLocs$origCity,baseStat)
  }
  
