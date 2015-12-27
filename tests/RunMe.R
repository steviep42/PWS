library(PWS)

#
# Let's get weather info on a city
#
myWobj = getWeatherData("Palo Alto,CA",5)
summary(myWobj)
# plot(wobj) 

# Get a summary
getLocsByRadius(myWobj,"KCALOSAL3",1)

# Let's get some particulars from the object
getWobjData(myWobj,c("ID","ELEV"))

# Let's get some historic data from the object
dateVec = c("1/13/11","2/21/11")
myHist = getHist(myWobj,dateVec)
summary(myHist)

# Let's get historic data for a specific station
myStationHist = getHistInfo(myHist,"KCAPALOA15")
#plot(myStationHist)

