#------------
# Functions: Distance between to GPS in meters                                                       
#------------



#This uses the ‘haversine’ formula to calculate the great-circle distance between two points 
# – that is, the shortest distance over the earth’s surface 
# - giving an ‘as-the-crow-flies’ distance between the points (ignoring any hills, of course!).
#
#Haversine formula:
#	a = sin²(Δφ/2) + cos(φ1).cos(φ2).sin²(Δλ/2)
#	c = 2.atan2(√a, √(1−a))
#	d = R.c
#
#where:
#	φ is latitude, λ is longitude, R is earth’s radius (mean radius = 6,371km)
#
#Note: that angles need to be in radians to pass to trig functions!

Distance.between.GPS.in.meters<-function(GPS.lat1,GPS.lat2,GPS.long1,GPS.long2){
	# Change to radians
	GPS.lat1 <-GPS.lat1*pi/180
	GPS.lat2 <-GPS.lat2*pi/180
	GPS.long1 <-GPS.long1*pi/180
	GPS.long2 <-GPS.long2*pi/180
	
	# Change in lat and long	
	delta.lat <-abs(GPS.lat1-GPS.lat2)
	delta.long<-abs(GPS.long1-GPS.long2)
	
	# Calculations 
	a<-(sin(delta.lat/2))^2+cos(GPS.lat1)*cos(GPS.lat2)*((sin(delta.long/2))^2)
	c<-2*atan2(sqrt(a),sqrt(1-a))
	d<-6371000*c # 6371000 is the radius of the earth in meters

	#print(a)
	#print(c)
	#print(d)
	
	d
}

xcovert<-function(lat,long){
  lat<-lat*pi/180; long<-long*pi/180; 
  xcart<- 6371000 * cos(lat) * cos(long)
  return(xcart)
}

ycovert<-function(lat,long){
  lat<-lat*pi/180; long<-long*pi/180; 
  ycart<- 6371000 * cos(lat) * sin(long)
  return(ycart)
}