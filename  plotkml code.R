
#tempdata<-Data
#coordinates(tempdata) <- ~UTM_X_Zone+UTM_Y_Zone
#proj4string(tempdata) <- CRS("+proj=utm +zone=48 +datum=WGS84") # possible 47 (T, north, 40(?))

PlotDataKML<-function(tempdata2, savefilename,COLS){
  coordinates(tempdata2) <- ~Longitude+Latitude
  proj4string(tempdata2) <- CRS("+proj=longlat +datum=WGS84") 
  shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
  kml(tempdata2, shape = shape, filename = paste(savefilename,".kml") , labels=" ", size=0.2)
}