if(!(exists("p1dat")))
  return()

#store player coordinates
lines <- data.frame(lon = c(p1dat()$lon, p2dat()$lon), 
                    lat = c(p1dat()$lat, p2dat()$lat))

proxy <- leaflet::leafletProxy("minimap", session)

proxy <- leaflet::clearShapes(proxy)
proxy <- leaflet::clearMarkers(proxy)

#redefine map boundary
if(!(rv$p1 == ""))
  proxy <- leaflet::fitBounds(proxy, min(lines$lon) - .5, min(lines$lat) - .5,
                     max(lines$lon) + .5, max(lines$lat) + .5)

#add line to map with popup showing distance in miles
dist_mi <- distHaversine(lines[1,], lines[2,]) * (0.000621371)

dist_mi <- round(dist_mi, 2)

dist_mi <- prettyNum(dist_mi, big.mark = ",", scientific = F)

proxy <- leaflet::addPolylines(proxy, lines[,1], lines[,2], 
                      popup = paste("<b>",dist_mi, " Miles</b>", sep = ""),
                      opacity = 0.75, color = "blue")

#add markers
colss <- sub(".*:","",rv$pcolors)
colss <- sub(";","",colss)

p1nam <- as.character(p1dat()$player)
p2nam <- as.character(p2dat()$player)

proxy <- leaflet::addCircleMarkers(proxy, lines[,1], lines[,2], color = colss,
                          opacity = 0.75, 
                          popup = paste(c(p1nam, p2nam)))