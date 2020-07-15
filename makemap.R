#create a map of competitors

#count frequencies, and append matching coordinates
freq <- as.data.frame(table(oa$locations[!(duplicated(oa$player))]))
freq$Freq <- freq$Freq[order(as.character(freq$Var1))]
freq$Var1 <- sort(as.character(freq$Var1))

real_freqs <- freq$Freq

lons <- unique(oa$lon[order(as.character(oa$locations))])
lats <- unique(oa$lat[order(as.character(oa$locations))])

freq$Freq <- normalize(freq$Freq) * 10

places <- cbind(freq,lons,lats)

places <- places[complete.cases(places),]

#pmap <- pmap + 
 # geom_point(aes(x = lons, y = lats), data = places, col = "red", alpha = 0.6,
  #           size = places$Freq*.8)+ 
  #scale_size_continuous(range = range(places$Freq))

locations_strong <- paste(sep = "", "<b>", freq$Var1, "</b>" )

cities_txt <- paste(locations_strong, "<br/>",
                "Players:", real_freqs)

pmap <- leaflet::addCircleMarkers(pmap, lng = places$lons, lat = places$lats, opacity = 0.8,
                   radius = places$Freq, color = "red", popup = cities_txt[-1],
                 layerId = freq$Var1[-1])

pmap <- leaflet::addCircleMarkers(pmap, lng = venue[1], lat = venue[2], opacity = 0.9,
                         radius = 12, color = "green", 
                         popup = venue_txt, layerId = "hi")
