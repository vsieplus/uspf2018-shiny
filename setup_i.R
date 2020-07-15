library(ggmap)

#function to transform a vector with the charts nicely for users to see
nice_chrts <- function(df){
  chrts <- (as.character(df$song_chart))
  
  title <- substr(chrts, 1, nchar(chrts) - 4)
  
  title <- gsub("_", " ", title)
  
  title <- str_to_title(title)
  
  difficulty <- toupper(str_extract(chrts, "[a-z]+[0-9]+"))
  
  titles <-  paste(title, ", ", difficulty, sep = "")
  
  return(cbind(df,titles))
}

mydfs <- lapply(mydfs, nice_chrts)

#combine dfs
oq <- rbind(mydfs[[5]],mydfs[[1]])

o8 <- rbind(mydfs[[2]], mydfs[[7]])

#grab map data (lon/lat) for each df

add_map <- function(df){
  
  locations <- paste(df$city, df$state)
  
  coords <- geocode(locations)
  
  return(cbind(df,coords))
  
}

mydfs <- lapply(mydfs, add_map)


for(i in 1:length(fnames)){
  write.csv(mydfs[[i]], file = fnames[i], row.names = F)
}
