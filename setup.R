#load packages
packages <- c("shiny","ggplot2", "reshape2","stringr", "geosphere",
              "plyr","ggmap","leaflet", "magrittr", "tools", "scales")

lapply(packages, library, character.only = T)

#load data
fnames <- list.files("data", "*.csv")
fnames <- paste("data/",fnames, sep = "")
mydfs <- lapply(fnames, read.csv, header = T, strip.white = T)

mydfs[[1]]$player <- as.character(mydfs[[1]]$player)
mydfs[[5]]$player <- as.character(mydfs[[5]]$player)

mydfs[[3]]$round <- 1
mydfs[[4]]$round <- 2

fa <- rbind(mydfs[[3]], mydfs[[4]])
fa$type <- "fs"

oq <- rbind(mydfs[[1]],mydfs[[5]])

o8 <- rbind(mydfs[[2]], mydfs[[7]])

stps <- c("perfects","greats","goods","bads","misses")

#reshape and transform data

long_vrs <- c("player",stps)
long_dat <- list(oq_long = oq, m2_long = mydfs[[6]], o8_long =o8, fs_long = fa)

long_dat <- lapply(long_dat, function(d) melt(d[,c(long_vrs, "titles")]))
long_dat <- lapply(long_dat, negate_misses)

mxcmb_dat <- list(q_mc = oq[order(oq$rank),c("player","max.combo")], 
                  m2_mc = mydfs[[6]][order(mydfs[[6]]$rank),c("player","max.combo")])

stps_e <- c("score", "grade", "steps", stps, "max.combo","velocity", 
            "noteskin", "region")

fs_core <- c("steps", stps, "max.combo", "region")

core_cols <- c("player","rank","grade","score","perfects","greats","goods",
               "bads","misses","max.combo","velocity","noteskin", "city",
               "state","region", "lon","lat", "steps", "matchup")

core_cols2 <- c("player","grade","score","perfects_ratio","greats_ratio",
                "goods_ratio","bads_ratio","misses_ratio","max_combo_ratio",
                "city","state","region","lon","lat","noteskin","velocity","fd",
                "locations", "max.combo")

round_names <- c("Quarterfinals", "Semifinals", "3rd-Place Match", "Finals")

sel_plyrs <- "Select Player(s)"
up_plyrs <- "Update Player(s)"

comps <- c("Male Speed","Female Speed","Freestyle")

ranges <- c("prange","grange","gdrange","brange","mrange","mcrange")

oa <- rbind(oq[,core_cols2],o8[,core_cols2])

oa$type <-  "sp"

oa <- rbind(oa, fa[,c(core_cols2, "type")])

ratios <- c(paste(stps,"_ratio",sep = ""),"max_combo_ratio", "velocity")

ratios_shrt <- c("prat","grat","gdrat","brat","mrat","mcrat")

#given the name of a numeric variable, this function finds the min, mean, and max
#for each city, returned in a list
get_city_avgs <- function(num.v){
  
  #store values
  values <- oa[,num.v]
  
  avgz <- tapply(values, oa$locations, mean, na.rm=T)
  
  avgz <- round(avgz, 3)
   
  avgz <- avgz[sort(names(avgz))]
  
  return(avgz)
}

avgz_xcity <- lapply(ratios, get_city_avgs)
names(avgz_xcity) <- ratios


#map stuff
#us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
#map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
#pmap <- ggmap(map, extent = "device")

venue <- c(-121.8138,37.32444)


venue_txt <- paste(sep = "<br/>", "<b>Round 1, San Jose</b>", "<b>Eastridge Mall</b>",
                   "Competition Venue")

pmap <- leaflet::leaflet()
pmap <- leaflet::addTiles(pmap)
pmap <- leaflet::fitBounds(pmap, -125, 25.75, -75, 49)

pmap2 <- pmap

title_cols <- c("color:green;", "color:red;")

#for qualifier plots
#store colors and labels 
jdg_col <- c("#19BFFD", "#2AFE39","#FCE67E", "#DC26E9","red" )
jdg_lab <- c(" Perfects ", " Greats ", " Goods ", " Bads ", " Misses ")