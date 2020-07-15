#all the functions we use in the app

#function to round to nearest 50
round50 <- function(x){
  fifties <- round(x/50)
  
  return(fifties * 50)
  
}

slyder <- function(n1, n2){
  sliderInput(inputId = n1, label = n2,
              min = 0, max = 1, value = c(0,1), step= .01)
}

rat_txt <- function(jdg, val){
  paste(sep = "","<b>", jdg, " Ratio: </b>", val, "<br/>")
}

update_rats <- function(rat, cty){
  return(avgz_xcity[[rat]][cty])
}

mini_plot <- function(dat, chartsteps){
  #fetch relevant max combo
  if(!(any(fa$player %in% dat$player))){
    mxcmbo <- o8$max.combo[o8$player %in% dat$player &
                           o8$titles %in% dat$titles]
  } else {
    mxcmbo <- fa$max.combo[fa$player %in% dat$player &
                           fa$titles %in% dat$titles]
  }
  
  mxcmbo <- rep(mxcmbo, 5)
  
  mxx <- round50(chartsteps) + 50
  mnn <- round50(dat$value[dat$variable == "misses"]) - 50
  
  brkz <- seq(mnn, mxx, 50)
  
  g_s <- ggplot(dat, aes(x = player,  y = value, fill = variable)) +
    geom_bar(stat = "identity", width = 0.8, color = "black") + 
    geom_hline(yintercept = 0) + 
    geom_errorbar(aes(ymin = mxcmbo, ymax = mxcmbo, color = "Max Combo"),
                  width = 0.8)+
    scale_y_continuous(expand = c(0,0), limits = c(mnn, mxx),
                       breaks = brkz) +
    scale_fill_manual(values = jdg_col, labels = jdg_lab) + 
    scale_color_manual(values = ("Max Combo" = "black")) +
    theme_bw() + 
    theme(text=element_text(size = 10, family = "Trebuchet MS"),
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          legend.position = "right",
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(title = "Step Breakdown")
  
  return(g_s)
  
}

#transpose a data frame for disply in shiny
t_df <- function(df){
  td <- t(df)
  colnames(td) <- NULL
  
  return(data.frame(stat = rownames(td), value = td))
}

#negate misses in long df
negate_misses <- function(df){
  misses <- df$variable == "misses"
  df$value[misses] <- -1 * df$value[misses]
  
  return(df)
}

normalize <- function(x){
  return((x - min(x,na.rm=T))/((max(x,na.rm=T)-min(x,na.rm=T))))
}
