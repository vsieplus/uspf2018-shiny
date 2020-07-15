#grab relevant df in long format
rd_dat <- long_dat[[rv$qrd]]

q_long <- rd_dat[rd_dat$player %in% q_dat()$player &
                 rd_dat$variable %in% input$steps,]

mxcmbs <- mxcmb_dat[[rv$qrd]]
mxcmbs <- mxcmbs[mxcmbs$player %in% q_dat()$player,"max.combo"]

mxcmbs <- rep(mxcmbs, 5)

psteps <- 0

if(!(all(input$steps == "misses"))){
  non_misses <- input$steps[!(input$steps %in% "misses")]
  
  #get columns for relevant steps (except misses)
  step_cols <- as.list(q_dat()[,non_misses])

  if(length(non_misses) == 1){
    psteps <- unlist(step_cols)
  } else {
    psteps <- Reduce("+", step_cols)
  }
}
  
mx <- max(psteps)
mn <- -1 * max(q_dat()$misses)

#setup breaks and colors for y axis
brks <- sort(c(0, 200, 400, 600, 800, mx))
clrs <- rep("black", 6)
clrs[which(brks == mx)] <- "blue"

#boolean vec to track which judgments the user wants to see
which_jdg <- tolower(trimws(jdg_lab)) %in% input$steps

gp <- ggplot(q_long, aes(x = player, y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_hline(yintercept = mx, color = "blue", lty = 5) +
  geom_hline(yintercept=0) +
  theme_bw()+
  theme(text=element_text(size = 13, family = "Trebuchet MS"),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 65, hjust = 1, color = "black"),
        axis.text.y = element_text(color = clrs),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5, size = 11)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(mn - 50, mx + 50),
                     breaks = brks) +
  scale_x_discrete(labels = paste(rev(q_dat()$player)," 
                                  (", rev(q_dat()$rank), ")", sep = ""),
                   limits = rev(q_dat()$player)) +
  labs(x = "", y = "Steps\nhit", title = "Step breakdown by player rank") +
  scale_fill_manual(values = jdg_col[which_jdg],
                    labels = jdg_lab[which_jdg]) +
  scale_color_manual(values = ("Max Combo"= "black"))

#only add max combo markers if all steps are selected
if(all(stps %in% input$steps)){
  gp <- gp +
    geom_errorbar(aes(ymin = mxcmbs,
                      ymax = mxcmbs, color = " Max Combo"),
                  width = 0.7, size = 1)
}

print(gp)
