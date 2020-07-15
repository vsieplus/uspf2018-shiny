#store min and max and mean velocity
min_vel <- round50(min(q_dat()$velocity))
max_vel <- round50(max(q_dat()$velocity))

mn_vel <- round(mean(q_dat()$velocity),2)

brks <- sort(c(seq(min_vel - 50, max_vel + 50, 50), mn_vel))

#setups colors corresponding to break order
clrs <- c(rep("black",length(brks)))
clrs[which(brks == mn_vel)] <- "purple"

gm2 <- ggplot(q_dat(), aes(x = velocity, y = score)) +
  geom_point()+
  geom_vline(xintercept = mn_vel, color = "purple", alpha = 0.7) +
  theme_bw() +
  theme(text=element_text(size = 13, family = "Trebuchet MS"), 
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_blank(),
        axis.text.x = element_text(color = clrs)) +
  scale_x_continuous(breaks = brks, labels = as.character(brks),
                     limits = c(min_vel-60, max_vel + 60)) +
  scale_y_continuous(label = comma)
  labs(x= "Velocity",y = "", title = "Score against velocity")


print(gm2)
