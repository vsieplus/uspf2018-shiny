#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#setwd("C:/Users/Ryan/Documents/Rr/shiny/firstapp/uspf2018")
#
#TOFIX:
# top8 missing dat?

source("fns.R", local = T)
source("setup.R", local = T)

# Define UI for application (declare inputs/outputs)
ui <- fluidPage(
  theme = "style.css",
  
  h1(tags$strong("USPF 2018 Stats")),
  
  tabsetPanel(
    tabPanel("Speed - Qualifiers", 
             p(),
             
             #code for tab 1
             fluidRow(
              
                column(4, offset = 1,
                      actionButton(inputId = "male", label = "Male"),
                      actionButton(inputId = "male2", label = "Male (Top 16)"),
                      actionButton(inputId = "female", label = "Female"),
                      p(),
                      uiOutput("select"),
                      selectInput(inputId = "select", label = sel_plyrs,
                                  choices = as.list(mydfs[[5]]$player), multiple = T,
                                  selected = as.list(mydfs[[5]]$player)[1:16]),
                      actionButton(inputId = "update", label = up_plyrs)),
              
                column(2, offset = 1,
                      checkboxGroupInput(inputId = "steps", label = "Choose Steps to Display", 
                                         choices = as.list(stps), selected = as.list(stps)))
             ),
             plotOutput(outputId = "bar", height = 550),
             plotOutput(outputId = "vel", height = 550), 
             dataTableOutput(outputId = "table"),
             
             a(href = "http://www.mediafire.com/file/i111gi15844p4f0",
               "Download USPF 2018 Master Data")
             ),
    tabPanel("Speed - Top 8", p(),
             
             fluidRow(column(3, offset = 1,
                             actionButton(inputId = "m8", label = "Male"),
                             actionButton(inputId = "f8", label = "Female"),
                             p(),
                             radioButtons(inputId = "round8", label = "Select Round",
                                          choiceNames = round_names, 
                                          choiceValues = c(1,2,3,4))
                             ),
                      
                      column(3, 
                             uiOutput("select8"),
                             selectInput(inputId = "select8", label = "Select Matchup",
                                         choices = levels(o8$matchup[o8$gender == "m" &
                                                                     o8$round == 1])),
                             uiOutput("chart8"),
                             selectInput(inputId = "chart8", label = "Select Chart",
                                         choices = levels(o8$titles[o8$gender == "m" &
                                                                      o8$round == 1])),
                             actionButton("update8", label = "Update")
                             ), 
                      
                      column(3, offset = 1,
                             tags$h4("Player Map"),
                             leaflet::leafletOutput("minimap", height = 250)
                             )
             
                      ),
             
             tags$br(), tags$br(),
             
             fluidRow(column(2, offset = 1,
                             htmlOutput(outputId = "p1"),
                             tableOutput(outputId = "p1stats")       
                             
                             ),
                      
                      column(2, 
                             plotOutput(outputId = "p1steps", width = "220px")
                             
                             ),
                      
                      column(2, offset = 1,
                             htmlOutput(outputId = "p2"),
                             tableOutput(outputId = "p2stats")
                             ),
                      
                      column(2, 
                             plotOutput(outputId = "p2steps",width = "220px")
                             
                             )
                      
                      )
             
             ),
    tabPanel("Freestyle", 
             
             fluidRow(
               column(2, offset = 1, tags$br(),
                      radioButtons(inputId = "fsrd", label = "Choose Round:",
                                   choiceNames = c("Qualifiers","Top 8"),
                                   choiceValues = c(1,2)),
                      tags$br(),
                      uiOutput("fsplyrs"),
                      selectInput(inputId = "fsplyrs", label = "Select Player",
                                  choices = NULL)
                      
                      ),
               column(3, offset = 1, tags$br(),
                      htmlOutput(outputId = "fsstats"),
                      tableOutput(outputId = "fstable")
                      ),
               column(3, 
                      plotOutput(outputId = "fsbar")
                      )
               
             )
             
             ),
    tabPanel("Competitor Map", p(),
            
             fluidRow(column(8,
                             leaflet::leafletOutput(outputId = "map")),                      
                      column(4,
                             htmlOutput(outputId = "citystats"))
                      )
             
             ),
    tabPanel("About", tags$br(),
             "USPF 2018 was a national Pump it Up tournament held on April 28, 2018. 
              Nearly 70 Players from across America traveled to San Jose to compete in 
              both speed and freestyle competitions. The event was organized by the 
              'Norcal World Order' Team and was made possible with the the help of many
              volunteers. I created this app as a way to explore some data from the 
              event, so have fun using it and see what you can find! :D ",
             
             tags$br(), tags$br(), "~vSie")
  )
  
  
)

#tell server how to assemble inputs into outputs
server <- function(input, output, session) {

  #reactive values used in the app
  #gender - string indicating which qualifiers 
  #gender8 - string indicating which top8 data 
  #round - integer indicating the top of round 8 (1-4, qf-f)
  #city - stores the last clicked marker on the map
  #players - qualifier player list
  #mc - stores max combo/player for current city
  #mv - stores max velocity/player for current city
  #spct, fsct - counts # of speed/freestyle players for city
  #prat, ..., mcrat, vel - store avg ratios for current city
  #noteskin - store most popular noteskin(s) for city
  #p1/p2 - store top 8 p1/p2 player names
  #pcolors - string indicating green/red,red/green based on who won
  #cs - chartsteps for current selected chart in top8 viewer
  #qrd - indicates which qualifier round the user has selected
  #
  rv <- reactiveValues(gender = "m", gender8 = "m", round = 1, city = "",
                       players = "", mc = "", mv = "", spct = 0, fsct = 0,
                       prat = 0, grat = 0, gdrat = 0, brat = 0, mrat = 0, mcrat = 0,
                       noteskin = "", p1 = "", p2 = "", pcolors = c("",""), cs = 0,
                       vel = 0,qrd = 1)
  
  #change player list and chart list for speed - top 8
  observeEvent(input$m8, {
    rv$gender8 <- "m"
  })
  
  observeEvent(input$f8, {
    rv$gender8 <- "f"
  })
 
  observeEvent(input$fsrd, {
    updateSelectInput(session, "fsplyrs", 
                      choices = fa$player[fa$round == input$fsrd])
  })
  
  #update options for player/chart select input based on gender/round
  observeEvent(c(input$round8, input$m8, input$f8), {
    
    dat_len <- length(t8_data()[,1])
    
    if(dat_len > 0){
        if(rv$round != 4){      
          everyother <- seq(2, dat_len, 4)
        } else {
          everyother <- 1
        }
      
    } else {
      everyother <- 0
    }
    
    updateSelectInput(session, "select8",
                      choices = t8_data()$matchup[everyother])
  
  })
  
  observeEvent(input$select8, {
    updateSelectInput(session, "chart8", 
                      choices = t8_data()$titles[t8_data()$matchup == input$select8])
  })
  
  #change player list for speed - qualifiers
  observeEvent(input$male, {
    rv$gender = "m"
    updateSelectInput(session, "select", choices = oq[oq$gender == "m","player"],
                      selected = oq[oq$gender == "m","player"][1:16])
  })
    
  observeEvent(input$male2, {
    rv$gender = "m2"
    updateSelectInput(session, "select", choices = mydfs[[6]]$player,
                      selected = mydfs[[6]]$player[1:8])
  })
  
  observeEvent(input$female, {
    rv$gender = "f"
    updateSelectInput(session, "select", choices = oq[oq$gender == "f","player"],
                      selected = oq[oq$gender == "f","player"][1:8])
  })    
  
  #find which players the user wants to see, and isolate that data
  q_dat <- eventReactive(c(input$update), {
    
    if(rv$gender == "m"){
      pindex <- mydfs[[5]]$player %in% input$select
      rv$qrd <- 1
      mydfs[[5]][pindex,]
    } else  if(rv$gender == "f"){
      pindex <- mydfs[[1]]$player %in% input$select
      rv$qrd <- 1
      mydfs[[1]][pindex,]
    } else {
      pindex <- mydfs[[6]]$player %in% input$select
      rv$qrd <- 2
      mydfs[[6]][pindex,]
    }

  })
  
  #add 'bar' object to output
  #braces allow a code block to be passed
  output$bar <- renderPlot({
    #create plot
    source("createPlot.R", local = T)
  })
  
  #top8 data
  t8_data <- eventReactive(c(input$round8, input$m8, input$f8), {
    rv$round <- input$round8
    o8[o8$gender == rv$gender8 & o8$round == rv$round,]
  })  
  
  fs_dat <- eventReactive(input$fsplyrs, {
    fa[fa$round == input$fsrd & fa$player == input$fsplyrs,]
  })
  
  output$vel <- renderPlot({
    source("createPlotVel.R", local = T)
  })

  output$table <- renderDataTable({
    q_dat()[,core_cols[1:15]]
  })
  
  output$map <- leaflet::renderLeaflet({
    source("makemap.R", local = T)
    pmap
  })
  
  output$minimap <- leaflet::renderLeaflet({
    pmap2
  })
  
  observeEvent(input$update8, {
    source("makemap2.R", local = T)
    proxy
  })
  
  #update reactive values based on which city the user clicks
  observe({
    click <- input$map_marker_click
    
    if(is.null(click) || click$id == "hi")
      return()
    
    rv$city <- click$id
    
    incity <- oa$locations == rv$city
    
    competitors <- oa$player[incity]
    competitors <- unique(competitors)
    
    rv$players <- paste(competitors, collapse = ', ')
    
    maxc <- max(oa$max.combo[incity], na.rm =T)
    maxv <- max(oa$velocity[incity], na.rm=T)
    
    maxc_p <- oa$player[oa$max.combo == maxc & incity][1]
    maxv_p <- oa$player[oa$velocity == maxv & incity][1]
    
    rv$mc <- paste(maxc, " (", maxc_p,")", collapse = "")
    rv$mv <- paste(maxv, " (", maxv_p,")", collapse = "")
    
    #grab unique players and competitor types
    types_u <- oa$type[!(duplicated(oa$player)) & incity]
    
    rv$fsct <- sum(types_u == "fs")
    rv$spct <- sum(types_u == "sp")
    
    #reassign ratios using update_rats function
    for(i in 1:length(ratios_shrt)){
      rv[[ratios_shrt[i]]] <- update_rats(ratios[i], rv$city)
    }
    
    rv$vel <- update_rats("velocity", rv$city)
    
    #find most popular noteskin (unique users)
    cty_nt <- oa$noteskin[!(duplicated(oa$player)) & incity]
    mu_nt <- max(table(cty_nt))
    
    pop_noteskin <- levels(cty_nt)[which(table(cty_nt) == mu_nt)]
    
    tmp_nt <- paste(toTitleCase(pop_noteskin), " (", mu_nt, ")",sep = "")
    
    rv$noteskin <- paste(tmp_nt, collapse = ', ')
    
  })

  #the text for the stats displayed next to the map
  output$citystats <- renderText({
      if(rv$city == "")
        return()
    
      paste( sep = "", "<b>", rv$city,"</b>", "<br/>", "<br/>",
      "<b>Players: </b>", rv$players, "<br/>","<br/>", 
      "<b>Speed: </b>", rv$spct, "<br/>",
      "<b>Freestyle: </b>", rv$fsct, "<br/>", "<br/>",
      "<b>Highest Combo: </b>", rv$mc, "<br/>",
      "<b>Highest Velocity: </b>", rv$mv, "<br/>",
      "<b>Average Velocity: </b>", rv$vel, "<br/>", "<br/>",
      rat_txt("Perfects", rv$prat), rat_txt("Greats", rv$grat),
      rat_txt("Goods", rv$gdrat), rat_txt("Bads", rv$brat),
      rat_txt("Misses", rv$mrat), rat_txt("Max Combo", rv$mcrat), "</br>", "</br>",
      "<b>Most Popular Noteskin(s): </b>", rv$noteskin
      )
      
  })
  
  observeEvent(input$chart8,{
    chrt <- input$chart8
    
    plyd_chrt <- o8$titles == chrt
    
    if(input$round8 == 1){
      rv$p1 <- o8$player[plyd_chrt & o8$player_side == "p1"]
      rv$p2 <- o8$player[plyd_chrt & o8$player_side == "p2"]
    } else {
      rv$p1 <- o8$player[plyd_chrt & o8$order == 1]
      rv$p2 <- o8$player[plyd_chrt & o8$order == 2]
    }
    rv$pcolors <- ifelse(o8$score[plyd_chrt] > 
                        rev(o8$score[plyd_chrt]),
                      title_cols[1], title_cols[2])
    
    rv$cs <- o8$chartsteps[plyd_chrt][1]
  })
  
  #update top8 player data
  p1dat <- eventReactive(input$update8, {
    isplayer <- o8$player == rv$p1
    ischart <- o8$titles == input$chart8
    
    d <- o8[isplayer & ischart ,]
    d$col <- rv$pcolors[1]
    d
  })
  
  p2dat <- eventReactive(input$update8, {
    isplayer <- o8$player == rv$p2
    ischart <- o8$titles == input$chart8
    
    d <- o8[isplayer & ischart,]
    d$col <- rv$pcolors[2]
    d
  })

  #update player nammes
  output$p1 <- renderText({
    paste(tags$strong(style = p1dat()$col, p1dat()$player))
  })
  
  output$p2 <- renderText({
    paste(tags$strong(style = p2dat()$col, p2dat()$player))
  })
  
  output$p1stats <- renderTable({
    Sys.sleep(.25)
    t_df(p1dat()[,stps_e])
  })
  
  p1_long <- eventReactive(input$update8, {
    q <- long_dat[[3]][long_dat[[3]]$player == rv$p1 &
                    long_dat[[3]]$titles == input$chart8, ]
    q$cs <- rv$cs
    q
  })
  
  output$p1steps <- renderPlot({
    Sys.sleep(.35)
    print(mini_plot(p1_long(),p1_long()$cs[1]))
  })

  output$p2stats <- renderTable({
    Sys.sleep(.25)
    t_df(p2dat()[,stps_e])
  })
  
  p2_long <- eventReactive(input$update8, {
    q <- long_dat[[3]][long_dat[[3]]$player == rv$p2 &
                             long_dat[[3]]$titles == input$chart8, ]
    q$cs <- rv$cs
    q
  })
    
  output$p2steps <- renderPlot({
    Sys.sleep(.35)
    
    print(mini_plot(p2_long(),p2_long()$cs[1]))
  })

  output$fsstats <- renderText({
    paste(sep = "",
          tags$strong("Chart: "), fs_dat()$titles, tags$br(), tags$br(),
          tags$strong("Dance Grade: "), fs_dat()$grade, tags$br(),
          tags$strong("Dance Score (*/100): "), fs_dat()$d_scr, tags$br(), tags$br(),
          tags$strong("Ranking: ", fs_dat()$rank)
          )
  })
  
  output$fstable <- renderTable({
    Sys.sleep(.25)
    t_df(fs_dat()[,fs_core])
  })
  
  fss_long <- eventReactive(input$fsplyrs, {
    q <- long_dat[[4]]
    q <- q[q$player == fs_dat()$player &
                    q$titles == fs_dat()$titles,]
  })
  
  output$fsbar <- renderPlot({
    print(mini_plot(fss_long(), sum(fs_dat()[,stps]) - 20))
  })
  
  #observeEvent(input$select, {print(input$select)})
}

# Run the application
shinyApp(ui = ui, server = server)

#deploy app
# library(rsconnect)
# deployApp(appdir)
