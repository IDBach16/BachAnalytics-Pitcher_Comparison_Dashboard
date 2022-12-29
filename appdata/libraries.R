library(jpeg)
library(patchwork)
library(bslib)
library(shiny)
library(shinyMobile)
library(highcharter)
library(apexcharter)
library(shinyWidgets)
library(cowplot)
library(httr)
library(gtools)
library(apexcharter)
library(RcppArmadillo)
library(dashboardthemes)
library(sortable)
library(flexdashboard)
library(scales)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(devtools)
library(kableExtra)
library(data.table)
library(DT)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(ggdark)
library(ggpubr)
library(ggimage)
library(readr)
library(RMySQL)
library(stringr)
library(glue)
library(tidyr)
library(kableExtra)
library(webshot)
library(formattable)
library(gt)
library(gtExtras)
library(baseballr)
library(dplyr)
library(data.table)
library(DT)
library(FactoMineR)
library(FNN)
library(ggplot2)
library(gtools)
library(MASS)
library(plotly)
library(stringr)
library(tidyr)
library(tools)
library(vegan)
library(shiny)
library(shinyWidgets)
library(shinydashboard)


#setwd("/Users/ezcz/Dropbox/Consulting/Individual Tutoring/IBach 1-1 Intensive/Project/App/appdata")
#setwd("C:/Users/ibach/Dropbox/IBach 1-1 Intensive/Project/App/appdata")
bat <- fread("bat.csv")
pitch <- fread("pitch.csv")

pitch <- pitch %>% 
  mutate(teamseason=paste0(Team,"-",Season))


`%!in%` <- function(x,y)!('%in%'(x,y))


library(plotly)
library(ggplot2)
library(dplyr)

dat <- data.frame(x=c(1,1.1,-.5,-.2),
                  z=c(2,0.4,3,2.2),
                  Pitch=c("Fastball","Fastball","Curveball","Curve"),
                  result=c("called_strike","ball","swinging_strike","called_strike")
)


kzoneplot <- function(dat){
  
  topKzone = 3.5
  botKzone = 1.5
  inKzone = -.85
  outKzone = 0.85
  
  kZone = data.frame(
    PlateLocSide = c(inKzone, inKzone, outKzone, outKzone, inKzone)
    , PlateLocHeight = c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  
  kplot<- 
    ggplot() +
    geom_point(data = dat,
               aes(x=x, y=z,color =Pitch,label = result,label=result))+ 
    coord_equal() +
    geom_label()+
    geom_path(aes(PlateLocSide, PlateLocHeight), data = kZone) + 
    theme_bw()+ 
    xlim(-2,2) + 
    ylim(-.5,6)+
    ggtitle(paste0("Pitch Location ")) + 
    xlab ("Plate Location Side (ft)") + 
    ylab("Plate Location Height (ft)") +
    coord_fixed()+
    theme(plot.title = element_text(hjust = 1))+
    geom_segment(aes(x=-8.5/12,xend=8.5/12,y=(10/12)-1,yend=(10/12)-1),color="black")+
    geom_segment(aes(x=-8.5/12,xend=-8.5/12,y=(10/12)-1,yend=0),color="black")+
    geom_segment(aes(x=8.5/12,xend=8.5/12,y=(10/12)-1,yend=0),color="black")+
    geom_segment(aes(x=8.5/12,xend=0,y=0,yend=(15/12)-1),color="black")+
    geom_segment(aes(x=-8.5/12,xend=0,y=0,yend=(15/12)-1),color="black")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  kplotly <- ggplotly(
    ggplot() +
      geom_point(data = dat,
                 aes(x=x, y=z,color =Pitch,label = result,label=result))+ 
      coord_equal() +
      geom_label()+
      geom_path(aes(PlateLocSide, PlateLocHeight), data = kZone) + 
      theme_bw()+ 
      xlim(-2,2) + 
      ylim(-.5,6)+
      ggtitle(paste0("Pitch Location ")) + 
      xlab ("Plate Location Side (ft)") + 
      ylab("Plate Location Height (ft)") +
      coord_fixed()+
      theme(plot.title = element_text(hjust = 1))+
      geom_segment(aes(x=-8.5/12,xend=8.5/12,y=(10/12)-1,yend=(10/12)-1),color="black")+
      geom_segment(aes(x=-8.5/12,xend=-8.5/12,y=(10/12)-1,yend=0),color="black")+
      geom_segment(aes(x=8.5/12,xend=8.5/12,y=(10/12)-1,yend=0),color="black")+
      geom_segment(aes(x=8.5/12,xend=0,y=0,yend=(15/12)-1),color="black")+
      geom_segment(aes(x=-8.5/12,xend=0,y=0,yend=(15/12)-1),color="black")+
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  assign("kplot",kplot,envir = .GlobalEnv)
  assign("kplotly",kplotly,envir = .GlobalEnv)
}

pitchNumsToChars <- function(pitch_combination_num, pitch_types_list){
  pitches_in_combination <- c()
  for(index in 1:nchar(pitch_combination_num)){
    if(strsplit(pitch_combination_num, '')[[1]][index] == 1){
      pitches_in_combination <- c(pitches_in_combination, pitch_types_list[index])
    }
  }
  pitches_in_combination <- paste(pitches_in_combination, collapse = '-')
  return(pitches_in_combination)
}

pitch_abbrvs <- c('pitch1_fb4'='FF', 
                  'pitch2_ch'='CH', 
                  'pitch3_cv'='CV',
                  'pitch4_cut'='FC', 
                  'pitch5_si'='SI', 
                  'pitch6_sl'='SL')

pitches <- data.frame()
for(pitch in c('pitch1_fb4', 'pitch2_ch', 'pitch3_cv', 'pitch4_cut', 'pitch5_si', 'pitch6_sl')){
  df <- fread(paste0(pitch, '.csv'))
  df <- df %>% dplyr::filter(total_pitches>=500)
  df$pitch <- pitch_abbrvs[pitch]
  pitches <- smartbind(pitches, df)
}

rownames(pitches) <- seq(1:nrow(pitches))

pitches <- pitches %>% 
  mutate(link=paste0("https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/w_426,q_auto:best/v1/people/",player_id,"/headshot/67/current"))
pitches <- pitches %>% dplyr::select('player_id','player_name', 'pitch', 'spin_rate', 'velocity', 'pitch_percent','link')

pitches_wide <- pitches %>% dplyr::select(player_name, pitch, spin_rate, velocity)
pitches_wide <- as.data.frame(dcast(setDT(pitches_wide), player_name~pitch, value.var=c('velocity', 'spin_rate')))

# Collect pitch mix combinations
pitch_types_list <- sort(unname(pitch_abbrvs))
pitch_combinations_nums <- sort(unique(unname(apply(table(pitches$player_name, pitches$pitch), 1, function(x){paste(x, collapse = '')}))))
pitch_combinations_chars <- c()
for(combo in pitch_combinations_nums){
  pitch_combinations_chars <- c(pitch_combinations_chars, pitchNumsToChars(pitch_combination_num = combo, pitch_types_list = pitch_types_list))
  rm(combo)
}
rm(pitch_combinations_nums)


sav_data <- rbind(
  scrape_savant_leaderboards(
    leaderboard = "expected_statistics",
    year = 2021,
    abs = 50,
    min_pa = "q",
    min_pitches = 100,
    min_field = "q",
    min_run = 0,
    player_type = "pitcher",
    fielding_type = "player",
    oaa_position = "",
    oaa_roles = "",
    team = "",
    arsenal_type = "n_",
    run_type = "raw",
    min2b = 5,
    min3b = 0,
    position = "",
    bats = "",
    hand = ""
  ),
  scrape_savant_leaderboards(
    leaderboard = "expected_statistics",
    year = 2022,
    abs = 50,
    min_pa = "q",
    min_pitches = 100,
    min_field = "q",
    min_run = 0,
    player_type = "pitcher",
    fielding_type = "player",
    oaa_position = "",
    oaa_roles = "",
    team = "",
    arsenal_type = "n_",
    run_type = "raw",
    min2b = 5,
    min3b = 0,
    position = "",
    bats = "",
    hand = ""
  )) %>% 
  mutate(Player=paste0(last_name,", ",first_name),
         link=paste0("https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/w_426,q_auto:best/v1/people/",player_id,"/headshot/67/current"))


test <- baseballr::fg_pitcher_leaders(2022,2022)
