library(tidyverse)
library(hoopR)
library(janitor)
library(rvest)
library(stringi)
library(DataExplorer)
library(tidytable)
library(furrr)



kp_team_history <- function(team){
  
  tryCatch(
    expr = {
      if (!has_kp_user_and_pw()) stop("This function requires a KenPom subscription e-mail and password combination,\n      set as the system environment variables KP_USER and KP_PW.", "\n       See ?kp_user_pw for details.", call. = FALSE)
      
      browser <- login()
      
      # Check teams parameter in teams list names
      if(!(team %in% hoopR::teams_links$Team)){
        cli::cli_abort( "Incorrect team name as compared to the website, see hoopR::teams_links for team name parameter specifications.")
      }
      teams_links <- hoopR::teams_links[hoopR::teams_links$Year == as.integer(format(Sys.Date(), "%Y")),]
      team_name = teams_links$team.link.ref[teams_links$Team == team]
      
      ### Pull Data
      url <- paste0("https://kenpom.com/history.php?",
                    "t=",team_name)
      
      page <- rvest::session_jump_to(browser, url)
      
      header_cols<- c('Year','Team.Rk','Coach',	'Conf','WL',	'AdjT', 'AdjO',	'AdjD',
                      'Off.eFG.Pct',	'Off.TO.Pct',	'Off.OR.Pct','Off.FTRate',
                      'Off.FG_2.Pct',	'Off.FG_3.Pct',	'Off.FT.Pct',	'Off.FG_3A.Pct',
                      'Off.A.Pct',	'Off.APL',
                      'Def.eFG.Pct', 'Def.TO.Pct',	'Def.OR.Pct',	'Def.FTRate',
                      'Def.FG_2.Pct',	'Def.FG_3.Pct',
                      'Def.Blk.Pct',	'Def.FG_3A.Pct',	'Def.A.Pct',
                      'Def.APL',	'Foul2Partic.Pct')
      
      x<- (page %>%
             xml2::read_html() %>%
             rvest::html_elements(css='#player-table'))[[1]]
      
      ## removing national rankings for easier manipulation
      ## TODO: Add these rankings back as columns
      conf <- (page %>%
                 xml2::read_html() %>%
                 rvest::html_elements(css='#player-table'))[[1]]
      
      conf_record <- (page %>%
                        xml2::read_html() %>%
                        rvest::html_elements("td:nth-child(5) > span"))
      conf_record <- dplyr::bind_rows(lapply(rvest::html_text(conf_record),
                                             function(x){
                                               data.frame(x, stringsAsFactors=FALSE)
                                             }))
      conf_record <- conf_record %>% dplyr::rename(WL.Conf = .data$x)
      tmrank <- conf %>% rvest::html_elements(".tmrank")
      
      # xml2::xml_remove(tmrank)
      
      conf <- conf %>% rvest::html_table()
      
      colnames(conf) <- header_cols
      
      x <- x %>% rvest::html_table()
      
      colnames(x) <- header_cols
      
      
      suppressWarnings(
        conf <- conf %>% dplyr::filter(!is.na(as.numeric(.data$AdjT)))
      )
      x <- x %>% dplyr::select(-.data$WL)
      x <- x %>%
        dplyr::filter(!is.na(.data$Year)) %>%
        dplyr::mutate(
          AdjT.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$AdjT)) == 4, stri_sub(.data$AdjT, -3), ifelse(
            nchar(sub('.*\\.', '', .data$AdjT)) == 3, stri_sub(.data$AdjT, -2), ifelse(
              nchar(sub('.*\\.', '', .data$AdjT)) == 2, stri_sub(.data$AdjT, -1),'N')))),
          AdjO.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$AdjO)) == 4, stri_sub(.data$AdjO, -3), ifelse(
            nchar(sub('.*\\.', '', .data$AdjO)) == 3, stri_sub(.data$AdjO, -2), ifelse(
              nchar(sub('.*\\.', '', .data$AdjO)) == 2, stri_sub(.data$AdjO, -1),'N')))),
          AdjD.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$AdjD)) == 4, stri_sub(.data$AdjD, -3), ifelse(
            nchar(sub('.*\\.', '', .data$AdjD)) == 3, stri_sub(.data$AdjD, -2), ifelse(
              nchar(sub('.*\\.', '', .data$AdjD)) == 2, stri_sub(.data$AdjD, -1),'N')))),
          Off.eFG.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.eFG.Pct)) == 4, stri_sub(.data$Off.eFG.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.eFG.Pct)) == 3, stri_sub(.data$Off.eFG.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.eFG.Pct)) == 2, stri_sub(.data$Off.eFG.Pct, -1),'N')))),
          Off.TO.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.TO.Pct)) == 4, stri_sub(.data$Off.TO.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.TO.Pct)) == 3, stri_sub(.data$Off.TO.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.TO.Pct)) == 2, stri_sub(.data$Off.TO.Pct, -1),'N')))),
          Off.OR.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.OR.Pct)) == 4, stri_sub(.data$Off.OR.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.OR.Pct)) == 3, stri_sub(.data$Off.OR.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.OR.Pct)) == 2, stri_sub(.data$Off.OR.Pct, -1),'N')))),
          Off.FTRate.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.FTRate)) == 4, stri_sub(.data$Off.FTRate, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.FTRate)) == 3, stri_sub(.data$Off.FTRate, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.FTRate)) == 2, stri_sub(.data$Off.FTRate, -1),'N')))),
          Off.FG_2.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.FG_2.Pct)) == 4, stri_sub(.data$Off.FG_2.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.FG_2.Pct)) == 3, stri_sub(.data$Off.FG_2.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.FG_2.Pct)) == 2, stri_sub(.data$Off.FG_2.Pct, -1),'N')))),
          Off.FG_3.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.FG_3.Pct)) == 4, stri_sub(.data$Off.FG_3.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.FG_3.Pct)) == 3, stri_sub(.data$Off.FG_3.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.FG_3.Pct)) == 2, stri_sub(.data$Off.FG_3.Pct, -1),'N')))),
          Off.FT.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.FT.Pct)) == 4, stri_sub(.data$Off.FT.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.FT.Pct)) == 3, stri_sub(.data$Off.FT.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.FT.Pct)) == 2, stri_sub(.data$Off.FT.Pct, -1),'N')))),
          Off.FG_3A.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.FG_3A.Pct)) == 4, stri_sub(.data$Off.FG_3A.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.FG_3A.Pct)) == 3, stri_sub(.data$Off.FG_3A.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.FG_3A.Pct)) == 2, stri_sub(.data$Off.FG_3A.Pct, -1),'N')))),
          Off.A.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.A.Pct)) == 4, stri_sub(.data$Off.A.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.A.Pct)) == 3, stri_sub(.data$Off.A.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.A.Pct)) == 2, stri_sub(.data$Off.A.Pct, -1),'N')))),
          Off.APL.Rk =  as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Off.APL)) == 4, stri_sub(.data$Off.APL, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Off.APL)) == 3, stri_sub(.data$Off.APL, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Off.APL)) == 2, stri_sub(.data$Off.APL, -1),'N')))),
          Def.eFG.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.eFG.Pct)) == 4, stri_sub(.data$Def.eFG.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.eFG.Pct)) == 3, stri_sub(.data$Def.eFG.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.eFG.Pct)) == 2, stri_sub(.data$Def.eFG.Pct, -1),'N')))),
          Def.TO.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.TO.Pct)) == 4, stri_sub(.data$Def.TO.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.TO.Pct)) == 3, stri_sub(.data$Def.TO.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.TO.Pct)) == 2, stri_sub(.data$Def.TO.Pct, -1),'N')))),
          Def.OR.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.OR.Pct)) == 4, stri_sub(.data$Def.OR.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.OR.Pct)) == 3, stri_sub(.data$Def.OR.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.OR.Pct)) == 2, stri_sub(.data$Def.OR.Pct, -1),'N')))),
          Def.FTRate.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.FTRate)) == 4, stri_sub(.data$Def.FTRate, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.FTRate)) == 3, stri_sub(.data$Def.FTRate, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.FTRate)) == 2, stri_sub(.data$Def.FTRate, -1),'N')))),
          Def.FG_2.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.FG_2.Pct)) == 4, stri_sub(.data$Def.FG_2.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.FG_2.Pct)) == 3, stri_sub(.data$Def.FG_2.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.FG_2.Pct)) == 2, stri_sub(.data$Def.FG_2.Pct, -1),'N')))),
          Def.FG_3.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.FG_3.Pct)) == 4, stri_sub(.data$Def.FG_3.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.FG_3.Pct)) == 3, stri_sub(.data$Def.FG_3.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.FG_3.Pct)) == 2, stri_sub(.data$Def.FG_3.Pct, -1),'N')))),
          Def.Blk.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.Blk.Pct)) == 4, stri_sub(.data$Def.Blk.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.Blk.Pct)) == 3, stri_sub(.data$Def.Blk.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.Blk.Pct)) == 2, stri_sub(.data$Def.Blk.Pct, -1),'N')))),
          Def.FG_3A.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.FG_3A.Pct)) == 4, stri_sub(.data$Def.FG_3A.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.FG_3A.Pct)) == 3, stri_sub(.data$Def.FG_3A.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.FG_3A.Pct)) == 2, stri_sub(.data$Def.FG_3A.Pct, -1),'N')))),
          Def.A.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.A.Pct)) == 4, stri_sub(.data$Def.A.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.A.Pct)) == 3, stri_sub(.data$Def.A.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.A.Pct)) == 2, stri_sub(.data$Def.A.Pct, -1),'N')))),
          Def.APL.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Def.APL)) == 4, stri_sub(.data$Def.APL, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Def.APL)) == 3, stri_sub(.data$Def.APL, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Def.APL)) == 2, stri_sub(.data$Def.APL, -1),'N')))),
          Foul2Partic.Pct.Rk = as.numeric(ifelse(nchar(sub('.*\\.', '', .data$Foul2Partic.Pct)) == 4, stri_sub(.data$Foul2Partic.Pct, -3), ifelse(
            nchar(sub('.*\\.', '', .data$Foul2Partic.Pct)) == 3, stri_sub(.data$Foul2Partic.Pct, -2), ifelse(
              nchar(sub('.*\\.', '', .data$Foul2Partic.Pct)) == 2, stri_sub(.data$Foul2Partic.Pct, -1),'N')))),
          
          AdjT = substr(sprintf("%.*f",2, as.numeric(.data$AdjT)), 1,
                        nchar(sprintf("%.*f",2, as.numeric(.data$AdjT))) - 1),
          AdjO = substr(sprintf("%.*f",2, as.numeric(.data$AdjO)), 1,
                        nchar(sprintf("%.*f",2, as.numeric(.data$AdjO))) - 1),
          AdjD = substr(sprintf("%.*f",2, as.numeric(.data$AdjD)), 1,
                        nchar(sprintf("%.*f",2, as.numeric(.data$AdjD))) - 1),
          Off.eFG.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Off.eFG.Pct)), 1,
                               nchar(sprintf("%.*f",2, as.numeric(.data$Off.eFG.Pct))) - 1),
          Off.TO.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Off.TO.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(.data$Off.TO.Pct))) - 1),
          Off.OR.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Off.OR.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(.data$Off.OR.Pct))) - 1),
          
          Off.FTRate = substr(sprintf("%.*f",2, as.numeric(.data$Off.FTRate)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(.data$Off.FTRate))) - 1),
          Off.FG_2.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Off.FG_2.Pct)), 1,
                                nchar(sprintf("%.*f",2, as.numeric(.data$Off.FG_2.Pct))) - 1),
          Off.FG_3.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Off.FG_3.Pct)), 1,
                                nchar(sprintf("%.*f",2, as.numeric(.data$Off.FG_3.Pct))) - 1),
          Off.FT.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Off.FT.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(.data$Off.FT.Pct))) - 1),
          Off.FG_3A.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Off.FG_3A.Pct)), 1,
                                 nchar(sprintf("%.*f",2, as.numeric(.data$Off.FG_3A.Pct))) - 1),
          Off.A.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Off.A.Pct)), 1,
                             nchar(sprintf("%.*f",2, as.numeric(.data$Off.A.Pct))) - 1),
          Off.APL = substr(sprintf("%.*f",2, as.numeric(.data$Off.APL)), 1,
                           nchar(sprintf("%.*f",2, as.numeric(.data$Off.APL))) - 1),
          
          Def.eFG.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Def.eFG.Pct)), 1,
                               nchar(sprintf("%.*f",2, as.numeric(.data$Def.eFG.Pct))) - 1),
          Def.TO.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Def.TO.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(.data$Def.TO.Pct))) - 1),
          Def.OR.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Def.OR.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(.data$Def.OR.Pct))) - 1),
          Def.FTRate = substr(sprintf("%.*f",2, as.numeric(.data$Def.FTRate)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(.data$Def.FTRate))) - 1),
          Def.FG_2.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Def.FG_2.Pct)), 1,
                                nchar(sprintf("%.*f",2, as.numeric(.data$Def.FG_2.Pct))) - 1),
          Def.FG_3.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Def.FG_3.Pct)), 1,
                                nchar(sprintf("%.*f",2, as.numeric(.data$Def.FG_3.Pct))) - 1),
          Def.Blk.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Def.Blk.Pct)), 1,
                               nchar(sprintf("%.*f",2, as.numeric(.data$Def.Blk.Pct))) - 1),
          
          Def.FG_3A.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Def.FG_3A.Pct)), 1,
                                 nchar(sprintf("%.*f",2, as.numeric(.data$Def.FG_3A.Pct))) - 1),
          Def.A.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Def.A.Pct)), 1,
                             nchar(sprintf("%.*f",2, as.numeric(.data$Def.A.Pct))) - 1),
          Def.APL = substr(sprintf("%.*f",2, as.numeric(.data$Def.APL)), 1,
                           nchar(sprintf("%.*f",2, as.numeric(.data$Def.APL))) - 1),
          Foul2Partic.Pct = substr(sprintf("%.*f",2, as.numeric(.data$Foul2Partic.Pct)), 1,
                                   nchar(sprintf("%.*f",2, as.numeric(.data$Foul2Partic.Pct))) - 1),
          
          Team.Finish = stringr::str_extract(.data$Coach, stringr::regex("R1|R2|S16|E8|F4|2nd|CH",ignore_case = FALSE)),
          Coach = stringr::str_replace(.data$Coach, stringr::regex("R1|R2|S16|E8|F4|2nd|CH",ignore_case = FALSE),""),
          NCAA_Seed = NA_integer_)
      x <- dplyr::mutate(x,
                         "NCAA_Seed" = sapply(.data$Coach, function(arg) { as.numeric(gsub("[^0-9]", "", arg)) }),
                         "Coach" = sapply(.data$Coach, function(arg) {
                           stringr::str_trim(stringr::str_replace(stringr::str_remove(arg,'\\d+| \\*| \\*+'),'\\*+','')) }))
      
      suppressWarnings(
        x <- x %>%
          dplyr::filter(!is.na(as.numeric(.data$AdjT))) %>%
          dplyr::mutate_at(c('Year','Team.Rk','AdjT', 'AdjO',	'AdjD',
                             'Off.eFG.Pct',	'Off.TO.Pct',	'Off.OR.Pct','Off.FTRate',
                             'Off.FG_2.Pct',	'Off.FG_3.Pct',	'Off.FT.Pct',	'Off.FG_3A.Pct',
                             'Off.A.Pct',	'Off.APL',
                             'Def.eFG.Pct', 'Def.TO.Pct',	'Def.OR.Pct',	'Def.FTRate',
                             'Def.FG_2.Pct',	'Def.FG_3.Pct',
                             'Def.Blk.Pct',	'Def.FG_3A.Pct',	'Def.A.Pct',
                             'Def.APL',	'Foul2Partic.Pct',
                             'Off.eFG.Pct.Rk',	'Off.TO.Pct.Rk',	'Off.OR.Pct.Rk','Off.FTRate.Rk',
                             'Off.FG_2.Pct.Rk',	'Off.FG_3.Pct.Rk',	'Off.FT.Pct.Rk',	'Off.FG_3A.Pct.Rk',
                             'Off.A.Pct.Rk',	'Off.APL.Rk',
                             'Def.eFG.Pct.Rk', 'Def.TO.Pct.Rk',	'Def.OR.Pct.Rk',	'Def.FTRate.Rk',
                             'Def.FG_2.Pct.Rk',	'Def.FG_3.Pct.Rk',
                             'Def.Blk.Pct.Rk',	'Def.FG_3A.Pct.Rk',	'Def.A.Pct.Rk',
                             'Def.APL.Rk',	'Foul2Partic.Pct.Rk'
          ), as.numeric)
      )
      
      
      x <- x %>%
        dplyr::mutate(Team = team_name) %>%
        dplyr::select(.data$Year, .data$Team.Rk,.data$Team,tidyr::everything())
      ### Store Data
      kenpom <- x %>%
        janitor::clean_names()
      
    }
  )
  return(kenpom)
}

team_list <- list()


teams <- teams_links %>%
  filter.(Year == 2022 & Team != 'St. Thomas') %>%
  distinct.(Team) %>%
  as.data.frame()

tictoc::tic()

for (i in 1:nrow(teams)) {
  team <- teams[i,1]
  hist <- kp_team_history(team)
  team_list[[i]] <- hist
}
tictoc::toc()

tictoc::tic()
plan(multisession)
team_list <- future_map_dfr(teams[1:100,], kp_team_history)
tictoc::toc()

#need to look at regex for the ranking fields
#issue with the column getting cut
all_hist <- do.call(bind_rows, team_list) %>%
  filter.(year >= 2002) %>%
  select.(-c(coach,conf,off_apl,off_apl_rk,def_apl,def_apl_rk, foul2partic_pct, foul2partic_pct_rk,))

tourney_teams <- all_hist %>%
  filter.(!is.na(team_finish))

s16_teams <- tourney_teams %>%
  filter.(!(team_finish %in% c('R1','R2'))) %>%
  select.(year, team, team_rk, ends_with('rk'), team_finish, ncaa_seed)

# s16_teams %>%
#   create_report(
#     report_title = 'S16 Data'
#   )

e8_teams <- tourney_teams %>%
  filter.(!(team_finish %in% c('R1','R2','S16'))) %>%
  select.(year, team, team_rk, ends_with('rk'), team_finish, ncaa_seed)

f4_teams <- tourney_teams %>%
  filter.(!(team_finish %in% c('R1','R2','S16','E8'))) %>%
  select.(year, team, team_rk, ends_with('rk'), team_finish, ncaa_seed)

final_teams <- tourney_teams %>%
  filter.(!(team_finish %in% c('R1','R2','S16','E8','F4'))) %>%
  select.(year, team, team_rk, ends_with('rk'), team_finish, ncaa_seed)

champions <- tourney_teams %>%
  filter.(team_finish == 'CH') %>%
  select.(year, team, team_rk, ends_with('rk'), team_finish, ncaa_seed)


# champions %>%
#   create_report(
#     report_title = 'Champs Data'
#   )


s16_teams %>%
  filter.(ncaa_seed >= 10)
