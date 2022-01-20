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
      conf_record <- bind_rows.(lapply(rvest::html_text(conf_record),
                                       function(x){
                                         data.frame(x, stringsAsFactors=FALSE)
                                       }))
      conf_record <- conf_record %>% dplyr::rename(WL.Conf = x)
      tmrank <- conf %>% rvest::html_elements(".tmrank")
      
      # xml2::xml_remove(tmrank)
      
      conf <- conf %>% rvest::html_table()
      
      colnames(conf) <- header_cols
      
      x <- x %>% rvest::html_table()
      
      colnames(x) <- header_cols
      
      
      suppressWarnings(
        conf <- conf %>% filter.(!is.na(as.numeric(AdjT))) %>%
          as.data.frame()
      )
      x <- x %>% select.(-WL) %>%
        as.data.frame()
      x <- x %>%
        filter(!is.na(Year)) %>%
        mutate.(
          AdjT.Rk = as.numeric(stri_sub(AdjT, -nchar(sub('.*\\.', '', AdjT))+1)),
          AdjO.Rk = as.numeric(stri_sub(AdjO, -nchar(sub('.*\\.', '', AdjO))+1)),
          AdjD.Rk = as.numeric(stri_sub(AdjD, -nchar(sub('.*\\.', '', AdjD))+1)),
          Off.eFG.Pct.Rk = as.numeric(stri_sub(Off.eFG.Pct, -nchar(sub('.*\\.', '', Off.eFG.Pct))+1)),
          Off.TO.Pct.Rk = as.numeric(stri_sub(Off.TO.Pct, -nchar(sub('.*\\.', '', Off.TO.Pct))+1)),
          Off.OR.Pct.Rk = as.numeric(stri_sub(Off.OR.Pct, -nchar(sub('.*\\.', '', Off.OR.Pct))+1)),
          Off.FTRate.Rk = as.numeric(stri_sub(Off.FTRate, -nchar(sub('.*\\.', '', Off.FTRate))+1)),
          Off.FG_2.Pct.Rk = as.numeric(stri_sub(Off.FG_2.Pct, -nchar(sub('.*\\.', '', Off.FG_2.Pct))+1)),
          Off.FG_3.Pct.Rk = as.numeric(stri_sub(Off.FG_3.Pct, -nchar(sub('.*\\.', '', Off.FG_3.Pct))+1)),
          Off.FT.Pct.Rk = as.numeric(stri_sub(Off.FT.Pct, -nchar(sub('.*\\.', '', Off.FT.Pct))+1)),
          Off.FG_3A.Pct.Rk = as.numeric(stri_sub(Off.FG_3A.Pct, -nchar(sub('.*\\.', '', Off.FG_3A.Pct))+1)),
          Off.A.Pct.Rk = as.numeric(stri_sub(Off.A.Pct, -nchar(sub('.*\\.', '', Off.A.Pct))+1)),
          Off.APL.Rk =  as.numeric(stri_sub(Off.APL, -nchar(sub('.*\\.', '', Off.APL))+1)),
          Def.eFG.Pct.Rk = as.numeric(stri_sub(Def.eFG.Pct, -nchar(sub('.*\\.', '', Def.eFG.Pct))+1)),
          Def.TO.Pct.Rk = as.numeric(stri_sub(Def.TO.Pct, -nchar(sub('.*\\.', '', Def.TO.Pct))+1)),
          Def.OR.Pct.Rk = as.numeric(stri_sub(Def.OR.Pct, -nchar(sub('.*\\.', '', Def.OR.Pct))+1)),
          Def.FTRate.Rk = as.numeric(stri_sub(Def.FTRate, -nchar(sub('.*\\.', '', Def.FTRate))+1)),
          Def.FG_2.Pct.Rk = as.numeric(stri_sub(Def.FG_2.Pct, -nchar(sub('.*\\.', '', Def.FG_2.Pct))+1)),
          Def.FG_3.Pct.Rk = as.numeric(stri_sub(Def.FG_3.Pct, -nchar(sub('.*\\.', '', Def.FG_3.Pct))+1)),
          Def.Blk.Pct.Rk = as.numeric(stri_sub(Def.Blk.Pct, -nchar(sub('.*\\.', '', Def.Blk.Pct))+1)),
          Def.FG_3A.Pct.Rk = as.numeric(stri_sub(Def.FG_3A.Pct, -nchar(sub('.*\\.', '', Def.FG_3A.Pct))+1)),
          Def.A.Pct.Rk = as.numeric(stri_sub(Def.A.Pct, -nchar(sub('.*\\.', '', Def.A.Pct))+1)),
          Def.APL.Rk = as.numeric(stri_sub(Def.APL, -nchar(sub('.*\\.', '', Def.APL))+1)),
          Foul2Partic.Pct.Rk = as.numeric(stri_sub(Foul2Partic.Pct, -nchar(sub('.*\\.', '', Foul2Partic.Pct))+1)),
          
          AdjT = substr(sprintf("%.*f",2, as.numeric(AdjT)), 1,
                        nchar(sprintf("%.*f",2, as.numeric(AdjT))) - 1),
          
          AdjO = substr(sprintf("%.*f",2, as.numeric(AdjO)), 1,
                        nchar(sprintf("%.*f",2, as.numeric(AdjO))) - 1),
          AdjD = substr(sprintf("%.*f",2, as.numeric(AdjD)), 1,
                        nchar(sprintf("%.*f",2, as.numeric(AdjD))) - 1),
          Off.eFG.Pct = substr(sprintf("%.*f",2, as.numeric(Off.eFG.Pct)), 1,
                               nchar(sprintf("%.*f",2, as.numeric(Off.eFG.Pct))) - 1),
          Off.TO.Pct = substr(sprintf("%.*f",2, as.numeric(Off.TO.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(Off.TO.Pct))) - 1),
          Off.OR.Pct = substr(sprintf("%.*f",2, as.numeric(Off.OR.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(Off.OR.Pct))) - 1),
          
          Off.FTRate = substr(sprintf("%.*f",2, as.numeric(Off.FTRate)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(Off.FTRate))) - 1),
          Off.FG_2.Pct = substr(sprintf("%.*f",2, as.numeric(Off.FG_2.Pct)), 1,
                                nchar(sprintf("%.*f",2, as.numeric(Off.FG_2.Pct))) - 1),
          Off.FG_3.Pct = substr(sprintf("%.*f",2, as.numeric(Off.FG_3.Pct)), 1,
                                nchar(sprintf("%.*f",2, as.numeric(Off.FG_3.Pct))) - 1),
          Off.FT.Pct = substr(sprintf("%.*f",2, as.numeric(Off.FT.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(Off.FT.Pct))) - 1),
          Off.FG_3A.Pct = substr(sprintf("%.*f",2, as.numeric(Off.FG_3A.Pct)), 1,
                                 nchar(sprintf("%.*f",2, as.numeric(Off.FG_3A.Pct))) - 1),
          Off.A.Pct = substr(sprintf("%.*f",2, as.numeric(Off.A.Pct)), 1,
                             nchar(sprintf("%.*f",2, as.numeric(Off.A.Pct))) - 1),
          Off.APL = substr(sprintf("%.*f",2, as.numeric(Off.APL)), 1,
                           nchar(sprintf("%.*f",2, as.numeric(Off.APL))) - 1),
          
          Def.eFG.Pct = substr(sprintf("%.*f",2, as.numeric(Def.eFG.Pct)), 1,
                               nchar(sprintf("%.*f",2, as.numeric(Def.eFG.Pct))) - 1),
          Def.TO.Pct = substr(sprintf("%.*f",2, as.numeric(Def.TO.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(Def.TO.Pct))) - 1),
          Def.OR.Pct = substr(sprintf("%.*f",2, as.numeric(Def.OR.Pct)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(Def.OR.Pct))) - 1),
          Def.FTRate = substr(sprintf("%.*f",2, as.numeric(Def.FTRate)), 1,
                              nchar(sprintf("%.*f",2, as.numeric(Def.FTRate))) - 1),
          Def.FG_2.Pct = substr(sprintf("%.*f",2, as.numeric(Def.FG_2.Pct)), 1,
                                nchar(sprintf("%.*f",2, as.numeric(Def.FG_2.Pct))) - 1),
          Def.FG_3.Pct = substr(sprintf("%.*f",2, as.numeric(Def.FG_3.Pct)), 1,
                                nchar(sprintf("%.*f",2, as.numeric(Def.FG_3.Pct))) - 1),
          Def.Blk.Pct = substr(sprintf("%.*f",2, as.numeric(Def.Blk.Pct)), 1,
                               nchar(sprintf("%.*f",2, as.numeric(Def.Blk.Pct))) - 1),
          
          Def.FG_3A.Pct = substr(sprintf("%.*f",2, as.numeric(Def.FG_3A.Pct)), 1,
                                 nchar(sprintf("%.*f",2, as.numeric(Def.FG_3A.Pct))) - 1),
          Def.A.Pct = substr(sprintf("%.*f",2, as.numeric(Def.A.Pct)), 1,
                             nchar(sprintf("%.*f",2, as.numeric(Def.A.Pct))) - 1),
          Def.APL = substr(sprintf("%.*f",2, as.numeric(Def.APL)), 1,
                           nchar(sprintf("%.*f",2, as.numeric(Def.APL))) - 1),
          Foul2Partic.Pct = substr(sprintf("%.*f",2, as.numeric(Foul2Partic.Pct)), 1,
                                   nchar(sprintf("%.*f",2, as.numeric(Foul2Partic.Pct))) - 1),
          
          Team.Finish = stringr::str_extract(Coach, stringr::regex("R1|R2|S16|E8|F4|2nd|CH",ignore_case = FALSE)),
          Coach = stringr::str_replace(Coach, stringr::regex("R1|R2|S16|E8|F4|2nd|CH",ignore_case = FALSE),""),
          NCAA_Seed = NA_integer_) %>%
        as.data.frame()
      x <- mutate(x,
                  "NCAA_Seed" = sapply(Coach, function(arg) { as.numeric(gsub("[^0-9]", "", arg)) }),
                  "Coach" = sapply(Coach, function(arg) {
                    stringr::str_trim(stringr::str_replace(stringr::str_remove(arg,'\\d+| \\*| \\*+'),'\\*+','')) }))
      
      suppressWarnings(
        x <- x %>%
          dplyr::filter(!is.na(as.numeric(AdjT))) %>%
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

kp_team_history('Duke')


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

plan(multisession)
tictoc::tic()
team_list <- future_map_dfr(teams[1:200,], kp_team_history)
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
