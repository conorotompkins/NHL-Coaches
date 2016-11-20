setwd("~/github folder/NHL-Coaches")
library(ggthemes)
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(scales)
library(readr)
library(tidyr)
library(ggrepel)

#Load 05/06-14/15 data

coach_data <- read.csv("coach.data.csv")
coach_data <- coach_data %>%
        filter(Game.Type == "Regular Season", season != "20152016")

coach_data$Date <- mdy(coach_data$Date)

coach_data$season <- as.character(coach_data$season)

coach_data$Team <- as.character(coach_data$Team)


#clean up coach.data column names
coach_data <- coach_data %>%
        rename(game_type = Game.Type,
               head_coach = Head.Coach,
               first_name = First.Name,
               last_name = Last.Name,
               team = Team,
               date = Date
        )

#load 15/16 data

season_load <- read.csv("15.16.update.5.21.csv")
season_load$Date <- ymd(season_load$Date)

season_load$season <- as.character(season_load$season)

season_load$Team <- as.character(season_load$Team)

#clean up season_load column names
season_load <- season_load %>%
        select(-c(X, CF..1, OSh..1, OFOn..1, FO..1, OSv..1))
season_load <- season_load %>%
        rename(C.diff = C..., 
               F.diff = F..., 
               S.diff = S..., 
               SC.diff = SC..., 
               HSC.diff = HSC..., 
               G.diff = G..., 
               CF.per = CF.,
               FF.per = FF.,
               SF.per = SF.,
               SCF.per = SCF., 
               HSCF.per = HSCF.,
               ZSO.per = ZSO.,
               OCOn.per = OCOn.,
               GF.per = GF.,
               OFenSh.per = OFenSh.,
               OSh.per = OSh.,
               OSv.per = OSv.,
               FO.per = FO.,
               OFOn.per = OFOn.,
               team = Team,
               date = Date
        )

season_load$game_type <- "Regular Season"
season_load$head_coach <- NA
season_load$first_name <- NA
season_load$last_name <- NA
season_load$CP60.1 <- NULL

#combined coach.data and season.load
combined <- rbind(coach_data, season_load)

#create game number columns
df <- NULL

#create franchise names
data <- NULL
team_list <- as.character(unique(combined$team))
franchise_list <- c("N.J", "CHI", "CBJ", "EDM", "DET", "NYR", "PHX_ARI", "CGY", "S.J", "ATL_WPG", "CAR", "MTL", "TOR", "BUF", "L.A", "DAL", "NYI", "OTT", "BOS", "T.B", "FLA", "NSH", "MIN", "VAN", "PHI", "STL", "COL", "WSH", "ANA", "PIT", "ATL_WPG", "PHX_ARI")
data <- as.data.frame(cbind(team_list, franchise_list))
data <- data %>%
        rename(team = team_list,
               franchise = franchise_list) %>%
        mutate(team = as.character(team),
               franchise = as.character(franchise))

#join franchise names to combined
combined <- left_join(combined, data, by = "team")

df <- combined %>%
        group_by(franchise) %>%
        mutate(team_game_number=dense_rank(date)) %>%
        ungroup()

df <- df %>%
        group_by(season, franchise) %>%
        mutate(season_game_number=dense_rank(date)) %>%
        ungroup()

#create full team names
full_team_names <- c("New Jersey Devils", "Chicago Blackhawks", "Columbus Blue Jackets", "Edmonton Oilers", "Detroit Red Wings", "New York Rangers", "Phoenix Coyotes", "Calgary Flames", "San Jose Sharks",  "Atlanta Thrashers", "Carolina Hurricanes", "Montreal Canadiens", "Toronto Maple Leafs", "Buffalo Sabres", "Los Angeles Kings", "Dallas Stars", "New York Islanders", "Ottawa Senators", "Boston Bruins", "Tampa Bay Lightning", "Florida Panthers", "Nashville Predators", "Minnesota Wild", "Vancouver Canucks", "Philadelphia Flyers", "St. Louis Blues", "Colorado Avalanche", "Washington Capitals", "Anaheim Ducks", "Pittsburgh Penguins", "Winnipeg Jets", "Arizona Coyotes")
full_team_names <- as.data.frame(cbind(team_list, full_team_names))
full_team_names <- full_team_names %>%
        mutate(team_list = as.character(team_list),
               full_team_names = as.character(full_team_names)) %>%
        rename(team = team_list)
full_team_names$franchise_name <- full_team_names$full_team_names
full_team_names$franchise_name[full_team_names$franchise_name =="Arizona Coyotes"]<-"Phoenix_Arizona Coyotes"
full_team_names$franchise_name[full_team_names$franchise_name =="Phoenix Coyotes"]<-"Phoenix_Arizona Coyotes"
full_team_names$franchise_name[full_team_names$franchise_name =="Atlanta Thrashers"]<-"Atlanta Thrashers_Winnipeg Jets"
full_team_names$franchise_name[full_team_names$franchise_name =="Winnipeg Jets"]<-"Atlanta Thrashers_Winnipeg Jets"

#join full team names to combined
df <- left_join(df, full_team_names, by = "team")

#Create Head Coach Column
df$head_coach[df$season == "20152016" & df$team == "PIT" & df$date <= "2015-12-14"]<-"Mike Johnston"
df$head_coach[df$season == "20152016" & df$team == "PIT" & is.na(df$head_coach)]<-"Mike Sullivan"
df$head_coach[df$season == "20152016" & df$team == "CBJ" & df$date <= "2015-10-20"]<-"Todd Richards"
df$head_coach[df$season == "20152016" & df$team == "CBJ" & is.na(df$head_coach)]<-"John Tortorella"
df$head_coach[df$season == "20152016" & df$team == "MIN" & df$date <= "2016-02-13"]<-"Mike Yeo"
df$head_coach[df$season == "20152016" & df$team == "MIN" & is.na(df$head_coach)]<-"John Torchetti"
df$head_coach[df$season == "20152016" & df$team == "ANA"]<-"Bruce Boudreau"
df$head_coach[df$season == "20152016" & df$team == "ARI"]<-"Dave Tippett"
df$head_coach[df$season == "20152016" & df$team == "BOS"]<-"Claude Julien"
df$head_coach[df$season == "20152016" & df$team == "BUF"]<-"Dan Bylsma"
df$head_coach[df$season == "20152016" & df$team == "CGY"]<-"Bob Hartley"
df$head_coach[df$season == "20152016" & df$team == "CAR"]<-"Bill Peters"
df$head_coach[df$season == "20152016" & df$team == "CHI"]<-"Joel Quenneville"
df$head_coach[df$season == "20152016" & df$team == "COL"]<-"Patrick Roy"
df$head_coach[df$season == "20152016" & df$team == "DAL"]<-"Lindy Ruff"
df$head_coach[df$season == "20152016" & df$team == "DET"]<-"Jeff Blashill"
df$head_coach[df$season == "20152016" & df$team == "EDM"]<-"Todd McLellan"
df$head_coach[df$season == "20152016" & df$team == "FLA"]<-"Gerrard Gallant"
df$head_coach[df$season == "20152016" & df$team == "L.A"]<-"Darryl Sutter"
df$head_coach[df$season == "20152016" & df$team == "MTL"]<-"Michel Therrien"
df$head_coach[df$season == "20152016" & df$team == "NSH"]<-"Peter Laviolette"
df$head_coach[df$season == "20152016" & df$team == "N.J"]<-"John Hynes"
df$head_coach[df$season == "20152016" & df$team == "NYI"]<-"Jack Capuano"
df$head_coach[df$season == "20152016" & df$team == "NYR"]<-"Alain Vigneault"
df$head_coach[df$season == "20152016" & df$team == "OTT"]<-"Dave Cameron"
df$head_coach[df$season == "20152016" & df$team == "PHI"]<-"Dave Hakstol"
df$head_coach[df$season == "20152016" & df$team == "S.J"]<-"Peter Deboer"
df$head_coach[df$season == "20152016" & df$team == "STL"]<-"Ken Hitchcock"
df$head_coach[df$season == "20152016" & df$team == "T.B"]<-"Jon Cooper"
df$head_coach[df$season == "20152016" & df$team == "TOR"]<-"Mike Babcock"
df$head_coach[df$season == "20152016" & df$team == "VAN"]<-"Willie Desjardins"
df$head_coach[df$season == "20152016" & df$team == "WSH"]<-"Barry Trotz"
df$head_coach[df$season == "20152016" & df$team == "WPG"]<-"Paul Maurice"

#create Head.Coach_u and substitute select coach names
df$head_coach <- as.character(df$head_coach)
df$head_coach_u <- df$head_coach
df$head_coach_u[df$head_coach =="Bob Gainey" & df$season == "20052006"]<-"Bob Gainey1"
df$head_coach_u[df$head_coach =="Bob Gainey" & df$season == "20082009"]<-"Bob Gainey2"
df$head_coach_u[df$head_coach =="Lou Lamoriello" & df$season == "20052006"]<-"Lou Lamoriello1"
df$head_coach_u[df$head_coach =="Lou Lamoriello" & df$season == "20062007"]<-"Lou Lamoriello2"
df$head_coach_u[df$head_coach =="Jacques Lemaire" & df$season == "20092010"]<-"Jacques Lemaire1"
df$head_coach_u[df$head_coach =="Jacques Lemaire" & df$season == "20102011"]<-"Jacques Lemaire2"
df$head_coach_u[df$head_coach =="Bryan Murray" & (df$season == "20052006")]<-"Bryan Murray1"
df$head_coach_u[df$head_coach =="Bryan Murray" & (df$season == "20062007")]<-"Bryan Murray1"
df$head_coach_u[df$head_coach =="Bryan Murray" & (df$season == "20072008")]<-"Bryan Murray2"


#create division lists
dv1_seasons = c("20052006", "20062007", "20072008", "20082009", "20092010", "20102011", "20112012", "20122013")
dv2_seasons = c("20132014", "20142015", "20152016")

atlantic_division <- list(franchise_name = c("New Jersey Devils", "Philadelphia Flyers", "New York Rangers", "New York Islanders", "Pittsburgh Penguins"), seasons = dv1_seasons)
northeast_division <- list(franchise_name = c("Ottawa Senators", "Buffalo Sabres", "Montreal Canadiens", "Toronto Maple Leafs", "Boston Bruins"), seasons = dv1_seasons)
southeast_division <- list(franchise_name = c("Carolina Hurricanes", "Tampa Bay Lightning", "Atlanta Thrashers_Winnipeg Jets", "Florida Panthers", "Washington Capitals"), seasons = dv1_seasons)
central_division <- list(franchise_name = c("Detroit Red Wings", "Nashville Predators", "Columbus Blue Jackets", "Chicago Blackhawks", "St. Louis Blues"), seasons = dv1_seasons)
northwest_division <- list(franchise_name = c("Calgary Flames", "Colorado Avalanche", "Edmonton Oilers", "Vancouver Canucks", "Minnesota Wild"), seasons = dv1_seasons)
pacific_division <- list(franchise_name = c("Dallas Stars", "San Jose Sharks", "Anaheim Ducks", "Los Angeles Kings", "Phoenix_Arizona Coyotes"), seasons = dv1_seasons)

atlantic2_division <- list(franchise_name = c("Boston Bruins", "Tampa Bay Lightning", "Montreal Canadiens", "Detroit Red Wings", "Ottawa Senators", "Toronto Maple Leafs", "Florida Panthers", "Buffalo Sabres"), seasons = dv2_seasons)
metropolitan_division <- list(franchise_name = c("Pittsburgh Penguins", "New York Rangers", "New York Islanders", "Philadelphia Flyers", "Columbus Blue Jackets", "Carolina Hurricanes", "New Jersey Devils", "Washington Capitals"), seasons = dv2_seasons)
central2_division <- list(franchise_name = c("Colorado Avalanche", "St. Louis Blues", "Chicago Blackhawks", "Minnesota Wild", "Dallas Stars", "Nashville Predators", "Atlanta Thrashers_Winnipeg Jets"), seasons = dv2_seasons)
pacific2_division <- list(franchise_name = c("Anaheim Ducks", "San Jose Sharks", "Los Angeles Kings", "Phoenix_Arizona Coyotes", "Vancouver Canucks", "Calgary Flames", "Edmonton Oilers"), seasons = dv2_seasons)

#create conference lists
conference1 <- list(east = c("New Jersey Devils", "Philadelphia Flyers", "New York Rangers", "New York Islanders", "Pittsburgh Penguins",
                             "Ottawa Senators", "Buffalo Sabres", "Montreal Canadiens", "Toronto Maple Leafs", "Boston Bruins",
                             "Carolina Hurricanes", "Tampa Bay Lightning", "Atlanta Thrashers_Winnipeg Jets", "Florida Panthers", "Washington Capitals"), 
                    west = c("Detroit Red Wings", "Nashville Predators", "Columbus Blue Jackets", "Chicago Blackhawks", "St. Louis Blues", 
                             "Calgary Flames", "Colorado Avalanche", "Edmonton Oilers", "Vancouver Canucks", "Minnesota Wild", 
                             "Dallas Stars", "San Jose Sharks", "Anaheim Ducks", "Los Angeles Kings", "Phoenix_Arizona Coyotes"),
                    seasons = c("20052006", "20062007", "20072008", "20082009", "20092010", "20102011", "20112012", "20122013"))

conference2 <- list(east = c("Boston Bruins", "Tampa Bay Lightning", "Montreal Canadiens", "Detroit Red Wings", "Ottawa Senators", "Toronto Maple Leafs", "Florida Panthers", "Buffalo Sabres",
                             "Pittsburgh Penguins", "New York Rangers", "New York Islanders", "Philadelphia Flyers", "Columbus Blue Jackets", "Carolina Hurricanes", "New Jersey Devils", "Washington Capitals"),
                    west = c("Colorado Avalanche", "St. Louis Blues", "Chicago Blackhawks", "Minnesota Wild", "Dallas Stars", "Nashville Predators", "Atlanta Thrashers_Winnipeg Jets", 
                             "Anaheim Ducks", "San Jose Sharks", "Los Angeles Kings", "Phoenix_Arizona Coyotes", "Vancouver Canucks", "Calgary Flames", "Edmonton Oilers"),
                    seasons = c("20132014", "20142015", "20152016"))


#add division column
df$division <- NULL
df$division[df$franchise_name %in% atlantic_division$franchise_name & df$season %in% atlantic_division$seasons] <- "Atlantic Division"
df$division[df$franchise_name %in% northeast_division$franchise_name & df$season %in% northeast_division$seasons] <- "Northeast Division"
df$division[df$franchise_name %in% southeast_division$franchise_name & df$season %in% southeast_division$seasons] <- "Southeast Division"

df$division[df$franchise_name %in% central_division$franchise_name & df$season %in% central_division$seasons] <- "Central Division"
df$division[df$franchise_name %in% northwest_division$franchise_name & df$season %in% northwest_division$seasons] <- "Northwest Division"
df$division[df$franchise_name %in% pacific_division$franchise_name & df$season %in% pacific_division$seasons] <- "Pacific Division"


df$division[df$franchise_name %in% atlantic2_division$franchise_name & df$season %in% atlantic2_division$seasons] <- "Atlantic2 Division"
df$division[df$franchise_name %in% metropolitan_division$franchise_name & df$season %in% metropolitan_division$seasons] <- "Metropolitan Division"
df$division[df$franchise_name %in% pacific2_division$franchise_name & df$season %in% pacific2_division$seasons] <- "Pacific2 Division"
df$division[df$franchise_name %in% central2_division$franchise_name & df$season %in% central2_division$seasons] <- "Central2 Division"

#add conference column
df$conference <- NULL
df$conference[df$franchise_name %in% conference1$east & df$season %in% conference1$seasons] <- "Eastern Conference"
df$conference[df$franchise_name %in% conference1$west & df$season %in% conference1$seasons] <- "Western Conference"
df$conference[df$franchise_name %in% conference2$east & df$season %in% conference2$seasons] <- "Eastern Conference"
df$conference[df$franchise_name %in% conference2$west & df$season %in% conference2$seasons] <- "Western Conference"


df <- df %>%
        group_by(head_coach) %>%
        mutate(coach_game_number=dense_rank(date)) %>%
        ungroup()

df <- df %>%
        arrange(date) %>%
        group_by(franchise_name) %>%
        mutate(franchise_game_number=dense_rank(date)) %>%
        ungroup()

df <- df %>%
        arrange(franchise_name, date) %>%
        mutate(key = paste(team, season))

games_coached <- df %>%
        select(date, head_coach) %>%
        group_by(head_coach) %>%
        summarize(games_coached = n())

df <- left_join(df, games_coached, by = "head_coach")

team_summaries <- df %>%
        select(season, key, CF.per) %>%
        group_by(season, key) %>%
        summarize(CF.percent.season = mean(CF.per))

df <- left_join(df, team_summaries, by = "key")

df <- df %>%
        rename(season = season.x) %>%
        select(-season.y)

df <- df %>%
        mutate(game_type = as.character(game_type),
               first_name = as.character(first_name),
               last_name = as.character(last_name))

df.division <- df %>%
        select(season, date, season_game_number, division, full_team_names, CF.per, CF.percent.season) %>%
        filter(season == "20152016" & division == "Metropolitan Division") %>%
        arrange(-CF.percent.season) %>%
        mutate(full_team_names = reorder(full_team_names, -CF.percent.season))

write_csv(df, "combined.coaches.csv")

team_data <- read_csv("combined.coaches.csv")

cleaned_coach_data <- team_data %>%
        select(game_type, season, date, team, full_team_names, franchise, franchise_name, team_game_number, season_game_number, coach_game_number, GF:TOI, -first_name, -last_name)

write_csv(cleaned_coach_data, "NHH_coach_data.csv")


df.division <- df %>%
        select(season, date, season_game_number, division, full_team_names, team, franchise_name, CF.per, CF.percent.season) %>%
        filter(season == "20152016") %>%
        arrange(-CF.percent.season) %>%
        mutate(full_team_names = reorder(full_team_names, -CF.percent.season))

ggplot(df.division, aes(season_game_number, CF.per, color = franchise_name, label = team)) +
        geom_hline(yintercept = 50) +
        geom_smooth(se = FALSE) +
        #geom_label_repel(data = filter(df.division, season_game_number == 80), aes(season_game_number, CF.per, color = franchise_name, label = team), force = 2) +
        facet_wrap(division~season) +
        xlab("Game Number") +
        ylab("Corsi For %") +
        guides(color = F) +
        theme_bw()

df.conference <- df %>%
        select(season, franchise_game_number, conference, franchise_name, CF.per)

ggplot(df.conference, aes(franchise_game_number, CF.per, color = franchise_name)) +
        geom_hline(yintercept = 50) +
        geom_smooth(se = FALSE, size = 2) +
        #geom_label_repel(data = filter(df.conference, franchise_game_number == 82), aes(franchise_game_number, CF.per, color = franchise_name, label = team), force = 2) +
        facet_wrap(~conference) +
        xlab("Game Number") +
        ylab("Corsi For %") +
        guides(color = F) +
        theme_bw()

