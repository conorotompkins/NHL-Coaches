library(tidyverse)
library(broom)
library(viridis)
library(ggtree)
library(curl)
library(ggrepel)
library(viridis)

setwd("C:/Users/conor/githubfolder/NHL-Coaches/Graphs")

#load custom theme
source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")
theme_set(theme_nhh())

set_caption <- "@Null_HHockey"

league_data <- read_csv("https://raw.githubusercontent.com/conorotompkins/NHL-Coaches/master/NHH_coach_data.csv")



x <- 82
lines <- c(x, x*2:7, ((x*7)+48), (((x*7)+48)+82), ((((x*7)+48)+82)+82))

seasons <- c("06-07",
             "07-08",
             "08-09",
             "09-10",
             "10-11",
             "11-12",
             "12-13",
             "13-14",
             "14-15",
             "15-16")

team_data <- league_data %>% 
        group_by(franchise_name) %>% 
        do(augment(loess(CF.per ~ team_game_number, span = .15, data = .), newdata = .))

ggplot(team_data, aes(team_game_number, .fitted)) +
        geom_hline(yintercept = 50, size = .25, alpha = I(1)) +
        geom_vline(xintercept = lines, size = .25, alpha = I(1)) +
        geom_ribbon(aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit), fill = head_coach_u), alpha = I(.5)) +
        geom_line(aes(color = head_coach_u), size = 2) +
        scale_x_continuous(breaks = lines, labels = seasons) +
        facet_wrap(~franchise_name, ncol = 6) +
        scale_color_viridis(discrete = TRUE) +
        scale_fill_viridis(discrete = TRUE) +
        coord_cartesian(ylim = c(40, 60)) +
        labs(y = "5v5 Shots For %", 
             x = "Season",
             title = "NHL Head Coaches Historical View, 2005-2016") +
        guides(fill = FALSE, color = FALSE) +
        theme(panel.grid.major = element_blank(), 
              axis.text.x = element_text(size = 10))
ggsave("NHL coaches big graph.png", height = 12, width = 45)


brewer.pal(n = 2, name = "Set2")

coach <- "Peter Deboer"
set_team <- "N.J"
df <- team_data %>%
        filter(team == set_team) %>%
        select(team, franchise_name, team_game_number, head_coach_u, CF.per, CF60, CA60) %>%
        gather(metric, measure, -c(team, franchise_name, team_game_number, head_coach_u))


df <- df %>% 
        group_by(team, metric) %>% 
        do(augment(loess(measure ~ team_game_number, span = .15, data = .), newdata = .)) %>%
        mutate(coach_alpha = ifelse(head_coach_u == coach, .2, .1),
               key = paste(head_coach_u, team))

df$metric[df$metric == "CF60"] <- "Shots For"
df$metric[df$metric == "CA60"] <- "Shots Against"

team_plot <- ggplot(filter(df, metric == "CF.per"), aes(team_game_number, .fitted)) +
        geom_hline(yintercept = 50, size = .25, alpha = I(1)) +
        geom_vline(xintercept = lines, size = .25, alpha = I(.5)) +
        geom_ribbon(aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit)), fill = "black", alpha = I(.4)) +
        geom_line(aes(alpha = coach_alpha), color = "black", size = 1) +
        scale_x_continuous(breaks = lines, labels = seasons) +
        coord_cartesian(ylim = c(40, 60)) +
        guides(fill = FALSE, color = FALSE, alpha = FALSE) +
        labs(y = "5v5 Shots For %", 
             x = NULL,
             title = unique(df$franchise_name)) +
        theme(panel.grid.major = element_blank(), 
              axis.text = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 8),
              title = element_text(size = 8),
              legend.position = "bottom")

coach_plot <- ggplot(filter(df, metric %in% c("Shots For", "Shots Against") & head_coach_u == coach), aes(team_game_number, .fitted)) +
        geom_vline(xintercept = lines, size = .25, alpha = I(1)) +
        geom_ribbon(aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit), fill = metric), alpha = I(.5)) +
        geom_line(aes(color = metric), size = 2) +
        coord_cartesian(xlim = c(max(df$team_game_number[df$head_coach_u == coach]),
                                 min(df$team_game_number[df$head_coach_u == coach])),
                        ylim = c(30, 70)) +
        facet_wrap(~key) +
        scale_x_continuous(breaks = lines, labels = seasons) +
        scale_color_viridis(discrete = TRUE) +
        scale_fill_viridis(discrete = TRUE) +
        labs(y = "5v5 Shots Per 60", 
             x = "Season",
             title = "NHL Head Coaches Historical View, 2005-2016",
             legend = "Shot Type",
             caption = set_caption) +
        theme(panel.grid.major = element_blank(), 
              axis.text.x = element_text(size = 16),
              legend.position = "top",
              legend.title = element_blank(),
              plot.caption = element_text(hjust = 1))

subview(coach_plot, team_plot, x = 541, y = 33, width = .5, height = .5)
ggsave(paste(coach, set_team, ".png"), width = 18, height = 9, dpi = 300)

#all teams in mone graph
team_data <- league_data %>% 
        filter(season == "20152016") %>%
        group_by(franchise_name) %>% 
        do(augment(loess(CF.per ~ team_game_number, span = .7, data = .), newdata = .))
        
set_team <- "PIT"
ggplot(team_data, aes(season_game_number, .fitted, color = franchise_name)) +
        geom_hline(yintercept = 50, size = .25, alpha = I(1)) +
        #geom_vline(xintercept = lines, size = .25, alpha = I(.5)) +
        geom_ribbon(data = filter(team_data, team == set_team), aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit), fill = franchise_name), alpha = I(.1)) +
        geom_line(aes(color = franchise_name, size = ifelse(team == set_team, 2, 1))) +
        geom_label_repel(data = filter(team_data, season_game_number == 82), aes(season_game_number, .fitted, label = team), alpha = 1, force = 2) +
        #scale_x_continuous(breaks = 1:82, labels = 1:82) +
        scale_color_viridis(discrete = TRUE) +
        scale_fill_viridis(discrete = TRUE) +
        coord_cartesian(ylim = c(40, 60)) +
        facet_wrap(~conference) +
        labs(y = "5v5 Shots For %", 
             x = "Game Number",
             title = team_data$season) +
        guides(fill = FALSE, color = FALSE) +
        theme(panel.grid.major = element_blank(), 
              axis.text.x = element_text(size = 10))
?geom_text_repel
?geom_label_repel

test <- league_data %>%
        select(team, season, season_game_number, date) %>%
        group_by(team, season) %>%
        count()
league_data %>%
        filter(team == "L.A" & season == "20052006") %>%
        View()