library(tidyverse)
library(broom)
library(viridis)
library(ggtree)
library(curl)

setwd("C:/Users/conor/githubfolder/NHL-Coaches/Graphs")

#load custom theme
source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")
theme_set(theme_nhh())

team_data <- read_csv("https://raw.githubusercontent.com/conorotompkins/NHL-Coaches/master/NHH_coach_data.csv")



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

team_data <- team_data %>% 
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

coach <- "Alain Vigneault"
set_team <- "VAN"
df <- team_data %>%
        filter(team == set_team) %>%
        select(team, franchise_name, team_game_number, head_coach_u, CF.per, CF60, CA60) %>%
        gather(metric, measure, -c(team, franchise_name, team_game_number, head_coach_u))


df <- df %>% 
        group_by(team, metric) %>% 
        do(augment(loess(measure ~ team_game_number, span = .15, data = .), newdata = .)) %>%
        mutate(coach_alpha = ifelse(head_coach_u == coach, .2, .1),
               key = paste(head_coach_u, team))

df$metric[df$metric == "CF60"] <- "Shots For Per 60"
df$metric[df$metric == "CA60"] <- "Shots Against Per 60"

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

coach_plot <- ggplot(filter(df, metric %in% c("Shots For Per 60", "Shots Against Per 60") & head_coach_u == coach), aes(team_game_number, .fitted)) +
        geom_vline(xintercept = lines, size = .25, alpha = I(1)) +
        geom_ribbon(aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit), fill = metric), alpha = I(.5)) +
        geom_line(aes(color = metric), size = 2) +
        coord_cartesian(xlim = c(max(df$team_game_number[df$head_coach_u == coach]),
                                 min(df$team_game_number[df$head_coach_u == coach])),
                        ylim = c(40, 60)) +
        facet_wrap(~key) +
        scale_x_continuous(breaks = lines, labels = seasons) +
        scale_color_viridis(discrete = TRUE) +
        scale_fill_viridis(discrete = TRUE) +
        labs(y = "5v5 Shots Per 60", 
             x = NULL,
             title = "NHL Head Coaches Historical View, 2005-2016",
             legend = "Shot Type") +
        theme(panel.grid.major = element_blank(), 
              axis.text.x = element_text(size = 16),
              legend.position = "top",
              legend.title = element_blank())

subview(coach_plot, team_plot, x = 550, y = 41, width = .3, height = .3)
ggsave(paste(coach, set_team, ".png"), width = 18, height = 9, dpi = 300)

?subview


ggplot(df) +
        geom_line(aes(team_game_number, .fitted, color = metric)) +
        geom_ribbon(aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit), fill = metric), alpha = I(.5)) +
        #geom_line(aes(team_game_number, .fitted, color = metric)) +
        facet_wrap(~team)
unique(df$metric)
