library(tidyverse)
library(broom)
library(viridis)
library(ggforce)
library(ggtree)

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

df <- team_data %>%
        filter(team == "PIT") %>%
        select(team, franchise_name, team_game_number, head_coach_u, CF.per, CF60, CA60) %>%
        gather(metric, measure, -c(team, franchise_name, team_game_number, head_coach_u))


df <- df %>% 
        group_by(team, metric) %>% 
        do(augment(loess(measure ~ team_game_number, span = .15, data = .), newdata = .)) %>%
        mutate(coach_alpha = ifelse(head_coach_u == coach, .2, .1),
               key = paste(head_coach_u, team))


team_plot <- ggplot(filter(df, metric == "CF.per"), aes(team_game_number, .fitted)) +
        geom_hline(yintercept = 50, size = .25, alpha = I(1)) +
        geom_vline(xintercept = lines, size = .25, alpha = I(1)) +
        geom_ribbon(aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit), fill = head_coach_u), alpha = I(.4)) +
        geom_line(aes(color = head_coach_u, alpha = coach_alpha), size = 2) +
        scale_x_continuous(breaks = lines, labels = seasons) +
        scale_color_viridis(discrete = TRUE) +
        scale_fill_viridis(discrete = TRUE) +
        coord_cartesian(ylim = c(40, 60)) +
        guides(fill = FALSE, color = FALSE, alpha = FALSE) +
        labs(y = "5v5 Shots For %", 
             x = NULL,
             title = unique(df$franchise_name)) +
        theme(panel.grid.major = element_blank(), 
              axis.text = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 8),
              legend.position = "bottom")
coach <- "Dan Bylsma"

max(df$team_game_number[df$head_coach_u == coach])
min(df$team_game_number[df$head_coach_u == coach])

coach_plot <- ggplot(filter(df, metric %in% c("CF60", "CA60") & head_coach_u == "Dan Bylsma"), aes(team_game_number, .fitted)) +
        geom_hline(yintercept = 50, size = .25, alpha = I(1)) +
        geom_vline(xintercept = lines, size = .25, alpha = I(1)) +
        geom_ribbon(aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit), fill = metric), alpha = I(.5)) +
        geom_line(aes(color = metric), size = 2) +
        coord_cartesian(xlim = c(703, 304), ylim = c(40, 60)) +
        facet_wrap(~key) +
        scale_x_continuous(breaks = lines, labels = seasons) +
        labs(y = "5v5 Shots For %", 
             x = "Season",
             title = "NHL Head Coaches Historical View, 2005-2016") +
        theme(panel.grid.major = element_blank(), 
              axis.text.x = element_text(size = 10))

subview(coach_plot, team_plot, x = 650, y = 42, width = .3, height = .3)
ggsave("subview_test.png")

?subview


ggplot(df) +
        geom_line(aes(team_game_number, .fitted, color = metric)) +
        geom_ribbon(aes(ymax = (.fitted + 1.96 * .se.fit), ymin = (.fitted - 1.96 * .se.fit), fill = metric), alpha = I(.5)) +
        #geom_line(aes(team_game_number, .fitted, color = metric)) +
        facet_wrap(~team)
unique(df$metric)
