library(tidyverse)
library(broom)
library(viridis)

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

df <- team_data %>%
        filter(team %in% c("DET", "PIT"))
#the problem is that the model thinks the data is 1732 rows long, and the dplyr pipe splits the data into two ~830 row sets
model <- loess(CF.per ~ team_game_number, data = df, span = .15)



?augment

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
