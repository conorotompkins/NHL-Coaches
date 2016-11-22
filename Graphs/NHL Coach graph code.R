library(ggthemes)
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(scales)
library(readr)
library(tidyr)
library(cowplot)

team_data <- read_csv("https://raw.githubusercontent.com/conorotompkins/NHL-Coaches/master/NHH_coach_data.csv")

#load custom theme
source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")

#create season lines
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

#set graph criteria
team <- "DET"
franchise <- "Detroit Red Wings"

DET_team <- team_data %>%
        filter(team == "DET")

#create statistical model
fit <- loess(DET_team$CF.per ~ DET_team$team_game_number, span = .15)

pred <- predict(fit, se=TRUE)

DET_team$fit <- pred$fit
DET_team$se.fit <- pred$se.fit
fit <- NULL

#create plot
det_plot <- ggplot(DET_team, aes(x = team_game_number, y = fit)) +
        geom_hline(yintercept = 50, size = .25, alpha = I(1)) +
        geom_vline(xintercept = lines, size = .25, alpha = I(1)) +
        geom_ribbon(aes(ymax = (fit + 1.96 * se.fit), ymin = (fit - 1.96 * se.fit), fill = head_coach_u), alpha = I(.5)) +
        geom_line(aes(color = head_coach_u), size = 2) +
        scale_x_continuous(breaks = lines, labels = seasons) +
        scale_color_discrete(name = paste(franchise, "Head Coaches"), 
                             breaks = unique(team_data$head_coach_u[team_data$franchise_name == franchise]),
                             labels = unique(team_data$head_coach_u[team_data$franchise_name == franchise])) +
        coord_cartesian(ylim = c(40, 60)) +
        labs(y = "5v5 Shots For %", 
             x = "Season",
             title = "NHL Head Coaches Historical View, 2005-2016") +
        guides(fill = FALSE) +
        theme_nhh() +
        theme(panel.grid = element_blank(), 
              legend.position = "top")
print(det_plot)

#save plot
ggsave(paste(franchise, "coach graph.png"), width = 15, height = 9)
