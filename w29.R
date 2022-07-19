##### Tidy Tuesday 2022 - Week 29 #####
#### By: José Miguel Cerda ####


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggbump)
library(Roboto)
library(glue)
library(ggtext)

# Import data ---------------------------------------------------------------

technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

# Plot --------------------------------------------------------------------

my_colours <- c("#842931",
                "#E63A42",
                "#F78C4A",
                "#FFDE52",
                "#C59C21",
                "#845A08",
                "#6B3A10", 
                "#A56342",
                "#E68D5A",
                "#19427B", 
                "#3A73A5", 
                "#737384",
                "#BDC5DE")

font <- "Roboto"

subt <- glue(
  "Immunization rates in Chile. Percentage of children (less than or equal 
  to one years old) as the target population, between 1980 and 2020.")

(bump_chart <- technology %>% 
    filter(iso3c == "CHL" & category == "Vaccines") %>%
    filter(str_detect(label, "%")) %>%
    ggplot(aes(year, value, col = variable)) +
    geom_point(size = 1.2) +
    geom_bump(size = 0.6) +
  theme_classic() +
  theme(text = element_text(family = font),
        legend.title = element_text(size = 11, colour = "black", family = font),
        legend.text = element_text(size = 10, colour = "black", family = font),
        axis.text = element_text(size = 10, colour = "black", face = "bold"), 
        axis.title = element_text(size = 12, colour = "black", face = "bold"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(size = 1, colour = "#8EFF0B"),
        plot.title = element_text(size = 12, face = "bold", colour = "black"),
        plot.background = element_rect(fill = "#DDFFB6", color = "#DDFFB6"),
        legend.background = element_rect(fill = "#DDFFB6", color = "#DDFFB6"),
        panel.background = element_rect(fill = "#DDFFB6", colour = "#DDFFB6")) +
  scale_y_continuous(breaks=seq(0,100,by=10)) +
  scale_x_continuous(breaks=seq(1980,2020,by=10)) +
  scale_colour_manual(name = "Vaccine", values = my_colours) +
  labs(x = "Year", y = "Percent of children who received a vaccine", title = "Technology Adoption: Vaccines in Chile 1980-2020",
       subtitle = subt))
  
ggsave("C:/Users/Lenovo/Desktop/Trabajo/TidyTuesday/w29/w29_bump.PNG", bump_chart, w = 6, h = 6, dpi = 300)


