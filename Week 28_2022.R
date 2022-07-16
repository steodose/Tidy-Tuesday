##### Tidy Tuesday 2022 - Week 28 #####
#### By: Stephan Teodosescu ####

library(tidyverse)
library(ggbump)
library(patchwork)

# Repo link: https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-12

## Load data
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv') %>% 
    janitor::clean_names()


## Create custom themes

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Define an aspect ratio to use throughout. This value is the golden ratio which provides a wider than tall rectangle
asp_ratio <- 1.618 

# Function for plot with logo generation
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
    
    # Requires magick R Package https://github.com/ropensci/magick
    
    # Useful error message for logo position
    if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
        stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
    }
    
    # read in raw images
    plot <- magick::image_read(plot_path)
    logo_raw <- magick::image_read(logo_path)
    
    # get dimensions of plot for scaling
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    # default scale to 1/10th width of plot
    # Can change with logo_scale
    logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
    
    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height
    
    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding
    
    if (logo_position == "top right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "top left") {
        x_pos = 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "bottom right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    } else if (logo_position == "bottom left") {
        x_pos = 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    }
    
    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
    
}


# Define colors to be used in viz

country_colors <- c(
    'United Kingdom' = "#012169",
    'Germany' = "#DD0000",
    'Spain' = "#F1BF00",
    'France' = "#002654",
    'Italy' = "#008C45", 
    'Türkiye' = "#C8102E",
    'Norway' = "#BA0C2F",
    'Netherlands' = "#F36C21",
    'Portugal' = "#046A38",
    'Switzerland' = "#DA291C",
    'Poland' = "#DC143C",
    'Greece' = "#001489",
)

description_color <- 'grey40'

## Data preparation

country_flights_by_year <- flights %>% 
    select(year, state = state_name, flights = flt_tot_1) %>% 
    group_by(year, state) %>% 
    summarise(flights = sum(flights))

country_rank_by_year <- country_flights_by_year %>% 
    group_by(year) %>% 
    mutate(
        rank = row_number(desc(flights))
    ) %>% 
    ungroup() %>% 
    arrange(rank, year) 

max_rank <- 12

todays_top <- country_rank_by_year %>% 
    filter(year == 2022, rank <= max_rank) %>% 
    pull(state)



## Make bump plot

country_rank_by_year %>% 
    filter(state %in% todays_top) %>% 
    ggplot(aes(year, rank, col = state)) + 
    geom_point(size = 2) +
    geom_bump(size = 1) +
    geom_text(
        data = country_rank_by_year %>% 
            filter(year == 2016, state %in% todays_top),
        aes(label = state),
        hjust = 1,
        nudge_x = -0.1,
        fontface = 'bold',
        family = 'Outfit'
    ) +
    geom_text(
        data = country_rank_by_year %>% 
            filter(year == 2022, state %in% todays_top),
        aes(label = rank),
        hjust = 0,
        nudge_x = 0.1,
        size = 5,
        fontface = 'bold',
        family = 'Outfit'
    ) +
    annotate(
        'text',
        x = c(2016, 2022),
        y = c(0.25, 0.25),
        label = c(2016, 2022),
        hjust = c(0, 1),
        vjust = 1,
        size = 4,
        fontface = 'bold',
        family = 'Outfit'
    ) +
    scale_y_reverse(position = 'right', breaks = seq(16, 2, -2)) +
    scale_color_manual(values = country_colors) +
    coord_cartesian(xlim = c(2014, 2022.5), ylim = c(10.5, 0.25), expand = F) +
    theme_custom() +
    theme(
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(
            margin = margin(t = 3, b = 2, unit = 'mm'),
            hjust = 0.5
        ),
        plot.title = element_text(
            face = 'bold',
            size = 20,
            hjust = 0.5
        )
    ) +
    labs(x = "",
         y = "",
        title = 'Top 10 European Countries for Flights',
        subtitle = 'Ranked by number of incoming and outgoing flights from 2016-2022. 2022 data thru May.',
        caption = "TidyTuesday 2022 - Week 28 | Source: Eurocontrol | Plot: @steodosescu"
    )


ggsave("Flight Bumps Chart.png")


# Add logo to plot
flight_bumps_chart_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tidy-Tuesday/Flight Bumps Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Tidy-Tuesday/european-union.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(flight_bumps_chart_with_logo, "Flight Bumps Chart with Logo.png")



## Build small multiples chart

# prepare data
flights_grouped <- flights %>% 
    select(year, month_num, month_mon, state = state_name, flights = flt_tot_1) %>% 
    group_by(year, month_num, state) %>% 
    summarise(flights = mean(flights))
