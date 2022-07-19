##### Tidy Tuesday 2022 - Week 28 #####
#### By: Stephan Teodosescu ####

library(tidyverse)
library(ggbump)
library(patchwork)
library(magick)
library(cowplot)
library(glue)
library(ggtext)
library(ggimage) #for working with logos
library(janitor)
library(lubridate)
library(zoo)
library(scales)
library(gt)


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

# Table theme
gt_theme_538 <- function(data,...) {
    data %>%
        opt_table_font(
            font = list(
                google_font("Outfit"),
                default_fonts()
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = TRUE,
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        )  %>% 
        tab_options(
            column_labels.background.color = "white",
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        ) 
}


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
    'Turkey' = "#C8102E",
    'Norway' = "#BA0C2F",
    'Netherlands' = "#F36C21",
    'Portugal' = "#046A38",
    'Switzerland' = "#DA291C",
    'Poland' = "#DC143C",
    'Greece' = "#001489"
)

description_color <- 'grey40'

## Data preparation

# change Turkey's name as the formatting doesn't play nice with code
flights <- flights %>%
    mutate(state_name = case_when(
        state_name == 'Türkiye' ~ 'Turkey',
                           TRUE ~ state_name))


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



## 1. ------------- Make bump plot ------------------

flights_bump_chart <- country_rank_by_year %>% 
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
    coord_cartesian(xlim = c(2014, 2022.5), ylim = c(13.5, 0.25), expand = F) +
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
        title = 'Top 12 European Countries for Flights',
        subtitle = 'Ranked by no. of departing and arriving flights from 2016-2022. 2022 data thru May.'
    )

flights_bump_chart

ggsave("Flight Bumps Chart.png")



## 2. -------------- Build small multiples chart ----------------

# prepare data
flights_grouped <- flights %>% 
    select(year, month_num, month_mon, state = state_name, flights = flt_tot_1) %>% 
    group_by(year, month_num, month_mon, state) %>% 
    summarise(flights = sum(flights))

# use zoo package to create year-month date object for plotting
flights_grouped$date <- as.yearmon(paste(flights_grouped$year, flights_grouped$month_num), "%Y %m")

# set up duplicate country column for charting purposes 
flights_grouped$stateDuplicate <- flights_grouped$state


# Make flag colors and logo df

flag_colors <- tibble(
    state = c('United Kingdom','Germany', 'Spain', 'France', 'Italy', 'Turkey', 
              'Netherlands', 'Norway', 'Switzerland', 'Greece', 'Portugal', 'Poland'),
    state_abbr = c('UK', 'DE', 'ES', 'FR', 'IT', 'TK', 'NL', 'NO', 'CH', 'GR', 'PL', 'PO'),
    color = c("#012169","#DD0000","#F1BF00","#002654","#008C45","#C8102E","#F36C21",
                   "#BA0C2F","#DA291C","#001489","#046A38","#DC143C")
    )

flag_colors <- flag_colors %>% 
    mutate('flag_logo' = paste0('https://raw.githubusercontent.com/steodose/Tidy-Tuesday/master/flags/', state, '.png'))

# change UK flag logo path to account for the space in United Kingdom
flag_colors <- flag_colors %>% 
    mutate(flag_logo = case_when(
        flag_logo == 'https://raw.githubusercontent.com/steodose/Tidy-Tuesday/master/flags/United Kingdom.png' ~ 'https://raw.githubusercontent.com/steodose/Tidy-Tuesday/master/flags/United%20Kingdom.png',
        TRUE ~ flag_logo))

# filter for 12 countries and join flag icons
flights_grouped2 <- flights_grouped %>% 
    filter(state %in% todays_top)

flights_grouped2 <- flights_grouped2 %>% 
    left_join(flag_colors, by = 'state')


# make chart
p1 <- flights_grouped2 %>% 
    ggplot(aes(x = date, y = flights)) + 
    #geom_smooth(data = mutate(team_games2, home = NULL), aes(group = teamDuplicate), method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE, colour = 'grey80', size = .25, alpha = .5) +
    #geom_smooth(aes(group = team, color = team_color1), method = "lm",  formula = y ~ splines::bs(x, 5), se = FALSE, size = .5, alpha = 1, show.legend = FALSE) +
    geom_line(data = mutate(flights_grouped2, state = NULL), aes(group = stateDuplicate), colour = 'grey80', size = .25, alpha = .5) +
    geom_line(aes(group = state, color = flights_grouped2$color), size = .5, alpha = 1, show.legend = FALSE) +
    scale_color_identity() +
    facet_wrap(~fct_reorder(state, -flights)) +
    theme_custom() + 
    theme(plot.title.position = 'plot', 
          plot.title = element_text(
              face = 'bold',
              size = 20,
              hjust = 0.5
          ),
          plot.subtitle = element_text(
              margin = margin(t = 3, b = 2, unit = 'mm'),
              hjust = 0.5
          ),
          plot.margin = margin(10, 10, 15, 10),
          axis.text.x=element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    scale_y_continuous(
        labels = scales::comma) +
    labs(x = "", 
         y = "Total IFR Movements", 
         title = "Monthly Seasonality of Flying",
         subtitle = "Ordered by total number of departing and arriving flights from Jan 2016 thru May 2022.",
         caption = "TidyTuesday 2022 - Week 28 | Source: Eurocontrol | Plot: @steodosescu") +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

p1

ggsave("Flights Facet Chart.png")

# add logos to each facet 

## Reference: https://github.com/tonyelhabr/sports_viz/blob/master/42-202122_new_players/01-main.R
p_bld <- ggplot_gtable(ggplot_build(p1))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
    p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
    id <- facet_id[i]
    url <-
        flag_colors %>% filter(state == !!id) %>% pull(flag_logo)
    lab <-
        grid::textGrob(
            id,
            x = unit(0, 'npc'),
            gp = grid::gpar(
                col = 'black',
                fontfamily = 'Outfit',
                fontface = 'bold',
                fontsize = 8
            ),
            hjust = 0
        )
    img <-
        grid::rasterGrob(
            image = magick::image_read(url),
            # just = 'right',
            hjust = 1,
            x = unit(1, 'npc'),
            ## 1 and 0.75 is also fine
            vp = grid::viewport(height = 1, width = 0.75)
        )
    tot_tree <- grid::grobTree(lab, img)
    p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}

p1 <- cowplot::ggdraw(p_bld)

ggsave("Flights Facet Chart.png", p1, w = 6, h = 6, dpi = 300)



## Combine plots with patchwork
flights_bump_chart/p1 +
    theme(
        plot.background = element_rect(color = "floralwhite", fill = "floralwhite"),
        plot.margin = margin(1, 0.5, 0.5, 0.5, unit = "line")
        )

ggsave("European Flights Patchwork.png", width = 8, height = 10)

# Add logo to plot
flight_bumps_chart_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tidy-Tuesday/European Flights Patchwork.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Tidy-Tuesday/europe.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(flight_bumps_chart_with_logo, "European Flights Patchwork with Logo.png")


## 3. -------------- Monthly Rankings Table ----------------

flights_grouped3 <- flights_grouped %>% 
    filter(year <= 2019) %>% 
    group_by(month_mon) %>% 
    summarise(flights = sum(flights)) %>% 
    arrange(desc(flights)) %>%
    mutate(flights_per_year = flights/4,
           rank = row_number()) %>% 
    relocate(rank)

# compute total flights in timeframe
total_flights <- sum(flights_grouped3$flights_per_year)

# make table
flights_grouped3 %>% 
    mutate(percent_total = flights_per_year/total_flights) %>% 
    gt() %>%
    cols_label(
        rank = "Rank",
        month_mon = "Month",
        flights = "Total Flights",
        flights_per_year = "Flights per Year",
        percent_total = "Yearly Share"
    ) %>% 
    data_color(
        columns = 4, 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::deep-orange_material"
            ) %>% as.character(),
            domain = NULL
        )) %>%
    cols_width(
        rank ~ px(60)
    ) %>% 
    cols_width(
        month_mon ~ px(50)
    ) %>% 
    fmt_number(columns = vars(flights,flights_per_year), decimals = 0, sep_mark = ",") %>%
    fmt_percent(columns = vars(percent_total)) %>% 
    tab_options(
        table.background.color = "floralwhite",
        table.font.names = "Outfit", 
        table.font.color = 'black',
        table.border.top.color = "transparent",
        table.width = 450,
    ) %>%
    tab_header(title = md("**Monthly Flights in Europe**"),
               subtitle ="Analysis limited to pre COVID-19 pandemic timeframe (2016 thru 2019) to get a cleaner view of monthly travel trends.") %>%
    tab_source_note(
        source_note = md("Data: Eurocontrol<br>Table: @steodosescu")) %>% 
    tab_footnote(
        footnote = "IFR air transport movements are calculated as the sum of take offs and landings performed under instrument flight rules. It's used as a proxy for how many flights are taking place in a given country.",
        locations = cells_column_labels(vars(flights))
    ) %>% 
    gtsave("Monthly European Flights Table.png")


# Add logo to table
flights_table_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tidy-Tuesday/Monthly European Flights Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Tidy-Tuesday/europe.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(flights_table_with_logo, "Monthly European Flights Table with Logo.png")
