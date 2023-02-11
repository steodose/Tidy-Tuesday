##### Big Tech Stocks #####
##### By: Stephan Teodosescu #####
##### February 2023 #####

library(tidyverse)
library(ggtext)
library(ggimage)
library(ggstream)
library(scales)
library(patchwork)

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}


# create aspect ration to use throughout
asp_ratio <- 1.618


## --------------- Data Wrangling ---------------------------
big_tech_companies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv")

# Assign companies colors
big_tech_companies$clr <- c("#A2AAAD", "#FF0000", "#FF9900", "#1798c1", "#00BCEB", "#34A853", "#0530AD", "#0068B5", "#0668E1", "#F14F21", "#E50914", "#76B900", "#C74634", "#E31937")

# Companies logos (downloades in folder Logos)
big_tech_companies <- big_tech_companies |>
    mutate(
        lower_stock = tolower(stock_symbol),
        company_logo_path = paste0(here::here("Logos/"), lower_stock, ".png")
    )

big_tech_stock_prices <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv")

big_tech_stock_prices <- big_tech_stock_prices |>
    left_join(big_tech_companies) |> 
    mutate(market_cap = adj_close*volume) |> 
    mutate(duplicate = stock_symbol)

# Get company max closing value and min date
companies_logos_df <- big_tech_stock_prices |>
    group_by(stock_symbol, company_logo_path) |>
    summarise(
        first_date = min(date),
        max_adj_close = max(adj_close)
    ) |>
    ungroup()


## 1. ----------------- Big Tech Facet ---------------------

# Main plot: company facet

big_tech_plot <- ggplot() +
     geom_line(data = big_tech_stock_prices, aes(date, adj_close, color = clr, group = stock_symbol), size = .75) +
     #geom_line(data = big_tech_stock_prices, aes(date, adj_close, color = 'grey', alpha = 0.5, group = duplicate), size = .75) +
     geom_ribbon(data = big_tech_stock_prices, aes(date, ymin = 0, ymax = adj_close, fill = clr), alpha = .6) +
     geom_richtext(
         data = companies_logos_df, aes(x = first_date, y = max_adj_close, label = glue::glue("<img src='{company_logo_path}' width='35'/>")),
         hjust = 0,
         fill = NA,
         label.padding = unit(.125, "lines"),
         label.r = unit(0, "lines"),
         label.size = unit(0, "lines")
     ) +
     labs(
         title = "Big Tech has Gotten Bigger",
         subtitle = "Evolution of select technology stock prices between 2010 and 2022",
         caption = "Data: Yahoo Finance/Evan Gower | Graphic: @steodosescu"
     ) +
     scale_color_identity() +
     scale_fill_identity() +
     coord_cartesian(clip = "off", expand = F) +
     facet_wrap(vars(stock_symbol), scales = "free", nrow = 5) +
     #facet_wrap(~fct_reorder(stock_symbol, market_cap), scales = "free", nrow = 5) +
     theme_custom() +
     theme(
         plot.title = element_text(color = "black", face = "bold", family = "Outfit", hjust = 0.5, size = rel(3.0), margin = margin(b = .5, unit = "cm")),
         plot.subtitle = element_text(color = "#444444", family = "Outfit", hjust = 0.5, size = rel(1.2), margin = margin(b = 1, unit = "cm")),
         strip.text = element_blank(),
         panel.spacing.x = unit(1, "cm"),
         panel.spacing.y = unit(1.5, "cm"),
         panel.grid = element_blank(),
         axis.title = element_blank(),
         axis.line.x = element_line(),
         axis.text.x = element_text(color = "black", family = "Outfit", size = rel(1.0)),
         axis.text.y = element_blank(),
         axis.ticks.length = unit(.125, "cm"),
         axis.ticks.x = element_line(size = .25)
     )

big_tech_plot


ggsave("2023/W6_Big Tech Stocks.png", width = 11.5, height = 10.5, device = ragg::agg_png, dpi = 300)



## 2. ------------------ Dollar Volumes Stream Plot ---------------------

big_six_colors <- big_tech_companies %>%
    filter(lower_stock %in% c('aapl', 'msft', 'googl', 'amzn', 'nvda', 'nflx'))

dollar_volume_plot <- big_tech_stock_prices %>%
    filter(lower_stock %in% c('aapl', 'msft', 'googl', 'amzn', 'nvda', 'nflx')) %>%
    ggplot(aes(x = date, y = market_cap, fill = stock_symbol)) +
    geom_stream(type = 'ridge', color = 'black', alpha = .95, n_grid = 4900) + # n_grid needs to be at least 2% more than the number of observations per group 
    scale_fill_manual(values = big_six_colors$clr) +
    #scale_y_continuous(labels = scales::dollar())+
    scale_y_continuous(labels = label_number(accuracy=0.1, suffix = "B", scale = 1e-9)) + 
    scale_x_continuous(breaks = seq(2010, 2022, 1)) +
    coord_cartesian(clip = 'off') +
    theme_custom() +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(size = 10, 
                                     vjust = 2.75, 
                                     hjust = 1.5),
          panel.grid.major.x = element_blank(), 
          plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold',
                                    hjust = 0.5,
                                    size = 20), 
          plot.subtitle = element_text(hjust = 0.5),
          plot.margin = margin(10, 10, 15, 10)) +
    labs(x = "", 
         y = "Dollar Volume ($)", 
         title = "Big Tech Trading Volumes", 
         subtitle = "Dollar volume (Price * Volume) of six of the US's biggest tech companies have been declining in recent months.")

dollar_volume_plot

ggsave("2023/W6_Big Tech Dollar Volumes.png", device = ragg::agg_png, dpi = 300)


## Combine plots with patchwork
big_tech_plot / dollar_volume_plot +
    theme(
        plot.background = element_rect(color = "floralwhite", fill = "floralwhite"),
        plot.margin = margin(1, 0.5, 0.5, 0.5, unit = "line")
    )

ggsave("2023/W6_Big Tech Patchwork.png", device = ragg::agg_png, dpi = 300, width = 8, height = 16)


