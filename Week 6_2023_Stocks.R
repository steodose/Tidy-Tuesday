##### Big Tech Stocks #####
##### By: Stephan Teodosescu #####
##### February 2023 #####

library(tidyverse)
library(ggtext)
library(ggimage)
library(ggstream)
library(scales)
library(patchwork)
library(grid)
library(lubridate)
library(gganimate)
library(here)

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


## 1. ----------------- Big Tech Facet Plot ---------------------

# Main plot: company facet

big_tech_plot <- ggplot() +
     geom_line(data = big_tech_stock_prices, aes(date, adj_close, color = clr, group = stock_symbol), size = .75) +
     #geom_line(data = mutate(big_tech_stock_prices, stock_symbol = NULL), aes(date, adj_close, color = 'grey', alpha = 0.5, group = duplicate), size = .75) +
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
         subtitle = "Evolution of select technology stock prices between 2010 and 2022"
     ) +
     scale_color_identity() +
     scale_fill_identity() +
     coord_cartesian(clip = "off", expand = F) +
     facet_wrap(vars(stock_symbol), scales = "free", nrow = 5) +
     #facet_wrap(~fct_reorder(stock_symbol, market_cap), scales = "free", nrow = 5) +
     theme_custom() +
     theme(
         plot.title = element_text(color = "black", face = "bold", family = "Outfit", hjust = 0.5, size = rel(3.0), margin = margin(b = .5, unit = "cm")),
         plot.subtitle = element_text(color = "#444444", family = "Outfit", hjust = 0.5, margin = margin(b = 1, unit = "cm")),
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

# isolate colors
big_six_colors <- big_tech_companies %>%
    filter(lower_stock %in% c('aapl', 'msft', 'googl', 'amzn', 'nvda', 'nflx'))

# make stream plot

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
          legend.title = element_blank(),
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
         y = "Close Price * Volume ($)", 
         title = "Big Tech Trading Volumes", 
         subtitle = "Dollar volumes of six of the US's biggest tech stocks have increased overall, but are in decline recently.",
         caption = "Data: Yahoo Finance/Evan Gower | Graphic: @steodosescu")

dollar_volume_plot

ggsave("2023/W6_Big Tech Dollar Volumes.png", device = ragg::agg_png, dpi = 300)


## Combine plots with patchwork
big_tech_plot / dollar_volume_plot +
    theme(
        panel.background = element_rect(fill= "floralwhite", color=NA),
        plot.background = element_rect(color = "floralwhite", fill = "floralwhite"),
        plot.margin = margin(1, 0.5, 0.5, 0.5, unit = "line")
    )



ggsave("2023/W6_Big Tech Patchwork.png", device = ragg::agg_png, dpi = 300, width = 8, height = 16)


## 3. -------- Animated stock prices -----------------

# inspiration from Ansgar Wolsing: https://github.com/bydata/tidytuesday/blob/main/2023/06/eda.R

start_date <- as_date("2013-01-01")
max_date <- as_date("2022-12-29")

pal <- c("#A2AAAD", "#FF0000", "#FF9900", "#1798c1", "#00BCEB", "#34A853", 
         "#0530AD", "#0068B5", "#0668E1", "#F14F21", "#E50914", "#76B900", "#C74634", "#E31937")

df_plot <- big_tech_stock_prices %>% 
    filter(date >= start_date) %>% 
    group_by(stock_symbol) %>% 
    mutate(adj_close_rel = 100 * adj_close / .$adj_close[.$stock_symbol == stock_symbol & .$date == min(.$date)]) %>% 
    # filter(date == max_date) %>% 
    ungroup()  %>% 
    # quick & dirty formatting of company names
    mutate(company2 = str_extract(company, "[^\\s,\\.]+"),
           company2 = ifelse(stock_symbol == "IBM", "IBM", company2),
           company2 = fct_reorder(company2, adj_close_rel),
           company_label = glue::glue("<img src='{company_logo_path}' width='30'/>")) %>% 
    select(stock_symbol, company = company2, company_logo_path, company_label, date, adj_close_rel)

# test it
df_plot %>% 
    filter(date == max_date) %>% 
    ggplot(aes(company, adj_close_rel)) +
    geom_col(aes(fill = big_tech_companies$clr)) +
    geom_richtext(aes(label = company_label)) +
    scale_fill_identity()+
    coord_flip()


bar_offset <- 0.35
df_plot_anim <- df_plot %>% 
    # filter(date >= as_date("2018-01-01")) %>% 
    filter(date <= max_date) %>% 
    mutate(month = floor_date(date, "1 month")) %>% 
    arrange(date) %>% 
    # calculate summary statistic per month
    group_by(company, company_label, month) %>% 
    summarize(adj_close_rel_first = first(adj_close_rel),
              adj_close_rel_last = last(adj_close_rel),
              adj_close_rel_avg = mean(adj_close_rel), .groups = "drop") %>% 
    group_by(month) %>% 
    mutate(rank = rank(-adj_close_rel_first), ties.method = "first") %>% 
    ungroup() 

# join in colors
big_tech_companies2 <- big_tech_companies %>%
    mutate(company2 = str_extract(company, "[^\\s,\\.]+"),
           company2 = ifelse(stock_symbol == "IBM", "IBM", company2))

df_plot_anim <- df_plot_anim %>%
    left_join(big_tech_companies2, by = c("company" = "company2"))


p <- df_plot_anim %>% 
    # filter(month == max(month)) %>% 
    ggplot(aes(adj_close_rel_last, rank)) +
    # use geom_rect instead of geom_col for smooth transitions of the bars
    geom_rect(
        aes(ymin = rank - bar_offset, ymax = rank + bar_offset,
            xmin = 0, xmax = adj_close_rel_last,
            fill = clr)
    ) +
    # company icons
    geom_richtext(
        aes(x = -6, label = company_label, hjust = 1),
        label.size = 0, fill = NA
    ) +
    # stock value
    geom_text(
        aes(label = paste("$", round(adj_close_rel_last))),
        size = 3, family = "Outfit", hjust = -0.15) +
    scale_x_continuous() +
    scale_y_reverse() +
    scale_fill_identity() +
    coord_cartesian(clip = "off") +
    #guides(fill = "none", color = "none") +
    labs(
        title = "What's it Worth Today?",
        subtitle = "...if you had invested $100 in one of these Tech stocks 10 years ago
    <br>
    <br><span style='font-size: 16pt; color: #888888; font-family: Outfit'>
    {format(frame_time, '%Y (%B)')}</span>",
        x = "Close Price",
        y = "",
        caption = "Closing price of the first day of a month after adjustments 
    for all applicable splits and dividends shown. |
    **Source:** Yahoo Finance | 
    **Graphic:** @steodosescu"
    )  +
    theme_custom() +
    theme(
        plot.margin = margin(t = 2, b = 2, l = 18, r = 18),
        plot.title = element_text(size = 20, family = "Outfit", face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(lineheight = 1.1),
        plot.caption = element_textbox(size = 7, width = 1, lineheight = 1.1,
                                       margin = margin(t = 4, b = 8))
    )


# Animate
p_anim <- p + 
    transition_time(month) +
    view_follow(fixed_x = FALSE, fixed_y = TRUE)

# this part takes a while
fps <- 12
animate(p_anim, ref_frame = 1, nframes = length(unique(df_plot_anim$month)) * 5,
        end_pause = 2 * fps,
        res = 150, width = 800, height = 800, fps = fps)

anim_save("hindsight-bias.gif")


