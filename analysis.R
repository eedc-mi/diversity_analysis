library(tidyverse)
library(scales)

file_path <- file.path(
  "C:",
  "Users",
  "PReid",
  "Edmonton Economic Development Corporation",
  "Market Intelligence - Project Files",
  "Corporate",
  "Strategy & Policy",
  "Diversification",
  "data"
)

data_gdp <- read_csv(file.path(file_path, "data_gdp.csv"))
data_emp <- read_csv(file.path(file_path, "data_emp.csv"))
major_cmas <- c("Edmonton", "Calgary", "Ottawa-Gatineau", "Montreal", "Toronto", "Vancouver")

clean_data <- function(tbl_df) {
  tbl_df <- tbl_df[-1, ]
  
  tbl_df <- tbl_df %>%
    rename(year = "Description:") %>%
    gather(key = "desc", value = "val", -year) %>%
    mutate(year = as.numeric(year), val = as.numeric(val)) %>%
    filter(
      str_detect(desc, "by Industry"),
      ! str_detect(desc, "Goods Producing"),
      ! str_detect(desc, "Service-Producing"),
      ! str_detect(desc, "Services-Producing")) %>%
    mutate(
      city = str_match(desc, " - ([\\w ,-]+) \\(")[, 2],
      city = str_match(city, " - ([\\w -]+)")[, 2],
      industry = str_match(desc, " - ([-\\w,\\s]+) - ")[, 2],
      industry = str_replace_all(industry, "-", ","),
      industry = case_when(
        str_detect(industry, "Finance") ~ "FIRE",
        TRUE ~ industry),
      city = fct_relevel(city, "Edmonton")) %>%
    select(-desc) %>%
    group_by(year, city) %>%
    mutate(share = val / sum(val))
  
  tbl_df %>%
    filter(city != "Victoria") %>%
    group_by(city, industry) %>%
    mutate(growth_yoy = c(NA, diff(log(val))))
}

to_herf_plots <- function(tbl_df) {
  tbl_df %>%
    group_by(city, year) %>%
    summarise(herfindahl = 1 / sum(share ** 2))
}

calc_portfolio_var <- function(tbl_df) {
  cov_matrix <- tbl_df %>%
    select(-val, -share) %>%
    spread(industry, growth_yoy) %>%
    ungroup() %>%
    select(-year, -city) %>%
    cov() %>%
    as.matrix()
  
  weights <- tbl_df %>%
    group_by(city, industry) %>%
    summarise(val = sum(val)) %>%
    mutate(share = val / sum(val)) %>%
    ungroup() %>%
    pull(share)
  
  t(weights) %*% cov_matrix %*% weights
}

make_var_df <- function(tbl_df, start_year, end_year) {
  tbl_df %>%
    filter(year > start_year, year <= end_year) %>%
    group_by(city) %>%
    do(as_tibble(calc_portfolio_var(.) * 100)) %>%
    rename(var = V1)
}

to_var_plots <- function(tbl_df) {
  bind_cols(
    make_var_df(tbl_df, 1987, 2017),
    make_var_df(tbl_df, 1987, 1997),
    make_var_df(tbl_df, 1997, 2007),
    make_var_df(tbl_df, 2007, 2017)) %>%
    select(
      city, 
      t87_17 = var, 
      t87_97 = var1,
      t97_07 = var2,
      t07_17 = var3) %>%
    gather("time", "var", -city) %>%
    mutate(time = fct_relevel(time, "t87_97", "t97_07", "t07_17"))
}

theme_eedc <- 
  theme_grey() + 
  theme(
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(),
    axis.line=element_line(color="black"),
    axis.ticks=element_blank(),
    panel.background=element_blank(), 
    legend.key=element_blank(),
    strip.background=element_blank())

plot_wrapper <- function(plot, title_pos = 4, caption_pos = 9) {
  plot <- plot + theme_eedc
  
  grob <- ggplotGrob(plot)
  grob$layout$l[
    grob$layout$name == "title" | grob$layout$name == "subtitle"] <- title_pos
  grob$layout$r[grob$layout$name == "caption"] <- caption_pos
  #grid::grid.draw(grob)
  
  grob
}

make_herf_bar <- function (tbl_df, subtitle) {
  plot_wrapper(
    ggplot(
      tbl_df %>% filter(year == 2017),
      aes(x = reorder(city, herfindahl), y = herfindahl)) +
      geom_bar(stat = "identity") +
      labs(
        y = "Diversity Index",
        x = element_blank(),
        title = "Economic Diversity of Canadian CMAs, 2017",
        subtitle = subtitle,
        caption = "Source: Conference Board of Canada and EEDC Calculations") +
      coord_flip()
  )
}

make_herf_lines <- function(tbl_df, subtitle) {
  plot_wrapper(
    ggplot(tbl_df, aes(x = year, y = herfindahl)) +
      geom_line() +
      facet_wrap(~ city) +
      scale_x_continuous(breaks = seq(1987, 2017, 10)) +
      labs(
        y = "Diversity Index",
        x = "Year",
        color = element_blank(),
        title = "Economic Diversity of Canadian CMAs, 1987 - 2017",
        subtitle = subtitle,
        caption = "Source: Conference Board of Canada and EEDC Calculations"),
  caption_pos = 17)
}

make_var_bar <- function(tbl_df, subtitle) {
  plot_wrapper(
    ggplot(
      tbl_df %>% filter(time == "t87_17"),
      aes(x = reorder(city, var), y = var)) +
      geom_bar(stat = "identity") +
      labs(
        y = "Volatility Index",
        x = element_blank(),
        title = "Economic Volatility of Canadian CMAs, 1987-2017",
        subtitle = subtitle,
        caption = "Source: Conference Board of Canada and EEDC Calculations") +
      coord_flip()
  )
}

make_var_lines <- function(tbl_df, subtitle) {
  plot_wrapper(
    ggplot(
      tbl_df %>% filter(time != "t87_17"),
      aes(x = time, y = var, group = reorder(city, var))) +
      geom_line() +
      geom_point() +
      facet_wrap(~city) +
      scale_x_discrete(
        labels = c("'87-'97", "'98-'07", "'08-'17")) +
      labs(
        y = "Volatility Index",
        x = "Time period",
        title = "Economic Volatility of Canadian CMAs",
        subtitle = subtitle,
        caption = "Source: Conference Board of Canada and EEDC Calculations"
      ),
    caption_pos = 17
  )
}

make_share_bar <- function(tbl_df, title, subtitle) {
  plot_wrapper(
    ggplot(
      tbl_df %>% filter(year == 2017, city %in% major_cmas),
      aes(x = city, y = share, fill = industry)) +
      geom_bar(position = position_stack(), colour = "white", stat = "identity") +
      geom_text(
        aes(label=percent(share, 1)), position = position_stack(vjust = 0.5),
        colour = "white", size = 2.5) +
      scale_y_continuous(labels = percent) +
      labs(
        title = title,
        subtitle = subtitle,
        caption = "Source: Conference Board of Canada and EEDC Calculations",
        x = element_blank(),
        y = "% of total",
        fill = "Industry")
  )
}

data_gdp <- clean_data(data_gdp)
data_emp <- clean_data(data_emp)

df_herf_gdp <- to_herf_plots(data_gdp)
df_herf_emp <- to_herf_plots(data_emp)
df_var_gdp <- to_var_plots(data_gdp)
df_var_emp <- to_var_plots(data_emp)

plot1 <- make_herf_bar(
  df_herf_gdp, 
  "Based on Conference Board of Canada GDP by Industry Estimates, Higher = More Diverse")

plot2 <- make_herf_bar(
  df_herf_emp,
  "Based on Employment by Industry, Higher = More Diverse")

plot3 <- make_herf_lines(
  df_herf_gdp,
  "Based on Conference Board of Canada GDP by Industry Estimates, Higher = More Diverse")

plot4 <- make_herf_lines(
  df_herf_emp,
  "Based on Employment by Industry, Higher = More Diverse")

plot5 <- make_var_bar(
  df_var_gdp,
  "Based on Conference Board of Canada GDP by Industry Estimates, Higher = More Volatile")

plot6 <- make_var_bar(
  df_var_emp,
  "Based on Employment by Industry, Higher = More Volatile")

plot7 <- make_var_lines(
  df_var_gdp,
  "Based on Conference Board of Canada GDP by Industry Estimates, Higher = More Volatile")

plot8 <- make_var_lines(
  df_var_emp,
  "Based on Employment by Industry, Higher = More Volatile")

plot9 <- make_share_bar(
  data_gdp,
  "Distribution of GDP by Industry for Major Canadian CMAs, 2017",
  "Note: FIRE includes implicit rental income from owner-occupied dwellings")

plot10 <- make_share_bar(
  data_emp,
  "Distribution of Employment by Industry for Major Canadian CMAs, 2017",
  element_blank())

plots_to_save <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10)

for (i in 1:length(plots_to_save)) {
  ggsave(file.path("plots", paste0("plot", i, ".jpg")), plots_to_save[[i]])
}
