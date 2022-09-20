
# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(hrbrthemes)
library(ggtext)
library(here)

nicaragua <- read_xlsx(here("Data", "excel", "nic2019-20220418.xlsx")) %>% 
  janitor::clean_names() %>% 
  rename(
    year = 1
  )

# Fonts -------------------------------------------------------------------

jetbrains <- "JetBrains Mono"
econsans  <- "EconSansCndReg"

# Cleaning ----------------------------------------------------------------

nicaragua <- nicaragua %>% 
  mutate(
    year = str_remove_all(year, "\\(FYTD\\)") %>% str_trim(.) %>% as.numeric(.)
  ) %>% 
  # select(-total) %>% 
  pivot_longer(
    -year,
    names_to = "month",
    values_to = "migrants"
  ) %>% 
  group_by(year) %>% 
  mutate(
    month = fct_relevel(
      month, 
      "oct", "nov", "dec", "jan", "feb", "mar", 
      "apr", "may", "jun", "jul", "aug", "sep"
    ),
    year = as_factor(year),
    cgroup = ifelse(year == 2022, TRUE, FALSE)
  )

nicaragua %>%
  filter(!is.na(migrants)) %>% 
  ggplot(
    aes(
      x = month,
      y = migrants,
      group = year,
      color = cgroup
    )
  ) +
  geom_line(
    size = 1
  ) +
  annotate(
    geom = "text", 
    x = 5, 
    y = 14000, 
    label = "In Dec. 2021,\nthere were 15,334 encounters", 
    vjust = 1, 
    size = 5,
    family = econsans,
    color = "black"
  ) + 
  geom_curve(
    aes(
      xend = 3.1, 
      yend = 15334,
      x = 5,
      y = 14200
    ),
    size = 0.25, 
    color = "black",
    curvature = 0.1,
    arrow = arrow(length = unit(0.01, "npc"))
  ) +
  annotate(
    geom = "text", 
    x = 11, 
    y = 14000, 
    label = "13,509 encounters\nin Jul. 2021", 
    vjust = 1, 
    size = 4.5,
    family = econsans,
    color = "#545454"
  ) + 
  annotate(
    geom = "text", 
    x = 6, 
    y = 1000, 
    label = "FY 2020", 
    vjust = 1, 
    size = 4.5,
    family = econsans,
    color = "#545454"
  ) + 
  coord_cartesian(clip = "off", expand = FALSE) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_color_manual(values = c("grey", "#ca0e19")) +
  labs(
    caption = "**Source**: U.S. Customs and Border Protection (CBP) · **Plot**: @rrmaximiliano",
    x = "",
    y = "Encounters",
    title = "Nationwide Encounters of Nicaraguans by Month in <span style = 'color:#ca0e19;'>Fiscal Year 2022</span>",
    subtitle = "So far, in FY 2022, there has been a total of 49,954 encounters. As reference, in FY 2021, there were a total of 50,722 encounters<br>at the borders. Fiscal year runs from October of one calendar year through September 30 of the next."
  ) +
  theme_ipsum_es() +
  theme(
    plot.title = element_markdown(size = rel(2)),
    plot.subtitle = element_markdown(size = rel(1.3), margin = margin(0,0,30,0)),
    plot.caption = element_markdown(size = rel(1.2), hjust = 0),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = rel(1.1), vjust = -1, color = "black"),
    axis.text.y = element_text(size = rel(1), color = "black", family = jetbrains),
    axis.title.y = element_text(size = rel(1.6), hjust = 0.5),
    axis.ticks.x = element_line(size = .5, color = "black"), 
    axis.ticks.length.x = unit(.25, "cm"),
    strip.text = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(20, 40, 20, 40),
    axis.line = element_line(colour = "grey70")
  )

ggsave(
  "Figs/encounters.png",
  dpi = 320,
  height = 8,
  width = 14,
  bg = "white"
)


# yearly data -------------------------------------------------------------

all <- read_rds(here("Data", "clean", "apprehensions_2007_2020.rds")) %>% 
  filter(country == "Nicaragua") %>% 
  group_by(year) %>% 
  summarize(
    migrants = sum(migrants, na.rm = TRUE)
  ) %>% 
  filter(year != 2020)

nicaragua_2022 <- nicaragua %>%
  filter(year == 2022, !is.na(migrants)) %>% 
  select(year, month, migrants) %>% 
  mutate(
    year = paste0(year, "-", month)
  ) %>% 
  select(-month)

nicaragua_2007_2021 <- nicaragua %>% 
  group_by(year) %>% 
  summarize(
    migrants = sum(migrants, na.rm = TRUE)
  ) %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  bind_rows(all) %>% 
  arrange(year)

nicaragua_2007_2021 %>%
  mutate(
    cgroup = ifelse(year == 2022, TRUE, FALSE)
  ) %>% 
  # filter(year > 2015) %>%
  ggplot(
    aes(
      x = year,
      y = migrants,
      fill = cgroup
    )
  ) +
  geom_col(
    color = "black",
  ) +
  geom_text(
    aes(
      label = scales::comma(migrants), 
      x = year,
      y = migrants
    ),
    family = jetbrains, 
    vjust = -1,
    inherit.aes = FALSE, 
    data = filter(nicaragua_2007_2021, year %in% c(2021,2022))
  ) +
  coord_cartesian(clip = "off", expand = FALSE) + 
  scale_y_continuous(label = scales::comma, limits = c(0,150000)) +
  scale_fill_manual(values = c("#666666", "#254b84")) +
  labs(
    x = "Fiscal Year",
    y = "Encounters",
    title = "Nationwide Encounters of Nicaraguans by Year",
    subtitle = "For year <span style = 'color:#254b84;'>**2022**</span>, data as of August 2022. Fiscal year runs from October of one calendar year through September 30 of the next.",
    caption = "**Source**: U.S. Customs and Border Protection (CBP) · **Plot**: @rrmaximiliano",
  ) +
  theme_ipsum_es() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = rel(2)),
    plot.subtitle = element_markdown(size = rel(1.5), margin = margin(0,0,30,0)),
    plot.caption = element_markdown(size = rel(1.2), hjust = 0),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = rel(1.1), vjust = -1, color = "black"),
    axis.text.y = element_text(size = rel(1), color = "black", family = jetbrains),
    axis.title.y = element_text(size = rel(1.6), hjust = 0.5),
    axis.title.x = element_text(size = rel(1.6), hjust = 1, vjust = -1),
    axis.ticks.x = element_line(size = .5, color = "black"), 
    axis.ticks.length.x = unit(.25, "cm"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 40, 20, 40),
    axis.line = element_line(colour = "grey70")
  )

ggsave(
  "Figs/encounters_yearly.png",
  dpi = 320,
  height = 8,
  width = 14,
  bg = "white"
)
