##### 2021 RStudio Table Contest: Premier League Classification  #####
##### By: Stephan Teodosescu #####
##### October 2021 #####

library(tidyverse)
library(worldfootballR) # installed using "remotes::install_github("JaseZiv/worldfootballR")"
library(gt) # for beautiful static tables
library(gtExtras) # for add-ons working with {gt} package
library(magick)
library(webshot) # saving high quality images of gt tables
library(glue)
library(ggimage) # for working with logos
library(rlang)
library(RCurl)

##### Data import: Load data from {worldfootballR} and other sources #####

# Function to extract Premier League match results data from FBREF
EPL_2022 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2022, tier = "1st")

# Load team mapping file
team_mapping <- "https://raw.githubusercontent.com/steodose/Club-Soccer-Forecasts/main/team_mapping.csv" %>%
  read_csv()

##### Set up themes for table #####

# Define color palette to use in tables
my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")

# Create 538 GT table theme from Thomas Mock's blog. Idea comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3
gt_theme_538 <- function(data, ...) {
  data %>%
    # Add team logos w/ web_image
    text_transform(
      locations = cells_body(
        vars(url_logo_espn)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    # Relabel columns
    cols_label(
      url_logo_espn = ""
    ) %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
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
    ) %>%
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

matchweek <- 9 # Specify how many full matchweeks have been played
last_week <- matchweek - 1

games_df <- EPL_2022 %>%
  filter(Wk <= matchweek) %>%
  mutate(Result = HomeGoals - AwayGoals) %>%
  select(Home, Away, Result, Wk, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
  pivot_longer(Home:Away, names_to = "home_away", values_to = "Team") %>%
  mutate(
    Result = ifelse(home_away == "Home", Result, -Result),
    win = ifelse(Result == 0, 0.5, ifelse(Result > 0, 1, 0))
  ) %>%
  select(Wk, Team, HomeGoals, AwayGoals, win, Result) %>%
  drop_na()

team_mapping2 <- team_mapping %>%
  select(squad_fbref, url_logo_espn)

joined_df <- games_df %>%
  group_by(Team) %>%
  summarise(
    Wins = length(win[win == 1]),
    Losses = length(win[win == 0]),
    Draws = length(win[win == 0.5]),
    MP = sum(Wins, Losses, Draws),
    Points = (Wins * 3) + (Draws * 1),
    `Points Percentage` = (100 * Points / (MP * 3)),
    GD = sum(Result),
    form = list(win), .groups = "drop"
  ) %>%
  left_join(team_mapping2, by = c("Team" = "squad_fbref")) %>%
  select(url_logo_espn, Team, Points, MP, Wins, Draws, Losses, GD, `Points Percentage`, form) %>%
  arrange(desc(Points), desc(GD)) %>%
  ungroup() %>%
  mutate(Rank = row_number()) %>%
  relocate(Rank) %>%
  rename(Squad = Team) %>%
  mutate(list_data = list(c(Wins, Draws, Losses)))

# Reorganize list data
# create list_data column for table graphic
joined_df2 <- joined_df %>%
  gather(attr_num, list_data, c(Wins, Draws, Losses)) %>%
  # could use pivot_longer here
  group_by_at(vars(-attr_num, -list_data)) %>%
  summarise(list_data = list(list_data)) %>%
  ungroup()


# Determine prior week rankings (doing this a little different than above)
home_pw <- EPL_2022 %>%
  group_by(Home) %>%
  filter(Wk <= last_week) %>%
  summarize(
    MP = n(),
    Pts = sum((HomeGoals > AwayGoals) * 3 + (HomeGoals == AwayGoals) * 1)
  ) %>%
  ungroup()

away_pw <- EPL_2022 %>%
  group_by(Away) %>%
  filter(Wk <= last_week) %>%
  summarize(
    MP = n(),
    Pts = sum((AwayGoals > HomeGoals) * 3 + (HomeGoals == AwayGoals) * 1)
  ) %>%
  ungroup()

joined_df_pw <- inner_join(home_pw, away_pw, by = c("Home" = "Away")) %>%
  group_by(Home) %>%
  mutate(
    MP = sum(MP.x, MP.y),
    Points = sum(Pts.x, Pts.y)
  ) %>%
  rename("Squad" = "Home") %>%
  arrange(desc(Points)) %>%
  ungroup() %>%
  mutate(Rank = row_number()) %>%
  relocate(Rank) %>%
  rename("pw_rank" = "Rank") %>%
  select(Squad, pw_rank)

# join current week and prior week together
joined_df3 <- joined_df2 %>%
  right_join(joined_df_pw) %>%
  mutate(`1-Wk Change` = pw_rank - Rank) %>%
  select(-pw_rank) %>%
  select(Rank, `1-Wk Change`, url_logo_espn:list_data)

# Add in goals data
home_goals <- EPL_2022 %>%
  group_by(Home) %>%
  filter(Wk <= matchweek) %>%
  # filter for only games that have occurred
  summarize(
    MP = n(),
    GS = sum(HomeGoals),
    xGS = sum(Home_xG),
    GC = sum(AwayGoals),
    xGC = sum(Away_xG)
  ) %>%
  ungroup()

away_goals <- EPL_2022 %>%
  group_by(Away) %>%
  filter(Wk <= matchweek) %>%
  # filter for only games that have occurred
  summarize(
    MP = n(),
    GS = sum(AwayGoals),
    xGS = sum(Away_xG),
    GC = sum(HomeGoals),
    xGC = sum(Home_xG)
  ) %>%
  ungroup()

# join goals data together
goals_joined <- inner_join(home_goals, away_goals, by = c("Home" = "Away")) %>%
  group_by(Home) %>%
  mutate(
    MP = sum(MP.x, MP.y),
    GS = sum(GS.x, GS.y),
    GC = sum(GC.x, GC.y),
    xGS = sum(xGS.x, xGS.y),
    xGC = sum(xGC.x, xGC.y)
  ) %>%
  rename("Squad" = "Home") %>%
  mutate(
    xGD = xGS - xGC
  ) %>%
  select(Squad, GS, GC, xGS, xGC, xGD)


# join final data frame together

joined_df4 <- joined_df3 %>%
  right_join(goals_joined) %>%
  select(Rank:Points, GS, GC, GD, xGS, xGC, xGD, form, `Points Percentage`, list_data)

##### Make Table Viz #####

## Make league table highlighting Brentford
joined_df4 %>%
  gt() %>%
  data_color(
    columns = 5,
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>%
  gt_theme_538() %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFAA0") # highlighting the Brentford row.
    ),
    locations = cells_body(rows = Squad == "Brentford")
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_body(rows = 4)
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_body(rows = 17)
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "red")
    ),
    locations = cells_body(
      columns = vars(xGD),
      rows = xGD <= 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "blue")
    ),
    locations = cells_body(
      columns = vars(xGD),
      rows = xGD > 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "red")
    ),
    locations = cells_body(
      columns = vars(GD),
      rows = GD <= 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "blue")
    ),
    locations = cells_body(
      columns = vars(GD),
      rows = GD > 0
    )
  ) %>%
  tab_spanner(
    label = "Via StatsBomb",
    columns = c(9:11)
  ) %>%
  cols_width(Squad ~ 190) %>%
  cols_align(
    align = "left",
    columns = 1
  ) %>%
  cols_align(
    align = "center",
    columns = 2
  ) %>%
  text_transform(
    locations = cells_body(columns = `1-Wk Change`),
    fn = function(x) {
      change <- as.integer(x)
      choose_logo <- function(x) {
        if (x == 0) {
          gt::html(fontawesome::fa("equals", fill = "#696969"))
        } else if (x > 0) {
          gt::html(glue::glue("<span style='color:#191970;text-indent:16px;font-face:bold;font-size:16px;'>{x}</span>"), fontawesome::fa("chevron-up", fill = "#1134A6"))
        } else if (x < 0) {
          gt::html(glue::glue("<span style='color:#DA2A2A;font-face:bold;font-size:16px;'>{x}</span>"), fontawesome::fa("chevron-down", fill = "#DA2A2A"))
        }
      }
      map(change, choose_logo)
    }
  ) %>%
  tab_header(
    title = md("**2021-22 Premier League Table**"),
    subtitle = md(glue("**<span style = 'color:#e30613'>Brentford</span>** are well above the relegation zone in the club's first season in the Premier League. The Bees' Expected Goals values indicate they may even be underperforming their true quality. Teams sorted based on points thru **Matchweek {matchweek}**."))
  ) %>%
  tab_source_note(
    source_note = md("DATA: fbref.com via {worldfootballR}.<br>Table: @steodosescu (Between the Pipes) | Inspired by Tom Mock.")
  ) %>%
  gt_plt_bar_pct(column = `Points Percentage`, scaled = TRUE, fill = "navy", background = "gray") %>%
  gt_plt_winloss(form, max_wins = 10) %>%
  gt_plt_bar_stack(list_data,
    width = 55,
    labels = c("  WINS  ", "  DRAWS  ", "  LOSSES  "),
    palette = c("#ff4343", "#bfbfbf", "#0a1c2b")
  ) %>%
  tab_footnote(
    footnote = "Share of total available points captured. Win = 3 points, Draw = 1 point, Loss = 0 points.",
    locations = cells_column_labels(vars(`Points Percentage`))
  ) %>%
  tab_footnote(
    footnote = "Expected Goals (xG) are the probability that a shot will result in a goal based on the characteristics of that shot. Effectively a measure of the quality of chances created.",
    locations = cells_column_labels(vars(xGS:xGD))
  ) %>%
  gtsave("2021-22 Premier League Table.png")
