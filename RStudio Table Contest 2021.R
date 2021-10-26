##### 2021 RStudio Table Contest: Premier League Classification  #####
##### By: Stephan Teodosescu #####
##### October 2021 #####

library(tidyverse)
library(worldfootballR) #installed using "remotes::install_github("JaseZiv/worldfootballR")"
library(magick)
library(gt) #for 538-themed tables
library(gtExtras) #for add-ons working with {gt} package
library(webshot) #saving high quality images of gt tables
library(glue)
library(rlang)
library(RCurl)
library(ggimage) #for working with logos

##### Data import: Load data from {worldfootballR} and other sources #####

# Function to extract Premier League match results data from FBREF
EPL_2022 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2022, tier = "1st")

# Load team mapping file
team_mapping <- 'https://raw.githubusercontent.com/steodose/Club-Soccer-Forecasts/main/team_mapping.csv' %>% 
    read_csv()

matchweek <- 9 # Specify how many full matchweeks have been played
last_week <- matchweek-1


##### Set up themes for table #####

# Define color palette to use in tables
my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")

# Create 538 GT table theme from Thomas Mock's blog. Idea comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
gt_theme_538 <- function(data,...) {
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
        opt_all_caps()  %>%
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


##### Data Wrangling #####

## Tidy dataframe for input into table

home <- EPL_2022 %>% 
    group_by(Home) %>%
    filter(Wk <= matchweek) %>% #filter for only games that have occurred
    mutate(win = if_else(HomeGoals > AwayGoals, 1, 0),
           loss = if_else(AwayGoals > HomeGoals, 1, 0),
           draw = if_else(HomeGoals == AwayGoals, 1, 0)) %>% 
    summarize(
        MP = n(),
        Pts = sum((HomeGoals > AwayGoals) * 3 + (HomeGoals == AwayGoals) * 1),
        GS = sum(HomeGoals),
        xGS = sum(Home_xG),
        GC = sum(AwayGoals),
        xGC = sum(Away_xG),
        win = sum(win),
        loss = sum(loss),
        draw = sum(draw)) %>% 
    ungroup()

away <- EPL_2022 %>% 
    group_by(Away) %>%
    filter(Wk <= matchweek) %>% #filter for only games that have occurred
    mutate(win = if_else(AwayGoals > HomeGoals, 1, 0),
           loss = if_else(HomeGoals > AwayGoals, 1, 0),
           draw = if_else(HomeGoals == AwayGoals, 1, 0)) %>% 
    summarize(
        MP = n(),
        Pts = sum((AwayGoals > HomeGoals) * 3 + (HomeGoals == AwayGoals) * 1),
        GS = sum(AwayGoals),
        xGS = sum(Away_xG),
        GC = sum(HomeGoals),
        xGC = sum(Home_xG),
        win = sum(win),
        loss = sum(loss),
        draw = sum(draw)) %>% 
    ungroup()


# Join home and away data, and logos from team mapping dataset

EPL_2022_table <- inner_join(home, away, by = c("Home" = "Away")) %>% 
    group_by(Home) %>% 
    mutate(
        MP = sum(MP.x, MP.y),
        GS = sum(GS.x, GS.y),
        GC = sum(GC.x, GC.y),
        xGS = sum(xGS.x, xGS.y),
        xGC = sum(xGC.x, xGC.y),
        Points = sum(Pts.x, Pts.y),
        `Points Percentage` = Points/(MP*3),
        win = sum(win.x, win.y),
        draw = sum(draw.x, draw.y),
        loss = sum(loss.x, loss.y)) %>%
    rename("Squad" = "Home") %>% 
    mutate(
        GD = GS-GC,
        xGD = xGS-xGC
    ) %>% 
    select(Squad, Points, MP, GS, GC, GD, xGS, xGC, xGD, `Points Percentage`, win, draw, loss)

team_mapping2 <- team_mapping %>% 
    select(squad_fbref, url_logo_espn)

EPL_2022_table <- EPL_2022_table %>%
    left_join(team_mapping2, by = c("Squad" = "squad_fbref"))

EPL_2022_table2 <- EPL_2022_table %>%
    relocate(url_logo_espn) %>% 
    arrange(desc(Points), desc(GD)) %>% 
    ungroup() %>% 
    mutate(Rank = row_number()) %>% 
    relocate(Rank) %>%
    mutate(list_data = list(c(win,draw,loss)))

# create list_data column for table graphic
EPL_2022_table3 <- EPL_2022_table2 %>% 
    gather(attr_num, list_data, c(win,draw,loss)) %>%  #could use pivot_longer here
    group_by_at(vars(-attr_num, -list_data)) %>% 
    summarise(list_data = list(list_data)) %>% 
    ungroup()

# Determine prior week rankings
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
        Points = sum(Pts.x, Pts.y)) %>%
    rename("Squad" = "Home") %>%
    arrange(desc(Points)) %>% 
    ungroup() %>% 
    mutate(Rank = row_number()) %>% 
    relocate(Rank) %>% 
    rename("pw_rank" = "Rank") %>% 
    select(Squad, pw_rank)

# join together
EPL_2022_table4 <- EPL_2022_table3 %>% 
   right_join(joined_df_pw) %>% 
    mutate(`1-Wk Change` = pw_rank - Rank) %>% 
    select(-pw_rank) %>% 
    select(Rank, `1-Wk Change`, url_logo_espn:list_data)



##### Make Table Viz #####

## Make league table highlighting Brentford

EPL_2022_table4 %>%
    gt() %>%
    data_color(columns = 5,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)
    ) %>%
    gt_theme_538() %>%
    tab_style(
        style = list(
            cell_fill(color = "#FFFAA0") #highlighting the Brentford row.
        ),
        locations = cells_body(rows = Squad=="Brentford")
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
        columns = c(10:12)
    ) %>% 
    cols_width(Squad ~ 200) %>% 
    cols_align(align = "left",
               columns = 1) %>%
    cols_align(align = "center",
               columns = 2) %>%
    text_transform(
        locations = cells_body(columns = `1-Wk Change`),
        fn = function(x){
            change <- as.integer(x)
            choose_logo <-function(x){
                if (x == 0){
                    gt::html(fontawesome::fa("equals", fill = "#696969"))
                } else if (x > 0){
                    gt::html(glue::glue("<span style='color:#191970;text-indent:16px;font-face:bold;font-size:16px;'>{x}</span>"), fontawesome::fa("chevron-up", fill = "#1134A6"))
                } else if (x < 0) {
                    gt::html(glue::glue("<span style='color:#DA2A2A;font-face:bold;font-size:16px;'>{x}</span>"), fontawesome::fa("chevron-down", fill = "#DA2A2A"))
                }
            }
            map(change, choose_logo)
        }
    ) %>%
    tab_header(title = md("**2021-22 Premier League Table**"),
               subtitle = md(glue("**<span style = 'color:#e30613'>Brentford</span>** are well above the relegation zone in the club's first season in the Premier League. The Bees' Expected Goals values indicate they may even be underperforming their true quality. Teams sorted based on points thru **Matchweek {matchweek}**."))) %>% 
    tab_source_note(
        source_note = md("DATA: fbref.com via {worldfootballR}.<br>Table: @steodosescu (Between the Pipes) | Inspired by Tom Mock.")) %>% 
    gt_plt_bar_pct(column = `Points Percentage`, scaled = FALSE, fill = "navy", background = "gray") %>% 
    gt_plt_bar_stack(list_data, width = 65,
                     labels = c("  WINS  ", "  DRAWS  ", "  LOSSES  "),
                     palette= c("#ff4343", "#bfbfbf", "#0a1c2b")) %>% 
    tab_footnote(
        footnote = "Points earned as a share of the total available from each squad's matches played.",
        locations = cells_column_labels(vars(`Points Percentage`)) 
        ) %>% 
    tab_footnote(
        footnote = "Expected Goals (xG) is the probability that a shot will result in a goal based on the characteristics of that shot.",
        locations = cells_column_labels(vars(xGS:xGD)) 
    ) %>% 
    gtsave("2021-22 Premier League Table.png")
