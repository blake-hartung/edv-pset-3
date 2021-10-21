library(tidyverse)
library(GGally)

df <- read_csv("https://data.ny.gov/api/views/ca8h-8gjq/rows.csv")

df_2020_grouped <- df %>%
  filter(Year == 2020) %>%
  group_by(County, Region) %>%
  summarise(murder = sum(Murder),
            rape = sum(Rape),
            robbery = sum(Robbery),
            aggravated_assault = sum(`Aggravated Assault`),
            burglary = sum(Burglary),
            larceny = sum(Larceny),
            motor_theft = sum(`Motor Vehicle Theft`),
            total = sum(`Index Total`)
            ) %>%
  arrange(desc(total)) %>%
  mutate(cat = case_when(Region == 'New York City' ~ 0,
                         Region == 'Non-New York City' ~ 1))

simple_graph <- GGally::ggparcoord(data = df_2020_grouped,
                                   columns = 3:9,
                                   scale = 'globalminmax')

complicated_graph <- GGally::ggparcoord(data = df_2020_grouped,
                                        columns = 3:9,
                                        scale = 'std',
                                        showPoints = T,
                                        alphaLines = 0.2,
                                        order = c(8, 6, 7, 9, 5, 4, 3))

interactive_graph <- df_2020_grouped %>%
  plotly::plot_ly(type = 'parcoords',
                  line = list(color = ~cat,
                             colorscale = list(c(0, 'red'),
                                               c(1, 'blue'))),
                  dimensions = list(
                   list(range = c(0, 33000),
                        label = 'Larceny', values= ~larceny),
                   list(range = c(0, 10000),
                        label = 'Aggravated Assault', values= ~aggravated_assault),
                   list(range = c(0, 5500),
                        label = 'Burglary', values= ~burglary),
                   list(range = c(0, 4000),
                        label = 'Motor Theft', values= ~motor_theft),
                   list(range = c(0, 4000),
                        label = 'Robbery', values= ~robbery),
                   list(range = c(0, 750),
                        label = 'Rape', values= ~rape),
                   list(range = c(0, 200),
                        label = 'Murder', values= ~murder)
                 ))
