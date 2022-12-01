
## Section 2  ---- 
library(dplyr)
library(tidyr)
library(tidyverse)

trend <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)
#----------------------------------------------------------------------------#
# Which state has the highest black population in jail?

state_highest_black <- trend %>% 
  filter(black_jail_pop == max(na.omit(black_jail_pop))) %>% 
  pull(state)

state_highest_white <- trend %>% 
  filter(white_jail_pop == max(na.omit(white_jail_pop))) %>% 
  pull(state)

# Which year has the highest number of population in jail?

highest_pop<- trend %>% 
  filter(total_jail_pop == max(na.omit(total_jail_pop))) %>% 
  pull(total_jail_pop)

highest_pop_year <- trend %>% 
  filter(total_jail_pop == max(na.omit(total_jail_pop))) %>% 
  pull(year)

# Which county has the highest DCRP?

dcrp_state <- trend %>%
  filter(total_jail_pop_dcrp == max(na.omit(total_jail_pop_dcrp))) %>% 
  pull(state)

dcrp_county <- trend %>% 
  filter(total_jail_pop_dcrp == max(na.omit(total_jail_pop_dcrp))) %>% 
  pull(county_name)

#----------------------------------------------------------------------------#

## Section 3  ---- 
library(ggplot2)
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function returns the dataframe that shows the total population every year 

get_year_jail_pop <- function() {
  result <- trend %>% 
    group_by(year) %>% 
    filter(total_jail_pop != "NA") %>% 
    summarise(total_jail_pop = sum(total_jail_pop)) 
  return(result)
}

# This function produces a chart of the total jail population every year from 1970-2018.

plot_year_jail_pop_us<- function() {
  ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
       x = "Years", 
       y = "Total Jail Population")
}

plot_year_jail_pop_us()
  
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas

# This function creates the dataframe that shows the states and their
# total population per year
get_jail_pop_by_states <- function(states) {
  result <- trend %>% 
    filter(str_detect(state, paste(states, collapse = "|")) == TRUE) %>% 
    group_by(year, state) %>% 
    filter(total_jail_pop != "NA") %>% 
    summarise(total_jail_pop = sum(total_jail_pop)) 
  return(result)
} 

# This function graphs the dataframe and displays the values for 
# CA, OR, and WA.

plot_jail_pop_by_states <- function(states) {
  ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop)) + 
    geom_line(aes(color = state, linetype = state)) +
    labs(title = "Jail Population in States (1970-2018)", caption = "Jail population for three different states", 
         x = "Years", 
         y = "Total Jail Population")
}
  
plot_jail_pop_by_states(c("WA", "OR", "CA"))


#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Total white jail population vs total black jail population
# Your functions might go here ... <todo:  update comment>
# See Canvas

comparison <- trend %>%
select(year, white_jail_pop, black_jail_pop) %>%
gather(key = "variable", value = "value", -year, na.rm = TRUE)

ggplot(comparison, aes(x = year, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(title = "White vs Black Jail Population in U.S. (1970-2018)", 
       x = "Years", 
       y = "Total Jail Population")


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Comparing different groups' population in jail 
# Your functions might go here ... <todo:  update comment>
# See Canvas

library(maps)

black_jail_data <- trend %>% 
  select(state, year, black_jail_pop) %>% 
  filter(year == 2018) %>% 
  group_by(state) %>% 
  summarise(black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>% 
  mutate(state_name = tolower(state.name[match(state, state.abb)]))

map_state <- map_data("state") %>% 
 rename(state_name = region) %>% 
  left_join(black_jail_data, by= "state_name")

jail_state <- map_state %>% 
  filter(!is.na(map_state$black_jail_pop))

map1 <- ggplot(data = jail_state, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = black_jail_pop), color = "black")
map1

map2 <- map1 + scale_fill_gradient(name = "Population Count",
  low = "yellow", high = "red", na.value = "grey50") +
  labs(title = "Black Jail Population in U.S. (2018)") +
  theme(
    axis.text.x= element_blank(),        
    axis.text.y = element_blank(),       
    axis.ticks = element_blank(),       
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    rect = element_blank())
map2


#----------------------------------------------------------------------------#

## Load data frame ---- 


