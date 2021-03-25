
# Loading packages --------------------------------------------------------

library(tidyverse)
library(readxl)
library(googlesheets4)
library(stringr)

# Loading data ------------------------------------------------------------

# We use here a package to read data directly from google spreadsheets:

elemental  <- read_sheet("https://docs.google.com/spreadsheets/d/1SZBrcFhISsxXDdsvYLeN6DSk4a6VYTOK1rjCI1UAh8k/edit?usp=sharing") 




# Data Wrangling ----------------------------------------------------------

# here we clean data, pivot the table and select element name, this prepares data to be graphed
 
data_graph <- elemental %>% 
        filter(!is.na(Muestra)) %>% 
        janitor::clean_names() %>% 
        select(-description) %>% 
        select(!contains("conc")) %>% 
        pivot_longer(cols = ag_rsd_percent:zn_rsd_percent, names_to = "element", values_to = "percent") %>% 
        mutate(element = word(element,1,sep = "\\_"))




# Data plotting -----------------------------------------------------------

data_graph %>% 
        ggplot(aes(x = reorder(element, percent), y = percent)) +
        geom_col(col = "black", 
                 fill = "grey90") +
        coord_flip() +
        facet_wrap(~muestra, 
                   scales = "free_y") +
        labs(x = "% concentration", 
             y = "Element name (Abbr.)", 
             title = "Element concentration in biomineralization", 
             subtitle = "Coralline algae")
        






