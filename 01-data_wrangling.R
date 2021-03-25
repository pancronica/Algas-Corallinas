
# Loading packages --------------------------------------------------------

library(tidyverse)
library(readxl)
library(googlesheets4)
library(stringr)
library("gplots")




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


# for the heatmap

mtx <- elemental %>% 
        filter(!is.na(Muestra)) %>% 
        janitor::clean_names() %>% 
        select(contains("perc")) %>% 
        as.matrix() %>% 
        scale()

rownames(mtx) <- elemental$Muestra


rownames(mtx) <- elemental$Muestra

colnames(mtx) <- word(colnames(mtx),1,sep = "\\_")

# Data plotting -----------------------------------------------------------


# Exploratory graph

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

# Heatmap example

heatmap.2(mtx, 
          Rowv = T,
          scale = "none", 
          col = bluered(100), 
          trace = "none", 
          density.info = "none", 
          )


## More complex heatmap example


# website reference https://jokergoo.github.io/ComplexHeatmap-reference/book/


library(ComplexHeatmap)
library(circlize)

col_fun <- colorRamp2(c(-2.5, 0, 2.5), c("dodgerblue4", "white", "firebrick")) #select colour ramp

Heatmap(mtx, 
        name = "Scale", 
        cluster_columns = FALSE,
        col = col_fun,
        clustering_distance_rows = "pearson",
        row_split = factor(elemental$Depth, 
                           levels = c("1", "2"), 
                           labels = c("Winter", "Summer")),
        row_title_gp = gpar(fill = c("#D0EAE1", "#EBB6AC")),
        rect_gp = gpar(col = "white", lwd = 2),
        row_dend_reorder = TRUE,
        #row_km = 2,
        row_names_gp = gpar(fontsize = 5))
        



