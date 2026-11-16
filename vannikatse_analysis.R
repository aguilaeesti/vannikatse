# libraries
# Data management
library(tidyverse)
library(stringr)
library(dplyr)

# Plotting
library(ggplot2)
library(patchwork)
library(scales)


library(igraph) # for family tree
library(ggraph)


## Data cleaning
df_raw <- read.csv("data_vannikatse.csv", sep=";", dec=",")

df_clean <- df_raw %>%
  filter(ID != "A2") %>%    # removed both, too close and not distinguishable. They are too strong a case for dormancy, and possibly wrong.
  filter(ID != "A3") %>%
  
  filter(ID != "A23") %>%   # remove it, because of close distance to A25
  filter(ID != "A23a10") %>%
  
  filter(ID != "A7") %>%    # removed it together with A9, as they are interchangeable = individual profile too unsure
  filter(ID != "A7a3") %>%
  filter(ID != "A9") %>%
  filter(ID != "A9a2") %>%
  filter(ID != "A9b8") %>%
  
  filter(ID != "C6a8") %>% # removed, as it was a strange case. Growing from the same place as C6 in 2026, and flowering in the first year (as did C6 in 2026). Could be C6 growing two flowering stalks, or C5 which disappeared only in 2025, or a strange flowering sidebulb. Not possible to explain.
  mutate(
    year_num   = as.numeric(year),
    ID         = as.character(ID),
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering  = as.integer(!is.na(genh)),
    h1_cm      = h1 / 10
  ) %>%
  mutate(
    tray = str_extract(ID, "^[A-Za-z]"),                                    # extract tray letters A, B, C from the ID                                
    tray = factor(tray),                                                    # factorize trays
    year_num = as.numeric(year),                                            # add a numeric year column
    year = factor(year),                                                    # factorize years
    ID = factor(ID),                                                        # factorize IDs
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),            # counts the number of leaf height measurements = nr of leaves
    flowering = as.integer(!is.na(flower_len)),                             # assigns all individuals with flower_len value != 0
    sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)), # assigns the number of new sidebulbs to each mature plant.
    grew_sidebulbs = ifelse(sidebulbs > 0, 1, 0),                           # sidebulbs 1 0
  )

#____________________________________________________________________________###
# Transition matrix -------------------------------------------------------
#____________________________________________________________________________###

# Load and clean data
# Should I exclude all individuals that appear only one year - I guess yes, for the transition matrix. 
df_trans <- df_clean %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  # filter out all that appear only once
  filter(n() > 1) %>%
  # filter out all sidebulbs
  filter(motherplant == 1) %>%
  mutate(
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),           # to set the dormancy 
    leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)), # to set the dormancy
    
    state = case_when(
      flowering == 1 ~ "flowering",                                         # define the stage of flowering (as flowers present)
      leaf_count == 1 ~ "1-leaf",                                           # define the stage of 1-leaf (exactly 1 leaf)
      leaf_count >= 2 ~ "vegetative",                                       # define the stage of vegetative (1+ leaf and not flowering)
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
      leaf_count == 0 & leaves_after ~ "dormant",                           # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death condition
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", # any individual that is gone for longer than 1 year
      
      # The rest will be unknown
      leaf_count == 0 ~ "unknown" # all individuals that we don't know if it is dead or dormant (last years)
    )
  ) %>%
  # Let's mark the rest of the years of death as "dead" too
  mutate(
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()

##### TRANSITION MATRIX T -> T1####


# Transition pairs t-> t1
# we loose two years of transitions (2021 - 2022. But we know this!) It only doesn't give too much information... As they were all equalized by drying them.
# add initial stage to the individuals
# and 2026 - 2027. Yeah, we cannot know this
transitions_1 <- df_trans %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  mutate(
    # Adding current and following state
    state_t = state,       # state of the previous year
    state_t1 = lead(state) # state of the current year
  ) %>%
  # We can only keep the pairs where we know both the start and end state 
  # Let's also exclude the dead and unknown states
  filter(!is.na(state_t), !is.na(state_t1)) %>%
         # !state_t %in% c("unknown", "dead"),
         # !state_t1 %in% c("unknown", "dead")) %>%
  ungroup()

#write.csv(transitions_1,"trans.csv")

# Calculating transition probabilities
transition_matrix <- transitions_1 %>%
  count(state_t, state_t1) %>%
  group_by(state_t) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup()


#table(df_trans$ID,df_trans$state)

#------------------------------------------------------------------------------#
##### _______________________ Plot T-matrix ______________________________ #####
#------------------------------------------------------------------------------#
# We can view it as a table...
library(tidyr)
transition_matrix %>%
  select(state_t, state_t1, probability) %>%
  pivot_wider(names_from = state_t1, values_from = probability, values_fill = 0)

# ..or use ggplot to view it as a transition matrix
library(ggplot2)
library(scales)

# So they would be in logical order in the matrix
state_order <- c(
  "unknown",
  "dead",
  "1-leaf",
  "vegetative",
  "flowering",
  "dormant"
)

transition_matrix <- transition_matrix %>%
  mutate(
    state_t = factor(state_t, levels = state_order),
    state_t1 = factor(state_t1, levels = state_order)
  )


ggplot(transition_matrix, aes(x = state_t1, y = state_t, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.5) +
  # adding the probabilities inside the squares
  geom_text(aes(label = sprintf("%.1f", probability*100)), # sprintf("%.1f", probability) rounds to exactly two decimals
            color = "black", size = 5) +
  # hex color from the picture again :)
  scale_fill_gradient(low = "white", high = "#b641b8", limit = c(0,1), name = "Transition\nProbability",
                      labels = percent_format(accuracy = 1)) +
  labs(
    title = "Plant Life-Stage Transition Matrix (t to t+1)",
    x = "State in Year t + 1",
    y = "State in Year t"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10)
  )


# state transition diagramme ----------------------------------------------

library(igraph)


edges <- transition_matrix %>%
  transmute(
    from = state_t,
    to = state_t1,
    probability = probability,
    percent = probability * 100
  )

g <- graph_from_data_frame(edges) # using igraph to turn the transition matrix into IGRAPH object

E(g)$weight <- edges$percent

# simple plot 
#plot(g)

# pretty diagramme function
diagramPlot <- function(g) {plot.igraph(
  g,
  main = "State transition diagram",
  margin = c(0.3,0.3,0.3,0.3),
  
  # vertices (tips of the graph)
  vertex.size = 65,
  #vertex.color = alpha(c("palegreen", "#636363", "brown",
                   #"#b641b8", "#DADADA", "palegreen4"),alpha=0.5), # incl dead and unknowns
  vertex.color = alpha(c("palegreen", "brown",
                       "#b641b8", "palegreen4"),alpha=0.5),
  vertex.frame.color = "grey20",
  vertex.label.cex = 1.5,
  vertex.label.color = "grey10",
  
  # edges and arrows
  edge.width = E(g)$percent/10+1,
  edge.color = alpha("grey70",alpha = 0.5),
  edge.arrow.size = E(g)$percent/100+1,
  edge.curved = 0.3,
  
  # labels
  edge.label = paste0(round(E(g)$percent,0), "%"),
  edge.label.cex = 1.4,
  edge.label.color = "grey10",
  edge.label.font = 2, # bold
  
  #layout
  layout =  layout_in_circle(g, order = V(g)[c(3,2,1,4)]) # c(top, right, left, bottom) 1 = 1-leaf, 2 = dormant, 3 = vegetative, 4 = flowering
  #layout = layout_in_circle(g) # incl dead and unknowns 
)
}


# pretty, incl dead and unknown
# diagramPlot(g)

# pretty, excl unknown and dead
edges1 <- edges[edges$to!="unknown" & edges$to !="dead",]
g1 <- graph_from_data_frame(edges1) # using igraph to turn the transition matrix into IGRAPH object

# remove '#' to save file into OUTPUT folder
#png(filename = "OUTPUT/stateTransitionDiagramme.png", width = 500,height = 550)
diagramPlot(g1)
#dev.off()

