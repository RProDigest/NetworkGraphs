######################################################
## Using Networks to Visualize  relationships
## Written By RProDigest X handle @RProDigest   
## Date 23rd Sept., 2023
## Dataset: Palmerpenguins
## All Atributions acknoweldged for use of PalmerPenguins & R Packages
## Tidyverse, Magrittr, visNetwork, htmlwidgets and Broom
## Historical Fact: Leonhard Euler solved the Seven Bridges of Königsberg problem
## in 1736. Königsberg is now Kaliningrad, Russia.
##
## References: 
## 1. https://datastorm-open.github.io/visNetwork/legend.html
## 2. https://datastorm-open.github.io/visNetwork/options.html
#######################################################

# Load and install libraries using the pacman package manager
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,    # For data manipulation and visualization
  visNetwork,   # For network visualization
  tidymodels,   # For data preprocessing
  htmlwidgets,  # For creating R bindings to JavaScript
  magrittr,     # Required for magrittr pipes
  palmerpenguins, # Access to the Penguins dataset
  showtext      # For customized fonts
)

# Enable custom fonts
font_add_google("Roboto", 'Roboto')
showtext_auto()


# Load the dataset and glimpse it
pengs <- palmerpenguins::penguins 
 
glimpse(pengs)

# One hot encode species & drop other variables

pengs$species %<>% as.numeric() 

pengs |>
  filter(sex != "female") |> 
  select(-sex, -island, -year) |>
  drop_na() ->
  penguins_cleaned

## Notes to myself
## Species Adelie = 1, Chinstrap = 2, Gentoo = 3

# Additional Data Processing
# We keep only the unique rows since duplicate rows would have a distance of 0.

# Create a distance matrix
penguins_cleaned_distance <-  penguins_cleaned |>
  stats::dist()

# Normalize the distances to values between 0 and 1
penguins_cleaned_distance <- penguins_cleaned_distance / max(penguins_cleaned_distance)

# Use 1 - distance to obtain the value for similarity
penguin_similarity <- 1 - penguins_cleaned_distance

# Create the network edges

# Create an edge dataframe
penguin_edges <- penguin_similarity |>
  tidy()

# Rename columns for visNetwork input
colnames(penguin_edges) <- c("from", "to", "value")

# We set the median as the threshold for edge values
penguin_edges <- penguin_edges |>
  subset(value > median(penguin_edges$value))

# Arrange by order of edge thickness
penguin_edges <- penguin_edges |>
  arrange(desc(value))

# Get only the top 300 edges
penguin_edges <- penguin_edges[1:500,]

# Create the network nodes

# Get unique nodes from the edges dataframe and combine them
penguin_nodes_from <- data.frame(id = unique(penguin_edges$from))
penguin_nodes_to <- data.frame(id = unique(penguin_edges$to))
penguin_nodes <- bind_rows(penguin_nodes_from, penguin_nodes_to)

# Retain unique nodes in case nodes are repeated in `from` and `to` columns
penguin_nodes <- unique(penguin_nodes)

# Add color to the nodes dataframe based on the Species value 

# Get species for the legend
palmerpenguins::penguins |>
  filter(sex != "female") |>
  drop_na() |>
  select(species) ->
  penguin_species

penguin_species$id <- rownames(penguin_species)

# Join species information to the nodes dataframe
penguin_nodes |>
  left_join(penguin_species, by = "id") ->
  penguin_nodes


# Rename the 'species.y' column to 'group'
penguin_nodes |>
  rename(group = species) ->
  penguin_nodes                     

# Create the network
penguin_network <- visNetwork(penguin_nodes,
  penguin_edges,
  improvedLayout =TRUE,
  main = list(
    text = "Palmer Penguins (Male) Network\nTop 500 Network Connections",
    style = "font-family:Roboto;text-align:center;font-weight:bold;font-size:18px"
  ),
  submain = list(
    text = "@RProDigest (X Handle)",
    style = "font-family:Roboto;text-align:center;font-weight:bold"
  )
) %>%
  visGroups(groupname = "Adelie", color = "#D55E00") |> 
  visGroups(groupname = "Chinstrap", color = "#0072B2") |> 
  visGroups(groupname = "Gentoo", color = "#009E73") |> 
  visLegend(
    width = 0.1, position = "right",
    main = list(
      text = "Penguins",
      style = "font-family:Roboto;text-align:center;font-weight:bold"
    ),
    stepY = 100,
    stepX = 50
  ) |>
  visLayout(randomSeed = 2023) |>
  visOptions(highlightNearest =TRUE)

# Display the network
penguin_network
