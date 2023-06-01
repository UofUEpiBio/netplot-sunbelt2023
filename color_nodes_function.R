# Function to extract vertex attribute from igraph object
# and use it to color the nodes in the plot
# Input: igraph object, vertex attribute name
# Output: plot of the graph with colored nodes
# TODO:
# 1. Figure out if the attribute is a factor, numeric, or a character (all should be able to be graphed)
# 2. Add an argument to specify a color palette
# 3. Explore how to work with formulas. Ideally, we need the following
#    a. Read the formula
#    b. Capture the elements. For now, all should be vertex attributes
#    c. Extract the vertex attributes from the graph
#    d. If it doesn't exist, throw an error using the stop() function.
# 4. Go over the netplot_edge_formulae function.
#------------------------------------------------------------
# Download packages
# install.packages("igraph")
# install.packages("RColorBrewer")
# install.packages("tidyverse")

# Load required packages
library(igraph)
library(RColorBrewer)
library(tidyverse)
library(netplot)

color_nodes <- function(g, attribute, palette = "Set 1") {
  
  # Check that attribute exists
  if (!(attribute %in% names(vertex_attr(g)))) {
    stop("Attribute does not exist in graph")
  }
  
  # Check attribute type
  value <- igraph::vertex_attr(g, name = attribute)
  attr_type <- class(value) 
  
  if (attr_type == "character") {
    value <- as.factor(value)
    attr_type <- "factor"
  }
  
  # Check that color palette exists
  if (!(palette %in% grDevices::palette.pals())) {
    palette <- "Set 1"
    message("Invalid palette name, using Set1")
  }
  palette <- grDevices::palette.colors(palette = palette) 
  
  # Handle factors
  if (attr_type == "factor") {
    
    # Extract factor levels
    levels <- levels(value) 
    
    # Map levels to colors
    col_map <- palette[1:length(levels)]  
    
    # Color nodes
    value <- col_map[value]
  }  
  
  # Handle numerics 
  else if (attr_type == "numeric") {
    
    # Find min and max
    attr_min <- min(value)
    attr_max <- max(value)  
    
    # Create color scale
    value <- colorRamp(palette.colors())(
      (attr_min:attr_max - attr_min)/(attr_max - attr_min)
    )  
    
    # Color nodes based on attribute value
    value <- grDevices::rgb(value, maxColorValue = 255)
  }
  
  # Handle logicals
  else if (attr_type == "logical") {
    
    # Map TRUE/FALSE to colors
    col_map <- c("blue", "red")  
    
    # Color nodes 
    value <- col_map[as.integer(value) + 1]
  }
  
  # Handle other types (characters, dates)
  else {
    stop("Attribute type not supported")
  }  
  
  
  
  # Use layout_in_circle() for igraph < 0.7.0
  if (packageVersion("igraph") < "0.7.0") {
    nplot(g, vertex.color = value,  layout = layout_in_circle(g))
  } else {
    nplot(g, vertex.color = value, layout = layout_nicely(g))
  }
}


# Factor attribute
g1 <- graph_from_data_frame(d = data.frame(from = c("1", "2", "3"),  
                                           to = c("2", "3", "1")),  
                            directed = FALSE)

# Assign unique vertex names
set_vertex_attr(g1, "name", value = c("v1", "v2", "v3"))

# Add group attribute to graph as a vertex attribute
vertex_attr(g1)$group <- c("group1", "group2", "group3")

# Color nodes by group attribute
color_nodes(g1, "group")





# Numeric attribute
g2 <- graph_from_data_frame(d = data.frame(from = c(1, 2, 3), 
                                           to = c(2, 3, 1)), 
                            directed = FALSE)
V(g2)$value <- c(1, 3, 2)
color_nodes(g2, "value", "Blues")



# Logical attribute
g3 <- graph_from_data_frame(d = data.frame(from = c(1, 2, 3), 
                                           to = c(2, 3, 1)), 
                            directed = FALSE)
V(g3)$selected <- c(TRUE, FALSE, TRUE)
color_nodes(g3, "selected")



# Invalid palette name 
g4 <- graph_from_data_frame(d = data.frame(from = c(1, 2, 3), 
                                           to = c(2, 3, 1)), 
                            directed = FALSE) 
V(g4)$group <- factor(c("A", "A", "B"))
color_nodes(g4, "group", "InvalidPalette") 



# Attribute that does not exist
color_nodes(g1, "fake_attr")

