# Purpose: Extract ESSAS website information
# Author: Bia Dias
# Date: 6/24/2024
#---------------------------------
#Notes: I did not used polite:: package, but I would suggest to use it

# Scrape Website rvest package

require("rvest")
library(tidyverse)
library(here)

# URL for scraping
url <- "https://essas.arc.hokudai.ac.jp"

# Read the HTML structure
page <- read_html(url)

# Extract links
links <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  unique()

# Filter internal links
internal_links <- links[grep("^https?://essas.arc.hokudai.ac.jp", links)]

# Initialize the sitemap
sitemap <- data.frame(Page = character(),
                      Parent = character(),
                      stringsAsFactors = FALSE)

#-------------------------------------------
# Function to recursively build sitemap
build_sitemap <- function(url, parent = NULL) {
  # Print current URL being processed (for debugging)
  print(paste("Processing:", url))
  
  # Read HTML content from the URL
  page <- read_html(url)
  
  # Extract links on the current page
  links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique()
  
  # Filter internal links (adjust domain as needed)
  internal_links <- links[grep("^https?://essas.arc.hokudai.ac.jp", links)]
  
  # Add current page to sitemap
  new_entry <- data.frame(Page = url,
                          Parent = parent,
                          stringsAsFactors = FALSE)
  sitemap <<- rbind(sitemap, new_entry)
  
  # Recursively build sitemap for child pages
  for (link in internal_links) {
    if (!(link %in% sitemap$Page)) {
      build_sitemap(link, parent = url) # Set current URL as parent for child links
    }
  }
}

# Replace with the URL of the website you want to scrape
start_url <- "https://essas.arc.hokudai.ac.jp"

# Start building the sitemap from the homepage
build_sitemap(start_url, parent = start_url)

# Display the resulting sitemap (for debugging)
print(sitemap)


#4. Visualize the Sitemap

require(DiagrammeR)

# Create a graph
sitemap_graph <- create_graph()

# Add nodes to the graph
for (node in sitemap$Page) {
  sitemap_graph <- add_node(sitemap_graph, label = node)
}

# Add edges
for (i in 1:nrow(sitemap)) {
  if (!is.na(sitemap[i, "Parent"])) {
    sitemap_graph <- add_edge(sitemap_graph, from = sitemap[i, "Parent"], to = sitemap[i, "Page"])
  }
}

# Display the resulting graph (optional)
render_graph(sitemap_graph)

sitemap_graph_i <- to_igraph(sitemap_graph)

#5. Add the site text to nodes and edges

library(visNetwork)
library(googledrive)
library(igraph)

# Function to extract text from a page
extract_text <- function(url) {
  page <- read_html(url)
  text <- page %>%
    rvest::html_nodes("p") %>%
    rvest::html_text() %>%
    paste(collapse = " ")
  
  return(text)
}


# Add text to sitemap dataframe
sitemap$Text <- sapply(sitemap$Page, function(url) {
  extract_text(url)
})

#save local
write.csv(sitemap, file = "ESSAS_sitemap.csv")

# Save into google sheets

library(googledrive)

drive_auth()
tmp_csv <- tempfile(fileext = ".csv")
write.csv(sitemap, tmp_csv, row.names = FALSE)

drive_file <- drive_upload(tmp_csv, name = "ESSAS_sitemap.csv", type = "text/csv")
drive_file
