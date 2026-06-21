# convert a bunch of xml files into csv files
# and then combine them into one csv file
library(XML)
library(tidyverse)
library(stringr)
library(lubridate)
library(xml2)

# set working directory
xmls_path <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/annotations/xmls"
out_path <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/annotations/"


# get all xml files
xmls <- list.files(xmls_path, pattern = ".xml", full.names = TRUE)
# get xml file names
xml_names <- list.files(xmls_path, pattern = ".xml", full.names = FALSE) %>%
  str_remove(".xml") %>%
  str_remove_all(" ")

parse_annotation <- function(xml_file_path) {
  # Read the XML file
  doc <- read_xml(xml_file_path)
  
  # Extract basic info
  folder <- xml_text(xml_find_first(doc, ".//folder"))
  filename <- xml_text(xml_find_first(doc, ".//filename"))
  
  width <- as.numeric(xml_text(xml_find_first(doc, ".//size/width")))
  height <- as.numeric(xml_text(xml_find_first(doc, ".//size/height")))
  depth <- as.numeric(xml_text(xml_find_first(doc, ".//size/depth")))
  
  # Find all <object> nodes
  object_nodes <- xml_find_all(doc, ".//object")
  
  # Extract bounding box info from each <object>
  # We capture name, pose, truncated, difficult, xmin, ymin, xmax, ymax
  object_df <- data.frame(
    name = sapply(object_nodes, function(node) xml_text(xml_find_first(node, ".//name"))),
    pose = sapply(object_nodes, function(node) xml_text(xml_find_first(node, ".//pose"))),
    truncated = sapply(object_nodes, function(node) as.numeric(xml_text(xml_find_first(node, ".//truncated")))),
    difficult = sapply(object_nodes, function(node) as.numeric(xml_text(xml_find_first(node, ".//difficult")))),
    xmin = sapply(object_nodes, function(node) as.numeric(xml_text(xml_find_first(node, ".//bndbox/xmin")))),
    ymin = sapply(object_nodes, function(node) as.numeric(xml_text(xml_find_first(node, ".//bndbox/ymin")))),
    xmax = sapply(object_nodes, function(node) as.numeric(xml_text(xml_find_first(node, ".//bndbox/xmax")))),
    ymax = sapply(object_nodes, function(node) as.numeric(xml_text(xml_find_first(node, ".//bndbox/ymax")))),
    stringsAsFactors = FALSE
  )
  
  # Return everything in a structured list
  return(list(
    folder = folder,
    filename = filename,
    width = width,
    height = height,
    depth = depth,
    objects = object_df
  ))
}

# Usage example (assuming "example.xml" is a file with the content shown):
# annotation_data <- parse_annotation("example.xml")
# print(annotation_data)

# loop through all xml files and parse them
xml_data <- map(xmls, ~ parse_annotation(.x)) 
names(xml_data) <- xml_names
                 
# Get only the blemishes and convert into a data frame
blemishes_data <- map2(xml_data, names(xml_data), ~ as_tibble(.x$objects) %>% 
                         mutate(file = .y, 
                                label = case_when(name == "D00" ~ "longitudinal cracks",
                                                  name == "D10" ~ "transverse cracks",
                                                  name == "D20" ~ "alligator cracks",
                                                  name == "D30" ~ "repaired cracks",
                                                  name == "D40" ~ "potholes",
                                                  name == "D50" ~ "pedestrian crossing blurs",
                                                  name == "D60" ~ "lane line blurs",
                                                  name == "D70" ~ "manhole covers",
                                                  name == "D80" ~ "patchy road sections",
                                                  name == "D90" ~ "rutting",
                                                  TRUE ~ NA),
                                width = .x$width,
                                height = .x$height,
                                depth = .x$depth) ) %>% bind_rows 


readr::write_csv(blemishes_data, 
                 paste0(out_path, "blemishes_data.csv"), 
                 col_names = TRUE )

# No obs for name == 30, 50, 60, 70, 80, 90. Only D00, D10, D20, D40 are present.
blemishes_data %>% filter(name == "D00")

blemishes_data$name %>% unique()

length(xmls) # 4805 labeled images in total
