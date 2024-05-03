library(tidyverse)
library(jsonlite)
cat("Darbinė direktorija:", getwd())
download.file("https://atvira.sodra.lt/imones/downloads/2023/monthly-2023.json.zip", "../data/temp")
unzip("../data/temp",  exdir = "../data/")
readLines("../data/monthly-2023.json", 20)

data = fromJSON("../data/monthly-2023.json")

data %>%
  filter(ecoActCode== 692000) %>%
  saveRDS('../data/692000.rds')





file.remove("../data/temp")
file.remove("../data/monthly-2023.json")
file.remove("../data/monthly-2023.csv")
file.remove("../img/pavyzdys1.png")
file.remove("../img/pavyzdys2.png")
file.remove("../img/pavyzdys3.png")
file.remove("../img/shiny_example.png")
