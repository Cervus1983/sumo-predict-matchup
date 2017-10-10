library(rvest)
library(tidyverse)

sumodbBanzukeSimpleQuery <- function(basho) {
  raw_html <- tryCatch(
    read_html(paste0("http://sumodb.sumogames.de/Banzuke.aspx?b=", basho, "&h=on&c=on&simple=on")),
		error = function(e) {},
		warning = function(w) {}
  )

  df <- tryCatch(
    raw_html %>% 
      html_node("table.banzuke") %>% 
      html_table() %>% 
      set_names(tolower(colnames(.))),
		error = function(e) {},
		warning = function(w) {}
  )
  
  if (ncol(df) == 5) df
}
