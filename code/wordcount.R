# function for calculating number of words in R Markdown document
# regular expressions in sub / gsub removes all YAML, code chunks, inline code
# also removes # symbols used to create section headers 
# requires the magrittr package for the pipe operator
wordcount <- function(rmd) {
  if ( !("magrittr" %in% installed.packages() ) ) {stop("Please install magrittr package") }
  library(magrittr)
  readChar(rmd, 
           nchars = file.info(rmd)$size) %>% 
    sub("---.*?---", "", x = .) %>% 
    gsub("```.*?```", "", x = .) %>% 
    gsub("`r.*?`", "", x = .) %>% 
    gsub("#", "", x = .) %>% 
    strsplit( "[[:space:]]+") %>% 
    unlist() %>% 
    length()
}