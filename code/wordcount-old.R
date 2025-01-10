# function for calculating number of words in R Markdown document
# regular expression in gsub removes all YAML, code chunks and inline code
# requires the magrittr package for the pipe operator
wordcount <- function(rmd) {
  if ( !("magrittr" %in% installed.packages() ) ) {stop("Please install magrittr package") }
  library(magrittr)
  readChar(rmd, 
           nchars = file.info(rmd)$size) %>% 
    gsub("---.*?---|```.*?```|`r.*?`|##", "", x = .) %>% 
    strsplit( "[[:space:]]+") %>% 
    unlist() %>% 
    length()
}