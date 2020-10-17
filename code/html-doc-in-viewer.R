# this function opens a self-contained html document in the RStudio viewer
htmldoc_in_viewer <- function(html_file) {
  html_file_name <- basename(html_file)
  tempdir <- tempfile()
  dir.create(tempdir)
  file.copy(html_file, paste0(tempdir, "/", html_file) )
  rstudioapi::viewer(paste0(tempdir, "/", html_file) )
}