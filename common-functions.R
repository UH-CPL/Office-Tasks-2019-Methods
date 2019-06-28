default_width = 12
default_height = 10



convert_to_csv <- function(df, file_name) {
  write.table(df, file = file.path(file_name), row.names=F, sep = ',')
}

is_match <- function(str, pattern) { 
  return(grepl(pattern, str)) 
} 

print_msg <- function(msg) {
  print(msg)
  message(msg)
}

replace_to_underscore <- function(str) {
  gsubfn('.', list('.' = '_', ' ' = '_', '-' = '_'), tolower(str))
}

replace_to_space <- function(str) {
  gsubfn('.', list('_' = ' ', '-' = ' '), str)
}


save_plot <- function(plot_name, plot, width=default_width, height=default_height) {
  plot_path <- file.path(plots_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=width, height=height)
  
  plot_path <- file.path(plots_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=width, height=height)
}