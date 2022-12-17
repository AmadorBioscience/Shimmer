############################################
## Container for documentation
UIdocumentation <- function() {
  
  shiny::tags$iframe(src = ("_book/index.html"), frameborder="0", style="overflow:hidden;height:100vh;width:100%")
  
  
}