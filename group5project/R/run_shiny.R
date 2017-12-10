#' @export
Oral_Health = function(){
  appDir = system.file("Shiny", package = "group5project")
  shiny::runApp(appDir, display.mode = "normal")
}
