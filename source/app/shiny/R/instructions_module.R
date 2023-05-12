instructions_module <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Introduction"),
    "This calculator supports sample size estimation for, and analysis of modified Rankin Scale outcome data for stroke research.",
    h2("Using the calculator"),
    "Instructions are shown at each component of the calculator",
    "as they are relevant.",
    bsCollapse(id = "instructions_topLevel_help", open = "Help",
               bsCollapsePanel("Help",
                               "Boxes like this one contain instructions and descriptions throuhgout the calculator.",
                               "You can show and hide this information by clicking \"Help\" above."
                               , style="info")
    ),
    h2("Downloadable version"),
    "An offline version of this tool can be downloaded from ",
    tags$a("here",href="https://github.com/HannahJohns/StrokeTournamentMethodsApp/releases",target="_blank"),
    ". Please note that the downloadable version of the tool only works on Windows.",
    h2("Plots"),
    "Please note that for security purposes, this calculator does not produce
    scaleable images in .tiff, .eps, .pdf or similar formats.
    While we provide controls enabling high-resolution plots, they may not be
    suitable for publication depending on the requirements of a specific journal.
    To generate figures that may be saved as in a scaleable format, we recommend
    using the ", tags$a(href="https://cran.r-project.org/web/packages/rankinPlot/","rankinPlot"),
    "package, which was developed for and is used by this tool.",
    "It will produce equivalent figures in R on your local machine that may then be saved in whatever format suits your needs.",
    h2("Credits"),
    tags$em("Dr. Hannah Johns"),tags$br(),
    tags$em("Prof. Bruce Campbell"),tags$br(),
    tags$em("Dr. Guillaume Turc"),tags$br(),
    tags$em("Prof. Leonid Churilov"),
    h2("Resources and Contact Information"),
    tags$a(href="https://github.com/HannahJohns/StrokeTournamentMethodsApp",target="_target","Link to github"), tags$br(),
    # "Link to paper",tags$br(),
    tags$br(),
    tags$a(href="mailto:hannah.johns@unimelb.edu.au","hannah.johns@unimelb.edu.au")
  )
}
