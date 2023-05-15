#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)


source("R/instructions_module.R")

# Generate dynamic number of stratum following the approach
# given here:
# Taken from https://gist.github.com/wch/5436415/

# Define UI for application that draws a histogram
ui <- fluidPage(
    includeCSS("www/styles.css"),
    # Application title
    titlePanel("Tournament Methods for Stroke Research"),
    fluidRow(column(width=12,
                    tags$h4(tags$em("Developed by the ", tags$a("Australian Stroke Alliance",href="https://austrokealliance.org.au/",target="_target")))
                    )),
    hr(),
    tabsetPanel(type = "tabs",
        tabPanel("Overview",
                 instructions_module("overview")
        ),
        tabPanel("Power Analysis",
                 bsCollapse(id = "power_topLevel_help",
                            bsCollapsePanel("Help",
                                            "This tab facilitates sample size estimation.",
                                            "Here, you can specify the name of your control/treatment groups,",
                                            "as well as significance level (α) and Power.",
                                            tags$br(),
                                            "You can also specify the number of stratum here. Strata can either be used",
                                            "to specify different mRS distributions in different subgroups",
                                            "or to conveniently compare multiple sample size results at once with different",
                                            "assumptions.",
                                            tags$br(),
                                            "If you wish to combine certain mRS categories, select them here. Please note that",
                                            "the calculator provides all mRS options when specifying assumed treatment effect.",
                                            "If you wish to e.g. combine mRS 5 and 6, you can set the mRS 6 field to be empty and",
                                            "use the mRS 5 category for combined mRS 5-6 inputs."
                                            , style="info")
                 ),
                         fluidRow(column(width=2, offset=1,
                                           textInput("power_controlName","Control group name",value="Control")
                                   ),
                                   column(width=2,
                                          textInput("power_treatmentName","Treatment group name",value="Treatment")
                                   ),
                                   column(width=6, offset=1,
                                          sliderInput("power_nStrata","Number of Strata",min=1,max=10,value=1,step=1)
                                   ),
                                  fluidRow(
                                    column(width=3,offset=1,numericInput("power_alpha","Significance level (α)","0.05")),
                                    column(width=3,numericInput("power_power","Power","0.8"))
                                  )
                         ),
                 fluidRow(column(width=12,checkboxGroupInput("power_merge","",
                                                             choices = c("Combine mRS 0-1",
                                                                         "Combine mRS 1-2",
                                                                         "Combine mRS 2-3",
                                                                         "Combine mRS 3-4",
                                                                         "Combine mRS 4-5",
                                                                         "Combine mRS 5-6"
                                                             ),inline = T))
                 ),
          tags$hr(),
          tags$h3("Sample Size Assumptions"),
          bsCollapse(id = "power_effect_help",
                     bsCollapsePanel("Help",
                                     "To determine power, you also need to specify the effect size. This tool provides four methods for doing this.",
                                     tags$br(),
                                     tags$br(),
                                     tags$b("Direct Specification of mRS"),
                                     " - Specify the expected control and treatment arm mRS distributions directly.",
                                     tags$br(),
                                     tags$b("Counterfactual Approach"),
                                     " - Specify the expected control arm mRS distributions directly, and assume the proportion of treatment patients who have their mRS outcome change.",
                                     tags$br(),
                                     tags$b("Proportional Odds Approach"),
                                     " - Specify the expected control arm mRS distribution and, assuming the proportional odds assumption is correct, specify a common odds ratio.",
                                     tags$br(),
                                     tags$b("Direct Pair Count Approach"),
                                     " - Specify the expected proportion of pairs where treatment is better than control, where control is better than treatment, and where there is no difference in outcome.",
                                     tags$br(),
                                     tags$br(),
                                     "For each strata, you may also specify a strata name and weight.",
                                     "The weight of the strata indicates the relative size of each strata.",
                                     "For example, if two strata have weights of 1 and 1, then their sizes",
                                     "will be equal. You may equivalently specify the proportion of patients",
                                     "belonging to each strata (e.g. weights of 0.5 and 0.5 instead).",
                                     "Please note that sample size estimation in this calculator currently assumes",
                                     "an equal allocation of patients to control and treatment arms.",
                                     tags$br(),
                                     tags$br(),
                                     "You may specify these distributions using expected counts of patients in each group",
                                     "(e.g. if you are using existing data to inform your distributions)",
                                     "or can instead provide this information as percentages or proportions.",
                                     "The calculator will convert these inputs for you.",
                                     style="info")
          ),
           tabsetPanel(type = "tabs",id="power_tab_method",
                 tabPanel("Direct Specification of mRS",
                          bsCollapse(id = "power_effect_direct_help",
                                     bsCollapsePanel("Help",
                                                     "To determine the required sample size,",
                                                     "specify the anticpated mRS distributions in control and treatment arms here.",
                                                     style="info")
                          ),
                         fluidRow(
                           uiOutput("power_strata")
                         )
                 ),
                 tabPanel("Counterfactual Approach",
                          bsCollapse(id = "power_effect_sg_help",
                                     bsCollapsePanel("Help",
                                                     "To determine the required sample size,",
                                                     "Specify the control arm distribution",
                                                     "and fill in the counterfactual table.",
                                                     tags$br(),
                                                     tags$br(),
                                                     "This table specifies the assumed distribution of participants",
                                                     "that would have had their mRS outcome change had they recieved",
                                                     "the treatment instead.",
                                                     "As with mRS distributions, this information may either be provided",
                                                     "as percentages/proportions, or instead may be provided",
                                                     "as relative numbers.",
                                                     tags$br(),
                                                     tags$br(),
                                                     "If multiple strata/assumptions are being used,",
                                                     "specify the control distribution for each strata.",
                                                     "The same counterfactual table is used for each",
                                                     "strata.",
                                                     style="info")
                          ),
                          fluidRow(
                            uiOutput("power_sg_control")
                          ),
                          tags$hr(),
                          fluidRow(
                                    column(width=1,offset=1,tags$b("Percentage of mRS 0 under control that would have been:")),
                                    column(width=1,tags$b("Percentage of mRS 1 under Control that would have been:")),
                                    column(width=1,tags$b("Percentage of mRS 2 under Control that would have been:")),
                                    column(width=1,tags$b("Percentage of mRS 3 under Control that would have been:")),
                                    column(width=1,tags$b("Percentage of mRS 4 under Control that would have been:")),
                                    column(width=1,tags$b("Percentage of mRS 5 under Control that would have been:")),
                                    column(width=1,tags$b("Percentage of mRS 6 under Control that would have been:"))
                                   ),
                          fluidRow(
                            column(width=1,tags$b("mRS 0 under Treatment")),
                            column(width=1,numericInput("sg_0_0",label="",value = 1)),
                            column(width=1,numericInput("sg_1_0",label="",value = 1)),
                            column(width=1,numericInput("sg_2_0",label="",value = 1)),
                            column(width=1,numericInput("sg_3_0",label="",value = 1)),
                            column(width=1,numericInput("sg_4_0",label="",value = 1)),
                            column(width=1,numericInput("sg_5_0",label="",value = 1)),
                            column(width=1,numericInput("sg_6_0",label="",value = 1))
                          ),
                          fluidRow(
                            column(width=1,tags$b("mRS 1 under Treatment")),
                            column(width=1,numericInput("sg_0_1",label="",value = 1)),
                            column(width=1,numericInput("sg_1_1",label="",value = 1)),
                            column(width=1,numericInput("sg_2_1",label="",value = 1)),
                            column(width=1,numericInput("sg_3_1",label="",value = 1)),
                            column(width=1,numericInput("sg_4_1",label="",value = 1)),
                            column(width=1,numericInput("sg_5_1",label="",value = 1)),
                            column(width=1,numericInput("sg_6_1",label="",value = 1))
                          ),
                          fluidRow(
                            column(width=1,tags$b("mRS 2 under Treatment")),
                            column(width=1,numericInput("sg_0_2",label="",value = 1)),
                            column(width=1,numericInput("sg_1_2",label="",value = 1)),
                            column(width=1,numericInput("sg_2_2",label="",value = 1)),
                            column(width=1,numericInput("sg_3_2",label="",value = 1)),
                            column(width=1,numericInput("sg_4_2",label="",value = 1)),
                            column(width=1,numericInput("sg_5_2",label="",value = 1)),
                            column(width=1,numericInput("sg_6_2",label="",value = 1))
                          ),

                          fluidRow(
                            column(width=1,tags$b("mRS 3 under Treatment")),
                            column(width=1,numericInput("sg_0_3",label="",value = 1)),
                            column(width=1,numericInput("sg_1_3",label="",value = 1)),
                            column(width=1,numericInput("sg_2_3",label="",value = 1)),
                            column(width=1,numericInput("sg_3_3",label="",value = 1)),
                            column(width=1,numericInput("sg_4_3",label="",value = 1)),
                            column(width=1,numericInput("sg_5_3",label="",value = 1)),
                            column(width=1,numericInput("sg_6_3",label="",value = 1))
                          ),

                          fluidRow(
                            column(width=1,tags$b("mRS 4 under Treatment")),
                            column(width=1,numericInput("sg_0_4",label="",value = 1)),
                            column(width=1,numericInput("sg_1_4",label="",value = 1)),
                            column(width=1,numericInput("sg_2_4",label="",value = 1)),
                            column(width=1,numericInput("sg_3_4",label="",value = 1)),
                            column(width=1,numericInput("sg_4_4",label="",value = 1)),
                            column(width=1,numericInput("sg_5_4",label="",value = 1)),
                            column(width=1,numericInput("sg_6_4",label="",value = 1))
                          ),

                          fluidRow(
                            column(width=1,tags$b("mRS 5 under Treatment")),
                            column(width=1,numericInput("sg_0_5",label="",value = 1)),
                            column(width=1,numericInput("sg_1_5",label="",value = 1)),
                            column(width=1,numericInput("sg_2_5",label="",value = 1)),
                            column(width=1,numericInput("sg_3_5",label="",value = 1)),
                            column(width=1,numericInput("sg_4_5",label="",value = 1)),
                            column(width=1,numericInput("sg_5_5",label="",value = 1)),
                            column(width=1,numericInput("sg_6_5",label="",value = 1))
                          ),

                          fluidRow(
                            column(width=1,tags$b("mRS 6 under Treatment")),
                            column(width=1,numericInput("sg_0_6",label="",value = 1)),
                            column(width=1,numericInput("sg_1_6",label="",value = 1)),
                            column(width=1,numericInput("sg_2_6",label="",value = 1)),
                            column(width=1,numericInput("sg_3_6",label="",value = 1)),
                            column(width=1,numericInput("sg_4_6",label="",value = 1)),
                            column(width=1,numericInput("sg_5_6",label="",value = 1)),
                            column(width=1,numericInput("sg_6_6",label="",value = 1))
                          )
                          # tags$hr(),
                          # fluidRow(column(width=3,"Percentage of mRS 0 under control that would have been:"),
                          #          column(width=8, splitLayout(
                          #   numericInput("sg_0_0",label="mRS 0 under Treatment",value = 1),
                          #   numericInput("sg_0_1",label="mRS 1 under Treatment",value = 0),
                          #   numericInput("sg_0_2",label="mRS 2 under Treatment",value = 0),
                          #   numericInput("sg_0_3",label="mRS 3 under Treatment",value = 0),
                          #   numericInput("sg_0_4",label="mRS 4 under Treatment",value = 0),
                          #   numericInput("sg_0_5",label="mRS 5 under Treatment",value = 0),
                          #   numericInput("sg_0_6",label="mRS 6 under Treatment",value = 0)
                          #   ))
                          # ),
                          # fluidRow(column(width=3,"Percentage of mRS 1 under control that would have been:"),
                          #          column(width=8,splitLayout(
                          #   numericInput("sg_1_0",label="mRS 0 under Treatment",value = 0),
                          #   numericInput("sg_1_1",label="mRS 1 under Treatment",value = 1),
                          #   numericInput("sg_1_2",label="mRS 2 under Treatment",value = 0),
                          #   numericInput("sg_1_3",label="mRS 3 under Treatment",value = 0),
                          #   numericInput("sg_1_4",label="mRS 4 under Treatment",value = 0),
                          #   numericInput("sg_1_5",label="mRS 5 under Treatment",value = 0),
                          #   numericInput("sg_1_6",label="mRS 6 under Treatment",value = 0)
                          # ))),
                          # fluidRow(column(width=3,"Percentage of mRS 2 under control that would have been:"),
                          #          column(width=8,splitLayout(
                          #   numericInput("sg_2_0",label="mRS 0 under Treatment",value = 0),
                          #   numericInput("sg_2_1",label="mRS 1 under Treatment",value = 0),
                          #   numericInput("sg_2_2",label="mRS 2 under Treatment",value = 1),
                          #   numericInput("sg_2_3",label="mRS 3 under Treatment",value = 0),
                          #   numericInput("sg_2_4",label="mRS 4 under Treatment",value = 0),
                          #   numericInput("sg_2_5",label="mRS 5 under Treatment",value = 0),
                          #   numericInput("sg_2_6",label="mRS 6 under Treatment",value = 0)
                          # ))),
                          # fluidRow(column(width=3,"Percentage of mRS 3 under control that would have been:"),
                          #          column(width=8,splitLayout(
                          #   numericInput("sg_3_0",label="mRS 0 under Treatment",value = 0),
                          #   numericInput("sg_3_1",label="mRS 1 under Treatment",value = 0),
                          #   numericInput("sg_3_2",label="mRS 2 under Treatment",value = 0),
                          #   numericInput("sg_3_3",label="mRS 3 under Treatment",value = 1),
                          #   numericInput("sg_3_4",label="mRS 4 under Treatment",value = 0),
                          #   numericInput("sg_3_5",label="mRS 5 under Treatment",value = 0),
                          #   numericInput("sg_3_6",label="mRS 6 under Treatment",value = 0)
                          # ))),
                          # fluidRow(column(width=3,"Percentage of mRS 4 under control that would end up as:"),
                          #          column(width=8,splitLayout(
                          #   numericInput("sg_4_0",label="mRS 0 under Treatment",value = 0),
                          #   numericInput("sg_4_1",label="mRS 1 under Treatment",value = 0),
                          #   numericInput("sg_4_2",label="mRS 2 under Treatment",value = 0),
                          #   numericInput("sg_4_3",label="mRS 3 under Treatment",value = 0),
                          #   numericInput("sg_4_4",label="mRS 4 under Treatment",value = 1),
                          #   numericInput("sg_4_5",label="mRS 5 under Treatment",value = 0),
                          #   numericInput("sg_4_6",label="mRS 6 under Treatment",value = 0)
                          # ))),
                          # fluidRow(column(width=3,"Percentage of mRS 5 under control that would have been:"),
                          #          column(width=8,splitLayout(
                          #   numericInput("sg_5_0",label="mRS 0 under Treatment",value = 0),
                          #   numericInput("sg_5_1",label="mRS 1 under Treatment",value = 0),
                          #   numericInput("sg_5_2",label="mRS 2 under Treatment",value = 0),
                          #   numericInput("sg_5_3",label="mRS 3 under Treatment",value = 0),
                          #   numericInput("sg_5_4",label="mRS 4 under Treatment",value = 0),
                          #   numericInput("sg_5_5",label="mRS 5 under Treatment",value = 1),
                          #   numericInput("sg_5_6",label="mRS 6 under Treatment",value = 0)
                          # ))),
                          # fluidRow(column(width=3,"Percentage of mRS 6 under control that would have been:"),
                          #          column(width=8,splitLayout(
                          #   numericInput("sg_6_0",label="mRS 0 under Treatment",value = 0),
                          #   numericInput("sg_6_1",label="mRS 1 under Treatment",value = 0),
                          #   numericInput("sg_6_2",label="mRS 2 under Treatment",value = 0),
                          #   numericInput("sg_6_3",label="mRS 3 under Treatment",value = 0),
                          #   numericInput("sg_6_4",label="mRS 4 under Treatment",value = 0),
                          #   numericInput("sg_6_5",label="mRS 5 under Treatment",value = 0),
                          #   numericInput("sg_6_6",label="mRS 6 under Treatment",value = 1)
                          # )))
                    ),
                    tabPanel("Proportional odds approach",
                             bsCollapse(id = "power_effect_polr_help",
                                        bsCollapsePanel("Help",
                                                        "To determine the required sample size,",
                                                        "Specify the control arm distribution",
                                                        "and specify a common odds ratio (the effect estimate returned by ordinal logistic regression).",
                                                        tags$br(),
                                                        "Please note that this method assumes",
                                                        "that the proportional odds assumption is true,",
                                                        "i.e. that the dichotomous odds ratio at each possible cutpoint is constant",
                                                        "This assumption may be inaccurate when positive treatment effect is concentrated",
                                                        "at one end of the mRS spectrum, or treatment may have an adverse effect on",
                                                        "some patients",
                                                        style="info")
                             ),
                             fluidRow(
                               fluidRow(
                                 uiOutput("power_polr_control")
                               )
                             ),
                             fluidRow(
                               column(width=2,offset=1,
                                  numericInput("power_polr_odds","Common Odds Ratio",value = 1)
                                )
                               )

                    ),
                    tabPanel("Direct Pair Count approach",
                             bsCollapse(id = "power_effect_pairs_help",
                                        bsCollapsePanel("Help",
                                                        "Unlike the other three methods",
                                                        "this approach to sample size calculation",
                                                        "requires you to specify the proportion of pairs where",
                                                        "the treatment paitent wins, loses wins, losses or is tied.",
                                                        "This method may be used for any outcome data, including",
                                                        "ordinal scales and complex, multivariate outcomes.",
                                                        tags$br(),
                                                        "Please note that at present, stratification is not supported for this method.",
                                                        "Strata may only be used for sensitivity analysis.",
                                                        style="info")
                             ),
                             fluidRow(
                               uiOutput("power_pairs_control")
                             )
                    )
                ),
                tags$hr(),
                tags$h3("Sample Size Results"),
                conditionalPanel("input.power_tab_method != 'Direct Pair Count approach'",
                bsCollapse(id = "power_results_help_rankin",
                           bsCollapsePanel("Help",
                                           "Sample size requried results are given here.",
                                           "If multiple strata were used, they may either be",
                                           "treated as specific subgroups within the trial,",
                                           "or may be used to specify different assumptions used",
                                           "for sample size estimation",
                                           "The results shown, and the sample size formulas used, will reflect this choice.",
                                           tags$br(),
                                           "Sample sizes are reported as a total number of patients.",
                                           "Divide the results below by 2 to get the per-arm sample size.",
                                           tags$br(),
                                           "The list of sample size formulas provided are:",
                                           tags$br(), tags$b("Gen OR:"),
                                           tags$a(href="https://support.sas.com/resources/papers/proceedings/proceedings/sugi31/209-31.pdf",target="_target",
                                                  "O’Brien, Ralph G., and John Castelloe.",
                                                  "\"Exploiting the link between the Wilcoxon-Mann-Whitney test and a simple odds statistic.\"",
                                                  tags$em("Proceedings of the Thirty-first Annual SAS Users Group International Conference."),
                                                  "Cary, NC: SAS Institute, 2006."),
                                           tags$br(), tags$b("WinP:"),
                                           tags$a(href="https://doi.org/10.1161/STROKEAHA.121.037744",target="_target",
                                             "Zou G., Zou L., and Choi Y-H.",
                                             "\"A Distribution-Free Approach to the Design and Analysis of Randomized Stroke Trials with the Modified Rankin Scale.\"",
                                             tags$em("Stroke"),"(2022)"
                                           ),
                                           tags$br(), tags$b("Win Ratio:"), tags$a(
                                             href="https://doi.org/10.1002/sim.9297",target="_target",
                                             "Yu, Ron Xiaolong, and Jitendra Ganju.,",
                                             "\"Sample size formula for a win ratio endpoint.\"",
                                             tags$em("Statistics in medicine"), "41.6 (2022): 950-963."
                                           ),
                                           tags$br(), tags$b("Wilcoxon-Mann-Whitney:"),
                                           tags$a(href="https://doi.org/10.1002/sim.4780122404",target="_target",
                                                  "Whitehead, John.",
                                                  "\"Sample size calculations for ordered categorical data.\"",
                                                  tags$em("Statistics in medicine"),
                                                  "12.24 (1993): 2257-2271."
                                                  ),
                                           tags$br(), tags$b("van Elteren test (stratified WMW):"),
                                           tags$a(href="https://doi.org/10.1080/10543400802369020",target="_target",
                                                  "Zhao, Yan D., Dewi Rahardja, and Yajun Mei.",
                                                  "\"Sample size calculation for the van Elteren test adjusting for ties.\"",
                                                  tags$em("Journal of Biopharmaceutical Statistics"), "18.6 (2008): 1112-1119."
                                                  ),
                                           tags$br(), tags$b("Tang's exact WMW power formula:"),
                                           tags$a(href="https://doi.org/10.1002/sim.4407",target="_target",
                                                  "Tang, Yongqiang",
                                                  "\"Size and power estimation for the Wilcoxon–Mann–Whitney test for ordered categorical data.\"",
                                                  tags$em("Statistics in Medicine"), "30.29 (2011): 3461-3470."
                                           ),
                                    style="info")
                ),
                fluidRow(
                  column(width=6,offset=1,radioButtons("power_strata_method",label = "Treat strata as:",choices = c("Actual strata for a stratified trial"="strata",
                                                                                                                    "Different assumptions for sensitivity analysis"="sensitivity")))
                )
                ),
                conditionalPanel("input.power_tab_method == 'Direct Pair Count approach'",
                                 bsCollapse(id = "power_results_help_pairs",
                                             bsCollapsePanel("Help",
                                                             "Sample size requried results are given here.",
                                                             "For the pair count method, strata are treated as",
                                                             "different assumptions for power analysis.",
                                                             "The sample size formula used here is given by:",
                                                             tags$br(),
                                                             tags$a(
                                                               href="https://doi.org/10.1002/sim.9297",target="_target",
                                                             "Yu, Ron Xiaolong, and Jitendra Ganju.,",
                                                             "\"Sample size formula for a win ratio endpoint.\"",
                                                             tags$em("Statistics in medicine"), "41.6 (2022): 950-963."
                                                             ),
                                                      style="info")
                           )
          ),
                fluidRow(
                  column(width=6,offset=1,tableOutput("power_results"))
                ),
                conditionalPanel("input.power_tab_method != 'Direct Pair Count approach'",
                tags$hr(),
                bsCollapse(id = "power_plots_help",
                           bsCollapsePanel("Help",
                                           "Plots summarising the treatment effect are shown here.",
                                           "Use this to help with validating the reasonableness of your assumptions and interpret",
                                           "the results when investigating the sensitivity of sample size estimation.",
                                           style="info")
                ),
                fluidRow(
                  column(width=3,
                         numericInput("power_image_width","Image Width (pixels, max 3600)",min=1,max=3600, value=600)
                  ),
                  column(width=3,
                         numericInput("power_image_height","Image Height (pixels, max 3600)",min=1,max=3600, value=600)
                  )
                ),
                fluidRow(
                  column(width=3,
                         radioButtons("power_image_colorScheme","Plot colour scheme",
                                      choices=c("Colour"="lowGreen","Grayscale"="grayscale")
                         )
                  )
                ),
                tabsetPanel(type="tabs",
                            tabPanel("Grotta Bar",
                                     bsCollapse(id = "analysis_plots_grottaBar_help",
                                                bsCollapsePanel("Help",
                                                                "This tab produces a Grotta bar summarising the assumptions used",
                                                                "for power analysis",
                                                                "The figure may be customised with the following options:",
                                                                tags$br(),
                                                                tags$br(),
                                                                tags$p(tags$b("Bar Width"),
                                                                       "How thick the grotta bars are. Use to adjust the space between",
                                                                       "control and treatment groups."
                                                                ),
                                                                tags$p(tags$b("Display Counts"),
                                                                       "If enabled, print the number of observations in each non-empty mRS category."
                                                                ),
                                                                tags$p(tags$b("Number Size"),
                                                                       "The size of the printed number of observations."
                                                                ),
                                                                tags$p(tags$b("Line Width"),
                                                                       "The thicness of lines in the figure."
                                                                ),
                                                                tags$p(tags$b("Text Size"),
                                                                       "The size of all label text in the figure."
                                                                ),
                                                                style="info")
                                     ),
                                     fluidRow(
                                       column(width=4,
                                              sliderInput("power_grotta_width","Bar Width",min = 0.1, max=1, value = 0.9,step = 0.01)
                                       ),
                                       # column(width=2,sliderInput("grotta_height",label = "Plot height",min=0,max=400, value=300)
                                       # ),
                                       column(width=2,
                                              radioButtons("power_grotta_printNumbers","Display counts",choices = c(
                                                "Proportions"="proportion",
                                                "Percentages" = "percentage",
                                                "None"="none"))
                                       )
                                     ),
                                     fluidRow(
                                       column(width=3,
                                              sliderInput("power_grotta_numberSize","Number Size",min = 1, max=10, value = 5,step = 0.1)
                                       ),
                                       column(width=3,
                                              sliderInput("power_grotta_lineSize","Line Width",min = 0, max=2, value = 0.5,step = 0.1)
                                       ),
                                       column(width=3,
                                              sliderInput("power_grotta_textSize","Text Size",min = 0, max=100, value = 10,step = 0.1)
                                       )
                                     ),
                                     fluidRow(column(width=6,
                                                     uiOutput("power_plots")
                                     ))
                            )
                )
              )
          ),
          tabPanel("Analysis of mRS data",
                   bsCollapse(id = "analysis_toplevel_help",
                              bsCollapsePanel("Help",
                                              "This tab facilitates the analysis of mRS data using Tournament Methods.",
                                              "In this section, you can specify the name of your control/treatment groups.",
                                              "If your data is stratified, specify the number of strata here.",
                                              tags$br(),
                                              "For each strata, give the counts of each mRS outcome in the control and treatment arms.",
                                              tags$br(),
                                              "If you wish to combine certain mRS categories, select them here.",
                                              style="info")
                   ),
                         fluidRow(column(width=2, offset=1,
                                         textInput("controlName","Control group name",value="Control")
                         ),
                         column(width=2,
                                textInput("treatmentName","Treatment group name",value="Treatment")
                         ),
                         column(width=6, offset=1,
                                sliderInput("nStrata","Number of Strata",min=1,max=10,value=1,step=1)
                         )
                         ),
                         fluidRow(column(width=12,checkboxGroupInput("analysis_merge","",
                                                                     choices = c("Combine mRS 0-1",
                                                                                 "Combine mRS 1-2",
                                                                                 "Combine mRS 2-3",
                                                                                 "Combine mRS 3-4",
                                                                                 "Combine mRS 4-5",
                                                                                 "Combine mRS 5-6"
                                                                       ),inline = T))
                        ),
                         tags$hr(),
                         fluidRow(
                             uiOutput("strata")
                         ),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Analysis",
                                              bsCollapse(id = "analysis_results_help",
                                                         bsCollapsePanel("Help",
                                                                         "Tournament Methods analysis results are shown here.",
                                                                         "There are multiple variations on the approach, each of which is supported here.",
                                                                         "Please make choices for the following:",
                                                                         tags$p(tags$br(),tags$b("How ties should be treated"),tags$br(),
                                                                                "Ties may either be dropped from consideration",
                                                                                "or may be split evenly between being considered",
                                                                                "in favour of control or treatment.",
                                                                                "The primary difference between these methods is that",
                                                                                "Dropping ties is assumption-free and results in an effect size measure",
                                                                                "that may be interpreted as",
                                                                                "the odds/probability that a random treatment particiant will",
                                                                                "achieve a better outcome than a random control particiant",
                                                                                tags$em("given that there was a difference in outcomes as measured by mRS."),
                                                                                tags$br(),
                                                                                "Splitting ties results an effect size measure that requires two assumptions:",
                                                                                "First, that ties in outcome do not exist in nature, and, were the mRS to be more granular, all ties could be broken.",
                                                                                "Second, that the null hypothesis (no treatment effect) is true, or in other words, equipoise is maintained.",
                                                                                "Provided that both of these assumptions are met, splitting ties results in",
                                                                                "an effect size measure that may be interpreted as the the odds/probability that a",
                                                                                "random treatment particiant will achieve a better outcome than a random control participant."
                                                                                ),
                                                                         tags$p(tags$b("Method of calculating standard error"),tags$br(),
                                                                                "Standard error may be calculated either assuming that",
                                                                                "the null hypothesis (no treatment effect) is true,",
                                                                                "or without making this assumption.",
                                                                                "This assumption is common to in many statistical tests,",
                                                                                "including the Wicoxon-Mann-Whitney test."
                                                                                ),
                                                                         tags$p(tags$b("Significance (α) level"),tags$br(),
                                                                                "Specify the significance (α) error for determining the width of confidence intervals"
                                                                                ),
                                                                         tags$p(tags$b("Report results as either odds or proportions"),tags$br(),
                                                                                "Results may be reported as either odds (following the approach given by Churilov et. al (2014)) or as proportions (following the approach given by Zou et al. (2022)).",
                                                                                "This does not impact the underling analysis method."
                                                                                ),
                                                                         tags$p("This calculator also provides plain-language explanations of the treatment effect following advice given by Howard et al. (2012).",
                                                                                "These statements may be copied and pasted into your results section to facilitate the reporting and interpretation of Tournament Method outcomes"),
                                                                         style="info")
                                              ),
                                              fluidRow(
                                                  column(width=3,radioButtons("analysis_ties","How ties should be handled",
                                                                                       choices=c("Drop Ties"="drop","Split Ties"="split"))
                                                  ),
                                                  column(width=3,checkboxInput("assumeNull","Assume null hypothesis (no effect) is true",FALSE)
                                                  ),
                                                  column(width=3,numericInput("analysis_alpha","Significance level (α) rate", 0.05)
                                                  ),
                                                  column(width=3,radioButtons("analysis_results","Report results as:",
                                                                              choices=c("Odds"="genodds","Proportion"="winP"))
                                                  )
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                  column(width=6,
                                                                  tableOutput("genodds"),
                                                                  uiOutput("genodds_heterogeneity")
                                                  ),
                                                  column(width=6,
                                                         htmlOutput("howard_statement")
                                                  )
                                              )
                                     ),
                                     tabPanel("Plots",
                                              bsCollapse(id = "analysis_plots_help",
                                                         bsCollapsePanel("Help",
                                                                         "This tab contains plot outputs summarising the results",
                                                                         "In this section, you can control the width and height of the plot (in pixels)",
                                                                         "and also control if the figure should be in colour, or should be grayscale.",
                                                                         "Please note that this calculator does not produce scalar vector graphics",
                                                                         "as required by some journals. Please see the Overview tab for for information",
                                                                         "and alternatives.",
                                                                         style="info")
                                              ),
                                              fluidRow(
                                              column(width=3,
                                                     numericInput("image_width","Image Width (pixels, max 3600)",min=1,max=3600, value=600)
                                                     ),
                                              column(width=3,
                                                numericInput("image_height","Image Height (pixels, max 3600)",min=1,max=3600, value=600)
                                              )
                                              ),
                                              fluidRow(
                                                column(width=3,
                                                       radioButtons("image_colorScheme","Plot colour scheme",
                                                                    choices=c("Colour"="lowGreen","Grayscale"="grayscale")
                                                                    )
                                                       )
                                              ),
                                              tabsetPanel(type="tabs",
                                              tabPanel("Grotta Bar",
                                                       bsCollapse(id = "analysis_plots_grottaBar_help",
                                                                  bsCollapsePanel("Help",
                                                                                  "This tab produces a Grotta bar summarising the results.",
                                                                                  "The figure may be customised with the following options:",
                                                                                  tags$br(),
                                                                                  tags$br(),
                                                                                  tags$p(tags$b("Bar Width"),
                                                                                         "How thick the grotta bars are. Use to adjust the space between",
                                                                                         "control and treatment groups."
                                                                                         ),
                                                                                  tags$p(tags$b("Display Counts"),
                                                                                         "If enabled, print the number of observations in each non-empty mRS category."
                                                                                         ),
                                                                                  tags$p(tags$b("Number Size"),
                                                                                         "The size of the printed number of observations."
                                                                                         ),
                                                                                  tags$p(tags$b("Line Width"),
                                                                                         "The thicness of lines in the figure."
                                                                                         ),
                                                                                  tags$p(tags$b("Text Size"),
                                                                                         "The size of all label text in the figure."
                                                                                         ),
                                                                                  style="info")
                                                       ),
                                                       fluidRow(
                                                           column(width=4,
                                                                  sliderInput("grotta_width","Bar Width",min = 0.1, max=1, value = 0.9,step = 0.01)
                                                           ),
                                                           # column(width=2,sliderInput("grotta_height",label = "Plot height",min=0,max=400, value=300)
                                                           # ),
                                                           column(width=2,
                                                                  radioButtons("grotta_printNumbers","Display counts",choices = c("Counts"="count",
                                                                                                                                  "Proportions"="proportion",
                                                                                                                                  "Percentages" = "percentage",
                                                                                                                                  "None"="none"))
                                                           )
                                                       ),
                                                       fluidRow(
                                                           column(width=3,
                                                                  sliderInput("grotta_numberSize","Number Size",min = 1, max=10, value = 5,step = 0.1)
                                                           ),
                                                           column(width=3,
                                                                  sliderInput("grotta_lineSize","Line Width",min = 0, max=2, value = 0.5,step = 0.1)
                                                           ),
                                                           column(width=3,
                                                                  sliderInput("grotta_textSize","Text Size",min = 0, max=100, value = 10,step = 0.1)
                                                                  )
                                                       ),
                                                       fluidRow(column(width=6,
                                                                       uiOutput("grottaBar")
                                                       ))
                                              ),
                                              tabPanel("Forest Plot",
                                                       bsCollapse(id = "analysis_forestPlot_help",
                                                                  bsCollapsePanel("Help",
                                                                                  "This tab contains a forest plot summarising the pooled treatment effect.",
                                                                                  "See the Analysis tab for an explanation of analysis options",
                                                                                  style="info")
                                                       ),
                                                       fluidRow(
                                                         column(width=2,radioButtons("forestPlot_ties","How ties should be handled",
                                                                                     choices=c("Drop Ties"="drop","Split Ties"="split"))
                                                         ),
                                                         column(width=2,checkboxInput("forestPlot_assumeNull","Assume null hypothesis (no effect) is true",FALSE)
                                                         ),
                                                         column(width=2,numericInput("forestPlot_alpha","Significance level (α)", 0.05)
                                                         ),
                                                         column(width=2,radioButtons("forestPlot_results","Report results as:",
                                                                                     choices=c("Odds"="genodds","Proportion"="winP"))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(width=3,
                                                                sliderInput("forestPlot_pointSize","Point Size",min = 1, max=10, value = 5,step = 0.1)
                                                         ),
                                                         column(width=3,
                                                                sliderInput("forestPlot_textSize","Text Size",min = 0, max=100, value = 10,step = 0.1)
                                                         )
                                                       ),
                                                       fluidRow(column(width=6,
                                                                       uiOutput("forestPlot")
                                                       )
                                                       )
                                              )
                                              )
                                     )

                         )
                )
        )
)

