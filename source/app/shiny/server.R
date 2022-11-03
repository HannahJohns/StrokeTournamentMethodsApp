library(shiny)
library(tidyverse)
library(epitools)
library(rankinPlot)


# TO DO: SEPARATE OUT ALL OF THE INSTRUCTIONS TO HAND-HOLD USER THROUGH EVERYTHING
# USE COLLABSABLE PANELS FOR "IN THIS SECTION YOU CAN ....." EXPLANATIONS THROUHGOUT

# Load functions for analysis/calculations/etc

source("R/power_formulas.R")
source("R/analysis_formulas.R")

# A function for taking instructions on how to combine
# mRS labels and spitting out what the labels should actually be
get_combined_mRS_labels <- function(choices){
  choices <- gsub("Combine mRS ","",choices)
  choices <- strsplit(choices,split = "-")

  x_max <- 0:6
  for(i in choices) x_max <- gsub(i[1],i[2],x_max)

  x_min <- 0:6
  for(i in rev(choices)) x_min <- gsub(i[2],i[1],x_min)

  x_label <- ifelse(x_min==x_max,x_min,
                    paste(x_min,x_max,sep="-")
  )

  x_label
}

collapse_mRS_values <- function(x){
  do.call("rbind",by(x,paste(x$strata,x$mRS),function(tmp){
    data.frame(strata=unique(tmp$strata),
               mRS = unique(tmp$mRS),
               control=sum(tmp$control),
               treatment=sum(tmp$treatment))
  }))
}


server <- function(input, output, session) {


   session$onSessionEnded(function() {
     stopApp()
   })

  alld <- reactiveValues()
  alld$strata <- list()

  output$power_strata <- renderUI({

    strata_list <- lapply(1:(input$power_nStrata), function(i) {

      isolate({
        fluidRow(
          column(width=12,offset=1,

                 fluidRow(
                   textInput(paste0("power_direct_strataName_",i),paste0("Name of strata ",i,":"),paste0("strata",i)),
                   numericInput(paste0("power_direct_w_",i),paste0("Weight of of strata ",i,":"),value="1",min="0")
                   ),

                 h5("Count of treatment outcomes"),
                 fluidRow(
                   column(width=9,offset=1,
                          splitLayout(
                            numericInput(paste0("power_direct_t0_",i),"mRS 0",value = "1",min = "0"),
                            numericInput(paste0("power_direct_t1_",i),"mRS 1",value = "1",min = "0"),
                            numericInput(paste0("power_direct_t2_",i),"mRS 2",value = "1",min = "0"),
                            numericInput(paste0("power_direct_t3_",i),"mRS 3",value = "1",min = "0"),
                            numericInput(paste0("power_direct_t4_",i),"mRS 4",value = "1",min = "0"),
                            numericInput(paste0("power_direct_t5_",i),"mRS 5",value = "1",min = "0"),
                            numericInput(paste0("power_direct_t6_",i),"mRS 6",value = "1",min = "0"),
                          )
                   )
                 ),
                 h5("Count of control outcomes"),
                 fluidRow(
                   column(width=9,offset=1,
                          splitLayout(
                            numericInput(paste0("power_direct_c0_",i),"mRS 0",value = "1",min = "0"),
                            numericInput(paste0("power_direct_c1_",i),"mRS 1",value = "1",min = "0"),
                            numericInput(paste0("power_direct_c2_",i),"mRS 2",value = "1",min = "0"),
                            numericInput(paste0("power_direct_c3_",i),"mRS 3",value = "1",min = "0"),
                            numericInput(paste0("power_direct_c4_",i),"mRS 4",value = "1",min = "0"),
                            numericInput(paste0("power_direct_c5_",i),"mRS 5",value = "1",min = "0"),
                            numericInput(paste0("power_direct_c6_",i),"mRS 6",value = "1",min = "0"),
                          )
                   )
                 )


          )
        )
      })

    })
  })


    output$power_sg_control <- renderUI({

      strata_list <- lapply(1:(input$power_nStrata), function(i) {

        isolate({
          fluidRow(
            column(width=11,offset=1,

                   fluidRow(
                     textInput(paste0("power_sg_strataName_",i),paste0("Name of strata ",i,":"),paste0("strata",i)),
                     numericInput(paste0("power_sg_w_",i),paste0("Weight of of strata ",i,":"),value="1",min="0")
                    ),

                   h5("Count of control outcomes"),
                   fluidRow(
                     column(width=9,offset=1,
                            splitLayout(
                              numericInput(paste0("power_sg_c0_",i),"mRS 0",value = "1",min = "0"),
                              numericInput(paste0("power_sg_c1_",i),"mRS 1",value = "1",min = "0"),
                              numericInput(paste0("power_sg_c2_",i),"mRS 2",value = "1",min = "0"),
                              numericInput(paste0("power_sg_c3_",i),"mRS 3",value = "1",min = "0"),
                              numericInput(paste0("power_sg_c4_",i),"mRS 4",value = "1",min = "0"),
                              numericInput(paste0("power_sg_c5_",i),"mRS 5",value = "1",min = "0"),
                              numericInput(paste0("power_sg_c6_",i),"mRS 6",value = "1",min = "0")
                            )
                     )
                   )


            )
          )
        })

      })

    # cat(length(strata_list))

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, strata_list)
  })


    output$power_polr_control <- renderUI({

      strata_list <- lapply(1:(input$power_nStrata), function(i) {

        isolate({
          fluidRow(
            column(width=11,offset=1,

                   fluidRow(
                     textInput(paste0("power_polr_strataName_",i),paste0("Name of strata ",i,":"),paste0("strata",i)),
                     numericInput(paste0("power_polr_w_",i),paste0("Weight of of strata ",i,":"),value="1",min="0")
                     ),

                   h5("Count of control outcomes"),
                   fluidRow(
                     column(width=9,offset=1,
                            splitLayout(
                              numericInput(paste0("power_polr_c0_",i),"mRS 0",value = "1",min = "0"),
                              numericInput(paste0("power_polr_c1_",i),"mRS 1",value = "1",min = "0"),
                              numericInput(paste0("power_polr_c2_",i),"mRS 2",value = "1",min = "0"),
                              numericInput(paste0("power_polr_c3_",i),"mRS 3",value = "1",min = "0"),
                              numericInput(paste0("power_polr_c4_",i),"mRS 4",value = "1",min = "0"),
                              numericInput(paste0("power_polr_c5_",i),"mRS 5",value = "1",min = "0"),
                              numericInput(paste0("power_polr_c6_",i),"mRS 6",value = "1",min = "0")
                            )
                     )
                   )


            )
          )
        })

      })

      # cat(length(strata_list))

      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, strata_list)
    })


  output$power_pairs_control <- renderUI({


    strata_list <- lapply(1:(input$power_nStrata), function(i) {

      ({
        fluidRow(
        fluidRow(
          column(width=12,offset=1,
                 textInput(paste0("power_pairs_strataName_",i),paste0("Name of strata ",i,":"),paste0("strata",i))
          )
        ),
        fluidRow(
          column(width=3, offset=1, numericInput(paste0("power_pairs_win_",i),
                                                HTML("Number of pairs where<br/>treatment is better"),
                                                 value = "1",min = "0")),
          column(width=3, numericInput(paste0("power_pairs_tie_",i),
                                      HTML("Number of pairs where there is<br/>no difference in outcome"),value = "1",min = "0")),
          column(width=3,numericInput(paste0("power_pairs_loss_",i),
                                      HTML("Number of pairs where<br/>control is better"),
                                      value = "1",min = "0"))
        )
        )
      })

    })

  })

  # Power #######################################################################

  # Track the last method that was used
  # This is only really needed for the generic outcome approach
  lastPowerMethod <- reactiveVal(value="direct")

  # Proportions are stored as a reactive list
  power_xtab <- reactiveVal(NA)
  power_xtab_generic <- reactiveVal(NA)
  power_weights <- reactiveVal(NA)

  # observeEvent(input$power_tab_method, {
    # print(paste("You clicked tab:", input$power_tab_method))
  # })

  # observe({

    # cat("================\n")
    # x <- power_xtab()

    # print(x)


    # print(input$power_merge)
  # })

  # Update power_xtab based on direct approach

  observe({

    if(input$power_tab_method == "Direct Specification of mRS")
    {
      tryCatch({
        do.call("rbind",lapply(1:(input$power_nStrata), function(i) {
          data.frame(strata=input[[paste0("power_direct_strataName_",i)]],
                     mRS = 0:6,
                     control=sapply(0:6,function(j){input[[sprintf("power_direct_c%d_%d",j,i)]]}),
                     treatment=sapply(0:6,function(j){input[[sprintf("power_direct_t%d_%d",j,i)]]})
                    ) -> x

                    x$control <- x$control/sum(x$control)
                    x$treatment <- x$treatment/sum(x$treatment)

                    return(x)
                  }
                ))},
        error=function(e){e}
        ) -> x

    tryCatch({
      do.call("c",lapply(1:(input$power_nStrata), function(i) {
        input[[paste0("power_direct_w_",i)]]
        }))
      },
      error=function(e){e}
      ) -> w

      # if we didn't return an error/class is a data frame/etc
      # then update the proportions used for power calculations

      if("data.frame" %in% class(x))
      {
        #print(class(x))
        #print(x)

        strataList <- sapply(1:input$power_nStrata,function(i){input[[paste0("power_direct_strataName_",i)]]})
        x$strata <- factor(x$strata, levels=strataList)

        x$mRS <- factor(x$mRS, levels=0:6,labels=get_combined_mRS_labels(input$power_merge))
        x <- collapse_mRS_values(x)

        power_xtab(x)
        power_weights(w)
      }
    }

  })



  # Update power_xtab based on counterfactual approach

  observe({

    if(input$power_tab_method == "Counterfactual Approach")
    {
      # print("counterfactual approach selected")

      tryCatch({
        lapply(1:(input$power_nStrata), function(i) {
            x <- sapply(0:6,function(j){input[[sprintf("power_sg_c%d_%d",j,i)]]})
            x <- x/sum(x)
            return(x)
        }
        )},
        error=function(e){e}
      ) -> p0

      # print(p0)


      tryCatch({
        do.call("rbind",lapply(0:6, function(i) {
          sapply(0:6,function(j){input[[sprintf("sg_%d_%d",i,j)]]})
        }
        ))},
        error=function(e){e}
      ) -> M

      M <- M/t(sapply(1:7,function(i){rep(sum(M[i,]),7)}))

      # print(M)

      # each p1 is given by matrix multiplication


      tryCatch({
        lapply(p0,function(this_p0){

          x <- c(this_p0 %*% M)
          # x <- x/sum(x)
          return(x)

        }
        )},
        error=function(e){e}
      ) -> p1

      # print(p1)


      tryCatch({
        do.call("rbind",lapply(1:(input$power_nStrata), function(i){
          data.frame(strata=input[[paste0("power_sg_strataName_",i)]],
                     mRS = 0:6,
                     control=p0[[i]],
                     treatment=p1[[i]]
          )
      }))
      },
        error=function(e){e}
      ) -> x


      tryCatch({
        do.call("c",lapply(1:(input$power_nStrata), function(i) {
          input[[paste0("power_sg_w_",i)]]
        }))
      },
      error=function(e){e}
      ) -> w

      if("data.frame" %in% class(x))
      {
        strataList <- sapply(1:input$power_nStrata,function(i){input[[paste0("power_sg_strataName_",i)]]})
        x$strata <- factor(x$strata, levels=strataList)

        x$mRS <- factor(x$mRS, levels=0:6,labels=get_combined_mRS_labels(input$power_merge))
        x <- collapse_mRS_values(x)

        power_xtab(x)
        power_weights(w)
      }
    }

  })


  # Update power_xtab based on proportional odds

  observe({

    # Each p0 needs to be converted to cumulative "good outcome" probability

    # print("Cumulative Probability")

    tryCatch({
      lapply(1:(input$power_nStrata), function(i) {
        x <- sapply(0:6,function(j){input[[sprintf("power_polr_c%d_%d",j,i)]]})
        x <- x/sum(x)
        x <- cumsum(x)
        return(x)
      }
      )},
      error=function(e){e}
    ) -> p0

    # print(p0)

    # Cumulative p1 is given as a function of cumulative p0

    tryCatch({
      lapply(p0, function(this_p0) {
        (input$power_polr_odds * this_p0) / ((input$power_polr_odds - 1) * this_p0 + 1)
      }
      )},
      error=function(e){e}
    ) -> p1

    # print(p1)

    # Convert both back to probability

    # print("Probability")

    tryCatch({
      lapply(p0, function(this_p0) {
        diff(c(0,this_p0))
      }
      )},
      error=function(e){e}
    ) -> p0

    # print(p0)

    tryCatch({
      lapply(p1, function(this_p1) {
        diff(c(0,this_p1))
      }
      )},
      error=function(e){e}
    ) -> p1

    # print(p1)

    tryCatch({
      do.call("rbind",lapply(1:(input$power_nStrata), function(i){
        data.frame(strata=input[[paste0("power_polr_strataName_",i)]],
                   mRS = 0:6,
                   control=p0[[i]],
                   treatment=p1[[i]]
        )
      }))
    },
    error=function(e){e}
    ) -> x

    tryCatch({
      do.call("c",lapply(1:(input$power_nStrata), function(i) {
        input[[paste0("power_polr_w_",i)]]
      }))
    },
    error=function(e){e}
    ) -> w

    if("data.frame" %in% class(x))
    {

      strataList <- sapply(1:input$power_nStrata,function(i){input[[paste0("power_polr_strataName_",i)]]})
      x$strata <- factor(x$strata, levels=strataList)

      x$mRS <- factor(x$mRS, levels=0:6,labels=get_combined_mRS_labels(input$power_merge))
      x <- collapse_mRS_values(x)

      power_xtab(x)
      power_weights(w)
    }

  })


  observe({

    if(input$power_tab_method == "Direct Pair Count approach")
    {
      tryCatch({
        do.call("rbind",lapply(1:(input$power_nStrata), function(i) {
          data.frame(strata=input[[paste0("power_pairs_strataName_",i)]],
                     pWin = input[[paste0("power_pairs_win_",i)]],
                     pTie = input[[paste0("power_pairs_tie_",i)]],
                     pLoss = input[[paste0("power_pairs_loss_",i)]]
          ) -> x

          sum <- x$pWin+x$pTie+x$pLoss

          x$pWin <- x$pWin/sum
          x$pTie <- x$pTie/sum
          x$pLoss <- x$pLoss/sum

          return(x)
        }
        ))},
        error=function(e){e}
      ) -> x

      # tryCatch({
      #   do.call("c",lapply(1:(input$power_nStrata), function(i) {
      #     input[[paste0("power_direct_w_",i)]]
      #   }))
      # },
      # error=function(e){e}
      # ) -> w

      # if we didn't return an error/class is a data frame/etc
      # then update the proportions used for power calculations

      if("data.frame" %in% class(x))
      {
        #print(class(x))
        #print(x)

        strataList <- sapply(1:input$power_nStrata,function(i){input[[paste0("power_direct_strataName_",i)]]})
        x$strata <- factor(x$strata, levels=strataList)

        power_xtab_generic(x)
      }
    }

  })

  ## Power Results ##################################################################################

  ### Individual plot code #########################################################################

  # grotta_plotHeight <- reactive(input$grotta_height * input$nStrata)

  #### Grotta Bar #########################################################

  observe({
  output$make_power_grottaBar <- renderPlot(
  {

      x <- power_xtab()


      # Poor man's gather()
      x1 <- cbind(x[,c("strata","mRS","control")],group="control")
      x2 <- cbind(x[,c("strata","mRS","treatment")],group="treatment")

      colnames(x1) <- colnames(x2) <- c("strata","mRS","n","group")

      x <- rbind(x1,x2)
      x <- x[,c(1,4,2,3)]

      x$group <- factor(x$group,
                        levels=c("control","treatment"),
                        labels=c(input$power_controlName,input$power_treatmentName)
      )
      x$mRS <- as.factor(x$mRS)


      strataName <- "strata"

      if(input$power_nStrata == 1){
        x <- x[,c("group","mRS","n")]
        strataName <- NULL
      }

      width=input$power_grotta_width
      printNumbers = input$power_grotta_printNumbers
      textsize=input$power_grotta_textSize
      numbersize=input$power_grotta_numberSize
      linesize=input$power_grotta_lineSize
      colourScheme = input$power_image_colorScheme

      #### These will eventually be controllable settings #############################
      ncol=1
      dir="v"

      ggp <- rankinPlot::grottaBar(x,
                                   groupName = "group",
                                   scoreName="mRS",
                                   strataName = strataName,
                                   returnData = FALSE,
                                   textSize = textsize,
                                   numberSize = numbersize,
                                   lineSize = linesize,
                                   width = width,
                                   printNumbers = printNumbers,
                                   colourScheme = colourScheme
                                   # REST OF INPUTS NEEDS ADDED IN HERE
      )


#       #### These will eventually be controllable settings
#
#       width=0.5
#       printNumbers = T
#       textsize=1
#       numbersize=1
#       linesize=1
#
#       ncol=1
#       dir="v"
#
#       # Get proportions. This has to be done by strata.
#       x <- by(x,x$strata,function(x){
#         x$p <- x$n
#         x[x$group == x$group[1],"p"] <- x[x$group == x$group[1],"p"]/sum(x[x$group == x$group[1],"p"])
#         x[x$group != x$group[1],"p"] <- x[x$group != x$group[1],"p"]/sum(x[x$group != x$group[1],"p"])
#
#         x <- do.call("rbind",by(x,x$group,function(df){
#           df$p_prev <- cumsum(df$p)-df$p
#           df
#         }))
#
#         x
#       })
#       x <- do.call("rbind",x)
#       rownames(x) <- NULL
#
#
#       rownames(x) <- NULL
#
#       groupLevels <- levels(x$group)
#       x$group <- as.numeric(x$group)
#
#       # Get dataset for lines. This should also be done by strata
#       y <- by(x,x$strata,function(x){
#         y <- {}
#         for(i in 1:(length(groupLevels)-1)){
#           this_y <- x[which(x$group %in% i:(i+1)),]
#
#           this_y$mRS <- as.numeric(this_y$mRS)
#
#           this_y <- rbind(this_y,
#                           data.frame(strata=unique(x$strata),
#                                      group=c(i,i+1),
#                                      mRS=c(0,0),
#                                      n = c(0,0),
#                                      p=c(0,0),
#                                      p_prev=c(0,0)
#                           )
#           )
#
#           this_y$line_id <- paste(this_y$mRS,i)
#
#           this_y$group <- this_y$group - (-1)^(this_y$group==i) * width/2
#
#           y <- rbind(y,this_y)
#         }
#         y
#       })
#       y <- do.call("rbind",y)
#
#       ggp <- ggplot2::ggplot(x)+
#         ggplot2::geom_rect(color="black",# size=linesize,
#                            ggplot2::aes(xmin=group-width/2,xmax=group+width/2,
#                                         ymin=p_prev,ymax=p_prev+p,fill=mRS))+
#         ggplot2::geom_line(data=y, #size=linesize,
#                            ggplot2::aes(x=group,y=p+p_prev,group=line_id))
#
#       if(printNumbers)
#       {
#         ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0),], #size=numbersize,
#                                        ggplot2::aes(x=group,y=p_prev+0.5*p,
#                                                     label=sprintf("%2.2f",100*n)))
#       }
#
#       ggp <- ggp+
#         scale_x_continuous(breaks = 1:length(groupLevels), labels=groupLevels)+
#         scale_y_continuous(labels=scales::percent_format(),expand = expansion(add=0.01))+
#         ggplot2::coord_flip(clip="off")+
#         labs(fill="mRS")+
#         theme_bw()+
#         theme(axis.title = element_blank(),legend.position = "top",
#               strip.background = element_rect(fill="white"),
#               panel.grid.major =element_blank(),
#               panel.grid.minor = element_blank(),
#               # text = element_text(size=textsize),
#               plot.margin = margin(1, 1, 1, 1, "cm")
#         )+
#         scale_fill_brewer(palette="RdYlGn",direction = -1)+
#         guides(fill=guide_legend(nrow = 1))+
#         facet_wrap(~strata)
#
#
#
#       if(input$nStrata>1){
#         ggp <- ggp+facet_wrap(~strata,ncol=1,dir = dir)
#       }

      ggp
    },
    width=ifelse(input$power_image_width<=3600,input$power_image_width,3600),
    height=ifelse(input$power_image_height<=3600,input$power_image_height,3600)
  )
  })

  ### UI Links ################################################

  # Power plots will depend on whichever tab was selected last

  output$power_plots <- renderUI({
    if(input$power_tab_method == "Direct Specification of mRS"){

      div(
        plotOutput("make_power_grottaBar"),
      )

    } else if(input$power_tab_method == "Counterfactual Approach"){

      div(
        plotOutput("make_power_grottaBar")
      )

    } else if(input$power_tab_method == "Proportional odds approach"){

      div(
        plotOutput("make_power_grottaBar")
      )
    } else {
    }


  })

  ## Power Table #############
  # Pull from whichever tab was selected last

  output$power_results<- renderTable({

    # print(power_xtab())

    if(input$power_tab_method == "Direct Pair Count approach"){


      x <- power_xtab_generic()

      out <- do.call("rbind",by(x,x$strata,function(x){

        data.frame(p_win = x$pWin,
                   p_tie = x$pTie,
                   p_loss = x$pLoss,
                   yu = yu_power_generic(p_win = x$pWin,
                                         p_tie = x$pTie,
                                         p_loss = x$pLoss,
                                         k = 1,
                                         alpha = input$power_alpha,
                                         power=input$power_power))

      }))

      colnames(out)[c(1,3)] <- sprintf("Proportion of pairs where %s is better",
                                    c(input$power_treatmentName,input$power_controlName))

      colnames(out)[c(2,4)] <- c("Proportion of pairs where there is no difference in outcome",
                              "Total sample size needed")


      out <- cbind(Assumption=rownames(out),out)

    } else {

      # print("updating power results")

      x <- power_xtab()
      w <- power_weights()

      if(input$power_strata_method == "strata")
      {
        p0 <- do.call("rbind",by(x,x$strata,function(x){
          out <- x[,"control"]
          out/sum(out)
        }))

        p1 <- do.call("rbind",by(x,x$strata,function(x){
          out <- x[,"treatment"]
          out/sum(out)
        }))

        out <- get_sample_size(p0=p0, p1=p1, w=w,
                               power = input$power_power, alpha = input$power_alpha, nStrata = input$power_nStrata)
      }
      else
      {
        out <- do.call("rbind",by(x,x$strata,function(x){

          p0 <- x[,"control"]
          p0/sum(p0)
          p0 <- matrix(p0,nrow=1)

          p1 <- x[,"treatment"]
          p1/sum(p1)
          p1 <- matrix(p1,nrow=1)

          get_sample_size(p0=p0, p1=p1, w=1,
                          power = input$power_power, alpha = input$power_alpha, nStrata = 1)
        }))
        out <- cbind(assumption=rownames(out),out)
      }
    }

    return(out)
  })

  # Analysis ####################################################################

  # Insert the right number of plot output objects into the web page
  output$strata <- renderUI({

    strata_list <- lapply(1:(input$nStrata), function(i) {

      isolate({
      fluidRow(
        column(width=12,offset=1,

        fluidRow(textInput(paste0("strataName_",i),paste0("Name of strata ",i,":"),paste0("strata",i))),

        h5("Count of treatment outcomes"),
        fluidRow(
          column(width=9,offset=1,
                 splitLayout(
                   numericInput(paste0("t0_",i),"mRS 0",value = "1",min = "0"),
                   numericInput(paste0("t1_",i),"mRS 1",value = "1",min = "0"),
                   numericInput(paste0("t2_",i),"mRS 2",value = "1",min = "0"),
                   numericInput(paste0("t3_",i),"mRS 3",value = "1",min = "0"),
                   numericInput(paste0("t4_",i),"mRS 4",value = "1",min = "0"),
                   numericInput(paste0("t5_",i),"mRS 5",value = "1",min = "0"),
                   numericInput(paste0("t6_",i),"mRS 6",value = "1",min = "0"),
                 )
          )
        ),
        h5("Count of control outcomes"),
        fluidRow(
                column(width=9,offset=1,
                splitLayout(
                numericInput(paste0("c0_",i),"mRS 0",value = "1",min = "0"),
                numericInput(paste0("c1_",i),"mRS 1",value = "1",min = "0"),
                numericInput(paste0("c2_",i),"mRS 2",value = "1",min = "0"),
                numericInput(paste0("c3_",i),"mRS 3",value = "1",min = "0"),
                numericInput(paste0("c4_",i),"mRS 4",value = "1",min = "0"),
                numericInput(paste0("c5_",i),"mRS 5",value = "1",min = "0"),
                numericInput(paste0("c6_",i),"mRS 6",value = "1",min = "0"),
            )
            ),
        )


        )
      )
      })

    })

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, strata_list)
  })



  ## Common information across everything ###################################################
  # If we're being proper this should probably be a reactive value but this works well enough

  get_xtab <- function(){

    df <- do.call("rbind",lapply(1:(input$nStrata), function(i) {
      data.frame(strata=input[[paste0("strataName_",i)]],
                 mRS = 0:6,
                 control=sapply(0:6,function(j){input[[sprintf("c%d_%d",j,i)]]}),
                 treatment=sapply(0:6,function(j){input[[sprintf("t%d_%d",j,i)]]})
      )
    }
    ))

    df$strata <- factor(df$strata,levels=sapply(1:(input$nStrata), function(i){input[[paste0("strataName_",i)]]}))

    df$mRS <- factor(df$mRS, levels=0:6,labels=get_combined_mRS_labels(input$analysis_merge))
    df <- collapse_mRS_values(df)

    return(df)
  }

  ## Results ##########################

  ### Genodds #################################

  output$genodds <- renderTable({
    x <- get_xtab()

    alpha <- input$analysis_alpha
    assume_no_effect <- input$assumeNull
    contr_fav <- ifelse(input$analysis_ties=="drop",NA,0.5)
    proportion_transform <- input$analysis_results =="winP"


    results <- genodds(x,contr_fav=contr_fav,assume_no_effect=assume_no_effect, alpha=alpha)

    stratum_results <- do.call("rbind",lapply(results,function(x){
      c(x$odds, x$conf.int, x$p)}
    ))

    colnames(stratum_results) <- c("Generalised Odds Ratio", "Lower CI", "Upper CI", "p-value")
    stratum_results <- as.data.frame(stratum_results)

    if(length(results)>1)
    {

      pooled_lnodds <- do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2 * log(x$odds) ))/
        do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2 ))

      pooled_SElnodds <- sqrt(1/do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2)))
      pooled_SElnnull <- sqrt(1/do.call("sum",lapply(results,function(x) 1/x$SEnull^2)))

      SE <- ifelse(assume_no_effect,pooled_SElnnull,pooled_SElnodds)
      pooled_lnconf.int=qnorm(c(alpha/2,1-alpha/2),mean=pooled_lnodds,sd=SE)
      pooled_p=pnorm(abs(pooled_lnodds),sd=SE,lower.tail=FALSE)*2
      stratum_results <- rbind(stratum_results, `Pooled Effect`=c(exp(pooled_lnodds), exp(pooled_lnconf.int), pooled_p))

    }

    if(proportion_transform)
    {
      stratum_results[,1:3] <- stratum_results[,1:3]/(1+stratum_results[,1:3])
      colnames(stratum_results)[1] <- "Win Proportion"
    }

    if(length(results)>1) stratum_results <- cbind(Strata = rownames(stratum_results),stratum_results)


    stratum_results
  },digits = 4)



  output$test_homogeneity <- renderTable({
    x <- get_xtab()

    alpha <- input$analysis_alpha
    assume_no_effect <- input$assumeNull
    contr_fav <- ifelse(input$analysis_ties=="drop",NA,0.5)
    proportion_transform <- input$analysis_results =="winP"


    results <- genodds(x,contr_fav=contr_fav,assume_no_effect=assume_no_effect, alpha=alpha)

    stratum_results <- do.call("rbind",lapply(results,function(x){
      c(x$odds, x$SEodds)}
    ))

    if(length(results)>1)
    {

      pooled_lnodds <- do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2 * log(x$odds) ))/
        do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2 ))

      pooled_SElnodds <- sqrt(1/do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2)))


      V <- sum((log(stratum_results[,1]) - pooled_lnodds)^2/(pooled_SElnodds^2))

      out <- data.frame(V=(V/input$nStrata),p.value=pchisq(q = V,df = input$nStrata-1,lower.tail = FALSE))

      colnames(out) <- c("Average deviation from pooled log-odds","Heterogeneity test p-value")


    } else {
      out <- data.frame()
    }

    return(out)

  })


  output$genodds_heterogeneity <- renderUI({
    fluidRow(column(width=12,tableOutput("test_homogeneity")))
  })


  ### Howard Statement #################################

  output$howard_statement <- renderUI({

    x <- get_xtab()

    out <- paste(howard_statement(x,c_label = input$controlName,t_label = input$treatmentName), collapse="<br/><br/>")

    HTML(out)

  })



  ## Plots ##################################################################################

  ### Grotta Bar #######################################################

  grotta_plotHeight <- reactive(input$grotta_height * input$nStrata)

  observe({
  output$make_grottaBar <- renderPlot(
    {
      x <- get_xtab()

      # Poor man's gather()
      x1 <- cbind(x[,c("strata","mRS","control")],group="control")
      x2 <- cbind(x[,c("strata","mRS","treatment")],group="treatment")

      colnames(x1) <- colnames(x2) <- c("strata","mRS","n","group")

      x <- rbind(x1,x2)
      x <- x[,c(1,4,2,3)]

      x$group <- factor(x$group,
                        levels=c("control","treatment"),
                        labels=c(input$controlName,input$treatmentName)
      )
      x$mRS <- as.factor(x$mRS)

      width=input$grotta_width
      printNumbers = input$grotta_printNumbers
      textsize=input$grotta_textSize
      numbersize=input$grotta_numberSize
      linesize=input$grotta_lineSize
      colourScheme = input$image_colorScheme

      #### These will eventually be controllable settings #############################
      ncol=1
      dir="v"

      strataName <- "strata"

      if(input$nStrata == 1){
        x <- x[,c("group","mRS","n")]
        strataName <- NULL
      }

      ggp <- rankinPlot::grottaBar(x,
                                   groupName = "group",
                                   scoreName="mRS",
                                   strataName = strataName,
                                   returnData = FALSE,
                                   textSize = textsize,
                                   numberSize = numbersize,
                                   lineSize = linesize,
                                   width = width,
                                   printNumbers = printNumbers,
                                   colourScheme = colourScheme
                                   # REST OF INPUTS NEEDS ADDED IN HERE
                                  )

#
#       # Get proportions. This has to be done by strata.
#       x <- by(x,x$strata,function(x){
#         x$p <- x$n
#         x[x$group == x$group[1],"p"] <- x[x$group == x$group[1],"p"]/sum(x[x$group == x$group[1],"p"])
#         x[x$group != x$group[1],"p"] <- x[x$group != x$group[1],"p"]/sum(x[x$group != x$group[1],"p"])
#
#         x <- do.call("rbind",by(x,x$group,function(df){
#           df$p_prev <- cumsum(df$p)-df$p
#           df
#         }))
#
#         x
#       })
#       x <- do.call("rbind",x)
#       rownames(x) <- NULL
#
#
#       rownames(x) <- NULL
#
#       groupLevels <- levels(x$group)
#       x$group <- as.numeric(x$group)
#
#       # Get dataset for lines. This should also be done by strata
#       y <- by(x,x$strata,function(x){
#         y <- {}
#         for(i in 1:(length(groupLevels)-1)){
#           this_y <- x[which(x$group %in% i:(i+1)),]
#
#           this_y$mRS <- as.numeric(this_y$mRS)
#
#           this_y <- rbind(this_y,
#                           data.frame(strata=unique(x$strata),
#                                      group=c(i,i+1),
#                                      mRS=c(0,0),
#                                      n = c(0,0),
#                                      p=c(0,0),
#                                      p_prev=c(0,0)
#                           )
#           )
#
#           this_y$line_id <- paste(this_y$mRS,i)
#
#           this_y$group <- this_y$group - (-1)^(this_y$group==i) * width/2
#
#           y <- rbind(y,this_y)
#         }
#         y
#       })
#       y <- do.call("rbind",y)
#
#       ggp <- ggplot2::ggplot(x)+
#         ggplot2::geom_rect(color="black", size=linesize,
#                            ggplot2::aes(xmin=group-width/2,xmax=group+width/2,
#                                         ymin=p_prev,ymax=p_prev+p,fill=mRS))+
#         ggplot2::geom_line(data=y, size=linesize,
#                            ggplot2::aes(x=group,y=p+p_prev,group=line_id))
#
#       if(printNumbers)
#       {
#         ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0),], size=numbersize,
#                                        ggplot2::aes(x=group,y=p_prev+0.5*p,
#                                                     label=sprintf("%d",n,100*p)))
#       }
#
#       ggp <- ggp+
#         scale_x_continuous(breaks = 1:length(groupLevels), labels=groupLevels)+
#         scale_y_continuous(labels=scales::percent_format(),expand = expansion(add=0.01))+
#         ggplot2::coord_flip(clip="off")+
#         labs(fill="mRS")+
#         theme_bw()+
#         theme(axis.title = element_blank(),legend.position = "top",
#               strip.background = element_rect(fill="white"),
#               panel.grid.major =element_blank(),
#               panel.grid.minor = element_blank(),
#               text = element_text(size=textsize),
#               plot.margin = margin(1, 1, 1, 1, "cm")
#         )
#
#       if(input$impace_colorScheme=="colour"){
#         ggp <- ggp + scale_fill_brewer(palette="RdYlGn",direction = -1)+
#                 guides(fill=guide_legend(nrow = 1))
#       } else if (input$impace_colorScheme=="grayscale"){
#         ggp <- ggp + scale_fill_manual(
#           values=c("#ffffff",
#                    "#e3e3e3",
#                    "#c9c9c9",
#                    "#bababa",
#                    "#b0b0b0",
#                    "#949494",
#                    "#7d7d7d"
#                    )
#                                        )+
#           guides(fill=guide_legend(nrow = 1))
#       }
#
#
#
#       if(input$nStrata>1){
#         ggp <- ggp+facet_wrap(~strata,ncol=1,dir = dir)
#       }

      ggp
    },
    width=ifelse(input$image_width<=3600,input$image_width,3600),
    height=ifelse(input$image_height<=3600,input$image_height,3600)
    )
  })

  output$grottaBar <- renderUI({
    div( class = "scaled",
    plotOutput("make_grottaBar"),
    )
  })


  ### Forest Plot ##############################################################

  observe({
  output$make_forestPlot <- renderPlot({

    x <- get_xtab()

    alpha <- input$forestPlot_alpha
    assume_no_effect <- input$forestPlot_assumeNull
    contr_fav <- ifelse(input$forestPlot_ties=="drop",NA,0.5)
    proportion_transform <- input$forestPlot_results =="winP"

    results <- genodds(x,contr_fav=contr_fav,assume_no_effect=assume_no_effect, alpha=alpha)

    stratum_results <- do.call("rbind",lapply(results,function(x){
      c(x$odds, x$conf.int, x$p)}
    ))

    colnames(stratum_results) <- c("effect", "cil", "ciu", "p")
    stratum_results <- as.data.frame(stratum_results)

    stratum_results$type <- 0

    if(length(results)>1)
    {

      pooled_lnodds <- do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2 * log(x$odds) ))/
        do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2 ))

      pooled_SElnodds <- sqrt(1/do.call("sum",lapply(results,function(x) x$odds^2/x$SEodds^2)))
      pooled_SElnnull <- sqrt(1/do.call("sum",lapply(results,function(x) 1/x$SEnull^2)))

      SE <- ifelse(assume_no_effect,pooled_SElnnull,pooled_SElnodds)
      pooled_lnconf.int=qnorm(c(alpha/2,1-alpha/2),mean=pooled_lnodds,sd=SE)
      pooled_p=pnorm(abs(pooled_lnodds),sd=SE,lower.tail=FALSE)*2
      stratum_results <- rbind(stratum_results, `Pooled Effect`=c(exp(pooled_lnodds), exp(pooled_lnconf.int), pooled_p, type=1))

    }

    stratum_results$type <- factor(stratum_results$type,levels=0:1)


    if(proportion_transform)
    {
      stratum_results[,1:3] <- stratum_results[,1:3]/(1+stratum_results[,1:3])
    }

    if(length(results)>1)
    {
      stratum_results <- cbind(Strata = rownames(stratum_results),stratum_results)

      stratum_results$Strata <- factor(stratum_results$Strata,
                                       levels=stratum_results$Strata[nrow(stratum_results):1]
                                       )

    }
    else
    {
      stratum_results <- cbind(Strata = "", stratum_results)
    }

    # print(stratum_results)

    ggp <- ggplot(stratum_results, aes(x=Strata))+
      geom_hline(yintercept=ifelse(proportion_transform,0.5,1),
                 linetype="dashed",
                 color=ifelse(input$image_colorScheme=="lowGreen","red","gray")
                 )+
      geom_linerange(aes(ymin=cil,ymax=ciu),
                     )+
      geom_point(aes(y=effect,size=type,shape=type),
                 fill="white")

      if(proportion_transform){
        ggp <- ggp + labs(y="Win Proportion")
      } else {
        ggp <- ggp + labs(y="Generalised odds ratio") + scale_y_log10()
      }

      ggp + coord_flip()+
      scale_shape_manual(values=c(16, 23),guide="none")+
      scale_size_manual(values=input$forestPlot_pointSize*c(1,2.5),guide="none")+
      theme_bw()+
      theme(text = element_text(size=input$forestPlot_textSize))
  },
  width=ifelse(input$image_width<=3600,input$image_width,3600),
  height=ifelse(input$image_height<=3600,input$image_height,3600)
  )
  })

  output$forestPlot <- renderUI({
    plotOutput("make_forestPlot")
  })


}
