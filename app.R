#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyglide)
library(shinyWidgets)
library(readxl)
library(httr)

##########################
# data
##########################

GET("https://github.com/zoidy/ModelDataRubric/raw/main/Descriptor-classifications-worksheet-v2.0.xlsx",
    write_disk("rubric.xlsx", overwrite = TRUE))
data <- read_excel("rubric.xlsx")
file.remove("rubric.xlsx")


##########################
# UI - variables and funcs
##########################
numqs <<- 0

controls <- tags$div(
    tags$div(class="my-control prev-screen"),
    tags$div(class="my-control next-screen")
)

css <- "
    .my-control {
      font-size: 0em;
    }

    .pretty { /* https://github.com/dreamRs/shinyWidgets/issues/478 */
          white-space: normal;
          margin-bottom: 5px;
    }
    .pretty .state label {
      line-height: 1.5em;
      margin-top: -4px;
    }
    .pretty .state label::after, .pretty .state label::before {
      top: -2px;
    }
    
    .link_button {
        -webkit-border-radius: 4px;
        -moz-border-radius: 4px;
        border-radius: 4px;
        border: solid 1px #20538D;
        text-shadow: 0 -1px 0 rgba(0, 0, 0, 0.4);
        -webkit-box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.4), 0 1px 1px rgba(0, 0, 0, 0.2);
        -moz-box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.4), 0 1px 1px rgba(0, 0, 0, 0.2);
        box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.4), 0 1px 1px rgba(0, 0, 0, 0.2);
        background: #4479BA;
        color: #FFF;
        padding: 8px 12px;
        text-decoration: none;
    }
    
    max-width: 800px;
  "

build_header <- function(num, theme = TRUE, bigpicture = TRUE) {
    list(
        if (theme) h2("Theme:",data$`Section Theme`[num]) else span(),
        if (bigpicture) h3(data$`Big Picture Question`[num]) else span(),
        br()
    )
}

build_question <- function(num) {
    numqs <<- numqs + 1
    list(
        strong("Question",num),
        p(data$Descriptor[num]),
        p(em(data$`Descriptor definition`[num])),
        br(),
        prettyRadioButtons(
            inputId = paste0("Q",num),
            label = NULL,
            width = "100%",
            choices = build_question_listchoices(num)
        )
    )
}

build_question_listchoices <- function(num) {
    l <- setNames(
        c(if(is.na(data$`Class 1`[num])) {} else paste0("1:",data$`Suggested Weight                (If score > 1)`[num]),
          if(is.na(data$`Class 2`[num])) {} else paste0("2:",data$`Suggested Weight                (If score > 1)`[num]),
          if(is.na(data$`Class 3`[num])) {} else paste0("3:",data$`Suggested Weight                (If score > 1)`[num])),
        c(if(is.na(data$`Class 1`)[num]) {} else data$`Class 1`[num], 
          if(is.na(data$`Class 2`)[num]) {} else data$`Class 2`[num], 
          if(is.na(data$`Class 3`)[num]) {} else data$`Class 3`[num])
    )
    
}

build_nav <- function(first = FALSE, last = FALSE) {
    #avoid the built-in shinyglide buttons. They appear too far down
    #so build our own
    nav <- div(`data-glide-el`="controls")
    
    if(!first && !last) {
        first <- TRUE
        last <- TRUE
    }
    
    if(last)
        nav <- tagAppendChildren(nav,
                            a(`data-glide-dir`="<", href="#", "Back", class="link_button"),
                            span("  ")
                )
                        
    if(first)
        nav <- tagAppendChild(nav,
                              a(`data-glide-dir`=">", href="#", "Next", class="link_button")
                )
    
    nav <- tagAppendChildren(p(br()),nav,br())
}

##########################
# UI - shiny
##########################
ui <- fluidPage(
    tags$head(
        tags$style(css)
    ),
    
    titlePanel("What About Model Data? Best Practices for Preservation and Replicability"),
    glide(
        next_label = icon("chevron-right", lib="glyphicon"),
        previous_label = icon("chevron-left", lib="glyphicon"),
        loading_label = icon("hourglass", lib="glyphicon"),
        swipe = TRUE,
        keyboard = FALSE, #keyboard nav changes the radio button selection
        custom_controls = controls,
        
        screen(
            h1("Instructions"),
            p("The rubric is built to help researchers make decisions about what simulation output needs to be
                shared via a repository, i.e. made accessible and preserved a sufficient time to satisfy the
                requirements of publishers and funding agencies. Ultimately, these decisions are based on the
                goal of all community members (researchers, publishers, end users) to communicate knowledge
                in a sustainable way. Note this rubric is not meant to dictate what a researcher or research
                group keeps on their own local storage.
                The rubric is a list of simulation/experiment descriptors, organized into themes. To use the
                rubric, consider a specific simulation workflow and review all of the descriptors, selecting a
                Class (1, 2, or 3) that best fits your workflow for each descriptor. If you find that your workflow
                could be a Class 1 or 3 based on which descriptor you are looking at, you can separate the
                workflow into logical sections and then classify each section separately.
            "),
            p("See the ", a(href="https://modeldatarcn.github.io/rubrics-worksheets/Rubric-Instructions-and-Use-Cases.pdf","instructions"), 
              "and", a(href="https://modeldatarcn.github.io","https://modeldatarcn.github.io"), 
              "for more information"),
            p(a(href="https://github.com/zoidy/ModelDataRubric","Source code")),
            build_nav(first = T)
        ),
        screen(
            #Build the header for the current page using q1 (could use any q from the same group)
            build_header(1),
            lapply(c(1, 2, 3), build_question),
            build_nav()
        ),
        screen(
            build_header(4),
            lapply(c(4, 5), build_question),
            build_nav()
        ),
        screen(
            build_header(6),
            lapply(c(6, 7, 8, 9), build_question),
            build_nav()
        ),
        screen(
            build_header(10),
            lapply(c(10, 11, 12), build_question),
            build_nav()
        ),
        screen(
            build_header(13),
            build_question(13),
            build_nav()
        ),
        screen(
            build_question(14),
            build_nav()
        ),
        screen(
            build_header(15),
            lapply(c(15, 16), build_question),
            build_nav()
        ),
        screen(
            build_header(17),
            build_question(17),
            build_nav()
        ),
        screen(
            h1("Results"),
            h2("Your weighted score:",textOutput("s1", inline = T)),
            p(strong("Rubric Total Weighted Score < 48")),
            tags$ul(
                tags$li("Preserve few simulation workflow outputs"),
                tags$li("Preserve and provide access to simulation workflow configuration and code components"),
                tags$li("See Use Case 1"),
            ),
            br(),
            p(strong("Rubric Total Weighted Score between 48 and 72")),
            tags$ul(
                tags$li("Preserve selected simulation workflow outputs"),
                tags$li("Preserve and provide access to simulation workflow configuration and code components"),
                tags$li("See Use Case 2"),
            ),
            br(),
            p(strong("Rubric Total Weighted Score between 48 and 72")),
            tags$ul(
                tags$li("Preserve the majority simulation workflow outputs"),
                tags$li("Preserve and provide access to simulation workflow configuration and code components"),
                tags$li("See Use Case 3"),
            ),
            br(),
            p(a(href="https://modeldatarcn.github.io/rubrics-worksheets/Rubric-Instructions-and-Use-Cases.pdf","Use cases reference")),
            build_nav(last = T)
        )
    ),
)

##########################
# Server
##########################
server <- function(input, output, session) {
    
    result <- reactiveVal(0)
    
    output$s1 <- renderText({ 
        c(result())
    })
    
    event_triggers <- reactive({
        #Listen to events from all question inputs
        #https://stackoverflow.com/a/41961038
        #https://community.rstudio.com/t/looping-through-shiny-inputs-by-name-using-get-not-working-why/24145/2
        l <- list()
        for(i in seq(numqs))
            l <- append(l, input[[paste0("Q",i)]])
        l
    })
    
    observeEvent(event_triggers(),{
        # Calculate the score on every event and update the reactive variable for display
        score <- 0
        for(i in seq(numqs)) {
            q_vals <- strsplit(input[[paste0("Q",i)]],":")
            score <- score + prod(as.numeric(q_vals[[1]]))
            print(paste(i,prod(as.numeric(q_vals[[1]]))))
        }
        print(paste("score:", score))
        result(score)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
