library(shiny)
library(miniUI)
library(shinyAce)
library(shinythemes)
library(R.utils)
library(randomForest)
source("R/utils.R")

ui <- miniPage(
   #includeCSS("R/revisit.css"),
   tags$style(type = "text/css",
              "label { padding: 0px; font-size: 20px }",
              ".btn { padding: 0px; font-size: 20px }",
              ".form-control { padding: 4px; font-size: 20px }",
              ".form-group { padding: 0px; font-size: 20px }",
              "#ace { font-weight: bold }",
              "#cancel { padding: 0px; font-size: 16px }",
              "#done { padding: 0px; font-size: 16px }",
              "#message { padding: 0px; font-size: 20px }"
   ),
   #shinythemes::themeSelector(),
   theme = shinytheme("united"),
   gadgetTitleBar("Revisit"),
   miniContentPanel(
      tabsetPanel(
         tabPanel(
            "Main",
            fluidRow(
               div(class = "col-xs-4 col-md-4",
                   selectInput("cases", "Case Studies",
                               choices = "Pima diabetes study",
                               selected = "Pima diabetes study")
               ),
               div(class = "col-xs-8 col-md-8",
                   textInput("desc", "Description", width = "760px")
               )
            ),
            stableColumnLayout(
               textInput("file", "Filename (w/o Branch# or .R)", value = "Pima/pima"),
               numericInput("runstart", "Run Start Line", value = 1),
               numericInput("saveBn", "Save Branch #", value = 1)
            ),
            stableColumnLayout(
               numericInput("loadBn", "Load Branch #", value = 0),
               numericInput("runthru", "Run Through Line", value = -1),
               textInput("username", "Username", value =  "e.g. LastName, FirstName") # username here and force a userID
            ),
            miniButtonBlock(
               actionButton("loadb", "Load Code"),
               actionButton("nxt",   "Next"),
               actionButton("runb",  "Run/Continue"),
               actionButton("saveb", "Save Code")
            ),
            htmlOutput("message"),
            aceEditor("ace", value = "...",mode='r', fontSize = 20),
            stableColumnLayout(
               numericInput("aceFontSize", "Editor Font Size", value = 20),
               numericInput("pcount", "P-value/CI Count", value = 0)
            )
         ),
         tabPanel(
            "Text",
            h4("Text Output from Run"),
            verbatimTextOutput("runoutput")
         ),
         tabPanel(
            "Plot",
            h4("Plot Output from Run"),
            #plotOutput("runplot", width = "100%", height = "600px")
            plotOutput("runplot", width = "1200px", height = "600px")
         ),
         tabPanel(
            "Console",
            h4("Run R Command"),
            textInput("rcmd", NULL, value = ""),
            actionButton("cmdsubmit", "Submit Command", icon("refresh")),
            verbatimTextOutput("cmdoutput")
            # # The following is for debug only
            # h4("Run System Command"),
            # textInput("syscmd", NULL, value = ""),
            # actionButton("syscmdsubmit", "Submit System Command", icon("refresh")),
            # verbatimTextOutput("syscmdoutput")
         ),
         tabPanel(
            "README",
            h4("README for Example"),
            htmlOutput("readme")
         )
      )
   )
)
