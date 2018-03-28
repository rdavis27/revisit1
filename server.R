library(shiny)
library(png)
library(rdrop2)
library(reshape2) # add for examples
library(ggplot2)  # add for examples
#library(revisit)
source("R/revisit.R")
rvinit()
rv <- reactiveValues()
rv$statusmsg <- ""
rv$output <- ""
rv$runcnt <- 0

shinyServer(function(input, output, session) {

   startOfSession <- TRUE
   loadBn_succ <- NULL
   caselist <- NULL
   ###casepath <- system.file("CaseStudies", package="revisit")
   casepath <- "inst/CaseStudies"
   if (casepath == ""){
      status <- paste("***** ERROR: system.file(\"CaseStudies\", package=\"revisit\") returned an empty string")
      rv$statusmsg <<- status
      print(status)
   }
   else{
      casefile <- paste0(casepath, "/CaseStudyList.txt")
      if (file.exists(casefile)){
         caselist <- read.csv(casefile, strip.white = TRUE, sep = ",")
         updateSelectInput(session, "cases", choices = caselist$label, selected = caselist$label[caselist$n == 1])
      }
      else{
         status <- paste("***** ERROR:", casefile, "not found")
         rv$statusmsg <<- status
         print(status)
      }
   }

   doLoad <- function(infile, loadBn){
      status <- ""
      iend <- regexpr("/[^/]*$", infile)
      infiledir <- substr(infile, 1, (iend-1)) # no trailing slash
      fullpath    <- paste0("revisit/",input$username,"/",casepath)
      fullfiledir <- paste0(fullpath,"/",infiledir)
      if (loadBn <= 0){
         infilename  <- paste0(infile, ".R")
         infilename0 <- paste0(infile, ".0.R")
         casefilename <- paste0(casepath,"/",infilename) # one common .R file in casepath
         fullfilename <- paste0(fullpath,"/",infilename)
         if (file.exists(fullfilename) == FALSE){
            if (dir.exists(fullfiledir) == FALSE){
               dir.create(fullfiledir, recursive = TRUE)
            }
            file.copy(casefilename, fullfiledir) # copy to user directory
         }
         if (loadBn == 0){
            makebranch0(fullfilename) # always create .0.R file
            infilename <- infilename0
            fullfilename <- paste0(fullpath,"/",infilename0)
         }
      } else {
         infilename <- paste0(infile, ".", as.character(loadBn), ".R")
         fullfilename <- paste0(fullpath,"/",infilename)
         if (input$remote){
            if (file.exists(fullfilename) == FALSE){
               fullfiledir <- paste0(fullpath,"/",infiledir)
               if (dir.exists(fullfiledir) == FALSE){
                  dir.create(fullfiledir, recursive = TRUE)
               }
               tryCatch(drop_download(fullfilename, local_path = fullfilename),
                        warning = function(w) {
                           cat(file=stderr(), paste0("WARNING: ", w))
                        },
                        error = function(e) {
                           cat(file=stderr(), paste0("***** ", e))
                           cat(file=stderr(), paste0("***** File ", fullfilename, " not found\n"))
                           if (file.exists(fullfilename)) file.remove(fullfilename)
                        })
            }
         }
      }
      if (file.exists(fullfilename)){
         loadb(fullfilename)
         loadBn_succ <<- loadBn
         status <- paste(infilename, "loaded")
         currcode <- paste(rvenv$currcode, collapse = '\n')
         updateAceEditor(session, "ace", value = currcode, fontSize = input$aceFontSize)
         updateNumericInput(session, "runstart", value = 1)
         updateNumericInput(session, "runthru",  value = length(rvenv$currcode))
         nextBn <- loadBn + 1
         infilename <- paste0(infile, ".", as.character(nextBn), ".R")
         fullfilename <- paste0(fullpath,"/",infilename)
         while (file.exists(fullfilename)){
            nextBn <- nextBn + 1
            infilename <- paste0(infile, ".", as.character(nextBn), ".R")
            fullfilename <- paste0(fullpath,"/",infilename)
         }
         updateNumericInput(session, "saveBn",  value = nextBn)
      } else {
         if (!startOfSession){
            if (loadBn == 0){
               status <- paste("***** ERROR:", fullfilename, "and", filename0, "not found")
            } else {
               status <- paste("***** ERROR:", fullfilename, "not found")
            }
         } else {
            startOfSession <<- FALSE
            #status <- "" # don't overwrite startup error
         }
      }
      if (status != ""){
         rv$statusmsg <<- status
         print(status)
      }
   }

   doSave <- function(infilename, saveBn, action){
      description <- paste(input$username, "-", input$desc)
      iend <- regexpr("/[^/]*$", infilename)
      infiledir <- substr(infilename, 1, iend)
      fullpath     <- paste0("revisit/",input$username,"/",casepath)
      fullfiledir  <- paste0(fullpath,"/",infiledir)
      fullfilename <- paste0(fullpath,"/",infilename)
      if (dir.exists(fullfiledir) == FALSE){
         dir.create(fullfiledir, recursive = TRUE)
      }
      saveb(saveBn, description, fullfilename)
      status <- paste(infilename, "saved")
      if (input$remote){
         drop_upload(fullfilename, path = fullfiledir)
         status <- paste(infilename, "saved remotely")
      }
      updateNumericInput(session, "loadBn",  value = saveBn)
      rv$statusmsg <<- status
      print(status)
   }

   reactiveLoad <- reactive({
      infile <- isolate(input$infile)
      loadBn <- input$loadBn
      doLoad(infile, loadBn)
      return(list(loaded = rvenv$currcode))
   })

   output$pcount <- renderText({
      rvenv$pcount
   })

   output$message <- renderText({
      #spec <- reactiveLoad()
      if (substring(rv$statusmsg, 1, 1) == "*"){
         msg <- paste0("<b><font color=\"red\">", rv$statusmsg, "</font></b>")
      } else {
         msg <- paste0("<b>", rv$statusmsg, "</b>")
      }
      msg
   })

   output$ace <- renderCode({
      spec <- reactiveLoad()
      highlightCode(session, "ace")
      paste(spec$loaded, collapse = "\n")
   })

   output$cmdoutput <- renderPrint({
      input$cmdsubmit
      rcmd <- isolate(input$rcmd)
      if (rcmd != ""){
         print(rcmd)
         #print(shell(syscmd, intern=TRUE)) # to run shell command on Windows
         print(eval(parse(text=rcmd)))
      }
   })

   # # This function is for debug only
   # output$syscmdoutput <- renderPrint({
   #    input$syscmdsubmit
   #    syscmd <- isolate(input$syscmd)
   #    if (syscmd != ""){
   #       cmdlist <- strsplit(syscmd, " ")
   #       cmd <- cmdlist[[1]][1]
   #       args <- cmdlist[[1]][-1]
   #       print(system2(cmd, args = args, stdout = TRUE, stderr = TRUE))
   #    }
   # })
   
   output$runoutput <- renderText({
      rv$runcnt
      paste0(rv$output,"\n")
   })

   output$runplot <- renderPlot({
      rv$runcnt
      mypng <- readPNG("output.png")
      grid::grid.raster(mypng)
      #grid::grid.raster(mypng, width=1, height=1)
      #grid::grid.raster(mypng, width=0.5, height=1)
   })

   output$readme <- renderText({
      iend <- regexpr("/[^/]*$", input$infile)
      filedir <- substr(input$infile, 1, iend)
      filename <- paste0(casepath,"/",filedir,"/README")
      if (file.exists(filename)){
         includeMarkdown(filename)
      }
   })
   
   observeEvent(input$cases, {
      cases <- input$cases
      infile <- caselist$file[caselist$label == cases]
      desc <- caselist$desc[caselist$label == cases]
      cat(file=stderr(), paste0("##### Case=", cases, ", file=", infile, "\n"))
      updateTextInput(session, "desc", value = desc)
      updateTextInput(session, "infile", value = infile)
      doLoad(infile, 0)
   })

   observeEvent(input$loadb, {
      infile <- input$infile
      loadBn <- input$loadBn
      doLoad(infile, loadBn)
   })

   observeEvent(input$nxt, {
      rvenv$currcode <- unlist(strsplit(input$ace, "\n")) # update currcode
      rvenv$pc <- input$runstart
      rc <- try(
         nxt()
      )
      if (class(rc) == 'try-error'){
         rv$statusmsg <- paste("*****", rc)
      } else {
         rv$statusmsg <- paste("RUN", input$runstart)
         if (input$runstart < length(rvenv$currcode)){
            updateNumericInput(session, "runstart", value = input$runstart + 1)
         } else {
            updateNumericInput(session, "runstart", value = length(rvenv$currcode) + 1) # set to last line + 1
         }
         updateNumericInput(session, "runthru",  value = length(rvenv$currcode))
      }
   })

   observeEvent(input$runb, {
      rv$runcnt <<- rv$runcnt + 1
      rvenv$currcode <- unlist(strsplit(input$ace, "\n")) # update currcode
      runstart <- input$runstart
      runthru  <- input$runthru
      if (runstart < 1){
         runstart <- 1
      }
      if (runthru > length(rvenv$currcode)){
         runthru <- length(rvenv$currcode)
      }
      png(filename = "output.png", width = 800, height = 480)
      if (runthru < 1){
         print(paste("RUN FROM", runstart))
         rc <- try(
            rv$output <<- capture.output(runb(startline = runstart), split = TRUE)
         )
         if (class(rc) == 'try-error'){
            #rv$statusmsg <- "***** RUN ERROR: see error message in console"
            rv$statusmsg <- paste("*****", rc)
         } else {
            rv$statusmsg <- paste("RUN FROM", runstart)
         }

      }
      else{
         print(paste("RUN FROM", runstart, "THROUGH", runthru))
         rc <- try(
            rv$output <<- capture.output(runb(startline = runstart, throughline = runthru), split = TRUE)
         )
         if (class(rc) == 'try-error'){
            rv$statusmsg <- paste("*****", rc)
         } else {
            rv$statusmsg <- paste("RUN FROM", runstart, "THROUGH", runthru)
            if (runthru < length(rvenv$currcode)){
               updateNumericInput(session, "runstart", value = runthru + 1)
               updateNumericInput(session, "runthru",  value = length(rvenv$currcode))
            } else {
               updateNumericInput(session, "runstart", value = length(rvenv$currcode) + 1) # set to last line + 1
               updateNumericInput(session, "runthru",  value = length(rvenv$currcode))
            }
         }
      }
      dev.off()
      updateNumericInput(session, "pcount", value = rvenv$pcount)
   })

   observeEvent(input$saveb, {
      infile <- input$infile
      saveBn <- input$saveBn
      username <- input$username # add username to revisit comment
      if (is.null(loadBn_succ)){
         showModal(modalDialog(
            title = "SAVE ERROR",
            "Cannot save because no branch has yet been loaded."
         ))
         return()
      }
      if (saveBn <= 0){
         showModal(modalDialog(
            title = "SAVE ERROR",
            "Save Branch# must be greater than 0 in order to save."
         ))
         return()
      }
      desc <- gsub("^\\s+|\\s+$", "", input$desc)
      if (desc == ""){
         showModal(modalDialog(
            title = "SAVE ERROR",
            "Description must be set."
         ))
         return()
      }
      username <- gsub("^\\s+|\\s+$", "", input$username)
      if (username == ""){
         showModal(modalDialog(
            title = "SAVE ERROR",
            "Username must be set."
         ))
         return()
      }
      infilename <- paste0(infile, ".", as.character(saveBn), ".R")
      fullfilename <- paste0("revisit/",input$username,"/",casepath,"/",infilename)
      if (file.exists(fullfilename)){
         question <- paste("WARNING: ", infilename, "exists. Overwrite it?")
         saveinfilename <<- infilename
         showModal(yesNoModal(msg = question, yesAction="ok", yesLabel="Yes", noLabel="No"))
         return()
      }
      rvenv$currcode <- unlist(strsplit(input$ace, "\n")) # update currcode
      doSave(infilename, input$saveBn, "SAVE")
   })

   yesNoModal <- function(msg="Continue?", yesAction="yes", yesLabel="Yes", noLabel="No"){
      modalDialog(
         span(msg),
         footer = tagList(
            actionButton(yesAction, yesLabel),
            modalButton(noLabel)
         )
      )
   }

   observeEvent(input$ok, {
      rvenv$currcode <- unlist(strsplit(input$ace, "\n")) # update currcode
      doSave(saveinfilename, input$saveBn, "OVERWRITE")
      removeModal()
   })

   observeEvent(input$done, {
      invisible(stopApp())
   })

   observeEvent(input$cancel, {
      stopApp(NULL)
   })
})
