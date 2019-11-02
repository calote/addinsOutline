#' Addin for Displays Outline of an Bookdown Project
#'
#' Addin for displays outline of an bookdown project
#' (R Markdown files in directory project).
#'
#' @return RStudio Addin with browserViewer()
#' This 'RStudio' addin will show a list of the different sections established
#' in the 'bookdown' project (R Markdown files in directory project)
#' and clicking on any element shown in the list
#' will cause 'RStudio' to show that section by opening the file
#' that contains it if it was not already open previously.
#' To open a bookdown project select the file _bookdown.yml or index.Rmd.
#'
#' @seealso The function \code{\link{run_addinsOutline_tex}()} and
#'     \code{\link{run_addinsOutline_Rmd}()}.
#'
#' @examples
#' if (interactive()) {
#'    library(addinsOutline)
#'    run_addinsOutline_Rmd_bookdown()
#' }
#' @importFrom DT DTOutput renderDT datatable JS
#' @importFrom yaml read_yaml
#' @importFrom dplyr filter select arrange bind_rows tibble %>%
#' @importFrom fs path_home
#' @import miniUI rstudioapi shiny shinyFiles stringr
#' @export
run_addinsOutline_Rmd_bookdown <- function() {

 tx_title = "Table of Contents of bookdown-Rmd" # "Tabla de Contenido de Rmd"
 tx_filesbutton_lb = 'Select Rmd' # 'Selecciona Rmd'
 tx_filesbutton_ti = 'Select Rmd file' # 'Seleccione un fichero Rmd'
 tx_checkopen = "Click: Open(T)/Non Open(F)" # "Abrir(T)/Cerrar(F)"
 tx_selectchild = "Select file" # "Selecciona el fichero"
 tx_selectchild_2 = "Non Select (non child files)" # "No Seleccionable (no contiene ficheros hijos)"
 tx_done = "Exit" # "Done"
 tx_Todo = "All"  # "Todo"
 tx_updatebutton = "Update Rmd" # "Actualizar RMD"
 tx_labelfileRmd = "File Rmd: " # "Fichero Rmd: "
 tx_message = "Click on the row you want to go to" # "Haz clic en la fila a la que quiera ir"
 B_spanish = FALSE
 tx_colnames_DT = c("File","Title","Pos","InChunk")

  ui <- miniPage(
    gadgetTitleBar(tx_title,
                   left = NULL,
                   right = miniTitleBarButton("done", tx_done, primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Outline", icon = icon("table"),

        miniContentPanel(padding = 15,scrollable = FALSE,
          fluidRow(
            column(width=2,
                   shinyFilesButton('fichero_main', label=tx_filesbutton_lb, title=tx_filesbutton_ti, multiple=FALSE,
                                    style="margin-top: 25px;",icon = icon("folder-open"))
            ),
            column(width=3,
                   div(style="margin-top: 30px;",
                shiny::checkboxInput("IdCheckAbrir",tx_checkopen,value=TRUE)
                   )
            ),
            column(width=5,offset = 0,
              shiny::selectInput("IdFichero",tx_selectchild,
                                 choices = lficheros,selected = 1)
            ),
            column(width=2,
                   span(tx_message, style="color:red")
            )
          ),
          DT::DTOutput("TablaDT"),
          fluidRow(
            column(width=2,
                   div(style="margin-top: 30px;",
                     shiny::actionButton("IdActualizar",tx_updatebutton,icon = icon("refresh"))
                   )
            ),
            column(width=1,
                   div(style="margin-top: 35px;float:right;",
                       span(tx_labelfileRmd, style="color:red;font-size:10pt;")
                   )
            ),
            column(width=8,offset=0,
                   div(style="margin-top: 35px;",
                       #verbatimTextOutput('rawInputValue')
                       span(textOutput('rawInputValue'), style="color:blue")
                   )
            )
          ) # fluidRow

        ) # miniContentPanel
      )  #, # miniTabPanel
      # miniTabPanel("Parameters", icon = icon("sliders"),
      #       # Nada
      # ) # miniTabPanel


    ) # MiniTabstripPanel
  ) # ui
##########----------

##########----------
  server <- function(input, output, session) {

    volumes <- c(Home = fs::path_home(), getVolumes()())
    contexto <- rstudioapi::getActiveDocumentContext()
    texto_contexto <- contexto$contents
    Ini_nfichero_prin = contexto$path
    if (file.exists(Ini_nfichero_prin)) {
      Ini_dir_trab = dirname(Ini_nfichero_prin)
      Ini_tb_lr_limpio2 <- func_tcontenido_Rmd_todo_bookdown(Ini_nfichero_prin)
      if (is.null(Ini_tb_lr_limpio2)) {
        Ini_tb_lr_limpio2 <- func_tcontenido_Rmd_todo_no_prin_bookdown(Ini_nfichero_prin)
        if (!is.null(Ini_tb_lr_limpio2)) {
          tb_lr_limpio_Fijo = Ini_tb_lr_limpio2
        }
        lficheros <- c(tx_Todo)
        VG_label_select <- tx_selectchild_2
      } else {
        tb_lr_limpio_Fijo = Ini_tb_lr_limpio2
        #lficheros <- c(tx_Todo,sort(func_unique_bookdown(Ini_tb_lr_limpio2$Fichero)))
        lficheros <- c(tx_Todo,func_unique_bookdown(Ini_tb_lr_limpio2$Fichero))
        VG_label_select <- tx_selectchild

        updateSelectInput(session, "IdFichero",
                          label = VG_label_select,
                          choices = lficheros,
                          selected = lficheros[1]
        )

      }



    } else {
      Ini_nfichero_prin = "/nofile.Rmd"
      if (file.exists(Ini_nfichero_prin)) {
        Ini_dir_trab = dirname(Ini_nfichero_prin)
        Ini_tb_lr_limpio2 = func_tcontenido_Rmd_todo_bookdown(Ini_nfichero_prin)
        if (is.null(Ini_tb_lr_limpio2)) {
          Ini_tb_lr_limpio2 = tibble::tibble(
            Fichero = basename(Ini_nfichero_prin),
            Titulos = Ini_nfichero_prin,
            PosicionFila = 1,
            Inchunk = FALSE
          )

        }
        tb_lr_limpio_Fijo = Ini_tb_lr_limpio2
        #lficheros = c(tx_Todo,sort(func_unique_bookdown(Ini_tb_lr_limpio2$Fichero)))
        lficheros = c(tx_Todo,func_unique_bookdown(Ini_tb_lr_limpio2$Fichero))
        VG_label_select <- tx_selectchild
      } else {
        Ini_dir_trab = dirname(Ini_nfichero_prin)
        Ini_tb_lr_limpio2 = tibble::tibble(
          Fichero = c(NA),
          Titulos = c(NA),
          PosicionFila = c(NA),
          TipoChunk = c(NA)
        )

        tb_lr_limpio_Fijo = Ini_tb_lr_limpio2
        #lficheros = c(tx_Todo,sort(func_unique_bookdown(Ini_tb_lr_limpio2$Fichero)))
        lficheros = c(tx_Todo,func_unique_bookdown(Ini_tb_lr_limpio2$Fichero))
        VG_label_select <- tx_filesbutton_ti

      }

    }



    VR_Info <- reactiveValues(
      nfichero_prin = Ini_nfichero_prin,
      dir_trab = Ini_dir_trab,
      tb_lr_limpio2 = Ini_tb_lr_limpio2
    )


    observe({
       isolate({
         VR_Info$nfichero_prin = Ini_nfichero_prin
         VR_Info$dir_trab = Ini_dir_trab
         VR_Info$tb_lr_limpio2 = Ini_tb_lr_limpio2
       })

    })


    observeEvent(input$done, {
      stopApp("Exit: Done") }
    )



    #output$rawInputValue <- renderPrint({
    output$rawInputValue <- renderText({
      x = input$IdActualizar
      if (length(unlist(input$fichero_main[[1]])[-1])>0) {

        #listado = c("/Users", unlist(input$fichero_main[[1]])[-1] )
        listado = c("~", unlist(input$fichero_main[[1]])[-1] )
        #nfichero = paste(listado,sep="",collapse = "/")
        nfichero = parseFilePaths(roots=volumes, input$fichero_main)$datapath
        if (file.exists(nfichero)) {
          isolate({
            VR_Info$nfichero_prin <- nfichero
            VR_Info$dir_trab <- dirname(VR_Info$nfichero_prin)
            VR_Info$tb_lr_limpio2 <- func_tcontenido_Rmd_todo_bookdown(VR_Info$nfichero_prin)
            #cat(file=stderr(), "Paso 1", nfichero, "bins", "\n")
            if (is.null(VR_Info$tb_lr_limpio2)) {
              VR_Info$tb_lr_limpio2 <- func_tcontenido_Rmd_todo_no_prin_bookdown(VR_Info$nfichero_prin)
              tb_lr_limpio_Fijo <<- VR_Info$tb_lr_limpio2
              lficheros <<- c(tx_Todo)
              VG_label_select <<- tx_selectchild_2
            } else {
              tb_lr_limpio_Fijo <<- VR_Info$tb_lr_limpio2
              #lficheros <<- c(tx_Todo,sort(func_unique_bookdown(VR_Info$tb_lr_limpio2$Fichero)))
              lficheros <<- c(tx_Todo,func_unique_bookdown(VR_Info$tb_lr_limpio2$Fichero))
              VG_label_select <<- tx_selectchild

            }

          })
          updateSelectInput(session, "IdFichero",
                            label = VG_label_select,
                            choices = lficheros,
                            selected = lficheros[1]
          )

          #print(nfichero)
          nfichero
        } else {
          #print("In 1")
          "In 1"
        }

      } else {

        nfichero = VR_Info$nfichero_prin
        if (file.exists(nfichero)) {
          isolate({
            VR_Info$nfichero_prin <- nfichero
            VR_Info$dir_trab <- dirname(VR_Info$nfichero_prin)
            VR_Info$tb_lr_limpio2 <- func_tcontenido_Rmd_todo_bookdown(VR_Info$nfichero_prin)
            #cat(file=stderr(), "Paso 1", nfichero, "bins", "\n")
            if (is.null(VR_Info$tb_lr_limpio2)) {
              VR_Info$tb_lr_limpio2 <- func_tcontenido_Rmd_todo_no_prin_bookdown(VR_Info$nfichero_prin)
              tb_lr_limpio_Fijo <<- VR_Info$tb_lr_limpio2
              lficheros <<- c(tx_Todo)
              VG_label_select <<- tx_selectchild_2
            } else {
              tb_lr_limpio_Fijo <<- VR_Info$tb_lr_limpio2
              #lficheros <<- c(tx_Todo,sort(func_unique_bookdown(VR_Info$tb_lr_limpio2$Fichero)))
              lficheros <<- c(tx_Todo,func_unique_bookdown(VR_Info$tb_lr_limpio2$Fichero))
              VG_label_select <<- tx_selectchild

            }
          })
        }

        updateSelectInput(session, "IdFichero",
                          label = VG_label_select,
                          choices = lficheros,
                          selected = lficheros[1]
        )

        #print(VR_Info$nfichero_prin)
        VR_Info$nfichero_prin
      }



    })


    observeEvent(input$fichero_main, {
      #nfichero = shinyFileChoose(input, 'fichero_main', roots=c(roots='/Users/'), filetypes=c('Rmd','yml'))
      #nfichero = shinyFileChoose(input, 'fichero_main', roots=c(roots='~'), filetypes=c('Rmd','yml'))
      nfichero = shinyFileChoose(input, 'fichero_main', roots=volumes, filetypes=c('Rmd','yml'))

      updateSelectInput(session, "IdFichero",
                        label = VG_label_select,
                        choices = lficheros,
                        selected = lficheros[1]
      )


    })



    observeEvent(input$TablaDT_row_last_clicked, {
      if (input$IdCheckAbrir) {
        cual_sel = input$TablaDT_row_last_clicked
        func_abrir_tituloficheroRmd_bookdown(VR_Info$tb_lr_limpio2,cual=cual_sel,VR_Info$dir_trab)
      }
    })

    observeEvent(input$IdFichero, {
      ss = input$IdFichero
      if (ss!=tx_Todo) {
        VR_Info$tb_lr_limpio2 <- tb_lr_limpio_Fijo %>%
          dplyr::filter(Fichero==ss)
      } else {
        VR_Info$tb_lr_limpio2 <- tb_lr_limpio_Fijo
      }

    })

    output$TablaDT = DT::renderDT({
      s1 = input$IdFichero
      isolate({
        s2 = input$fichero_main
      })
      if (B_spanish) {
        DT::datatable(VR_Info$tb_lr_limpio2,
                      selection = "single",
                      class = 'cell-border stripe compact',
                      extensions = 'Scroller',colnames = tx_colnames_DT,
                      option = list(autoWidth = TRUE,pageLenght=10,
                                    lengthMenu = c(10,25,100,200),
                                    searchHighlight = TRUE,
                                    deferRender = TRUE,
                                    scrollY = 300,
                                    scroller = TRUE,
                                    language = list(search = "Filtrar:",
                                                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                    initComplete = DT::JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'background-color': '#008', 'color': '#fff'});",
                                      "}")
                      )
        )
      } else {
        DT::datatable(VR_Info$tb_lr_limpio2,
                      selection = "single",
                      class = 'cell-border stripe compact',
                      extensions = 'Scroller',colnames = tx_colnames_DT,
                      option = list(autoWidth = TRUE,pageLenght=10,
                                    lengthMenu = c(10,25,100,200),
                                    searchHighlight = TRUE,
                                    deferRender = TRUE,
                                    scrollY = 300,
                                    scroller = TRUE,
                                    initComplete = DT::JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'background-color': '#008', 'color': '#fff'});",
                                      "}")
                      )
        )

      }

    })


  } # final server
  #runGadget(ui, server, viewer = paneViewer(minHeight = "maximize")) # default
  #runGadget(ui, server, viewer = paneViewer()) # default
  if (is.null(getOption("addinsOutline"))) {
    runGadget(ui, server, viewer = browserViewer())
  } else if (getOption("addinsOutline")=="dialog") {
    runGadget(ui, server, viewer = dialogViewer("Table of Contents Bookdown", height = 600,width = 900))
  } else if (getOption("addinsOutline")=="pane") {
    runGadget(ui, server, viewer = paneViewer())
  } else {
    runGadget(ui, server, viewer = browserViewer())
  }
}

#get_tcontents()

source("./R/functions_tcontents_bookdown.R")
tx_Todo = "All"  # "Todo"
lficheros <- c(tx_Todo)
globalVariables(c("Fichero"))
