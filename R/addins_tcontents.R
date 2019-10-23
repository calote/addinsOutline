#' Subset a Data Frame.
#'
#' Interactively subset a \code{data.frame}. The resulting
#' code will be emitted as a call to the \code{\link{subset}}
#' function.
#'
#' This addin can be used to interactively subset a \code{data.frame}.
#' The intended way to use this is as follows:
#'
#' 1. Highlight a symbol naming a \code{data.frame} in your R session,
#'    e.g. \code{mtcars},
#' 2. Execute this addin, to interactively subset it.
#'
#' When you're done, the code performing this operation will be emitted
#' at the cursor position.
#'
#' @export
get_tcontents <- function() {


  ui <- miniPage(
    gadgetTitleBar("Tabla de Contenido"),
    miniContentPanel(
      fluidRow(
        column(width=4,
               shinyFilesButton('fichero_main', label='Selecciona Rmd', title='Seleccione un fichero Rmd', multiple=FALSE)
          #shiny::textInput("fichero_main","Selecciona camino+nombre doc. R Markdown")
          # shiny::fileInput("fichero_main","Selecciona fichero Rmd principal",
          #                  accept = c(
          #                    "text", #"text/csv",
          #                    "text/plain",
          #                    #"text/comma-separated-values,text/plain",
          #                    ".Rmd"))
        ),
        column(width=2,
            shiny::checkboxInput("IdCheckAbrir","Abrir(T)/Cerrar(F)",value=F)
        ),
        column(width=5,offset = 1,
          #passwordInput("password", "")
          shiny::selectInput("IdFichero","Selecciona el fichero",
                             choices = lficheros,selected = 1)
        )
      ),
      DT::DTOutput("TablaDT"),
      fluidRow(
        column(width=2,
               shiny::actionButton("IdActualizar","Actualizar RMD")
        ),
        column(width=11,offset=1,
           verbatimTextOutput('rawInputValue')
        )
      )
      #verbatimTextOutput('filepaths')
    )
  ) # final ui
##########----------

##########----------
  server <- function(input, output, session) {

    contexto <- rstudioapi::getActiveDocumentContext()
    texto_contexto <- contexto$contents
    Ini_nfichero_prin = contexto$path
    #browser()
    if (file.exists(Ini_nfichero_prin)) {
      Ini_dir_trab = dirname(Ini_nfichero_prin)
      Ini_tb_lr_limpio2 <- func_tcontenido_Rmd_todo(Ini_nfichero_prin)
      if (is.null(Ini_tb_lr_limpio2)) {
        # si se produce error es individual
        Ini_tb_lr_limpio2 <- func_tcontenido_Rmd_todo_no_prin(Ini_nfichero_prin)
        tb_lr_limpio_Fijo = Ini_tb_lr_limpio2
        lficheros <- c("Todo")
        VG_label_select <- "No Seleccionable (no contiene ficheros hijos)"
      } else {
        tb_lr_limpio_Fijo = Ini_tb_lr_limpio2
        lficheros <- c("Todo",sort(unique(Ini_tb_lr_limpio2$Fichero)))
        VG_label_select <- "Selecciona el fichero"
      }



    } else {
      #browser()
      Ini_nfichero_prin = "/Users/calvo/Downloads/FESTAD/FESTADRMD/FESTADmain.Rmd"
      if (file.exists(Ini_nfichero_prin)) {
        Ini_dir_trab = dirname(Ini_nfichero_prin)
        Ini_tb_lr_limpio2 = func_tcontenido_Rmd_todo(Ini_nfichero_prin)
        tb_lr_limpio_Fijo = Ini_tb_lr_limpio2
        lficheros = c("Todo",sort(unique(Ini_tb_lr_limpio2$Fichero)))
        VG_label_select <- "Selecciona el fichero"
      } else {
        Ini_dir_trab = dirname(Ini_nfichero_prin)
        Ini_tb_lr_limpio2 = tibble::tibble(
          Fichero = c(NA),
          Titulos = c(NA),
          PosicionFila = c(NA),
          TipoChunk = c(NA)
        )

        tb_lr_limpio_Fijo = Ini_tb_lr_limpio2
        lficheros = c("Todo",sort(unique(Ini_tb_lr_limpio2$Fichero)))
        VG_label_select <- "Seleccione un fichero RMD"

      }

    }



    #browser()
    # VR_Info_inicial = reactiveValues(
    #    fic_inicial00 = rstudioapi::getActiveDocumentContext(),
    #    fic_inicial = rstudioapi::getActiveDocumentContext()$path
    # )

    VR_Info <- reactiveValues(
      #nfichero_prin = "/Users/calvo/Downloads/FESTAD/FESTADRMD/FESTADmain.Rmd",
      #nfichero_prin = rstudioapi::getActiveDocumentContext()$path,
      #dir_trab = dirname(nfichero_prin),
      #tb_lr_limpio2 = func_tcontenido_Rmd_todo(nfichero_prin)
      nfichero_prin = Ini_nfichero_prin,
      dir_trab = Ini_dir_trab,
      tb_lr_limpio2 = Ini_tb_lr_limpio2
    )


    observe({
       isolate({
         #browser()
         VR_Info$nfichero_prin = Ini_nfichero_prin
         VR_Info$dir_trab = Ini_dir_trab
         VR_Info$tb_lr_limpio2 = Ini_tb_lr_limpio2
       })

    })

        # observe({
    #   #browser()
    #   nfichero = rstudioapi::getActiveDocumentContext()$path
    #
    #   isolate({
    #     if (file.exists(nfichero)) {
    #       VR_Info$nfichero_prin <- nfichero
    #       VR_Info$dir_trab <- dirname(VR_Info$nfichero_prin)
    #       VR_Info$tb_lr_limpio2 <- func_tcontenido_Rmd_todo(VR_Info$nfichero_prin)
    #       if (is.null(VR_Info$tb_lr_limpio2)) {
    #         # si se produce error es individual
    #         VR_Info$tb_lr_limpio2 <- func_tcontenido_Rmd_todo_no_prin(VR_Info$nfichero_prin)
    #         lficheros <<- c("Todo")
    #         VG_label_select <<- "No Seleccionable (no contiene ficheros hijos)"
    #       } else {
    #         tb_lr_limpio_Fijo = VR_Info$tb_lr_limpio2
    #         lficheros <<- c("Todo",sort(unique(tb_lr_limpio2$Fichero)))
    #         VG_label_select <<- "Selecciona el fichero"
    #       }
    #     }
    #
    #   })
    #
    #
    # })

    #VR_Info$nfichero_prin <- nfichero_prin
    #VR_Info$dir_trab = dir_trab
    #VR_Info$tb_lr_limpio2 = tb_lr_limpio2

    observeEvent(input$done, {
      stopApp("Sale pulsando: Done") }
    )
    observeEvent(input$cancel, {
      #stopApp(stop("Sale pulsando: Cancel", call. = FALSE))
      stopApp("Sale pulsando: Cancel")
    })

    # observeEvent(input$fichero_main, {
    #   ficherodf = input$fichero_main
    #   fichero_path = ficherodf$datapath[1]
    #   fichero_name = ficherodf$name[1]
    #   stopApp(stop(fichero_path, call. = FALSE))
    # })
    #/Users/calvo/Downloads/FESTAD/FESTADRMD/FESTADmain.Rmd


    # #observe({
    # observeEvent(input$fichero_main,{
    #   #x <- input$fichero_main
    #
    #   # Can use character(0) to remove all choices
    #   #if (is.null(x))
    #   #  x <- character(0)
    #
    #   # shiny::selectInput("IdFichero","Selecciona el fichero",
    #   #                    choices = lficheros,selected = 1)
    #
    #
    #   # Can also set the label and select items
    #   updateSelectInput(session, "IdFichero",
    #                     label = "Selecciona el fichero 2",
    #                     choices = lficheros,
    #                     selected = 1
    #   )
    # })

    # output$filepaths <- renderPrint({
    #   x = input$IdCheckAbrir
    #   #browser()
    #   VR_Info_inicial$fic_inicial
    #   #VR_Info_inicial$fic_inicial00
    # })

    output$rawInputValue <- renderPrint({
      #str(input$fichero_main)
      #browser()
      x = input$IdActualizar
      if (length(unlist(input$fichero_main[[1]])[-1])>0) {
      #if (length(input$fichero_main)>0) {
      #  if (length(unlist(input$fichero_main[[1]]))>0) {

        listado = c("/Users", unlist(input$fichero_main[[1]])[-1] )
        nfichero = paste(listado,sep="",collapse = "/")
        if (file.exists(nfichero)) {

          isolate({
            VR_Info$nfichero_prin <- nfichero
            VR_Info$dir_trab <- dirname(VR_Info$nfichero_prin)
            VR_Info$tb_lr_limpio2 <- func_tcontenido_Rmd_todo(VR_Info$nfichero_prin)
            if (is.null(VR_Info$tb_lr_limpio2)) {
              # si se produce error es individual
              VR_Info$tb_lr_limpio2 <- func_tcontenido_Rmd_todo_no_prin(VR_Info$nfichero_prin)
              tb_lr_limpio_Fijo <<- VR_Info$tb_lr_limpio2
              lficheros <<- c("Todo")
              VG_label_select <<- "No Seleccionable (no contiene ficheros hijos)"
            } else {
              tb_lr_limpio_Fijo <<- VR_Info$tb_lr_limpio2
              lficheros <<- c("Todo",sort(unique(VR_Info$tb_lr_limpio2$Fichero)))
              VG_label_select <<- "Selecciona el fichero"

            }

          })

          # nfichero_prin <<- nfichero
          # dir_trab <<- dirname(nfichero_prin)
          # tb_lr_limpio2 <<- func_tcontenido_Rmd_todo(nfichero_prin)
          # if (is.null(tb_lr_limpio2)) {
          #   # si se produce error es individual
          #   tb_lr_limpio2 <<- func_tcontenido_Rmd_todo_no_prin(nfichero_prin)
          # }

          nfichero
        } else {
          ""
        }

      } else {
        ""
      }


    })


    observeEvent(input$fichero_main, {
      nfichero = shinyFileChoose(input, 'fichero_main', roots=c(roots='/Users/'), filetypes=c('Rmd'))
      #nfichero_prin <<- "/Users/calvo/Downloads/FESTAD/FESTADRMD/FESTADmain.Rmd"
      #nfichero = paste("/Users",unlist(input$fichero_main[[1]]),sep="/",collapse = "/")


      updateSelectInput(session, "IdFichero",
                        label = VG_label_select,
                        choices = lficheros,
                        selected = 1
      )

    })


    # output$filepaths <- renderPrint({
    #   ## parseFilePaths('/Users/', input$fichero_main)
    #   #nfichero_prin
    #   })


    #input$TablaDT_row_last_clicked
    #input$TablaDT_rows_selected
    observeEvent(input$TablaDT_row_last_clicked, {
      if (input$IdCheckAbrir) {
        cual_sel = input$TablaDT_row_last_clicked
        #func_abrir_tituloficheroRmd(tb_lr_limpio2,cual=cual_sel,dir_trab)
        func_abrir_tituloficheroRmd(VR_Info$tb_lr_limpio2,cual=cual_sel,VR_Info$dir_trab)
      }
    })

    observeEvent(input$IdFichero, {
      #tb_lr_limpio_Fijo = tb_lr_limpio2
      #browser()
      #ss = lficheros[input$IdFichero]
      ss = input$IdFichero

      if (ss!="Todo") {
        VR_Info$tb_lr_limpio2 <- tb_lr_limpio_Fijo %>%
          filter(Fichero==ss)
      } else {
        VR_Info$tb_lr_limpio2 <- tb_lr_limpio_Fijo
      }

      # if (ss!="Todo") {
      # tb_lr_limpio2 <<- tb_lr_limpio_Fijo %>%
      #   filter(Fichero==ss)
      # } else {
      #   tb_lr_limpio2 <<- tb_lr_limpio_Fijo
      # }


    })

    output$TablaDT = DT::renderDT({
      s1 = input$IdFichero
      s2 = input$fichero_main
      #browser()
      DT::datatable(VR_Info$tb_lr_limpio2,  #tb_lr_limpio2,
                    selection = "single",
                    class = 'cell-border stripe compact',
                    extensions = 'Scroller',
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
                                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                    "}")
                                  )
      )

    })


  } # final server
  #runGadget(ui, server, viewer = paneViewer(minHeight = "maximize")) # default
  #runGadget(ui, server, viewer = paneViewer()) # default
  #runGadget(ui, server, viewer = dialogViewer("Tabla Contenido", height = 600,width = 900))
  runGadget(ui, server, viewer = browserViewer())
}

#get_tcontents()


# Referencias curiosas:

# - DT:
#       * <https://laustep.github.io/stlahblog/posts/PlotDatatableColumns.html>

# - ShinyFiles:
#        * <https://www.rdocumentation.org/packages/shinyFiles/versions/0.7.3>



# library(miniUI)
# library(shiny)
# library(shinyFiles)
# library(DT)
# library(stringr)


source("./R/funciones_tcontents.R")
lficheros <- c("Todo")

#fic02 = readLines("/Users/calvo/Downloads/FESTAD/FESTADRMD/FESTADmain.Rmd")


# nfichero_prin = "/Users/calvo/Downloads/FESTAD/FESTADRMD/FESTADmain.Rmd"
# dir_trab = dirname(nfichero_prin)
# tb_lr_limpio2 = func_tcontenido_Rmd_todo(nfichero_prin)
#
# #DT::datatable(tb_lr_limpio2)
# #func_abrir_tituloficheroRmd(tb_lr_limpio2,cual=104,dir_trab)
# tb_lr_limpio_Fijo = tb_lr_limpio2
# lficheros = c("Todo",sort(unique(tb_lr_limpio2$Fichero)))
# VG_label_select <<- "Selecciona el fichero"
