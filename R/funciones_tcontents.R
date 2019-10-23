library(stringr)
library(dplyr)
library(tidyr)

func_tcontenido_Rmd = function(ficheroRmd_prin) {
  fic02 = readLines(ficheroRmd_prin,warn = FALSE)
  lrmd = str_which(fic02,"^```\\{r child[:space:]?=[:space:]?[:graph:]*\\}")
  crmd = str_extract(fic02[lrmd],"'[:graph:]*\\.Rmd'")
  crmd2 = str_replace_all(crmd,"'","")
  if (length(crmd2)<1) {
    return(NULL)
  }

  lista_res = vector("list",length(crmd2))
  #browser()
  for (i in 1:length(crmd2)) {
    lista_parcial = list()
    ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
    fic01 = readLines(ficRmd,warn = FALSE)
    lt1 = str_which(fic01,"^# ")
    lt2 = str_which(fic01,"^## ")
    lt3 = str_which(fic01,"^### ")
    lt4 = str_which(fic01,"^#### ")
    lt5 = str_which(fic01,"^##### ")
    #fic01[27]
    lt = c(lt1,lt2,lt3,lt4,lt5)
    titulos_posiciones = sort(lt)
    titulos = fic01[titulos_posiciones]
    lista_parcial$ficheroRmd_nb = crmd2[i]
    lista_parcial$ficheroRmd_pathnb = ficRmd
    lista_parcial$titulos_posiciones = titulos_posiciones
    lista_parcial$titulos = titulos
    lista_res[[i]] = lista_parcial
  }
  return(lista_res)
}

func_tcontenido_Rmd_no_prin = function(ficheroRmd) {
    i = 1
    lista_res = vector("list",1)
    lista_parcial = list()
    #ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
    ficRmd = ficheroRmd
    fic01 = readLines(ficRmd,warn = FALSE)
    lt1 = str_which(fic01,"^# ")
    lt2 = str_which(fic01,"^## ")
    lt3 = str_which(fic01,"^### ")
    lt4 = str_which(fic01,"^#### ")
    lt5 = str_which(fic01,"^##### ")
    #fic01[27]
    lt = c(lt1,lt2,lt3,lt4,lt5)
    titulos_posiciones = sort(lt)
    titulos = fic01[titulos_posiciones]
    lista_parcial$ficheroRmd_nb = ficRmd
    lista_parcial$ficheroRmd_pathnb = ficRmd
    lista_parcial$titulos_posiciones = titulos_posiciones
    lista_parcial$titulos = titulos
    lista_res[[i]] = lista_parcial
  return(lista_res)
}


func_tcontenido_Rmd_tb = function(lr) {
  tt = NULL
  for (i in 1:length(lr)) {
    fiche = lr[[i]]$ficheroRmd_nb
    vtitulos = lr[[i]]$titulos
    ptitulos = lr[[i]]$titulos_posiciones
    t1 = tibble::tibble(
      Fichero = rep(fiche,length(vtitulos)),
      Titulos = vtitulos,
      PosicionFila = ptitulos
    )
    if (is.null(tt)) {
      tt = t1
    } else {
      tt = bind_rows(tt,t1)
    }
  }
  return(tt)
}

func_abrir_tituloficheroRmd = function(tb_lr,cual,dir_trabajo) {
  #browser()
  fichero = paste0(dir_trabajo,"/",tb_lr$Fichero[cual])
  fila = tb_lr$PosicionFila[cual]
  rstudioapi::navigateToFile(file = fichero,
                             line = fila,
                             column = 1)
}

func_limpiar_dentrochunk = function(ficheroRmd_prin) {
  #browser()
  # parte 1
  fic02 = readLines(ficheroRmd_prin,warn = FALSE)
  lrmd = str_which(fic02,"^```\\{r child[:space:]?=[:space:]?[:graph:]*\\}")
  crmd = str_extract(fic02[lrmd],"'[:graph:]*\\.Rmd'")
  crmd2 = str_replace_all(crmd,"'","")
  lista_res = vector("list",length(crmd2))
  #browser()
  for (i in 1:length(crmd2)) {
    lista_parcial = list()
    ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
    fic01 = readLines(ficRmd,warn = FALSE)
    lt1 = str_which(fic01,"^```\\{")  # "inicio"
    lt1b = str_which(fic01,"^```r")  # "inicio"
    lt2 = str_which(fic01,"^```[:space:]*")      # "fin"
    lt2 = setdiff(lt2,lt1)
    lt2 = setdiff(lt2,lt1b)
    lt = c(lt1,lt1b,lt2)
    lbtt = tibble::tibble(Filas = lt,
                          Chunk = c(rep("inicio",length(lt1)),
                                    rep("inicio",length(lt1b)),
                                    rep("fin",length(lt2)))
    )
    lbtt2 = lbtt %>%
      arrange(Filas)
    #titulos_posiciones = sort(lt)
    titulos_posiciones = lbtt2$Filas
    titulos = fic01[titulos_posiciones]
    lista_parcial$ficheroRmd_nb = crmd2[i]
    lista_parcial$ficheroRmd_pathnb = ficRmd
    lista_parcial$titulos_posiciones = titulos_posiciones
    lista_parcial$titulos_chunk = lbtt2$Chunk
    lista_parcial$titulos = titulos
    lista_res[[i]] = lista_parcial
  }
  #return(lista_res)
  # parte 2
  lr = lista_res
  tt = NULL
  for (i in 1:length(lr)) {
    fiche = lr[[i]]$ficheroRmd_nb
    vtitulos = lr[[i]]$titulos
    ptitulos = lr[[i]]$titulos_posiciones
    pchunk = lr[[i]]$titulos_chunk
    t1 = tibble::tibble(
      Fichero = rep(fiche,length(vtitulos)),
      Titulos = vtitulos,
      PosicionFila = ptitulos,
      TipoChunk = pchunk
    )
    if (is.null(tt)) {
      tt = t1
    } else {
      tt = bind_rows(tt,t1)
    }
  }
  #return(tt)
  return(tt)


}


func_limpiar_dentrochunk_no_prin = function(ficheroRmd_prin) {
  #browser()
  # parte 1
  # fic02 = readLines(ficheroRmd_prin,warn = FALSE)
  # lrmd = str_which(fic02,"^```\\{r child[:space:]?=[:space:]?[:graph:]*\\}")
  # crmd = str_extract(fic02[lrmd],"'[:graph:]*\\.Rmd'")
  # crmd2 = str_replace_all(crmd,"'","")
  crmd2 = basename(ficheroRmd_prin)
  lista_res = vector("list",length(crmd2))
  #browser()
  for (i in 1:length(crmd2)) {
    lista_parcial = list()
    ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
    fic01 = readLines(ficRmd,warn = FALSE)
    lt1 = str_which(fic01,"^```\\{")  # "inicio"
    lt1b = str_which(fic01,"^```r")  # "inicio"
    lt2 = str_which(fic01,"^```[:space:]*")      # "fin"
    lt2 = setdiff(lt2,lt1)
    lt2 = setdiff(lt2,lt1b)
    lt = c(lt1,lt1b,lt2)
    lbtt = tibble::tibble(Filas = lt,
                          Chunk = c(rep("inicio",length(lt1)),
                                    rep("inicio",length(lt1b)),
                                    rep("fin",length(lt2)))
    )
    lbtt2 = lbtt %>%
      arrange(Filas)
    #titulos_posiciones = sort(lt)
    titulos_posiciones = lbtt2$Filas
    titulos = fic01[titulos_posiciones]
    lista_parcial$ficheroRmd_nb = crmd2[i]
    lista_parcial$ficheroRmd_pathnb = ficRmd
    lista_parcial$titulos_posiciones = titulos_posiciones
    lista_parcial$titulos_chunk = lbtt2$Chunk
    lista_parcial$titulos = titulos
    lista_res[[i]] = lista_parcial
  }
  #return(lista_res)
  # parte 2
  lr = lista_res
  tt = NULL
  for (i in 1:length(lr)) {
    fiche = lr[[i]]$ficheroRmd_nb
    vtitulos = lr[[i]]$titulos
    ptitulos = lr[[i]]$titulos_posiciones
    pchunk = lr[[i]]$titulos_chunk
    t1 = tibble::tibble(
      Fichero = rep(fiche,length(vtitulos)),
      Titulos = vtitulos,
      PosicionFila = ptitulos,
      TipoChunk = pchunk
    )
    if (is.null(tt)) {
      tt = t1
    } else {
      tt = bind_rows(tt,t1)
    }
  }
  #return(tt)
  return(tt)


}


# nfichero_prin = "/Users/calvo/Downloads/FESTAD/FESTADRMD/FESTADmain.Rmd"
# dir_trab = dirname(nfichero_prin)
# lr = func_tcontenido_Rmd(nfichero_prin)
# tb_lr = func_tcontenido_Rmd_tb(lr)
# tb_lr2 = tb_lr %>%
#   filter(!str_detect(Titulos,"salida_"))
# func_abrir_tituloficheroRmd(tb_lr2,cual=78,dir_trab)
# DT::datatable(tb_lr2)
# tb_limp = func_limpiar_dentrochunk(nfichero_prin)
# func_abrir_tituloficheroRmd(tb_limp,cual=3,dir_trab)


func_limpiar_mejorado = function(tb_lr,tb_limp) {
  tb_lr_limpio = tb_lr
  tb_lr_limpio$borrar = FALSE
  for (i in 1:nrow(tb_lr)) {
    fila = tb_lr$PosicionFila[i]
    fichero = tb_lr$Fichero[i]
    Bsi = F
    Bsi1 = F
    Bsisi2 = F
    j = 1
    while (j <= nrow(tb_limp)) {
      fichero2 = tb_limp$Fichero[j]
      if (fichero==fichero2) {
        Bsi = T
        Bsi1 = T
        fila2 = tb_limp$PosicionFila[j]
        tipo2 = tb_limp$TipoChunk[j]
        if ((tipo2=="inicio") & (fila>fila2) ){
          Bsisi2 = T
          fila3 = tb_limp$PosicionFila[j+1]
          tipo3 = tb_limp$TipoChunk[j+1]
          if ((tipo3=="fin") & (fila<fila3) ){
            tb_lr_limpio$borrar[i] = TRUE
            j = nrow(tb_limp)+1
          }
        }
      }
      if ((Bsi1) & (fichero!=fichero2)) {
        j = nrow(tb_limp)+1
      }
      j = j+1
    }

  }


  tb_lr_limpio2 = tb_lr_limpio %>%
    filter(!borrar) %>%
    select(-borrar)

  return(tb_lr_limpio2)
}


func_tcontenido_Rmd_todo = function(nfichero_prin) {

  lr = func_tcontenido_Rmd(nfichero_prin)
  if (is.null(lr)) {
    return(NULL)
  }
  tb_lr = func_tcontenido_Rmd_tb(lr)
  tb_limp = func_limpiar_dentrochunk(nfichero_prin)
  tb_lr_limpio2 = func_limpiar_mejorado(tb_lr,tb_limp)
  #func_abrir_tituloficheroRmd(tb_limp,cual=3,dir_trab)
  return(tb_lr_limpio2)

}


func_tcontenido_Rmd_todo_no_prin = function(nfichero_prin) {

  #lr = func_tcontenido_Rmd(nfichero_prin)
  #browser()
  lr = func_tcontenido_Rmd_no_prin(nfichero_prin)
  tb_lr = func_tcontenido_Rmd_tb(lr)
  #tb_limp = func_limpiar_dentrochunk(nfichero_prin)
  tb_limp = func_limpiar_dentrochunk_no_prin(nfichero_prin)
  tb_lr_limpio2 = func_limpiar_mejorado(tb_lr,tb_limp)
  #func_abrir_tituloficheroRmd(tb_limp,cual=3,dir_trab)
  tb_lr_limpio2$Fichero = basename(tb_lr_limpio2$Fichero)
  return(tb_lr_limpio2)

}





# tb_lr_limpio2 = func_limpiar_mejorado(tb_lr,tb_limp)
# DT::datatable(tb_lr_limpio2)
# func_abrir_tituloficheroRmd(tb_lr_limpio2,cual=104,dir_trab)

#nfichero_prin = "/Users/calvo/Downloads/FESTAD/FESTADRMD/asigFEPR_13_ProgramaFunciones.Rmd"
#func_tcontenido_Rmd_todo_no_prin(nfichero_prin)
