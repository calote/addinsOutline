library(stringr)
library(dplyr)
#library(tidyr)
globalVariables(c("%>%", "Filas", "borrar"))

file_ini = "addinsOutline_ini.txt"

func_unique_bookdown = function(x) {
  #return(unique(x))
  return(x[!duplicated(x)])
}

func_readLines_bookdown = function(file1,encoding1 = "UTF-8") {
  if (!file.exists(file1)) return(NULL)
  fic02 = readLines(file1,warn = FALSE,encoding = encoding1)
  return(fic02)
}


func_extract_files_bookdown = function(ficheroRmd_prin) {

  n_ficheroRmd_prin = basename(ficheroRmd_prin)
  if (n_ficheroRmd_prin=="_bookdown.yml") {
    inf1 = yaml::read_yaml(ficheroRmd_prin)
    crmd2 = inf1[["rmd_files"]]

    if (is.null(crmd2)) {
      # npr = inf1[["book_filename"]]
      # if (!is.null(npr)) {
      #   ficRmd2 = paste0(dirname(ficheroRmd_prin),"/",npr)
      # } else {
      #   ficRmd2 = paste0(dirname(ficheroRmd_prin),"/","index.Rmd")
      # }
      ficRmd2 = paste0(dirname(ficheroRmd_prin),"/","index.Rmd")
      if (file.exists(ficRmd2)) {
        inf1 = rmarkdown::yaml_front_matter(input=ficRmd2)
        crmd2 = inf1[["rmd_files"]]
        if (is.null(crmd2)) {
          dir1 = dirname(ficheroRmd_prin)
          lfRmd = list.files(path=dir1,pattern=".Rmd")
          crmd2 = c("index.Rmd",setdiff(lfRmd,"index.Rmd"))
        } else if (class(crmd2)=="list") {
          crmd2 = crmd2[[1]]  # el primero: "html", "latex", "word"
        }
      } else {
        return(NULL)
      }
    }
  }

  if (n_ficheroRmd_prin=="index.Rmd") {
    inf1 = rmarkdown::yaml_front_matter(input=ficheroRmd_prin)
    crmd2 = inf1[["rmd_files"]]
    if (is.null(crmd2)) {
      dir1 = dirname(ficheroRmd_prin)
      lfRmd = list.files(path=dir1,pattern=".Rmd")
      crmd2 = c("index.Rmd",setdiff(lfRmd,"index.Rmd"))
    } else if (class(crmd2)=="list") {
      crmd2 = crmd2[[1]]  # el primero: "html", "latex", "word"
    }

  } else if (n_ficheroRmd_prin!="_bookdown.yml") {
    #fic02 = readLines(ficheroRmd_prin,warn = FALSE)
    fic02 = func_readLines_bookdown(ficheroRmd_prin)
    lrmd = stringr::str_which(fic02,"^```\\{r child[:space:]?=[:space:]?[:graph:]*\\}")
    crmd = stringr::str_extract(fic02[lrmd],"['|\"][:graph:]*\\.Rmd['|\"]")
    crmd2 = stringr::str_replace_all(crmd,"['|\"]","")
    crmd2 = c(basename(ficheroRmd_prin),crmd2)
  }

  if (length(crmd2)>=1) {
    crmd3 = vector("character",length = 0)
    for (i in 1:length(crmd2)) {
      ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
      if (file.exists(ficRmd)) {
        crmd3 = c(crmd3,crmd2[i])
      }
    }
    crmd2 = crmd3
  }

  if (length(crmd2)<1) {
    return(NULL)
  }

  return(crmd2)

}


func_tcontenido_Rmd_bookdown = function(ficheroRmd_prin) {


  crmd2 = func_extract_files_bookdown(ficheroRmd_prin)

  #crmd2 = c(basename(ficheroRmd_prin),crmd2)
  lista_res = vector("list",length(crmd2))
  for (i in 1:length(crmd2)) {
    lista_parcial = list()
    ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
    #fic01 = readLines(ficRmd,warn = FALSE)
    fic01 = func_readLines_bookdown(ficRmd)
    lt1 = stringr::str_which(fic01,"^# ")
    lt2 = stringr::str_which(fic01,"^## ")
    lt3 = stringr::str_which(fic01,"^### ")
    lt4 = stringr::str_which(fic01,"^#### ")
    lt5 = stringr::str_which(fic01,"^##### ")
    lt = c(lt1,lt2,lt3,lt4,lt5)
    titulos_posiciones = sort(lt)
    titulos = fic01[titulos_posiciones]
    Inchunk = c(rep(FALSE,length(titulos)))
    nfic02 = paste0(dirname(ficheroRmd_prin),"/",file_ini)
    if (file.exists(nfic02)) {
      #fic02 = readLines(nfic02,warn=FALSE)
      fic02 = func_readLines_bookdown(nfic02)
      ltg_all = vector()
      lcad_all = vector()
      lchunk_all = vector()
      for (j in 1:length(fic02)) {
        sl1 = unlist(strsplit(fic02[j],";;"))
        if (length(sl1)>2) {
          if (sl1[2]=="rmdbd") {
            ltg = stringr::str_which(fic01,sl1[3])
            s_si = fic01[ltg]
            lcad = stringr::str_extract(s_si,sl1[3])
            ltg_all = c(ltg_all,ltg)
            lcad_all = c(lcad_all,lcad)
            if (length(ltg)>0) {
              if (length(sl1)>3) {
                if (sl1[4]=="inchunk") {
                  lchunk_all = c(lchunk_all,rep(TRUE,length(ltg)))
                } else {
                  lchunk_all = c(lchunk_all,rep(FALSE,length(ltg)))
                }
              } else {
                lchunk_all = c(lchunk_all,rep(FALSE,length(ltg)))
              }
            }
          }
        }
      }
      if (length(ltg_all)>0) {
        nll = length(titulos_posiciones)
        tt1 = tibble(
          titulos_posiciones = c(titulos_posiciones,ltg_all),
          titulos = c(titulos,lcad_all),
          inchunk = c(rep(FALSE,nll),lchunk_all)
        )
        tt1 = tt1 %>% arrange(titulos_posiciones)
        titulos_posiciones = tt1$titulos_posiciones
        titulos = tt1$titulos
        Inchunk = tt1$inchunk
      }
    }


    lista_parcial$ficheroRmd_nb = crmd2[i]
    lista_parcial$ficheroRmd_pathnb = ficRmd
    lista_parcial$titulos_posiciones = titulos_posiciones
    lista_parcial$titulos = titulos
    lista_parcial$Inchunk = Inchunk
    lista_res[[i]] = lista_parcial
  }
  return(lista_res)
}

func_tcontenido_Rmd_no_prin_bookdown = function(ficheroRmd) {
    i = 1
    lista_res = vector("list",1)
    lista_parcial = list()
    ficRmd = ficheroRmd
    #fic01 = readLines(ficRmd,warn = FALSE)
    fic01 = func_readLines_bookdown(ficRmd)
    lt1 = stringr::str_which(fic01,"^# ")
    lt2 = stringr::str_which(fic01,"^## ")
    lt3 = stringr::str_which(fic01,"^### ")
    lt4 = stringr::str_which(fic01,"^#### ")
    lt5 = stringr::str_which(fic01,"^##### ")
    lt = c(lt1,lt2,lt3,lt4,lt5)
    titulos_posiciones = sort(lt)
    titulos = fic01[titulos_posiciones]
    Inchunk = c(rep(FALSE,length(titulos)))
    nfic02 = paste0(dirname(ficheroRmd),"/",file_ini)
    if (file.exists(nfic02)) {
      #fic02 = readLines(nfic02,warn=FALSE)
      fic02 = func_readLines_bookdown(fic02)
      ltg_all = vector()
      lcad_all = vector()
      lchunk_all = vector()
      for (j in 1:length(fic02)) {
        sl1 = unlist(strsplit(fic02[j],";;"))
        if (length(sl1)>2) {
          if (sl1[2]=="rmdbd") {
            ltg = stringr::str_which(fic01,sl1[3])
            s_si = fic01[ltg]
            lcad = stringr::str_extract(s_si,sl1[3])
            ltg_all = c(ltg_all,ltg)
            lcad_all = c(lcad_all,lcad)
            if (length(ltg)>0) {
              if (length(sl1)>3) {
                if (sl1[4]=="inchunk") {
                  lchunk_all = c(lchunk_all,rep(TRUE,length(ltg)))
                } else {
                  lchunk_all = c(lchunk_all,rep(FALSE,length(ltg)))
                }
              } else {
                lchunk_all = c(lchunk_all,rep(FALSE,length(ltg)))
              }
            }
          }
        }
      }
      if (length(ltg_all)>0) {
        nll = length(titulos_posiciones)
        tt1 = tibble(
          titulos_posiciones = c(titulos_posiciones,ltg_all),
          titulos = c(titulos,lcad_all),
          inchunk = c(rep(FALSE,nll),lchunk_all)
        )
        tt1 = tt1 %>% arrange(titulos_posiciones)
        titulos_posiciones = tt1$titulos_posiciones
        titulos = tt1$titulos
        Inchunk = tt1$inchunk
      }
    }
    lista_parcial$ficheroRmd_nb = ficRmd
    lista_parcial$ficheroRmd_pathnb = ficRmd
    lista_parcial$titulos_posiciones = titulos_posiciones
    lista_parcial$titulos = titulos
    lista_parcial$Inchunk = Inchunk
    lista_res[[i]] = lista_parcial
  return(lista_res)
}


func_tcontenido_Rmd_tb_bookdown = function(lr) {
  tt = NULL
  for (i in 1:length(lr)) {
    fiche = lr[[i]]$ficheroRmd_nb
    vtitulos = lr[[i]]$titulos
    ptitulos = lr[[i]]$titulos_posiciones
    pinchunk = lr[[i]]$Inchunk
    if (length(vtitulos)>0) {
      t1 = tibble::tibble(
        Fichero = rep(fiche,length(vtitulos)),
        Titulos = vtitulos,
        PosicionFila = ptitulos,
        Inchunk = pinchunk
      )
      if (is.null(tt)) {
        tt = t1
      } else {
        tt = dplyr::bind_rows(tt,t1)
      }
    }
  }
  return(tt)
}

func_abrir_tituloficheroRmd_bookdown = function(tb_lr,cual,dir_trabajo) {
  fichero = paste0(dir_trabajo,"/",tb_lr$Fichero[cual])
  fila = tb_lr$PosicionFila[cual]
  rstudioapi::navigateToFile(file = fichero,
                             line = fila,
                             column = 1)
}

func_limpiar_dentrochunk_bookdown = function(ficheroRmd_prin) {
  # parte 1
  # fic02 = readLines(ficheroRmd_prin,warn = FALSE)
  # lrmd = stringr::str_which(fic02,"^```\\{r child[:space:]?=[:space:]?[:graph:]*\\}")
  # crmd = stringr::str_extract(fic02[lrmd],"['|\"][:graph:]*\\.Rmd['|\"]")
  # crmd2 = stringr::str_replace_all(crmd,"['|\"]","")

  crmd2 = func_extract_files_bookdown(ficheroRmd_prin)

  crmd2 = c(basename(ficheroRmd_prin),crmd2)
  lista_res = vector("list",length(crmd2))
  for (i in 1:length(crmd2)) {
    lista_parcial = list()
    ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
    #fic01 = readLines(ficRmd,warn = FALSE)
    fic01 = func_readLines_bookdown(ficRmd)
    lt1 = stringr::str_which(fic01,"^```\\{")  # "inicio"
    lt1b = stringr::str_which(fic01,"^```r")  # "inicio"
    lt2 = stringr::str_which(fic01,"^```[:space:]*")      # "fin"
    lt2 = setdiff(lt2,lt1)
    lt2 = setdiff(lt2,lt1b)
    v_posi = stringr::str_which(fic01,"^---") # cab. yaml
    if (length(v_posi)>1) {
      lt = c(lt1,lt1b,lt2,v_posi[1:2])
      lbtt = tibble::tibble(Filas = lt,
                            Chunk = c(rep("inicio",length(lt1)),
                                      rep("inicio",length(lt1b)),
                                      rep("fin",length(lt2)),
                                      c("inicio","fin")
                            )
      )
    } else {
      lt = c(lt1,lt1b,lt2)
      lbtt = tibble::tibble(Filas = lt,
                            Chunk = c(rep("inicio",length(lt1)),
                                      rep("inicio",length(lt1b)),
                                      rep("fin",length(lt2))
                                      )
      )
    }
    lbtt2 = lbtt %>%
      arrange(Filas)
    titulos_posiciones = lbtt2$Filas
    titulos = fic01[titulos_posiciones]
    lista_parcial$ficheroRmd_nb = crmd2[i]
    lista_parcial$ficheroRmd_pathnb = ficRmd
    lista_parcial$titulos_posiciones = titulos_posiciones
    lista_parcial$titulos_chunk = lbtt2$Chunk
    lista_parcial$titulos = titulos
    lista_res[[i]] = lista_parcial
  }
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
      tt = dplyr::bind_rows(tt,t1)
    }
  }
  return(tt)


}


func_limpiar_dentrochunk_no_prin_bookdown = function(ficheroRmd_prin) {
  crmd2 = basename(ficheroRmd_prin)
  lista_res = vector("list",length(crmd2))
  for (i in 1:length(crmd2)) {
    lista_parcial = list()
    ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
    #fic01 = readLines(ficRmd,warn = FALSE)
    fic01 = func_readLines_bookdown(ficRmd)
    lt1 = stringr::str_which(fic01,"^```\\{")  # "inicio"
    lt1b = stringr::str_which(fic01,"^```r")  # "inicio"
    lt2 = stringr::str_which(fic01,"^```[:space:]*")      # "fin"
    lt2 = setdiff(lt2,lt1)
    lt2 = setdiff(lt2,lt1b)
    v_posi = stringr::str_which(fic01,"^---") # cab. yaml
    if (length(v_posi)>1) {
      lt = c(lt1,lt1b,lt2,v_posi[1:2])
      lbtt = tibble::tibble(Filas = lt,
                          Chunk = c(rep("inicio",length(lt1)),
                                    rep("inicio",length(lt1b)),
                                    rep("fin",length(lt2)),
                                    c("inicio","fin")
                                    )
             )
    } else {
      lt = c(lt1,lt1b,lt2)
      lbtt = tibble::tibble(Filas = lt,
                            Chunk = c(rep("inicio",length(lt1)),
                                      rep("inicio",length(lt1b)),
                                      rep("fin",length(lt2)))
      )

    }
    lbtt2 = lbtt %>%
      arrange(Filas)
    titulos_posiciones = lbtt2$Filas
    titulos = fic01[titulos_posiciones]
    lista_parcial$ficheroRmd_nb = crmd2[i]
    lista_parcial$ficheroRmd_pathnb = ficRmd
    lista_parcial$titulos_posiciones = titulos_posiciones
    lista_parcial$titulos_chunk = lbtt2$Chunk
    lista_parcial$titulos = titulos
    lista_res[[i]] = lista_parcial
  }
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
      tt = dplyr::bind_rows(tt,t1)
    }
  }
  return(tt)


}




func_limpiar_mejorado_bookdown = function(tb_lr,tb_limp) {
  tb_lr_limpio = tb_lr
  tb_lr_limpio$borrar = FALSE
  for (i in 1:nrow(tb_lr)) {
    fila = tb_lr$PosicionFila[i]
    fichero = tb_lr$Fichero[i]
    Bsi = FALSE
    Bsi1 = FALSE
    Bsisi2 = FALSE
    j = 1
    while (j <= nrow(tb_limp)) {
      fichero2 = tb_limp$Fichero[j]
      if (fichero==fichero2) {
        Bsi = TRUE
        Bsi1 = TRUE
        fila2 = tb_limp$PosicionFila[j]
        tipo2 = tb_limp$TipoChunk[j]
        if ((tipo2=="inicio") & (fila>fila2) ){
          Bsisi2 = TRUE
          fila3 = tb_limp$PosicionFila[j+1]
          tipo3 = tb_limp$TipoChunk[j+1]
          if ((tipo3=="fin") & (fila<fila3) ){
            if (!tb_lr_limpio$Inchunk[i]) {
              tb_lr_limpio$borrar[i] = TRUE
            }
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
    dplyr::filter(!borrar) %>%
    dplyr::select(-borrar)

  return(tb_lr_limpio2)
}


func_tcontenido_Rmd_todo_bookdown = function(nfichero_prin) {
  lr = func_tcontenido_Rmd_bookdown(nfichero_prin)
  if (is.null(lr)) {
    return(NULL)
  }
  tb_lr = func_tcontenido_Rmd_tb_bookdown(lr)
  tb_limp = func_limpiar_dentrochunk_bookdown(nfichero_prin)
  tb_lr_limpio2 = func_limpiar_mejorado_bookdown(tb_lr,tb_limp)
  return(tb_lr_limpio2)

}


func_tcontenido_Rmd_todo_no_prin_bookdown = function(nfichero_prin) {

  lr = func_tcontenido_Rmd_no_prin_bookdown(nfichero_prin)
  tb_lr = func_tcontenido_Rmd_tb_bookdown(lr)
  #cat(file=stderr(), "drawing histogram with", nfichero_prin, "bins", "\n")
  #stopApp("Adios")
  #str(lr)
  if (is.null(tb_lr)) {
    tb_lr_limpio2 = tibble::tibble(
      Fichero = basename(nfichero_prin),
      Titulos = nfichero_prin,
      PosicionFila = 1,
      Inchunk = FALSE
    )
  } else {
    tb_lr$Fichero = basename(tb_lr$Fichero)
    tb_limp = func_limpiar_dentrochunk_no_prin_bookdown(nfichero_prin)
    tb_limp$Fichero = basename(tb_limp$Fichero)
    tb_lr_limpio2 = func_limpiar_mejorado_bookdown(tb_lr,tb_limp)
    tb_lr_limpio2$Fichero = basename(tb_lr_limpio2$Fichero)
  }
  return(tb_lr_limpio2)

}



