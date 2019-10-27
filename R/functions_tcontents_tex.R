library(stringr)
library(dplyr)
#library(tidyr)
globalVariables(c("%>%", "Filas", "borrar"))

file_ini = "addinsOutline_ini.txt"

func_readLines_tex = function(file1,encoding1 = "UTF-8") {
  if (!file.exists(file1)) return(NULL)
  fic02 = readLines(file1,warn = FALSE,encoding = encoding1)
  return(fic02)
}

func_tcontenido_Rmd_tex = function(ficheroRmd_prin) {
  #fic02 = readLines(ficheroRmd_prin,warn = FALSE,encoding = "UTF-8")
  fic02 = func_readLines_tex(ficheroRmd_prin)
  lrmd = stringr::str_which(fic02,"^\\\\input\\{([:graph:]*)\\}")
  crmd = stringr::str_extract(fic02[lrmd],"\\{[:graph:]*\\.(tex|Rnw)\\}")
  crmd2 = stringr::str_replace_all(crmd,fixed("{"),"")
  crmd2 = stringr::str_replace_all(crmd2,fixed("}"),"")

  lrmda = stringr::str_which(fic02,"^\\\\include\\{([:graph:]*)\\}")
  crmda = stringr::str_extract(fic02[lrmda],"\\{[:graph:]*\\.(tex|Rnw)\\}")
  crmd2a = stringr::str_replace_all(crmda,fixed("{"),"")
  crmd2a = stringr::str_replace_all(crmd2a,fixed("}"),"")

  crmd2 = c(crmd2,crmd2a)

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

  crmd2 = c(basename(ficheroRmd_prin),crmd2)

  lista_res = vector("list",length(crmd2))
  for (i in 1:length(crmd2)) {
    lista_parcial = list()
    ficRmd = paste0(dirname(ficheroRmd_prin),"/",crmd2[i])
    #fic01 = readLines(ficRmd,warn = FALSE,encoding = "UTF-8")
    fic01 = func_readLines_tex(ficRmd)
    lt1 = str_which(fic01,"\\\\chapter\\{([:graph:]*)")
    lt2 = str_which(fic01,"\\\\section\\{([:graph:]*)")
    lt3 = str_which(fic01,"\\\\subsection\\{([:graph:]*)")
    lt4 = str_which(fic01,"\\\\subsubsection\\{([:graph:]*)")
    lt5 = str_which(fic01,"\\\\subsubsubsection\\{([:graph:]*)")
    lt = c(lt1,lt2,lt3,lt4,lt5)
    titulos_posiciones = sort(lt)
    titulos = fic01[titulos_posiciones]
    Inchunk = c(rep(FALSE,length(titulos)))
    nfic02 = paste0(dirname(ficheroRmd_prin),"/",file_ini)
    if (file.exists(nfic02)) {
      #fic02 = readLines(nfic02,warn=FALSE,encoding = "UTF-8")
      fic02 = func_readLines_tex(nfic02)
      ltg_all = vector()
      lcad_all = vector()
      lchunk_all = vector()
      for (j in 1:length(fic02)) {
        sl1 = unlist(strsplit(fic02[j],";;"))
        if (length(sl1)>2) {
          if (sl1[2]=="tex") {
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

func_tcontenido_Rmd_no_prin_tex = function(ficheroRmd) {
    i = 1
    lista_res = vector("list",1)
    lista_parcial = list()
    ficRmd = ficheroRmd
    #fic01 = readLines(ficRmd,warn = FALSE,encoding = "UTF-8")
    fic01 = func_readLines_tex(ficRmd)
    lt1 = str_which(fic01,"^\\\\chapter\\{([:graph:]*)")
    lt2 = str_which(fic01,"^\\\\section\\{([:graph:]*)")
    lt3 = str_which(fic01,"^\\\\subsection\\{([:graph:]*)")
    lt4 = str_which(fic01,"^\\\\subsubsection\\{([:graph:]*)")
    lt5 = str_which(fic01,"^\\\\subsubsubsection\\{([:graph:]*)")
    lt = c(lt1,lt2,lt3,lt4,lt5)
    titulos_posiciones = sort(lt)
    titulos = fic01[titulos_posiciones]
    Inchunk = c(rep(FALSE,length(titulos)))
    nfic02 = paste0(dirname(ficheroRmd),"/",file_ini)
    if (file.exists(nfic02)) {
      #fic02 = readLines(nfic02,warn=FALSE,encoding = "UTF-8")
      fic02 = func_readLines_tex(nfic02)
      ltg_all = vector()
      lcad_all = vector()
      lchunk_all = vector()
      for (j in 1:length(fic02)) {
        sl1 = unlist(strsplit(fic02[j],";;"))
        if (length(sl1)>2) {
          if (sl1[2]=="tex") {
            #browser()
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


func_tcontenido_Rmd_tb_tex = function(lr) {
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
        tt = bind_rows(tt,t1)
      }
    }
  }
  return(tt)
}

func_abrir_tituloficheroRmd_tex = function(tb_lr,cual,dir_trabajo) {
  fichero = paste0(dir_trabajo,"/",tb_lr$Fichero[cual])
  fila = tb_lr$PosicionFila[cual]
  rstudioapi::navigateToFile(file = fichero,
                             line = fila,
                             column = 1)
}




func_tcontenido_Rmd_todo_tex = function(nfichero_prin) {
  lr = func_tcontenido_Rmd_tex(nfichero_prin)
  if (is.null(lr)) {
    return(NULL)
  }
  tb_lr = func_tcontenido_Rmd_tb_tex(lr)
  tb_lr_limpio2 = tb_lr
  return(tb_lr_limpio2)

}


func_tcontenido_Rmd_todo_no_prin_tex = function(nfichero_prin) {

  lr = func_tcontenido_Rmd_no_prin_tex(nfichero_prin)
  tb_lr = func_tcontenido_Rmd_tb_tex(lr)
  if (is.null(tb_lr)) {
    tb_lr_limpio2 = tibble::tibble(
      Fichero = basename(nfichero_prin),
      Titulos = nfichero_prin,
      PosicionFila = 1,
      Inchunk = FALSE
    )
  } else {
    tb_lr$Fichero = basename(tb_lr$Fichero)
    tb_lr_limpio2 = tb_lr
  }
  return(tb_lr_limpio2)

}



