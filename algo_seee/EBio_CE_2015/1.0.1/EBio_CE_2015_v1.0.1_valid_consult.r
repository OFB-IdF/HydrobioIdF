# Type d'algorithme : EBio_CE_2015
# Auteur(s)         : Cedric MONDY
# Date              : 2020-04-01
# Version           : 1.0.1
# Interpreteur      : R version 3.5.3 (2019-03-11)
# Pre-requis        : Packages dplyr, tidyr
# Fichiers lies     : EBio_CE_2015_params_IBD.csv, EBio_CE_2015_params_IBG-DCE.csv, EBio_CE_2015_params_IBMR.csv, EBio_CE_2015_params_IPR.csv, EBio_CE_2015_params_MGCE.csv
# Commentaires 	  	:

# Copyright 2019 Cedric MONDY
# Ce programme est un logiciel libre; vous pouvez le redistribuer ou le modifier
# suivant les termes de la GNU General Public License telle que publiee par la
# Free Software Foundation; soit la version 3 de la licence, soit (a votre gre)
# toute version ulterieure.
# Ce programme est distribue dans l'espoir qu'il sera utile, mais SANS AUCUNE
# GARANTIE; sans meme la garantie tacite de QUALITE MARCHANDE ou d'ADEQUATION A
# UN BUT PARTICULIER. Consultez la GNU General Public License pour plus de
# details.
# Vous devez avoir recu une copie de la GNU General Public License en meme temps
# que ce programme; si ce n'est pas le cas, consultez
# <http://www.gnu.org/licenses>.

## VERSION ----
indic  <- "EBio_CE_2015"
vIndic <- "v1.0.1"

## CHARGEMENT DES PACKAGES ----
dependencies <- c("dplyr", "tidyr")

loadDependencies <- function(dependencies) {
  suppressAll <- function(expr) {
    suppressPackageStartupMessages(suppressWarnings(expr))
  }

  lapply(dependencies,
         function(x)
         {
           suppressAll(library(x, character.only = TRUE))
         }
  )
  invisible()
}

loadDependencies(dependencies)

## FICHIERS DE CONFIGURATION ----
typo_nationale <- read.csv2("EBio_CE_2015_params_IBD.csv",
                            stringsAsFactors = FALSE)

## DECLARATION DES FONCTIONS ----
## Fonction permettant l'import des fichiers de résultats
importResultats <- function(File) {
    textImport <- readLines(con = File)
    headerLoc <- sapply(textImport, grepl, pattern = "CODE_OPERATION") %>%
        which()
    
    out <- read.table(File,
                      header = TRUE, sep = ";", quote = "\"",
                      as.is  = TRUE, skip = headerLoc - 1
    )
    
    if (nrow(out) == 0) {
        mutate(out, ID = seq(1)[0])
    } else {
        mutate(out, ID = seq(n()) + 1)
    }
}

## Fonction alternative a ifelse, plus simple
siNon <- function(test, yes, no) {
  if (test) {
    return(yes)
  } else {
    return(no)
  }
}

# Fonction permettant d'initialiser la sortie de tests
initResult <- function() {
  list(verif  = "ok",
       sortie = tibble(colonne = "", ligne = "", message = "")[0,])
}

# Fonction permettant de tester si le tableau est un data frame
funDataFrame <- function(Table, tableau) {
  if (!"data.frame" %in% class(Table)) {
    test <- list(verif = "ko",
                 sortie = tibble(colonne = "", ligne = "",
                                 message = paste0("Le tableau ", tableau,
                                                  " n'est pas un data.frame")))
  } else {
    test <- list(verif = "ok",
                 sortie = tibble(colonne = "", ligne = "", message = "")[0,])
  }

  test
}

# Fonction permettant de changer les valeurs de sortie/verif en fonction des
# resultats d'un test
funTest <- function(test, result) {
  result$verif <- siNon(test$verif == "ko", "ko", result$verif)

  result$sortie <- siNon(test$verif == "ko",
                         bind_rows(result$sortie,
                                   test$sortie),
                         result$sortie)

  result
}

# Fonction testant l'import des fichiers
funImport <- function(Table, result, empty = FALSE) {
  test <- siNon(empty,
                is.null(Table),
                any(is.null(Table), nrow(Table) == 0))

  out <- siNon(empty,
               "Le fichier doit etre au bon format",
               "Le fichier doit etre au bon format et non vide")
  if (test) {
    test <- list(verif  = "ko",
                 sortie = tibble(colonne = NA, ligne = NA,
                                 message = out))
  } else {
    test <- initResult()
  }

  funTest(test, result)
}

# Fonction permettant de tester la presence de champs obligatoires
funColonnes <- function(Table, cols, result) {
  # recupere le nom de l'objet passe a l'argument Table
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {

    test <- which(! cols %in% colnames(Table))

    if (length(test) > 0) {
      test <-
        list(verif = "ko",
             sortie = tibble(colonne = NA, ligne = NA,
                             message = paste0("Les champs obligatoires",
                                              " suivants ne sont pas ",
                                              "presents : ",
                                              paste(cols[test],
                                                    collapse = ", "))))
    } else {
      test <- list(verif  = "ok",
                   sortie = initResult())
    }
  }

  funTest(test, result)
}

# Fonction retournant un commentaire pour un test donné
funCommentaire <- function(test, message, Table) {
  test <- test[sapply(test, length) > 0]

  if (length(test) > 0) {
    return(
      list(verif = "ko",
           sortie = lapply(1:length(test),
                           function(i) {
                             tibble(colonne =
                                      paste0("Colonne ",
                                             names(test)[i]),
                                    ligne   =
                                      paste0("Ligne ",
                                             Table$ID[test[[i]]]),
                                    message = message)
                           }) %>%
             bind_rows()
      )
    )
  } else {
    return(list(verif = "ok",
                sortie = initResult()))
  }
}

# Fonction permettant de tester la présence de doublons
funDoublon <- function(Table, result, groups = NULL, unique) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        if (is.null(groups)) {
            no_valid <- group_by_at(.tbl = Table, vars(unique)) %>% 
                summarise(.data = ., N = n()) %>% 
                filter(N > 1) 
            
            test <- mutate(Table, ID = seq(n())) %>% 
                filter_at(.vars = vars(unique), 
                          any_vars(.  %in% pull(no_valid, unique))) %>% 
                pull(ID) %>% 
                (function(x) {
                    out <- list(x)
                    names(out) <- paste(groups, collapse = " ")
                    out
                }) %>% 
                funCommentaire(test = .,
                               message = "doublon",
                               Table = Table)
            
        } else {
            no_valid <- group_by_at(Table, vars(groups)) %>% 
                summarise_at(.tbl = ., .vars = vars(unique), 
                             .funs = n_distinct) %>% 
                filter_at(.tbl = ., .vars = vars(unique), any_vars(. > 1)) %>% 
                unite_(data = ., col = "key", groups, sep = "_")
            
            test <- mutate(Table, ID = seq(n())) %>% 
                unite(col = "key", groups, sep = "_") %>% 
                filter(.data = ., key %in% no_valid$key) %>% 
                pull(ID) %>% 
                (function(x) {
                    out <- list(x)
                    names(out) <- paste(groups, collapse = " ")
                    out
                }) %>% 
                funCommentaire(test = .,
                               message = "doublon",
                               Table = Table)
            
        }
    }
    
    funTest(test, result)
}

# Fonction permettant de tester la presence de cellules vides
funVide <- function(Table, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testEmpty <- function(x) {
      return(which(is.na(x) | x == ""))
    }

    test <- lapply(select(Table, -ID), testEmpty) %>%
      funCommentaire(test    = .,
                     message = "cellule vide",
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de tester la presence d'espace
funEspace <- function(Table, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testSpace <- function(x) {
      which(grepl(" ", x))
    }

    test <- lapply(select(Table, -ID), testSpace) %>%
      funCommentaire(test    = .,
                     message = "cellule avec des caracteres 'espace'",
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de tester si les valeurs sont numeriques
funNumerique <- function(Table, tableau = NULL, result) {
  if (is.null(tableau)) tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testNumeric <- function(x) {
      as.character(x)  %>%
        (function(i) {
          suppressWarnings(as.numeric(i))
        })           %>%
        is.na()      %>%
        which()
    }

    test <- lapply(select(Table, -ID), testNumeric) %>%
      funCommentaire(test    = .,
                     message = "cellule avec valeur non numerique",
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de tester si les nombres sont dans un intervalle donne
funIntervalle <- function(Table, mini, maxi, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testInterval <- function(x, mini, maxi) {
      suppressWarnings(as.numeric(x)) %>%
        (function(x) {
          (x < mini | x > maxi) %>%
            which()
        })
    }

    test <- funNumerique(Table, tableau, initResult())       %>%
      funTest(test = lapply(select(Table, -ID),
                            testInterval, mini, maxi) %>%
                funCommentaire(test    = .,
                               message =
                                 paste0("cellule avec valeur ",
                                        "inferieure a ", mini,
                                        " ou superieure a ", maxi),
                               Table   = Table),
              result = .)
  }

  funTest(test, result)
}

# Fonction permettant de tester si les valeurs d'un champ sont plus grandes que
# celle d'un autre
funOrdre <- function(Table, lower, upper, result) {
    tableau = deparse(substitute(Table))

    test <- funDataFrame(Table, tableau)

    if (test$verif == "ok") {
        testOrder <- function(Table, lower, upper) {
            mutate_at(.tbl = Table,
                      .vars = c(lower, upper),
                      .funs = function(x){
                          suppressWarnings(as.numeric(x))
                      }) %>%
                mutate(.data = ., delta = .[[upper]] - .[[lower]]) %>%
                filter(delta < 0) %>%
                (function(df) {pull(df, ID) - 1})

        }

        funTest(test = list("NA" = testOrder(Table,
                                             lower = lower, upper = upper)) %>%
                    funCommentaire(test = .,
                                   message =
                                       paste0(lower, " plus grand que ", upper),
                                   Table = Table),
                result = result)
    }
}

# Fonction permettant de tester si les nombres sont entiers
funEntier <- function(Table, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testInteger <-   function(x) {
      suppressWarnings(as.numeric(x)) %>%
        (function(x) abs(x - round(x)) >
           .Machine$double.eps^0.5) %>%
        which()                       %>%
        return()
    }

    test <- funNumerique(Table, tableau, initResult())         %>%
      funTest(test = lapply(select(Table, -ID), testInteger) %>%
                funCommentaire(test    = .,
                               message = paste0("cellule avec valeur ",
                                                "non entiere"),
                               Table   = Table),
              result = .)

  }

  funTest(test, result)
}

# Fonction permettant de tester si les nombres sont positifs
funPositif <- function(Table, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testPositive <- function(x) {
      x <- suppressWarnings(as.numeric(x))

      which(x != abs(x))
    }

    test <- funNumerique(Table, tableau, initResult()) %>%
      funTest(test = lapply(select(Table, -ID), testPositive) %>%
                funCommentaire(test    = .,
                               message = "cellule avec valeur non positive",
                               Table   = Table),
              result = .)
  }

  funTest(test, result)
}

# Fonction permettant de tester la validite d'une periode
funPeriode <- function(Table, start, end, result) {
    tableau <- deparse(substitute(Table))
    
    test <- funDataFrame(Table, tableau)
    
    if (test$verif == "ok") {
        test <- list(which(Table[[start]] > Table[[end]]))
        names(test) <- paste(start, end, sep = " ")
        
        test <- funCommentaire(test = test,
                               message = "debut > fin",
                               Table = Table)
    }
    
    funTest(test, result)
}

# Fonction permettant de tester le format des dates
funDate <- function(Table, dateFormat, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testDate <- function(x, dateFormat) {
      suppressWarnings(as.Date(x, format = dateFormat)) %>%
        is.na()                                         %>%
        which()
    }

    test <- lapply(select(Table, -ID),
                   testDate, dateFormat = dateFormat) %>%
      funCommentaire(test    = .,
                     message = "cellule avec format de date non valide",
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de tester des codes
funCodes <- function(Table, codes, codeType, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testCodes <- function(x, codes) {
      return(which(! x %in% codes))
    }

    test <- lapply(select(Table, -ID), testCodes, codes) %>%
      funCommentaire(test    = .,
                     message = paste0("cellule avec code ",
                                      codeType, " non valide"),
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de nettoyer les sorties
funSortie <- function(result) {
  if (result$verif == "ok") {
    ""
  } else {
    sortie <- result$sortie

    msgLevels <- c("vide", "espace",
                   "numerique", "entiere", "positive",
                   "date", "code")

    messages <- tibble(message = unique(sortie$message),
                       level   = NA)

    for (i in 1:length(msgLevels)) {
      i2 <- which(grepl(pattern = msgLevels[i], x = messages$message))

      messages$level[i2] <- i
    }

    messages <- arrange(messages, level) %>%
      mutate(message = factor(message, levels = message))

    mutate(sortie,
           message = factor(message,
                            levels = messages$message)) %>%
      group_by(colonne, ligne)                          %>%
      summarise(message = message[as.numeric(message) ==
                                    min(as.numeric(message))][1])
  }
}

## Fonction permettant de verifier la validite de l'ensemble des tests
funValid <- function(resultats) {
  sapply(resultats,
         function(x) {
           x$verif == "ko"
         }) %>%
    (function(x) siNon(any(x), "ko", "ok"))
}

## Fonction permettant d'ecrire le fichier de sortie
funResult <- function(indic, vIndic, heure_debut, valid, sortie, file) {
    # determination du temps de calcul
    heure_fin <- Sys.time()
    heure_dif <- heure_fin - heure_debut
    temps_execution <- paste(round(heure_dif, 2),
                             attr(heure_dif, "units"),
                             sep = "")

    # creation du bandeau d'information
    etiquette <- paste(indic, vIndic, Sys.Date(),
                       "Temps d'execution :", temps_execution)

    # information de validation ou non des donnees d'entree
    print(valid)

    cat(paste0(etiquette, "\n"), file = file, sep = "")

    for (i in names(sortie)) {
        cat(paste0("Fichier de donnees ", i, ":\n"), file = file,
            append = TRUE)
        write.table(sortie[[i]], file = file, sep = ";",
                    col.names = FALSE, row.names = FALSE, quote = FALSE,
                    append = TRUE)
        cat("\n", file = file, append = TRUE)
    }

}

## INITIALISATION DU TRAITEMENT ----

# Ne pas afficher les messages d'avis ni d'erreur
options(warn = -1)

# Recuperation du fichier d'entree
File_station <- "EBio_CE_2015_entree_01_stations.txt"
File_ibd     <- "EBio_CE_2015_entree_02_IBD.csv"
File_ibmr    <- "EBio_CE_2015_entree_03_IBMR.csv"
File_mib     <- "EBio_CE_2015_entree_04_IBG-DCE.csv"
File_ipr     <- "EBio_CE_2015_entree_05_IPR.csv"

# Initialisation de l'heure
heure_debut <- Sys.time()

### IMPORT DES FICHIERS ----

# Import des fichiers d'entree
data_station <- read.table(File_station, header = TRUE, sep = "\t", 
                           quote = "\"", stringsAsFactors = FALSE,
                           colClasses = c(CODE_STATION   = "character",
                                          TYPO_NATIONALE = "character")) %>% 
    mutate(ID = seq(n()) + 1)

data_ibd  <- importResultats(File_ibd) 
data_ibmr <- importResultats(File_ibmr) 
data_mib  <- importResultats(File_mib) 
data_ipr  <- importResultats(File_ipr) 

## VALIDATION DES DONNEES ----

# Fichier de caracterisation de l'evaluation
resultat_station <- initResult() %>%
    funImport(
        Table  = data_station,
        empty  = FALSE,
        result = .)              %>%
    funColonnes(
        Table  = data_station,
        cols   = c("CODE_STATION", "TYPO_NATIONALE", 
                   "PERIODE_DEBUT", "PERIODE_FIN"),
        result = .)

if (resultat_station$verif == "ok") {
    resultat_station <- resultat_station %>%
        funVide(
            Table    = select(data_station,
                              ID, CODE_STATION, TYPO_NATIONALE,
                              PERIODE_DEBUT, PERIODE_FIN),
            result   = .)                %>%
        funEspace(
            Table    = select(data_station,
                              ID, CODE_STATION, TYPO_NATIONALE,
                              PERIODE_DEBUT, PERIODE_FIN),
            result   = .)                %>%
        funDoublon(
            Table = data_station,
            groups = "CODE_STATION",
            unique = "TYPO_NATIONALE",
            result = .
        )                                %>% 
        funCodes(
            Table    = select(data_station, ID, TYPO_NATIONALE),
            codes    = typo_nationale$TYPO_NATIONALE,
            codeType = "typologie",
            result   = .)                %>%
        funEntier(
            Table    = select(data_station, ID, PERIODE_DEBUT, PERIODE_FIN),
            result   = .)                %>%
        funPositif(
            Table    = select(data_station, ID, PERIODE_DEBUT, PERIODE_FIN),
            result   = .)                %>% 
        funPeriode(
            Table = select(data_station, ID, PERIODE_DEBUT, PERIODE_FIN),
            start = "PERIODE_DEBUT", 
            end = "PERIODE_FIN",
            result = .
        )
}

# Fichier de resultats de l'IBD
resultat_ibd <- initResult() %>%
    funImport(
        Table  = data_ibd,
        empty  = TRUE,
        result = .)          %>%
    funColonnes(
        Table  = data_ibd,
        cols   = c("CODE_STATION", "DATE", "CODE_PAR", "RESULTAT"),
        result = .)

if (resultat_ibd$verif == "ok") {
    resultat_ibd <- resultat_ibd %>%
        funVide(
            Table      = select(data_ibd, 
                                ID, CODE_STATION, DATE, CODE_PAR),
            result     = .)      %>%
        funEspace(
            Table      = select(data_ibd,
                                ID, CODE_STATION, DATE, CODE_PAR, RESULTAT),
            result     = .)      %>%
        funDate(
            Table      = select(data_ibd, ID, DATE),
            dateFormat = "%d/%m/%Y",
            result     = .)      %>%
        funCodes(
            Table      = select(data_ibd, ID, CODE_PAR),
            codes      = c(8060, 8059, 5856),
            codeType   = "parametre",
            result     = .)      %>%
        funPositif(
            Table      = filter(data_ibd, ! is.na(RESULTAT)) %>% 
                select(ID, RESULTAT),
            result     = .)
}

# Fichier de resultats de l'IBMR
resultat_ibmr <- initResult() %>%
    funImport(
        Table  = data_ibmr,
        empty  = TRUE,
        result = .)           %>%
    funColonnes(
        Table  = data_ibmr,
        cols   = c("CODE_STATION", "DATE", "CODE_PAR", "RESULTAT"),
        result = .)

if (resultat_ibmr$verif == "ok") {
    resultat_ibmr <- resultat_ibmr %>%
        funVide(
            Table      = select(data_ibmr, 
                                ID, CODE_STATION, DATE, CODE_PAR),
            result     = .)        %>%
        funEspace(
            Table      = select(data_ibmr,
                                ID, CODE_STATION, DATE, CODE_PAR, RESULTAT),
            result     = .)        %>%
        funDate(
            Table      = select(data_ibmr, ID, DATE),
            dateFormat = "%d/%m/%Y",
            result     = .)        %>%
        funCodes(
            Table      = select(data_ibmr, ID, CODE_PAR),
            codes      = c(7974, 2928, 8063),
            codeType   = "parametre",
            result     = .)        %>%
        funPositif(
            Table      = filter(data_ibmr, !is.na(RESULTAT)) %>% 
                select(ID, RESULTAT),
            result     = .)
}

# Fichier de resultats de l'indice invertebres
resultat_mib <- initResult() %>%
    funImport(
        Table  = data_mib,
        empty  = TRUE,
        result = .)          %>%
    funColonnes(
        Table  = data_mib,
        cols   = c("CODE_STATION", "DATE", "CODE_PAR", "RESULTAT"),
        result = .) 

if (resultat_mib$verif == "ok") {
    resultat_mib <- resultat_mib %>%
        funVide(
            Table      = select(data_mib,
                                ID, CODE_STATION, DATE, CODE_PAR),
            result     = .)      %>%
        funEspace(
            Table      = select(data_mib, 
                                ID, CODE_STATION, DATE, CODE_PAR, RESULTAT),
            result     = .)      %>%
        funDate(
            Table      = select(data_mib, ID, DATE),
            dateFormat = "%d/%m/%Y",
            result     = .)      %>%
        funCodes(
            Table      = select(data_mib, ID, CODE_PAR),
            codes      = c(8051, 6035, 6034, 5910,
                           8130, 6959, 6955, 6951),
            codeType   = "parametre",
            result     = .)      %>%
        funPositif(
            Table      = filter(data_mib, !is.na(RESULTAT)) %>% 
                select(ID, RESULTAT),
            result     = .)
}

# Fichier de resultats de l'IPR
resultat_ipr <- initResult() %>%
    funImport(
        Table  = data_ipr,
        empty  = TRUE,
        result = .)          %>%
    funColonnes(
        Table  = data_ipr,
        cols   = c("CODE_STATION", "DATE", "CODE_PAR", "LIB_PAR", "RESULTAT"),
        result = .)

if (resultat_ipr$verif == "ok") {
    resultat_ipr <- resultat_ipr %>%
        funVide(
            Table     = select(data_ipr,
                               ID, CODE_STATION, DATE, LIB_PAR),
            result     = .)      %>%
        funVide(
            Table      = filter(data_ipr, LIB_PAR != "ALT") %>%
                select(ID, CODE_PAR),
            result     = .)      %>%
        funEspace(
            Table      = select(data_ipr, 
                                ID, CODE_STATION, DATE, CODE_PAR, RESULTAT),
            result     = .)      %>%
        funDate(
            Table = select(data_ipr, ID, DATE),
            dateFormat = "%d/%m/%Y",
            result     = .)      %>%
        funCodes(
            Table      = select(data_ipr, ID, CODE_PAR),
            codes      = c(NA, 7744, 7743, 7644, 7786, 7746, 7745, 7787, 7036),
            codeType   = "parametre",
            result     = .) %>%
        funCodes(
            Table      = select(data_ipr, ID, LIB_PAR),
            codes      = c("ALT", "Score NER", "Score NEL", "Score NTE", 
                           "Score DIT", "Score DIO", "Score DII", 
                           "Score DTI", "IPR"),
            codeType   = "libelle",
            result     = .) %>%
        funPositif(
            Table      = filter(data_ipr, !is.na(RESULTAT)) %>% 
                select(ID, RESULTAT),
            result     = .)
}

# Parametre de succes/echec de la validation
valid <- funValid(resultats = list(resultat_station,
                                   resultat_ibd,
                                   resultat_ibmr,
                                   resultat_mib,
                                   resultat_ipr))

# SORTIE DU RAPPORT D'ERREUR ----
sortie <- list(`de caractérisation de l'evaluation` = funSortie(resultat_station),
               IBD              = funSortie(resultat_ibd),
               IBMR             = funSortie(resultat_ibmr),
               macroinvertebres = funSortie(resultat_mib),
               IPR              = funSortie(resultat_ipr))

outputFile <- paste0(indic, "_", vIndic, "_rapport_erreur.csv")
funResult(indic       = indic,
          vIndic      = vIndic,
          heure_debut = heure_debut,
          valid       = valid,
          sortie      = sortie,
          file        = outputFile)
