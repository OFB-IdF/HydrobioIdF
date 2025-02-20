# Type d'algorithme : EBio_CE_2018
# Auteur(s)         : Cedric MONDY
# Date              : 2023-02-23
# Version           : 1.0.1
# Interpreteur	   	: R version 3.6.3 (2020-02-29)
# Pre-requis        : Packages dplyr, tidyr
# Fichiers lies   	: EBio_CE_2018_params_I2M2.csv, EBio_CE_2018_params_IBD.csv, EBio_CE_2018_params_IBG-DCE.csv, EBio_CE_2018_params_IBMR.csv, EBio_CE_2018_params_IPR.csv, EBio_CE_2018_params_MGCE.csv
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
indic  <- "EBio_CE_2018"
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

## IMPORT DES FICHIERS DE CONFIGURATION ----
indicateurs <- data.frame(
  CODE_PAR = c(
    "5856",
    "2928",
    "5910",
    "7613",
    "6951",
    "7036"
  ),
  LIB_PAR = c(
    "IndiceBioDiat",
    "IBMR",
    "MPCE phases A+B",
    "Ind Invert Multimetrique",
    "MGCE 12 prelevements",
    "Indice Poisson Riviere"
  ),
  indic = c(
    "IBD",
    "IBMR",
    "IBG-DCE",
    "I2M2",
    "MGCE",
    "IPR"
  ),
  stringsAsFactors = FALSE
)

limites_ibd  <- read.csv2("EBio_CE_2018_params_IBD.csv",
                          stringsAsFactors = FALSE)
limites_ibmr <- read.csv2("EBio_CE_2018_params_IBMR.csv",
                          stringsAsFactors = FALSE)
limites_ibg  <- read.csv2("EBio_CE_2018_params_IBG-DCE.csv", 
                          stringsAsFactors = FALSE)
limites_i2m2 <- read.csv2("EBio_CE_2018_params_I2M2.csv",
                          stringsAsFactors = FALSE)
limites_mgce <- read.csv2("EBio_CE_2018_params_MGCE.csv",
                          stringsAsFactors = FALSE)
limites_ipr  <- read.csv2("EBio_CE_2018_params_IPR.csv",
                          stringsAsFactors = FALSE)

## DECLARATION DES FONCTIONS ----
## Fonction permettant d'importer les fichiers de resultat issus du SEEE
funImport <- function(File, stations = NULL, params = NULL) {
  textImport <- readLines(con = File)
  headerLoc <- sapply(textImport, grepl, pattern = "CODE_OPERATION") %>%
    which()
  
  df <- read.csv2(File, as.is = TRUE, 
                  quote = "\"", skip = headerLoc - 1,
                  colClasses = c(CODE_OPERATION = "character",
                                 CODE_STATION   = "character",
                                 DATE           = "character",
                                 CODE_PAR       = "character",
                                 RESULTAT       = "character"))
  
  if (!is.null(stations)) {
    df <- filter(df, CODE_STATION %in% stations)
  }
  
  if (!is.null(params)) {
    df <- filter(df, CODE_PAR %in% params | LIB_PAR == "ALT")
  }
  
  df <- mutate(df, 
               RESULTAT       = as.numeric(RESULTAT))
  
  return(df[, colnames(df) != "TYPO_NATIONALE"])
}

## Fonction permettant de faire la moyenne des indicateurs par site et par
## periode
funMoyenne <- function(valeurs, dateFormat = "%d/%m/%Y", sites) {
  sites <- mutate(sites, 
                  PERIODE_DEBUT = as.integer(as.character(PERIODE_DEBUT)),
                  PERIODE_FIN   = as.integer(as.character(PERIODE_FIN)),
                  CODE_STATION  = as.character(CODE_STATION))
  
  mutate(valeurs,
         ANNEE = as.Date(DATE,
                         format = dateFormat
         )              %>%
           format("%Y") %>%
           as.numeric()
  )                                      %>%
    left_join(sites, by = "CODE_STATION") %>%
    mutate(VALID = ANNEE >= PERIODE_DEBUT &
             ANNEE <= PERIODE_FIN)                     %>%
    group_by(
      CODE_STATION, TYPO_NATIONALE, TG_BV,
      CODE_PAR, LIB_PAR,
      PERIODE_DEBUT, PERIODE_FIN
    )                                    %>%
    summarise(RESULTAT = mean(RESULTAT[VALID])) %>%
    ungroup()                            %>%
    mutate(
      CODE_STATION   = as.character(CODE_STATION),
      TYPO_NATIONALE = as.character(TYPO_NATIONALE),
      TG_BV         = as.character(TG_BV),
      CODE_PAR       = as.integer(as.character(CODE_PAR)),
      LIB_PAR        = as.character(LIB_PAR),
      PERIODE_DEBUT  = as.integer(as.character(PERIODE_DEBUT)),
      PERIODE_FIN    = as.integer(as.character(PERIODE_FIN))
    )
}

## Fonction permettant de calculer des EQR si necessaire
funEQR <- function(valeurs, references) {
  if (nrow(valeurs) > 0) {
    columns <- colnames(valeurs)
    
    # IBD
    if (unique(valeurs$CODE_PAR) %in% 5856) {
      valeurs <- left_join(x = valeurs,
                           y = references,
                           by = c("TYPO_NATIONALE", "TG_BV")) %>%
        mutate(EQR = (RESULTAT - MINIMUM) / (REFERENCE - MINIMUM))
    }
    
    # IBMR
    if (unique(valeurs$CODE_PAR) %in% 2928) {
      valeurs <- mutate(valeurs,
                        TYPO_NATIONALE = gsub(pattern = "TTGA\\d",
                                              replacement = "TTGA",
                                              x = TYPO_NATIONALE)) %>% 
        left_join(x = .,
                  y = references,
                  by = "TYPO_NATIONALE") %>%
        mutate(EQR = RESULTAT / REFERENCE)
    }
    
    # IBG-DCE et MGCE
    if (unique(valeurs$CODE_PAR %in% c(5910, 6951))) {
      valeurs <- mutate(valeurs,
                        TYPO_NATIONALE = gsub(pattern = "TTGA\\d",
                                              replacement = "TTGA")) %>% 
        left_join(x = .,
                  y = references,
                  by = "TYPO_NATIONALE") %>%
        mutate(EQR = (RESULTAT - 1) / (REFERENCE - 1))
    }
    
    valeurs[, c(columns, "EQR")]
    
  } else {
    mutate(valeurs, EQR = NA)
  }
}

## Fonction permettant d'obtenir la classe de qualite d'un indicateur
funClass <- function(valeurs, limites) {
  columns <- colnames(valeurs)
  
  allocate_class <- function(val, lims, type = 1) {
    if (type == 1) {
      cond <- round(val, 8) >= 
        round(lims[, c("TRES_BON", "BON", "MOYEN",
                       "MEDIOCRE", "MAUVAIS")], 8)
    }
    
    if (type == 2) {
      cond <- round(val, 8) <= 
        round(lims[, c("TRES_BON", "BON", "MOYEN",
                       "MEDIOCRE", "MAUVAIS")], 8)
    }
    
    cond %>%
      apply(
        MARGIN = 1,
        (function(x) {
          ind <- (1:5)[x == TRUE]
          
          if (all(is.na(ind))) {
            return(NA)
          } else {
            return(c(
              "TRES_BON", "BON", "MOYEN",
              "MEDIOCRE", "MAUVAIS"
            )[min(ind)])
          }
        })
      )
  }
  
  if (nrow(valeurs) == 0) {
    valeurs <- mutate(valeurs,
                      CLASSE = as.character(RESULTAT)
    )
  } else {
    
    if (unique(valeurs$CODE_PAR) %in% c(5856, 2928, 5910, 6951, 7613)) {
      if("TG_BV" %in% colnames(limites)) {
        lims <- left_join(valeurs, limites, 
                          by = c("TYPO_NATIONALE", "TG_BV"))
      } else {
        lims <- left_join(valeurs, limites, by = "TYPO_NATIONALE")
        
      }
      
      valeurs <-
        mutate(valeurs,
               CLASSE = allocate_class(
                 val  = EQR,
                 lims = lims,
                 type = 1
               )
        )
    }
    
    if (unique(valeurs$CODE_PAR) %in% c(7036)) {
      valeurs <- mutate(valeurs,
                        ALT2 = ifelse(ALT < 500,
                                      "BASSE_ALTITUDE",
                                      "HAUTE_ALTITUDE"
                        ),
                        RESULTAT = RESULTAT %>%
                          as.character()    %>%
                          as.numeric()
      )
      
      lims <- left_join(valeurs, limites,
                        by = "TYPO_NATIONALE"
      ) %>%
        mutate(BON = ifelse(!is.na(BON),
                            BON,
                            ifelse(ALT2 == "BASSE_ALTITUDE",
                                   BASSE_ALTITUDE,
                                   HAUTE_ALTITUDE
                            )
        ))
      
      valeurs <-
        mutate(valeurs,
               CLASSE = allocate_class(
                 val  = RESULTAT,
                 lims = lims,
                 type = 2
               )
        )
    }
  }
  
  return(valeurs[, c(columns, "CLASSE")])
}

## Fonction permettant d'etablr la classe de qualite biologique
funAggregation <- function(classes) {
  classLevels <- structure(1:5,
                           .Names = c(
                             "MAUVAIS", "MEDIOCRE",
                             "MOYEN", "BON", "TRES_BON"
                           )
  )
  
  names(classLevels)[min(classLevels[as.character(classes)], na.rm = TRUE)]
}

## Fonction permettant d'isoler les codes stations de certains types de cours
## d'eau
funStation <- function(sites, types) {
  filter(sites, TYPO_NATIONALE %in% types) %>%
    (function(df) as.character(df$CODE_STATION))
}

## Fonction permettant de faire les arrondis a l'inferieur si 0 a 4 et au superieur si 5 a 9
funArrondi <- function (x, digits = 0) {
  .local <- function(x, digits) {
    x <- x * (10^digits)
    ifelse(abs(x%%1 - 0.5) < .Machine$double.eps^0.5,
           ceiling(x)/(10^digits),
           round(x)/(10^digits))
  }
  
  if (is.data.frame(x))
    return(data.frame(lapply(x, .local, digits)))
  .local(x, digits)
}

## Fonction initialisant le fichier de sortie
funSortie <- function(data_entree, paramsOut, ...) {
  select(data_entree, ...) %>%
    distinct()             %>%
    (function(df) {
      df[rep(1:nrow(df), each = nrow(paramsOut)),] %>%
        as.tbl()
    })                     %>%
    mutate(CODE_PAR = rep(paramsOut$CODE_PAR,
                          n() / nrow(paramsOut)),
           LIB_PAR  = rep(paramsOut$LIB_PAR,
                          n() / nrow(paramsOut)))
}

## Fonction permettant d'ecrire le fichier de sortie
funResult 		<- function(indic, vIndic, heure_debut,
                        data_sortie, data_complementaire, complementaire,
                        file, file_complementaire)
{
  # determination du temps de calcul
  heure_fin       <- Sys.time()
  heure_dif       <- heure_fin - heure_debut
  temps_execution <- paste0(round(heure_dif, 2),
                            attr(heure_dif, "units"))
  
  # creation du bandeau d'information
  etiquette <- paste(indic, vIndic, Sys.Date(),
                     "Temps d'execution :", temps_execution,
                     sep = ";")
  
  # sortie du bandeau d'information
  cat(paste0(etiquette, "\n"), file = file, sep = "")
  
  # sortie du fichier de sortie
  write.table(data_sortie, row.names = FALSE, quote = FALSE, sep = ";",
              file = file, append = TRUE)
  
  # Sortie complementaire
  if(complementaire)
  {
    if (file == "") {
      print("Fichier")
    }
    
    cat(paste0(etiquette, "\n"), file = file_complementaire, sep = "")
    write.table(data_complementaire, row.names = FALSE, quote = FALSE,
                sep = ";", file = file_complementaire, append = TRUE)
  }
  
}# fin de la fonction funResult

## INITIALISATION DU TRAITEMENT ----
# Ne pas afficher les messages d'avis ni d'erreur
options(warn = -1)

# Recuperation du fichier d'entree
File_station <- "EBio_CE_2018_entree_01_stations.txt"
File_ibd     <- "EBio_CE_2018_entree_02_IBD.csv"
File_ibmr    <- "EBio_CE_2018_entree_03_IBMR.csv"
File_mib     <- "EBio_CE_2018_entree_04_I2M2.csv"
File_ipr     <- "EBio_CE_2018_entree_05_IPR.csv"
complementaire <- FALSE

# Initialisation de l'heure
heure_debut <- Sys.time()

##  IMPORT DES FICHIERS ----
# Import des fichiers d'entree
data_station <- read.table(File_station, header = TRUE, sep = "\t",
                           quote = "\"", stringsAsFactors = FALSE,
                           colClasses = c(CODE_STATION   = "character",
                                          TYPO_NATIONALE = "character")) %>% 
  mutate(TG_BV = if_else(condition = (TG_BV == "" | is.na(TG_BV)) & 
                           !grepl(pattern = "TG", x = TYPO_NATIONALE),
                         true  = "NON",
                         false = TG_BV))

data_ibd  <- funImport(File     = File_ibd,
                       stations = data_station$CODE_STATION,
                       params   = indicateurs$CODE_PAR)

data_ibmr  <- funImport(File     = File_ibmr,
                        stations = data_station$CODE_STATION,
                        params   = indicateurs$CODE_PAR)

data_mib  <- funImport(File     = File_mib,
                       stations = data_station$CODE_STATION,
                       params   = indicateurs$CODE_PAR)

data_ipr  <- funImport(File     = File_ipr,
                       stations = data_station$CODE_STATION,
                       params   = indicateurs$CODE_PAR) 

alt <- filter(data_ipr, LIB_PAR %in% "ALT") %>%
  mutate(ALT = RESULTAT)                  %>%
  group_by(CODE_STATION)                  %>%
  summarise(ALT = unique(ALT))

data_ipr <- filter(data_ipr, CODE_PAR %in% 7036)

## INITIALISATION DU FICHIER DE SORTIE ----
paramsOut <- data.frame(
  CODE_PAR = c(
    indicateurs$CODE_PAR,
    "8353"
  ),
  LIB_PAR = c(
    indicateurs$LIB_PAR,
    "Etat Biologique"
  ),
  stringsAsFactors = FALSE
)

data_sortie <- funSortie(data_entree = data_station, paramsOut = paramsOut,
                         CODE_STATION, TYPO_NATIONALE, TG_BV, 
                         PERIODE_DEBUT, PERIODE_FIN) %>% 
  mutate(CODE_STATION   = as.character(CODE_STATION),
         PERIODE_DEBUT  = as.integer(PERIODE_DEBUT),
         PERIODE_FIN    = as.integer(PERIODE_FIN),
         TYPO_NATIONALE = as.character(TYPO_NATIONALE),
         TG_BV          = as.character(TG_BV),
         CODE_PAR       = as.character(CODE_PAR),
         LIB_PAR        = as.character(LIB_PAR))

## CALCUL DE L'INDICE ----
# Moyenne des valeurs d'indice sur la periode renseignee
table_ibd <- funMoyenne(valeurs    = data_ibd,
                        dateFormat = "%d/%m/%Y",
                        sites      = data_station)

table_ibmr <- funMoyenne(valeurs    = data_ibmr,
                         dateFormat = "%d/%m/%Y",
                         sites      = data_station)

table_mib <- funMoyenne(valeurs    = data_mib,
                        dateFormat = "%d/%m/%Y",
                        sites      = data_station)

table_ipr <- funMoyenne(valeurs    = data_ipr,
                        dateFormat = "%d/%m/%Y",
                        sites      = data_station) 

# Calcul des EQR

eqr_ibd <- funEQR(valeurs    = table_ibd,
                  references = limites_ibd)

eqr_ibmr <- funEQR(valeurs    = table_ibmr,
                   references = limites_ibmr)

if (nrow(table_mib) > 0) {
  if (unique(table_mib$CODE_PAR) == 5910) limites_mib <- limites_ibg
  if (unique(table_mib$CODE_PAR) == 6951) limites_mib <- limites_mgce
  if (unique(table_mib$CODE_PAR) == 7613) limites_mib <- limites_i2m2
  
  if (unique(table_mib$CODE_PAR) %in% c(5910, 6951)) {
    eqr_mib <- funEQR(valeurs    = table_mib,
                      references = limites_mib)
  }
  
  if (unique(table_mib$CODE_PAR) %in% c(7613)) {
    eqr_mib <- mutate(table_mib,
                      EQR = RESULTAT)
  }
  
} else {
  eqr_mib <- mutate(table_mib,
                    EQR = RESULTAT)
}

eqr_ipr <- mutate(table_ipr,
                  EQR = NA)

# Attribution des classes de qualite
eqr_ibd <- funClass(valeurs = eqr_ibd,
                    limites = limites_ibd)

eqr_ibmr <- funClass(valeurs = eqr_ibmr,
                     limites = limites_ibmr)

eqr_mib <- funClass(valeurs = eqr_mib,
                    limites = limites_mib)

eqr_ipr <- funClass(valeurs = left_join(eqr_ipr, alt, by = "CODE_STATION"),
                    limites = limites_ipr)

eqr <- bind_rows(eqr_ibd, eqr_ibmr, eqr_mib, eqr_ipr) %>%
  mutate(LIB_PAR = indicateurs$LIB_PAR[match(CODE_PAR, 
                                             indicateurs$CODE_PAR)] %>%
           as.character())                        %>%
  select(CODE_STATION, PERIODE_DEBUT, PERIODE_FIN,
         CODE_PAR, LIB_PAR, RESULTAT, EQR, CLASSE)  %>%
  mutate(CODE_PAR = as.character(CODE_PAR))

ce <- group_by(eqr, CODE_STATION, PERIODE_DEBUT, PERIODE_FIN) %>%
  summarise(
    CODE_PAR = "8353",
    LIB_PAR  = "Etat Biologique",
    RESULTAT = NA,
    EQR      = NA,
    CLASSE   = funAggregation(CLASSE)
  )                                        %>%
  ungroup()                                %>%
  mutate(
    RESULTAT = as.numeric(RESULTAT),
    EQR      = as.numeric(EQR)
  )

sortie <- bind_rows(eqr, ce)     %>%
  arrange(CODE_STATION, PERIODE_DEBUT, PERIODE_FIN) %>%
  mutate(
    CODE_PAR   = as.character(CODE_PAR),
    CLASSE_NUM = as.numeric(factor(CLASSE,
                                   levels   = c(
                                     "TRES_BON", "BON", "MOYEN",
                                     "MEDIOCRE", "MAUVAIS"
                                   )
    ))
  )

## RESULTATS COMPLEMENTAIRES ----
if (complementaire) {
  data_complementaire <- NULL
} else {
  data_complementaire <- NULL
}

## SORTIE DES RESULTATS ----

data_sortie <- left_join(x  = data_sortie,
                         y  = sortie,
                         by = c("CODE_STATION",
                                "PERIODE_DEBUT", "PERIODE_FIN",
                                "CODE_PAR", "LIB_PAR")) %>% 
  mutate(CODE_PAR = factor(CODE_PAR,
                           levels = c(indicateurs$CODE_PAR, "8353"))) %>% 
  arrange(CODE_STATION, PERIODE_DEBUT, PERIODE_FIN, CODE_PAR) %>% 
  mutate(CODE_PAR = as.character(CODE_PAR))

## COMMENTAIRES ----

## Donnees absentes
data_sortie <- 
  mutate(data_sortie,
         COMMENTAIRES = if_else(is.na(RESULTAT) & CODE_PAR != 8353, 
                                "Aucunes donnees disponibles sur la periode d'evaluation definie",
                                NA_character_))

## IBD
selSites <- funStation(
  sites = data_station,
  types = filter(limites_ibd, is.na(TRES_BON))$TYPO_NATIONALE)

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(CODE_PAR %in% "5856" &
                                              CODE_STATION %in% selSites &
                                              !is.na(RESULTAT) &
                                              is.na(COMMENTAIRES),
                                            "Absence de reference pour ce type de cours d'eau",
                                            COMMENTAIRES))
selSites <- NULL

## IBMR
selSites <- funStation(
  sites = data_station,
  types = c("TTGA", "TTGL"))

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(CODE_PAR %in% "2928" &
                                              CODE_STATION %in% selSites &
                                              !is.na(RESULTAT) &
                                              is.na(COMMENTAIRES),
                                            "Valeur de reference approximative provisoire",
                                            COMMENTAIRES))

selSites <- NULL

selSites <- funStation(
  sites = data_station,
  types = filter(limites_ibmr, is.na(TRES_BON))$TYPO_NATIONALE)

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(CODE_PAR %in% "2928" &
                                              CODE_STATION %in% selSites &
                                              !is.na(RESULTAT) &
                                              is.na(COMMENTAIRES),
                                            "Absence de reference pour ce type de cours d'eau",
                                            COMMENTAIRES))

selSites <- NULL

## IPR
selSites <- funStation(
  sites = data_station,
  types = c(
    "TP21", "TP3", "TG17/3-21", "TG15",
    "TG10-15/4", "TG5/2", "TTGA", "GM5/2",
    "G2", "MP2", "TP2", "GMP7", "TG6-7/2",
    "GM6/2-7",
    "TG6/1-8", "TP11", "TG11/3-21", "TG14/3-11",
    "TP14", "TG14/1", "TP13", "TP1", "TP12-B",
    "TTGL", "TG9", "TP9", "TG9/21", "TG22/10"))

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(CODE_PAR %in% "7036" &
                                              CODE_STATION %in% selSites &
                                              !is.na(RESULTAT) &
                                              is.na(COMMENTAIRES),
                                            "A valider a dire d'expert si se justifie au regard des limites d'application",
                                            COMMENTAIRES))

selSites <- NULL

selSites <- funStation(
  sites = data_station,
  types = c("G16", "M16-A", "M16-B", "PTP16-A", "PTP16-B"))

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(CODE_PAR %in% "7036" &
                                              CODE_STATION %in% selSites &
                                              !is.na(RESULTAT) &
                                              is.na(COMMENTAIRES),
                                            "Hors domaine d'application",
                                            COMMENTAIRES))

selSites <- NULL

## IBG-DCE
selSites <- funStation(
  sites = data_station,
  types = filter(limites_ibg, is.na(TRES_BON))$TYPO_NATIONALE)

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(CODE_PAR %in% "5910" &
                                              CODE_STATION %in% selSites &
                                              !is.na(RESULTAT) &
                                              is.na(COMMENTAIRES),
                                            "Absence de reference pour ce type de cours d'eau",
                                            COMMENTAIRES))

selSites <- NULL

## MGCE
selSites <- funStation(
  sites = data_station,
  types = filter(limites_mgce, is.na(TRES_BON))$TYPO_NATIONALE)

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(CODE_PAR %in% "6951" &
                                              CODE_STATION %in% selSites &
                                              !is.na(RESULTAT) &
                                              is.na(COMMENTAIRES),
                                            "Absence de reference pour ce type de cours d'eau",
                                            COMMENTAIRES))

## I2M2
selSites <- funStation(
  sites = data_station,
  types = filter(limites_i2m2, is.na(TRES_BON))$TYPO_NATIONALE
)

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(CODE_PAR %in% "7613" &
                                              CODE_STATION %in% selSites &
                                              !is.na(RESULTAT) &
                                              is.na(COMMENTAIRES),
                                            "Absence de reference pour ce type de cours d'eau",
                                            COMMENTAIRES
                      )
)

selSites <- NULL

data_sortie <- mutate(data_sortie,
                      COMMENTAIRES = ifelse(is.na(COMMENTAIRES),
                                            "",
                                            COMMENTAIRES))


fichierResultat               <- paste0(indic, "_", vIndic, "_resultats.csv")
fichierResultatComplementaire <- paste0(indic, "_", vIndic,
                                        "_resultats_complementaires.csv")
funResult(indic               = indic,
          vIndic              = vIndic,
          heure_debut         = heure_debut,
          data_sortie         = data_sortie,
          data_complementaire = data_complementaire,
          complementaire      = complementaire,
          file                = fichierResultat,
          file_complementaire = fichierResultatComplementaire)
