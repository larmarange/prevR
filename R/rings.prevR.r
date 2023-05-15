#' @exportMethod rings
setGeneric(
  "rings",
  function(object, N = seq(100, 500, 50), R = Inf, progression = TRUE) {
    standardGeneric("rings")
  }
)

#' Calculation of rings of equal number of observation and/or equal radius.
#'
#' For each cluster, this function determines a ring of equal number of
#' observations and/or equal radius and calculates several indicators from
#' observations located inside that ring.
#'
#' @param object object of class [prevR-class].
#' @param N minimum number of observations.
#' @param R maximum rings radius (in kilometers if coordinates in decimal
#'   degrees, in the unit of the projection otherwise).
#' @param progression show a progress bar?
#'
#' @details For each row of the data frame `clusters` of `object`, `rings()`
#' determines a ring, centered on the cluster. It could be:
#'
#'   - rings of equal number of observations if `N` is finite and `R = Inf`;
#'   - rings of equal radius if `N = Inf` and `R` is finite;
#'   - a combination of both (see below) if `N` and `R` are both finite.
#'
#' For *rings of equal number of observations, `rings()` selects the smallest
#' ring containing at least `N` valid observations.
#'
#' For *rings of equal radius*, `rings()` selects all clusters located at a
#' lower distance than `R` from the central cluster.
#'
#' For *combination of both*, `rings()` calculates first the ring with the
#' minimum number of observations and test if its radius is lower than `R` or
#' not. If so, the ring is kept, otherwise the ring of maximum radius is
#' calculated.
#'
#' Different series of rings could be simultaneously calculated by providing
#' different values for `N` and `R`. `rings()` will calculate rings
#' corresponding to each couple (`N`,`R`).
#'
#' @return Return `object` with the slot `rings` completed for each couple
#' (N,R).
#'
#' Each entry is composed of 3 elements: `N`, minimum number of observations per
#' ring; `R`, maximum radius of rings and `estimates`, a data frame with the
#' following variables:
#'
#' - "id" cluster ID.
#' - "r.pos" number of positive cases inside the ring.
#' - "r.n" number of valid observations inside the ring.
#' - "r.prev" observed prevalence (in %) inside the ring (r.pos/r.n).
#' - "r.radius" ring radius (in kilometers if coordinates in decimal degrees,
#'   in the unit of the projection otherwise).
#' - "r.clusters" number of clusters located inside the ring.
#' - "r.wpos" (optional) sum of weights of positive cases inside the ring.
#' - "r.wn" (optional) sum of weights of valid observations inside the ring.
#' - "r.wprev" (optional) weighted observed prevalence (in %) inside the ring
#'   (r.wpos/r.wn).
#'
#' Note: the list `rings` is named, the name of each element is
#' N\emph{N_value}.R\emph{R_value}, for example \emph{N300.RInf}.
#'
#' Note 2: \emph{r.wpos}, \emph{r.wn} and \emph{r.wprev} are calculated only if
#' the slot `clusters` of `object` contains weighted data.
#'
#' @seealso [prevR-class].
#'
#' @references
#' Larmarange Joseph, Vallo Roselyne, Yaro Seydou, Msellati Philippe and Meda
#' Nicolas (2011) "Methods for mapping regional trends of HIV prevalence from
#' Demographic and Health Surveys (DHS)",
#' \emph{Cybergeo : European Journal of Geography}, no 558,
#' \url{https://journals.openedition.org/cybergeo/24606},
#' DOI: 10.4000/cybergeo.24606.
#'
#' Larmarange Joseph (2007)
#' \emph{Prévalences du VIH en Afrique : validité d'une mesure},
#' PhD thesis in demography, directed by Benoît Ferry, université Paris
#' Descartes, \url{https://theses.hal.science/tel-00320283}.
#'
#' @examples
#' \dontrun{
#' print(fdhs)
#' dhs <- rings(fdhs, N = c(100, 200, 300, 400, 500))
#' print(dhs)
#' }
#' @aliases rings rings-methods rings,prevR-method
#' @keywords math spatial
#' @importFrom fields rdist rdist.earth

setMethod(
  "rings", "prevR",
  function(object, N = seq(100, 500, 50), R = Inf, progression = TRUE) {
    # On teste la coherence des donnees d'entree N et R
    # Pour plus de precision regardez la fonction .isInputOk.prevR
    .isInputOk.prevR(N = N, R = R)
    clusters <- slot(object, "clusters")
    coord.clust <- clusters[, c("x", "y")]

    projCRS <- slot(object, "proj")
    proj <- format(projCRS)
    # Si les donnees georeferencees sont exprimees en longitude latitude
    # les distances entre clusters sont calculees en Km
    # par la fonction rdist.earth du package fields
    # Autrement les distances sont calculees par la fonction rdist (fields)
    # en unite des donnees de depart
    if (regexpr("longlat", proj) == -1 && regexpr("latlong", proj) == -1) {
      distances <- fields::rdist(coord.clust)
    } else {
      distances <- fields::rdist.earth(coord.clust, miles = FALSE)
    }
    rings <- slot(object, "rings")
    # On cree les couple N-R a partir desquels seront calclules les rayons
    # des cercles et par consequent l'effectif et la prevalence
    # dans chaque cercle
    couples <- cbind(N = N, R = R)
    # Barre de progression
    if (progression) {
      message("Progress of calculations:", domain = "R-prevR")
      barre <- txtProgressBar(
        min = 0,
        max = nrow(couples) * nrow(clusters),
        initial = 0,
        style = 3
      )
    }
    # boucle sur les couples  N R
    for (ic in seq_len(nrow(couples))) {
      one.N <- couples[ic, "N"]
      one.R <- couples[ic, "R"]

      result <- data.frame()
      # boucle sur les clusters
      for (i in seq_len(nrow(clusters))) {
        one.clust <- clusters[i, ]
        temp <- clusters
        temp$dist <- distances[i, ]
        temp <- temp[order(temp$dist), ]

        temp$cum.n <- cumsum(temp[["n"]])
        if (length(temp[temp$cum.n >= one.N, ]$cum.n) == 0) {
          maxi <- Inf
        } else {
          maxi <- min(temp[temp$cum.n >= one.N, ]$cum.n)
        }
        temp <- temp[temp$cum.n <= maxi, ]
        #
        # A ce niveau temp contient un sous dataframe de clusters qui est tel
        # que les clusters (c'est a dire les lignes de temp) sont classes en
        # fonction de la distance au cluster d'origine (one.clust), du plus pres
        # au plus loin.
        # On ne garde que les clusters qui sont tels que l'effectif des clusters
        # dans le cercle est de l'ordre de N sous la contrainte que le rayon
        # doit etre inferieur a R.

        temp2 <- temp[temp$dist <= one.R, ]
        one.result <- data.frame(id = one.clust[, "id"])
        # r.pos : le nombre de cas positifs dans le cercle
        # r.n : l'effectif du cercle
        # r.prev : la prevalence du cercle (100*r.pos/r.n)
        # r.radius :  le rayon du cercle
        # r.clusters : le nombre de clusters dans le cercle
        one.result$r.pos <- sum(temp2[["pos"]], na.rm = TRUE)
        one.result$r.n <- sum(temp2[["n"]], na.rm = TRUE)
        one.result$r.prev <- 100 * one.result$r.pos / one.result$r.n
        if (one.R != Inf && one.N == Inf) {
          one.result$r.radius <- R
        } else {
          one.result$r.radius <- max(temp2$dist)
        }
        one.result$r.clusters <- length(temp2$dist)

        # Si des donnees ponderes sont presentes on calule
        # r.wpos : le nombre pondere de cas positifs dans le cercle
        # r.wn : l'effectif pondere du cercle
        # r.wprev : la prevalence ponderee du cercle (100*r.wpos/r.wn)
        wn <- temp2[["wn"]]
        if (!is.null(wn)) {
          one.result$r.wpos <- sum(temp2[["wpos"]])
          one.result$r.wn <- sum(temp2[["wn"]])
          one.result$r.wprev <- 100 * one.result$r.wpos / one.result$r.wn
        }
        result <- rbind(result, one.result)

        if (progression) {
          barre.cur <- (ic - 1) * nrow(clusters) + i
          setTxtProgressBar(barre, value = barre.cur)
        }
      }
      rings[[paste("N", one.N, ".R", one.R, sep = "")]] <-
        list(N = one.N, R = one.R, estimates = result)
    }

    if (progression) {
      close(barre)
    }
    slot(object, "rings") <- rings
    return(object)
  }
)
