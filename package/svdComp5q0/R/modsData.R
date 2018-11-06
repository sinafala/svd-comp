#' SVD-Comp Models Data Set - 'mods'.
#'
#' An R object containing a hierarchy of lists that contain SVD-derived components,
#'     estimated model coefficients, and other parameter
#'     values necessary to predict new 1qx values using the SVD Component
#'     mortality model indexed by child/child-adult mortality implemented by
#'     the 'svdComp5q0()' function.  The model objects
#'     have been 'cleaned' to remove large collections
#'     of data that are not necessary to perform predictions - this dramatically
#'     reduces their size.
#'
#' @format An R list object with members:
#' \describe{
#'   \item{\strong{Female}:}{
#'       \describe{
#'         \item{\strong{comps}: 4 raw SVD-derived components}{}
#'         \item{\strong{comps.sm}: 4 smoothed SVD-derived components}{}
#'         \item{\strong{aml}: lm() model object for adult mortality model}{}
#'         \item{\strong{v1}: lm() model object for v1}{}
#'         \item{\strong{v2}: lm() model object for v2}{}
#'         \item{\strong{v3}: lm() model object for v3}{}
#'         \item{\strong{v4}: lm() model object for v4}{}
#'         \item{\strong{offset}: offset used when calculating SVD}{}
#'         \item{\strong{q0}: lm() model object for mortality at age 0}{}
#'         \item{\strong{rownames}: row labels for the predicted values}{}
#'       }
#'     }
#'   \item{\strong{Male}:}{
#'       \describe{
#'         \item{\strong{comps}: 4 raw SVD-derived components}{}
#'         \item{\strong{comps.sm}: 4 smoothed SVD-derived components}{}
#'         \item{\strong{aml}: lm() model object for adult mortality model}{}
#'         \item{\strong{v1}: lm() model object for v1}{}
#'         \item{\strong{v2}: lm() model object for v2}{}
#'         \item{\strong{v3}: lm() model object for v3}{}
#'         \item{\strong{v4}: lm() model object for v4}{}
#'         \item{\strong{offset}: offset used when calculating SVD}{}
#'         \item{\strong{q0}: lm() model object for mortality at age 0}{}
#'         \item{\strong{rownames}: row labels for the predicted values}{}
#'       }
#'     }
#' }
#' @source See model development in \url{https://arxiv.org/abs/1612.01408}
"mods"

# load the model data: svd components and regression coefficients
load("./data/mods.RData")



