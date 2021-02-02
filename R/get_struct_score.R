# Structural Scoring

#' @importFrom magrittr %>%

#' get_struct_score
#'
#' Get structural score data from CXL object
#'
#' This function generates a score for concept maps by
#' analyzing the structural elements of a CXL object. It
#' provides a structural score based on various published
#' scoring systems. Additionally, it exposes raw score
#' data.
#'
#' @param cxl CXL Object
#'
#' @return list of concept map score attributes
#' @export
get_struct_score <- function(cxl, method = "west") {

  if (!.is_cxl_object(cxl)) stop("Argument must be CXL object")

  map_size <- .get_map_size_actual(cxl)

  # Number of links (2 pts each)
  link_count <- .get_link_count(cxl)

  # Hierarchy - Count of longest top to bottom connections (5 points each)
  ## TODO: Algorithm to calculate clusters
  ## TODO: Algorithm to detect longest hierarchical cluster

  # Number of cross-map links (10 pts each)
  ## TODO: Algorithm to detect links across clusters

  # Total Score (sum of all scores)

  # Self-reference - Number of "I", "Me", or "My" concepts

  list("size" = map_size, "link_count" = link_count)

}

#' get_map_size_actual
#'
#' Returns map size in pixels based on concept positions
#'
#' Although the CXL object contains dimension properties, this
#' method returns CMAP size based on concept map location. This
#' is useful for returning a more precise map area size.
#'
#' @param cxl CXL Object
#'
#' @return vector containing width and height of cmap
.get_map_size_actual <- function(cxl) {

  if (!.is_cxl_object(cxl)) stop("Argument must be CXL object")

  cm_concepts <- cxl$get_concepts()
  cm_x <- range(cm_concepts$x)
  cm_y <- range(cm_concepts$y)

  width <- cm_x[2] - cm_x[1]
  height <- cm_y[2] - cm_y[1]

  c("width" = width, "height" = height)
}

#' get_link_count
#'
#' Returns count of all links in a concept map
#'
#' Counts all links within a concept map. Provides options
#' to count all links or link subsets.
#'
#' @param cxl CXL Object
#'
#' @return integer
.get_link_count <- function(cxl, has_arrows = TRUE) {

  if (!.is_cxl_object(cxl)) stop("Argument must be CXL object")

  cm_links <- cxl$get_connections()

  if (has_arrows) {
    cm_links <- cm_links %>%
      dplyr::filter(arrowhead != "no")
  }

  nrow(cm_links)

}

.get_concept_clusters <- function(cxl) {

  if (!.is_cxl_object(cxl)) stop("Argument must be CXL object")

  cm_links <- cxl$get_connections()

  # Data Needed
  ## Number of link chains for each concept
  ## Direction of each link
  ## Length of each link


}
