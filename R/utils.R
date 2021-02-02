#Utility functions

#' Get XML attributes
#'
#' This helper function extracts xml elements specific to CXL docs. It takes two
#' child element arguments, as is needed for CXL document traversing
#'
#' @param c1 numeric: xml2 child element
#' @param c2 numeric: xml2 child element
#' @param dat xml document
#'
#' @return data frame of xml attributes
.element_attr_df <- function(c1, c2, dat) {
  dat %>%
    xml2::xml_child(c1) %>%
    xml2::xml_child(c2) %>%
    xml2::xml_contents() %>%
    purrr::map(xml2::xml_attrs) %>%
    purrr::map_dfr(as.list)
}

#' Checks if CXL object
#'
#' Determines whether the object is of class "Cxl"
#'
#' @param obj Object
#'
#' @return Boolean
.is_cxl_object <- function(obj) {
  if (class(obj) == "Cxl") TRUE else FALSE
}
