#' @importFrom magrittr %>%
#' @importFrom methods new

# Import S3 class from xml2 package
setOldClass(c("xml_node", "xml_document"))

#' Cxl class
#'
#' \code{Cxl} Creates a Cxl class from a CXL document
#'
#' @field doc xml_document. Class from xml2 package.
#' @field width numeric.
#' @field height numeric.
#'
#' @return Cxl object
#' @export
Cxl <-
  setRefClass("Cxl",
              fields = list(doc = "xml_document",
                            width = "numeric",
                            height = "numeric"),
              methods = list(initialize = function(cxl_data){
                              "Validate and initialize fields"
                              tryCatch({
                                cxl_doc <- xml2::read_xml(cxl_data)
                                if (xml2::xml_ns(cxl_doc)["d1"] != "http://cmap.ihmc.us/xml/cmap/") {
                                  stop("File is not defined as CXL")
                                }
                                # Helper function to set properties
                                .set_attr_ <- function(node, a) { #map attributes
                                  map_node <- xml2::xml_child(cxl_doc, node)
                                  if (xml2::xml_has_attr(map_node, a)) {
                                    return(xml2::xml_attr(map_node, a))
                                  } else {
                                    return(NULL)
                                  }
                                }
                                #Set properties
                                doc <<- cxl_doc
                                width <<- as.numeric(.set_attr_(2, "width"))
                                height <<- as.numeric(.set_attr_(2, "height"))
                              },error = function(e) {
                                print(paste("Error loading CXL:", e))
                            })
                          },
                          get_concepts = function() {
                              "Returns CXL concepts in a dataframe"
                              concepts <- .element_attr_df(2,1, doc)
                              concept_viz <- .element_attr_df(2, 4, doc)
                              concepts <- merge(concepts, concept_viz)
                          },
                          get_connections = function() {
                            "Returns CXL connections and phrases in a dataframe"
                            phrases <- .element_attr_df(2, 2, doc)
                            connect <- .element_attr_df(2, 3, doc)
                            connect_viz <- .element_attr_df(2, 6, doc)
                            phrases_viz <- .element_attr_df(2, 5, doc)

                            phrases <- merge(phrases, phrases_viz)
                            connect <- merge(connect, connect_viz)

                            connect <- dplyr::select(connect, -c("id"))
                            links <- phrases %>%
                              dplyr::left_join(connect, by=c("id" = "from-id")) %>%
                              dplyr::left_join(connect, by=c("id" = "to-id")) %>%
                              dplyr::rename("phrase_id" = id, "to_concept" = "to-id",
                                            "from_concept" = "from-id", "to_pos" = "to-pos.x", "from_pos" = "from-pos.x",
                                            "arrowhead" = "arrowhead.x") %>%
                              tibble::rowid_to_column("id") %>%
                              dplyr::select(-c("from-pos.y", "to-pos.y", "arrowhead.y")) %>%
                              # Reorder columns
                              dplyr::select(id, "phrase_id", label, "to_concept", "from_concept", dplyr::everything())
                          }
                          )
  )

