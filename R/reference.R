#' Get reference data frame
#' 
#' @param antigen The antigen to get the reference data frame for
#' @param epitope The epitope to get the reference data frame for
#' @param org The organism to get the reference data frame for
#' @param binding Whether the BCR is binding to the antigen
#' @return A data frame with the reference data
#' @import dplyr magrittr stringr data.table
#' @export

get_reference <- function(antigen, epitope, org, binding = TRUE){

    # check antigen is valid
    stopifnot(antigen %in% c("SarsCoV2"))
    stopifnot(epitope %in% c("S2"))
    stopifnot(all(org %in% c("human", "mouse")))
    stopifnot(is.logical(binding))

    # get positive reference
    binding_str <- ifelse(binding, "+", "-")
    reference <- read.csv(system.file("extdata", paste0(antigen, "_", epitope, binding_str, ".csv"), package = "ClonoMappeR"), header = T, sep = ",") %>%
        filter(ref_org %in% org)

    return(reference)
}
