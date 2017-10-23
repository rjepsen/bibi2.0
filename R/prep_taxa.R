#'Prepare Taxonomic Counts
#'
#'@param long.df Taxonomic counts in long data format.
#'@return A data frame.
#'@export

prep_taxa <- function(long.df) {
  final.df <- long.df %>%
  filter(
    phylum %in% c("annelida", "arthropoda", "mollusca", "platyhelminthes"),
    subphylum %in% c("clitellata", "crustacea", "hexapoda", "rhabditophora") | is.na(subphylum),
    !class %in% c("branchiopoda", "maxillopoda", "ostracoda"),
    !order %in% c("hymenoptera"),
    !family %in% c("gerridae", "hebridae", "veliidae", "hydrometridae",
                   "saldidae"),
    !genus %in% c("stenus")
  )

  class.spp <- c("class", "subclass", "order", "suborder",
                 "family", "subfamily", "tribe", "genus", "species")
  final.df[final.df$class %in% "bivalvia", class.spp] <- "bivalvia"
  final.df[final.df$class %in% "gastropoda", class.spp] <- "gastropoda"
  final.df[final.df$class %in% "oligochaeta", class.spp] <- "oligochaeta"
  final.df[final.df$class %in% "trepaxonemata", class.spp] <- "trepaxonemata"
  #============================================================================
  # These taxa were not consitently identified to the same taxonomic rank
  # by the agencies that contributed data. Therefore, the samples were rolled
  # up to the lowest common denominator.
  # These columns will be influenced by the common denominator taxa.
  order.spp <- c("order", "suborder", "family", "subfamily",
                 "tribe", "genus", "species")
  final.df[final.df$order %in% "collembola", order.spp] <- "collembola"
  final.df[final.df$order %in% "lepidoptera", order.spp] <- "lepidoptera"
  final.df[final.df$order %in% "neuroptera", order.spp] <- "neuroptera"
  final.df[final.df$order %in% "neoophora", order.spp] <- "neoophora"

  return(final.df)
}
