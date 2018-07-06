#------------------------------------------------------------------------------
# Extract
sections.path <- c("notebook/misc/disturbance_gradient/sections")
r.files.vec <- list.files(sections.path)
r.files.vec <- r.files.vec[grepl(".Rmd", r.files.vec)]

extracted.path <- c("notebook/misc/disturbance_gradient/raw_script/extracted")
purrr::map(r.files.vec, function(file.i) {
  file.name <- gsub(".Rmd", "", file.i)
  extracted.file <- paste0(file.name, ".R")
  knitr::purl(file.path(sections.path, file.i),
              file.path(extracted.path, extracted.file))
})

#------------------------------------------------------------------------------
# Run
source.vec <- c(
  "preprocessing.R",
  "functions.R",
  # "spatial_visualization.R",
  # "impervious_plots.R",
  # "general_impervious_response.R",
  # "remove_imp_forest.R",
  "prep_features.R"#,
  # "feature_plots.R"
)

purrr::map(source.vec, function(source.i) {
  source(file.path(extracted.path, source.i))
})

#------------------------------------------------------------------------------
