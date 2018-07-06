# StreamCat Files ---------------------------------------------------------
scat.file.path <- "H:/GIS3/GIS5/New_GIS/streamcat/unzip"
scat.files.vec <- list.files(scat.file.path)

# Needed? -----------------------------------------------------------------
ripbuf.files.vec <- scat.files.vec[grepl("RipBuf", scat.files.vec)]
non.ripbuf.files.vec <- scat.files.vec[!grepl("RipBuf", scat.files.vec)]
common.cols.vec <- c("comid", "catareasqkm", "wsareasqkm", "catpctfull",
                     "wspctfull")
# Import and Join all StreamCat Files -------------------------------------
scat.df <- map(scat.files.vec , function(file.i) {
  print(file.i)
  data.table::fread(file.path(scat.file.path, file.i)) %>%
    prep_df()
}) %>%
  reduce(full_join)
