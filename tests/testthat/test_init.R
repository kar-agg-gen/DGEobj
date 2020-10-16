context("DGEobj - tests for init.R functions")


test_that('init.R: initDGEobj()', {

    # collect data from test object to initialize new DGEobj
    counts     <- getItem(DGEobj, "intensity_orig")
    rowData    <- getItem(DGEobj, "peptideAnnotation_orig")
    colData    <- getItem(DGEobj, "design_orig")
    level      <- "gene" # peptide level is not available
    customAttr <- list(Genome    = "Mouse.B38",
                       GeneModel = "Ensembl.R84")

    # checking warning as Grange object is not available.
    expect_warning({test_DgeObj <- initDGEobj(counts     = counts,
                                             rowData    = rowData,
                                             colData    = colData,
                                             level      = level,
                                             customAttr = customAttr)},
                   "Couldn't build a GRanges object")
    # verifying class
    expect_s3_class(test_DgeObj, "DGEobj")
    expect_type(attributes(test_DgeObj), "list")

    # checking names and dimensions
    expect_setequal(names(test_DgeObj), c("counts_orig", "counts", "design_orig", "design", "geneData_orig", "geneData" ))
    expect_equal(dim(test_DgeObj), c(5900, 165))

    # verifying missing value errors
    expect_error(initDGEobj(rowData =  rowData, colData =  colData, level =  level, customAttr = customAttr),
                 regexp = "!missing(counts) is not TRUE",
                 fixed  = TRUE)
    expect_error(initDGEobj(counts = counts, colData =  colData, level =  level, customAttr = customAttr),
                 regexp = "!missing(rowData) is not TRUE",
                 fixed  = TRUE)
    expect_error(initDGEobj(counts = counts, rowData =  rowData, level =  level, customAttr = customAttr),
                 regexp = "!missing(colData) is not TRUE",
                 fixed  = TRUE)
    expect_error(initDGEobj(counts = counts, rowData =  rowData, colData =  colData, customAttr = customAttr),
                 regexp = "!missing(level) is not TRUE",
                 fixed  = TRUE)
})
