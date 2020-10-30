context("DGEobj - tests for reset.R functions")


test_that('reset.R: ', {

    # checking DGEobj without levels
    expect_error(resetDGEobj(DGEobj),
                 regexp = "!is.null(attr(dgeObj, \"level\")) is not TRUE",
                 fixed  = TRUE)

    # checking DGEobj without platformType
    test_DGEobj <- setAttributes(item = DGEobj, list("level" = "gene"))
    test_DGEobj <- setAttributes(item = test_DGEobj, list("PlatformType" = NULL))
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "Required attribute \"PlatformType\" is missing.",
                 fixed  = TRUE)

    # checking DGEobj with wrong counts matirx
    test_DGEobj <- setAttributes(item = test_DGEobj, list("PlatformType" = "RNA-Seq"))
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "`%in%`(x = itemName, table = names(dgeObj)) is not TRUE",
                 fixed  = TRUE)

    # checking DGEobj with wrong data
    names(test_DGEobj) <- c( "counts_orig" , "counts", "design_orig" , "design", "peptideAnnotation_orig")
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "Gene/isoform/exon/protein data not found",
                 fixed  = TRUE)

    # checking valid object
    for (matrix_name in  c("geneData_orig", "isoformData_orig", "exonData_orig", "proteinData_orig")) {
        names(test_DGEobj) <- c( "counts_orig" , "counts", "design_orig" , "design", matrix_name)
        # Expecting warning as current object could not build the GRanges object!
        expect_warning({test_reset_DgeObj <- resetDGEobj(test_DGEobj)},
                       regexp = "Couldn't build a GRanges object!",
                       fixed  = TRUE)

        expect_s3_class(test_reset_DgeObj, "DGEobj")
    }

    test_DGEobj <- addItem(test_DGEobj,
                           item = test_DGEobj$proteinData_orig,
                           itemName = "effectiveLength_orig",
                           itemType = "effectiveLength_orig")
    attr(test_DGEobj, "objDef")$type[["effectiveLength"]] <- "meta"
    expect_warning({test_reset_DgeObj <- resetDGEobj(test_DGEobj)},
                   regexp = "Couldn't build a GRanges object!",
                   fixed  = TRUE)

    expect_s3_class(test_reset_DgeObj, "DGEobj")

    # checking DGEobj with effectiveLength_orig data
    # -- code
})
