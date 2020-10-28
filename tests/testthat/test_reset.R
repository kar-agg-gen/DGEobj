context("DGEobj - tests for reset.R functions")


test_that('reset.R: ', {

    # checking DGEobj without levels
    expect_error(resetDGEobj(DGEobj,"RNA-Seq"),
                 regexp = "!is.null(attr(dgeObj, \"level\")) is not TRUE",
                 fixed  = TRUE)

    # checking DGEobj without platformType
    test_DGEobj <- setAttributes(item = DGEobj,list("level" = "gene"))

    expect_error(resetDGEobj(test_DGEobj,"RNA-Seq"),
                 regexp = "Required attribute \"PlatformType\" is missing!  Must use platformType argument.",
                 fixed  = TRUE)

    # checking DGEobj with wrong counts matirx
    test_DGEobj <- setAttributes(item = test_DGEobj, list("PlatformType" = "RNA-Seq"))
    expect_error(resetDGEobj(test_DGEobj,"RNA-Seq"),
                 regexp = "`%in%`(x = itemName, table = names(dgeObj)) is not TRUE",
                 fixed  = TRUE)

    # checking DGEobj with wrong data
    names(test_DGEobj) <- c( "counts_orig" , "counts", "design_orig" , "design", "peptideAnnotation_orig")
    expect_error(resetDGEobj(test_DGEobj,"RNA-Seq"),
                 regexp = "Gene/isoform/exon/protein data not found",
                 fixed  = TRUE)

    # checking valid object
    names(test_DGEobj) <- c( "counts_orig" , "counts", "design_orig" , "design", "isoformData_orig")
    # Expecting warning as current object could not build the GRanges object!
    expect_warning({test_reset_DgeObj <- resetDGEobj(test_DGEobj,"RNA-Seq")},
                   regexp = "Couldn't build a GRanges object!",
                   fixed  = TRUE)

    expect_s3_class(test_reset_DgeObj, "DGEobj")

    # checking DGEobj with effectiveLength_orig data
    # -- code
})
