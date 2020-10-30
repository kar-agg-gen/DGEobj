context("DGEobj - tests for reset.R functions")


test_that('reset.R: ', {

    # testing DGEobj without levels
    expect_error(resetDGEobj(DGEobj),
                 regexp = "!is.null(attr(dgeObj, \"level\")) is not TRUE",
                 fixed  = TRUE)

    # testing DGEobj without platformType
    test_DGEobj <- setAttributes(item = DGEobj, list("level" = "gene"))
    test_DGEobj <- setAttributes(item = test_DGEobj, list("PlatformType" = NULL))
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "Required attribute \"PlatformType\" is missing.",
                 fixed  = TRUE)

    # testing DGEobj without counts_orig matrix
    test_DGEobj <- setAttributes(item = test_DGEobj, list("PlatformType" = "RNA-Seq"))
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "`%in%`(x = itemName, table = names(dgeObj)) is not TRUE",
                 fixed  = TRUE)

    # testing DGEobj with unavailable data
    names(test_DGEobj) <- c("counts_orig", "counts", "design_orig", "design", "peptideAnnotation_orig")
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "Gene/isoform/exon/protein data not found",
                 fixed  = TRUE)

    # testing valid object
    names(test_DGEobj) <- c("counts_orig", "counts", "design_orig", "design", "isoformData_orig")
    # Warning  expected as current object could not build the GRanges object!
    expect_warning({test_reset_DgeObj <- resetDGEobj(test_DGEobj)},
                   regexp = "Couldn't build a GRanges object!",
                   fixed  = TRUE)

    expect_s3_class(test_reset_DgeObj, "DGEobj")

    # testing DGEobj with effectiveLength_orig data
    # -- code
})
