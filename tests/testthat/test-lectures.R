context("lectures template")

tmp <- tempfile("scuro")
dir.create(tmp)
d <- file.path(tmp, "test")

test_that("all expected files are present", {
    rmarkdown::draft(d, "lectures", "scuro", edit=F)

    expect_equal(list.files(d, recursive=T),
        c("Makefile",
          file.path("notes", "notes-sample.Rmd"),
          file.path("scripts", "script-sample.Rmd"),
          "slide-meta.yaml",
          "test.Rmd"))
    unlink(d, recursive=T)
    expect_false(file.exists(d))
})

test_that("making slides does what we want", {
    rmarkdown::draft(d, "lectures", "scuro", edit=F)
    system2("make", c("-C", d, file.path("slides", "notes-sample.pdf")))
    expect_equal(list.files(file.path(d, "slides")),
                 c("notes-sample.pdf", "notes-sample.tex"))
    unlink(d, recursive=T)
    expect_false(file.exists(d))
})

test_that("default make does what we want", {
    rmarkdown::draft(d, "lectures", "scuro", edit=F)
    system2("make", c("-C", d))
    expect_equal(list.files(file.path(d, "slides")),
                 c(paste0("notes-", c("sample.pdf", "sample.tex")),
                   paste0("script-", c("sample.pdf", "sample.tex"))))
    expect_equal(list.files(file.path(d, "lectures")),
                 c(paste0("notes-", c("sample.pdf", "sample.tex")),
                   paste0("script-", c("sample.pdf", "sample.tex"))))
    expect_equal(list.files(file.path(d, "handouts")),
                 c(paste0("notes-", c("sample.pdf", "sample.tex")),
                   paste0("script-", c("sample.pdf", "sample.tex"))))
    unlink(d, recursive=T)
    expect_false(file.exists(d))
})

test_that("make phony does what we want", {
    rmarkdown::draft(d, "lectures", "scuro", edit=F)
    system2("make", c("-C", d, "notes-sample"))
    expect_equal(list.files(file.path(d, "slides")),
                 paste0("notes-", c("sample.pdf", "sample.tex")))
    expect_equal(list.files(file.path(d, "lectures")),
                 paste0("notes-", c("sample.pdf", "sample.tex")))
    expect_equal(list.files(file.path(d, "handouts")),
                 paste0("notes-", c("sample.pdf", "sample.tex")))
    unlink(d, recursive=T)
    expect_false(file.exists(d))
})

unlink(tmp)
