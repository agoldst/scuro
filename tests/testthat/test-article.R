context("article template")

tmp <- tempfile("scuro")
dir.create(tmp)
d <- file.path(tmp, "test")

tmpl_files <- c("Makefile", "sources.bib", "test.Rmd")
test_that("all expected files are present", {
    rmarkdown::draft(d, "article", "scuro", edit=F)

    expect_equal(list.files(d, recursive=T), tmpl_files)

    unlink(d, recursive=T)
    expect_false(file.exists(d))
})

test_that("making does what we want", {
    rmarkdown::draft(d, "article", "scuro", edit=F)
    system2("make", c("-C", d))
    expect_true(all(c("test.tex", "test.pdf", "figure")
                    %in% list.files(d)))

    system2("make", c("-C", d, "clean"))
    expect_false(file.exists(file.path(d, "test.tex")),
                 info="make clean")
    expect_true(file.exists(file.path(d, "test.pdf")),
                info="make clean")

    system2("make", c("-C", d, "reallyclean"))
    expect_equal(list.files(d, recursive=T), tmpl_files,
                info="make reallyclean")

    unlink(d, recursive=T)
    expect_false(file.exists(d))
})
