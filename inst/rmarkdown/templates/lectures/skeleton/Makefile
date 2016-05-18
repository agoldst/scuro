# Usage:
# make [all]	generate slides, audience handout, and speaker notes

rmds := $(wildcard notes/*.Rmd) $(wildcard scripts/*.Rmd)
mds := $(patsubst %.Rmd,%.md,$(rmds))
texs := $(patsubst %.Rmd,%.tex,$(addprefix slides/,$(notdir $(rmds)))) \
    $(patsubst %.Rmd,%.tex,$(addprefix lectures/,$(notdir $(rmds)))) \
    $(patsubst %.Rmd,%.tex,$(addprefix handouts/,$(notdir $(rmds))))
pdfs := $(patsubst %.tex,%.pdf,$(texs))

.PHONY: $(mds) $(texs) $(pdfs) all
$(mds): %.md: %.Rmd
	R -e 'rmarkdown:render("$<")'

$(texs) $(pdfs) all:
	R -e 'scuro::make("$<")'

.DEFAULT_GOAL := all

