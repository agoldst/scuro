# Usage:
# make [all]		create PDF(s) from Rmd(s)
# make clean		clean up md -> PDF intermediate files, leaving PDFs
# make reallyclean	clean up md -> Rmd intermediate files and PDFs too

rmds := $(wildcard *.Rmd)
mds := $(patsubst %.Rmd,%.md,$(rmds))
texs := $(patsubst %.Rmd,%.tex,$(rmds))
pdfs := $(patsubst %.tex,%.pdf,$(texs))

.PHONY: $(mds) $(texs) $(pdfs) all clean reallyclean

$(mds): %.md: %.Rmd
	R -e 'rmarkdown::render("$<")'

$(texs): %.tex: %.md
	R -e 'scuro::make_article("$@")'

$(pdfs): %.pdf: %.tex
	R -e 'scuro::make_article("$@")'

all: $(pdfs)

clean:
	rm -rf tmp
	rm -f $(texs)

reallyclean: clean
	rm -rf $(mds)
	rm -rf $(addsuffix _cache,$(basename $(rmds)))
	rm -f $(addsuffix -tikzDictionary,$(basename $(rmds)))
	rm -rf figure
	rm -f $(pdfs)

.DEFAULT_GOAL := all

