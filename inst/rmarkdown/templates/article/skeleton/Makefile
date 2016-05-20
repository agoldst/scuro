# Usage:
# make [all]		create PDF(s) from Rmd(s)
# make clean		clean up md -> PDF intermediate files, leaving PDFs
# make reallyclean	clean up md -> Rmd intermediate files and PDFs too

rmds := $(wildcard *.Rmd)
pdfs := $(patsubst %.Rmd,%.pdf,$(rmds))
texs := $(patsubst %.pdf,%.tex,$(pdfs))

.PHONY: $(pdfs) all clean reallyclean

$(pdfs): %.pdf: %.Rmd
	R -e 'rmarkdown::render("$<")'

all: $(pdfs)

clean:
	rm -f $(texs)

reallyclean: clean
	rm -rf $(mds)
	rm -rf $(addsuffix _cache,$(basename $(rmds)))
	rm -f $(addsuffix -tikzDictionary,$(basename $(rmds)))
	rm -rf figure
	rm -f $(pdfs)

.DEFAULT_GOAL := all

