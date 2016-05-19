
rmds := $(wildcard *.Rmd)
mds := $(patsubst %.Rmd,md/%.md,$(rmds))
texs := $(patsubst %.Rmd,out/%.tex,$(rmds))
pdfs := $(patsubst %.tex,%.pdf,$(texs))

.PHONY: $(mds) $(texs) $(pdfs) all clean reallyclean

$(mds): md/%.md: %.Rmd
	mkdir -p $(dir $@)
	R -e 'rmarkdown::render("$<", output_dir="$(dir $@)",  output_file="$(notdir $@)")'

$(texs): out/%.tex: md/%.md
	R -e 'scuro::make_article("$@")'

$(pdfs): %.pdf: %.tex
	R -e 'scuro::make_article("$@")'

all: $(pdfs)

clean:
	R -e 'scuro::make_article("$@")'

reallyclean: clean
	R -e 'scuro::make_article("$@")'
	rm -rf $(mds)
	rm -rf $(addsuffix _cache,$(basename $(rmds)))
	rm -f $(addsuffix -tikzDictionary,$(basename $(rmds)))
	rm -rf figure
	-rmdir md

.DEFAULT_GOAL := all

