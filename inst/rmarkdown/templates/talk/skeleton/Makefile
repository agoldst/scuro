## Usage ##

# make [all]	generate slides, audience handout, and speaker notes
# make all_key	all of the above, plus Keynote version of slides
# 		(requires Keynote and PDF To Keynote:
# 		https://www.cs.hmc.edu/~oneill/freesoftware/pdftokeynote.html
# 		see note below on Keynote errors)

rmds := $(wildcard *.Rmd)
slides := $(patsubst %.Rmd,%-slides.pdf,$(rmds))
slides_key := $(patsubst %.Rmd,%-slides.key,$(rmds))
handouts := $(patsubst %.Rmd,%-handouts.pdf,$(rmds))
notes := $(patsubst %.Rmd,%-notes.pdf,$(rmds))

all: $(slides) $(handouts) $(notes)

all_key: all $(slides_key)

.DEFAULT_GOAL := all

.PHONY: all all_key

## Knitting ##

$(slides): %-slides.pdf: %.Rmd
	R -e 'rmarkdown::render("$<", "ragoldstBeamer::slides", "$@")'

$(handouts): %-handouts.pdf: %.Rmd
	R -e 'rmarkdown::render("$<", "ragoldstBeamer::handout", "$@")'

$(notes): %-notes.pdf: %.Rmd
	R -e 'rmarkdown::render("$<", "ragoldstBeamer::notes", "$@")'

## Slides in Keynote format ##

# PDF To Keynote automatically opens its output, but for some reason
# Keynote is unhappy with this and says it can't open the result.
# However, if you then open the Keynote file yourself from the finder
# or with the open shell command, it will work fine.

$(slides_key): %.key: %.pdf
	rm -f $@
	open -a "PDF To Keynote" $<
	sleep 2
	osascript -e 'tell application "PDF To Keynote"' \
	    -e 'save document 1 in "$(abspath ./$(basename $@))"' \
	    -e 'end tell'

