# Usage:
# make [all]           generate slides, audience handout, and speaker notes
# make slides/X.pdf    generate slides from notes/X.Rmd or scripts/X.Rmd

notes_rmd := $(wildcard notes/*.Rmd)
notes_md := $(patsubst notes/%.Rmd,notes_md/%.md,$(notes_rmd))
scripts_rmd := $(wildcard scripts/*.Rmd)
scripts_md := $(patsubst scripts/%.Rmd,scripts_md/%.md,$(scripts_rmd))

rmds := $(notes_rmd) $(scripts_rmd)
mds := $(notes_md) $(scripts_md)
texs := $(patsubst %.Rmd,%.tex,$(addprefix slides/,$(notdir $(rmds)))) \
    $(patsubst %.Rmd,%.tex,$(addprefix lectures/,$(notdir $(rmds)))) \
    $(patsubst %.Rmd,%.tex,$(addprefix handouts/,$(notdir $(rmds))))
pdfs := $(patsubst %.tex,%.pdf,$(texs))

.PHONY: $(mds) $(texs) $(pdfs) all clean reallyclean
$(notes_md): notes_md/%.md: notes/%.Rmd
$(scripts_md): scripts_md/%.md: scripts/%.Rmd
	mkdir -p $(dir $@)
	R -e 'rmarkdown:render("$<", output_dir="$(dir $@)",  output_file="$(notdir $@)")'

$(texs) $(pdfs) all:
	R -e 'scuro::make("$@")'

clean reallyclean:
	rm -rf $(mds)
	rm -rf figure
	-rmdir notes_md scripts_md
	R -e 'scuro::make("$@")'

.DEFAULT_GOAL := all

