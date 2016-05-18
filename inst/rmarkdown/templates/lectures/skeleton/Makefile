# Usage:
# make [all]           generate slides, audience handout, and speaker notes
# make slides/X.pdf    generate slides from notes/X.Rmd or scripts/X.Rmd
# make -B slides/X.pdf force regeneration even if make thinks it's not needed
#                      (every dependency will be rebuilt; to rerun latex only,
#                      touch slides/X.tex; make slides/X.pdf)
# make clean

notes_rmd := $(wildcard notes/*.Rmd)
notes_md := $(patsubst notes/%.Rmd,notes_md/%.md,$(notes_rmd))
scripts_rmd := $(wildcard scripts/*.Rmd)
scripts_md := $(patsubst scripts/%.Rmd,scripts_md/%.md,$(scripts_rmd))

rmds := $(notes_rmd) $(scripts_rmd)
mds := $(notes_md) $(scripts_md)

slides_notes_tex := $(patsubst %.Rmd,%.tex,$(addprefix slides/,$(notdir $(notes_rmd))))
lectures_notes_tex := $(patsubst %.Rmd,%.tex,$(addprefix lectures/,$(notdir $(notes_rmd))))
handouts_notes_tex := $(patsubst %.Rmd,%.tex,$(addprefix handouts/,$(notdir $(notes_rmd))))
slides_scripts_tex := $(patsubst %.Rmd,%.tex,$(addprefix slides/,$(notdir $(scripts_rmd))))
lectures_scripts_tex := $(patsubst %.Rmd,%.tex,$(addprefix lectures/,$(notdir $(scripts_rmd))))
handouts_scripts_tex := $(patsubst %.Rmd,%.tex,$(addprefix handouts/,$(notdir $(scripts_rmd))))

notes_tex := $(slides_notes_tex) $(lectures_notes_tex) $(handouts_notes_tex)
scripts_tex := $(slides_scripts_tex) $(lectures_scripts_tex) $(handouts_scripts_tex)

texs := $(notes_tex) $(scripts_tex)

notes_pdf := $(patsubst %.tex,%.pdf, $(notes_tex))
scripts_pdf := $(patsubst %.tex,%.pdf, $(scripts_tex))

pdfs := $(notes_pdf) $(scripts_pdf)

.PHONY: $(mds) $(texs) $(pdfs) all clean reallyclean

$(notes_md): notes_md/%.md: notes/%.Rmd
	mkdir -p $(dir $@)
	R -e 'rmarkdown::render("$<", output_dir="$(dir $@)",  output_file="$(notdir $@)")'

$(scripts_md): scripts_md/%.md: scripts/%.Rmd
	mkdir -p $(dir $@)
	R -e 'rmarkdown::render("$<", output_dir="$(dir $@)",  output_file="$(notdir $@)")'

$(slides_notes_tex): slides/%.tex: notes_md/%.md
	R -e 'scuro::make("$@")'
$(lectures_notes_tex): lectures/%.tex: notes_md/%.md
	R -e 'scuro::make("$@")'
$(handouts_notes_tex): handouts/%.tex: notes_md/%.md
	R -e 'scuro::make("$@")'
$(slides_scripts_tex): slides/%.tex: scripts_md/%.md
	R -e 'scuro::make("$@")'
$(lectures_scripts_tex): lectures/%.tex: scripts_md/%.md
	R -e 'scuro::make("$@")'
$(handouts_scripts_tex): handouts/%.tex: scripts_md/%.md
	R -e 'scuro::make("$@")'
$(pdfs): %.pdf: %.tex
	R -e 'scuro::make("$@")'

all: $(pdfs)

clean:
	R -e 'scuro::make("$@")'

reallyclean: clean
	R -e 'scuro::make("$@")'
	rm -rf $(mds)
	rm -rf $(addsuffix _cache,$(basename $(notes_rmd)))
	rm -rf $(addsuffix _cache,$(basename $(scripts_rmd)))
	rm -f $(addsuffix -tikzDictionary,$(basename $(notes_rmd)))
	rm -f $(addsuffix -tikzDictionary,$(basename $(scripts_rmd)))
	rm -rf figure
	-rmdir notes_md scripts_md


.DEFAULT_GOAL := all

