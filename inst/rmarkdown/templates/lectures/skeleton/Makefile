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

slides_notes_pdf := $(patsubst %.Rmd,%.pdf,$(addprefix slides/,$(notdir $(notes_rmd))))
lectures_notes_pdf := $(patsubst %.Rmd,%.pdf,$(addprefix lectures/,$(notdir $(notes_rmd))))
handouts_notes_pdf := $(patsubst %.Rmd,%.pdf,$(addprefix handouts/,$(notdir $(notes_rmd))))
slides_scripts_pdf := $(patsubst %.Rmd,%.pdf,$(addprefix slides/,$(notdir $(scripts_rmd))))
lectures_scripts_pdf := $(patsubst %.Rmd,%.pdf,$(addprefix lectures/,$(notdir $(scripts_rmd))))
handouts_scripts_pdf := $(patsubst %.Rmd,%.pdf,$(addprefix handouts/,$(notdir $(scripts_rmd))))

notes_pdf := $(slides_notes_pdf) $(lectures_notes_pdf) $(handouts_notes_pdf)
scripts_pdf := $(slides_scripts_pdf) $(lectures_scripts_pdf) $(handouts_scripts_pdf)

pdfs := $(notes_pdf) $(scripts_pdf)
texs := $(patsubst %.pdf,%.tex,$(pdfs))

.PHONY: $(mds) $(pdfs) all clean reallyclean

$(notes_md): notes_md/%.md: notes/%.Rmd
	mkdir -p $(dir $@)
	R -e 'rmarkdown::render("$<", output_dir="$(dir $@)",  output_file="$(notdir $@)")'

$(scripts_md): scripts_md/%.md: scripts/%.Rmd
	mkdir -p $(dir $@)
	R -e 'rmarkdown::render("$<", output_dir="$(dir $@)",  output_file="$(notdir $@)")'

$(slides_notes_pdf): slides/%.pdf: notes_md/%.md
	R -e 'scuro::render_pdf("$<", "slides")'
$(lectures_notes_pdf): lectures/%.pdf: notes_md/%.md
	R -e 'scuro::render_pdf("$<", "lecture")'
$(handouts_notes_pdf): handouts/%.pdf: notes_md/%.md
	R -e 'scuro::render_pdf("$<", "handout")'
$(slides_scripts_pdf): slides/%.pdf: scripts_md/%.md
	R -e 'scuro::render_pdf("$<", "slides")'
$(lectures_scripts_pdf): lectures/%.pdf: scripts_md/%.md
	R -e 'scuro::render_pdf("$<", "lecture")'
$(handouts_scripts_pdf): handouts/%.pdf: scripts_md/%.md
	R -e 'scuro::render_pdf("$<", "handout")'

all: $(pdfs)

clean:
	rm -rf lectures/tmp slides/tmp handouts/tmp
	rm -f $(texs)

# clean up everything including pdfs
reallyclean: clean
	rm -f $(pdfs)
	-rmdir lectures slides handouts
	rm -rf $(mds)
	rm -rf $(addsuffix _cache,$(basename $(notes_rmd)))
	rm -rf $(addsuffix _cache,$(basename $(scripts_rmd)))
	rm -f $(addsuffix -tikzDictionary,$(basename $(notes_rmd)))
	rm -f $(addsuffix -tikzDictionary,$(basename $(scripts_rmd)))
	rm -rf figure
	-rmdir notes_md scripts_md


.DEFAULT_GOAL := all

