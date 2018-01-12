## ---- user config ----

# Set to anything non-empty to suppress most of latex's messaging. To diagnose
# LaTeX errors, you may want to do `make latex_quiet=""` to get verbose output
latex_quiet := true

# Set to anything non-empty to reprocess TeX files every time we make a PDF.
# Otherwise these files will be regenerated only when the source markdown
# changes; in that case, if you change other dependencies (e.g. a
# bibliography), use the -B option to make in order to force regeneration.
# always_latexmk := true
always_latexmk := 

# Set to anything non-empty to use xelatex rather than pdflatex. I always do
# this in order to use system fonts and better Unicode support. pdflatex is
# faster, and there are some packages with which xelatex is incompatible.
xelatex := true

# list of markdown files that are not to be made into PDFs
EXCLUDE := README.md

# Extra options to pandoc (e.g. "-H mypreamble.tex")
PANDOC_OPTIONS :=

## ---- special external file ----

# Normally this does not need to be changed:
# works if the template is local or in ~/.pandoc/templates
PANDOC_TMPL := memoir-article.latex

## ---- subdirectories (normally, no need to change) ----

# source of markdowns
md_dir := .

# temporary file subdirectory; will be removed after every latex run
temp_dir := tmp

# name of output directory for .tex and .pdf files
out_dir := out

## ---- commands ----

# Change these only to really change the behavior of the whole setup

pandoc2 := `pandoc -v | head -1 | grep '^pandoc 2'`
pandoc_xelatex := $(if $(xelatex),$(if $(pandoc2),--pdf-engine,--latex-engine) xelatex)
PANDOC := pandoc $(pandoc_xelatex) \
    --template $(PANDOC_TMPL) $(PANDOC_OPTIONS)

LATEXMK := latexmk $(if $(xelatex),-xelatex,-pdflatex="pdflatex %O %S") \
    -pdf -dvi- -ps- $(if $(latex_quiet),-silent,-verbose) \
    -outdir=$(temp_dir)

## ---- build rules ----

mds := $(filter-out $(addprefix $(md_dir)/,$(EXCLUDE)),$(wildcard $(md_dir)/*.md))

texs := $(patsubst $(md_dir)/%.md,$(out_dir)/%.tex,$(mds))
pdfs := $(patsubst $(md_dir)/%.md,$(out_dir)/%.pdf,$(mds)) 

$(texs): $(out_dir)/%.tex: $(md_dir)/%.md
	mkdir -p $(dir $@)
	$(PANDOC) -o $@ $<

phony_pdfs := $(if $(always_latexmk),$(pdfs) $(notes_pdf))

.PHONY: $(phony_pdfs) all clean reallyclean
$(pdfs): %.pdf: %.tex
	mkdir -p $(dir $@)
	rm -rf $(dir $@)$(temp_dir)
	cd $(dir $<); $(LATEXMK) $(notdir $<)
	mv $(dir $@)$(temp_dir)/$(notdir $@) $@
	rm -r $(dir $@)$(temp_dir)

all: $(pdfs)

# clean up everything except final pdfs
clean:
	rm -rf $(out_dir)/$(temp_dir)
	rm -f $(texs)

# clean up everything including pdfs
reallyclean: clean
	rm -f $(pdfs)
	-rmdir $(out_dir)

.DEFAULT_GOAL := all
