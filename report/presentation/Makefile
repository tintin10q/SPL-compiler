TARGET_DIR=dist
LATEXMK=latexmk
LATEXMK_OPTS=-pdf -auxdir=$(TARGET_DIR) -outdir=$(TARGET_DIR) -f -shell-escape

.PHONY : clean

presentation.pdf : presentation.tex
	$(LATEXMK) $(LATEXMK_OPTS) -pvc presentation.tex

clean : ; rm -r $(TARGET_DIR)

