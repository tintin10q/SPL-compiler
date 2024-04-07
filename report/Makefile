REPORT_TARGET=report.pdf
REPORT_SOURCE=report.md

PANDOC_ARGS=--listings --citeproc -o $(REPORT_TARGET) $(REPORT_SOURCE)

$(REPORT_TARGET): $(REPORT_SOURCE)
	pandoc $(PANDOC_ARGS)

watch:
	make && xdg-open $(REPORT_TARGET) && echo $(REPORT_SOURCE) | entr make $(REPORT_TARGET)

clean:
	rm -f $(REPORT_TARGET)

.PHONY: watch clean