LATEXMK = latexmk
BUILD = _build

.PHONY: all clean main

all: main

main:
	@mkdir -p $(BUILD)
	$(LATEXMK) \
		-pdf \
		-interaction=nonstopmode \
		-outdir=$(BUILD) \
		main.tex

clean:
	rm -rf $(BUILD)
