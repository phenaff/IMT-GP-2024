SOURCES=$(shell find . -name "*.Rmd")
TARGET = $(SOURCES:%.Rmd=%.pdf)

%.pdf: %.Rmd
	Rscript -e "rmarkdown::render('$<')"

pdf: $(TARGET)

clean:
	rm -rf $(TARGET)
