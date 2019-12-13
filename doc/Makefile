# Render ABP documentation to HTML

all: abp.html

clean:
	rm -f abp.html cdsoft.css
	rm -rf img

export ABP_ROOT = ../
export doc = ./
export PLANTUML = {{PLANTUML}}

CSS = cdsoft.css

abp.html: abp.md hello.dot $(CSS)
	mkdir -p $(doc)img
	pandoc -F abp --to=html5 --standalone --self-contained --css=$(CSS) $< -o $@

cdsoft.css:
	curl -O https://cdsoft.fr/cdsoft.css
