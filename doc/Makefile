# Render ABP documentation to HTML

all: abp.html

clean:
	rm -f abp.html cdelord.css
	rm -rf img

export ABP_ROOT = ../
export doc = ./
export PLANTUML = {{PLANTUML}}
export DITAA = {{DITAA}}

CSS = cdelord.css

abp.html: abp.md hello.dot $(CSS)
	mkdir -p $(doc)img
	pandoc -F abp --to=html5 --standalone --self-contained --css=$(CSS) $< -o $@

cdelord.css:
	wget http://cdelord.fr/cdelord.css -O cdelord.css
