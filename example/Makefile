.PHONY: clean all

TGTS=farmer.lus farmer.pdf farmer.html

all: $(TGTS)

farmer.pdf: farmer.md lustre.png
	pandoc -t pdf -o $@ $<

farmer.html: farmer.md farmer.css lustre.png
	pandoc -t html --css=farmer.css --standalone -o $@ $<

farmer.lus: farmer.md
	pandoc -t html -o /dev/null --lua-filter=lustre.lua $^ > $@


clean:
	-rm $(TGTS)

