.PHONY: clean all

TGTS=farmer.lus farmer.pdf

all: $(TGTS)

farmer.pdf: farmer.md
	pandoc -t pdf -o $@ $^

farmer.lus: farmer.md
	pandoc -t html -o /dev/null --lua-filter=lustre.lua $^ > $@


clean:
	-rm $(TGTS)

