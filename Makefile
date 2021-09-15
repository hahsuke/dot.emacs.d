
EMACS = emacs

all: init.elc

%.elc: %.el
	$(EMACS) --batch -f batch-byte-compile $<

clean:
	rm *.elc
