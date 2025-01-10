.POSIX:
EMACS = emacs

all: youtube-dl-loaddefs.el youtube-dl.elc youtube-dl-play.elc youtube-dl-view.elc youtube-dl-w3m.elc

youtube-dl-loaddefs.el: youtube-dl.el youtube-dl-play.el youtube-dl-view.el youtube-dl-w3m.el
	$(EMACS) -batch -Q --script make-loaddefs.el $@ $^

simulate: youtube-dl.elc
	$(EMACS) -Q -L . -l tests/youtube-dl-simulate.el

clean:
	rm -f *.elc youtube-dl-loaddefs.el simulation.db simulation.db.tmp

youtube-dl.elc: youtube-dl.el youtube-dl-loaddefs.el
youtube-dl-play.elc: youtube-dl-play.el youtube-dl.elc
youtube-dl-view.elc: youtube-dl-view.el youtube-dl.elc
youtube-dl-w3m.elc: youtube-dl-w3m.el youtube-dl.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -l youtube-dl-loaddefs.el --eval '(push "." load-path)' -batch -Q -f batch-byte-compile $<
