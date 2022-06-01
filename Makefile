.POSIX:
EMACS = emacs

all: youtube-dl-loaddefs.el youtube-dl.elc youtube-dl-play.elc youtube-dl-view.elc youtube-dl-w3m.elc

youtube-dl-loaddefs.el: youtube-dl.el youtube-dl-play.el youtube-dl-view.el youtube-dl-w3m.el
	$(EMACS) -batch -Q --eval \
		'(let ((make-backup-files nil) \
		       (generated-autoload-file (expand-file-name "youtube-dl-loaddefs.el"))) \
		   (update-file-autoloads "youtube-dl.el" t) \
		   (update-file-autoloads "youtube-dl-play.el" t) \
		   (update-file-autoloads "youtube-dl-view.el" t) \
		   (update-file-autoloads "youtube-dl-w3m.el" t)))'

simulate: youtube-dl.elc
	$(EMACS) -Q -L . -l tests/youtube-dl-simulate.el

clean:
	rm -f *.elc youtube-dl-loaddefs.el simulation.db simulation.db.tmp

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -f batch-byte-compile $<
