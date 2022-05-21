.POSIX:
EMACS = emacs

all: youtube-dl-loaddefs.el youtube-dl.elc

youtube-dl-loaddefs.el: youtube-dl.el
	$(EMACS) -batch -Q --eval \
		'(let ((make-backup-files nil) \
		       (generated-autoload-file (expand-file-name "youtube-dl-loaddefs.el"))) \
		   (update-file-autoloads "youtube-dl.el" t)))'

simulate: youtube-dl.elc
	$(EMACS) -Q -L . -l tests/youtube-dl-simulate.el

clean:
	rm -f youtube-dl.elc simulation.db simulation.db.tmp

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -f batch-byte-compile $<
