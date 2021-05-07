-include ../config.mk
include ../default.mk

.PHONY: install clean AUTHORS.md stats

ORGPAGES  = $(addsuffix .org,$(PKG))
TEXIPAGES = $(addsuffix .texi,$(PKG))
INFOPAGES = $(addsuffix .info,$(PKG))

distfiles = $(strip $(TEXIPAGES) AUTHORS.md makefile \
	$(wildcard release-notes/$(VERSION).txt))

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = $(LOAD_PATH)
# https://github.com/tkf/org-mode
ORG_LOAD_PATH += -L $(EMACS_GITHUB_DIR)/org-mode/contrib/lisp
# https://github.com/emacsmirror/org/
ORG_LOAD_PATH += -L $(EMACS_GITHUB_DIR)/org/lisp
ORG_LOAD_PATH += -L $(EMACS_GITHUB_DIR)/org/contrib/lisp
# https://github.com/tarsius/ox-texinfo-plus
ORG_LOAD_PATH += -L $(EMACS_GITHUB_DIR)/ox-texinfo-plus
endif

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH)
ORG_ARGS += -l org-man -l ox-extra -l ox-texinfo+.el
ORG_EVAL  = --eval "(ox-extras-activate '(ignore-headlines))"
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --eval "(setq make-backup-files nil)"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo


all: info

info: $(ORGPAGES) dir

%.texi: %.org
	@$(EMACS) $(ORG_ARGS) $< $(ORG_EVAL)
	@printf "\n" >> $@

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

install: install-info install-docs

install-docs: install-info
	@$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) AUTHORS.md $(DESTDIR)$(docdir)

install-info: info
	@$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) $(INFOPAGES) $(DESTDIR)$(infodir)

clean:
	@printf "Cleaning Documentation/*...\n"
	@$(RMDIR) dir $(INFOPAGES) $(TEXIPAGES)

prepare-dist: info
	@$(MKDIR) $(distdirdir)/docs
	@$(CP) $(distfiles) $(distdir)/docs

stats:
	@printf "Generating statistics\n"
	@gitstats -c style=/assets/stats.css -c max_authors=999 $(TOP) $(statsdir)

authors: AUTHORS.md

AUTHORS.md:
	@printf "Generating AUTHORS.md..."
	@test -e $(TOP).git \
	&& (printf "$$AUTHORS_HEADER\n" > $@ \
	&& git log --pretty=format:'- %aN <%aE>' | sort -u >> $@ \
	&& printf "done\n" ; ) \
	|| printf "FAILED (non-fatal)\n"

define AUTHORS_HEADER
Authors
=======

The following people have contributed to '$(PKG)'.

Maintainer
----------

- Medardo Antonio Rodriguez <med@merchise.org>

Developers
----------

- Manuel Vazquez Acosta <manuel@merchise.org>
- Medardo Antonio Rodriguez <med@merchise.org>

Contributors
------------

endef
export AUTHORS_HEADER
