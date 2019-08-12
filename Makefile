-include config.mk
include default.mk

.PHONY: all help lisp docs info \
	install install-lisp install-docs install-info local-install \
	clean clean-lisp clean-info clean-archives \
	release dist prepare-dist $(PKG)-$(VERSION).tar.gz


all: lisp docs

define HELP

make [OPTION]

See "default.mk" for variables you might want to set.

BUILD OPTIONS
    [all]          - compile elisp and documentation
    lisp           - compile package emacs-lisp library
    docs           - generate info manuals
    info           - generate info manuals

INSTALL OPTIONS
    install        - install emacs-lisp and documentation
    install-lisp   - install emacs-lisp
    local-install  - local install using ELPA directory as target
    install-docs   - install all documentation
    install-info   - install info manuals only

CLEAN OPTIONS
    clean          - clean emacs-lisp, documentation and tarball
    clean-lisp     - clean emacs-lisp library
    clean-docs     - clean documentation
    clean-info     - clean documentation
endef

help:
	$(info $(HELP))
	@printf "\n"


## Build

lisp:
	@$(MAKE) -C lisp lisp

docs:
	@$(MAKE) -C docs all

info:
	@$(MAKE) -C docs info


## Install

install: install-lisp install-info

install-lisp: lisp
	@$(MAKE) -C lisp install

install-info: info
	@$(MAKE) -C docs install

local-install:
	@$(MAKE) -C lisp local-install


## Clean

clean: clean-lisp clean-docs clean-archives

clean-lisp:
	@$(MAKE) -C lisp clean

clean-docs: clean-info

clean-info:
	@$(MAKE) -C docs clean

clean-archives:
	@$(RM) *.tar.gz *.tar lisp/$(PKG)-version.el
	@$(RMDIR) $(PKG)-$(VERSION)


## Distribution tar file

dist: prepare-dist $(PKG)-$(VERSION).tar.gz

prepare-dist:
	@printf "Packing $@\n"
	$(eval distname = $(PKG)-$(VERSION))
	$(eval distfiles = LICENSE default.mk Makefile README.md)
	@$(MKDIR) $(distname)
	@$(CP) $(distfiles) $(distname)
	@$(MAKE) distdir=$(TOP)/$(distname) -C lisp prepare-dist
	@$(MAKE) distdir=$(TOP)/$(distname) -C docs prepare-dist

$(PKG)-$(VERSION).tar.gz:
	@$(TAR) cz --mtime=$(distname) -f $(distname).tar.gz $(distname)
	@$(RMDIR) $(distname)


# Release management

check-not-melpa::
ifdef MELPA
  $(error A MELPA version is not allowed for a new release)
endif

check-new-release::
	$(eval CVER = $(call cversion,$(VERSION)))
	$(eval CREL = $(call cversion,$(LAST_RELEASE)))
	$(if $(shell test $(CREL) -ge $(CVER) && echo "BAD"),\
	  $(error New release '$(VERSION)' must be > '$(LAST_RELEASE)'))

check-clean-git:
	$(if $(shell git status --porcelain 2>/dev/null),\
	  $(error GIT tree not clean, commit changes first))

set-package-version:
	@$(BATCH) --eval "(let ((xorns-version \"$(VERSION)\"))\
	$$SET_PACKAGE_VERSION)"


git-tag-version: check-not-melpa check-new-release check-clean-git
	$(eval NEWREL = $(call nversion,$(VERSION)))
	@printf "Tagging new release $(NEWREL)\n"
	@git tag $(NEWREL)

release: git-tag-version set-package-version
	@printf "Commiting new release $(NEWREL)\n"
	@git commit -am "Set new release to $(NEWREL)"


define SET_PACKAGE_VERSION
(with-temp-file "lisp/xorns.el"
  (insert-file-contents "lisp/xorns.el")
  (re-search-forward "^;; Version: ")
  (delete-region (point) (line-end-position))
  (insert xorns-version))
endef
export SET_PACKAGE_VERSION
