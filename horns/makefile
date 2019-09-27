-include ../config.mk
include ../default.mk

.PHONY: $(PKG)-autoloads.el $(PKG)-version.el $(PKG)-pkg.el \
	all lisp autoloads pkgdef local-install

SPECIALS = $(PKG)-autoloads.el $(PKG)-version.el $(PKG)-pkg.el
ELS = $(filter-out $(SPECIALS),$(wildcard *.el))
ELCS = $(ELS:.el=.elc)

pkg_pair = ($(call pkg_name,$(1)) "$(call pkg_version,$(1))")
pkg_pairs = $(foreach p,$(DEPENDENCIES),$(call pkg_pair,$(p)))
enum_kwords = $(foreach kw,$(PKG_KWORDS),"$(kw)")

all: lisp

lisp: $(ELCS) autoloads pkgdef versionlib
autoloads: $(PKG)-autoloads.el
pkgdef: $(PKG)-pkg.el
versionlib: $(PKG)-version.el

install: lisp
	@$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) magit-autoloads.el magit-version.el $(ELCS) $(DESTDIR)$(lispdir)

local-install: pkgdef
	@printf "Local installing '$(PKG)'\n"
	@$(BATCH) --debug --load=$(TOP)/etc/local-install.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\") (delete-file \"$@\"))\
	(setq with-editor-emacsclient-executable nil)\
	(setq package-enable-at-startup nil))" \
	--load=$(TOP)/package-archives \
	-f batch-byte-compile $<

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$AUTOLOADS_TMPL" > $@
	@$(BATCH) --eval "(let (\
	(make-backup-files nil)\
	(vc-handled-backends nil)\
	(default-directory (file-truename default-directory))\
	(generated-autoload-file (expand-file-name \"$@\"))\
	(find-file-visit-truename t))\
	(update-directory-autoloads default-directory))"

$(PKG)-version.el:
	@printf "Generating $@\n"
	@printf "%s" "$$VERSIONLIB_TMPL" > $@

$(PKG)-pkg.el:
	@printf "Generating $@\n"
	@$(BATCH) --eval "(let ((emacs-version \"$(EMACS_VERSION)\"))\
	$$PKGDEF_TMPL)"

clean:
	@printf "Cleaning lisp/*...\n"
	@$(RM) *.elc
	@$(RM) $(SPECIALS)

prepare-dist: lisp
	$(eval distfiles := $(ELS) $(PKG)-version.el makefile)
	@$(MKDIR) $(distdirdir)/lisp
	@$(CP) $(distfiles) $(distdir)/lisp


define AUTOLOADS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here

endef
#'
# IDK why those exports are needed
export AUTOLOADS_TMPL

define VERSIONLIB_TMPL
;;; $(PKG)-version.el --- the $(PKG) version you are using

(setq $(PKG)-version "$(VERSION)")

(provide '$(PKG)-version)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; $(PKG)-version.el ends here

endef
#'
export VERSIONLIB_TMPL

define PKGDEF_TMPL
(with-temp-file "$(PKG)-pkg.el"
  (insert (pp-to-string
`(define-package "$(PKG)" "$(VERSION)" ;`
   "$(PKG_DESC)"
   '((emacs ,emacs-version) ; '
   $(call pkg_pairs))
   :keywords '$(call enum_kwords)))) ;'
  (goto-char (point-min))
  (re-search-forward " \"M")
  (goto-char (match-beginning 0))
  (insert "\n  "))
endef
export PKGDEF_TMPL
