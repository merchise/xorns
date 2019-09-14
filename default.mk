export TOP := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
export PKG := $(lastword $(subst /, ,$(TOP)))

PKG_DESC   := Merchise extensions for Emacs
PKG_KWORDS := initialization convenience merchise

prefix   ?= /usr/local
sharedir ?= $(prefix)/share
lispdir  ?= $(sharedir)/emacs/site-lisp/$(PKG)
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/$(PKG)
statsdir ?= $(TOP)/docs/stats

CP    ?= install -p -m 644
MKDIR ?= install -p -m 755 -d
RMDIR ?= $(RM) -fr
TAR   ?= tar

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH)

INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO     ?= makeinfo
AWK          ?= $(shell command -v gawk || printf awk)

EMACS_VERSION = 26.1

# delete one of the next two definitions

_USER_EMACS_DIR ?= $(shell $(BATCH) --eval "(princ user-emacs-directory)")

USER_EMACS_DIR ?= $(HOME)/.emacs.d/


CHECK_EMACS := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(CHECK_EMACS)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

# function to normalize clean versions
nversion = $(shell echo "$(subst v,,$(1))" | $(AWK) -F. '{ printf("%d.%d.%d\n",$$1,$$2,$$3); }';)

# function to produce comparable versions
cversion = $(shell echo "$(1)" | $(AWK) -F. '{ printf("%03d%03d%03d\n",$$1,$$2,$$3); }';)


# functions to delete a file if exists with [y/N] prompt

answer_yn = $(findstring y,$(subst Y,y,\
  $(shell bash -c 'read -p "$(1) [y/N]: " var; echo $$var')))

prompt_delfile = $(and \
  $(wildcard $(1)),\
  $(call answer_yn,File \"$(1)\" is going to be deleted),\
  $(shell rm $(1)))


# constants

GIT_TAG := $(shell git describe --tags --abbrev=0 2> /dev/null)
LAST_RELEASE := $(call nversion,$(or $(GIT_TAG),0.1))
TAG_DATE := $(shell git log -1 --format=%cI $(GIT_TAG) 2> /dev/null | cut -c-16)
DATE_VERSION := $(or $(subst T,.,$(subst -,,$(subst :,,$(TAG_DATE)))),20010101)

CLAST_RELEASE := $(call cversion,$(or $(GIT_TAG),0.1))

DEPENDENCIES = \
	dash:2.16:20190424\
	f:0.20:20190109\
	s:1.12:20180406\
	use-package:2.4:20190405\
	bind-key:2.4:20180513\
	grizzl:0.1:20160818\
	projectile:2.1:20190509\
	fill-column-indicator:1.90:20171209\
	flycheck:32:20190503\
	lsp-mode:6.0:20190512\
	lsp-ui:6.0:20190512\
	company:0.9:20190430\
	company-lsp:2.1:20190505\
	yasnippet:0.13:20190502\
	pipenv:0.0.1:20190307\
	dash-functional:1.2:20180107\
	ht:2.3:20190404\
	pyvenv:1.20:20181228\
	spinner:1.7.3\
	tern:0.0.1:20181108\
	tern-auto-complete:0.0.1:20170521\
	markdown-mode:2.4:20190305\
	auto-complete:1.5:20170125\
	popup:0.5:20160709\
	string-utils:0.3:20140508\
	deft:0.8:20181226\
	realgud:1.4:20190504\
	load-relative:1.3:20170526\
	loc-changes:1.2:20160801\
	proof-general:20190618:4.5


pkg_name = $(firstword $(subst :, ,$(1)))

ifndef MELPA

VERSION ?= $(LAST_RELEASE)
pkg_version = $(word 2,$(subst :, ,$(1)))

else

VERSION = $(DATE_VERSION)
pkg_version = $(lastword $(subst :, ,$(1)))

endif

ifndef LOAD_PATH

ELPA_DIR ?= $(USER_EMACS_DIR)elpa/
EMACS_GITHUB_DIR ?= $(HOME)/softlib/emacs/

# final '/' listing ELPA_DIR is to avoid '<package>-<version>.signed' files
define dependency_dir
$(or
  $(shell ls -d $(ELPA_DIR)$(1)-[0-9]*/ 2> /dev/null | sort | tail -n 1),
  $(wildcard $(EMACS_GITHUB_DIR)$(1)))
endef

LOAD_PATH = -L $(TOP)/horns $(foreach pkg,$(DEPENDENCIES),\
	-L $(call dependency_dir,$(call pkg_name,$(pkg))))

endif # LOAD_PATH
