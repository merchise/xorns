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

EMACS ?= emacs
# BATCH = $(EMACS) -Q --batch $(LOAD_PATH)
BATCH = $(EMACS) -Q --batch

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
  use-package:2.4:20190405\


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
