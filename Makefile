TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = cbits include

ALL_DIRS     = System System/Posix System/Posix/DynamicLinker
PACKAGE      = unix
PACKAGE_DEPS = base

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"
SRC_HSC2HS_OPTS += -Iinclude -I../../mk/ $(unix_SRC_HSC2HS_OPTS)
SRC_HC_OPTS     += -Iinclude $(unix_SRC_HSC2HS_OPTS)

include $(TOP)/mk/target.mk
