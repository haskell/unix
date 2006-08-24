TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = cbits include

ALL_DIRS     = System System/Posix System/Posix/DynamicLinker System/Posix/Signals
PACKAGE      = unix
VERSION	     = 1.0
PACKAGE_DEPS = base

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"
SRC_HSC2HS_OPTS += -Iinclude -I../../mk/ $(unix_SRC_HSC2HS_OPTS)
SRC_HC_OPTS     += -Iinclude $(unix_SRC_HSC2HS_OPTS)

EXCLUDED_SRCS += Setup.hs

DIST_CLEAN_FILES += unix.buildinfo config.cache config.status 

include $(TOP)/mk/target.mk
