TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = System System/Posix
PACKAGE      = unix
PACKAGE_DEPS = base

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (unix package)"

include $(TOP)/mk/target.mk
