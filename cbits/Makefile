TOP = ../..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)

SRC_CC_OPTS += -Wall
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) -I../include

LIBRARY = libHSunix_cbits.a
LIBOBJS = $(C_OBJS)

include $(TOP)/mk/target.mk
