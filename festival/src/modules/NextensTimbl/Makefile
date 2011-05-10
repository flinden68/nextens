# Makefile for NextensTimbl
#
# Check in this Makefile that TIMBLDIR is correct.
#
# Check in the Timbl Makefile that MAKESERVER = NO
#
# Make sure that festival/config/config contains 
# COMPILERLIBS += -L $(TOP)/src/modules/NextensTimbl/Timbl5 -lTimbl
# 
# $Id: Makefile,v 1.3 2005/10/04 01:05:49 emarsi Exp $

TOP=../../..
DIRNAME=src/modules/NextensTimbl

TIMBLDIR = Timbl5

#export BIN_DIR = .
#export O_SYSTEM = '$(shell uname)'

SRCS = TimblInit.cc TimblMethods.cc

OBJS = $(SRCS:.cc=.o)

FILES = Makefile $(SRCS)

LOCAL_CLEAN = 
#LOCAL_INCLUDES = -I.

INLIB = $(TOP)/src/lib/libFestival.a

ALL = makeTimbl .buildlib

include $(TOP)/config/common_make_rules

makeTimbl:
	cd $(TIMBLDIR) && make MAKELEVEL=0 MAKE_SERVER=NO

# FIXME: clean does not clean the files in the Timbl dir 

