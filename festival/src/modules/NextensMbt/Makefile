# Makefile for NextensMbt
#
# Check in this Makefile that MBTDIR and TIMBLDIR are correct.
#
# Check in the Mbt Makefile that TIMBLDIR is correct, 
# and MAKESERVER = NO
#
# Make sure that festival/config/config contains 
# COMPILERLIBS += -L $(TOP)/src/modules/NextensMbt/Mbt2 -lMbt
# 
# $Id: Makefile,v 1.3 2005/10/04 01:10:52 emarsi Exp $


TOP=../../..
DIRNAME=src/modules/NextensMbt

MBTDIR = Mbt2
TIMBLDIR = ../NextensTimbl/Timbl5

SRCS = MbtInit.cc MbtMethods.cc

OBJS = $(SRCS:.cc=.o)

FILES = Makefile $(SRCS)

LOCAL_INCLUDES = -I. -I$(TIMBLDIR) -I$(MBTDIR)

INLIB = $(TOP)/src/lib/libFestival.a

ALL = make_mbt .buildlib

include $(TOP)/config/common_make_rules

# MAKELEVEL must be set to zero,
# otherwise the first part of the Mbt Makefile is skipped

make_mbt:
	cd $(MBTDIR) && make MAKELEVEL=0 MAKE_SERVER=NO TIMBLPATH=../$(TIMBLDIR)

# FIXME: clean does not clean the files in the Mbtw dir 
