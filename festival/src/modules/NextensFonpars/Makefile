TOP=../../..
DIRNAME=src/modules/NextensFonpars


SRCS = fonpInit.cc gfc.cc fonp.cc g0apho.cc g0apho1.cc g0apho2.cc \
       g0apho3.cc g0apho4.cc g0apho5.cc  g1apho.cc g1apho1.cc \
       grafeat.cc  grapho.cc  p0ldp.cc \
       p0ldp1.cc  p0ldp2.cc  ppldp.cc
             
OBJS = $(SRCS:.cc=.o)

LOCAL_CLEAN = $(OBJS)

FILES = Makefile $(SRCS)

LOCAL_INCLUDES = -I.

INLIB = $(TOP)/src/lib/libFestival.a

ALL = .buildlib
	
include $(TOP)/config/common_make_rules



