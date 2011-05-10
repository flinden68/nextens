# The sendpraat.cc source has been adapted for usage on cygwin
#
#		#include <ctype.h>
#
# in the include block at the beginning of sendpraat.cc
#
# further some typecasts (Display *) where added because the compiler complained
#
# Do not forget to add NextensPraat to the festival/config/config file.
# 
# And the following code must be added to th efestival/config/config
#
# ifdef DISPLAY
#	COMPILERLIBS += -L/usr/X11R6/lib -lX11
# endif

TOP=../../..
DIRNAME=src/modules/NextensPraat

SRCS = sendpraat.cc PraatInit.cc

OBJS = $(SRCS:.cc=.o)

FILES = Makefile $(SRCS)	

LOCAL_INCLUDES = -I.


ifeq '$(shell uname -s)' "Linux"
LOCAL_INCLUDES += -std=gnu99 \
                  -DUNIX \
                  $(shell pkg-config --cflags --libs gtk+-2.0)
endif


ifeq '$(shell uname -s)' "Darwin"
LOCAL_INCLUDES += -framework CoreServices \
                  -framework ApplicationServices \
                  -I/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/AE.framework/Versions/A/Headers \
                  -I/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/CarbonCore.framework/Versions/A/Headers 
endif 

INLIB = $(TOP)/src/lib/libFestival.a

ALL = .buildlib

include $(TOP)/config/common_make_rules
