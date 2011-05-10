MAKEFLAGS=--no-print-directory
CFLAGS=  -g -W -Wall -Wpointer-arith \
	-Wcast-qual -Wwrite-strings -Wconversion \
	-Wstrict-prototypes -Wmissing-prototypes \
	-pedantic -ansi -ffloat-store -Woverloaded-virtual 

MAKE_SERVER = yes

ifneq '$(PURE)' "" 
	CFLAGS := $(CFLAGS) -g 
endif

ifeq (0,${MAKELEVEL})

BIN_DIR = .
TIMBL_LIB_PATH = ..
TIMBL_INC_PATH = ..

ifeq '$(OSTYPE)' "solaris"
  O_SYSTEM = SOLARIS
  ifeq "i386" '$(MACHTYPE)'
    BIN_DIR := X86
    TIMBL_LIB_PATH := ../X86
  else	
    BIN_DIR := SPARC
   TIMBL_LIB_PATH := ../SPARC
  endif	
else
  ifeq '$(OSTYPE)' "linux"
    O_SYSTEM = LINUX
  else
    ifeq '$(OSTYPE)' "Linux"
      O_SYSTEM = LINUX
    else
      ifeq '$(OSTYPE)' "linux-gnu"
        O_SYSTEM = LINUX
      else
        ifeq '$(OSTYPE)' "mklinux"
          O_SYSTEM = LINUX
        else
          ifeq '$(OSTYPE)' "aix"
            O_SYSTEM = AIX
	  else 
            ifeq '$(OSTYPE)' "darwin"
              O_SYSTEM = DARWIN
	    else 
	      ifeq '$(shell uname)' "OSF1"
                O_SYSTEM = OSF
	      else
                O_SYSTEM = '$(shell uname)'"-UNKNOWN"
              endif
            endif
          endif
        endif
      endif
    endif
  endif
endif

export O_SYSTEM
export BIN_DIR
export TIMBL_LIB_PATH
export TIMBL_INC_PATH

endif

MSRCS   = tse.cxx classify.cxx api_test.cxx api_test2.cxx api_test3.cxx

EXES	= $(patsubst %.cxx, $(BIN_DIR)/%, $(MSRCS) )

CFLAGS := $(CFLAGS) -I$(TIMBL_INC_PATH)

ifeq '$(MAKE_SERVER)' "yes"

ifeq '$(O_SYSTEM)' "SOLARIS"
  TH_LINK = -lpthread -lsocket -lnsl
  CFLAGS := $(CFLAGS) -O2 -D PTHREADS
else
  ifeq '$(O_SYSTEM)' "OSF"
    TH_LINK = -lpthread -lexc
    CFLAGS := $(CFLAGS) -O2 -D PTHREADS -D _REENTRANT
  else
    ifeq '$(O_SYSTEM)' "DARWIN"
      TH_LINK = -lpthread -lexc
      CFLAGS := $(CFLAGS) -O2 -D PTHREADS -D _REENTRANT
    else
      TH_LINK = -lpthread
      CFLAGS := $(CFLAGS) -O2 -D PTHREADS
    endif
  endif
endif

else

  TH_LINK = 
  CFLAGS := $(CFLAGS) -O2 
endif

all: $(BIN_DIR)
	@echo "Making on $(O_SYSTEM) in $(BIN_DIR)"; 
	$(MAKE) CXXFLAGS='$(CFLAGS)' $(EXES)

pure: $(BIN_DIR)
	-@if [ $(MACHTYPE) = sparc ]; then (\
	echo "Making on Sparc"; \
	$(MAKE) PURE='purify' PROF_FLAGS=-g $(EXES) \
	   CXXFLAGS='$(CFLAGS)'; \
	) else ( \
	echo "Purify bestaat niet voor $(O_SYSTEM)"; ) fi;

clean:
	-rm $(EXES)
	-rm $(BIN_DIR)/*.o
	-rm $(BIN_DIR)/*.d

H_FILES = $(patsubst %.cxx, %.h, $(SRCS))

ifneq (0,${MAKELEVEL})
D_FILES = $(patsubst %.cxx, $(BIN_DIR)/%.d, $(MSRCS))
-include $(D_FILES)
endif

$(BIN_DIR): 
	mkdir $(BIN_DIR)

$(BIN_DIR)/%.d: %.cxx $(H_FILES)
	$(SHELL) -ec '$(CXX) -MM $(CXXFLAGS) $< | \
	sed '\''s?$*.o?$(BIN_DIR)/$*.o $@?g'\''' > $@

$(BIN_DIR)/%.o: %.cxx
	$(CXX) -c $(CXXFLAGS) $(PROF_FLAGS) $< -o $@

ifeq '$(PURE)' ""

$(EXES): $(BIN_DIR)/%: $(BIN_DIR)/%.o $(TIMBL_LIB_PATH)/libTimbl.a
	$(CXX) $(LDFLAGS) $(SC_LINK) $(TH_LINK) $(PROF_FLAGS) $^ -o $@
else
$(EXES): $(BIN_DIR)/%: $(BIN_DIR)/%.o $(TIMBL_LIB_PATH)/dbg_lib.a
	$(PURE) $(CXX) $(LDFLAGS) $(SC_LINK) $(TH_LINK) $(PROF_FLAGS) $^ -o $@
endif

