MAKE_SERVER = YES

ifeq (0,${MAKELEVEL})

ifeq (,$(TIMBLPATH))
TIMBLPATH=../Timbl5
endif

O_SYSTEM := $(shell uname)

ifeq '$(O_SYSTEM)' "SunOS"
  ifeq "i386" '$(MACHTYPE)'
	BIN_DIR = X86
  else	
	BIN_DIR = SPARC
  endif	
else
  ifeq '$(O_SYSTEM)' "Darwin"
	BIN_DIR = .
  else
    ifeq '$(O_SYSTEM)' "Linux"
	BIN_DIR = .
    else
      ifeq '$(O_SYSTEM)' "OSF1"
	BIN_DIR = .
      else
        ifeq '$(O_SYSTEM)' "IRIX"
	  BIN_DIR = .
	else
          ifeq '$(O_SYSTEM)' "IRIX64"
	    BIN_DIR = .
          else
	    BIN_DIR = .
	    O_SYSTEM = UNKNOWN
          endif
        endif
      endif
    endif
  endif
endif

TIMBLLIB := $(shell find $(TIMBLPATH)/$(BIN_DIR) -name libTimbl.a)
PTIMBLLIB := $(shell find $(TIMBLPATH)/$(BIN_DIR) -name dbg_lib.a)

export TIMBLLIB
export PTIMBLLIB
export BIN_DIR
export O_SYSTEM
endif

ifeq '$(TIMBLLIB)' ""
  ifneq '$(PTIMBLLIB)' ""
     LIB_OK = yes
  endif
else
  LIB_OK = yes
endif

all: all2

ifneq '$(LIB_OK)' "yes"
all1:
	@echo problem finding Timbl Library. Did you set TIMBLPATH correctly?
	@echo Current value = $(TIMBLPATH)
else

ifeq '$(O_SYSTEM)' "UNKNOWN"
all1:
	@echo I am unsure about your OS. 
	@echo Building Mbt with reasonable defaults. I hope...
endif

MAKEFLAGS=--no-print-directory
#CFLAGS= -Wall -I ${TIMBLPATH}
CFLAGS=  -I ${TIMBLPATH} -O2 -g -W -Wall -Wpointer-arith \
	-Wcast-qual -Wcast-align -Wwrite-strings -Wconversion \
	-Woverloaded-virtual -pedantic -ansi

PCCFLAGS=  -I ${TIMBLPATH}
CPPFLAGS=  -I ${TIMBLPATH}

CC=g++


TSRCS	= Mbtg.cxx Mbt.cxx demo.cxx
SRCS	= Pattern.cxx Sentence.cxx TagLex.cxx Tagger.cxx MbtAPI.cxx

OBJS    = $(patsubst %.cxx, $(BIN_DIR)/%.o, $(SRCS) )
DBG_OBJS = $(patsubst %.cxx, $(BIN_DIR)/%.d.o, $(SRCS) )
TEXES    = $(patsubst %.cxx, $(BIN_DIR)/%, $(TSRCS) )
PTEXES   = $(patsubst %.cxx, $(BIN_DIR)/%p, $(TSRCS) )

H_FILES = $(patsubst %.cxx, %.h, $(SRCS))
D_FILES1 = $(patsubst %.cxx, $(BIN_DIR)/%.d, $(TSRCS))
D_FILES2 = $(patsubst %.cxx, $(BIN_DIR)/%.d, $(SRCS))

ifeq '$(MAKE_SERVER)' "YES"

ifeq '$(O_SYSTEM)' "SunOS"
  TH_LINK = -lpthread -lsocket -lnsl
  CFLAGS := $(CFLAGS) -D PTHREADS
else
  ifeq '$(O_SYSTEM)' "OSF1"
    TH_LINK = -lpthread -lexc
    CFLAGS := $(CFLAGS) -D PTHREADS -D _REENTRANT
  else
    ifeq '$(O_SYSTEM)' "Darwin"
      TH_LINK = -lpthread
      CFLAGS := $(CFLAGS) -D PTHREADS -D_BSD_SOCKLEN_T_=int
    else
      ifeq '$(O_SYSTEM)' "IRIX64"
        TH_LINK =
        CFLAGS := $(CFLAGS) -mips4 -mabi=64
        LDFLAGS := $(LDFLAGS) -mips4 -mabi=64
      else # Linux, IRIX and unknown OS
        TH_LINK = -lpthread
        CFLAGS := $(CFLAGS) -D PTHREADS
      endif
    endif
  endif
endif

else
TH_LINK =
endif

ifneq (0,${MAKELEVEL})
	include $(D_FILES1)
	include $(D_FILES2)
endif

all2: $(BIN_DIR)
	@echo "Making on $(O_SYSTEM) in directory $(BIN_DIR)"
	@echo "Using Timbl library = " $(TIMBLLIB)
	$(MAKE) PROF_FLAGS=-g CXXFLAGS='$(CFLAGS)' $(TEXES) 

pure: $(BIN_DIR)
	-@if [ $(MACHTYPE) = sparc ]; then (\
	echo "Make a Purified version"; \
	$(MAKE) CXXFLAGS='$(PCCFLAGS) $(CFLAGS)'  PURE='purify' \
		PROF_FLAGS=-g $(PTEXES); \
	 ) else ( \
	echo "Purify not possible for $(O_SYSTEM)/X86"; ) fi;

clean:
	@rm -f $(TEXES) $(PTEXES)
	@rm -f $(BIN_DIR)/*.o
	@rm -f $(BIN_DIR)/*.d
	@rm -f $(BIN_DIR)/*.a

$(BIN_DIR):
	mkdir $(BIN_DIR)

$(BIN_DIR)/%.d: %.cxx $(H_FILES)
	$(SHELL) -ec '$(CXX) -MM $(CXXFLAGS) $< | \
	sed '\''s?$*.o?$(BIN_DIR)/$*.o $(BIN_DIR)/$*.d.o $@?g'\''' > $@

$(BIN_DIR)/%.o: %.cxx
	$(CXX) -c $(CXXFLAGS) $(PROF_FLAGS) $< -o $@

$(BIN_DIR)/%.d.o: %.cxx
	$(CXX) -c $(CXXFLAGS) $(PROF_FLAGS) $< -o $@

$(BIN_DIR)/%p.d.o: %.cxx
	$(CXX) -c $(CXXFLAGS) $(PROF_FLAGS) $< -o $@

$(BIN_DIR)/libMbt.a: $(OBJS)
	ar ruv $@ $^
ifneq '$(O_SYSTEM)' "IRIX64"
	ranlib $@
endif

$(BIN_DIR)/dbg_lib.a: $(DBG_OBJS)
	ar ruv $@ $^
ifneq '$(O_SYSTEM)' "IRIX64"
	ranlib $@
endif

$(TEXES): $(BIN_DIR)/%: $(BIN_DIR)/%.o $(BIN_DIR)/libMbt.a $(TIMBLLIB)
	$(PURE) $(CXX) $(LDFLAGS) $(TH_LINK) $(PROF_FLAGS) $^ -o $@

$(PTEXES): $(BIN_DIR)/%: $(BIN_DIR)/%.d.o $(BIN_DIR)/dbg_lib.a $(PTIMBLLIB)
	$(PURE) $(CXX) $(LDFLAGS) $(TH_LINK) $(PROF_FLAGS) $^ -o $@

endif
