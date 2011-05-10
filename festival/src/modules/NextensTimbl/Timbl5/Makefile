MAKEFLAGS=--no-print-directory
CFLAGS=  -W -g -Wall -Wpointer-arith \
	-Wcast-qual -Wwrite-strings -Wconversion \
	-pedantic -ansi -ffloat-store -Woverloaded-virtual # -Weffc++
PFLAGS = -O3
CXX = g++
#CXX = CC

MAKE_SERVER = YES

ifeq (0,${MAKELEVEL})

BIN_DIR = .

O_SYSTEM = $(shell uname)

ifeq '$(O_SYSTEM)' "SunOS"
  ifeq "i386" '$(MACHTYPE)'
    BIN_DIR := X86
  else	
    BIN_DIR := SPARC
  endif	
else
  ifeq '$(O_SYSTEM)' "Linux"
	BIN_DIR = .
  else
    ifeq '$(O_SYSTEM)' "Darwin"
	BIN_DIR = .
    else
      ifeq '$(O_SYSTEM)' "OSF1"
	BIN_DIR = .
      else
        ifeq '$(O_SYSTEM)' "IRIX64"
	  BIN_DIR = .
        else
          O_SYSTEM = UNKNOWN
	  BIN_DIR = .
        endif
      endif
    endif
  endif
endif

export BIN_DIR
export O_SYSTEM

endif

ifeq '$(MAKE_SERVER)' "YES"
MSRCS   = Timbl.cxx TimblClient.cxx

SRCS	= Instance.cxx IBtree.cxx MsgClass.cxx \
	MBLClass.cxx TimblExperiment.cxx Types.cxx Common.cxx \
	Tree.cxx GetOptClass.cxx SocketBasics.cxx \
	ServerProcs.cxx LogStream.cxx LogBuffer.cxx TimblAPI.cxx

OBJS	= $(patsubst %.cxx, $(BIN_DIR)/%.o, $(SRCS) )
EXES	= $(patsubst %.cxx, $(BIN_DIR)/%, $(MSRCS) )

ifeq '$(O_SYSTEM)' "SunOS"
  TH_LINK = -lpthread -lsocket -lnsl
  CFLAGS := $(CFLAGS) -DPTHREADS -DUSE_LOGSTREAMS
else
  ifeq '$(O_SYSTEM)' "OSF1"
    TH_LINK = -lpthread -lexc
    CFLAGS := $(CFLAGS) -DPTHREADS -D_REENTRANT -DUSE_LOGSTREAMS
  else
    ifeq '$(O_SYSTEM)' "Darwin"
      TH_LINK = -lpthread
      CFLAGS := $(CFLAGS) -D PTHREADS -D_BSD_SOCKLEN_T_=int -DUSE_LOGSTREAMS
    else
      ifeq '$(O_SYSTEM)' "IRIX64"
	ifeq '$(CXX)' "g++"
	  TH_LINK =
	  CFLAGS := $(CFLAGS) -mabi=64 -mips4
	  LDFLAGS := $(LDFLAGS) -mabi=64 -mips4
	else
          TH_LINK = 
	  PFLAGS = "-O2 -OPT:Olimit=0"
	  CFLAGS = -LANG:std -DIRIX64 -64
	  LDFLAGS := $(LDFLAGS) -64 -v
	endif
      else
        TH_LINK = -lpthread
        CFLAGS := $(CFLAGS) -DPTHREADS -DUSE_LOGSTREAMS -D_ISOC99_SOURCE
      endif
    endif
  endif
endif

else
MSRCS   = Timbl.cxx TimblClient.cxx 

SRCS	= Instance.cxx IBtree.cxx MsgClass.cxx \
	MBLClass.cxx TimblExperiment.cxx Types.cxx Common.cxx \
	Tree.cxx GetOptClass.cxx ServerProcs.cxx LogStream.cxx LogBuffer.cxx \
	TimblAPI.cxx 
OBJS	= $(patsubst %.cxx, $(BIN_DIR)/%.o, $(SRCS) )
EXES	= $(patsubst %.cxx, $(BIN_DIR)/%, $(MSRCS) )

TH_LINK =

endif

all: $(BIN_DIR)  version
	@echo "Making on $(O_SYSTEM) in directory $(BIN_DIR)"; 
	$(MAKE) CXXFLAGS='$(CFLAGS)' PROF_FLAGS=$(PFLAGS) $(EXES)

profile: $(BIN_DIR)
	$(MAKE) PFLAGS='-pg' all

pure: $(BIN_DIR)
	-@if [ $(MACHTYPE) = sparc ]; then (\
	echo "Making on Sparc"; \
	$(MAKE) PURE='purify' PROF_FLAGS=-g $(EXES) \
	   CXXFLAGS='$(CFLAGS)'; \
	) else ( \
	echo "Purify bestaat niet voor $(O_SYSTEM)/X86"; ) fi;

clean:
	-rm $(EXES)
	-rm $(BIN_DIR)/*.o
	-rm $(BIN_DIR)/*.a $(BIN_DIR)/*.d
	-rm $(BIN_DIR)/ii_files/*

H_FILES = $(patsubst %.cxx, %.h, $(SRCS))

ifneq (0,${MAKELEVEL})
D_FILES = $(patsubst %.cxx, $(BIN_DIR)/%.d, $(MSRCS))
D_FILES1 = $(patsubst %.cxx, $(BIN_DIR)/%.d, $(SRCS))

	include $(D_FILES)
ifneq  "" '${D_FILES1}'
	include $(D_FILES1)
endif
endif

$(BIN_DIR): 
	mkdir $(BIN_DIR)

$(BIN_DIR)/%.d: %.cxx $(H_FILES)
	$(SHELL) -ec 'g++ -MM $< | \
	sed '\''s?$*.o?$(BIN_DIR)/$*.o $@?g'\''' > $@

$(BIN_DIR)/%.o: %.cxx
	$(CXX) -c $(CXXFLAGS) $(PROF_FLAGS) $< -o $@

$(BIN_DIR)/libTimbl.a: $(OBJS)
	$(AR) ruv $@ $^
ifneq '$(O_SYSTEM)' "IRIX64"
	ranlib $@
endif

$(BIN_DIR)/dbg_lib.a: $(OBJS)
	$(AR) ruv $@ $^
ifneq '$(O_SYSTEM)' "IRIX64"
	ranlib $@
endif

version: $(SRCS) $(H_FILES)
	touch Common.cxx
	touch version

ifeq '$(PURE)' ""
  ifeq '$(O_SYSTEM)' "IRIX64"
    ifeq '$(CXX)' "CC"
$(EXES): $(BIN_DIR)/%: $(BIN_DIR)/%.o $(OBJS)
	$(CXX) $(LDFLAGS) $(SC_LINK) $(TH_LINK) $(PROF_FLAGS) $^ -64 -lm -lC -lCio -o $@
    else
$(EXES): $(BIN_DIR)/%: $(BIN_DIR)/%.o $(BIN_DIR)/libTimbl.a
	$(CXX) $(LDFLAGS) $(SC_LINK) $(TH_LINK) $(PROF_FLAGS) $^ -o $@
    endif
  else
$(EXES): $(BIN_DIR)/%: $(BIN_DIR)/%.o $(BIN_DIR)/libTimbl.a
	$(CXX) $(LDFLAGS) $(SC_LINK) $(TH_LINK) $(PROF_FLAGS) $^ -o $@
  endif
else
$(EXES): $(BIN_DIR)/%: $(BIN_DIR)/%.o $(BIN_DIR)/dbg_lib.a
	$(PURE) $(CXX) $(LDFLAGS) $(SC_LINK) $(TH_LINK) $(PROF_FLAGS) $^ -o $@
endif

