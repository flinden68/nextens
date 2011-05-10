/* STD.H */
/*------------------------------------------------------------------------

  FUNCTION: Includefile in all Wendy-main or Wendy-tools related C-sources.
	    Contains hardware and link configuration, compiler dependent
	    scalar types, I/O buffer sizes, macro's, and some general
	    functional service typedefs.

-------------------------------------------------------------------------*/
/*-- SYSTEM ENVIRONMENT FLAG --------------------------------------------*/
/*
  Before a program using TLS is compiled, you'll first decide
  where to run it:
*/

#define PC
/*#define VAXVMS */

#ifdef PC
#define SUN
#endif

/*
  Following TLS units depend on this #define:

  TTY ... interpretation of '\n', keyboard translation
  OST ... operating system dep. actions: spawn, yack, existfile etc
  TRC ... format translations %ld->%d in case of 32bit VAX
  STR ... some tools missing in VAX-stringlib, vsprintf float fix
  FIL ... for VAXVMS, use recordformat in binary file-create
  SCR ... when VAXVMS disable DIRECT, use TTY terminal I/O instead
  CLN ... interpretation of '\n', kind of (graphics) screen restores
  GDD ... decides which graphics are dummied out (..by default)
  WLP ... in case PC: use library numeric constant evaluation
  FFT ... in case PC: configure DSP resources when available

When the system is moved to yet another type of hardware,
hacks might be needed to make above TLS units work properly.
The above list contains the names of *all* system dependent
compiles. The rest of Wendy contains no conditional compiles.
Remember to link in a proper GDD server before doing any graphics.
For graphics maintenance and expansion: see GDD documentation.
*/

/*-- WENDY TOOLS LIBRARY CONFIGURATION ---------------------------------*/

/*
  tells FFT (or any other) unit to expect slow hardware
*/
/* #define SLOW */

/*
  tells CLN unit to assume dualmode capability
*/
#define DUALMODE /* */

/*
  tells SCR unit to echo directly into screen memory
*/
#define DIRECT /* */

#ifndef DIRECT
/*
   tells SCR unit to echo to stdout terminal
*/
/* #define STDOUT */

/*
   tells SCR unit to echo to stderr terminal
*/
/* #define STDERR */

#endif

/*
  tells USR unit to do "true blue" scrollregion behaviour
*/
#define OLDANSI /* */

/*
  tells any source that Wendy language is available
*/
/* #define WENLINK */ /* */

/*
  tells any source that (NO!! srd,trac, etc) graphics is available
*/
#define GRALINK /* */

/*
  tells any source that SCR is available
*/
#define SCRLINK /* */

/*
  signifies that Martin Sinot's SWAP.OBJ is available
*/
#define SWAP /* */

/*
  signifies that the SAM unit is available
*/
#define SAMLINK /* */

/*--- FORCED CHANGES ----------------------------------------------------*/
/*
  Correct common PC/DSP behaviour: dualmode capability switched off on PC
*/

#ifdef PC
#ifndef SUN
#undef DUALMODE       /* assume lack dualmode capability on true PC's    */
#endif
#endif

/*-- EVERLASTING IMMORTALS ----------------------------------------------*/

#define ERROR   0
#define SUCCESS 1
#define CR      13
#define PI      3.141592654

typedef long  int4;
typedef short int2;

#define testbit(n,b)   ((n&b)==(b))
#define setbit(n,b)    ((n)|=(b))
#define setnobit(n,b)  ((n)&=(~(b)))

/*-- BIN MAXIMAL SCOPE --------------------------------------------------*/
/*
  Below define decides about:

     * the maximum number of ports in Wendy

     * the maximum scope depth of Wendy language variables

     * (anything) the maximum scope of BIN.C set_scope()
*/

#define MAXPORTS 16

/*-- STANDARD LIBRARY RELATED MACRO'S -----------------------------------*/
/*
  Wendy makes use of stdlib heap calls. These macro's provide some safety.
*/

void free(void *p);
void *malloc(unsigned n);

#define free_buffer(p)       if ((*p)!=NULL) { free(*p); *p=NULL; }
#define make_buffer(p,es,ns) if ((*p)==NULL) (*p)=malloc(es*ns);

/*-- CONFIGURE FLOATING POINT PRECISION ---------------------------------*/

#ifdef VAXVMS
#define lfmt "%d"
typedef double real;
#else
#define lfmt "%ld"
typedef float real;
#endif

/*-- WENDY TOOLS RELATED DEFAULT SETTINGS (all systems) -----------------*/

#define linelength 255  /* TXT,HLP: maximum line expected in textfiles   */
#define namelength 64   /* BIN: maximum length of any name               */

#define scrh       25L  /* SCR,CLN: (default) number of screen lines     */
#define scrw       80L  /* SCR,CLN: (fixed) number of chars per line     */

#define empty_attr 7    /* SCR: empty screenspace.. soft attribute       */
#define empty_char ' '  /* SCR: empty screenspace.. space character      */

#define NRECALL    32   /* CLN number of recall buffer lines             */

#define CLEARSTART 0    /* CLN behaviour setting: clear scr at startup   */
#define CLEAREND   0    /* CLN behaviour setting: clear scr at exit      */

/*-- WENDY RELATED DEFAULT SETTINGS (WENDY LANGUAGE SYSTEMS ONLY) --------*/

#ifdef WENLINK

/*-- WENDY LANGUAGE LINK CONFIGURATION -----------------------------------*/

#define MAXUNITS 15   /* Max number of compiled language CM?? units (any) */

/*
  add reps+ports:  ,CM1,CM2,CM4,CM5,LNG
*/
#define WENCOM /* */
/*
  FFT,buttons etc: ,CM3,CM6,CM8, and higher
*/
#define WENEXT /* */

/*
  WENDY MAIN: spawn unknown commands
*/
/* #define SPAWN */

#define NUMPORTS   10   /* POR: number port records allocated at startup */
#define MINPORTH   16   /* WEN: minimal device portheight accepted       */
#define MAXPORTH   300  /* WEN: maximal device portheight accepted       */

#define STARTUP  "@INIWENDY."     /* MAIN: file executed system startup   */
#define PROMPT   "WENDY>"        /* MAIN: prompt string 		 */
#define EXTWEN   ".WEN"          /* MAIN: assumed extension commandfiles */

/*--- SOME WENDY LANGUAGE CONFIGURATIONS ---------------------------------*/

#define fbufsize   10000   /* max total size of a user procedure          */

#define QCASE      1   /* by default, case-sensitivity for Q's and labs   */

#define parsofs    '1' /* start index of $?? and $.?? arguments in WLP    */
#define resiofs    '0' /* start index of .:?? reserved and .[??] arrays   */

#define dels      '"'  /* assumed string delimiter                        */

#define MAXARGS  10    /* maximum number of arguments passed to any WLP   */

/*-- WENDY FUNCTION PROTOTYPES ------------------------------------------*/
/*
  Function adresses that are passed to the TLS and PLT layers.
*/

typedef int4 ts_cvtf(int4 n);
/*
  General longint conversion function type (WLP: satot,titos)
*/

typedef int2 endcond(int2 index, int4 pos);  /* general leave-at service */
/*
  General endcondition when scanning through data (REP plots, DMP dumps)
*/

/*--------------------------------------- end of Wendy dependent code ---*/
#endif

extern char strt[];
int tmes(int,char *);

/**** IBM SCREEN SPECIAL CHARACTERS *******/

#define leftupper   218
#define rightupper  191
#define straighthor 196
#define straightver 179
#define leftdown    192
#define rightdown   217
#define singledot   250

/**** KEYBOARD SPECIAL CHARACTERS *****/

#define CR  13
#define LF  10
#define DEL 127
#define TAB 9
#define BS   8
#define ESC  27
#define BSP 127

/* define cursorpad scans  */

#define CUP  'H'<<8  /* cursor     */
#define CDW  'P'<<8
#define CRG  'M'<<8
#define CLF  'K'<<8

#define CCRG 't'<<8  /* Ctrl curs  */
#define CCLF 's'<<8

#define PGU  'I'<<8  /* pages      */
#define PGD  'Q'<<8
#define HOM  'G'<<8
#define END  'O'<<8

#define CPGU '„'<<8 /* Ctrl pages */
#define CPGD 'v'<<8
#define CHOM 'w'<<8
#define CEND 'u'<<8

/* define edit control */

#define RUB 'S'<<8
#define INS 'R'<<8

