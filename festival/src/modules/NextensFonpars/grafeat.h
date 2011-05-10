/*----------------------------------------------------------------------

FUNCTION: Featuretable interface for grafeat

----------------------------------------------------------------------*/

#define gMAXKLANK 128
typedef char gverzklank[gMAXKLANK];

/*--- PROTOTYPES -------------------------------------------------*/

void gpreparesets(void);

/*--- INTERFACE TO DATA ---------------------------------------------------*/

#define inset(c,s) ((s)[(unsigned char)c])

#define TRUE (1)
#define FALSE (0)
#define true 1
#define false 0

extern short gj;
extern short gstartverw;
extern short gbeginverw;
extern short geindverw;
extern short gtraceact;
extern char *ginpregel;
extern short ginplgt;

extern char *gch;

/*--- INPUTSTRING ACCESSING MACROS ----------------------------------------*/

#ifndef DFMACROS
#define DFMACROS
#define initj gch = &(ginpregel[gj=gbeginverw])
#define gotoj(ofs) gj +=ofs; gch += ofs
#define forwj ++gch; ++gj;
#define backj --gch; --gj;
#define nextj (gj <= geindverw)
#define strnj (gj <  ginplgt)
#define leftj ((gj + gy) >= (gbeginverw-1))
#define rghtj ((gj + gx) <= ginplgt)
#define offch(ofs) gch[ofs]
#define prech (*(gch-1))
#define curch (*gch)
#define nexch (*(gch+1))
#endif

/*--- FONPARS SPECIFIC RULE SYNTAX SUPPORT --------------------------------*/

extern char gA,gB,gC,gD,gE,gF,gG;
extern short gx,gy,gk;
extern char gstop,gverhoogd,ggevonden;

extern char *gAverz  	;
extern char *gBverz  	;
extern char *gCverz  	;

extern gverzklank gOverz  	; /* occasionally filled (GRAFON 217) */
/*-----------------------------------------------------------------------*/

/* Control features */
extern gverzklank ginform;
extern gverzklank gklank;
extern gverzklank gklanken;


extern gverzklank gklank;
extern gverzklank ginform;
extern gverzklank gleesteken;
extern gverzklank gprom;
extern gverzklank gphoofd;
extern gverzklank gmhoofd;
extern gverzklank gpcijf;
extern gverzklank gpword;
extern gverzklank gpcons;
extern gverzklank gmcons;
extern gverzklank gpvoc;
extern gverzklank gmvoc;
extern gverzklank gpson;
extern gverzklank gmson;
extern gverzklank gphoog;
extern gverzklank gmhoog;
extern gverzklank gplaag;
extern gverzklank gmlaag;
extern gverzklank gpachter;
extern gverzklank gmachter;
extern gverzklank gprond;
extern gverzklank gmrond;
extern gverzklank gplang;
extern gverzklank gmlang;
extern gverzklank gpcont;
extern gverzklank gmcont;
extern gverzklank gpant;
extern gverzklank gmant;
extern gverzklank gpcor;
extern gverzklank gmcor;
extern gverzklank gpnas;
extern gverzklank gmnas;
extern gverzklank gpstrid;
extern gverzklank gmstrid;
extern gverzklank gpstem;
extern gverzklank gmstem;
extern gverzklank gpseg;
extern gverzklank gmseg;
extern gverzklank gpklem;
extern gverzklank gmklem;

extern short *gbuffer[];
char gvgl(short index, char left, char *context);
long gperc(long n, short perc);
long gcopy(short ofs, short kind);
void ginit_venster(void);
void gplace_venster(void);
void gtracetask(char *ruleset, int rule_number, int task);
void ghoofdklein(short ccnt, short k);
void gcorlengt(short aantal);
short gsegm(short par, short plts);


void gverander(short ccnt, short kind, gverzklank *changefeat);
extern gverzklank gnonseg;
/*-----------------------------------------------------------------------*/

