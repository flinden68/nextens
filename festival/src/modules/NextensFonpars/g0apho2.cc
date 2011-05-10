#define NULL 0L

extern char rulefile_g0apho[];
#define gletter(c,i) (((c>='A')&&(c<='Z'))?(c-'A'+'a'):1)
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


static int ggewerkt = 0;

void ovl_grapho0_2()
{
}


static void regel61(void)
{
 
 initj;
 do {
 if ((curch == 'f')) 
 if ((nexch == 'o')) 
 if ((offch(2) == 'y')) 
 if ((offch(3) == 'e')) 
 if ((offch(4) == 'r')) 
 if ( (((offch(5) == ' ')) || ((offch(5) == 's') && (offch(6) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,61,1);
gcorlengt(1);
     curch = 'f';
     nexch = 'w';
     offch(2) = 'A';
     offch(3) = 'j';
     offch(4) = 'e';
     offch(5) = 'e';

     gotoj(5);
   }

  forwj;
 } while nextj;
}

static void regel62(void)
{
 
 initj;
 do {
 if ((curch == 'g')) 
 if ((nexch == 'i')) 
 if ((offch(2) == 'n')) 
 if ( (((offch(3) == ' ')) || ((offch(3) == 's') && (offch(4) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,62,1);
gcorlengt(1);
     curch = 'd';
     nexch = 'j';
     offch(2) = 'I';
     offch(3) = 'n';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel63(void)
{
 
 initj;
 do {
 if ((curch == 'm')) 
 if ((nexch == 'i')) 
 if ((offch(2) == 'l')) 
 if ((offch(3) == 'l')) 
 if ((offch(4) == 'e')) 
 if ((offch(5) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,63,1);
gcorlengt(-1);
     curch = 'm';
     nexch = 'i';
     offch(2) = 'e';
     offch(3) = 'l';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel64(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'f')) 
 if ((offch(3) == 'e')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,64,1);
     curch = 's';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 'f';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel65(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ((nexch == 'c')) 
 if ((offch(2) == 'e')) 
 if ((offch(3) == 'n')) 
 if ((offch(4) == 'e')) 
 if ( (((offch(5) == ' ')) || ((offch(5) == 's') && (offch(6) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,65,1);
     curch = 's';
     nexch = 'E';
     offch(2) = ':';
     offch(3) = 'n';
     offch(4) = '&';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel66(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ((nexch == 'h')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 'g')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,66,1);
gcorlengt(-1);
     curch = 'S';
     nexch = 'E';
     offch(2) = 'k';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel67(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ((nexch == 'h')) 
 if ((offch(2) == 'i')) 
 if ((offch(3) == 'r')) 
 if ((offch(4) == 't')) 
 if ( (((offch(5) == ' ')) || ((offch(5) == 's') && (offch(6) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,67,1);
gcorlengt(-1);
     curch = 'S';
     nexch = 'U';
     offch(2) = 'r';
     offch(3) = 't';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel68(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ((nexch == 'h')) 
 if ((offch(2) == 'o')) 
 if ((offch(3) == 'w')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,68,1);
     curch = 'S';
     nexch = 'o';
     offch(2) = 'o';
     offch(3) = 'w';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel69(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ((nexch == 'p')) 
 if ((offch(2) == 'r')) 
 if ((offch(3) == 'a')) 
 if ((offch(4) == 'y')) 
 if ( (((offch(5) == ' ')) || ((offch(5) == 's') && (offch(6) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,69,1);
     curch = 's';
     nexch = 'p';
     offch(2) = 'r';
     offch(3) = 'e';
     offch(4) = 'e';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel70(void)
{
 
 initj;
 do {
 if ((curch == 'o')) 
 if ((nexch == 'k')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 'y')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,70,1);
     curch = 'o';
     nexch = 'k';
     offch(2) = 'e';
     offch(3) = 'e';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel71(void)
{
 
 initj;
 do {
 if ((curch == 'r')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'c')) 
 if ((offch(3) == 'e')) 
 if ( (((offch(4) == 'n') && (offch(5) == ' ')) || ((offch(4) == 's') && 
(offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,71,1);
gcorlengt(1);
     curch = 'r';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 's';
     offch(4) = '&';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel72(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'p')) 
 if ((offch(3) == 'e')) 
 if ((offch(4) == 'n')) 
 if ((offch(5) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,72,1);
gcorlengt(1);
     curch = 't';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 'p';
     offch(4) = '&';
     offch(5) = 'n';

     gotoj(5);
   }

  forwj;
 } while nextj;
}

static void regel73(void)
{
 
 initj;
 do {
 if ((curch == 'r')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'c')) 
 if ((offch(3) == 'e')) 
 if ((offch(4) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,73,1);
     curch = 'r';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 's';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel74(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'k')) 
 if ((offch(3) == 'e')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,74,1);
     curch = 'k';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 'k';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel75(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'p')) 
 if ((offch(3) == 'e')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,75,1);
     curch = 't';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 'p';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel76(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'p')) 
 if ((offch(3) == 'e')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,76,1);
     curch = 'k';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 'p';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel77(void)
{
 
 initj;
 do {
 if ((curch == 'g')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'm')) 
 if ((offch(3) == 'e')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,77,1);
     curch = 'G';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 'm';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel78(void)
{
 
 initj;
 do {
 if ((curch == 'f')) 
 if ((nexch == 'r')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 'm')) 
 if ((offch(4) == 'e')) 
 if ( (((offch(5) == ' ')) || ((offch(5) == 's') && (offch(6) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,78,1);
     curch = 'f';
     nexch = 'r';
     offch(2) = 'e';
     offch(3) = 'e';
     offch(4) = 'm';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel79(void)
{
 
 initj;
 do {
 if ((curch == 'j')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'm')) 
 if ( (((offch(3) == ' ')) || ((offch(3) == 's') && (offch(4) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,79,1);
     curch = 'Z';
     nexch = 'E';
     offch(2) = 'm';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel80(void)
{
 
 initj;
 do {
 if ((curch == 'j')) 
 if ((nexch == 'e')) 
 if ((offch(2) == 'e')) 
 if ((offch(3) == 'p')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,80,1);
     curch = 'Z';
     nexch = 'i';
     offch(2) = 'e';
     offch(3) = 'p';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel81(void)
{
 
 initj;
 do {
 if ((curch == 'j')) 
 if ((nexch == 'u')) 
 if ((offch(2) == 's')) 
 if ((offch(3) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,81,1);
gcorlengt(-1);
     curch = 'Z';
     nexch = 'u';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel82(void)
{
 
 initj;
 do {
 if ((curch == 'j')) 
 if ((nexch == 'e')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 'n')) 
 if ((offch(4) == 's')) 
 if ((offch(5) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,82,1);
     curch = 'Z';
     nexch = 'i';
     offch(2) = 'e';
     offch(3) = 'n';
     offch(4) = 's';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel83(void)
{
 
 initj;
 do {
 if ((curch == 'p')) 
 if ((nexch == 'l')) 
 if ((offch(2) == 'u')) 
 if ((offch(3) == 'c')) 
 if ((offch(4) == 'h')) 
 if ((offch(5) == 'e')) 
 if ((offch(6) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,83,1);
gcorlengt(-1);
     curch = 'p';
     nexch = 'l';
     offch(2) = 'u';
     offch(3) = 'u';
     offch(4) = 'S';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel84(void)
{
 
 initj;
 do {
 if ((curch == 'l')) 
 if ((nexch == 'i')) 
 if ((offch(2) == 'v')) 
 if ((offch(3) == 'e')) 
 if ((offch(4) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,84,1);
     curch = 'l';
     nexch = 'A';
     offch(2) = 'I';
     offch(3) = 'f';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel85(void)
{
 
 initj;
 do {
 if ((curch == 'l')) 
 if ((nexch == 'u')) 
 if ((offch(2) == 'x')) 
 if ((offch(3) == 'e')) 
 if ((offch(4) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,85,1);
gcorlengt(2);
     curch = 'l';
     nexch = 'u';
     offch(2) = 'u';
     offch(3) = 'k';
     offch(4) = 's';
     offch(5) = '&';

     gotoj(5);
   }

  forwj;
 } while nextj;
}

static void regel86(void)
{
 
 initj;
 do {
 if ((curch == 'l')) 
 if ((nexch == 'e')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 's')) 
 if ((offch(4) == 'e')) 
 if ( (((offch(5) == ' ')) || ((offch(5) == 'n') && (offch(6) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,86,1);
     curch = 'l';
     nexch = 'i';
     offch(2) = 'i';
     offch(3) = 's';
     offch(4) = '&';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel87(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if ((nexch == 'r')) 
 if ((offch(2) == 'u')) 
 if ( (((offch(3) == 'c')) || ((offch(3) == 'k'))))
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,87,1);
gcorlengt(1);
     curch = 't';
     nexch = 'r';
     offch(2) = 'u';
     offch(3) = 'u';
     offch(4) = 'k';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel88(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'n')) 
 if ((offch(3) == 'k')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,88,1);
     curch = 't';
     nexch = 'E';
     offch(2) = 'N';
     offch(3) = 'k';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel89(void)
{
 
 initj;
 do {
 if ((curch == 'f')) 
 if ((nexch == 'l')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 't')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,89,1);
     curch = 'f';
     nexch = 'l';
     offch(2) = 'E';
     offch(3) = 't';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel90(void)
{
 
 initj;
 do {
 if ( (((curch == 'c')) || ((curch == 'k'))))
 if ((nexch == 'l')) 
 if ((offch(2) == 'o')) 
 if ((offch(3) == 'w')) 
 if ((offch(4) == 'n')) 
 if ( (((offch(5) == ' ')) || ((offch(5) == 's') && (offch(6) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,90,1);
     curch = 'k';
     nexch = 'l';
     offch(2) = 'A';
     offch(3) = 'U';
     offch(4) = 'n';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel91(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ((nexch == 'p')) 
 if ((offch(2) == 'e')) 
 if ((offch(3) == 'e')) 
 if ((offch(4) == 'c')) 
 if ((offch(5) == 'h')) 
 if ((offch(6) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,91,1);
     curch = 's';
     nexch = 'p';
     offch(2) = 'i';
     offch(3) = 'e';
     offch(4) = 't';
     offch(5) = 'S';

     gotoj(5);
   }

  forwj;
 } while nextj;
}

static void regel92(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if ((nexch == 'o')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 's')) 
 if ((offch(4) == 't')) 
 if ((offch(5) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,92,1);
     curch = 't';
     nexch = 'o';
     offch(2) = 'o';
     offch(3) = 's';
     offch(4) = 't';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel93(void)
{
 
 initj;
 do {
 if ((curch == 'g')) 
 if ((nexch == 'o')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 'l')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,93,1);
     curch = 'G';
     nexch = 'o';
     offch(2) = 'o';
     offch(3) = 'l';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel94(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'o')) 
 if ((offch(2) == 'a')) 
 if ((offch(3) == 'c')) 
 if ((offch(4) == 'h')) 
 if ( (((offch(5) == ' ')) || ((offch(5) == 'e') && (offch(6) == 's') && 
(offch(7) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,94,1);
     curch = 'c';
     nexch = 'o';
     offch(2) = 'o';
     offch(3) = 't';
     offch(4) = 'S';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel95(void)
{
 
 initj;
 do {
 if ((curch == 'u')) 
 if ((nexch == 'w')) 
 if ((prech == 'e'))    {
     gtracetask(rulefile_g0apho,95,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel96(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((nexch == 'y')) 
 if ((offch(2) == ' '))    {
     gtracetask(rulefile_g0apho,96,1);
     curch = 'i';
     nexch = 'e';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel97(void)
{
 
 initj;
 do {
 if ((curch == 'y')) 
 if ((
inset(prech,gpcons))) 
 if ( (((nexch == ' ')) || ((nexch == 'c') && (offch(2) == 'l')) || (
(nexch == 'd') && (offch(2) == 'r')) || ((nexch == 'p') && 
(offch(2) == 'r')) || ((nexch == 's') && (offch(2) == 't')) || (
(nexch == 'c') && (offch(2) == 'h')) || ((nexch == 't') && 
(offch(2) == 'h')) || ((
inset(nexch,gpcons)) && 
(
inset(offch(2),gpvoc))) || (
(
inset(nexch,gpvoc)))))   {
     gtracetask(rulefile_g0apho,97,1);
gcorlengt(1);
     curch = 'i';
     nexch = 'e';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel98(void)
{
 
 initj;
 do {
 if ((curch == 'y')) 
 if ( (((nexch == 'e')) || ((
inset(nexch,gpcons)) && 
(
inset(offch(2),gmvoc)))))   {
     gtracetask(rulefile_g0apho,98,1);
     curch = 'i';

   }

  forwj;
 } while nextj;
}

static void regel99(void)
{
 
 initj;
 do {
 if ((curch == 'y'))    {
     gtracetask(rulefile_g0apho,99,1);
     curch = 'j';

   }

  forwj;
 } while nextj;
}

static void regel100(void)
{
 
 initj;
 do {
 if ((curch == 'o')) 
 if ((nexch == 'u')) 
 if ( (((
(!inset(offch(2),gmcons    )) && 
((inset(offch(2),gklank) && (offch(2)!='d'))) && 
((inset(offch(2),gklank) && (offch(2)!='w')))) && 
(
inset(offch(3),gpvoc))) || (
(offch(2) == 'c') && (offch(3) == 'h')) || ((offch(2) == 'r')) || (
(offch(2) == 'p')) || ((offch(2) == 't')) || ((offch(2) == 'v')) || (
(offch(2) == 's') && (offch(3) == 'i')) || ((offch(2) == 'i'))))
 if ( (((offch(-2) == 't') && (prech == 'h')) || ((prech == 't')) || (
(prech == 'r')) || ((prech == 'l')) || ((prech == 'j')) || (
(prech == 'd')) || ((prech == 'g')) || ((prech == 'c'))))   {
     gtracetask(rulefile_g0apho,100,1);
gcorlengt(-1);
     curch = 'y';

   }

  forwj;
 } while nextj;
}

static void regel101(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'h')) 
 if ((offch(2) == 'e')) 
 if ( (((offch(3) == ' ')) || ((offch(3) == 'n') && (offch(4) == ' ')) || (
(offch(3) == 's') && (offch(4) == ' '))))
 if ( (((offch(-3) == 'b') && (offch(-2) == 'r') && (prech == 'o')) || (
(offch(-3) == 'r') && (offch(-2) == 'a') && (prech == 'n')) || (
(prech == 'y'))))   {
     gtracetask(rulefile_g0apho,101,1);
gcorlengt(-1);
     curch = 'S';

   }

  forwj;
 } while nextj;
}

static void regel102(void)
{
 
 initj;
 do {
 if ((curch == 'd')) 
 if ((nexch == 'g')) 
 if ((offch(2) == 'e')) 
 if ( (((offch(3) == ' ')) || ((offch(3) == 'n') && (offch(4) == ' ')) || (
(offch(3) == 's') && (offch(4) == ' '))))   {
     gtracetask(rulefile_g0apho,102,1);
     curch = 't';
     nexch = 'S';
     offch(2) = '&';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel103(void)
{
 
 initj;
 do {
 if ( (((curch == 'e')) || ((curch == '&'))))
 if ((nexch == ' ')) 
 if ( (((offch(-2) == 'q') && (prech == 'u')) || ((prech == 'S'))))   {
     gtracetask(rulefile_g0apho,103,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel104(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'a')) 
 if ((gvgl( 1,false," 3ck 3cks4nch 3sh 4tch 000000000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,104,1);
     curch = 'E';

   }

  forwj;
 } while nextj;
}

static void regel105(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'c')) 
 if ((nexch == 'h')) 
 if ((gvgl( -1,true," 2En2un3 in4tret2Et0000000000000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,105,1);
gcorlengt(-1);
     curch = 'S';

   }

  forwj;
 } while nextj;
}

static void regel106(void)
{
 
 initj;
 do {
 if ( (((curch == 's') && (nexch == 'h')) || ((curch == 's') && 
(nexch == 'j'))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,106,1);
gcorlengt(-1);
     curch = 'S';

   }

  forwj;
 } while nextj;
}

static void regel107(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((nexch == 'a')) 
 if ((
inset(offch(2),gpcons))) 
 if ((offch(3) == ' ')) 
 if ((
inset(prech,gpcons))) 
 if ((offch(-2) == ' '))    {
     gtracetask(rulefile_g0apho,107,1);
     curch = 'i';
     nexch = 'e';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel108(void)
{
 
 initj;
 do {
 if ((curch == 'o')) 
 if ((nexch == 'e'))    {
     gtracetask(rulefile_g0apho,108,1);
gcorlengt(-1);
     curch = 'y';

   }

  forwj;
 } while nextj;
}

static void regel109(void)
{
 
 initj;
 do {
 if ((curch == 'i')) 
 if ((nexch == 'e')) 
 if ( (((prech == 'o')) || ((prech == 'a')) || ((prech == 'y'))))   {
     gtracetask(rulefile_g0apho,109,1);
     curch = 'j';
     nexch = '&';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel110(void)
{
 
 initj;
 do {
 if ( (((curch == 'i') && (nexch == 'j')) || ((curch == 'e') && 
(nexch == 'i'))))
 if (!((offch(2) == 'j')))    {
     gtracetask(rulefile_g0apho,110,1);
     curch = 'E';
     nexch = 'I';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel111(void)
{
 
 initj;
 do {
 if ((curch == 'q')) 
 if ((nexch == 'u')) 
 if ( (((offch(2) == 'a')) || ((offch(2) == 'o')) || ((offch(2) == 'e') && 
(offch(3) == 'n') && (offch(4) == 't'))))   {
     gtracetask(rulefile_g0apho,111,1);
gcorlengt(1);
     curch = '+';
     nexch = 'k';
     offch(2) = 'w';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel112(void)
{
 
 initj;
 do {
 if ((curch == 'q')) 
 if ((nexch == 'u'))    {
     gtracetask(rulefile_g0apho,112,1);
gcorlengt(-1);
     curch = 'k';

   }

  forwj;
 } while nextj;
}

static void regel113(void)
{
 
 initj;
 do {
 if ((curch == 'u')) 
 if ((nexch == 'i'))    {
     gtracetask(rulefile_g0apho,113,1);
     curch = 'U';
     nexch = 'I';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel114(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((prech == 'I')) 
 if (!((nexch == 'e')))    {
     gtracetask(rulefile_g0apho,114,1);
gcorlengt(1);
     curch = 'j';
     nexch = '&';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel115(void)
{
 
 initj;
 do {
 if ((curch == 'j')) 
 if ((nexch == 'y')) 
 if ((offch(2) == 'r')) 
 if ( (((offch(3) == 'n')) || ((offch(3) == ' '))))   {
     gtracetask(rulefile_g0apho,115,1);
     curch = 'Z';

   }

  forwj;
 } while nextj;
}

static void regel116(void)
{
 
 initj;
 do {
 if ((curch == 'a')) 
 if ((nexch == 'u')) 
 if ( (((offch(2) == 'f')) || ((offch(2) == 'r'))))
 if ( (((offch(-2) == 'c') && (prech == 'h')) || ((offch(-4) == 'r') && 
(offch(-3) == 'e') && (offch(-2) == 's') && (prech == 't'))))   {
     gtracetask(rulefile_g0apho,116,1);
     curch = 'o';
     nexch = 'o';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel117(void)
{
 
 initj;
 do {
 if ( (((curch == 'o') && (nexch == 'u')) || ((curch == 'a') && 
(nexch == 'u'))))   {
     gtracetask(rulefile_g0apho,117,1);
     curch = 'A';
     nexch = 'U';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel118(void)
{
 
 initj;
 do {
 if ((curch == 'a')) 
 if ((nexch == 'i')) 
 if ((offch(2) == 'l')) 
 if ((offch(3) == 'l'))    {
     gtracetask(rulefile_g0apho,118,1);
gcorlengt(-2);
     curch = 'A';
     nexch = 'j';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel119(void)
{
 
 initj;
 do {
 if ((curch == 'o')) 
 if ((nexch == 'i')) 
 if ( (((offch(2) == 'r')) || ((offch(2) == 't'))))
 if (!(((prech == ' ')) || ((prech == 'o'))))   {
     gtracetask(rulefile_g0apho,119,1);
gcorlengt(1);
     curch = 'w';
     nexch = 'a';
     offch(2) = 'a';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel120(void)
{
 
 initj;
 do {
 if ((curch == 'i')) 
 if ( (((offch(-4) == ' ') && (offch(-3) == 's') && (offch(-2) == 'k') && 
(prech == 'a')) || ((offch(-3) == ' ') && (offch(-2) == 'm') && 
(prech == 'a')) || ((prech == 'y')) || ((offch(-2) == 'a') && 
(prech == 'a')) || ((offch(-2) == 'o') && (prech == 'o'))))   {
     gtracetask(rulefile_g0apho,120,1);
     curch = 'j';

   }

  forwj;
 } while nextj;
}

/*--- STRING RULES ---------------------------------------------*/

void fono_grapho_0_2()
{
 regel61(); regel62(); regel63(); regel64();
 regel65(); regel66(); regel67(); regel68(); regel69(); regel70(); regel71(); regel72();
 regel73(); regel74(); regel75(); regel76(); regel77(); regel78(); regel79(); regel80();
 regel81(); regel82(); regel83(); regel84(); regel85(); regel86(); regel87(); regel88();
 regel89(); regel90(); regel91(); regel92(); regel93(); regel94(); regel95(); regel96();
 regel97(); regel98(); regel99(); regel100(); regel101(); regel102(); regel103(); regel104();
 regel105(); regel106(); regel107(); regel108(); regel109(); regel110(); regel111(); regel112();
 regel113(); regel114(); regel115(); regel116(); regel117(); regel118(); regel119(); regel120();
}

/*--- PARAMETRISATION RULES ------------------------------------*/
void fone_grapho_0_2()
{
}

