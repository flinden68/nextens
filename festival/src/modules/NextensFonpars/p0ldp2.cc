#define NULL 0L

extern char rulefile_p0ldp[];
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

void ovl_ppldp0_2()
{
}


static void regel85(void)
{
 
 initj;
 do {
 if ((curch == ' ')) 
 if ( (((prech == '^')) || ((prech == '`')) || ((prech == ' '))))   {
     gtracetask(rulefile_p0ldp,85,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel86(void)
{
 
 initj;
 do {
 if ( (((curch == ':')) || ((curch == ';'))))   {
     gtracetask(rulefile_p0ldp,86,1);
     curch = ',';

   }

  forwj;
 } while nextj;
}

static void regel87(void)
{
 
 if ((curch == '*')) 
 if (!(((
inset(nexch,gmhoofd))) || (
(
inset(nexch,gphoofd)))))   {
     gtracetask(rulefile_p0ldp,87,1);
gcorlengt(10);
     curch = ' ';
     nexch = 's';
     offch(2) = 't';
     offch(3) = 'e';
     offch(4) = 'r';
     offch(5) = 'r';
     offch(6) = 'e';
     offch(7) = 't';
     offch(8) = 'j';
     offch(9) = 'e';
     offch(10) = ' ';

     ggewerkt = true;
   }

}

static void regel88(void)
{
 
 if ((curch == '+')) 
 if ((nexch == '-'))    {
     gtracetask(rulefile_p0ldp,88,1);
gcorlengt(11);
     curch = ' ';
     nexch = 'p';
     offch(2) = 'l';
     offch(3) = 'u';
     offch(4) = 's';
     offch(5) = ' ';
     offch(6) = 'o';
     offch(7) = 'f';
     offch(8) = ' ';
     offch(9) = 'm';
     offch(10) = 'i';
     offch(11) = 'n';
     offch(12) = ' ';

     ggewerkt = true;
   }

}

static void regel89(void)
{
 
 if ((curch == '+'))    {
     gtracetask(rulefile_p0ldp,89,1);
gcorlengt(5);
     curch = ' ';
     nexch = 'p';
     offch(2) = 'l';
     offch(3) = 'u';
     offch(4) = 's';
     offch(5) = ' ';

     ggewerkt = true;
   }

}

static void regel90(void)
{
 
 if ((curch == '?'))    {
     gtracetask(rulefile_p0ldp,90,1);
gcorlengt(11);
     curch = ' ';
     nexch = 'v';
     offch(2) = 'r';
     offch(3) = 'a';
     offch(4) = 'a';
     offch(5) = 'g';
     offch(6) = 't';
     offch(7) = 'e';
     offch(8) = 'k';
     offch(9) = 'e';
     offch(10) = 'n';
     offch(11) = ' ';

     ggewerkt = true;
   }

}

static void regel91(void)
{
 
 if ((curch == '!'))    {
     gtracetask(rulefile_p0ldp,91,1);
gcorlengt(13);
     curch = ' ';
     nexch = 'u';
     offch(2) = 'i';
     offch(3) = 't';
     offch(4) = 'r';
     offch(5) = 'o';
     offch(6) = 'e';
     offch(7) = 'p';
     offch(8) = 't';
     offch(9) = 'e';
     offch(10) = 'k';
     offch(11) = 'e';
     offch(12) = 'n';
     offch(13) = ' ';

     ggewerkt = true;
   }

}

static void regel92(void)
{
 
 if ((curch == '='))    {
     gtracetask(rulefile_p0ldp,92,1);
gcorlengt(14);
     curch = ' ';
     nexch = 'i';
     offch(2) = 's';
     offch(3) = ' ';
     offch(4) = 'g';
     offch(5) = 'e';
     offch(6) = 'l';
     offch(7) = 'i';
     offch(8) = 'j';
     offch(9) = 'k';
     offch(10) = ' ';
     offch(11) = 'a';
     offch(12) = 'a';
     offch(13) = 'n';
     offch(14) = ' ';

     ggewerkt = true;
   }

}

static void regel93(void)
{
 
 if ((curch == '&'))    {
     gtracetask(rulefile_p0ldp,93,1);
gcorlengt(3);
     curch = ' ';
     nexch = 'e';
     offch(2) = 'n';
     offch(3) = ' ';

     ggewerkt = true;
   }

}

static void regel94(void)
{
 
 if ( (((curch == '/')) || ((curch == '\\'))))   {
     gtracetask(rulefile_p0ldp,94,1);
gcorlengt(5);
     curch = ' ';
     nexch = 's';
     offch(2) = 'l';
     offch(3) = 'e';
     offch(4) = 's';
     offch(5) = ' ';

     ggewerkt = true;
   }

}

static void regel95(void)
{
 
 if ((curch == '@'))    {
     gtracetask(rulefile_p0ldp,95,1);
gcorlengt(11);
     curch = ' ';
     nexch = 'a';
     offch(2) = 'p';
     offch(3) = 'e';
     offch(4) = ' ';
     offch(5) = 's';
     offch(6) = 't';
     offch(7) = 'a';
     offch(8) = 'a';
     offch(9) = 'r';
     offch(10) = 't';
     offch(11) = ' ';

     ggewerkt = true;
   }

}

static void regel96(void)
{
 
 if ((curch == '#'))    {
     gtracetask(rulefile_p0ldp,96,1);
gcorlengt(7);
     curch = ' ';
     nexch = 'r';
     offch(2) = 'a';
     offch(3) = 's';
     offch(4) = 't';
     offch(5) = 'e';
     offch(6) = 'r';
     offch(7) = ' ';

     ggewerkt = true;
   }

}

static void regel97(void)
{
 
 if ((curch == '|'))    {
     gtracetask(rulefile_p0ldp,97,1);
gcorlengt(21);
     curch = ' ';
     nexch = ',';
     offch(2) = ' ';
     offch(3) = 'v';
     offch(4) = 'e';
     offch(5) = 'r';
     offch(6) = 't';
     offch(7) = 'i';
     offch(8) = 'c';
     offch(9) = 'a';
     offch(10) = 'l';
     offch(11) = 'e';
     offch(12) = ' ';
     offch(13) = 's';
     offch(14) = 't';
     offch(15) = 'r';
     offch(16) = 'e';
     offch(17) = 'e';
     offch(18) = 'p';
     offch(19) = ' ';
     offch(20) = ',';
     offch(21) = ' ';

     ggewerkt = true;
   }

}

static void regel98(void)
{
 
 if ((curch == '%'))    {
     gtracetask(rulefile_p0ldp,98,1);
gcorlengt(8);
     curch = ' ';
     nexch = 'p';
     offch(2) = 'r';
     offch(3) = 'o';
     offch(4) = 'c';
     offch(5) = 'e';
     offch(6) = 'n';
     offch(7) = 't';
     offch(8) = ' ';

     ggewerkt = true;
   }

}

static void regel99(void)
{
 
 if ((curch == '$'))    {
     gtracetask(rulefile_p0ldp,99,1);
gcorlengt(7);
     curch = ' ';
     nexch = 'd';
     offch(2) = 'o';
     offch(3) = 'l';
     offch(4) = 'l';
     offch(5) = 'a';
     offch(6) = 'r';
     offch(7) = ' ';

     ggewerkt = true;
   }

}

static void regel100(void)
{
 
 if ((curch == '_'))    {
     gtracetask(rulefile_p0ldp,100,1);
gcorlengt(14);
     curch = ' ';
     nexch = 'o';
     offch(2) = 'n';
     offch(3) = 'd';
     offch(4) = 'e';
     offch(5) = 'r';
     offch(6) = 's';
     offch(7) = 't';
     offch(8) = 'r';
     offch(9) = 'e';
     offch(10) = 'p';
     offch(11) = 'i';
     offch(12) = 'n';
     offch(13) = 'g';
     offch(14) = ' ';

     ggewerkt = true;
   }

}

static void regel101(void)
{
 
 if ( (((curch == '{')) || ((curch == '(')) || ((curch == '['))))   {
     gtracetask(rulefile_p0ldp,101,1);
gcorlengt(16);
     curch = ' ';
     nexch = ',';
     offch(2) = ' ';
     offch(3) = 'h';
     offch(4) = 'a';
     offch(5) = 'a';
     offch(6) = 'k';
     offch(7) = ' ';
     offch(8) = 'o';
     offch(9) = 'p';
     offch(10) = 'e';
     offch(11) = 'n';
     offch(12) = 'e';
     offch(13) = 'n';
     offch(14) = ' ';
     offch(15) = ',';
     offch(16) = ' ';

     ggewerkt = true;
   }

}

static void regel102(void)
{
 
 if ( (((curch == '}')) || ((curch == ')')) || ((curch == ']'))))   {
     gtracetask(rulefile_p0ldp,102,1);
gcorlengt(17);
     curch = ' ';
     nexch = ',';
     offch(2) = ' ';
     offch(3) = 'h';
     offch(4) = 'a';
     offch(5) = 'a';
     offch(6) = 'k';
     offch(7) = ' ';
     offch(8) = 's';
     offch(9) = 'l';
     offch(10) = 'u';
     offch(11) = 'i';
     offch(12) = 't';
     offch(13) = 'e';
     offch(14) = 'n';
     offch(15) = ' ';
     offch(16) = ',';
     offch(17) = ' ';

     ggewerkt = true;
   }

}

static void regel103(void)
{
 
 initj;
 do {
 if ( (((curch == ' ')) || ((curch == '_'))))
 if ((prech == 's')) 
 if ((offch(-2) == '\'')) 
 if ((offch(-3) == ' '))    {
     gtracetask(rulefile_p0ldp,103,1);
     curch = '-';

   }

  forwj;
 } while nextj;
}

/*--- STRING RULES ---------------------------------------------*/

void fono_ppldp_0_2()
{
 regel85(); regel86();

 initj; 
 do {
 if ( (((curch == '-')) || ((curch == '+')) || ((curch == '&')) || (
(curch == '=')) || ((curch == '%')) || ((curch == '$')) || (
(curch == '*')) || ((curch == '/')) || ((curch == '\\')) || (
(curch == '(')) || ((curch == ')')) || ((curch == '|')) || (
(curch == '@')) || ((curch == '#')) || ((curch == '_')) || (
(curch == '[')) || ((curch == ']')) || ((curch == '}')) || (
(curch == '{')) || ((curch == '?')) || ((curch == '!')))){
 ggewerkt = 0;
 regel87();
 if (ggewerkt) goto l3; regel88();

 if (ggewerkt) goto l3; regel89();
 if (ggewerkt) goto l3; regel90();
 if (ggewerkt) goto l3; regel91();
 if (ggewerkt) goto l3; regel92();
 if (ggewerkt) goto l3; regel93();
 if (ggewerkt) goto l3; regel94();
 if (ggewerkt) goto l3; regel95();
 if (ggewerkt) goto l3; regel96();

 if (ggewerkt) goto l3; regel97();
 if (ggewerkt) goto l3; regel98();
 if (ggewerkt) goto l3; regel99();
 if (ggewerkt) goto l3; regel100();
 if (ggewerkt) goto l3; regel101();
 if (ggewerkt) goto l3; regel102();
 if (ggewerkt) goto l3;
 }
  l3:
 forwj; 
 } while strnj; regel103();}

/*--- PARAMETRISATION RULES ------------------------------------*/
void fone_ppldp_0_2()
{
}



