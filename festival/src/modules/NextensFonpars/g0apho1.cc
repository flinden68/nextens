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

void ovl_grapho0_1()
{
}


static void regel1(void)
{
 
 initj;
 do {
 if ((curch == '`'))    {
     gtracetask(rulefile_g0apho,1,1);
gcorlengt(1);
     curch = '`';
     nexch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel2(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'u'))    {
     gtracetask(rulefile_g0apho,2,1);
     curch = 'o';
     nexch = 'o';
     offch(2) = '+';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel3(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(curch,gpseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+1),gphoofd)))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gy-1) == ' ')) || ((
inset(offch(gy-1),gphoofd)))
))   gstop = true;
      else {

 if ((
inset(offch(gy-1),gpseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,3,1);
     ghoofdklein(0,1);

   }
}}
  forwj;
 } while nextj;
}

static void regel4(void)
{
 
 initj;
 do {
 if ((
inset(curch,gphoofd))) 
 if ( (((
inset(nexch,gmhoofd))) || (
(nexch == '\''))))
 if ( (((prech == ' ')) || ((prech == '\''))))   {
     gtracetask(rulefile_g0apho,4,1);
     ghoofdklein(0,-1);

   }

  forwj;
 } while nextj;
}

static void regel5(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ( (((
inset(curch,gpcons))) || (
(curch == 'y')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+1) == ' ')) || ((offch(gx+1) == '\'') && (offch(gx+2) == 's') && 
(offch(gx+3) == ' ')) || ((offch(gx+1) == '\'') && (offch(gx+2) == 'e') && 
(offch(gx+3) == 'n')) || ((offch(gx+1) == '\'') && (offch(gx+2) == 't') && 
(offch(gx+3) == 'j') && (offch(gx+4) == 'e'))))   gstop = true;
      else {

 if ((
inset(offch(gx+1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gphoofd))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,5,1);
     ghoofdklein(0,1);

   }
}}
  forwj;
 } while nextj;
}

static void regel6(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(curch,gmhoofd)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+1) == '.'))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,6,1);
     ghoofdklein(0,1);

   }
}
  forwj;
 } while nextj;
}

static void regel7(void)
{
 
 initj;
 do {
 if ((curch == '.')) 
 if ((nexch == ' ')) 
 if ((
inset(prech,gphoofd)))    {
     gtracetask(rulefile_g0apho,7,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel8(void)
{
 
 initj;
 do {
 if ((
inset(curch,gphoofd))) 
 if ( (((
inset(nexch,gphoofd)) && 
(offch(2) == ' ')) || ((
inset(nexch,gphoofd)) && 
(offch(2) == '\'')) || ((nexch == ' ')) || ((nexch == '\''))))
 if ((
inset(prech,gphoofd))) 
 if ( (((offch(-2) == '+')) || ((offch(-2) == ' '))))   {
     gtracetask(rulefile_g0apho,8,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel9(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(gletter(curch,-1),gpvoc)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(gletter(offch(gx+1),-1),gpvoc)))    gstop = true;
      else {

 if ((
inset(gletter(offch(gx+1),-1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,9,1);
     ghoofdklein(0,-1);

   }
}
  forwj;
 } while nextj;
}

static void regel10(void)
{
 
 initj;
 do {
 if ((
inset(gletter(curch,-1),gpcons))) 
 if ((
inset(gletter(nexch,-1),gpvoc))) 
 if ((
inset(gletter(offch(2),-1),gpcons)))    {
     gtracetask(rulefile_g0apho,10,1);
     ghoofdklein(0,-1);
     ghoofdklein(1,-1);
     ghoofdklein(2,-1);

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel11(void)
{
 
 initj;
 do {
 if ((
(inset(curch, gpcons) && 
inset(curch,gmson)))) 
 if ((
(inset(gletter(prech,-1), gpcons) && 
inset(gletter(prech,-1),gmson) && 
inset(gletter(prech,-1),gmcont)))) 
 if ((offch(-2) == ' '))    {
     gtracetask(rulefile_g0apho,11,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel12(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpcons))) 
 if ( (((prech == 'Q')) || ((prech == 'X'))))
 if ((offch(-2) == ' '))    {
     gtracetask(rulefile_g0apho,12,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel13(void)
{
 
 initj;
 do {
 if ( (((curch == 'l')) || ((curch == 'r')) || ((curch == 'n')) || (
(curch == 'm'))))
 if ((
(inset(gletter(prech,-1), gpcons) && 
inset(gletter(prech,-1),gpson)))) 
 if ((offch(-2) == ' '))    {
     gtracetask(rulefile_g0apho,13,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel14(void)
{
 
 initj;
 do {
 if ( (((curch == 'l')) || ((curch == 'n')) || ((curch == 'm'))))
 if ( (((prech == 'T')) || ((prech == 'D'))))
 if ((offch(-2) == ' '))    {
     gtracetask(rulefile_g0apho,14,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel15(void)
{
 
 initj;
 do {
 if ( (((curch == 'B')) || ((curch == 'V')) || ((curch == 'Z')) || (
(curch == 'X')) || ((curch == 'Q')) || ((curch == 'L')) || (
(curch == 'R')) || ((curch == 'N')) || ((curch == 'M'))))
 if ( (((nexch == ' ')) || ((nexch == '\''))))
 if ((
inset(prech,gpcons)))    {
     gtracetask(rulefile_g0apho,15,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel16(void)
{
 
 initj;
 do {
 if ((
inset(gletter(curch,-1),gpcons))) 
 if ( (((nexch == ' ')) || ((nexch == '\''))))
 if ((
inset(prech,gpcons))) 
 if (curch == prech)    {
     gtracetask(rulefile_g0apho,16,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel17(void)
{
 
 initj;
 do {
 if ( (((curch == 'F')) || ((curch == 'G')) || ((
(inset(gletter(curch,-1), gpcons) && 
inset(gletter(curch,-1),gmson))))
))
 if ( (((nexch == ' ')) || ((nexch == '\''))))
 if ((
(inset(prech, gpcons) && 
inset(prech,gmson))))    {
     gtracetask(rulefile_g0apho,17,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel18(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(curch,gphoofd)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+1),gmhoofd)))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gphoofd))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,18,1);
     ghoofdklein(0,-1);

   }
}
  forwj;
 } while nextj;
}

static void regel19(void)
{
 
 initj;
 do { gy = 0;

 if ((
inset(curch,gphoofd)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-1),gmhoofd)))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gphoofd))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,19,1);
     ghoofdklein(0,-1);

   }
}
  forwj;
 } while nextj;
}

static void regel20(void)
{
 
 initj;
 do {
 if ((
inset(curch,gphoofd))) 
 if ((
inset(prech,gphoofd)))    {
     gtracetask(rulefile_g0apho,20,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel21(void)
{
 
 initj;
 do {
 if ( (((curch == '+')) || ((curch == '.'))))
 if ((
inset(nexch,gphoofd))) 
 if ((
inset(prech,gphoofd)))    {
     gtracetask(rulefile_g0apho,21,1);
     curch = '-';

   }

  forwj;
 } while nextj;
}

static void regel22(void)
{
 
 initj;
 do {
 if ((
inset(curch,gphoofd))) 
 if ( (((nexch == ' ')) || ((nexch == '\'') && (offch(2) == 'e'))))
 if ((prech == '-'))    {
     gtracetask(rulefile_g0apho,22,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel23(void)
{
 
 initj;
 do {
 if ((
inset(gletter(curch,-1),gpvoc)))    {
     gtracetask(rulefile_g0apho,23,1);
gcorlengt(1);
     ghoofdklein(0,-1);
     ghoofdklein(1,-1);

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel24(void)
{
 
 initj;
 do {
 if ((curch == 'Q'))    {
     gtracetask(rulefile_g0apho,24,1);
gcorlengt(2);
     curch = 'k';
     nexch = 'u';
     offch(2) = 'u';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel25(void)
{
 
 initj;
 do {
 if ((curch == 'X'))    {
     gtracetask(rulefile_g0apho,25,1);
gcorlengt(2);
     curch = 'I';
     nexch = 'k';
     offch(2) = 's';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel26(void)
{
 
 initj;
 do {
 if ((curch == 'Y'))    {
     gtracetask(rulefile_g0apho,26,1);
gcorlengt(9);
     curch = 'g';
     nexch = 'r';
     offch(2) = 'i';
     offch(3) = 'i';
     offch(4) = 'k';
     offch(5) = 's';
     offch(6) = '&';
     offch(7) = '+';
     offch(8) = 'E';
     offch(9) = 'I';

     gotoj(9);
   }

  forwj;
 } while nextj;
}

static void regel27(void)
{
 
 initj;
 do {
 if ((curch == 'Z'))    {
     gtracetask(rulefile_g0apho,27,1);
gcorlengt(2);
     curch = 'z';
     nexch = 'E';
     offch(2) = 't';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel28(void)
{
 
 initj;
 do {
 if ( (((curch == 'B')) || ((curch == 'C')) || ((curch == 'D')) || (
(curch == 'G')) || ((curch == 'J')) || ((curch == 'P')) || (
(curch == 'T')) || ((curch == 'V')) || ((curch == 'W'))))   {
     gtracetask(rulefile_g0apho,28,1);
gcorlengt(2);
     ghoofdklein(0,-1);
     nexch = 'e';
     offch(2) = 'e';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel29(void)
{
 
 initj;
 do {
 if ( (((curch == 'H')) || ((curch == 'K'))))   {
     gtracetask(rulefile_g0apho,29,1);
gcorlengt(2);
     ghoofdklein(0,-1);
     nexch = 'a';
     offch(2) = 'a';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel30(void)
{
 
 initj;
 do {
 if ( (((curch == 'F')) || ((curch == 'L')) || ((curch == 'M')) || (
(curch == 'N')) || ((curch == 'R')) || ((curch == 'S'))))   {
     gtracetask(rulefile_g0apho,30,1);
gcorlengt(1);
     curch = 'E';
     ghoofdklein(1,-1);

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel31(void)
{
 
 initj;
 do {
 if ((curch == '\'')) 
 if ((nexch == 'e')) 
 if ( (((offch(2) == 'r') && (offch(3) == 's') && (offch(4) == ' ')) || (
(offch(2) == 'r') && (offch(3) == ' ')) || ((offch(2) == 'n') && 
(offch(3) == ' '))))   {
     gtracetask(rulefile_g0apho,31,1);
gcorlengt(-1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel32(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpcons))) 
 if ((nexch == '&')) 
 if ( (((offch(2) == 'r') && (offch(3) == 's') && (offch(4) == ' ')) || (
(offch(2) == 'r') && (offch(3) == ' ')) || ((offch(2) == 'n') && 
(offch(3) == ' '))))   {
     gtracetask(rulefile_g0apho,32,1);
gcorlengt(1);
     curch  = offch( 1);
     forwj;
   }

  forwj;
 } while nextj;
}

static void regel33(void)
{
 
 initj;
 do {
 if ((curch == 'o')) 
 if ((nexch == '^')) 
 if ((offch(2) == 'u'))    {
     gtracetask(rulefile_g0apho,33,1);
gcorlengt(-1);
     curch = 'o';
     nexch = 'e';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel34(void)
{
 
 initj;
 do {
 if ((curch == '~')) 
 if ((nexch == 'n'))    {
     gtracetask(rulefile_g0apho,34,1);
     curch = 'n';
     nexch = 'j';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel35(void)
{
 
 initj;
 do {
 if ((curch == '^')) 
 if ((nexch == 'c'))    {
     gtracetask(rulefile_g0apho,35,1);
gcorlengt(-1);
     curch = 's';

   }

  forwj;
 } while nextj;
}

static void regel36(void)
{
 
 initj;
 do {
 if ((curch == 'a')) 
 if ((nexch == '"')) 
 if ((offch(2) == 'i'))    {
     gtracetask(rulefile_g0apho,36,1);
gcorlengt(-1);
     curch = 'a';
     nexch = 'j';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel37(void)
{
 
 initj;
 do {
 if ( (((curch == '`')) || ((curch == '^'))))
 if ((nexch == 'e'))    {
     gtracetask(rulefile_g0apho,37,1);
     curch = 'E';
     nexch = ':';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel38(void)
{
 
 initj;
 do {
 if ((curch == 'a')) 
 if ((nexch == '^')) 
 if ((offch(2) == 'i'))    {
     gtracetask(rulefile_g0apho,38,1);
gcorlengt(-1);
     curch = 'E';
     nexch = ':';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel39(void)
{
 
 initj;
 do {
 if ((curch == '\'')) 
 if ((nexch == 'e')) 
 if ( (((
inset(offch(2),gmseg))) || (
(offch(2) == '\'') && (offch(3) == 's')) || ((offch(2) == '-'))))   {
     gtracetask(rulefile_g0apho,39,1);
     curch = 'e';

   }

  forwj;
 } while nextj;
}

static void regel40(void)
{
 
 initj;
 do {
 if ((curch == '\'')) 
 if ((nexch == 's')) 
 if ( (((offch(2) == ' ')) || ((offch(2) == '-'))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,40,1);
gcorlengt(-2);
     curch = 's';

   }

  forwj;
 } while nextj;
}

static void regel41(void)
{
 
 initj;
 do {
 if (inset(curch,gklank))
 if (   (
(!inset(curch,gmvoc     )) && 
(!inset(curch,gpcijf    )) && 
((inset(curch,gklank) && (curch!='y'))) && 
((inset(curch,gklank) && (curch!='e')))) ) 
 if ((nexch == '\'')) 
 if ( (((offch(2) == 'n')) || ((offch(2) == 's'))))
 if ((offch(3) == ' '))    {
     gtracetask(rulefile_g0apho,41,1);
gcorlengt(1);
     curch  = offch( 1);
     forwj;
   }

  forwj;
 } while nextj;
}

static void regel42(void)
{
 
 initj;
 do {
 if ( (((curch == '\'')) || ((curch == '^'))))
 if (!(((prech == '-'))))   {
     gtracetask(rulefile_g0apho,42,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel43(void)
{
 
 initj;
 do {
 if ( (((curch == 's') && (nexch == 's')) || ((curch == 'c')) || (
(curch == 't'))))
 if ((nexch == 'i')) 
 if ((offch(2) == '"')) 
 if ((offch(3) == 'e')) 
 if ( (((offch(4) == 'l') && (offch(5) == 'e')) || ((offch(4) == 'e') && 
(offch(5) == 'l')) || ((offch(4) == 'e') && (offch(5) == 'r')) || (
(offch(4) == 'r') && (offch(5) == 'e')) || ((offch(4) == 'n') && 
(offch(5) == 't'))))   {
     gtracetask(rulefile_g0apho,43,1);
gcorlengt(-1);
     curch = '\'';
     nexch = 'S';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel44(void)
{
 
 initj;
 do {
 if ((curch == '"')) 
 if ((
inset(nexch,gpvoc))) 
 if ((prech == 'e')) 
 if ( (((offch(-4) == ' ') && (offch(-3) == 'p') && (offch(-2) == 'r')) || (
(offch(-3) == ' ') && (offch(-2) == 'r')) || ((offch(-3) == ' ') && 
(offch(-2) == 'd')) || ((offch(-3) == ' ') && (offch(-2) == 'g')) || (
(offch(-3) == ' ') && (offch(-2) == 'b'))))   {
     gtracetask(rulefile_g0apho,44,1);
     curch = '+';

   }

  forwj;
 } while nextj;
}

static void regel45(void)
{
 
 initj;
 do {
 if ((curch == 'i')) 
 if ((nexch == '"')) 
 if ((
inset(offch(2),gpvoc))) 
 if ((
inset(prech,gpvoc)))    {
     gtracetask(rulefile_g0apho,45,1);
gcorlengt(-1);
     curch = 'j';

   }

  forwj;
 } while nextj;
}

static void regel46(void)
{
 
 initj;
 do {
 if ((curch == '"')) 
 if (!(((nexch == 'i'))))
 if ((prech == 'i'))    {
     gtracetask(rulefile_g0apho,46,1);
gcorlengt(1);
     curch = 'e';
     nexch = 'j';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel47(void)
{
 
 initj;
 do {
 if ((curch == '"')) 
 if (!(((nexch == 'e') && (offch(2) == 'i')) || ((nexch == 'i') && 
(offch(2) == 's')) || ((nexch == 'e') && (offch(2) == 'r') && 
(offch(3) == 'i') && (offch(4) == 'n') && (offch(5) == 'g')) || (
(nexch == 'e') && (offch(2) == 'r') && (offch(3) == 'e') && 
(offch(4) == 'n')) || ((nexch == 'i') && (offch(2) == 'e') && 
(offch(3) == 'n')) || ((nexch == 'i') && (offch(2) == 'n'))))
 if ((prech == 'e'))    {
     gtracetask(rulefile_g0apho,47,1);
     curch = 'j';

   }

  forwj;
 } while nextj;
}

static void regel48(void)
{
 
 initj;
 do {
 if ((curch == '"')) 
 if ((nexch == 'e')) 
 if ((offch(2) == 'n')) 
 if ((prech == 'o'))    {
     gtracetask(rulefile_g0apho,48,1);
     curch = 'w';

   }

  forwj;
 } while nextj;
}

static void regel49(void)
{
 
 initj;
 do {
 if ((curch == '"')) 
 if ((nexch == 'o')) 
 if (inset(offch(2),gklank))
 if (   (
(!inset(offch(2),gmcons    )) && 
((inset(offch(2),gklank) && (offch(2)!='h')))) ) 
 if ((
inset(offch(3),gpcons))) 
 if ((
inset(prech,gmvoc)))    {
     gtracetask(rulefile_g0apho,49,1);
gcorlengt(-1);
     curch = 'u';

   }

  forwj;
 } while nextj;
}

static void regel50(void)
{
 
 initj;
 do {
 if ((curch == '"')) 
 if ((nexch == 'o')) 
 if ((
inset(prech,gmvoc)))    {
     gtracetask(rulefile_g0apho,50,1);
     curch = 'e';
     nexch = 'u';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel51(void)
{
 
 initj;
 do {
 if ((curch == '"')) 
 if ((nexch == 'a')) 
 if ((
inset(prech,gmvoc)))    {
     gtracetask(rulefile_g0apho,51,1);
     curch = 'e';
     nexch = 'e';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel52(void)
{
 
 initj;
 do {
 if ((curch == '"')) 
 if ((nexch == 'u')) 
 if ((
inset(prech,gmvoc)))    {
     gtracetask(rulefile_g0apho,52,1);
     curch = 'u';
     nexch = 'u';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel53(void)
{
 
 initj;
 do {
 if ((curch == '"'))    {
     gtracetask(rulefile_g0apho,53,1);
     curch = '+';

   }

  forwj;
 } while nextj;
}

static void regel54(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == ' ')) 
 if ((
(inset(offch(gy-1), gmseg) && 
inset(offch(gy-1),gpstrid))))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gy-2) == ',')) || ((offch(gy-2) == '.')) || ((
inset(offch(gy-2),gpseg)))
))   gstop = true;
      else {

 if ((
(inset(offch(gy-2), gmseg) && 
inset(offch(gy-2),gpstrid)))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,54,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel55(void)
{
 
 initj;
 do {
 if ( (((curch == '.')) || ((curch == ',')) || ((curch == '\'')) || (
(curch == '`')) || ((curch == ':')) || ((curch == ';')) || (
(curch == '-')) || ((curch == '+')) || ((curch == '=')) || (
(curch == '%')) || ((curch == '$')) || ((curch == '*')) || (
(curch == '/')) || ((curch == '\\')) || ((curch == '(')) || (
(curch == ')')) || ((curch == '|')) || ((curch == '#')) || (
(curch == '_')) || ((curch == '[')) || ((curch == ']')) || (
(curch == '}')) || ((curch == '{')) || ((curch == '?')) || (
(curch == '!'))))
 if ( (((
inset(nexch,gphoofd))) || (
(
inset(nexch,gmhoofd)))))
 if ( (((
inset(prech,gmhoofd))) || (
(
inset(prech,gphoofd)))))   {
     gtracetask(rulefile_g0apho,55,1);
     curch = '-';

   }

  forwj;
 } while nextj;
}

static void regel56(void)
{
 
 initj;
 do {
 if ((curch == '-')) 
 if ((nexch == '-'))    {
     gtracetask(rulefile_g0apho,56,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel57(void)
{
 
 initj;
 do {
 if ((curch == 'a')) 
 if ((nexch == 'b')) 
 if ((offch(2) == 'c')) 
 if ( (((offch(3) == ' ')) || ((offch(3) == 's') && (offch(4) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,57,1);
gcorlengt(7);
     curch = 'a';
     nexch = 'a';
     offch(2) = ' ';
     offch(3) = 'b';
     offch(4) = 'e';
     offch(5) = 'e';
     offch(6) = ' ';
     offch(7) = 's';
     offch(8) = 'e';
     offch(9) = 'e';

     gotoj(9);
   }

  forwj;
 } while nextj;
}

static void regel58(void)
{
 
 initj;
 do {
 if ((curch == 'b')) 
 if ((nexch == 'l')) 
 if ((offch(2) == 'u')) 
 if ((offch(3) == 'e')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,58,1);
     curch = 'b';
     nexch = 'l';
     offch(2) = 'o';
     offch(3) = 'e';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel59(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'l')) 
 if ((offch(2) == 'o')) 
 if ((offch(3) == 'u')) 
 if ( (((offch(4) == ' ')) || ((offch(4) == 's') && (offch(5) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,59,1);
     curch = 'k';
     nexch = 'l';
     offch(2) = 'o';
     offch(3) = 'e';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel60(void)
{
 
 initj;
 do {
 if ((curch == 'f')) 
 if ((nexch == 'a')) 
 if ((offch(2) == 'n')) 
 if ( (((offch(3) == ' ')) || ((offch(3) == 's') && (offch(4) == ' '))))
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,60,1);
     curch = 'f';
     nexch = 'E';
     offch(2) = 'n';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

/*--- STRING RULES ---------------------------------------------*/

void fono_grapho_0_1()
{
 regel1(); regel2(); regel3(); regel4(); regel5(); regel6();

 initj; 
 do {
 if ((
inset(curch,gphoofd))) 
 if (strnj) {
    gplace_venster(); regel7(); regel8();
 regel9(); regel10(); regel11(); regel12(); regel13(); regel14(); regel15(); regel16();
 regel17(); regel18(); regel19(); regel20(); regel21(); regel22(); regel23(); regel24();
 regel25(); regel26(); regel27(); regel28(); regel29(); regel30(); regel31(); regel32();

 backj; 
 }
 forwj; 
 } while strnj;ginit_venster();


 initj; 
 do {
 if ( (((curch == '\'')) || ((curch == '`')) || ((curch == '"')) || (
(curch == '^')) || ((curch == '~'))))
 if (strnj) {
    gplace_venster(); regel33(); regel34(); regel35(); regel36(); regel37(); regel38(); regel39(); regel40();
 regel41(); regel42(); regel43(); regel44(); regel45(); regel46(); regel47(); regel48();
 regel49(); regel50(); regel51(); regel52(); regel53();
 backj; 
 }
 forwj; 
 } while strnj;ginit_venster();
 regel54(); regel55(); regel56();
 regel57(); regel58(); regel59(); regel60();}

/*--- PARAMETRISATION RULES ------------------------------------*/
void fone_grapho_0_1()
{
}

