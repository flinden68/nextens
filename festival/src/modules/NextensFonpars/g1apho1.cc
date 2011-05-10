#define NULL 0L

extern char rulefile_g1apho[];
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
extern gverzklank gnonseg;
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

void ovl_grapho1_1()
{
}


static void regel1(void)
{
 
 initj;
 do {
 if ((curch == '_'))    {
     gtracetask(rulefile_g1apho,1,1);
     curch = ' ';

   }

  forwj;
 } while nextj;
}

static void regel2(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'g')) {

 if ((offch(gy-1) == '\''))  gy = gy - 1;
 if ((offch(gy-1) == ' '))    {
     gtracetask(rulefile_g1apho,2,1);
     gverander(0,1,(&gmstem    ));
   }
}
  forwj;
 } while nextj;
}

static void regel3(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == '-')) 
 if ((
inset(offch(gx+1),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+2) == '-'))    gstop = true;
      else {

 if ((
inset(offch(gx+2),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g1apho,3,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel4(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'w'))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+1) == ' ')) || ((offch(gx+1) == '&')) || ((offch(gx+1) == '-') && 
(offch(gx+2) == '+') && (offch(gx+3) == '-')) || ((offch(gx+1) == '-'))))   gstop = true;
      else {

 if ((
inset(offch(gx+1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) {

 if ((offch(gy-1) == '-'))  gy = gy - 1;
 if (inset(offch(gy-1),gklank))
 if (   (
(!inset(offch(gy-1),gmvoc     )) && 
((inset(offch(gy-1),gklank) && (offch(gy-1)!='e'))) && 
((inset(offch(gy-1),gklank) && (offch(gy-1)!='i'))) && 
((inset(offch(gy-1),gklank) && (offch(gy-1)!='y')))) )    {
     gtracetask(rulefile_g1apho,4,1);
gcorlengt(-1);

     backj;
   }
}}
  forwj;
 } while nextj;
}

static void regel5(void)
{
 
 initj;
 do {
 if ((curch == 'g'))    {
     gtracetask(rulefile_g1apho,5,1);
     curch = 'x';

   }

  forwj;
 } while nextj;
}

static void regel6(void)
{
 
 initj;
 do {
 if ( (((curch == 'M')) || ((curch == '!')) || ((curch == '~'))))   {
     gtracetask(rulefile_g1apho,6,1);
     curch = 'n';

   }

  forwj;
 } while nextj;
}

static void regel7(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpstem))) 
 if ((nexch == '-'))    {
     gtracetask(rulefile_g1apho,7,1);
     gverander(0,1,(&gmstem    ));
   }

  forwj;
 } while nextj;
}

static void regel8(void)
{
 
 initj;
 do {
 if ((
(inset(curch, gpvoc) && 
inset(curch,gmlang)))) 
 if ((
(inset(prech, gpvoc) && 
inset(prech,gmlang)))) 
 if (curch == prech)    {
     gtracetask(rulefile_g1apho,8,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel9(void)
{
 
 initj;
 do {
 if ((curch == 'N')) 
 if ((
(inset(prech, gpvoc) && 
inset(prech,gplang))))    {
     gtracetask(rulefile_g1apho,9,1);
     curch = 'n';

   }

  forwj;
 } while nextj;
}

static void regel10(void)
{
 
 initj;
 do {
 if ((curch == 'X'))    {
     gtracetask(rulefile_g1apho,10,1);
     curch = 'x';

   }

  forwj;
 } while nextj;
}

static void regel11(void)
{
 
 initj;
 do {
 if ((curch == '`'))    {
     gtracetask(rulefile_g1apho,11,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel12(void)
{
 
 initj;
 do {
 if ((curch == '-'))    {
     gtracetask(rulefile_g1apho,12,1);
gcorlengt(1);
     curch = ')';
     nexch = '(';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel13(void)
{
 
 initj;
 do { gy = 0; gx = 0;
{

 if ((offch(gx) == '\''))  gx = gx + 1;
 if ((
inset(offch(gx),gpseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gmseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g1apho,13,1);
gcorlengt(2);
     curch = '(';
     nexch = '(';

     gotoj(2);
   }
}}
  forwj;
 } while nextj;
}

static void regel14(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == '(')) {

 if ((offch(gx+1) == '\''))  gx = gx + 1;
 if ((
inset(offch(gx+1),gpseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == '('))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gmseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-2),gpseg)))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gmseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g1apho,14,1);
gcorlengt(-1);

     backj;
   }
}}}
  forwj;
 } while nextj;
}

static void regel15(void)
{
 
 initj;
 do { gx = 0;
 {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gx),gmseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((
inset(prech,gpseg)))    {
     gtracetask(rulefile_g1apho,15,1);
gcorlengt(2);
     curch = ')';
     nexch = ')';

     gotoj(2);
   }
}
  forwj;
 } while nextj;
}

static void regel16(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == ')')) 
 if ((
inset(offch(gy-1),gpseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-2) == '('))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g1apho,16,1);
gcorlengt(2);
     curch = ')';
     nexch = '0';

     gotoj(2);
   }
}
  forwj;
 } while nextj;
}

static void regel17(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == ')')) 
 if ((
inset(offch(gy-1),gpseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-2) == '\''))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g1apho,17,1);
gcorlengt(2);
     curch = ')';
     nexch = '1';

     gotoj(2);
   }
}
  forwj;
 } while nextj;
}

static void regel18(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == ')')) 
 if ((
inset(offch(gy-1),gpseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-2) == '>'))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g1apho,18,1);
gcorlengt(2);
     curch = ')';
     nexch = '2';

     gotoj(2);
   }
}
  forwj;
 } while nextj;
}

static void regel19(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(offch(gx),gpseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+1) == ')'))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((prech == '('))    {
     gtracetask(rulefile_g1apho,19,1);
gcorlengt(1);
     curch = '(';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel20(void)
{
 
 initj;
 do {
 if ( (((curch == '>')) || ((curch == '\''))))   {
     gtracetask(rulefile_g1apho,20,1);
     curch = '(';

   }

  forwj;
 } while nextj;
}

static void regel21(void)
{
 
 initj;
 do {
 if ( (((curch == 'I') && (nexch == 'I')) || ((curch == 'I') && 
(nexch == ':'))))
 if ( (((offch(2) == 'r')) || ((offch(2) == 'R'))))   {
     gtracetask(rulefile_g1apho,21,1);
gcorlengt(-1);
     curch = 'e';

   }

  forwj;
 } while nextj;
}

static void regel22(void)
{
 
 initj;
 do {
 if ( (((curch == 'U') && (nexch == 'U')) || ((curch == 'U') && 
(nexch == ':'))))
 if ( (((offch(2) == 'r')) || ((offch(2) == 'R'))))   {
     gtracetask(rulefile_g1apho,22,1);
gcorlengt(-1);
     curch = '@';

   }

  forwj;
 } while nextj;
}

static void regel23(void)
{
 
 initj;
 do {
 if ((curch == 'G'))    {
     gtracetask(rulefile_g1apho,23,1);
gcorlengt(1);
     curch = '?';
     nexch = '?';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel24(void)
{
 
 initj;
 do {
 if ( (((curch == 'X')) || ((curch == 'g'))))   {
     gtracetask(rulefile_g1apho,24,1);
     curch = 'G';

   }

  forwj;
 } while nextj;
}

static void regel25(void)
{
 
 initj;
 do {
 if ((curch == '?')) 
 if ((nexch == '?'))    {
     gtracetask(rulefile_g1apho,25,1);
gcorlengt(-1);
     curch = 'g';

   }

  forwj;
 } while nextj;
}

static void regel26(void)
{
 
 initj;
 do {
 if ((curch == 'E')) 
 if ((nexch == 'I'))    {
     gtracetask(rulefile_g1apho,26,1);
     curch = 'E';
     nexch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel27(void)
{
 
 initj;
 do {
 if ((curch == 'U')) 
 if ((nexch == 'I'))    {
     gtracetask(rulefile_g1apho,27,1);
     curch = 'Y';
     nexch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel28(void)
{
 
 initj;
 do {
 if ((curch == 'A')) 
 if ((nexch == 'U'))    {
     gtracetask(rulefile_g1apho,28,1);
     curch = 'A';
     nexch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel29(void)
{
 
 initj;
 do {
 if ((curch == 'U'))    {
     gtracetask(rulefile_g1apho,29,1);
     curch = 'Y';

   }

  forwj;
 } while nextj;
}

static void regel30(void)
{
 
 initj;
 do {
 if ((curch == '@'))    {
     gtracetask(rulefile_g1apho,30,1);
     curch = '2';

   }

  forwj;
 } while nextj;
}

static void regel31(void)
{
 
 initj;
 do {
 if ((curch == '&'))    {
     gtracetask(rulefile_g1apho,31,1);
     curch = '@';

   }

  forwj;
 } while nextj;
}

static void regel32(void)
{
 
 initj;
 do {
 if ((curch == 'j')) 
 if ( (((offch(-2) == 'o') && (prech == ':')) || ((offch(-2) == 'a') && 
(prech == ':')) || ((prech == 'u'))))   {
     gtracetask(rulefile_g1apho,32,1);
     curch = 'i';

   }

  forwj;
 } while nextj;
}

static void regel33(void)
{
 
 initj;
 do {
 if ((curch == 'w')) 
 if ( (((offch(-2) == 'e') && (prech == ':')) || ((prech == 'i'))))   {
     gtracetask(rulefile_g1apho,33,1);
     curch = 'u';

   }

  forwj;
 } while nextj;
}

static void regel34(void)
{
 
 initj;
 do {
 if ((curch == 'Y')) 
 if ((nexch == 'w'))    {
     gtracetask(rulefile_g1apho,34,1);
     curch = 'y';
     nexch = 'u';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel35(void)
{
 
 initj;
 do {
 if ((curch == 'd')) 
 if ((nexch == 'j'))    {
     gtracetask(rulefile_g1apho,35,1);
     curch = 'd';
     nexch = 'Z';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel36(void)
{
 
 initj;
 do {
 if ((curch == 'L'))    {
     gtracetask(rulefile_g1apho,36,1);
     curch = 'l';

   }

  forwj;
 } while nextj;
}

static void regel37(void)
{
 
 initj;
 do {
 if ((curch == 'R'))    {
     gtracetask(rulefile_g1apho,37,1);
     curch = 'r';

   }

  forwj;
 } while nextj;
}

static void regel38(void)
{
 
 initj;
 do {
 if ((curch == 'C'))    {
     gtracetask(rulefile_g1apho,38,1);
gcorlengt(1);
     curch = 't';
     nexch = 'j';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel39(void)
{
 
 initj;
 do {
 if ((curch == '!'))    {
     gtracetask(rulefile_g1apho,39,1);
     curch = 'J';

   }

  forwj;
 } while nextj;
}

static void regel40(void)
{
 
 initj;
 do {
 if ( (((curch == 'M')) || ((curch == '~'))))   {
     gtracetask(rulefile_g1apho,40,1);
     curch = 'n';

   }

  forwj;
 } while nextj;
}

static void regel41(void)
{
 
 initj;
 do {
 if ((curch == 'c'))    {
     gtracetask(rulefile_g1apho,41,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel42(void)
{
 
 initj;
 do {
 if ((
inset(curch,gklank))) 
 if ( (((offch(-2) == 'O') && (prech == '~')) || ((offch(-2) == 'A') && 
(prech == '~')) || ((offch(-2) == 'Y') && (prech == '~')) || (
(offch(-2) == 'E') && (prech == '~')) || ((offch(-2) == 'O') && 
(prech == ':')) || ((offch(-2) == 'Y') && (prech == ':')) || (
(offch(-2) == 'E') && (prech == ':')) || ((offch(-2) == 'A') && 
(prech == '+')) || ((offch(-2) == 'Y') && (prech == '+')) || (
(offch(-2) == 'E') && (prech == '+'))))   {
     gtracetask(rulefile_g1apho,42,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel43(void)
{
 
 initj;
 do {
 if ( (((
inset(curch,gpseg))) || (
(curch == '@')) || ((curch == 'J')) || ((curch == 'L')) || (
(curch == 'R')) || ((curch == 'Y')) || ((curch == '9')) || (
(curch == '2'))))
 if ( (((prech == '2')) || ((prech == '9')) || ((prech == 'Y')) || (
(prech == 'R')) || ((prech == 'L')) || ((prech == 'J')) || (
(prech == '@')) || ((
inset(prech,gpseg)))
))   {
     gtracetask(rulefile_g1apho,43,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

/*--- STRING RULES ---------------------------------------------*/

void fono_grapho_1_1()
{
 regel1(); regel2(); regel3(); regel4(); regel5(); regel6(); regel7(); regel8();
 regel9(); regel10(); regel11(); regel12(); regel13(); regel14(); regel15(); regel16();
 regel17(); regel18(); regel19(); regel20(); regel21(); regel22(); regel23(); regel24();
 regel25(); regel26(); regel27(); regel28(); regel29(); regel30(); regel31(); regel32();
 regel33(); regel34(); regel35(); regel36(); regel37(); regel38(); regel39(); regel40();
 regel41(); regel42(); regel43();}

/*--- PARAMETRISATION RULES ------------------------------------*/
void fone_grapho_1_1()
{
}



