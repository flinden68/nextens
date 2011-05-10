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

void ovl_ppldp0_1()
{
}


static void regel1(void)
{
 
 initj;
 do { gx = 0;

 if ( (((curch == '"')) || ((curch == '`')) || ((curch == '\'')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+1) == '.') && (offch(gx+2) == ' ') && (offch(gx+3) == ' ') && 
(offch(gx+4) == ' ')) || ((offch(gx+1) == ',') && (offch(gx+2) == ' ')) || (
(offch(gx+1) == '?')) || ((offch(gx+1) == '!')) || ((offch(gx+1) == ':')) || (
(offch(gx+1) == ';')) || ((offch(gx+1) == '"')) || ((offch(gx+1) == '`')) || (
(offch(gx+1) == '\'')) || ((offch(gx+1) == ' '))))   gstop = true;
      else {

 if ((
inset(offch(gx+1),gmseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_p0ldp,1,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel2(void)
{
 
 initj;
 do {
 if ( (((prech == '^')) || ((prech == '`'))))
 if ((offch(-2) == ' '))    {
     gtracetask(rulefile_p0ldp,2,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel3(void)
{
 
 initj;
 do {
 if ((curch == 'u')) 
 if ((
inset(nexch,gpcijf))) 
 if ((
inset(prech,gpcijf)))    {
     gtracetask(rulefile_p0ldp,3,1);
gcorlengt(4);
     curch = ' ';
     nexch = 'u';
     offch(2) = 'u';
     offch(3) = 'r';
     offch(4) = ' ';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel4(void)
{
 
 initj;
 do {
 if ((curch == '-')) 
 if (!(((
inset(nexch,gmhoofd))) || (
(
inset(nexch,gphoofd)))))
 if (!(((
inset(prech,gphoofd))) || (
(
inset(prech,gmhoofd)))))   {
     gtracetask(rulefile_p0ldp,4,1);
gcorlengt(9);
     curch = ' ';
     nexch = 's';
     offch(2) = 't';
     offch(3) = 'r';
     offch(4) = 'e';
     offch(5) = 'e';
     offch(6) = 'p';
     offch(7) = 'j';
     offch(8) = 'e';
     offch(9) = ' ';

     gotoj(9);
   }

  forwj;
 } while nextj;
}

static void regel5(void)
{
 
 initj;
 do {
 if ((curch == '-'))    {
     gtracetask(rulefile_p0ldp,5,1);
     curch = ' ';

   }

  forwj;
 } while nextj;
}

static void regel6(void)
{
 
 initj;
 do {
 if ( (((curch == ',') && (nexch == ',')) || ((curch == '\'') && 
(nexch == '\''))))   {
     gtracetask(rulefile_p0ldp,6,1);
gcorlengt(-1);
     curch = ' ';

   }

  forwj;
 } while nextj;
}

static void regel7(void)
{
 
 initj;
 do {
 if ((curch == '.')) 
 if ((prech == '.'))    {
     gtracetask(rulefile_p0ldp,7,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel8(void)
{
 
 initj;
 do {
 if ( (((curch == '.') && (nexch == ' ') && (offch(2) == ' ') && 
(offch(3) == ' ')) || ((curch == ',') && (nexch == ' ')) || (
(curch == '?')) || ((curch == '!')) || ((curch == ':')) || (
(curch == ';'))))
 if ( (((
inset(prech,gpcijf))) || (
(
inset(prech,gphoofd))) || (
(
inset(prech,gmhoofd)))))   {
     gtracetask(rulefile_p0ldp,8,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel9(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpcijf))) 
 if ( (((
inset(prech,gmhoofd))) || (
(
inset(prech,gphoofd)))))   {
     gtracetask(rulefile_p0ldp,9,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel10(void)
{
 
 initj;
 do {
 if ( (((
inset(curch,gphoofd))) || (
(
inset(curch,gmhoofd)))))
 if ((
inset(prech,gpcijf)))    {
     gtracetask(rulefile_p0ldp,10,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel11(void)
{
 
 initj;
 do {
 if ((
inset(curch,gphoofd))) 
 if ((
inset(nexch,gmhoofd))) 
 if ((prech == ' ')) 
 if ((offch(-2) == '.'))    {
     gtracetask(rulefile_p0ldp,11,1);
     ghoofdklein(0,-1);

   }

  forwj;
 } while nextj;
}

static void regel12(void)
{
 
 initj;
 do {
 if ((
inset(curch,gphoofd))) 
 if ((prech == ' ')) 
 if (!(((offch(-2) == '^')) || ((offch(-2) == '`'))))   {
     gtracetask(rulefile_p0ldp,12,1);
gcorlengt(2);
     curch = '`';
     nexch = ' ';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel13(void)
{
 
 initj;
 do {
 if ((curch == '-')) 
 if ((
inset(prech,gphoofd))) 
 if ((nexch == 'e')) 
 if ( (((offch(2) == 'r') && (offch(3) == ' ')) || ((offch(2) == 'r') && 
(offch(3) == 's') && (offch(4) == ' ')) || ((offch(2) == 'n') && 
(offch(3) == ' '))))   {
     gtracetask(rulefile_p0ldp,13,1);
     curch = '\'';

   }

  forwj;
 } while nextj;
}

static void regel14(void)
{
 
 initj;
 do {
 if ( (((curch == '.')) || ((curch == ' '))))
 if ((
inset(nexch,gpcijf))) 
 if ((
inset(offch(2),gpcijf))) 
 if ((
inset(offch(3),gpcijf))) 
 if ( (((offch(4) == '.')) || ((offch(4) == ' '))))
 if ((
inset(prech,gpcijf)))    {
     gtracetask(rulefile_p0ldp,14,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel15(void)
{
 
 initj;
 do {
 if ((curch == 'm')) 
 if ((nexch == ' ')) 
 if ( (((prech == 'K')) || ((prech == 'k'))))
 if ((offch(-2) == ' '))    {
     gtracetask(rulefile_p0ldp,15,1);
gcorlengt(7);
     curch = 'i';
     nexch = 'l';
     offch(2) = 'o';
     offch(3) = 'm';
     offch(4) = 'e';
     offch(5) = 't';
     offch(6) = 'e';
     offch(7) = 'r';

     gotoj(7);
   }

  forwj;
 } while nextj;
}

static void regel16(void)
{
 
 initj;
 do {
 if ( (((curch == 'n') && (nexch == 'r')) || ((curch == 'N') && 
(nexch == 'R'))))
 if ((offch(2) == '.')) 
 if ((offch(3) == ' ')) 
 if ((
inset(offch(4),gpcijf)))    {
     gtracetask(rulefile_p0ldp,16,1);
gcorlengt(3);
     curch = 'n';
     nexch = 'u';
     offch(2) = 'm';
     offch(3) = 'm';
     offch(4) = 'e';
     offch(5) = 'r';
     offch(6) = ' ';

     gotoj(6);
   }

  forwj;
 } while nextj;
}

static void regel17(void)
{
 
 initj;
 do {
 if ( (((curch == 't') && (nexch == 'e') && (offch(2) == 'l')) || (
(curch == 'T') && (nexch == 'E') && (offch(2) == 'L'))))
 if ((offch(3) == '.')) 
 if ((offch(4) == ' ')) 
 if ((
inset(offch(5),gpcijf)))    {
     gtracetask(rulefile_p0ldp,17,1);
gcorlengt(4);
     curch = 't';
     nexch = 'e';
     offch(2) = 'l';
     offch(3) = 'e';
     offch(4) = 'f';
     offch(5) = 'o';
     offch(6) = 'o';
     offch(7) = 'n';
     offch(8) = ' ';

     gotoj(8);
   }

  forwj;
 } while nextj;
}

static void regel18(void)
{
 
 initj;
 do {
 if ( (((curch == 'r') && (nexch == 'e') && (offch(2) == 'k')) || (
(curch == 'R') && (nexch == 'E') && (offch(2) == 'K'))))
 if ((offch(3) == '.')) 
 if ((offch(4) == ' ')) 
 if ((
inset(offch(5),gpcijf)))    {
     gtracetask(rulefile_p0ldp,18,1);
gcorlengt(4);
     curch = 'r';
     nexch = 'e';
     offch(2) = 'k';
     offch(3) = 'e';
     offch(4) = 'n';
     offch(5) = 'i';
     offch(6) = 'n';
     offch(7) = 'g';
     offch(8) = ' ';

     gotoj(8);
   }

  forwj;
 } while nextj;
}

static void regel19(void)
{
 
 initj;
 do {
 if ( (((curch == '.')) || ((curch == ':'))))
 if ((
inset(nexch,gpcijf))) 
 if ((
inset(offch(2),gpcijf))) 
 if ((offch(3) == ' ')) 
 if ( (((offch(4) == 'u') && (offch(5) == '.') && (offch(6) == ' ')) || (
(offch(4) == 'u') && (offch(5) == 'u') && (offch(6) == 'r'))))   {
     gtracetask(rulefile_p0ldp,19,1);
gcorlengt(6);
     curch = ' ';
     nexch = '^';
     offch(2) = ' ';
     offch(3) = 'u';
     offch(4) = 'u';
     offch(5) = 'r';
     offch(6) = ' ';

     gotoj(6);
   }

  forwj;
 } while nextj;
}

static void regel20(void)
{
 
 initj;
 do {
 if ( (((curch == 'u') && (nexch == '.') && (offch(2) == ' ')) || (
(curch == 'u') && (nexch == 'u') && (offch(2) == 'r'))))
 if ((prech == ' ')) 
 if ((
inset(offch(-2),gpcijf))) 
 if ((
inset(offch(-3),gpcijf))) 
 if ((offch(-4) == ' ')) 
 if ((offch(-5) == 'r')) 
 if ((offch(-6) == 'u')) 
 if ((offch(-7) == 'u'))    {
     gtracetask(rulefile_p0ldp,20,1);
gcorlengt(-3);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel21(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == ',')) 
 if ((
inset(offch(gy-1),gpcijf)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gy-5) == ' ') && (offch(gy-4) == 'f') && (offch(gy-3) == '.') && 
(offch(gy-2) == ' ')) || ((offch(gy-4) == ' ') && (offch(gy-3) == 'f') && 
(offch(gy-2) == ' ')) || ((offch(gy-4) == 'f') && (offch(gy-3) == 'l') && 
(offch(gy-2) == ' ')) || ((offch(gy-5) == 'f') && (offch(gy-4) == 'l') && 
(offch(gy-3) == '.') && (offch(gy-2) == ' ')) || ((offch(gy-4) == ' ') && 
(offch(gy-3) == 'f') && (offch(gy-2) == '.')) || ((offch(gy-3) == ' ') && 
(offch(gy-2) == 'f')) || ((offch(gy-3) == 'f') && (offch(gy-2) == 'l')) || (
(offch(gy-4) == 'f') && (offch(gy-3) == 'l') && (offch(gy-2) == '.'))))   gstop = true;
      else {

 if ((
inset(offch(gy-2),gpcijf))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_p0ldp,21,1);
gcorlengt(9);
     curch = ' ';
     nexch = '^';
     offch(2) = ' ';
     offch(3) = 'g';
     offch(4) = 'u';
     offch(5) = 'l';
     offch(6) = 'd';
     offch(7) = 'e';
     offch(8) = 'n';
     offch(9) = ' ';

     gotoj(9);
   }
}
  forwj;
 } while nextj;
}

static void regel22(void)
{
 
 initj;
 do {
 if ((curch == '0')) 
 if ((nexch == '0')) 
 if ((offch(2) == ' ')) 
 if ((prech == ' ')) 
 if ( (((offch(-7) == 'g') && (offch(-6) == 'u') && (offch(-5) == 'l') && 
(offch(-4) == 'd') && (offch(-3) == 'e') && (offch(-2) == 'n')) || (
(offch(-4) == 'u') && (offch(-3) == 'u') && (offch(-2) == 'r'))))   {
     gtracetask(rulefile_p0ldp,22,1);
gcorlengt(-3);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel23(void)
{
 
 initj;
 do {
 if ((curch == '0')) 
 if ((
inset(nexch,gpcijf))) 
 if ((offch(2) == ' ')) 
 if ((prech == ' ')) 
 if ( (((offch(-7) == 'g') && (offch(-6) == 'u') && (offch(-5) == 'l') && 
(offch(-4) == 'd') && (offch(-3) == 'e') && (offch(-2) == 'n')) || (
(offch(-4) == 'u') && (offch(-3) == 'u') && (offch(-2) == 'r'))))   {
     gtracetask(rulefile_p0ldp,23,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel24(void)
{
 
 initj;
 do { gx = 0;

 if ( (((curch == 'f')) || ((curch == 'l')) || ((curch == '.'))))
 if ((prech == ' ')) {

 if ((offch(gx+1) == 'l'))  gx = gx + 1;{

 if ((offch(gx+1) == '.'))  gx = gx + 1;{

 if ((offch(gx+1) == ' '))  gx = gx + 1;
 if ((
inset(offch(gx+1),gpcijf)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+2) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gx+2),gpcijf))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((offch(gx+3) == '^')) 
 if ((offch(gx+4) == ' ')) 
 if ((offch(gx+5) == 'g')) 
 if ((offch(gx+6) == 'u'))    {
     gtracetask(rulefile_p0ldp,24,1);
gcorlengt(-1);

     backj;
   }
}}}}
  forwj;
 } while nextj;
}

static void regel25(void)
{
 
 initj;
 do {
 if ((
inset(prech,gpcijf))) 
 if ((offch(-2) == ' ')) 
 if ( (((
inset(offch(-3),gpcijf))) || (
(offch(-6) == 'm') && (offch(-5) == 'm') && (offch(-4) == 'e') && 
(offch(-3) == 'r')) || ((offch(-6) == 'g') && (offch(-5) == 'i') && 
(offch(-4) == 'r') && (offch(-3) == 'o')) || ((offch(-6) == 'f') && 
(offch(-5) == 'o') && (offch(-4) == 'o') && (offch(-3) == 'n')) || (
(offch(-7) == 'e') && (offch(-6) == 'n') && (offch(-5) == 'i') && 
(offch(-4) == 'n') && (offch(-3) == 'g'))))   {
     gtracetask(rulefile_p0ldp,25,1);
gcorlengt(1);
     curch = ' ';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel26(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == '0')) 
 if ((
inset(offch(gx+1),gpcijf)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+2) == '/'))    gstop = true;
      else {

 if ((
inset(offch(gx+2),gpcijf))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ( (((prech == '/')) || ((prech == ' '))))   {
     gtracetask(rulefile_p0ldp,26,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel27(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '1')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,27,1);
gcorlengt(6);
     curch = ' ';
     nexch = 'j';
     offch(2) = 'a';
     offch(3) = 'n';
     offch(4) = 'u';
     offch(5) = 'a';
     offch(6) = 'r';
     offch(7) = 'i';
     offch(8) = ' ';

     gotoj(8);
   }

  forwj;
 } while nextj;
}

static void regel28(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '2')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,28,1);
gcorlengt(7);
     curch = ' ';
     nexch = 'f';
     offch(2) = 'e';
     offch(3) = 'b';
     offch(4) = 'r';
     offch(5) = 'u';
     offch(6) = 'a';
     offch(7) = 'r';
     offch(8) = 'i';
     offch(9) = ' ';

     gotoj(9);
   }

  forwj;
 } while nextj;
}

static void regel29(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '3')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,29,1);
gcorlengt(4);
     curch = ' ';
     nexch = 'm';
     offch(2) = 'a';
     offch(3) = 'a';
     offch(4) = 'r';
     offch(5) = 't';
     offch(6) = ' ';

     gotoj(6);
   }

  forwj;
 } while nextj;
}

static void regel30(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '4')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,30,1);
gcorlengt(4);
     curch = ' ';
     nexch = 'a';
     offch(2) = 'p';
     offch(3) = 'r';
     offch(4) = 'i';
     offch(5) = 'l';
     offch(6) = ' ';

     gotoj(6);
   }

  forwj;
 } while nextj;
}

static void regel31(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '5')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,31,1);
gcorlengt(2);
     curch = ' ';
     nexch = 'm';
     offch(2) = 'e';
     offch(3) = 'i';
     offch(4) = ' ';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel32(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '6')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,32,1);
gcorlengt(3);
     curch = ' ';
     nexch = 'j';
     offch(2) = 'u';
     offch(3) = 'n';
     offch(4) = 'i';
     offch(5) = ' ';

     gotoj(5);
   }

  forwj;
 } while nextj;
}

static void regel33(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '7')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,33,1);
gcorlengt(3);
     curch = ' ';
     nexch = 'j';
     offch(2) = 'u';
     offch(3) = 'l';
     offch(4) = 'i';
     offch(5) = ' ';

     gotoj(5);
   }

  forwj;
 } while nextj;
}

static void regel34(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '8')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,34,1);
gcorlengt(7);
     curch = ' ';
     nexch = 'a';
     offch(2) = 'u';
     offch(3) = 'g';
     offch(4) = 'u';
     offch(5) = 's';
     offch(6) = 't';
     offch(7) = 'u';
     offch(8) = 's';
     offch(9) = ' ';

     gotoj(9);
   }

  forwj;
 } while nextj;
}

static void regel35(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '9')) 
 if ((offch(2) == '/'))    {
     gtracetask(rulefile_p0ldp,35,1);
gcorlengt(8);
     curch = ' ';
     nexch = 's';
     offch(2) = 'e';
     offch(3) = 'p';
     offch(4) = 't';
     offch(5) = 'e';
     offch(6) = 'm';
     offch(7) = 'b';
     offch(8) = 'e';
     offch(9) = 'r';
     offch(10) = ' ';

     gotoj(10);
   }

  forwj;
 } while nextj;
}

static void regel36(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '1')) 
 if ((offch(2) == '0')) 
 if ((offch(3) == '/'))    {
     gtracetask(rulefile_p0ldp,36,1);
gcorlengt(5);
     curch = ' ';
     nexch = 'o';
     offch(2) = 'k';
     offch(3) = 't';
     offch(4) = 'o';
     offch(5) = 'b';
     offch(6) = 'e';
     offch(7) = 'r';
     offch(8) = ' ';

     gotoj(8);
   }

  forwj;
 } while nextj;
}

static void regel37(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '1')) 
 if ((offch(2) == '1')) 
 if ((offch(3) == '/'))    {
     gtracetask(rulefile_p0ldp,37,1);
gcorlengt(6);
     curch = ' ';
     nexch = 'n';
     offch(2) = 'o';
     offch(3) = 'v';
     offch(4) = 'e';
     offch(5) = 'm';
     offch(6) = 'b';
     offch(7) = 'e';
     offch(8) = 'r';
     offch(9) = ' ';

     gotoj(9);
   }

  forwj;
 } while nextj;
}

static void regel38(void)
{
 
 initj;
 do {
 if ((curch == '/')) 
 if ((nexch == '1')) 
 if ((offch(2) == '2')) 
 if ((offch(3) == '/'))    {
     gtracetask(rulefile_p0ldp,38,1);
gcorlengt(6);
     curch = ' ';
     nexch = 'd';
     offch(2) = 'e';
     offch(3) = 'c';
     offch(4) = 'e';
     offch(5) = 'm';
     offch(6) = 'b';
     offch(7) = 'e';
     offch(8) = 'r';
     offch(9) = ' ';

     gotoj(9);
   }

  forwj;
 } while nextj;
}

static void regel39(void)
{
 
 initj;
 do {
 if ((curch == ',')) 
 if ((
inset(nexch,gpcijf)))    {
     gtracetask(rulefile_p0ldp,39,1);
gcorlengt(6);
     curch = ' ';
     nexch = 'k';
     offch(2) = 'o';
     offch(3) = 'm';
     offch(4) = 'm';
     offch(5) = 'a';
     offch(6) = ' ';

     gotoj(6);
   }

  forwj;
 } while nextj;
}

static void regel40(void)
{
 
 initj;
 do {
 if ((curch == '.')) 
 if ((
inset(nexch,gpcijf)))    {
     gtracetask(rulefile_p0ldp,40,1);
gcorlengt(5);
     curch = ' ';
     nexch = 'p';
     offch(2) = 'u';
     offch(3) = 'n';
     offch(4) = 't';
     offch(5) = ' ';

     gotoj(5);
   }

  forwj;
 } while nextj;
}

static void regel41(void)
{
 
 initj;
 do {
 if ((curch == '0')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_p0ldp,41,1);
gcorlengt(3);
     curch = 'n';
     nexch = 'u';
     offch(2) = 'l';
     offch(3) = ' ';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel42(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == ' ')) 
 if ((
inset(offch(gx+1),gprom)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+2) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gx+2),gprom))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ( (((offch(-4) == 'A') && (offch(-3) == '.') && (offch(-2) == 'D') && 
(prech == '.')) || ((offch(-4) == 'd') && (offch(-3) == 'e') && 
(offch(-2) == 'e') && (prech == 'l')) || ((offch(-6) == 'p') && 
(offch(-5) == 'a') && (offch(-4) == 'g') && (offch(-3) == 'i') && 
(offch(-2) == 'n') && (prech == 'a')) || ((offch(-6) == 'o') && 
(offch(-5) == 'o') && (offch(-4) == 'r') && (offch(-3) == 'l') && 
(offch(-2) == 'o') && (prech == 'g'))))   {
     gtracetask(rulefile_p0ldp,42,1);
gcorlengt(1);
     curch = ' ';
     nexch = '@';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel43(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(curch,gprom))) 
 if ((
inset(nexch,gprom))) 
 if ((
inset(offch(2),gprom))) 
 if ((
inset(offch(gx+3),gprom))) 
 if ((
inset(offch(gx+4),gprom)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+5) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gx+5),gprom))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((prech == ' ')) 
 if (nexch != offch(2))    {
     gtracetask(rulefile_p0ldp,43,1);
gcorlengt(1);
     curch = '@';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel44(void)
{
 
 initj;
 do {
 if ((curch == '@'))    {
     gtracetask(rulefile_p0ldp,44,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel45(void)
{
 
 initj;
 do {
 if ((curch == ' ')) 
 if ( (((
((inset(offch(-2),gklank) && (offch(-2)!='I')))) && 
(prech == 'X')) || ((prech == 'L')) || ((prech == 'C')) || (
(prech == 'D')) || ((prech == 'M'))))   {
     gtracetask(rulefile_p0ldp,45,1);
gcorlengt(1);
     curch = '0';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel46(void)
{
 
 initj;
 do {
 if ( (((curch == 'I')) || ((curch == 'V')) || ((curch == '0'))))
 if ( (((
((inset(offch(-2),gklank) && (offch(-2)!='X')))) && 
(prech == 'C')) || ((prech == 'D')) || ((prech == 'M'))))   {
     gtracetask(rulefile_p0ldp,46,1);
gcorlengt(1);
     curch = '0';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel47(void)
{
 
 initj;
 do {
 if ( (((curch == '0')) || ((curch == 'X')) || ((curch == 'L'))))
 if ((prech == 'M')) 
 if (inset(offch(-2),gklank))
 if (   (
((inset(offch(-2),gklank) && (offch(-2)!='C')))) )    {
     gtracetask(rulefile_p0ldp,47,1);
gcorlengt(1);
     curch = '0';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel48(void)
{
 
 initj;
 do {
 if ( (((curch == 'I') && (nexch == 'V')) || ((curch == 'X') && 
(nexch == 'L')) || ((curch == 'C') && (nexch == 'D'))))   {
     gtracetask(rulefile_p0ldp,48,1);
gcorlengt(-1);
     curch = '4';

   }

  forwj;
 } while nextj;
}

static void regel49(void)
{
 
 initj;
 do {
 if ( (((curch == 'I') && (nexch == 'X')) || ((curch == 'X') && 
(nexch == 'C')) || ((curch == 'C') && (nexch == 'M'))))   {
     gtracetask(rulefile_p0ldp,49,1);
gcorlengt(-1);
     curch = '9';

   }

  forwj;
 } while nextj;
}

static void regel50(void)
{
 
 initj;
 do {
 if ( (((curch == 'V') && (nexch == 'I') && (offch(2) == 'I') && 
(offch(3) == 'I')) || ((curch == 'L') && (nexch == 'X') && 
(offch(2) == 'X') && (offch(3) == 'X')) || ((curch == 'D') && 
(nexch == 'C') && (offch(2) == 'C') && (offch(3) == 'C'))))   {
     gtracetask(rulefile_p0ldp,50,1);
gcorlengt(-3);
     curch = '8';

   }

  forwj;
 } while nextj;
}

static void regel51(void)
{
 
 initj;
 do {
 if ( (((curch == 'V') && (nexch == 'I') && (offch(2) == 'I')) || (
(curch == 'L') && (nexch == 'X') && (offch(2) == 'X')) || (
(curch == 'D') && (nexch == 'C') && (offch(2) == 'C'))))   {
     gtracetask(rulefile_p0ldp,51,1);
gcorlengt(-2);
     curch = '7';

   }

  forwj;
 } while nextj;
}

static void regel52(void)
{
 
 initj;
 do {
 if ( (((curch == 'V') && (nexch == 'I')) || ((curch == 'L') && 
(nexch == 'X')) || ((curch == 'D') && (nexch == 'C'))))   {
     gtracetask(rulefile_p0ldp,52,1);
gcorlengt(-1);
     curch = '6';

   }

  forwj;
 } while nextj;
}

static void regel53(void)
{
 
 initj;
 do {
 if ( (((curch == 'V')) || ((curch == 'L')) || ((curch == 'D'))))   {
     gtracetask(rulefile_p0ldp,53,1);
     curch = '5';

   }

  forwj;
 } while nextj;
}

static void regel54(void)
{
 
 initj;
 do {
 if ( (((curch == 'I') && (nexch == 'I') && (offch(2) == 'I')) || (
(curch == 'X') && (nexch == 'X') && (offch(2) == 'X')) || (
(curch == 'C') && (nexch == 'C') && (offch(2) == 'C')) || (
(curch == 'M') && (nexch == 'M') && (offch(2) == 'M'))))   {
     gtracetask(rulefile_p0ldp,54,1);
gcorlengt(-2);
     curch = '3';

   }

  forwj;
 } while nextj;
}

static void regel55(void)
{
 
 initj;
 do {
 if ( (((curch == 'I') && (nexch == 'I')) || ((curch == 'X') && 
(nexch == 'X')) || ((curch == 'C') && (nexch == 'C')) || (
(curch == 'M') && (nexch == 'M'))))   {
     gtracetask(rulefile_p0ldp,55,1);
gcorlengt(-1);
     curch = '2';

   }

  forwj;
 } while nextj;
}

static void regel56(void)
{
 
 initj;
 do {
 if ( (((curch == 'I')) || ((curch == 'X')) || ((curch == 'C')) || (
(curch == 'M'))))   {
     gtracetask(rulefile_p0ldp,56,1);
     curch = '1';

   }

  forwj;
 } while nextj;
}

static void regel57(void)
{
 
 initj;
 do { gy = 0;

 if ((
inset(curch,gpcijf))) 
 if ((
inset(nexch,gpcijf))) 
 if ((
inset(offch(2),gpcijf))) 
 if ((
inset(offch(3),gpcijf))) 
 if ((
inset(offch(4),gpcijf))) 
 if ((
inset(offch(5),gpcijf))) 
 if ((
inset(offch(6),gpcijf))) 
 if ((
inset(offch(7),gpcijf))) 
 if ((
inset(offch(8),gpcijf))) 
 if ((!
inset(offch(9),gpcijf)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if (inset(offch(gy-1),gklank))
 if (   (
(!inset(offch(gy-1),gpseg     )) && 
((inset(offch(gy-1),gklank) && (offch(gy-1)!='0'))) && 
((inset(offch(gy-1),gklank) && (offch(gy-1)!=' ')))) )    gstop = true;
      else {

 if ((
inset(offch(gy-1),gpcijf))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_p0ldp,57,1);
gcorlengt(11);
     curch = ' ';
     nexch = '`';
     offch(2) = ' ';
     offch(3) = 'm';
     offch(4) = 'i';
     offch(5) = 'l';
     offch(6) = 'j';
     offch(7) = 'a';
     offch(8) = 'r';
     offch(9) = 'd';
     offch(10) = ' ';

     gotoj(11);
   }
}
  forwj;
 } while nextj;
}

static void regel58(void)
{
 
 initj;
 do { gy = 0;

 if ((
inset(curch,gpcijf))) 
 if ((
inset(nexch,gpcijf))) 
 if ((
inset(offch(2),gpcijf))) 
 if ((
inset(offch(3),gpcijf))) 
 if ((
inset(offch(4),gpcijf))) 
 if ((
inset(offch(5),gpcijf))) 
 if ((!
inset(offch(6),gpcijf)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if (inset(offch(gy-1),gklank))
 if (   (
(!inset(offch(gy-1),gpseg     )) && 
((inset(offch(gy-1),gklank) && (offch(gy-1)!='0'))) && 
((inset(offch(gy-1),gklank) && (offch(gy-1)!=' ')))) )    gstop = true;
      else {

 if ((
inset(offch(gy-1),gpcijf))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_p0ldp,58,1);
gcorlengt(11);
     curch = ' ';
     nexch = '`';
     offch(2) = ' ';
     offch(3) = 'm';
     offch(4) = 'i';
     offch(5) = 'l';
     offch(6) = 'j';
     offch(7) = 'o';
     offch(8) = 'e';
     offch(9) = 'n';
     offch(10) = ' ';

     gotoj(11);
   }
}
  forwj;
 } while nextj;
}

static void regel59(void)
{
 
 initj;
 do {
 if ((curch == '0')) 
 if ((
inset(nexch,gpcijf))) 
 if ((
inset(offch(2),gpcijf))) 
 if ((!
inset(offch(3),gpcijf))) 
 if (inset(prech,gklank))
 if (   (
(!inset(prech,gpseg     )) && 
((inset(prech,gklank) && (prech!='0'))) && 
((inset(prech,gklank) && (prech!=' ')))) )    {
     gtracetask(rulefile_p0ldp,59,1);
gcorlengt(11);
     curch = ' ';
     nexch = '`';
     offch(2) = ' ';
     offch(3) = 'd';
     offch(4) = 'u';
     offch(5) = 'i';
     offch(6) = 'z';
     offch(7) = 'e';
     offch(8) = 'n';
     offch(9) = 'd';
     offch(10) = ' ';

     gotoj(11);
   }

  forwj;
 } while nextj;
}

static void regel60(void)
{
 
 initj;
 do {
 if ( (((
inset(curch,gpcijf))) || (
(curch == '0'))))
 if ((
inset(nexch,gpcijf))) 
 if ((
inset(offch(2),gpcijf))) 
 if ((!
inset(offch(3),gpcijf))) 
 if ( (((
inset(offch(-2),gpcijf)) && 
(
(!inset(prech,gpseg     )) && 
((inset(prech,gklank) && (prech!='0'))) && 
((inset(prech,gklank) && (prech!=' '))))) || (
(
(!inset(offch(-3),gpseg     )) && 
((inset(offch(-3),gklank) && (offch(-3)!='0'))) && 
((inset(offch(-3),gklank) && (offch(-3)!=' ')))) && 
(
inset(offch(-2),gpcijf)) && 
(
inset(prech,gpcijf))) || (
(
(!inset(offch(-2),gpseg     )) && 
((inset(offch(-2),gklank) && (offch(-2)!='0'))) && 
((inset(offch(-2),gklank) && (offch(-2)!=' ')))) && 
(
inset(prech,gpcijf)))))   {
     gtracetask(rulefile_p0ldp,60,1);
gcorlengt(11);
     curch = ' ';
     nexch = '`';
     offch(2) = ' ';
     offch(3) = 'd';
     offch(4) = 'u';
     offch(5) = 'i';
     offch(6) = 'z';
     offch(7) = 'e';
     offch(8) = 'n';
     offch(9) = 'd';
     offch(10) = ' ';

     gotoj(11);
   }

  forwj;
 } while nextj;
}

static void regel61(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpcijf))) 
 if ((
inset(nexch,gpcijf))) 
 if ((!
inset(offch(2),gpcijf))) 
 if (inset(prech,gklank))
 if (   (
(!inset(prech,gpseg     )) && 
((inset(prech,gklank) && (prech!='0'))) && 
((inset(prech,gklank) && (prech!=' ')))) )    {
     gtracetask(rulefile_p0ldp,61,1);
gcorlengt(11);
     curch = ' ';
     nexch = '`';
     offch(2) = ' ';
     offch(3) = 'h';
     offch(4) = 'o';
     offch(5) = 'n';
     offch(6) = 'd';
     offch(7) = 'e';
     offch(8) = 'r';
     offch(9) = 'd';
     offch(10) = ' ';

     gotoj(11);
   }

  forwj;
 } while nextj;
}

static void regel62(void)
{
 
 initj;
 do {
 if ((curch == '1')) 
 if ((nexch == ' ')) 
 if ((offch(2) == '`')) 
 if ((offch(3) == ' ')) 
 if ( (((offch(4) == 'd')) || ((offch(4) == 'h'))))
 if ((prech == ' '))    {
     gtracetask(rulefile_p0ldp,62,1);
gcorlengt(-2);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel63(void)
{
 
 initj;
 do { gx = 0;
 {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if (inset(offch(gx),gklank))
 if (   (
(!inset(offch(gx),gpseg     )) && 
((inset(offch(gx),gklank) && (offch(gx)!='0'))) && 
((inset(offch(gx),gklank) && (offch(gx)!=' ')))) )    gstop = true;
      else {

 if ((
inset(offch(gx),gpcijf))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((prech == ' ')) 
 if (!((offch(-2) == '`')))    {
     gtracetask(rulefile_p0ldp,63,1);
gcorlengt(2);
     curch = '`';
     nexch = ' ';

     gotoj(2);
   }
}
  forwj;
 } while nextj;
}

static void regel64(void)
{
 
 initj;
 do {
 if ((curch == '1')) 
 if ((nexch == '1'))    {
     gtracetask(rulefile_p0ldp,64,1);
gcorlengt(1);
     curch = 'e';
     nexch = 'l';
     offch(2) = 'f';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel65(void)
{
 
 initj;
 do {
 if ((curch == '1')) 
 if ((nexch == '2'))    {
     gtracetask(rulefile_p0ldp,65,1);
gcorlengt(4);
     curch = 't';
     nexch = 'w';
     offch(2) = 'a';
     offch(3) = 'a';
     offch(4) = 'l';
     offch(5) = 'f';

     gotoj(5);
   }

  forwj;
 } while nextj;
}

static void regel66(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpcijf))) 
 if ((
inset(nexch,gpcijf))) 
 if ((!
inset(offch(2),gpcijf))) 
 if (curch != nexch)    {
     gtracetask(rulefile_p0ldp,66,1);
     gA   = curch;
     curch = nexch;
     nexch = gA;
     forwj;

   }

  forwj;
 } while nextj;
}

static void regel67(void)
{
 
 initj;
 do {
 if (inset(prech,gklank))
 if (   (
(!inset(prech,gpseg     )) && 
((inset(prech,gklank) && (prech!='0'))) && 
((inset(prech,gklank) && (prech!=' ')))) ) 
 if ((
inset(offch(-2),gpcijf))) 
 if ((!
inset(curch,gpcijf)))    {
     gtracetask(rulefile_p0ldp,67,1);
gcorlengt(3);
     curch = 't';
     nexch = 'i';
     offch(2) = 'g';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel68(void)
{
 
 initj;
 do {
 if ((curch == '1')) 
 if ((nexch == 't')) 
 if ((offch(2) == 'i')) 
 if ((offch(3) == 'g'))    {
     gtracetask(rulefile_p0ldp,68,1);
     curch = 't';
     nexch = 'i';
     offch(2) = 'e';
     offch(3) = 'n';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel69(void)
{
 
 initj;
 do {
 if ((curch == '0'))    {
     gtracetask(rulefile_p0ldp,69,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel70(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpcijf))) 
 if ((
inset(prech,gpcijf)))    {
     gtracetask(rulefile_p0ldp,70,1);
gcorlengt(3);
     curch = 'e';
     nexch = 'n';
     offch(2) = ' ';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel71(void)
{
 
 initj;
 do {
 if ((curch == '1'))    {
     gtracetask(rulefile_p0ldp,71,1);
gcorlengt(3);
     curch = 'e';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 'n';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel72(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpcijf))) 
 if ((nexch == 't')) 
 if ((offch(2) == 'i')) 
 if ((offch(3) == 'g')) 
 if ((prech == ' ')) 
 if ((offch(-2) == 'n')) 
 if ((offch(-3) == 'e'))    {
     gtracetask(rulefile_p0ldp,72,1);
gcorlengt(2);
     curch = '`';
     nexch = ' ';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel73(void)
{
 
 initj;
 do {
 if ((curch == '2')) 
 if ((nexch == 't'))    {
     gtracetask(rulefile_p0ldp,73,1);
gcorlengt(3);
     curch = 't';
     nexch = 'w';
     offch(2) = 'i';
     offch(3) = 'n';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel74(void)
{
 
 initj;
 do {
 if ((curch == '2'))    {
     gtracetask(rulefile_p0ldp,74,1);
gcorlengt(3);
     curch = 't';
     nexch = 'w';
     offch(2) = 'e';
     offch(3) = 'e';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel75(void)
{
 
 initj;
 do {
 if ((curch == '3')) 
 if ((nexch == 't'))    {
     gtracetask(rulefile_p0ldp,75,1);
gcorlengt(2);
     curch = 'd';
     nexch = 'e';
     offch(2) = 'r';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel76(void)
{
 
 initj;
 do {
 if ((curch == '3'))    {
     gtracetask(rulefile_p0ldp,76,1);
gcorlengt(3);
     curch = 'd';
     nexch = 'r';
     offch(2) = 'i';
     offch(3) = 'e';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel77(void)
{
 
 initj;
 do {
 if ((curch == '4')) 
 if ((nexch == 't'))    {
     gtracetask(rulefile_p0ldp,77,1);
gcorlengt(3);
     curch = 'v';
     nexch = 'e';
     offch(2) = 'e';
     offch(3) = 'r';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel78(void)
{
 
 initj;
 do {
 if ((curch == '4'))    {
     gtracetask(rulefile_p0ldp,78,1);
gcorlengt(3);
     curch = 'v';
     nexch = 'i';
     offch(2) = 'e';
     offch(3) = 'r';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel79(void)
{
 
 initj;
 do {
 if ((curch == '5'))    {
     gtracetask(rulefile_p0ldp,79,1);
gcorlengt(3);
     curch = 'v';
     nexch = 'i';
     offch(2) = 'j';
     offch(3) = 'f';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel80(void)
{
 
 initj;
 do {
 if ((curch == '6'))    {
     gtracetask(rulefile_p0ldp,80,1);
gcorlengt(2);
     curch = 'z';
     nexch = 'e';
     offch(2) = 's';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel81(void)
{
 
 initj;
 do {
 if ((curch == '7'))    {
     gtracetask(rulefile_p0ldp,81,1);
gcorlengt(4);
     curch = 'z';
     nexch = 'e';
     offch(2) = 'v';
     offch(3) = 'e';
     offch(4) = 'n';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

static void regel82(void)
{
 
 initj;
 do {
 if ((curch == '8')) 
 if ((nexch == 't')) 
 if ((offch(2) == 'i')) 
 if ((offch(3) == 'g'))    {
     gtracetask(rulefile_p0ldp,82,1);
gcorlengt(3);
     curch = 't';
     nexch = 'a';
     offch(2) = 'c';
     offch(3) = 'h';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel83(void)
{
 
 initj;
 do {
 if ((curch == '8'))    {
     gtracetask(rulefile_p0ldp,83,1);
gcorlengt(3);
     curch = 'a';
     nexch = 'c';
     offch(2) = 'h';
     offch(3) = 't';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel84(void)
{
 
 initj;
 do {
 if ((curch == '9'))    {
     gtracetask(rulefile_p0ldp,84,1);
gcorlengt(4);
     curch = 'n';
     nexch = 'e';
     offch(2) = 'g';
     offch(3) = 'e';
     offch(4) = 'n';

     gotoj(4);
   }

  forwj;
 } while nextj;
}

/*--- STRING RULES ---------------------------------------------*/

void fono_ppldp_0_1()
{
 regel1(); regel2(); regel3(); regel4(); regel5(); regel6(); regel7(); regel8();
 regel9(); regel10(); regel11(); regel12(); regel13(); regel14(); regel15(); regel16();
 regel17(); regel18(); regel19(); regel20(); regel21(); regel22(); regel23(); regel24();
 regel25(); regel26(); regel27(); regel28(); regel29(); regel30(); regel31(); regel32();
 regel33(); regel34(); regel35(); regel36(); regel37(); regel38(); regel39(); regel40();
 regel41(); regel42(); regel43();

 initj; 
 do {
 if ((curch == '@')) 
 if ((
inset(nexch,gprom))) 
 if (strnj) {
    gplace_venster(); regel44(); regel45(); regel46(); regel47(); regel48();
 regel49(); regel50(); regel51(); regel52(); regel53(); regel54(); regel55(); regel56();

 backj; 
 }
 forwj; 
 } while strnj;ginit_venster();


 initj; 
 do {
 if ((
inset(curch,gpcijf))) 
 if (strnj) {
    gplace_venster(); regel57(); regel58(); regel59(); regel60(); regel61(); regel62(); regel63(); regel64();
 regel65(); regel66(); regel67(); regel68(); regel69(); regel70(); regel71(); regel72();
 regel73(); regel74(); regel75(); regel76(); regel77(); regel78(); regel79(); regel80();
 regel81(); regel82(); regel83(); regel84();
 backj; 
 }
 forwj; 
 } while strnj;ginit_venster();
}

/*--- PARAMETRISATION RULES ------------------------------------*/
void fone_ppldp_0_1()
{
}

