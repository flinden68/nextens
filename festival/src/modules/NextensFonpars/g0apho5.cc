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

void ovl_grapho0_5()
{
}


static void regel241(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'A')) 
 if ((
(inset(nexch, gpson) && 
inset(nexch,gpnas)))) 
 if ((
inset(offch(2),gmseg))) 
 if ((gvgl( -1,true," 4t&rd5t&+rd4 sed4 mad3 el4 rom4 jap4 tir2gr000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,241,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel242(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'O')) 
 if ((
(inset(nexch, gmcont) && 
inset(nexch,gmstem)))) 
 if ((
inset(offch(2),gmseg))) 
 if (!(((prech == 'c')) || ((prech == '\'')) || ((prech == 'b')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-2) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gklank))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,242,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel243(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'O')) {

 if ((offch(gx+1) == 'n'))  gx = gx + 1;
 if ((
inset(offch(gx+1),gmseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gklank))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,243,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel244(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'y')) 
 if ((
inset(nexch,gpcons))) 
 if ((
inset(offch(2),gmseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gklank))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,244,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel245(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'i')) 
 if ((
inset(nexch,gmseg))) 
 if ( (((offch(-2) == 's') && (prech == 't')) || ((offch(-4) == 's') && 
(offch(-3) == '+') && (offch(-2) == 't') && (prech == 'r')) || (
(offch(-2) == 'r') && (prech == 'x')) || ((offch(-3) == 'g') && 
(offch(-2) == 'o') && (prech == 'r')) || ((offch(-3) == 'O') && 
(offch(-2) == 'k') && (prech == 's')) || ((offch(-2) == 'o') && 
(prech == 'm')) || ((offch(-4) == 'e') && (offch(-3) == 'j') && 
(offch(-2) == 'o') && (prech == 'r')) || ((offch(-2) == 'o') && 
(prech == 'd')) || ((offch(-5) == 'c') && (offch(-4) == 'r') && 
(offch(-3) == 'a') && (offch(-2) == 't') && (prech == 's')) || (
(offch(-2) == 'i') && (prech == 'b')) || ((offch(-2) == 'o') && 
(prech == 'b')) || ((prech == 'w')) || ((prech == 'p')) || (
(
((inset(offch(-2),gklank) && (offch(-2)!='f')))) && 
(prech == 'f')) || ((prech == 'g')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-2) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gklank))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,245,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel246(void)
{
 
 initj;
 do {
 if ((curch == '\'')) 
 if ((nexch == 'O')) 
 if ((offch(2) == 'n')) 
 if ((
inset(offch(3),gmseg))) 
 if ( (((offch(-2) == 'i') && (prech == 'k')) || ((offch(-2) == 'i') && 
(prech == 'j')) || ((offch(-3) == 'A') && (offch(-2) == 't') && 
(prech == '+')) || ((prech == 'c')) || ((offch(-2) == 'u') && 
(prech == 't')) || ((offch(-2) == 'k') && (prech == 't')) || (
(offch(-2) == 'l') && (prech == 't')) || ((offch(-2) == 'i') && 
(prech == 't')) || ((offch(-2) == 'I') && (prech == 't')) || (
(offch(-2) == 'm') && (prech == 'b')) || ((offch(-2) == 'k') && 
(prech == 's')) || ((offch(-2) == 'd') && (prech == 'r')) || (
(offch(-2) == 't') && (prech == 'r')) || ((prech == 'z')) || (
(prech == 'm'))))
 if (inset(offch(-2),gklank))
 if (   (
((inset(offch(-2),gklank) && (offch(-2)!=' ')))) )    {
     gtracetask(rulefile_g0apho,246,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel247(void)
{
 
 initj;
 do {
 if ((curch == '\'')) 
 if ((
(inset(nexch, gpklem) && 
inset(nexch,gmlang) && 
inset(nexch,gmrond)))) 
 if ((offch(2) == 'k')) 
 if ((offch(3) == 's')) 
 if ((
inset(offch(4),gmseg))) 
 if (!(((prech == '-')) || ((offch(-2) == 'p') && (prech == 'l')) || (
(offch(-2) == 'f') && (prech == 'l'))))   {
     gtracetask(rulefile_g0apho,247,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel248(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == '\'')) 
 if ((
(inset(nexch, gpklem) && 
inset(nexch,gmlang) && 
inset(nexch,gmhoog)))) 
 if ( (((
(inset(offch(2), gpcons) && 
inset(offch(2),gpson))) && 
(
(inset(offch(3), gmson) && 
inset(offch(3),gpstem)))) || (
(offch(2) == 'l') && (offch(3) == 'm')) || ((offch(2) == 'x') && 
(offch(3) == 't'))))
 if ((
inset(offch(4),gmseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-1),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,248,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel249(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(curch,gpklem)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+1) == 'i'))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((
inset(offch(gx+2),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if (inset(offch(gx+3),gklank))
 if (   (
(!inset(offch(gx+3),gmklem    )) && 
((inset(offch(gx+3),gklank) && (offch(gx+3)!='i')))) )    gstop = true;
      else {

 if ((
inset(offch(gx+3),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+4),gmseg)))    gstop = true;
      else {

 if ((
inset(offch(gx+4),gpcons))) {

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
inset(offch(gy-1),gklank))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,249,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}}}
  forwj;
 } while nextj;
}

static void regel250(void)
{
 
 initj;
 do { gy = 0;

 if ((
inset(curch,gpklem))) 
 if ((
inset(nexch,gpcons))) 
 if ((offch(2) == 'e')) 
 if ((offch(3) == 'j')) 
 if ( (((offch(4) == 'o')) || ((offch(4) == 'a'))))
 if ((
inset(offch(5),gmseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gklank))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,250,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel251(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(offch(gx),gpklem)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+1),gpcons)))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpklem))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+2),gpvoc)))    gstop = true;
      else {

 if ((
inset(offch(gx+2),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((
inset(offch(gx+3),gmseg))) || (
(offch(gx+3) == 'l') && (offch(gx+4) == '&') && (
inset(offch(gx+5),gmseg))) || (
(offch(gx+3) == '&') && (offch(gx+4) == 'n') && (
inset(offch(gx+5),gmseg))) || (
(offch(gx+3) == '&') && (offch(gx+4) == 'N')) || ((offch(gx+3) == 'r') && 
(offch(gx+4) == '&'))))   gstop = true;
      else {

 if ((
inset(offch(gx+3),gpcons))) {

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
inset(offch(gy-1),gklank))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,251,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}}}
  forwj;
 } while nextj;
}

static void regel252(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == '\''))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((!
inset(offch(gx+1),gklank)))    gstop = true;
      else {

 if ((!
(inset(offch(gx+1), gmseg) && 
inset(offch(gx+1),gpstrid)))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,252,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel253(void)
{
 
 initj;
 do {
 if ((curch == '&')) 
 if ((nexch == 'N'))    {
     gtracetask(rulefile_g0apho,253,1);
     curch = 'I';

   }

  forwj;
 } while nextj;
}

static void regel254(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'n')) 
 if ((nexch == 'g')) 
 if ((
(inset(prech, gpklem) && 
inset(prech,gmlang)))) {

 if ((offch(gy-2) == '\''))  gy = gy - 1;
 if (inset(offch(gy-2),gklank))
 if (   (
(!inset(offch(gy-2),gpvoc     )) && 
((inset(offch(gy-2),gklank) && (offch(gy-2)!='c')))) ) {

 if ((offch(gx+2) == '\''))  gx = gx + 1;
 if ( (((
(inset(offch(gx+2), gmlang) && 
inset(offch(gx+2),gpvoc)))) || (
(
inset(offch(gx+2),gmvoc)))))   {
     gtracetask(rulefile_g0apho,254,1);
gcorlengt(-1);
     curch = 'N';

   }
}}
  forwj;
 } while nextj;
}

static void regel255(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'c')) {

 if ((offch(gx+1) == '\''))  gx = gx + 1;
 if ( (((
(inset(offch(gx+1), gpvoc) && 
inset(offch(gx+1),gmlaag) && 
inset(offch(gx+1),gmrond)))) || (
(offch(gx+1) == 'j')) || ((offch(gx+1) == '@'))))   {
     gtracetask(rulefile_g0apho,255,1);
     curch = 's';

   }
}
  forwj;
 } while nextj;
}

static void regel256(void)
{
 
 initj;
 do {
 if ((curch == 'c'))    {
     gtracetask(rulefile_g0apho,256,1);
     curch = 'k';

   }

  forwj;
 } while nextj;
}

static void regel257(void)
{
 
 initj;
 do { gx = 0;

 if ((
(inset(curch, gmson) && 
inset(curch,gpcons) && 
inset(curch,gpstem)))) 
 if ((gvgl( 1,false," 1 1+1h3wAt4wArm3wEt3wAs00000000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,257,1);
     gverander(0,1,(&gmstem    ));
   }

  forwj;
 } while nextj;
}

static void regel258(void)
{
 
 initj;
 do {
 if ((curch == '+'))    {
     gtracetask(rulefile_g0apho,258,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel259(void)
{
 
 initj;
 do {
 if ((
(inset(curch, gmson) && 
inset(curch,gpcons)))) 
 if ( (((
(inset(nexch, gmson) && 
inset(nexch,gpcons) && 
inset(nexch,gmstem)))) || (
(nexch == 'g'))))   {
     gtracetask(rulefile_g0apho,259,1);
     gverander(0,1,(&gmstem    ));
   }

  forwj;
 } while nextj;
}

static void regel260(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'n'))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+1) == 'r')) || ((offch(gx+1) == 'g')) || ((offch(gx+1) == 'x'))))   gstop = true;
      else {

 if ((
inset(offch(gx+1),gmseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,260,1);
     curch = '~';

   }
}
  forwj;
 } while nextj;
}

static void regel261(void)
{
 
 initj;
 do {
 if ((
(inset(curch, gpnas) && 
inset(curch,gpcor)))) {

 if (inset(nexch,gpant      )) {
   gAverz = gpant     ;
gE = 'm';;
gA = 'p'; }
 if (inset(nexch,gmant      )) {
   gAverz = gmant     ;
gE = 'p';;
gA = 'm'; }{

 if (inset(nexch,gpcor      )) {
   gBverz = gpcor     ;
gF = 'm';;
gB = 'p'; }
 if (inset(nexch,gmcor      )) {
   gBverz = gmcor     ;
gF = 'p';;
gB = 'm'; }{

 if (inset(nexch,gpachter   )) {
   gCverz = gpachter  ;
gG = 'm';;
gC = 'p'; }
 if (inset(nexch,gmachter   )) {
   gCverz = gmachter  ;
gG = 'p';;
gC = 'm'; }
 if ((
(inset(nexch, gpcons) && 
inset(nexch,gAverz) && 
inset(nexch,gBverz) && 
inset(nexch,gCverz))))    {
     gtracetask(rulefile_g0apho,261,1);
     gverander(0,2,(gA=='p')?(&gpant     ):(&gmant     ));     gverander(0,4,(gB=='p')?(&gpcor     ):(&gmcor     ));     gverander(0,3,(gC=='p')?(&gpachter  ):(&gmachter  ));
   }
}}}
  forwj;
 } while nextj;
}

static void regel262(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(curch,gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+1),gpcons)))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gmseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if (curch == offch(gx+1))    {
     gtracetask(rulefile_g0apho,262,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel263(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(curch,gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+1) == '`'))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gmseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((offch(gx+2) == ' ')) {

 if ((offch(gx+3) == '\''))  gx = gx + 1;
 if ((
inset(offch(gx+3),gpcons))) 
 if (curch == offch(gx+3))    {
     gtracetask(rulefile_g0apho,263,1);
gcorlengt(-1);

     backj;
   }
}}
  forwj;
 } while nextj;
}

static void regel264(void)
{
 
 initj;
 do {
 if ( (((curch == 't')) || ((curch == 'd'))))
 if ((nexch == 'j'))    {
     gtracetask(rulefile_g0apho,264,1);
     curch = 'C';

   }

  forwj;
 } while nextj;
}

static void regel265(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 's')) 
 if ((nexch == 'j')) 
 if ((offch(2) == '&')) {

 if ((offch(gy-1) == ' '))  gy = gy - 1;
 if ((
inset(offch(gy-1),gpson)))    {
     gtracetask(rulefile_g0apho,265,1);
     curch = 'S';

   }
}
  forwj;
 } while nextj;
}

static void regel266(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'j')) {

 if ((offch(gy-1) == ' '))  gy = gy - 1;
 if ( (((offch(gy-1) == 'C')) || ((offch(gy-1) == 'S'))))   {
     gtracetask(rulefile_g0apho,266,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel267(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'g')) 
 if ((nexch == 'n')) 
 if ((prech == 'I')) {

 if ((offch(gx+2) == '\''))  gx = gx + 1;
 if ( (((offch(gx+2) == 'a')) || ((offch(gx+2) == 'o'))))   {
     gtracetask(rulefile_g0apho,267,1);
     curch = '!';
     nexch = 'j';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel268(void)
{
 
 initj;
 do { gx = 0;

 if ( (((offch(-3) == 'v') && (offch(-2) == '&') && (prech == 'r')) || (
(offch(-2) == 'b') && (prech == '&')) || ((offch(-2) == 'x') && 
(prech == '&')) || ((offch(-2) == 'g') && (prech == '&'))))
 if ((
inset(offch(gx),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+1) == '\'') && (
inset(offch(gx+2),gpklem))) || (
(
inset(offch(gx+1),gpklem)))))   gstop = true;
      else {

 if ((
inset(offch(gx+1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,268,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel269(void)
{
 
 initj;
 do {
 if ( (((curch == 's') && (nexch == 'x')) || ((curch == 'h')) || (
(curch == 'g') && (nexch == 'r')) || ((curch == 'g') && 
(nexch == 'l')) || ((curch == 'v') && (nexch == 'l')) || (
(curch == 'v') && (nexch == 'r')) || ((curch == 'b') && 
(nexch == 'l')) || ((curch == 'b') && (nexch == 'r'))))
 if (inset(prech,gklank))
 if (   (
((inset(prech,gklank) && (prech!=' ')))) )    {
     gtracetask(rulefile_g0apho,269,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel270(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(curch,gpcons))) {

 if ((offch(gx+1) == '\''))  gx = gx + 1;
 if ((
inset(offch(gx+1),gpvoc))) 
 if ( (((
inset(offch(-2),gpvoc)) && 
(prech == '~')) || ((
inset(offch(-2),gpvoc)) && 
(
inset(prech,gpcons))) || (
(
inset(prech,gpvoc)))))   {
     gtracetask(rulefile_g0apho,270,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel271(void)
{
 
 initj;
 do { gx = 0;

 if ( (((
(inset(curch, gmson) && 
inset(curch,gpcons))) && 
(nexch == 'r')) || ((
(inset(curch, gmson) && 
inset(curch,gpcons))) && 
(nexch == 'l')) || ((curch == 's') && (nexch == 't')))){

 if ((offch(gx+2) == '\''))  gx = gx + 1;
 if ((
inset(offch(gx+2),gpvoc))) 
 if ( (((
inset(offch(-2),gpvoc)) && 
(prech == '~')) || ((
inset(offch(-2),gpvoc)) && 
(
inset(prech,gpcons))) || (
(
inset(prech,gpvoc)))))   {
     gtracetask(rulefile_g0apho,271,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel272(void)
{
 
 initj;
 do { gx = 0;

 if ((
(inset(prech, gmson) && 
inset(prech,gpcons)))) 
 if ((
(inset(offch(-2), gpson) && 
inset(offch(-2),gpcons)))) 
 if ((
inset(offch(-3),gpvoc))) 
 if ((
(inset(curch, gmson) && 
inset(curch,gpcons)))) {

 if ((offch(gx+1) == '\''))  gx = gx + 1;
 if ((
inset(offch(gx+1),gpvoc)))    {
     gtracetask(rulefile_g0apho,272,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel273(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(prech,gpcons))) 
 if ((
inset(offch(-2),gpcons))) 
 if ((
inset(curch,gpcons))) 
 if ((
inset(nexch,gpcons))) {

 if ((offch(gx+2) == '\''))  gx = gx + 1;
 if ((
inset(offch(gx+2),gpvoc)))    {
     gtracetask(rulefile_g0apho,273,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel274(void)
{
 
 initj;
 do { gy = 0;

 if ((
inset(curch,gpcons))) 
 if ( (((nexch == '"')) || ((
inset(nexch,gpvoc))) || (
(nexch == '\'') && (
inset(offch(2),gpvoc)))
))
 if (inset(prech,gklank))
 if (   (
(!inset(prech,gmcons    )) && 
((inset(prech,gklank) && (prech!='N')))) ) {

 if ((
inset(offch(gy-2),gpcons)))  gy = gy - 1;
 if ((
inset(offch(gy-2),gpvoc)))    {
     gtracetask(rulefile_g0apho,274,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel275(void)
{
 
 initj;
 do { gx = 0;

 if ((prech == 'N')) 
 if ((
inset(offch(-2),gklank)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((
inset(offch(gx),gpvoc))) || (
(offch(gx) == '\'') && (
inset(offch(gx+1),gpklem)))
))   gstop = true;
      else {

 if ((
inset(offch(gx),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,275,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel276(void)
{
 
 initj;
 do { gx = 0;

 if (inset(prech,gklank))
 if (   (
(!inset(prech,gmvoc     )) && 
((inset(prech,gklank) && (prech!='U'))) && 
((inset(prech,gklank) && (prech!='A'))) && 
((inset(prech,gklank) && (prech!='E')))) ) {

 if ((offch(gx) == '\''))  gx = gx + 1;
 if (inset(offch(gx),gklank))
 if (   (
(!inset(offch(gx),gmvoc     )) && 
((inset(offch(gx),gklank) && (offch(gx)!=':')))) )    {
     gtracetask(rulefile_g0apho,276,1);
gcorlengt(1);
     curch = '-';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel277(void)
{
 
 initj;
 do { gx = 0;

 if ( (((prech == ' ')) || ((prech == '-'))))
 if ((
inset(offch(gx),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+1) == '\''))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,277,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel278(void)
{
 
 initj;
 do {
 if ((curch == '\'')) 
 if (!(((prech == ' ')) || ((prech == '-'))))   {
     gtracetask(rulefile_g0apho,278,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel279(void)
{
 
 initj;
 do {
 if ((curch == '\'')) 
 if ((prech == ' ')) 
 if ((offch(-2) == 'n')) 
 if ((offch(-3) == '&')) 
 if ((offch(-4) == ' ')) 
 if ((
inset(nexch,gpseg)))    {
     gtracetask(rulefile_g0apho,279,1);
     curch = '>';

   }

  forwj;
 } while nextj;
}

static void regel280(void)
{
 
 initj;
 do {
 if ((curch == 'y'))    {
     gtracetask(rulefile_g0apho,280,1);
     curch = '$';

   }

  forwj;
 } while nextj;
}

static void regel281(void)
{
 
 initj;
 do {
 if ((curch == 'u'))    {
     gtracetask(rulefile_g0apho,281,1);
     curch = 'y';

   }

  forwj;
 } while nextj;
}

static void regel282(void)
{
 
 initj;
 do {
 if ((curch == '$'))    {
     gtracetask(rulefile_g0apho,282,1);
     curch = 'u';

   }

  forwj;
 } while nextj;
}

static void regel283(void)
{
 
 initj;
 do {
 if ((curch == ' ')) 
 if ((prech == '`'))    {
     gtracetask(rulefile_g0apho,283,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

/*--- STRING RULES ---------------------------------------------*/

void fono_grapho_0_5()
{
 regel241(); regel242(); regel243(); regel244(); regel245(); regel246(); regel247(); regel248();
 regel249(); regel250(); regel251(); regel252(); regel253(); regel254(); regel255(); regel256();
 regel257(); regel258(); regel259(); regel260(); regel261(); regel262(); regel263(); regel264();
 regel265(); regel266(); regel267(); regel268(); regel269(); regel270(); regel271(); regel272();
 regel273(); regel274(); regel275(); regel276(); regel277(); regel278(); regel279(); regel280();
 regel281(); regel282(); regel283();}

/*--- PARAMETRISATION RULES ------------------------------------*/
void fone_grapho_0_5()
{
}



