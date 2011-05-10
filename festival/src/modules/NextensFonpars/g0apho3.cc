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

void ovl_grapho0_3()
{
}


static void regel121(void)
{
 
 initj;
 do {
 if ((curch == 'a')) 
 if ((nexch == 'i')) 
 if ( (((offch(2) == 'r')) || ((offch(2) == 's') && (offch(3) == 'e'))))   {
     gtracetask(rulefile_g0apho,121,1);
gcorlengt(1);
     curch = '\'';
     nexch = 'E';
     offch(2) = ':';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel122(void)
{
 
 initj;
 do {
 if ((curch == 'a')) 
 if ((nexch == 'i'))    {
     gtracetask(rulefile_g0apho,122,1);
     curch = 'e';
     nexch = 'e';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel123(void)
{
 
 initj;
 do {
 if ((prech == 'i')) 
 if ((curch == 'e')) 
 if ((nexch == 'e')) 
 if (!(((offch(2) == 'n') && (offch(3) == ' '))))   {
     gtracetask(rulefile_g0apho,123,1);
gcorlengt(1);
     curch = 'j';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel124(void)
{
 
 initj;
 do {
 if ((curch == 'i')) 
 if ((nexch == 'k')) 
 if ((offch(2) == 'e')) 
 if ((offch(3) == 'n'))    {
     gtracetask(rulefile_g0apho,124,1);
     curch = 'I';

   }

  forwj;
 } while nextj;
}

static void regel125(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((nexch == 'u')) 
 if ((offch(2) == 'm')) 
 if ( (((offch(3) == ' ')) || ((offch(3) == 's') && (offch(4) == ' ')) || (
(offch(3) == 'p') && (offch(4) == 'j') && (offch(5) == 'e'))))   {
     gtracetask(rulefile_g0apho,125,1);
gcorlengt(2);
     curch = '\'';
     nexch = 'e';
     offch(2) = 'j';
     offch(3) = 'U';

     gotoj(3);
   }

  forwj;
 } while nextj;
}

static void regel126(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((nexch == 'u'))    {
     gtracetask(rulefile_g0apho,126,1);
gcorlengt(-1);
     curch = '@';

   }

  forwj;
 } while nextj;
}

static void regel127(void)
{
 
 initj;
 do {
 if ((curch == 'i')) 
 if ( (((nexch == '@')) || ((nexch == 'y') && (offch(2) == 'n'))))
 if ( (((prech == 's')) || ((prech == 'l'))))   {
     gtracetask(rulefile_g0apho,127,1);
     curch = 'j';

   }

  forwj;
 } while nextj;
}

static void regel128(void)
{
 
 initj;
 do {
 if ((curch == 'i')) 
 if ((nexch == 'e'))    {
     gtracetask(rulefile_g0apho,128,1);
     curch = 'i';
     nexch = 'i';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel129(void)
{
 
 initj;
 do {
 if ((prech == 'i')) 
 if ((offch(-2) == 'i')) 
 if ((curch == 'e'))    {
     gtracetask(rulefile_g0apho,129,1);
gcorlengt(1);
     curch = 'j';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel130(void)
{
 
 initj;
 do {
 if ((curch == 'x'))    {
     gtracetask(rulefile_g0apho,130,1);
gcorlengt(1);
     curch = 'k';
     nexch = 's';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel131(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ((nexch == 'c')) 
 if ((offch(2) == 'h')) 
 if ((
inset(prech,gpcons))) 
 if ((offch(3) == ' '))    {
     gtracetask(rulefile_g0apho,131,1);
gcorlengt(-2);
     curch = 'S';

   }

  forwj;
 } while nextj;
}

static void regel132(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'h')) 
 if ((prech == 's')) 
 if (!(((offch(2) == 'e') && (offch(3) == 'f'))))   {
     gtracetask(rulefile_g0apho,132,1);
gcorlengt(-1);
     curch = 'x';

   }

  forwj;
 } while nextj;
}

static void regel133(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 't')) 
 if ((nexch == 'h')) 
 if ((gvgl( 2,false," 3eek3eca3eat3eke4erap3eor3ode3odi0000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,133,1);
     curch = '+';
     nexch = 't';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel134(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'h')) 
 if ((prech == 't')) 
 if (!(gvgl( -2,true," 2UI2aa2ee2ii2ch2on2oo3ors2ys000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,134,1);
     curch = '+';

   }

  forwj;
 } while nextj;
}

static void regel135(void)
{
 
 initj;
 do {
 if ((curch == 'u')) 
 if ((nexch == 'w'))    {
     gtracetask(rulefile_g0apho,135,1);
gcorlengt(1);
     curch = 'u';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel136(void)
{
 
 initj;
 do {
 if ((curch == 'h')) 
 if ((nexch == 'e')) 
 if ((offch(2) == 't')) 
 if ((offch(3) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,136,1);
gcorlengt(-1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel137(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((nexch == 'e')) 
 if ((offch(2) == 'n')) 
 if ((offch(3) == ' ')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,137,1);
gcorlengt(-1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel138(void)
{
 
 initj;
 do {
 if ((curch == 'E')) 
 if ((nexch == 'I')) 
 if ((offch(2) == 'z')) 
 if ((offch(3) == 'o')) 
 if ((prech == 'b')) 
 if ((offch(-2) == ' '))    {
     gtracetask(rulefile_g0apho,138,1);
gcorlengt(-1);
     curch = 'i';

   }

  forwj;
 } while nextj;
}

static void regel139(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if (!(((nexch == 'e') && (offch(2) == 'n')) || ((nexch == 'e') && 
(offch(2) == 'l') && (offch(3) == 'E') && (offch(4) == 'I') && 
(offch(5) == 'k'))))
 if ((prech == 'm'))    {
     gtracetask(rulefile_g0apho,139,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel140(void)
{
 
 initj;
 do {
 if ( (((curch == 'w')) || ((curch == 'b'))))
 if ((nexch == 't')) 
 if ( (((prech == 'r')) || ((prech == 'm'))))   {
     gtracetask(rulefile_g0apho,140,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel141(void)
{
 
 initj;
 do {
 if ((curch == 'w')) 
 if ((nexch == 'r')) 
 if ((prech == ' '))    {
     gtracetask(rulefile_g0apho,141,1);
     curch = 'v';

   }

  forwj;
 } while nextj;
}

static void regel142(void)
{
 
 initj;
 do {
 if ((curch == 'j')) 
 if ((nexch == 'e')) 
 if ((offch(2) == 's')) 
 if (!((prech == ' ')))    {
     gtracetask(rulefile_g0apho,142,1);
     curch = 'j';
     nexch = '&';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel143(void)
{
 
 initj;
 do {
 if ((curch == 'i')) 
 if ((nexch == 'g')) 
 if ( (((prech == 'n')) || ((prech == 'z')) || ((prech == 'd')) || (
(prech == 't'))))
 if (!(((offch(-4) == 'r') && (offch(-3) == 'e') && (offch(-2) == 's')) || (
(offch(-2) == ' '))))   {
     gtracetask(rulefile_g0apho,143,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel144(void)
{
 
 initj;
 do {
 if ((curch == 'U')) 
 if ((prech == 'g')) 
 if ((offch(-2) == 'n')) 
 if ((offch(-3) == 'i')) 
 if ((nexch == 'I')) 
 if ( (((offch(2) == 's') && (offch(3) == 't')) || ((offch(2) == 'n') && 
(offch(3) == ' ')) || ((offch(2) == 'n') && (offch(3) == 's'))))   {
     gtracetask(rulefile_g0apho,144,1);
     curch = 'w';

   }

  forwj;
 } while nextj;
}

static void regel145(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 's')) 
 if ((nexch == 'x')) 
 if ((prech == 'i')) {

 if ((offch(gx+2) == 'e'))  gx = gx + 1;{

 if ((offch(gx+2) == 'r'))  gx = gx + 1;
 if ((offch(gx+2) == ' '))    {
     gtracetask(rulefile_g0apho,145,1);
gcorlengt(1);
     curch = 'i';
     nexch = 'i';
     offch(2) = 's';

     gotoj(2);
   }
}}
  forwj;
 } while nextj;
}

static void regel146(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'i')) 
 if ((nexch == 's')) 
 if (inset(prech,gklank))
 if (   (
(!inset(prech,gpvoc     )) && 
((inset(prech,gklank) && (prech!='v'))) && 
((inset(prech,gklank) && (prech!='w'))) && 
((inset(prech,gklank) && (prech!=' '))) && 
((inset(prech,gklank) && (prech!='c'))) && 
((inset(prech,gklank) && (prech!='+'))) && 
((inset(prech,gklank) && (prech!='m'))) && 
((inset(prech,gklank) && (prech!='n'))) && 
((inset(prech,gklank) && (prech!='d')))) ) {

 if ((
inset(offch(gy-2),gpcons)))  gy = gy - 1;
 if ((
inset(offch(gy-2),gpklem))) 
 if (inset(offch(2),gklank))
 if (   (
((inset(offch(2),gklank) && (offch(2)!='m'))) && 
((inset(offch(2),gklank) && (offch(2)!='s'))) && 
((inset(offch(2),gklank) && (offch(2)!='e'))) && 
((inset(offch(2),gklank) && (offch(2)!='a'))) && 
((inset(offch(2),gklank) && (offch(2)!='t'))) && 
((inset(offch(2),gklank) && (offch(2)!='c'))) && 
((inset(offch(2),gklank) && (offch(2)!='p')))) )    {
     gtracetask(rulefile_g0apho,146,1);
gcorlengt(1);
     curch = '&';
     nexch = 's';
     offch(2) = '+';

     gotoj(2);
   }
}
  forwj;
 } while nextj;
}

static void regel147(void)
{
 
 initj;
 do {
 if ((prech == 's')) 
 if ((offch(-2) == 'g')) 
 if ((offch(-3) == 'n')) 
 if ((offch(-4) == 'i')) 
 if ((
inset(offch(-5),gpcons))) 
 if ((
inset(curch,gpseg))) 
 if ((
inset(nexch,gpseg))) 
 if ((
inset(offch(2),gpseg)))    {
     gtracetask(rulefile_g0apho,147,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel148(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ( (((curch == 'h') && (
((inset(nexch,gklank) && (nexch!=' '))))) || (
(gvgl( 0,false," 2bl5dert&6engel 8engelen 6engels4frik5gemen4plo 5press2vl2vr2wr4zelf4zelv5zicht000000000000000000000")))
))
 if (!(gvgl( -1,true," 1 1c3ver00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,148,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel149(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ( (((prech == 'd')) || ((prech == 't')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-2),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop) 
 if ( (((curch == 'p')) || ((curch == 'k')) || ((curch == 'b')) || (
(curch == 'f')) || ((curch == 'v')) || ((curch == 'z')) || (
(curch == 'g')) || ((curch == 'c')) || ((curch == 'l')) || (
(curch == 'n')) || ((curch == 'm')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+1),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,149,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel150(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ( (((curch == 'f')) || ((curch == 'v')) || ((curch == 'g')) || (
(curch == 'z')) || ((curch == 'c')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+1),gpklem)))    gstop = true;
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
(inset(prech, gmson) && 
inset(prech,gmcont) && 
inset(prech,gpseg))))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-2),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,150,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel151(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ( (((curch == 'p')) || ((curch == 'b')) || ((curch == 'k')) || (
(curch == 'G')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+1),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ( (((prech == 'c')) || ((prech == 'g')) || ((prech == 'z')) || (
(prech == 'v')) || ((prech == 'f')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-2),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,151,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel152(void)
{
 
 initj;
 do { gy = 0;

 if ( (((curch == 'v') && (nexch == 'e') && (offch(2) == 'r')) || (
(curch == 'z') && (nexch == 'o')) || ((curch == 'd') && 
(nexch == 'a') && (offch(2) == 'n')) || ((curch == 'v') && 
(nexch == 'e') && (offch(2) == 'e') && (offch(3) == 'l'))))
 if ((gvgl( -1,true," 3 di2zo2hy000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,152,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel153(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((gvgl( -1,true," 4 bEI3 na4 era5 mede4 mee4 ere3 af3 om3 on5 even4 aan3 in5 voor3 ty000000000000000000000000000000000"))) 

 if (!(((curch == 'a') && (
inset(nexch,gmvoc))) || (
(
inset(curch,gpcons)) && 
(
inset(nexch,gmseg))) || (
(
inset(curch,gpcons)) && 
(nexch == 'e') && (
inset(offch(2),gmseg))) || (
(
inset(curch,gpcons)) && 
(nexch == 'e') && (offch(2) == 'n') && (
inset(offch(3),gmseg))) || (
(gvgl( 0,false," 1 3cht3der3den3eme3iti3lle3mel4metj2nd3ner2o 3rre4rder3ssi2ti3tte3ter2ts0000000000000000000000000000")))
))   {
     gtracetask(rulefile_g0apho,153,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel154(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((gvgl( -1,true," 4 nog8 beneden6 boven7 binnen7 bUIten6 ps@do5 over6 onder7 achter7 verder7 eerder0000000000000000000"))) 

 if (!(gvgl( 0,false," 1 3der2e 3en 2ig4lEIk0000000000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,154,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel155(void)
{
 
 initj;
 do { gy = 0;

 if ((gvgl( -1,true," 3 in3 an3 op3 er4 her0000000000000000000000000000000000000000000000000000000000000000000000000000000"))) 

 if ( (((
(!inset(curch,gmvoc     )) && 
((inset(curch,gklank) && (curch!='i')))) && 
(
(!inset(nexch,gmseg     )) && 
((inset(nexch,gklank) && (nexch!='n'))) && 
((inset(nexch,gklank) && (nexch!='r'))) && 
((inset(nexch,gklank) && (nexch!='b'))))) || (
(curch == 'd') && (nexch == 'e') && (
(inset(offch(2), gmcont) && 
inset(offch(2),gpson))) && 
(
inset(offch(3),gpklem))) || (
(curch == 't') && (nexch == 'e') && (
(inset(offch(2), gpcons) && 
inset(offch(2),gpson))) && 
(
inset(offch(3),gpklem))) || (
(curch == 't') && (nexch == 'i')) || ((curch == 's') && 
(nexch == 't') && (offch(2) == 'e') && (offch(3) == 'l')) || (
(curch == 'e') && (nexch == 'n') && (offch(2) == '&')) || (
(curch == 'n') && (nexch == 'e') && (offch(2) == 'm')) || (
(curch == 'i') && (nexch == 'n'))))   {
     gtracetask(rulefile_g0apho,155,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel156(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 's')) 
 if ((
inset(prech,gpson))) 
 if ( (((gvgl( 1,false," 2&s2@r2@s4atii3eer5ering5eren 5erend3erv3eel4iive3ii 3itE4iiko3ico2io4iis 4iif 3ive2o 2ul2ue2us00000"))) || (
(nexch == 'i') && (offch(2) == 'i') && 
(
inset(offch(3),gpvoc))) || (
(nexch == 'e') && (offch(2) == 'l') && (offch(3) == 'e') && 
(
((inset(offch(4),gklank) && (offch(4)!='n')))))))   {
     gtracetask(rulefile_g0apho,156,1);
     curch = 'z';

   }

  forwj;
 } while nextj;
}

static void regel157(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'g')) 
 if ( (((nexch == 'i') && (
((inset(offch(2),gklank) && (offch(2)!='i')))) && 
(
((inset(offch(3),gklank) && (offch(3)!='i'))))) || (
(nexch == 'e') && (
inset(offch(2),gmseg))) || (
(gvgl( 1,false," 3es 3et 4ette4eren5eeren4enet00000000000000000000000000000000000000000000000000000000000000000000000")))
))
 if ((gvgl( -1,true," 4rot&3eta4orsa4anta5centa3ssa3rra3ara4baga3ala3 ra3lla4orta3bud3lle4 ori5resti3rri3rlo3 lo4char2fu00"))) 
   {
     gtracetask(rulefile_g0apho,157,1);
     curch = 'Z';

   }

  forwj;
 } while nextj;
}

static void regel158(void)
{
 
 initj;
 do { gy = 0;

 if ( (((curch == 'k')) || ((curch == 'n')) || ((curch == 'l'))))
 if ((nexch == 'l')) 
 if ((offch(2) == 'E')) 
 if ((gvgl( -1,true," 5midde3ade4zame4EIge4onin000000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,158,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel159(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'e')) 
 if ((gvgl( -1,true," 2 b2 d1g1j2 m000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"))) 

 if ((gvgl( 1,false," 4gena5gener5gene 5genen5gelEI3lyn4mene4neer5neren5never4teen0000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,159,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel160(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'e')) 
 if ( (((gvgl( 1,false," 4dere3gre4mii 3nd 2n 2ns3ne 4nen 1m3ter0000000000000000000000000000000000000000000000000000000000000"))) || (
(nexch == 'r') && (
((inset(offch(2),gklank) && (offch(2)!='g'))) && 
((inset(offch(2),gklank) && (offch(2)!='f')))))
))
 if ((gvgl( -1,true," 4prec4verd5gebed4epid5compl4na+m3gen4torp2cr3bet3int3bev3gev5gewez0000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,160,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel161(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((nexch == 'g')) 
 if ((offch(2) == 'e')) 
 if ( (((offch(3) == 'r') && (
inset(offch(4),gpvoc))) || (
(offch(3) == 'e') && (offch(4) == 'r'))))
 if ( (((offch(-4) == 'p') && (offch(-3) == 'r') && (offch(-2) == 'o') && 
(prech == 't')) || ((offch(-3) == 'd') && (offch(-2) == 'e') && 
(prech == 'l')) || ((prech == 'n')) || ((prech == 'r')) || (
(prech == 'b'))))   {
     gtracetask(rulefile_g0apho,161,1);
gcorlengt(1);
     curch = '&';
     nexch = 'g';
     offch(2) = 'e';

     gotoj(2);
   }

  forwj;
 } while nextj;
}

static void regel162(void)
{
 
 initj;
 do { gx = 0;

 if ( (((offch(-3) == 'v') && (offch(-2) == 'e') && (prech == 'r')) || (
(offch(-3) == 'b') && (offch(-2) == 'e') && (prech == 'g')) || (
(
inset(offch(-2),gmvoc)) && 
(prech == 'g')) || ((
inset(offch(-2),gmvoc)) && 
(prech == 'b')) || ((prech == 't'))))
 if ((curch == 'e')) 
 if ((gvgl( 1,false," 3nia3n&g3nea4rii 4riis3sel4ving4vend4ven 4vens3zem00000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,162,1);
gcorlengt(1);
     curch = 'e';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel163(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'e')) 
 if ( (((gvgl( 1,false," 2ga3len4ling3lig4ring3ren000000000000000000000000000000000000000000000000000000000000000000000000000"))) || (
(nexch == 'g') && (offch(2) == 'e') && 
(
((inset(offch(3),gklank) && (offch(3)!='l')))))))
 if ( (((gvgl( -1,true," 3oll2 r4ment2 v5fluuw0000000000000000000000000000000000000000000000000000000000000000000000000000000"))) || (
(
((inset(offch(-4),gklank) && (offch(-4)!='w')))) && 
(offch(-3) == 'e') && (offch(-2) == 'r') && (prech == 'v')) || (
(
inset(offch(-5),gmnas)) && 
(offch(-4) == 'o') && (offch(-3) == 'o') && (offch(-2) == 'r') && 
(prech == 'd')) || ((
inset(offch(-2),gmseg)) && 
(prech == 't')) || ((offch(-3) == ' ') && (
inset(offch(-2),gpvoc)) && 
(prech == 'm'))))   {
     gtracetask(rulefile_g0apho,163,1);
gcorlengt(1);
     curch = 'e';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel164(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if ((nexch == 'e')) 
 if ((
inset(prech,gmseg))) 
 if ( (((
inset(offch(2),gpcons)) && 
(
(!inset(offch(3),gmvoc     )) && 
((inset(offch(3),gklank) && (offch(3)!='e'))))) || (
(offch(2) == 'v') && (offch(3) == 'e') && (
inset(offch(4),gmnas))) || (
(
inset(offch(2),gpcons)) && 
(offch(3) == 'e') && (offch(4) == 'e')) || ((offch(2) == 'r') && 
(offch(3) == 'e') && (offch(4) == 'c') && (offch(5) == 'h') && 
(offch(6) == 't')) || ((offch(2) == 'g') && (offch(3) == 'e') && 
(offch(4) == 'l') && (offch(5) == 'E')) || ((offch(2) == 'b'))))   {
     gtracetask(rulefile_g0apho,164,1);
     curch = 't';
     nexch = '&';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel165(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'e')) 
 if ( (((offch(-3) == ' ') && (offch(-2) == 'p') && (prech == 'l')) || (
(offch(-2) == ' ') && (prech == 'r')) || ((offch(-3) == ' ') && 
(offch(-2) == 'p') && (prech == 'r'))))
 if ( (((nexch == 'l') && (
(!inset(offch(2),gmvoc     )) && 
((inset(offch(2),gklank) && (offch(2)!='i'))))) || (
(gvgl( 1,false," 3cem3cep3cen3cor4clam4ciis4ciiz4cizi3dak3dac2fr2fl2gr2kr4klam3lat3ljE3spe2ty4tent3tre4zent4zerv4ziir")))
))   {
     gtracetask(rulefile_g0apho,165,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel166(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ( (((prech == 'b')) || ((prech == 'v')) || ((prech == 'g'))))
 if ((
(inset(offch(-2), gpcons) && 
inset(offch(-2),gpson)))) 
 if ((
inset(offch(-3),gpklem))) 
 if ((
inset(offch(-4),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-5),gpklem)))    gstop = true;
      else {

 if ((
(inset(offch(gy-5), gpson) && 
inset(offch(gy-5),gpcons)))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop) 
 if ( (((nexch == 'l') && (offch(2) == ' ')) || ((nexch == 'l') && 
(offch(2) == 'd')) || ((nexch == 'r') && (offch(2) == 's')) || (
(nexch == 'n') && (offch(2) == 'd'))))   {
     gtracetask(rulefile_g0apho,166,1);
     curch = 'E';

   }
}
  forwj;
 } while nextj;
}

static void regel167(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'e')) 
 if ( (((nexch == 'n') && (offch(2) == 't') && (offch(3) == 'i') && 
(offch(4) == 'i') && (
((inset(offch(5),gklank) && (offch(5)!='n'))))) || (
(nexch == 'm') && (offch(2) == 'b') && (offch(3) == 'e') && 
(offch(4) == 'r') && (
inset(offch(5),gmvoc))) || (
(nexch == 'l') && (offch(2) == 'f') && (
((inset(offch(3),gklank) && (offch(3)!='l'))))) || (
(gvgl( 1,false," 5nzii 6nziis 4nzio5rtica4rzus5rzii 6rziis 0000000000000000000000000000000000000000000000000000000000")))
))
 if (!(((prech == 'b')) || ((prech == 'e'))))   {
     gtracetask(rulefile_g0apho,167,1);
     curch = 'E';

   }

  forwj;
 } while nextj;
}

static void regel168(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'e')) 
 if ((gvgl( 1,false," 2bb2ct3cht3chn2dd2ff2kk2kt3nt 3nkd3nte2pp2ss2tt00000000000000000000000000000000000000000000000000000"))) 

 if (!(gvgl( -1,true," 3ord1e3eff2yf2eg3EIg3rek3tek3bak3wap3tor3ket2tt3Ent2ev3oox000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,168,1);
     curch = 'E';

   }

  forwj;
 } while nextj;
}

static void regel169(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ( (((nexch == 'l')) || ((nexch == 'n'))))
 if ((offch(2) == 'd')) 
 if ( (((
inset(offch(3),gmvoc))) || (
(offch(3) == 'a')) || ((offch(3) == 'e')) || ((offch(3) == 'i') && 
(offch(4) == 'n') && (offch(5) == 'g')) || ((offch(3) == '&') && 
(offch(4) == 'g'))))
 if ((gvgl( -1,true," 3vid3+ag3 ag4 leg4 ell4+ell4bomm00000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,169,1);
     curch = 'E';

   }

  forwj;
 } while nextj;
}

static void regel170(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(prech,gpcons))) 
 if (!(((offch(-4) == ' ') && (offch(-3) == 'v') && (offch(-2) == 'e'))))
 if ( (((curch == 'j') && (
((inset(nexch,gklank) && (nexch!='y'))) && 
((inset(nexch,gklank) && (nexch!='a'))) && 
((inset(nexch,gklank) && (nexch!='i'))) && 
((inset(nexch,gklank) && (nexch!='e'))))) || (
(curch == 'g') && (nexch == 'r') && (
((inset(offch(2),gklank) && (offch(2)!='E'))))) || (
(gvgl( 0,false," 2ba2ch2gl3je 4rEIk4tUIg2tj00000000000000000000000000000000000000000000000000000000000000000000000000")))
))   {
     gtracetask(rulefile_g0apho,170,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel171(void)
{
 
 initj;
 do { gx = 0;

 if (inset(curch,gklank))
 if (   (
(!inset(curch,gmcons    )) && 
((inset(curch,gklank) && (curch!='h'))) && 
((inset(curch,gklank) && (curch!='g')))) ) 
 if ((
inset(nexch,gpcons))) 
 if (!(gvgl( 2,false," 1 2&g1+2e 3en 3el 4els 4iiis4ter 6teren 5ters 4ten 0000000000000000000000000000000000000000000000000"))) 

 if ((
inset(prech,gpcons))) 
 if (!(((
inset(offch(-2),gmvoc))) || (
(offch(-3) == 'p') && (offch(-2) == 'a')) || ((
inset(offch(-4),gmson)) && 
(offch(-3) == 'b') && (offch(-2) == 'e')) || ((
inset(offch(-4),gmson)) && 
(offch(-3) == 'g') && (offch(-2) == 'e')) || ((
inset(offch(-4),gmson)) && 
(offch(-3) == 'v') && (offch(-2) == 'e'))))   {
     gtracetask(rulefile_g0apho,171,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel172(void)
{
 
 initj;
 do {
 if ( (((
(inset(prech, gmcont) && 
inset(prech,gpseg) && 
inset(prech,gmnas)))) || (
(prech == 'f')) || ((prech == 'w')) || ((prech == 'j'))))
 if ((
inset(offch(-2),gpseg))) 
 if ( (((
(!inset(curch,gpcont    )) && 
(!inset(curch,gpnas     )) && 
((inset(curch,gklank) && (curch!='t'))) && 
((inset(curch,gklank) && (curch!='d'))) && 
((inset(curch,gklank) && (curch!='+'))) && 
((inset(curch,gklank) && (curch!=' '))))) || (
(curch == 'v')) || ((curch == 'z')) || ((curch == 'm')) || (
(curch == 'w') && (
((inset(nexch,gklank) && (nexch!='i'))) && 
((inset(nexch,gklank) && (nexch!='o'))) && 
((inset(nexch,gklank) && (nexch!='E'))) && 
((inset(nexch,gklank) && (nexch!='e'))) && 
((inset(nexch,gklank) && (nexch!='a'))))) || (
(curch == 't') && (
((inset(nexch,gklank) && (nexch!='i'))) && 
((inset(nexch,gklank) && (nexch!='e'))) && 
((inset(nexch,gklank) && (nexch!='+'))) && 
((inset(nexch,gklank) && (nexch!=' '))))) || (
(curch == 'd') && (
((inset(nexch,gklank) && (nexch!='e'))) && 
((inset(nexch,gklank) && (nexch!='+'))) && 
((inset(nexch,gklank) && (nexch!=' '))))) || (
(curch == 'd') && (nexch == 'e') && (offch(2) == 'l')) || (
(curch == 't') && (nexch == 'e') && (offch(2) == 'l') && 
(
((inset(offch(3),gklank) && (offch(3)!='E'))) && 
((inset(offch(3),gklank) && (offch(3)!='o'))))) || (
(curch == 'g')) || ((curch == 'c'))))
 if (prech != curch)    {
     gtracetask(rulefile_g0apho,172,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel173(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((prech == 'i')) {

 if ((offch(gy-2) == 'i'))  gy = gy - 1;
 if ((offch(gy-2) == 't')) 
 if ( (((offch(gy-6) == ' ') && (offch(gy-5) == 'o') && (offch(gy-4) == 'p') && 
(offch(gy-3) == '+')) || ((
((inset(offch(gy-4),gklank) && (offch(gy-4)!=' ')))) && 
(
(!inset(offch(gy-3),gmseg     )) && 
((inset(offch(gy-3),gklank) && (offch(gy-3)!='s')))))))
 if (!(((gvgl( 0,false," 1+2cu2ci2ca1d1i2kk2k 3k+t2ll5latii2ma2mm1n3st 3sti3ste3sme1t5zatii0000000000000000000000000000000000"))) || (
(
(inset(curch, gpcons) && 
inset(curch,gpson))) && 
(
inset(nexch,gmson))) || (
(curch == 'k') && (nexch == 'a') && (
inset(offch(2),gmseg)))
))   {
     gtracetask(rulefile_g0apho,173,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel174(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == '+')) 
 if ((prech == 'i')) {

 if ((offch(gy-2) == 'i'))  gy = gy - 1;
 if ((offch(gy-2) == 't')) 
 if ( (((nexch == 'k')) || ((nexch == 'v')) || ((nexch == 'f')) || (
(nexch == 's')) || ((nexch == 'z'))))
 if ( (((offch(2) == 'e')) || ((offch(2) == 'i'))))
 if ((gvgl( 3,false," 1 1c3erd3iis2l 4lEIk4ling4len 2n 3ne 2r 4ring4rend3re 4ren 2st3sme2tE0000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,174,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel175(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(prech,gpcons))) 
 if ((gvgl( -2,true," 1@2EI2UI2AU2aa2ee2ii2oo2uu1y000000000000000000000000000000000000000000000000000000000000000000000000"))) 

 if (!(((curch == 'e') && (
((inset(nexch,gklank) && (nexch!='e'))))) || (
(
inset(curch,gpcons)) && 
(nexch == '+')) || ((gvgl( 0,false," 1 1&1+1c2d&2de2el3es 1h3ing2ig4lEIk3nen3sen3ser3ste3sel2te3zel4zen 000000000000000000000000000000000")))
))   {
     gtracetask(rulefile_g0apho,175,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel176(void)
{
 
 initj;
 do {
 if ((curch == '+')) 
 if ( (((nexch == 'c') && (offch(2) == 'h')) || ((nexch == 'd') && 
(offch(2) == 's')) || ((nexch == 't') && (offch(2) == 'i'))))
 if ( (((offch(-2) == 'u') && (prech == 'n')) || ((offch(-3) == ' ') && 
(offch(-2) == 'o') && (prech == 'p')) || ((offch(-2) == 'o') && 
(prech == 'n')) || ((offch(-2) == 'a') && (prech == 'r'))))   {
     gtracetask(rulefile_g0apho,176,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel177(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ((nexch == 'n')) 
 if ( (((
inset(offch(2),gmseg))) || (
(offch(2) == 'd')) || ((offch(2) == 't')) || ((offch(2) == 's') && 
(offch(3) == 't')) || ((offch(2) == 's') && (offch(3) == 'd'))))
 if ((gvgl( -1,true," 3gek4misk3bek4 erk3+tk5 herk4verk4begr4verw3gew00000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,177,1);
     curch = 'E';

   }

  forwj;
 } while nextj;
}

static void regel178(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ((nexch == 'l')) 
 if ( (((offch(2) == 's')) || ((offch(2) == 'l')) || ((
inset(offch(2),gmson))) || (
(offch(2) == 't')) || ((offch(2) == 'd'))))
 if ( (((gvgl( -1,true," 3war2or3gat4bret4rret4vert4verv3bev3gez0000000000000000000000000000000000000000000000000000000000000"))) || (
(gvgl( -1,true," 4 tab4 lib3reb1c3ped3tad4ikad2ll4verm3gem3com2rn2sn3kap3gep5 rapp00000000000000000000000000000000000"))) || (
(
((inset(offch(-2),gklank) && (offch(-2)!='U'))) && 
((inset(offch(-2),gklank) && (offch(-2)!='u')))) && 
(prech == 'w')) || ((
((inset(offch(-3),gklank) && (offch(-3)!='a'))) && 
((inset(offch(-3),gklank) && (offch(-3)!='i')))) && 
(offch(-2) == 's') && (prech == 'p')) || ((
((inset(offch(-3),gklank) && (offch(-3)!='r'))) && 
((inset(offch(-3),gklank) && (offch(-3)!='o'))) && 
((inset(offch(-3),gklank) && (offch(-3)!='i')))) && 
(offch(-2) == 's') && (prech == 't')) || ((
inset(offch(-2),gmson)) && 
(prech == 'v')) || ((offch(-3) == 'm') && (offch(-2) == 'o') && 
(
(inset(prech, gmson) && 
inset(prech,gplang)))) || (
(offch(-3) == 'h') && (offch(-2) == 'o') && (
(inset(prech, gmson) && 
inset(prech,gplang)))) || (
(
inset(offch(-2),gpvoc)) && 
(prech == 'n')) || ((
(inset(offch(-3), gpcons) && 
inset(offch(-3),gpson) && 
inset(offch(-3),gmhoog))) && 
(offch(-2) == 'a') && (prech == 'm'))))   {
     gtracetask(rulefile_g0apho,178,1);
     curch = 'E';

   }

  forwj;
 } while nextj;
}

static void regel179(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == '+')) 
 if ((nexch == 's')) 
 if ((offch(2) == 't')) 
 if ((offch(3) == 'e')) 
 if (!(((offch(4) == 'n') && (offch(5) == 'e') && (offch(6) == 'n')) || (
(offch(4) == 'd') && (offch(5) == 'e') && (offch(6) == ' ')) || (
(offch(4) == 'm') && (offch(5) == ' ')) || ((offch(4) == 'm') && 
(offch(5) == 'm')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((
(!inset(offch(gy-1),gmvoc     )) && 
((inset(offch(gy-1),gklank) && (offch(gy-1)!='e'))))) || (
(
inset(offch(gy-3),gmseg)) && 
(
inset(offch(gy-2),gpcons)) && 
(offch(gy-1) == 'e')) || ((offch(gy-3) == ' ') && (offch(gy-2) == 'e') && 
(offch(gy-1) == 'k')) || ((offch(gy-2) == 'e') && (offch(gy-1) == 'e'))))   gstop = true;
      else {

 if ((
inset(offch(gy-1),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,179,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel180(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 's')) 
 if ((nexch == 't')) 
 if ((offch(2) == 'e')) 
 if ((offch(3) == 'r')) 
 if ((gvgl( -1,true," 5avond4aard5david3zee4rock4film3pop00000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,180,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel181(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ( (((nexch == 's') && (offch(2) == ' ')) || ((nexch == 't') && 
(offch(2) == '+') && (offch(3) == 'j'))))
 if ((gvgl( -1,true," 5waar+1Z2bb2dd2nd3nad3pad3@gd3tag3rag3lag2gg2ng2ll4mmel2mm2am2ym2yn2nn2pp3tip2rr2ur2ct00000000000000"))) 
   {
     gtracetask(rulefile_g0apho,181,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel182(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ((nexch == 's')) 
 if ( (((offch(2) == 'e') && (offch(3) == 'n') && (offch(4) == ' ')) || (
(
inset(offch(2),gmseg)))))
 if (!(((offch(-2) == 'n') && (prech == 's')) || ((offch(-2) == 'g') && 
(prech == 'd')) || ((offch(-2) == 'd') && (prech == 'r')) || (
(offch(-2) == 'k') && (prech == 's')) || ((prech == 'c')) || (
(prech == 'e')) || ((prech == ' ')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-2),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop) {

 if ((
inset(offch(gy-3),gpklem)))  gy = gy - 1; {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-3),gmseg)))    gstop = true;
      else {

 if ((
inset(offch(gy-3),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,182,1);
     curch = '&';

   }
}}}
  forwj;
 } while nextj;
}

static void regel183(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'e')) 
 if ((prech == 'd')) 
 if ((
(inset(offch(-2), gpson) && 
inset(offch(-2),gpcons)))) 
 if ((offch(-3) == 'e')) 
 if ((
inset(offch(gy-4),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-5),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gy-5),gpcons))) {

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
inset(offch(gx+1),gmseg)))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,183,1);
     curch = '&';

   }
}}
  forwj;
 } while nextj;
}

static void regel184(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ((
inset(nexch,gmseg))) 
 if ((
inset(prech,gpcons)))    {
     gtracetask(rulefile_g0apho,184,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel185(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ( (((nexch == 'n') && (offch(2) == ' ')) || ((nexch == ' '))))
 if ((prech == '+')) 
 if ((
inset(offch(-2),gpvoc)))    {
     gtracetask(rulefile_g0apho,185,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel186(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'e')) 
 if ((
(inset(nexch, gpson) && 
inset(nexch,gpcor)))) 
 if ((gvgl( 2,false," 1 2& 1+1E2d&2d 1s2t&2t 00000000000000000000000000000000000000000000000000000000000000000000000000000"))) 

 if ((
inset(prech,gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-2),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,186,1);
     curch = '&';

   }
}
  forwj;
 } while nextj;
}

static void regel187(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((curch == 'e')) 
 if ((
inset(offch(gy-1),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if (!(((
inset(offch(gy-2),gmklem))) || (
(offch(gy-4) == ' ') && (offch(gy-3) == 'g') && (offch(gy-2) == 'e'))))   gstop = true;
      else {

 if ((
inset(offch(gy-2),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop) 
 if ((
inset(offch(gx+1),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if (!(((
inset(offch(gx+2),gmvoc))) || (
(
inset(offch(gx+2),gpvoc)) && 
(offch(gx+3) == ' ')) || ((offch(gx+2) == 'e') && (
(inset(offch(gx+3), gpson) && 
inset(offch(gx+3),gpcons))) && 
(
(!inset(offch(gx+4),gmvoc     )) && 
((inset(offch(gx+4),gklank) && (offch(gx+4)!='a'))) && 
((inset(offch(gx+4),gklank) && (offch(gx+4)!='o'))) && 
((inset(offch(gx+4),gklank) && (offch(gx+4)!='u'))) && 
((inset(offch(gx+4),gklank) && (offch(gx+4)!='A'))) && 
((inset(offch(gx+4),gklank) && (offch(gx+4)!='y'))) && 
((inset(offch(gx+4),gklank) && (offch(gx+4)!='@'))) && 
((inset(offch(gx+4),gklank) && (offch(gx+4)!='O'))) && 
((inset(offch(gx+4),gklank) && (offch(gx+4)!='U'))))) || (
(gvgl( gx+2,false," 1&3EIk2en2i+2ic2is3ii+3ii 3iii2ig3ing3ion00000000000000000000000000000000000000000000000000000000000")))
))   gstop = true;
      else {

 if ((
inset(offch(gx+2),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,187,1);
gcorlengt(1);
     curch = '&';
     nexch = '+';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel188(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'v')) 
 if ((nexch == 'e')) 
 if ((offch(2) == 'r')) 
 if ((
inset(prech,gmvoc)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if (!(((offch(gx+3) == 'i') && (offch(gx+4) == 'n') && (offch(gx+5) == 'g')) || (
(
inset(offch(gx+3),gmklem)))))   gstop = true;
      else {

 if ((
inset(offch(gx+3),gpseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,188,1);
gcorlengt(1);
     curch = 'v';
     nexch = '&';
     offch(2) = 'r';
     offch(3) = '+';

     gotoj(3);
   }
}
  forwj;
 } while nextj;
}

static void regel189(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ( (((prech == 'g')) || ((prech == 'b'))))
 if ((
inset(nexch,gmson))) 
 if ( (((
inset(offch(2),gpklem))) || (
(
(!inset(offch(2),gmcons    )) && 
((inset(offch(2),gklank) && (offch(2)!='f'))) && 
((inset(offch(2),gklank) && (offch(2)!='v'))) && 
((inset(offch(2),gklank) && (offch(2)!='s'))) && 
((inset(offch(2),gklank) && (offch(2)!='z'))) && 
((inset(offch(2),gklank) && (offch(2)!='g'))) && 
((inset(offch(2),gklank) && (offch(2)!='b'))) && 
((inset(offch(2),gklank) && (offch(2)!='d'))) && 
((inset(offch(2),gklank) && (offch(2)!='k'))) && 
((inset(offch(2),gklank) && (offch(2)!='h')))) && 
(
inset(offch(3),gpklem))) || (
(
(!inset(offch(2),gpstem    )) && 
((inset(offch(2),gklank) && (offch(2)!='k'))) && 
((inset(offch(2),gklank) && (offch(2)!='f'))) && 
((inset(offch(2),gklank) && (offch(2)!='s'))) && 
((inset(offch(2),gklank) && (offch(2)!='c'))) && 
((inset(offch(2),gklank) && (offch(2)!='+'))) && 
((inset(offch(2),gklank) && (offch(2)!=' ')))) && 
(
(inset(offch(3), gpcor) && 
inset(offch(3),gmnas) && 
inset(offch(3),gpson))) && 
(
inset(offch(4),gpklem)))))
 if (nexch != offch(2))    {
     gtracetask(rulefile_g0apho,189,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel190(void)
{
 
 initj;
 do {
 if ((curch == 'e')) 
 if ( (((prech == 'g')) || ((prech == 'b'))))
 if ( (((
(inset(nexch, gpson) && 
inset(nexch,gpcons))) && 
(
inset(offch(2),gpklem))) || (
(
(!inset(nexch,gmvoc     )) && 
((inset(nexch,gklank) && (nexch!='e')))) && 
(
inset(offch(2),gpseg)))))   {
     gtracetask(rulefile_g0apho,190,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel191(void)
{
 
 initj;
 do {
 if ((prech == 'n')) 
 if ( (((offch(-2) == 'o')) || ((offch(-2) == 'i'))))
 if ((offch(-3) == '+')) 
 if (!(((curch == 'n') && (nexch == 'e') && (offch(2) == 'r')) || (
(curch == '&') && (nexch == 'l') && (offch(2) == '&'))))   {
     gtracetask(rulefile_g0apho,191,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel192(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ((
inset(prech,gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-2),gpklem)))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop) 
 if ((
(inset(nexch, gpson) && 
inset(nexch,gpcor)))) 
 if ( (((
inset(offch(2),gpvoc)) && 
(
((inset(offch(3),gklank) && (offch(3)!=' '))))) || (
(
(inset(offch(2), gmson) && 
inset(offch(2),gpcons))) && 
(
inset(offch(3),gpklem)) && 
(
((inset(offch(4),gklank) && (offch(4)!='i'))) && 
((inset(offch(4),gklank) && (offch(4)!=' '))))) || (
(
(inset(offch(2), gpson) && 
inset(offch(2),gpcons))) && 
(
((inset(offch(3),gklank) && (offch(3)!=' '))) && 
((inset(offch(3),gklank) && (offch(3)!='&'))))) || (
(offch(2) == 'b') && (offch(3) == '&') && (
((inset(offch(4),gklank) && (offch(4)!=' '))))) || (
(offch(2) == 'g') && (offch(3) == '&') && (
((inset(offch(4),gklank) && (offch(4)!=' ')))))
))
 if (nexch != offch(2))    {
     gtracetask(rulefile_g0apho,192,1);
gcorlengt(1);
     curch = '&';
     nexch = '+';

     forwj;
   }
}
  forwj;
 } while nextj;
}

/*--- STRING RULES ---------------------------------------------*/

void fono_grapho_0_3()
{
 regel121(); regel122(); regel123(); regel124(); regel125(); regel126(); regel127(); regel128();
 regel129(); regel130(); regel131(); regel132(); regel133(); regel134(); regel135(); regel136();
 regel137(); regel138(); regel139(); regel140(); regel141(); regel142(); regel143(); regel144();
 regel145(); regel146(); regel147(); regel148(); regel149(); regel150(); regel151(); regel152();
 regel153(); regel154(); regel155(); regel156(); regel157(); regel158(); regel159(); regel160();
 regel161(); regel162(); regel163(); regel164(); regel165();

 initj; 
 do {
 if ((curch == 'e')) 
 if (strnj) {
    gplace_venster(); regel166(); regel167(); regel168();
 regel169();
 backj; 
 }
 forwj; 
 } while strnj;ginit_venster();
 regel170(); regel171(); regel172(); regel173(); regel174(); regel175(); regel176();


 initj; 
 do {
 if ((curch == 'e')) 
 if (strnj) {
    gplace_venster(); regel177(); regel178(); regel179(); regel180(); regel181(); regel182(); regel183(); regel184();
 regel185(); regel186(); regel187(); regel188(); regel189(); regel190(); regel191(); regel192();

 backj; 
 }
 forwj; 
 } while strnj;ginit_venster();
}

/*--- PARAMETRISATION RULES ------------------------------------*/
void fone_grapho_0_3()
{
}

