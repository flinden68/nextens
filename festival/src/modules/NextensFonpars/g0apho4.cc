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

void ovl_grapho0_4()
{
}


static void regel193(void)
{
 
 initj;
 do {
 if ((prech == 'n')) 
 if ( (((offch(-2) == 'o')) || ((offch(-2) == 'i'))))
 if ((offch(-3) == '+')) 
 if (!(((curch == 'n') && (nexch == 'e') && (offch(2) == 'r')) || (
(curch == '&') && (nexch == 'l') && (offch(2) == '&'))))   {
     gtracetask(rulefile_g0apho,193,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel194(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == '&')) 
 if ((nexch == 'l')) 
 if ((offch(2) == '&')) {

 if ((offch(gx+3) == 'n'))  gx = gx + 1;
 if ((offch(gx+3) == ' ')) 
 if ( (((offch(-3) == 'E') && (offch(-2) == 'n') && (prech == 't')) || (
(offch(-3) == 't') && (offch(-2) == 'u') && (prech == 'r')) || (
(prech == 'n')) || ((
inset(offch(-2),gmstem)) && 
(prech == 'd')) || ((prech == 'u')) || ((offch(-4) == 'v') && 
(offch(-3) == 'e') && (offch(-2) == 'r') && (prech == 'z')) || (
(offch(-2) == 'r') && (prech == 'm'))))   {
     gtracetask(rulefile_g0apho,194,1);
gcorlengt(2);
     curch = '\'';
     nexch = 'e';
     offch(2) = 'e';

     gotoj(2);
   }
}
  forwj;
 } while nextj;
}

static void regel195(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'E')) 
 if ((nexch == 'I')) 
 if ((offch(2) == 'k')) 
 if (!(((offch(3) == '+') && (offch(4) == 'j') && (offch(5) == '&'))))
 if ((prech == 'l')) {

 if ((offch(gy-2) == 'r'))  gy = gy - 1;{

 if ((offch(gy-3) == '&')) 
 if ((offch(gy-2) == '+'))  gy = gy - 2; {
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
     gtracetask(rulefile_g0apho,195,1);
gcorlengt(-1);
     curch = '&';

   }
}}}
  forwj;
 } while nextj;
}

static void regel196(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'i')) 
 if ((nexch == 'g')) 
 if ( (((
(inset(offch(2), gmson) && 
inset(offch(2),gpcor)))) || (
(offch(2) == '&'))))
 if (!(((offch(3) == '+') && (offch(4) == 'r') && (offch(5) == '&'))))
 if ((
inset(prech,gpcons))) {

 if ((offch(gy-3) == '&')) 
 if ((offch(gy-2) == '+'))  gy = gy - 2; {
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
     gtracetask(rulefile_g0apho,196,1);
     curch = '&';

   }
}}
  forwj;
 } while nextj;
}

static void regel197(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ((nexch == 'm')) 
 if ((
inset(offch(gy-1),gpcons)))  {
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

 if ( (((offch(gy-3) == '&')) || ((offch(gy-3) == '+')) || ((offch(gy-3) == ' '))))   gstop = true;
      else {

 if ((
inset(offch(gy-3),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,197,1);
     curch = '&';

   }
}}}
  forwj;
 } while nextj;
}

static void regel198(void)
{
 
 initj;
 do {
 if ((
inset(curch,gplang))) 
 if ( (((
inset(nexch,gpcons)) && 
(
(!inset(offch(2),gpvoc     )) && 
((inset(offch(2),gklank) && (offch(2)!='r'))))) || (
(nexch == 'r') && (offch(2) == 'r')) || ((nexch == 'S')) || (
(
inset(nexch,gpcons)) && 
(offch(2) == 'r') && (
inset(offch(3),gpvoc)) && 
(
((inset(offch(4),gklank) && (offch(4)!=' ')))))))
 if ( (((
inset(prech,gklank))) || (
(prech == '\''))))
 if (curch != prech)    {
     gtracetask(rulefile_g0apho,198,1);
     gverander(0,1,(&gmlang    ));
   }

  forwj;
 } while nextj;
}

static void regel199(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'h')) 
 if ( (((offch(2) == 'I') && (offch(3) == 's')) || ((offch(2) == '&')) || (
(offch(2) == 'o') && (
((inset(offch(3),gklank) && (offch(3)!='o'))))) || (
(offch(2) == 'i'))))
 if (!(((offch(3) == 'r') && (offch(4) == '&'))))
 if (!(((offch(-2) == 'U') && (prech == 'n')) || ((offch(-2) == 'f') && 
(prech == 'I')) || ((offch(-2) == 'm') && (prech == 'A')) || (
(
inset(prech,gmseg)))))   {
     gtracetask(rulefile_g0apho,199,1);
gcorlengt(-1);
     curch = 'x';

   }

  forwj;
 } while nextj;
}

static void regel200(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'h')) 
 if ( (((
inset(offch(2),gmvoc))) || (
(
(inset(offch(2), gpvoc) && 
inset(offch(2),gplang) && 
inset(offch(2),gmhoog))) && 
(
(inset(offch(3), gpson) && 
inset(offch(3),gmnas) && 
inset(offch(3),gmant)))) || (
(
(inset(offch(2), gpvoc) && 
inset(offch(2),gplang) && 
inset(offch(2),gmhoog))) && 
(
(inset(offch(3), gpson) && 
inset(offch(3),gpnas) && 
inset(offch(3),gpant))))))
 if (!(((offch(-3) == 'm') && (offch(-2) == 'A') && (prech == 'r'))))   {
     gtracetask(rulefile_g0apho,200,1);
gcorlengt(-1);
     curch = 'x';

   }

  forwj;
 } while nextj;
}

static void regel201(void)
{
 
 initj;
 do {
 if ((curch == 'c')) 
 if ((nexch == 'h'))    {
     gtracetask(rulefile_g0apho,201,1);
gcorlengt(-1);
     curch = 'S';

   }

  forwj;
 } while nextj;
}

static void regel202(void)
{
 
 initj;
 do { gx = 0;

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

 if ((offch(gx+2) == 'i'))    gstop = true;
      else {

 if ((
inset(offch(gx+2),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((offch(gx+3) == 'i')) 
 if ((offch(gx+4) == 'i')) 
 if ((offch(gx+5) == 's')) 
 if ((
inset(prech,gmvoc)))    {
     gtracetask(rulefile_g0apho,202,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel203(void)
{
 
 initj;
 do {
 if ((
inset(curch,gpvoc))) 
 if ((
inset(nexch,gpvoc))) 
 if (curch == nexch)    {
     gtracetask(rulefile_g0apho,203,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel204(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == '&')) 
 if ((nexch == '+')) 
 if ((offch(2) == 'r')) 
 if ( (((offch(3) == '&')) || ((offch(3) == 'I'))))
 if ((offch(4) == 'n')) 
 if ( (((gvgl( -1,true," 3Orm3Onn4lOpp2yp2tt5kwEnt4pOrt4pret3dat2ct3lat4imit4stat3riv3mov3nov3&rv4grav4 lav000000000000000000"))) || (
(gvgl( -1,true," 2z+3rad4ikad4rAUd4alid3raf4vAkk3Okk3xak4sOmm4clam4blam0000000000000000000000000000000000000000000000"))) || (
(prech == 'j')) || (
(prech == 'c')) || ((
((inset(offch(-2),gklank) && (offch(-2)!='n')))) && 
(prech == 'n')) || ((prech == 'l')) || ((prech == 'r')) || (
(prech == 's')) || ((prech == 'Z')) || ((prech == 'z')) || (
(
((inset(offch(-3),gklank) && (offch(-3)!='s')))) && 
(offch(-2) == 'o') && (prech == 'b')) || ((offch(-2) == 'o') && 
(
inset(prech,gplang))) || (
(
((inset(offch(-4),gklank) && (offch(-4)!=' ')))) && 
(offch(-3) == 'O') && (offch(-2) == 'f') && (prech == 'f')) || (
(
((inset(offch(-3),gklank) && (offch(-3)!='m')))) && 
(offch(-2) == 'a') && (prech == 'g')) || ((
inset(offch(-3),gpcons)) && 
(
(!inset(offch(-2),gmvoc     )) && 
((inset(offch(-2),gklank) && (offch(-2)!='a'))) && 
((inset(offch(-2),gklank) && (offch(-2)!='y'))) && 
((inset(offch(-2),gklank) && (offch(-2)!='i'))) && 
((inset(offch(-2),gklank) && (offch(-2)!='@'))) && 
((inset(offch(-2),gklank) && (offch(-2)!='e'))) && 
((inset(offch(-2),gklank) && (offch(-2)!='o')))) && 
(
inset(prech,gpcons)))))   {
     gtracetask(rulefile_g0apho,204,1);
     curch = 'e';

   }

  forwj;
 } while nextj;
}

static void regel205(void)
{
 
 initj;
 do {
 if ((curch == '&')) 
 if ((nexch == '+')) 
 if ((offch(2) == 'r')) 
 if ( (((offch(3) == '&')) || ((offch(3) == 'I'))))
 if ((offch(4) == 'n')) 
 if ((
inset(prech,gpcons))) 
 if ((
inset(offch(-2),gpcons))) 
 if ( (((
((inset(offch(-5),gklank) && (offch(-5)!=' '))) && 
((inset(offch(-5),gklank) && (offch(-5)!='+')))) && 
(
((inset(offch(-4),gklank) && (offch(-4)!='f'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='v'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='w'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='z'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='p')))) && 
(offch(-3) == 'O')) || ((
((inset(offch(-4),gklank) && (offch(-4)!='k'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='w'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='h'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='l'))) && 
((inset(offch(-4),gklank) && (offch(-4)!=' ')))) && 
(offch(-3) == 'E')) || ((
((inset(offch(-4),gklank) && (offch(-4)!='k'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='w'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='+'))) && 
((inset(offch(-4),gklank) && (offch(-4)!=' ')))) && 
(offch(-3) == 'A')) || ((
((inset(offch(-4),gklank) && (offch(-4)!='w'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='g'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='U'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='E'))) && 
((inset(offch(-4),gklank) && (offch(-4)!=' '))) && 
((inset(offch(-4),gklank) && (offch(-4)!='x'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='v'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='l'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='m'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='k'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='h')))) && 
(offch(-3) == 'I')) || ((offch(-5) == 's') && (offch(-4) == 'p') && 
(
(inset(offch(-3), gpvoc) && 
inset(offch(-3),gmlang)))) || (
(
((inset(offch(-4),gklank) && (offch(-4)!='p'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='t'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='h'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='r'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='l'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='b')))) && 
(offch(-3) == 'U')) || ((
((inset(offch(-5),gklank) && (offch(-5)!=' ')))) && 
(offch(-4) == 'l') && (offch(-3) == 'A')) || ((offch(-4) == 'm') && 
(offch(-3) == 'O')) || ((offch(-4) == 's') && (offch(-3) == 'O')) || (
(offch(-5) == 'e') && (offch(-4) == 'p') && (offch(-3) == 'O')) || (
(offch(-4) == ' ') && (offch(-3) == 'O')) || ((
((inset(offch(-4),gklank) && (offch(-4)!='o'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='e'))) && 
((inset(offch(-4),gklank) && (offch(-4)!='I')))) && 
(offch(-3) == '+'))))
 if (prech != offch(-2))    {
     gtracetask(rulefile_g0apho,205,1);
     curch = 'e';

   }

  forwj;
 } while nextj;
}

static void regel206(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'e')) 
 if ((nexch == '+')) 
 if ((offch(2) == 'r')) 
 if ( (((offch(3) == '&')) || ((offch(3) == 'I'))))
 if ((offch(4) == 'n')) 
 if ( (((gvgl( -1,true," 4sOmb3Ilf4vlig4 Ank3Imk4lOnk4mArm3Emp4 Ett4xItt3Ast4Anst4 bot4lOnt3Utt4kAlv4zIlv3laz2+z0000000000000"))) || (
(
((inset(offch(-4),gklank) && (offch(-4)!='c')))) && 
(offch(-3) == 'O') && (offch(-2) == 't') && (prech == 't')) || (
(offch(-4) == 'l') && (offch(-3) == 'E') && (offch(-2) == 't') && 
(prech == 't')) || ((
inset(offch(-5),gmseg)) && 
(
inset(offch(-4),gpcons)) && 
(offch(-3) == 'E') && (offch(-2) == 't') && (prech == 't')) || (
(
((inset(offch(-3),gklank) && (offch(-3)!='s')))) && 
(offch(-2) == '+') && (prech == 'j'))))   {
     gtracetask(rulefile_g0apho,206,1);
     curch = '&';

   }

  forwj;
 } while nextj;
}

static void regel207(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if ((nexch == 'i')) 
 if ((offch(2) == '+')) 
 if ( (((
(inset(offch(3), gpcor) && 
inset(offch(3),gpstrid)))) || (
(
inset(offch(3),gpcons)) && 
(
inset(offch(4),gpklem))) || (
(
inset(offch(3),gpcons)) && 
(
inset(offch(4),gpcons)) && 
(
inset(offch(5),gpklem))) || (
(offch(3) == 'b') && (offch(4) == '&')) || ((offch(3) == 'g') && 
(offch(4) == '&')) || ((offch(3) == 'v') && (offch(4) == '&') && 
(offch(5) == 'r') && (offch(6) == '+'))))
 if ( (((prech == 'c')) || ((prech == 'n')) || ((
(inset(prech, gpcons) && 
inset(prech,gmcor))))
))   {
     gtracetask(rulefile_g0apho,207,1);
     curch = 's';

   }

  forwj;
 } while nextj;
}

static void regel208(void)
{
 
 initj;
 do {
 if ((curch == 't')) 
 if ((nexch == 'i')) 
 if ((offch(2) == '+')) 
 if ( (((
(inset(offch(3), gpcor) && 
inset(offch(3),gpstrid)))) || (
(
inset(offch(3),gpcons)) && 
(
inset(offch(4),gpklem))) || (
(
inset(offch(3),gpcons)) && 
(
inset(offch(4),gpcons)) && 
(
inset(offch(5),gpklem))) || (
(offch(3) == 'b') && (offch(4) == '&')) || ((offch(3) == 'g') && 
(offch(4) == '&')) || ((offch(3) == 'v') && (offch(4) == '&') && 
(offch(5) == 'r') && (offch(6) == '+'))))
 if ((
inset(prech,gpklem))) 
 if ( (((
inset(offch(-2),gpcons))) || (
(offch(-2) == 'c')) || ((offch(-2) == 'n')) || ((
(inset(offch(-2), gpcons) && 
inset(offch(-2),gmcor))))
))   {
     gtracetask(rulefile_g0apho,208,1);
gcorlengt(1);
     curch = 't';
     nexch = 's';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel209(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'i')) {

 if ((offch(gx+1) == '+'))  gx = gx + 1;
 if ((offch(gx+1) == 'j')) 
 if ( (((offch(gx+2) == 'O')) || ((offch(gx+2) == 'e')) || ((offch(gx+2) == 'a')) || (
(offch(gx+2) == 'o')))) {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+3) == 'l')) || ((offch(gx+3) == 'r')) || ((offch(gx+3) == '+') && 
(offch(gx+4) == 'r')) || ((offch(gx+3) == 'n') && (offch(gx+4) == '+')) || (
(offch(gx+3) == 'n') && (offch(gx+4) == '\'')) || ((offch(gx+3) == 't') && 
(offch(gx+4) == 'i')) || ((offch(gx+3) == 't') && (offch(gx+4) == 's') && 
(offch(gx+5) == 'i'))))   gstop = true;
      else {

 if ((
inset(offch(gx+3),gpseg))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ( (((prech == 'c')) || ((prech == 'z')) || ((prech == 's'))))
 if (!(((offch(-4) == ' ') && (offch(-3) == 'A') && (offch(-2) == 'k'))))   {
     gtracetask(rulefile_g0apho,209,1);
gcorlengt(-1);

     backj;
   }
}}
  forwj;
 } while nextj;
}

static void regel210(void)
{
 
 initj;
 do {
 if ((curch == 's')) 
 if ( (((offch(-5) == 'i') && (offch(-4) == '+') && (offch(-3) == 'v') && 
(offch(-2) == 'E') && (prech == 'r')) || ((offch(-4) == 'n') && 
(offch(-3) == 'a') && (offch(-2) == 'l') && (prech == 'i')) || (
(prech == 'u')) || ((prech == 'o')) || ((
((inset(offch(-2),gklank) && (offch(-2)!='p'))) && 
((inset(offch(-2),gklank) && (offch(-2)!='n')))) && 
(prech == 'e')) || ((prech == 'a'))))
 if ( (((nexch == '&') && (
inset(offch(2),gmseg))) || (
(nexch == 'i') && (
inset(offch(2),gmvoc))) || (
(nexch == 'E') && (
inset(offch(2),gmvoc)))
))   {
     gtracetask(rulefile_g0apho,210,1);
     curch = 'z';

   }

  forwj;
 } while nextj;
}

static void regel211(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if (inset(curch,gklank))
 if (   (
(!inset(curch,gmcons    )) && 
((inset(curch,gklank) && (curch!='h'))) && 
((inset(curch,gklank) && (curch!='g')))) ) 
 if ((
inset(nexch,gpklem)))  {
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
  if (gstop) 
 if ((
inset(prech,gpcons))) 
 if ((
inset(offch(-2),gpklem))) 
 if ((
inset(offch(gy-3),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gy-4),gmseg)))    gstop = true;
      else {

 if ((
inset(offch(gy-4),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,211,1);
gcorlengt(1);
     curch = '+';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel212(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'I')) 
 if ((nexch == 'n')) 
 if ((offch(2) == 'g')) 
 if ((
inset(prech,gpcons)))  {
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
     gtracetask(rulefile_g0apho,212,1);
gcorlengt(-1);
     curch = '&';
     nexch = 'N';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel213(void)
{
 
 initj;
 do { gx = 0;
{

 if ((offch(gx) == '+'))  gx = gx + 1;
 if ( (((offch(gx) == 'e')) || ((offch(gx) == 'a'))))
 if ( (((offch(gx+1) == 'l') && (offch(gx+2) == ' ')) || ((offch(gx+1) == 'l') && 
(offch(gx+2) == '&') && (offch(gx+3) == ' ')) || ((offch(gx+1) == 'n') && 
(offch(gx+2) == 's') && (offch(gx+3) == ' '))))
 if ( (((
inset(offch(-2),gpcons)) && 
(
inset(prech,gpcons))) || (
(offch(-3) == '&') && (offch(-2) == '+') && (prech == 'r')) || (
(offch(-2) == '&') && (prech == 'r')) || ((offch(-3) == 'O') && 
(offch(-2) == 'p') && (prech == '+')) || ((offch(-3) == 'O') && 
(offch(-2) == 'n') && (prech == '+')) || ((prech == 'j'))))   {
     gtracetask(rulefile_g0apho,213,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel214(void)
{
 
 initj;
 do {
 if ((curch == '+')) 
 if ((nexch == 't')) 
 if ( (((
inset(offch(-4),gmseg)) && 
(offch(-3) == 'I') && (offch(-2) == 'n') && (prech == 's')) || (
(
inset(offch(-3),gmseg)) && 
(offch(-2) == 'O') && (prech == 'n'))))   {
     gtracetask(rulefile_g0apho,214,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel215(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == '+'))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == 't'))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop) 
 if ((offch(gy-2) == 'n')) 
 if ((offch(gy-3) == 'O')) 
 if ((
inset(offch(gy-4),gmseg)))    {
     gtracetask(rulefile_g0apho,215,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel216(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == 'e')) {

 if ((offch(gx+1) == '+'))  gx = gx + 1;
 if ((offch(gx+1) == 'r')) 
 if ( (((offch(gx+2) == '&') && (offch(gx+3) == 'N')) || ((offch(gx+2) == 't')) || (
(offch(gx+2) == 'd')) || ((offch(gx+2) == '&') && (offch(gx+3) == 'n'))))   {
     gtracetask(rulefile_g0apho,216,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel217(void)
{
 
 initj;
 do {
 if ((curch == '+')) 
 if ( (((
(inset(prech, gpcons) && 
inset(prech,gpson)))) || (
(prech == 'c')) || ((
(inset(prech, gmcont) && 
inset(prech,gmson))))
))
 if ((offch(-2) == 'A')) 
 if ((offch(-3) == ' '))    {
     gtracetask(rulefile_g0apho,217,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel218(void)
{
 
 initj;
 do {
 if ((curch == '+')) 
 if ((prech == 't')) 
 if ( (((offch(-2) == 'r')) || ((
inset(offch(-2),gpvoc)))
))
 if ((
inset(nexch,gpvoc)))    {
     gtracetask(rulefile_g0apho,218,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel219(void)
{
 
 initj;
 do {
 if ((curch == '+')) 
 if ( (((offch(-4) == ' ') && (offch(-3) == 'c') && (offch(-2) == 'O') && 
(
inset(prech,gpcons))) || (
(offch(-3) == ' ') && (offch(-2) == 'O') && (prech == 'b')) || (
(offch(-4) == ' ') && (offch(-3) == 'f') && (offch(-2) == 'I') && 
(prech == 's')) || ((offch(-2) == ' ') && (prech == '@')) || (
(offch(-4) == ' ') && (offch(-3) == 'd') && (offch(-2) == 'I') && 
(prech == 's')) || ((offch(-4) == ' ') && (offch(-3) == 's') && 
(offch(-2) == 'U') && (prech == 'b'))))   {
     gtracetask(rulefile_g0apho,219,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel220(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == '+')) 
 if ((
inset(nexch,gpseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,220,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel221(void)
{
 
 initj;
 do { gx = 0;

 if ((curch == '+'))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+1) == ' '))    gstop = true;
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
inset(prech,gpseg)))    {
     gtracetask(rulefile_g0apho,221,1);
gcorlengt(-1);

     backj;
   }
}
  forwj;
 } while nextj;
}

static void regel222(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((gvgl( 0,false," 2Eg5Idd&l2ad3bar2eg2en2in2og4ov&r0000000000000000000000000000000000000000000000000000000000000000000"))) 

 if ((gvgl( -1,true," 3an+7Ind&+rd4In+m4On+m6ov&r+l6ov&r+w6ov&r+z5op&+n6teg&+n3UIt5vor+z0000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,222,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel223(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(offch(gy-1),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-2) == '+'))    gstop = true;
      else {

 if ((
inset(offch(gy-2),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop) 
 if ((offch(gy-3) == 'r')) 
 if ((offch(gy-4) == '&')) 
 if ((offch(gy-5) == 'd')) 
 if ((offch(gy-6) == 'n')) 
 if ((offch(gy-7) == 'O')) 
 if ((
inset(curch,gpklem))) {

 if ((
inset(offch(gx+1),gpklem)))  gx = gx + 1;{

 if ((offch(gx+1) == '+'))  gx = gx + 1;
 if ((
inset(offch(gx+1),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+2) == '&') && (offch(gx+3) == 'n') && (offch(gx+4) == ' ')) || (
(offch(gx+2) == 't') && (offch(gx+3) == ' ')) || ((offch(gx+2) == 'd') && 
(offch(gx+3) == '&') && (offch(gx+4) == ' '))))   gstop = true;
      else {

 if ((
inset(offch(gx+2),gpcons))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,223,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}}}
  forwj;
 } while nextj;
}

static void regel224(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == '+')) 
 if ((gvgl( -1,true," 3 Ec4 rEf5 cOnf3sIg4 mAg4 pIg4 sEg3 Ek3 En4 pro6 parAp4 App5 pErp5 cOmp4 jAs4 fOs3 pu000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,224,1);
gcorlengt(-1);

     backj;
   }

  forwj;
 } while nextj;
}

static void regel225(void)
{
 
 initj;
 do { gy = 0;

 if ((
inset(curch,gpklem)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((gvgl( gy+-1,true," 6 tel&+5 hEr+9 Ek+splic6 Socol3 On7 kam&+r4&rat8 In+stit7 sEItz+000000000000000000000000000000000000"))) 
   gstop = true;
      else {

 if ((
inset(offch(gy-1),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,225,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel226(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(curch,gpklem))) {

 if ((
inset(offch(gx+1),gpklem)))  gx = gx + 1; {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((
inset(offch(gx+1),gmseg)))    gstop = true;
      else {

 if ((
inset(offch(gx+1),gmklem))) {

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
inset(offch(gy-1),gmklem))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,226,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}}
  forwj;
 } while nextj;
}

static void regel227(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(curch,gpklem))) 
 if ((
inset(offch(gx+1),gmklem)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gx+2) == 'u'))    gstop = true;
      else {

 if ((
inset(offch(gx+2),gmklem))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((offch(gx+3) == 'w')) {

 if ((offch(gx+4) == '&'))  gx = gx + 1;
 if ((
inset(offch(gx+4),gmseg)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gmklem))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,227,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}}
  forwj;
 } while nextj;
}

static void regel228(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(curch,gpklem))) 
 if ((
(inset(offch(gx+1), gpseg) && 
inset(offch(gx+1),gmklem))))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((gvgl( gx+2,false," 3+ci3AU 4Ops 3am 3ams4am&n4ard 5bEId 6bEId+s4ort 3tin4yts 000000000000000000000000000000000000000000"))) 
   gstop = true;
      else {

 if ((
(inset(offch(gx+2), gpseg) && 
inset(offch(gx+2),gmklem)))) {

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
inset(offch(gy-1),gpseg))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,228,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel229(void)
{
 
 initj;
 do { gx = 0;

 if ((
inset(curch,gpklem))) {

 if ((
inset(offch(gx+1),gpklem)))  gx = gx + 1;{

 if ((offch(gx+1) == '+'))  gx = gx + 1;
 if ((
(inset(offch(gx+1), gpseg) && 
inset(offch(gx+1),gmklem))))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((gvgl( gx+2,false," 2&g3&nd4+l&k4+bar3bar3l&k3si+3si 4sis 00000000000000000000000000000000000000000000000000000000000000"))) 
   gstop = true;
      else {

 if ((
(inset(offch(gx+2), gpseg) && 
inset(offch(gx+2),gmklem)))) {

   gverhoogd = true; 
   gx = gx + 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && rghtj);
  if (gstop) 
 if ((!
inset(prech,gpklem)))    {
     gtracetask(rulefile_g0apho,229,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}}
  forwj;
 } while nextj;
}

static void regel230(void)
{
 
 initj;
 do { gx = 0;

 if ( (((gvgl( 0,false," 4Ent 5En+ts4Ent&6arijUm5arija3el 4el& 3iv&5idis 0000000000000000000000000000000000000000000000000000"))) || (
(
inset(curch,gpvoc)) && 
(nexch == 'Z')) || ((curch == 'i') && (nexch == 'f') && 
(
inset(offch(2),gmseg)))))   {
     gtracetask(rulefile_g0apho,230,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel231(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(curch,gpklem)))  {
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

 if ((offch(gx+2) == '&'))    gstop = true;
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

 if ((offch(gx+3) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gx+3),gklank))) {

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
inset(offch(gy-1),gmvoc))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,231,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}}}
  forwj;
 } while nextj;
}

static void regel232(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(curch,gpklem))) 
 if ((
inset(nexch,gpklem)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+2) == '&') && (offch(gx+3) == 's')) || ((offch(gx+2) == '&') && 
(
inset(offch(gx+3),gmseg))) || (
(offch(gx+2) == '&') && (offch(gx+3) == 'n') && (
inset(offch(gx+4),gmseg))) || (
(offch(gx+2) == '&') && (offch(gx+3) == 'n') && (offch(gx+4) == 'd')) || (
(
inset(offch(gx+2),gmseg))) || (
(offch(gx+2) == '&') && (offch(gx+3) == 'N')) || ((offch(gx+2) == '&') && 
(offch(gx+3) == 'r') && (offch(gx+4) == ' ')) || ((offch(gx+2) == '&') && 
(offch(gx+3) == 'r') && (offch(gx+4) == '&'))))   gstop = true;
      else {

 if ((
inset(offch(gx+2),gmklem))) {

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
     gtracetask(rulefile_g0apho,232,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel233(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
(inset(curch, gpklem) && 
inset(curch,gplang)))) 
 if ((
inset(offch(gx+1),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+2) == '&') && (
inset(offch(gx+3),gmseg))) || (
(offch(gx+2) == '&') && (offch(gx+3) == 'n') && (
inset(offch(gx+4),gmseg))) || (
(
inset(offch(gx+2),gmseg)))))   gstop = true;
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
     gtracetask(rulefile_g0apho,233,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel234(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
(inset(curch, gpklem) && 
inset(curch,gmlang)))) 
 if ((
inset(nexch,gpcons))) 
 if ((
inset(offch(gx+2),gpcons)))  {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ( (((offch(gx+3) == '&') && (
inset(offch(gx+4),gmseg))) || (
(offch(gx+3) == '&') && (offch(gx+4) == 'n') && (
inset(offch(gx+5),gmseg))) || (
(
inset(offch(gx+3),gmseg)))))   gstop = true;
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
     gtracetask(rulefile_g0apho,234,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}}
  forwj;
 } while nextj;
}

static void regel235(void)
{
 
 initj;
 do { gy = 0; gx = 0;

 if ((
inset(curch,gpklem))) 
 if ((gvgl( 1,false," 4katy4lman4lfab4lbat4malU4ngyr5ranav4rlat4ratO000000000000000000000000000000000000000000000000000000"))) 
 {
    gstop = FALSE;
   do { 
    gverhoogd = FALSE;

 if ((offch(gy-1) == ' '))    gstop = true;
      else {

 if ((
inset(offch(gy-1),gpcons))) {

   gverhoogd = true; 
   gy = gy - 1;
  }
 }
 } while ((gverhoogd) && (!gstop) && leftj);
  if (gstop)    {
     gtracetask(rulefile_g0apho,235,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel236(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'o')) 
 if ((gvgl( -1,true," 4 kad4 cad4 bur4 niv00000000000000000000000000000000000000000000000000000000000000000000000000000000"))) 
   {
     gtracetask(rulefile_g0apho,236,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }

  forwj;
 } while nextj;
}

static void regel237(void)
{
 
 initj;
 do { gy = 0;

 if ( (((curch == 'e')) || ((curch == 'u')) || ((curch == '@'))))
 if ((
inset(nexch,gmseg))) 
 if (!(((prech == 'c')) || ((prech == '\'')) || ((prech == 'k')))) {
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
     gtracetask(rulefile_g0apho,237,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel238(void)
{
 
 initj;
 do { gy = 0;

 if ((
(inset(curch, gpklem) && 
inset(curch,gmlang)))) 
 if ((nexch == 'l')) 
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
     gtracetask(rulefile_g0apho,238,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel239(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'E')) 
 if ((
(inset(nexch, gmson) && 
inset(nexch,gmstem)))) 
 if ((
inset(offch(2),gmseg))) 
 if (!(((offch(-3) == 'E') && (offch(-2) == 'n') && (prech == 'n')) || (
(prech == '\'')) || ((prech == 'b')))) {
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
     gtracetask(rulefile_g0apho,239,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

static void regel240(void)
{
 
 initj;
 do { gy = 0;

 if ((curch == 'A')) 
 if ((
(inset(nexch, gmson) && 
inset(nexch,gmstem)))) 
 if ((
inset(offch(2),gmseg))) 
 if (!(((prech == '\'')) || ((offch(-2) == 'b') && (prech == 'b')) || (
(offch(-3) == 'p') && (offch(-2) == 'i') && (prech == 'j')) || (
(offch(-2) == 't') && (prech == 'l')) || ((prech == 'n')) || (
(prech == 'k')) || ((offch(-2) == 'a') && (prech == 'j')) || (
(prech == 'd')))) {
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
     gtracetask(rulefile_g0apho,240,1);
gcorlengt(1);
     curch = '\'';

     forwj;
   }
}
  forwj;
 } while nextj;
}

/*--- STRING RULES ---------------------------------------------*/

void fono_grapho_0_4()
{
 regel193(); regel194(); regel195(); regel196(); regel197(); regel198();

 initj; 
 do {
 if ((curch == 'c')) 
 if ((nexch == 'h')) 
 if (strnj) {
    gplace_venster(); regel199(); regel200();
 regel201();
 backj; 
 }
 forwj; 
 } while strnj;ginit_venster();
 regel202(); regel203();

 initj; 
 do {
 if ((curch == '&')) 
 if ((nexch == '+')) 
 if ((offch(2) == 'r')) 
 if (strnj) {
    gplace_venster(); regel204(); regel205(); regel206();
 backj; 
 }
 forwj; 
 } while strnj;ginit_venster();
 regel207(); regel208();
 regel209(); regel210(); regel211(); regel212(); regel213(); regel214(); regel215(); regel216();
 regel217(); regel218(); regel219(); regel220(); regel221(); regel222(); regel223(); regel224();
 regel225(); regel226(); regel227(); regel228(); regel229(); regel230(); regel231(); regel232();
 regel233(); regel234(); regel235(); regel236(); regel237(); regel238(); regel239(); regel240();
}

/*--- PARAMETRISATION RULES ------------------------------------*/
void fone_grapho_0_4()
{
}

