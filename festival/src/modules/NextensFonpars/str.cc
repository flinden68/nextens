/* STR.C */

#define NULL 0L

#include <ctype.h>
#include "std.h"
#include <varargs.h>

void *malloc(unsigned n);
void free(void *p);
void *memcpy(void *p, void *s, unsigned n);
void *memset(void *p, int c, unsigned n);
int atoi(char *);
int vprintf(char *s, va_list argv);
int vsprintf(char *s, char *fmt, va_list argv);
int printf(char *s, ...);
int sprintf(char *s, char *fmt, ...);
int strlen(char *s);
char *strcpy(char *d, char *s);
char *strchr(char *d, char c);
char *strncpy(char *d, char *s, unsigned n);
char *strncat(char *d, char *s, unsigned n);
void qsort(void *base, unsigned nmemb, unsigned size,
	   int (*compar)(void *, void *));        /* used to sort formants */

#include "trc.h"
#include "str.h"

static void *tmp_buf = 0L;

/* missing library VAX-C */

char *strupr(char *s)
{
 int i;
 for (i=0; i<strlen(s); i++) if ((s[i]>='a')&&(s[i]<='z')) s[i]-='a'-'A';
 return(s);
}

int stricmp(char *s, char *p)
{
 char d,c1,c2;
 d='a'-'A';
 for (; *s; ) {
   c1= *s++; c2= *p++;
   c1=((c1>='a')&&(c1<='z'))?(c1-d):c1;
   c2=((c2>='a')&&(c2<='z'))?(c2-d):c2;
   if (c1!=c2) return((c1==0)?(-1):1);
  }
 if (*p) return(1);
 return(0);
}

int strnicmp(char *s, char *p, int n)
{

 char d,c1,c2;
 d='a'-'A';
 for (; (*s) && n; n--) {
   c1= *s++; c2= *p++;
   c1=((c1>='a')&&(c1<='z'))?(c1-d):c1;
   c2=((c2>='a')&&(c2<='z'))?(c2-d):c2;
   if (c1!=c2) return((c1==0)?(-1):1);
  }
 if (*p) return(1);
 return(0);
}

/* doorgeefluik vprintf() en vsprintf() igv VAXC (geen %f bugje) */

static void *f2d(char *fmt, void *bufd)
{
 int sgn;
 double dbl;
 float f;
 char *s,*d,*p;
 if (tmp_buf==0L) tmp_buf=malloc(400);
 for (p=fmt,s=bufd,d=tmp_buf; (*p); p++)
   if ((*p)=='%') {
     while (((p[1]>='0')&&(p[1]<='9'))||(p[1]=='.')||(p[1]=='-')) p++;
     switch (p[1]) {
       case 'c': memcpy(d,s,1); d+=1; s+=1; p++; break;
       case 'd': memcpy(d,s,2); sgn=(d[1]&128); d+=2; s+=2;
                 d[0]=(sgn)?255:0; d[1]=d[0]; d+=2;
                 p++; break;
       case 's': memcpy(d,s,4); d+=4; s+=4; p++; break;
       case 'g':
       case 'f': memcpy(&f,s,4); dbl=f;
		 memcpy(d,&dbl,8); d+=8; s+=8; p++; break;
       case 'l': switch (p[2]) {
                   case 'd': memcpy(d,s,4); d+=4; s+=4; p+=2; break;
	           case 'f': memcpy(d,s,8); d+=8; s+=8; p+=2; break;
	          }
	         break;
      }
    }
 return(tmp_buf);
}

int lvprintf(char *fmt, void *b)
{ return(vprintf(fmt,f2d(fmt,b))); }

int lvsprintf(char *s, char *fmt, void *b)
{ return(vsprintf(s,fmt,f2d(fmt,b))); }


/*****************************************************************************/
/*** ANY ENVIRONMENT *********************************************************/

char *cspace(char *s)
/*
  Skip leading spaces in string "s" and return adress of first non-space
*/
{
 char *p;
 for (p=s; ((*p)==' ')&&((*p)!=0); p++);
 return(p);
}

int find_maxs(int numlist, char **slist)
/*
  Find maximum stringlength within list of strings
*/
{
 int i,l,max=0;
 for (i=0; i<numlist; i++) {
   if (slist[i]==NULL) continue;
   l=strlen(slist[i]);
   if (l>max) max=l;
  }
 return(max);
}

char *mkstring(char *s,char fillchar, int n)
/*
  Produce a new string containing head "s", filled out to n positions
  using "fillchar".
*/
{
 char *s1;
 if (s==NULL) return(NULL);
 if (n<=0) {
   s1=malloc(strlen(s)+1);
   strcpy(s1,s);
  } else {
      s1=malloc(n+1);
      memset(s1,fillchar,n);
      strncpy(s1,s,strlen(s));
      s1[n]=0;
     }
  return(s1);
}

void strtailcut(char *s, char fillchar)
/*
  Cut tail of "s" that consists entirely of "fillchar"
*/
{
 int i;
 if (s[0]==0) return;
 for (i=strlen(s)-1; (s[i]==fillchar)&&(i>=0); i--) s[i]=0;
}

void strtailadd(char *s, char fillchar, int n)
/*
  Add a tail to "s" of length "n" containing characters "fillchar"
 */
{
 int i,l;
 l=strlen(s);
 for (i=0; i<n; i++) s[l++]=fillchar;
 s[l]=0;
}

int split_string(char **tok, char *s, char *spdelims, char *actdelims)
/*
  Split string 's', leaving each token in a new string in string array "tok".
  When "spdelims" is encountered,


  s=NULL       -> tok[0]=NULL
  s[0]=0       -> tok[0]="", tok[1]=NULL
  strlen(s)=1  -> tok[0]=s[0], tok[1]=NULL
  strlen(s)>1  -> tok[0]=word/actdelim, tok[1]=w/adm, tok[n]=NULL
*/
{
 int i,tc=0;
 char s1[255];
 if (s==NULL) { tok[0]=NULL; return(0); }
 if (s[0]==0) { tok[0]=(char *)malloc(1); tok[0][0]=0; tok[1]=NULL; return(1); }
 s1[0]=0;
 for (i=0; i<strlen(s); i++) {
  if ((actdelims[0]!=0)&&(strchr(actdelims,s[i])!=NULL)) {
    if (s1[0]!=0) {
      tok[tc]=malloc(strlen(s1)+1);
      strcpy(tok[tc++],s1);
      s1[0]=0;
     }
    tok[tc]=(char *)malloc(2);
    tok[tc][0]=s[i];
    tok[tc++][1]=0;
   } else
      if (strchr(spdelims,s[i])==NULL)  strncat(s1,&s[i],1);
	else
	 if (s1[0]!=0) {
	   tok[tc]=malloc(strlen(s1)+1);
	   strcpy(tok[tc++],s1);
	   s1[0]=0;
	  }
  }
 if (s1[0]!=0) {
   tok[tc]=malloc(strlen(s1)+1);
   strcpy(tok[tc++],s1);
  }
 return(tc);
}

int part_string(char **tok, char *s, char *spdelims)
/*
  s=NULL       -> tok[0]=NULL
  s[0]=0       -> tok[0]="", tok[1]=NULL
  strlen(s)=1  -> tok[0]=s[0], tok[1]=NULL
  strlen(s)>1  -> tok[0]=word/actdelim, tok[1]=w/adm, tok[n]=NULL
*/
{
 int i,tc=0;
 char *p;
 if (s==NULL) { tok[0]=NULL; return(0); }
 if (s[0]==0) { tok[0]=s; tok[1]=NULL; return(1); }
 for (i=0,p=s; s[i]; i++)
   if (strchr(spdelims,s[i])!=NULL) {
     tok[tc++]=p;
     p=(&(s[i])); (*p)=0; p++;
    }
 tok[tc++]=p;
 return(tc);
}

static int cmp_str(void *fb1, void *fb2)
{
 if ((*(unsigned long *)fb1)<(*(unsigned long *)fb2)) return(-1);
   else return(1);
}

int tokenize(char *s, char *tok[], char *delims, char *occdelims, int new)
/*
  Split string 's' in tokens tok[]. Return number of tokens found, leave
  successive delimiters in 'occdelims', recognize delimiters in 'delims'.
  When 'new' is 1, produce tok[] elements on heap. When new=0, tok[] elements
  point to positions in 's', and 0's are placed in 's' at end of each token.
  Method: for all delimiters, store successive positions in tok[]. Then qsort
  and update each tok[] to point at token after delimiter. tok[0] always
  holds 's'. This is a faster alternative to part_string() in case of long
  search strings. This is a NiceWare copy right...
*/
{
 char *p,*pv,c;
 int i,k,tc;
 if ((s==NULL)||(s[0]==0)) return(0);
 for (i=tc=0,p=pv=s; delims[i]>0; i++,p=s)
   while ((p=strchr(p,delims[i]))!=NULL) tok[tc++]=p++;
 qsort(tok,tc,sizeof(void *),cmp_str);
 if (new) {
   for (i=0; i<tc; i++) {
     p=tok[i];
     occdelims[i]=(*p);
     c=(*p);
     (*p)=0;
     tok[i]=malloc(strlen(pv)+1);
     strcpy(tok[i],pv);
     (*p)=c;
     pv=p+1;
    }
   tok[tc]=malloc(strlen(pv)+1);
   strcpy(tok[tc],pv);
  } else {
      for (i=0; i<tc; i++) {
	p=tok[i];
	occdelims[i]=(*p);
	(*p)=0;
	tok[i]=pv;
	pv=p+1;
       }
     tok[tc]=pv;
    }
 occdelims[tc]=0;
 return(tc+1);
}

char *getintpars(char *p, short *pvv, char *pcd, char delimiter)
/*
  Fetch integers in string "p" delimited by "delimiter".
  Put values in "pvv" and delimiters found in "pcd".
  Return adress in "p" where first non-number non-delimiter
  found. This procedure does string->int conversion without
  any system library use.
*/
{
 int2 i,k,tc,pv,pvc=0,n[25];
 pcd[0]=0;
 do {
   k=0;
   while (((*p)>47)&&((*p)<58)) n[k++]=(*p++)-48;
   for (tc=1, pv=0, i=k; (i--); tc*=10) pv+=n[i]*tc;
   pvv[pvc]=pv;
   pcd[pvc]=(*p);
   pvc++; if ((*p)==delimiter) p++;
  } while (((*p)>47)&&((*p)<58));
 pcd[pvc]=0;
 return(p);
}

int convert_introw(char *s, int n, short *par)
/*
  Convert a list of maximum 'n' numbers in 's' into integers 'par'.
  Return the number of integers found. Data must be delimited by space.
  This (fast) procedure uses TC library and is applied in RIAS.
*/
{
 int i=0;
 char *pp,*p;
 pp=s;
 do {
   for (p=pp; *p<=' '; p++) ;
   if (((*p)<'-')||((*p)>'9')) break;
   for ( pp=p; ((*p!=' ')&&(*p)); p++) ;
   par[i++]=atoi(pp);
   if (*p) pp = &(p[1]); else break;
  } while (i<n);
 return(i);
}

int stail(char *s, char c)
{
 int i;
 for (i=strlen(s)-1; (i>0)&&(s[i]==c); i--);
 return(i+1);
}

int shead(char *s, char c)
{
 int i;
 for (i=0; (i<strlen(s))&&(s[i]==c); i++);
 return(i);
}

void undress_string(char *s, char c)
{
 int i=1;
 if (s[0]==c)
   for (i=0; s[i]; i++) s[i]=s[i+1];
 if (s[i-2]==c) s[i-2]=0;
}

/*
int check_arg(int numarg, int least, int most,
               char *cname, char *csyntax, char *cusage)
{
 if ((numarg<least)||(numarg>most)) {
   sprintf(strt,"ERROR: use %s as %s,%s    (%s)\n",cname,csyntax,cname,cusage);
   tmes(-1,strt);
   return(ERROR);
  } else return(SUCCESS);
}
*/

int count_occ(char *s, char c)
{
 int i;
 char *p;
 for (i=0,p=s; (*p); p++) i+=((*p)==c);
 return(i);
}


char *sgets (char *dest, int size, char **src)
/* Sinot kitchen gear. */
{
  char *p;

  *dest = '\0';
  if (!**src) return NULL;
  for (p = dest ; size && **src && **src != '\n'; size--, (*src)++, p++)
	if (**src != '\r') *p = **src;
  if (**src == '\n') (*src)++;
  *p = '\0';
  return dest;
}

void del_occ(char *s, char ch)
{
 int i,k;
 char c,*p;
 for (i=k=0,p=s,c=(*s); c!=0; i++,c=s[i])
   if (c!=ch) *(p++)=c;
}

char *skip_numeric(char *s)
{
 char *p;
 p=s;
 do {
   for ( ; (!isdigit(*p))&&(*p); p++) ;
   for ( ; (isdigit(*p))&&(*p); p++) ;
   if (*p) if (strchr("Ee.",*p)!=NULL) { p++; if ((*p)=='-') p++; }
  } while (isdigit(*p));
 return(p);
}

/* */
