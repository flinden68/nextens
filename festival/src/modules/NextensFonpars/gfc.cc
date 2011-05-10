/* GFC.C */

#define STANDALONE

/*--------------------------------------------------------------------------

  FUNCTION: Framework for the following rulesets:
             -> Grapheme to Phoneme conversion and
             -> Language Dependent Preprocessing

            Use:
                   GFC @filename
             or..  GFC utterance


            Result one line phoneme string is written to standard output.
            For PC, create a response file from 'filename' as follows:

                   GFC [@]argument > filename

            where 'filename' is the target phonetic text file
            resulting from arguments.

  AUTHOR:   Lex Elich for KUN Taal & Spraak

---------------------------------------------------------------------------*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// original: void *malloc(unsigned);
// changed to compile on Darwin and Linux - not sure about Windows yet
//#if defined(__APPLE__)
    void *malloc(long unsigned int);
//#else
//  void *malloc(unsigned);
//#endif

void free(void *p);
void exit(int);

extern short gj;
extern short gstartverw;
extern short gbeginverw;
extern short geindverw;
extern short gtraceact;
extern char *ginpregel;
extern short ginplgt;

/*--- PROTOTYPES -------------------------------------------------*/

void gpreparesets(void);
void gtracetask(char *ruleset, int rule_number, int task);

extern FILE *gtrace;

/*--- STRING LENGTH ADJUSTMENTS -------------------------------------------*/

void gcorlengt(short aantal)
{
 int k;
 if (aantal < 0) {
   for (k = gj; k<=(ginplgt+aantal+1); k++) ginpregel[k] = ginpregel[k-aantal];
   for (k = (ginplgt+aantal+1); k<=ginplgt; k++) ginpregel[k] = ' ';
  }
 if (aantal > 0)
   for (k = ginplgt; k>=gj; k--) ginpregel[k+aantal] = ginpregel[k];
 geindverw = geindverw + aantal;
 ginplgt = ginplgt + aantal;
}

/*--- ANY STRING SUBSTITUTION ---------------------------------------------*/

void fono_ppldp(void);
void fono_grapho(void);
void fono_grapho_0_(void);
void fono_grapho_1_(void);

extern char rulefile_g0apho[26];
extern char rulefile_g1apho[26];

char *pGF(char *s1, short npos)
/*
  Execute language-dependent preprocessing (PPLDTP) and grapheme->phoneme
  conversion (GRAPHO) rules
*/
{
 strcpy(ginpregel,"           . ");
 gbeginverw=gstartverw=strlen(ginpregel)-2;
 strncat(ginpregel,s1,npos);
 ginpregel[geindverw=ginplgt=npos+14]=0;
 strcat(ginpregel," .            ");
 fprintf(gtrace,"\nPPLDP\n");
 fono_ppldp();
 fprintf(gtrace,"\n%s\n",ginpregel+8 );
 fprintf(gtrace,"\nGRAPHO\n");
 fprintf(gtrace,"\ngrapho %s\n",rulefile_g0apho);
 fono_grapho_0_();
 fprintf(gtrace,"\n%s\n",ginpregel+8 );
 fprintf(gtrace,"\ngrapho %s\n",rulefile_g1apho);
 fono_grapho_1_();
 fprintf(gtrace,"\n%s\n",ginpregel+8 );
/* printf("lengte: %d >%s<\n",ginplgt,ginpregel); */
 ginpregel[ginplgt-1]=0;
 gtracetask(NULL,1,0);
 return(&(ginpregel[13]));
}

void init_grafon(int nstr)
{
 ginpregel=(char *) malloc(nstr);
 gpreparesets();
}

void close_grafon(void)
{
 free(ginpregel);
}

/*--------------------------------------------------------------------------*/
/*--- STANDALONE GRAPHEME->PHONEME CONVERSION ------------------------------*/

#ifdef STANDALONE

char *pGF_report(char *s1, short npos, char *pgf(char *,short))
{
 int no_accents;
 char *rv,*p,*pp;
 rv=pgf(s1,npos);
 no_accents=(strchr(rv,'`')==NULL);
 for (pp=p=rv; *p; p++) {
   if ( (((*p)=='6')||((*p)=='8'))&&(no_accents)) *p='`';
   if (*p == '{') {
     p[-1]=0;
     printf("%s\n",pp);
     pp=p+2;
    }
  }
 if ((*pp)&&(pp[1]))
  /* printf("%s\n.\n",pp); 12-8-19976 JK */
   printf("%s\n",pp);
   return(rv);
}

/*--- MAIN PROGRAM ----------------------------------------------------------*/

int getch(void);
char *kfgets(char *s1,int maxlen,FILE *f)
{
 return(fgets(s1,maxlen,f));
}

void process_data_arguments(char *p(char *,short,    /* shell performing-    */
			    char *pgf(char *,short)), /* the translation      */
			    char *pgf(char *,short),  /* translation function */
			    int maxlen,              /* max # of characters  */
			    int argc,                /* argument count       */
			    char *argv[])            /* argument strings     */
/*
  Concatenate/space 'argc' input strings in 'argv[1..]', then process them
  using a rule based string translation function 'transrules' (type fono_ or
  fone_), passed to specified string handler function 'string_translation'.
  If the first argument argv[1][0]=='@', assume filename specified. In this
  case, copy lines starting with '.' (dot) , else translate line.
*/
{
 int i,l;
 FILE *f=NULL;
 char *s,s1[256];
 /*char gfrules[24];
   fetch_rulefilename(r,rulecategory,gfrules,0);
   fprintf(tracef,"%s %s\n",rulecategory,gfrules);*/
 s= (char *) malloc(maxlen);
 if (argc==1) f=stdin;                     /* no arguments.. assume terminal */
   else {
     strcat(strcpy(s,argv[1])," ");
     if (s[0]=='@') f=fopen(&(s[1]),"r");
    }
 if (f!=NULL) {
   s[0]=0;
   while (kfgets(s1,256,f)!=NULL) {
    if (*s1 <32) continue;                   /* no information ignore   */
    l=strlen(s1);                            /* measure input           */
    if (s1[l-1]<32) s1[--l]=0;               /* cut trailing LF         */
    while ((l)&&(s1[l]<=' ')) l--;           /* cut trailing space      */
    if (!l) continue;                        /* space line ignore       */
    s1[l+1]=0;                               /* cut string here         */
    if (strchr("?:!.",s1[l])!=NULL) {        /* ending endsent punct ?  */
      s1[l]=0;                               /* for SRD: parse off dot  */
      if (s1[--l]==' ') s1[l]=0;             /* and space..             */
      strcat(s,s1);                          /* concat to previous part */
      p(s,strlen(s),pgf);                    /* and flush it..          */
      s[0]=0;
     } else
	 if (strchr("?:!.",s1[0])!=NULL) {  /* starting endsent?           */
	   p(s,strlen(s),pgf);              /* yes.. assume flush previous */
	   s[0]=0;
	  } else {
#ifdef OLD
          if (s1[1]) strcat(s1," { ");     /* assume line represents domain */
	      strcat(s,s1);                /* concatenate line to sentence  */
#else
          p(s1,strlen(s1),pgf);
#endif
         }
   }
  if (f!=stdin) fclose(f);
 } else for (i=2; i<argc; i++)
	  strcat(strcat(s,argv[i])," ");
 if ((l=strlen(s)-1)>2)
   if (s[l-1]=='{') s[l-2]=0;
 if ((s[0])&&(s1[0]!=10)&&(s1[0]!=13))
   p(s,strlen(s),pgf);
}
/*
main(int argc, char *argv[])
{
 gtraceact = 1;
 gtrace=fopen("trace.gra","w");
 init_grafon(2048);
 process_data_arguments(pGF_report,pGF,2048,argc,argv);
 close_grafon();
 if (gtrace) fclose(gtrace);
 return(0);
}
*/
#endif

