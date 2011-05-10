
/*
 * This file contains a version of the char* pGF(char*, short) method without tracing
 * as found in the gfc.c file from the FONPARS code
 *
 * Implemented for the NEXTENS project by www.plankton.nl
 */

#include <string.h>
#include <stdio.h>

extern short gtraceact;
extern short gstartverw;
extern short gbeginverw;
extern short geindverw;
extern char *ginpregel;
extern short ginplgt;

extern void init_grafon(int);
extern void fono_ppldp(void);
extern void fono_grapho(void);
extern void fono_grapho_0_(void);
extern void fono_grapho_1_(void);


/*
 * Execute language-dependent preprocessing (PPLDTP) and grapheme->phoneme
 * conversion (GRAPHO) rules
 */
char* graphemeToPhoneme(char *graphemes) {
	gtraceact = 0;
 	init_grafon(2048);
	short npos = strlen(graphemes);
 	strcpy(ginpregel,"           . ");
 	gbeginverw=gstartverw=strlen(ginpregel)-2;
 	strncat(ginpregel,graphemes,npos);
 	ginpregel[geindverw=ginplgt=npos+14]=0;
 	strcat(ginpregel," .            ");
 	fono_ppldp();
 	fono_grapho_0_();
 	fono_grapho_1_();
 	ginpregel[ginplgt-1]=0;
 	return(&(ginpregel[13]));
}
