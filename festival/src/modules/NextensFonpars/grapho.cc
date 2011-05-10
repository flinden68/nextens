extern char rulefile_g0apho[];
extern char rulefile_g1apho[];

#include <stdio.h>

extern FILE *gtrace;


char *rulefile_grapho;

static char *rulefils_grapho[3] = {  rulefile_g0apho, rulefile_g1apho,0L };

void prep_grapho_access(void)
{
// prep_g0apho_access();
// prep_g1apho_access();
}

void fono_grapho_0_(void);
void fono_grapho_1_(void);
void fone_grapho_0_(void);
void fone_grapho_1_(void);



void fono_grapho(void)
{
 rulefile_grapho = rulefils_grapho[0];
 if (gtrace!=NULL) fprintf(gtrace,"\ngrapho %s\n",rulefile_grapho);
 fono_grapho_0_();
 rulefile_grapho = rulefils_grapho[1];
 if (gtrace!=NULL) fprintf(gtrace,"\ngrapho %s\n",rulefile_grapho);
 fono_grapho_1_();
}

void fone_grapho(void)
{
 rulefile_grapho = rulefils_grapho[0];
 fone_grapho_0_();
 rulefile_grapho = rulefils_grapho[1];
 fone_grapho_1_();
}

