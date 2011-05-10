extern char rulefile_p0ldp[];

#include <stdio.h>

extern FILE *gtrace;


char *rulefile_ppldp;

static char *rulefils_ppldp[2] = {  rulefile_p0ldp,0L };

void prep_ppldp_access(void)
{
// prep_p0ldp_access();
}

void fono_ppldp_0_(void);
void fone_ppldp_0_(void);



void fono_ppldp(void)
{
 rulefile_ppldp = rulefils_ppldp[0];
 if (gtrace!=NULL) fprintf(gtrace,"\nppldp %s\n",rulefile_ppldp);
 fono_ppldp_0_();
}

void fone_ppldp(void)
{
 rulefile_ppldp = rulefils_ppldp[0];
 fone_ppldp_0_();
}


