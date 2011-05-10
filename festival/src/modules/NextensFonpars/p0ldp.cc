char rulefile_p0ldp[25] = "dutch/ppned.dat";

void fono_ppldp_0_1(void);
void fono_ppldp_0_2(void);
void fone_ppldp_0_1(void);
void fone_ppldp_0_2(void);



void fono_ppldp_0_(void)
{
 fono_ppldp_0_1();
 fono_ppldp_0_2();
}

void fone_ppldp_0_(void)
{
 fone_ppldp_0_1();
 fone_ppldp_0_2();
}

void ovl_ppldp0_1(void);
void ovl_ppldp0_2(void);

void prep_p0ldp_access(void)
{
 ovl_ppldp0_1();
 ovl_ppldp0_2();
}
