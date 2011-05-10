/* STR.H */

char *cspace(char *s);
int find_maxs(int numlist, char **slist);
char *mkstring(char *s,char fillchar, int n);
void strtailcut(char *s, char fillchar);
void strtailadd(char *s, char fillchar, int n);
int split_string(char **tok, char *s, char *spdelims, char *actdelims);
int part_string(char **tok, char *s, char *spdelims);
int tokenize(char *s, char *tok[], char *delims, char *occdelims, int new);
char *getintpars(char *p, short *pvv, char *pcd, char delimiter);
int stail(char *s, char c);
int shead(char *s, char c);
void undress_string(char *s, char c);
int check_arg(int numarg, int least, int most,
	       char *cname, char *csyntax, char *cusage);
int count_occ(char *s, char c);
char *sgets (char *dest, int size, char **src);
int lvprintf(char *fmt, void *b);
int lvsprintf(char *s, char *fmt, void *b);
void del_occ(char *s, char ch);
int convert_introw(char *s, int n, short *par);
char *skip_numeric(char *s);

