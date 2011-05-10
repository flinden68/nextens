/* TRC.H */

/*** MESSAGE HANDLERS ********************************************************/

typedef void flushtask(void);

extern int traceon;      /* global trace flag (handle elsewhere, no use in TLS)  */

typedef int message_handler(unsigned char *fmt, ...);

extern message_handler *trmes;
extern flushtask       *flmes;

/*
extern message_handler *trcmes;
extern flushtask       *flsmes;
extern message_handler *errmes;
*/

/*
typedef int mess_handler();
#define errmes (mess_handler *)trmes
#define trcmes (mess_handler *)trmes
*/

int link_trcmes(message_handler *p);
int link_errmes(message_handler *p);
int open_trace(char *fname, int keepscr);
int close_trace(void);

/* int trcm(int level, char *fmt, ...); */
int tmes(int,char *);

int gmessnl(char *s);
int link_flsmes(flushtask *flush);

/* */

int nofile_message(char *kind, char *fname);

