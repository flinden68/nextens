#ifndef LOGGING_H
#define LOGGING_H

#include "LogStream.h"

extern LogStream default_log;
extern LogStream *cur_log;

#define LOG (*Log(cur_log))
#define DBG (*Dbg(cur_log))
#define xDBG (*xDbg(cur_log))
#define xxDBG (*xxDbg(cur_log))

extern LogLevel Tagger_Log_Level;

#endif // LOGGING_H
