#ifndef SOCKET_BASICS_H
#define SOCKET_BASICS_H
/*
 * SocketBasics.h
 *
 *    Socket utilities of Timbl
 *    uses PThreads and sockets
 *
 * Copyright (c) 1998 - 2006
 * ILK  -  Tilburg University
 * CNTS -  University of Antwerp
 *
 * All rights Reserved.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * For questions and suggestions, see:
 *	http://ilk.uvt.nl/software.html
 * or send mail to:
 *	Timbl@uvt.nl
 */

#ifdef _WIN32
#include <winsock.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <arpa/inet.h>
#endif

#ifdef __osf__
#define socklen_t int
#else
#ifdef __sgi__
#define socklen_t int
#endif
#endif

namespace SocketProcs {
  int make_connection( const std::string&, const std::string&, int );
  
  bool read_line( int, std::string&, int );
  bool write_line( int, const std::string& );
}

#endif
