/*
 * SocketBasics.cxx
 *
 *    Socket interface of Timbl
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

#include <string>
#include <iostream>

#ifdef IRIX64
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#else
#include <cstdio>
#include <cstdlib>
#include <cerrno>
#include <climits>
#endif

#include "Common.h"
#include "SocketBasics.h"

using namespace std;

namespace SocketProcs {
#ifndef PTHREADS
  // define stubs
  int atoport( const char *, const char * ){
    cerr << "No Socket operations available." << endl;
    return -1;
  }
  int make_connection( const string&, const string&, int ){
    cerr << "No Socket operations available." << endl;
    return -1;
  }
  bool read_line( int, string&, int){
    cerr << "No Socket operations available." << endl;
    return false;
  }
  bool write_line( int, const string& ){
    cerr << "No Socket operations available." << endl;
    return false;
  }
  
#else
/* Take a service name, and a service type, and return a port number.  If the
   service name is not found, it tries it as a decimal number.  The number
   returned is byte ordered for the network. */
int atoport( const char *service, const char *proto){
  int port;
  long int lport;
  struct servent *serv;
  char *errpos;

  /* First try to read it from /etc/services */
  serv = getservbyname(service, proto);
  if (serv != NULL)
    port = serv->s_port;
  else { /* Not in services, maybe a number? */
    lport = strtol(service,&errpos,0);
    if ( (errpos[0] != 0) || (lport < 1) || (lport > 65535) )
      return -1; /* Invalid port address */
    port = htons(lport);
  }
  return port;
}

/* Converts ascii text to in_addr struct.  NULL is returned if the address
   can not be found. */
struct in_addr *atoaddr( const char *address){
  struct hostent *host;
  static struct in_addr saddr;

  /* First try it as aaa.bbb.ccc.ddd. */
  saddr.s_addr = inet_addr(address);
  if (saddr.s_addr != (unsigned int)-1) {
    return &saddr;
  }
  host = gethostbyname(address);
  if (host != NULL) {
    return (struct in_addr *) *host->h_addr_list;
  }
  return NULL;
}

/* This is a generic function to make a connection to a given server/port.
   service is the port name/number,
   type is either SOCK_STREAM or SOCK_DGRAM, and
   netaddress is the host name to connect to.
   The function returns the socket, ready for action.*/
int make_connection( const string& service, 
		     const string& netaddress,
		     int type ){
  /* First convert service from a string, to a number... */
  int port = -1;
  struct in_addr *addr;
  int sock, connected;
  struct sockaddr_in address;

  if (type == SOCK_STREAM) 
    port = atoport(service.c_str(), "tcp");
  if (type == SOCK_DGRAM)
    port = atoport(service.c_str(), "udp");
  if (port == -1) {
    fprintf(stderr,"make_connection:  Invalid socket type.\n");
    return -1;
  }
  addr = atoaddr(netaddress.c_str());
  if (addr == NULL) {
    fprintf(stderr,"make_connection:  Invalid network address.\n");
    return -1;
  }
 
  memset((char *) &address, 0, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_port = (port);
  address.sin_addr.s_addr = addr->s_addr;

  sock = socket(AF_INET, type, 0);
  int val = 1;
  setsockopt( sock, SOL_SOCKET, SO_REUSEADDR, (void *)&val, sizeof(val) );
  val = 1;
  setsockopt( sock, IPPROTO_TCP, TCP_NODELAY, (void *)&val, sizeof(val) );

  if (type == SOCK_STREAM) {
    connected = connect(sock, (struct sockaddr *) &address, 
      sizeof(address));
    if (connected < 0) {
      printf( "Failed connecting to %s on port %d.\n",
	      inet_ntoa(*addr),htons(port));
      perror("connect");
      return -1;
    }
    return sock;
  }
  /* Otherwise, must be for udp, so bind to address. */
  if (bind(sock, (struct sockaddr *) &address, sizeof(address)) < 0) {
    perror("bind");
    return -1;
  }
  return sock;
}

//     This function reads from a socket, until it recieves a linefeed
//     character.  It fills the buffer "str" up to the maximum size "count".
//     This function will return -1 if the socket is closed during the read
//     operation.
//     Note that if a single line exceeds the length of count, the extra data
//     will be read and discarded!  You have been warned. 
int sock_read(int sockfd,char *str,size_t count){
  int total_count = 0;
  char last_read = 0;
  char *current_position = str;
  while (last_read != 10) {
    int bytes_read = read( sockfd, &last_read, 1 );
    if (bytes_read <= 0) {
      // The other side may have closed unexpectedly 
      return -1; 
    }
    if ( (total_count < (signed) count) && 
	 (last_read != 10) && (last_read !=13) ) {
      *current_position++ = last_read;
      total_count++;
    }
  }
  if (count > 0)
    *current_position = 0;
  return total_count;
}

int sock_write( int sockfd, const char *str ){
  // This is just like the write() system call, accept that it will
  // make sure that all data is transmitted. 
  // return -1 if the connection is closed while it is trying to write.
  size_t bytes_sent = 0;
  int this_write;
  unsigned int count = strlen( str );
  while (bytes_sent < count) {
    do {
      this_write = write(sockfd, str, count - bytes_sent);
    } while ( (this_write < 0) && (errno == EINTR) );
    if (this_write <= 0)
      return this_write;
    bytes_sent += this_write;
    str += this_write;
  }
  return count;
}

  bool read_line( int socknum, string& line, int Size ){
    char *tmp = new char[Size];
    line = "";
    if ( sock_read( socknum, tmp, Size ) < 0 ) {
      delete [] tmp;
      return false;
    }
    else {
      line = tmp;
      delete [] tmp;
      return true;
    }
  }

  bool write_line( int socknum, const string& line ){
    // write a line to the socket
    if ( !line.empty() )
      if ( sock_write( socknum, line.c_str() ) < 0 ) {
	return false;
      }
    return true;
  }

#endif // PTHREADS
}
