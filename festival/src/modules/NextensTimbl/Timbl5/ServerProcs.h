#ifndef SERVERPROCS_H
#define SERVERPROCS_H
/*
 * ServerProcs.h
 *
 *    Server part of Timbl
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

namespace TimblServer {
  void RunServer( TimblExperiment *Mother, int TCP_PORT );
  void RunClient( std::istream&, std::ostream&, 
		  const std::string&, const std::string&, bool );
}
#endif
