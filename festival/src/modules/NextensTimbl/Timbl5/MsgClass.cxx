/*
 * MsgClass.cc
 *
 *    Helper Class to provide messaging.
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

#include <iostream>
#include <string>
#include "MsgClass.h"

using std::cerr;
using std::endl;
using std::string;

namespace Messages {
  
  void MsgClass::Info( const string& out_line ){
    cerr << out_line << endl;
  }

  void MsgClass::Warning( const string& out_line ){
    cerr << "Warning:" << out_line << endl;
  }

  void MsgClass::Error( const string& out_line ){
    cerr << "Error:" << out_line << endl;
  }

  void MsgClass::FatalError( const string& out_line ){
    cerr << "Fatal Timbl Error:"
	 << out_line << endl
	 << "Please send a bugreport to Timbl@uvt.nl" << endl
	 << "include enough information, like:" << endl
	 << "- Type of computer, type and version of OS, "
	 << "and type and version of the compiler" << endl
	 << "- Which Commands and switches were used" << endl
	 << "- Which input was used, and which output was produced" << endl;
    exit(0);
  }
  
}
