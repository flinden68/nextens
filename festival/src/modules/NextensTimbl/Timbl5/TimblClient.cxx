/*
 * Timbl.cc
 *
 *    A Timbl Client program
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
#include <algorithm>
#include <iostream>
#include <fstream>

#ifdef IRIX64
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#else
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <ctime>
#endif

using namespace std;

#ifdef PTHREADS
#include <map>
#include "Common.h"
#include "MsgClass.h"
#include "LogStream.h"
#include "Types.h"
#include "Options.h"
#include "Tree.h"
#include "IBtree.h"
#include "Instance.h"
#include "MBLClass.h"
#include "GetOptClass.h"
#include "TimblExperiment.h"
#include "ServerProcs.h"
using namespace TimblServer;
using namespace Common;


inline void usage( char *name ){
  cerr << "Timbl Client V0.9.1" << endl
       << "For demonstration purposes only!" << endl
       << "Usage:" << endl
       << name << " NodeName PortNumber [InputFile [OutputFile [BATCH]]]"
       << endl;
}

int main(int argc, char *argv[] ){
  // the following trick makes it possible to parse lines from cin
  // as well from a user supplied file.
  istream *Input = &cin;
  ostream *Output = &cout;
  ifstream input_file;
  ofstream output_file;
  bool c_mode = false;
  if ( argc > 3 ){
    if ( (input_file.open( argv[3], ios::in ), !input_file.good() ) ){
      cerr << argv[0] << " - couldn't open inputfile " << argv[3] << endl;
      exit(1);
    }
    cout << "reading input from: " << argv[3] << endl;
    Input = &input_file;
    if ( argc > 4 ){
      if ( (output_file.open( argv[4], ios::out ), !output_file.good() ) ){
	cerr << argv[0] << " - couldn't open outputfile " << argv[4] << endl;
	exit(1);
      }
      cout << "writing output to: " << argv[4] << endl;
      Output = &output_file;
      if ( argc > 5 )
	c_mode = compare_nocase_n( "BATCH", argv[5] );
    }
  }
  else if ( argc < 3 ){
    usage( argv[0] );
    exit(1);
  }
  RunClient( *Input, *Output, argv[1], argv[2], c_mode );
  exit(0);
}

#else

int main(int argc, char *argv[] ){
  (void)argc;
  (void)argv;
  cerr << "No Client software is build." << endl;
}

#endif // PTHREADS

