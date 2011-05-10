/*
 * Common.h
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

#ifndef COMMON_H
#define COMMON_H

#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#ifdef IRIX64
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#else
#include <cctype>
#include <cerrno>
#include <cmath>
#include <cfloat>
#include <cstdlib>
#include <cstring>
#include <cassert>
#endif

namespace Common {
  const double Epsilon = DBL_EPSILON;   // smallest x so that 1+x != 1
  const int DEFAULT_MAX_FEATS = 2500;   // default maximun number of Features
  
  extern const int Version;             // current Version
  extern const int Revision;            // current Revision
  extern const char *RevCmnt;           // current Patch info
  
  std::string VersionInfo( bool = false );
  void ShowVersionInfo( std::ostream& );

  std::string compress( const std::string& );
  int split_at( const std::string&, std::vector<std::string>&, 
		const std::string& );
  int split_at_first_of( const std::string&, std::vector<std::string>&, 
			 const std::string& );
  inline int split( const std::string& s, std::vector<std::string>& vec ){
    return split_at_first_of( s, vec, " \r\t" );
  }
  std::string string_tok( const std::string&, 
			  std::string::size_type&,
			  const std::string& );
  bool compare_nocase( const std::string&, const std::string& );
  bool compare_nocase_n( const std::string&, const std::string& );

  std::string StrToCode( const std::string& );
  std::string CodeToStr( const std::string& );

  void lowercase( std::string& );
  void uppercase( std::string& );

  inline char look_ahead( std::istream &is ){
    char ch;
    while( isspace((ch=is.peek()))) is.get(ch);
    return ch;
  }
  
  inline void skip_spaces( std::istream &is ){
    char ch;
    while( isspace((ch=is.peek()))) is.get(ch);
  }
  
  inline double Log2(double number){
    // LOG base 2.
    if (number == 0.0)
      return(0.0);
    return(log(number) / log(2.0));
  }
  
  bool string2double( const std::string& , double& );

  inline bool string2long( const std::string& str, long& result ){
    const char *s = str.c_str();
    char *p = NULL;
    errno = 0;
    result = strtol( s, &p, 10 );
    return p != s && errno != ERANGE;
  }
  
  int stoi( const std::string& );
  const std::string itos( int );
  const std::string ltos( long int );
  double stod( const std::string& );
  const std::string dtos( double );
  
}
#endif
