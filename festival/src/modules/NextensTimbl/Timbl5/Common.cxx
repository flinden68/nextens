/*
 * Common.cc
 *
 * general usefull (inlined) functions
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

#include <algorithm>
#include <cstdio>
#include <string>
#ifdef IRIX64
#include <ctype.h>
#include <stdlib.h>
#else
#include <cctype>
#include <cstdlib>
#endif
#if __GNUC__ < 3
#include <strstream>
#else
#include <sstream>
#endif
#include "Common.h"

using namespace std;

namespace Common {
  const int Version  = 5;               // current Version
  const int Revision = 2;               // current Revision
  const char *RevCmnt = "(RC1)";           // comment on Revision

  void ShowVersionInfo( std::ostream& os, bool full ){
    os << Version << "." << Revision << RevCmnt;
    if ( full )
      os << ", compiled on " << __DATE__ << ", " << __TIME__;
  }
  
  string VersionInfo( bool full ){
    string result;
#if __GNUC__ < 3
    ostrstream oss;
#else
    ostringstream oss;
#endif
    ShowVersionInfo( oss, full );
#if __GNUC__ < 3
    oss << ends;
    char *buf = oss.str();
    result = buf;
    delete [] buf;
#else
    result = oss.str();
#endif
    return result; 
  }

  string StrToCode( const string &In ){
    string Out;
    string::const_iterator it = In.begin();
    while ( it != In.end() ){
      switch ( *it ){
      case ' ':
	Out += '\\';
	Out += '_';
	break;
      case '\t':
	Out += '\\';
	Out += 't';
	break;
      case '\\':
	Out += '\\';
	Out += '\\';
	break;
      default:
	Out += *it;
      }
      ++it;
    }
    return Out;
  }
  
  string CodeToStr( const string& in ){
    string out;
    string::const_iterator it = in.begin();
    while ( it != in.end() ){
      if ( *it == '\\' ){
	switch ( *++it ){
	case  '_':
	  out += ' ';
	  break;
	case '\\':
	  out += '\\';
	  break;
	case 't':
	  out += '\t';
	  break;
	default:
	  cerr << "Invalid value '" << *it << "' in switch (" 
	       << __FILE__ << "," << __LINE__ << ")" << endl;
	  cerr << "Input was '" << in << "'" << endl;
	  cerr << "ABORTING now" << endl;
	  abort();
	}
	++it;
      }
      else
	out += *it++;
    }
    return out;
  }
  
  int to_lower( const int& i ){ return tolower(i); }
  int to_upper( const int& i ){ return toupper(i); }

  void lowercase( string& s ){
    transform( s.begin(), s.end(), s.begin(), to_lower );
  }
  
  void uppercase( string& s ){
    transform( s.begin(), s.end(), s.begin(), to_upper );
  }

  string compress( const string& s ){
    // remove leading and trailing spaces from a string
    string::const_iterator b_it = s.begin();
    while ( b_it != s.end() && isspace( *b_it ) ) ++b_it;
    string::const_iterator e_it = s.end();
    --e_it;
    while ( e_it != s.begin() && isspace( *e_it ) ) --e_it;
    if ( b_it > e_it )
      return "";
    else
      return string( b_it, e_it+1 );
  }
  
  int split_at( const string& src, vector<string>& results, 
		const string& sep ){
    // split a string into substrings, using seps as seperator
    // silently skip empty entries (e.g. when two or more seperators co-incide)
    results.clear();
    string::size_type pos = 0, p;
    string res;
    while ( pos != string::npos ){
      p = src.find( sep, pos );
      if ( p == string::npos ){
	res = src.substr( pos );
	pos = p;
      }
      else {
	res = src.substr( pos, p - pos );
	pos = p + sep.length();
      }
      if ( !res.empty() )
	results.push_back( res );
    }
    return results.size();
  }
  
  int split_at_first_of( const string& src, vector<string>& results, 
			 const string& seps ){
    // split a string into substrings, using the characters in seps
    // as seperators
    // silently skip empty entries (e.g. when two or more seperators co-incide)
    results.clear();
    string::size_type e, s = src.find_first_not_of( seps );
    string res;
    while ( s != string::npos ){
      e = src.find_first_of( seps, s );
      if ( e == string::npos ){
	res = src.substr( s );
	s = e;
      }
      else {
	res = src.substr( s, e - s );
	s = src.find_first_not_of( seps, e );
      }
      if ( !res.empty() )
	results.push_back( res );
    }
    return results.size();
  }

  string string_tok( const string& s, 
		     string::size_type& pos,
		     const string& seps ){
    string::size_type b_pos = s.find_first_not_of( seps, pos ); 
    if ( b_pos != string::npos ){
      pos = s.find_first_of( seps, b_pos ); 
      if ( pos == string::npos )
	return string( s, b_pos );
      else
	return string( s, b_pos, pos - b_pos );
    }
    else {
      pos = string::npos;
    }
    return "";
  }

  bool nocase_cmp( char c1, char c2 ){
    return toupper(c1) == toupper(c2);
  }
  
  bool compare_nocase( const string& s1, const string& s2 ){
    if ( s1.size() == s2.size() &&
	 equal( s1.begin(), s1.end(), s2.begin(), nocase_cmp ) )
      return true;
    else
      return false;
  }
  
  bool compare_nocase_n( const string& s1, const string& s2 ){
    if ( s1.size() <= s2.size() &&
	 equal( s1.begin(), s1.end(), s2.begin(), nocase_cmp ) )
      return true;
    else
      return false;
  }
  
  int stoi( const string& s ){
    int result = INT_MIN;
    if ( !s.empty() ){
      errno = 0;
      long l_result = strtol( s.c_str(), NULL, 10 );
      if ( errno != ERANGE ){
	result = (int)l_result;
      }
    }
    return result;
  }

  const string itos( int i ){
    char tmp[30];
    sprintf( tmp, "%d", i );
    return tmp;
  }

  const string ltos( long int i ){
    char tmp[50];
    sprintf( tmp, "%li", i );
    return tmp;
  }

  double stod( const string& s ){
    double result = DBL_MIN;
    if ( !s.empty() ){
      errno = 0;
      char *p = NULL;
      double l_result = strtod( s.c_str(), &p );
      if ( p != s && errno != ERANGE ){
	result = l_result;
      }
    }
    return result;
  }

  const string dtos( double d ){
    char tmp[30];
    sprintf( tmp, "%g", d );
    return tmp;
  }

  bool string2double( const std::string& str, double &result ){
    const char*s = str.c_str();
    while ( isspace(*s) ) ++s;
    if ( isalpha(*s) ) // NaN for instance
      return false;
    result = stod( str );
    return result != DBL_MIN;
  }

}
