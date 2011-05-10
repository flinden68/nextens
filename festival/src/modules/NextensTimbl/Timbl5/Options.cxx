/*
 * Options.cc
 *
 *    Parsing and checking of commandline options.
 *    Also assigns defaults to RunTime Options.
 *
 * Copyright (c) 1998 - 2003
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
#include <cctype>
#include <cstring>
#include <cstdlib>

#include "Types.h"
#include "Common.h"
#include "Options.h"

using namespace std;

namespace Timbl {
  using namespace Common;

  bool MetricArrayOption::set_option( const string& line ){ 
    MetricType m;
    int i;
    string tmp[2];
    bool result = split( line, tmp, 2, "=" ) == 2 &&
      string_to( tmp[1], m ) && 
      string_to( tmp[0], i, 0, Size );
    if ( result ) 
      TA[i] = m;
    return result;
  }
  
  ostream& MetricArrayOption::show_opt( ostream &os ){
    os.width(20);
    os.setf( ios::left, ios::adjustfield );
    os << Name << " : ";
    int i;
    for ( i=0; i < Size; i++ )
      if ( TA[i] != DefaultMetric )
	os << i << ":" << to_string(TA[i]) << ", ";
    return os;
  }
  
  ostream& MetricArrayOption::show_full( ostream &os ){
    os.width(20);
    os.setf( ios::left, ios::adjustfield );
    os << Name << " : comma separated metricvalues, [";
    bool frst = true;
    int i;
    for ( i=0; i < Size; i++ ){
      if ( TA[i] != DefaultMetric ){
	if ( !frst )
	  os << ",";
	else
	  frst = false;
	os << i << ":" << to_string(TA[i]);
      }
    }
    os << "]";
    return os;
  }
  
  inline void split_line( const string& line, string& name, string& value ){
    string results[2];
    int i = split( line, results, 2, ":" );
    switch (i){
    case 2:
      name = compress(results[0]);
    case 1:
      value = compress(results[1]);
    default:
      break;
    }
  }
  
  OptionTableClass::~OptionTableClass(void){
    int i;
    for ( i=0; i < table_size; i++ )
      delete Table[i];
    delete [] Table;
  }
  
  OptionClass *OptionTableClass::lookup( const string& option_name, 
					 bool &runtime ){
    int i;
    for ( i=0; i < table_size; i++ )
      if ( compare_nocase( option_name, Table[i]->Name ) ){
	runtime = (i >= table_start || !table_frozen );
	return Table[i];
      }
    return NULL;
  }
  
  SetOptRes OptionTableClass::SetOption( const string& line ){ 
    SetOptRes result = Opt_OK;
    bool runtime = false;
    string option_name;
    string value;
    split_line( line, option_name, value );
    OptionClass *option = lookup( option_name, runtime );
    if ( option ){
      if ( !runtime )
	result = Opt_Frozen; // may not be changed at this stage
      else
	if ( !option->set_option( value ) )
	  result = Opt_Ill_Val; // illegal value
    }
    else 
      result = Opt_Unknown; // What the hell ???
    return result;
  }
  
  void OptionTableClass::ShowSettings( ostream& os ) const{
    for ( int i=0; i <table_size; i++)
      Table[i]->show_opt( os ) << endl;
  }
  
  void OptionTableClass::ShowOptions( ostream& os ) const {
    for ( int i=0; i <table_size; i++)
      Table[i]->show_full( os ) << endl;
  }

}
