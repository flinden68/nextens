/*
 * Types.cc
 *
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

#include <vector>
#ifdef IRIX64
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#else
#include <cstdlib>
#include <cstdio>
#include <cstring>
#endif

#include "Common.h"
#include "Types.h"

namespace Timbl {
  using namespace Common;
  using std::string;
  using std::vector;

// initializers

  const char *BoolName[][2] = { { "-", "False" },
				 { "+", "True" } };
  
  const char *AlgName[][2] = { { "Unknown", "Unknown Algorithm" },
			       { "IB1", "Memory Based Learning" },
			       { "IB2", "Adapted Memory Based Learning" },
			       { "IGTree", "Information Gain Tree" },
			       { "TRIBL", "Tree IB1" },
			       { "TRIBL2", "Tribl 2" },
			       { "LOO", "Leave One Out" },
			       { "CV", "Cross Validate" } };
  
  const char *MetricName[][2] = { { "U", "Unknown Metric" },
				  { "NeverUseThis", "Default" },
				  { "I", "Ignore" },
				  { "N", "Numeric" },
				  { "D", "Dot product" },
				  { "O", "Overlap" }, 
				  { "M", "Value Difference" },
				  { "J", "Jeffrey Divergence" } };
  
  const char *WeightName[][2] = { { "un", "Unknown Weighting" },
				  { "nw", "No Weighting" },
				  { "gr", "GainRatio" },
				  { "ig", "InfoGain" },
				  { "x2", "Chi-square" }, 
				  { "sv", "Shared Variance" }, 
				  { "ud", "User Defined"} };
  
  const char *DecayName[][2] = { { "Unknown", "Unknown Decay" },
				 { "Z", "Zero Decay" },
				 { "ID", "Inverse Distance" },
				 { "IL", "Inverse Linear Distance" },
				 { "ED", "Exponential Decay" } };
  
  const char *SmoothingName[][2] = { { "Unknown", "Unknown Smoothing" },
				     { "Default", "Default Smoothing" },
				     { "L", "Lidstone Smoothing" } };
  
  const char *OrderName[][2] = { { "Unknown", "Unknown Ordering" },
				 { "UDO", "Data File Ordering" },
				 { "DO", "Default Ordering" },
				 { "GRO", "GainRatio" },
				 { "IGO", "InformationGain" },
				 { "1/V", "Inverse Values" },
				 { "1/S", "Inverse SplitInfo" },
				 { "G/V", "GainRatio/Values" },
				 { "I/V", "InformationGain/Values" },
				 { "GxE", "GainRatio*Entropy" },
				 { "IxE", "InformationGain*Entropy" },
				 { "X2O", "Chi-Squared" },
				 { "SVO", "Shared Variance" },
				 { "X/V", "Chi-Squared/Values" },
				 { "S/V", "Shared Variance/Values" } };
  
  const char *IFName[][2] = { { "Unknown", "Unknown Input Format" }, 
			      { "Compact", "Compact" },
			      { "C45", "C4.5" },
			      { "Column", "Columns" },
			      { "ARFF", "ARFF" },
			      { "BINARY", "Sparse Binary" },
			      { "SPARSE", "Sparse" } };
  
  const char *VerbStrings[][2] = { { "Unknown", "erronous" },
				   { "S", "Silent" },
				   { "O", "Options" },
				   { "F", "Feature_Statistics" },
				   { "P", "Probability_arrays" },
				   { "E", "Exact_match" },
				   { "DI", "Distances" },
				   { "DB", "Distribution" },
				   { "N", "Nearest_Neighbours" },
				   { "AS", "Advanced_Statistics" },
				   { "CM", "Confusion_Matrix" },
				   { "CS", "Class_Statistics" },
				   { "CD", "Client_Debug" },
				   { "K", "All_K_values" } };
  
  bool string_to( const string& s, bool &b ){
    bool result = false;
    if ( compare_nocase( s, "TRUE" ) ||
	 s == "+" || s == "1" ){
      b = true;
      result = true;
    }
    else if ( compare_nocase( s, "FALSE" ) ||
	      s == "-" || s =="0" ){
      b = false;
      result = true;
    }
    return result;
  }
  
  bool string_to( const string& line, int &answer, int low, int upp ){
    long tmp;
    if ( string2long( line, tmp ) &&
	 (tmp >= low) && (tmp <= upp) ){
      answer = tmp;
      return true;
    }
    return false;
  }
  
  bool string_to( const string& line, double &answer, double low, double upp ){
    double tmp;
    if ( string2double( line, tmp ) &&
	 (tmp >= low) && (tmp <= upp) ){
      answer = tmp;
      return true;
    }
    return false;
  }
  
  bool string_to( const string& line, AlgorithmType &a ){
    a = Unknown_a;
    if ( isdigit(line[0]) ){
      switch ( line[0] ){
      case '0':
	a = IB1_a;
	break;
      case '1':
	a = IGTREE_a;
	break;
      case '2':
	a = TRIBL_a;
	break;
      case '3':
	a = IB2_a;
	break;
      case '4':
	a = TRIBL2_a;
	break;
      default:
	return false;
      }
      return true;
    }
    else 
      for ( ++a; a < Max_a; ++a )
	if ( compare_nocase( line, AlgName[a][0] ) ||
	     compare_nocase( line, AlgName[a][1] ) ){
	  return true;
	}
    return false;
  }
  
  bool string_to( const string& line, DecayType &d ){
    d = UnknownDecay;
    for ( ++d; d < MaxDecay; ++d )
      if ( compare_nocase( line, DecayName[d][0] ) ||
	   compare_nocase( line, DecayName[d][1] ) ){
	return true;
      }
    return false;
  }

  bool string_to( const string& line, SmoothingType &d ){
    d = UnknownSmoothing;
    for ( ++d; d < MaxSmoothing; ++d )
      if ( compare_nocase( line, SmoothingName[d][0] ) ||
	   compare_nocase( line, SmoothingName[d][1] ) ){
	return true;
      }
    return false;
  }

  bool string_to( const string& line, MetricType &i ){
    i=UnknownMetric;
    for ( ++i; i < MaxMetric; ++i )
      if ( compare_nocase( line, MetricName[i][0] ) ||
	   compare_nocase( line, MetricName[i][1] ) ){
	return true;
      }
    return false;
  }
  
  bool string_to( const string& line, WeightType &w ){
    w = Unknown_w;
    if ( line.length() == 1 && isdigit(line[0]) ){
      switch ( line[0] ){
      case '0':
	w = No_w;
	break;
      case '1':
	w = GR_w;
	break;
      case '2':
	w = IG_w;
	break;
      case '3':
	w = X2_w;
	break;
      case '4':
	w = SV_w;
	break;
      default:
	return false;
	break;
      }
      return true;
    }
    else
      for ( ++w; w < Max_w; ++w ){
	if ( compare_nocase( line, WeightName[w][0] ) ||
	     compare_nocase( line, WeightName[w][1] ) ){
	  return true;
	}
      }
    return false;
  }
  
  bool string_to( const string& line, InputFormatType &a ){
    a = UnknownInputFormat;
    for ( ++a ;a != MaxInputFormat; ++a )
      if ( compare_nocase( line, IFName[a][0] ) ||
	   compare_nocase( line, IFName[a][1] ) ){
	return true;
      }
    return false;
  }

  bool string_to( const string& line, OrdeningType &i ){
    i = UnknownOrdening;
    for ( ++i; i< MaxOrdening; ++i )
      if ( compare_nocase( line, OrderName[i][0] ) ||
	   compare_nocase( line, OrderName[i][1] ) ){
	return true;
      }
    return false;
  }

  inline bool string_to_verbflag( const string& line, VerbosityFlags &a ){
    unsigned int i;
    for ( i=0; i< (sizeof VerbStrings)/(2*sizeof(char*)); i++ )
      if ( compare_nocase( line, VerbStrings[i][0] ) ||
	   compare_nocase( line, VerbStrings[i][1] ) ){
	if ( i==0 ){	
	  a = NO_VERB;
	}
	else{
	  a = (VerbosityFlags)(1<<(i-1));
	}
	return true;
      }
    return false;
  }
  
  bool string_to( const string& line, VerbosityFlags &V ){
    VerbosityFlags Flag;
    vector<string> tmp;
    int cnt = split_at( line, tmp, "+" );
    V = NO_VERB;
    for ( int i=0; i < cnt; ++i ){
      if ( string_to_verbflag( tmp[i], Flag ) ){
	V |= Flag;
      }
      else {
	return false ;
      }
    }
    return true;
  }

  string to_string( int num, bool ){
    char buf[256];
    sprintf( buf, "%d", num );
    return buf;
  }
  
  string to_string( unsigned int num, bool ){
    char buf[256];
    sprintf( buf, "%u", num );
    return buf;
  }
  
  string to_string( double real, bool ){
    char buf[256];
    sprintf( buf, "%f", real );
    return buf;
  }
  
  const string to_string( bool b, bool full ){
    return BoolName[b][(full?1:0)];
  }
  
  const string to_string( WeightType W, bool full ){
    return WeightName[W][(full?1:0)];
  }
  
  const string to_string( AlgorithmType A, bool full ){
    return AlgName[A][(full?1:0)];
  }
  
  const string to_string( DecayType A, bool full ){
    return DecayName[A][(full?1:0)];
  }
  
  const string to_string( SmoothingType A, bool full ){
    return SmoothingName[A][(full?1:0)];
  }
  
  const string to_string( MetricType M, bool full ){
    return MetricName[M][(full?1:0)];
  }
  
  const string to_string( OrdeningType O, bool full ){
    return OrderName[O][(full?1:0)];
  }
  
  const string to_string( InputFormatType I, bool full ){
    return IFName[I][(full?1:0)];
  }
  
  inline string verbosity_to_string( int v, bool full ){
    if ( v == 0 )
      return VerbStrings[0][(full?1:0)];
    else {
      string OutLine;
      unsigned int i;
      bool first = true;
      for ( i=1; i< (sizeof VerbStrings)/(2*sizeof(char*)); i++ )
	if ( v & (1<<(i-1)) ){
	  if (first)
	    first = false;
	  else 
	    OutLine += '+';
	  OutLine += VerbStrings[i][(full?1:0)];
	}
      return OutLine;
    }
  }
  
  const string to_string( VerbosityFlags v, bool full ){
    return verbosity_to_string( (int)v, full );
  }
  
}
