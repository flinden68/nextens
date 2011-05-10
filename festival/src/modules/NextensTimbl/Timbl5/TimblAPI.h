#ifndef TIMBLAPI_H
#define TIMBLAPI_H

/*
 * TimblAPI.h
 *
 * defines all "external" functions of Timbl
 * this is the only safe way to use Timbl functionality in external
 * programs
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
#include <vector>
#include "Common.h"
#include "MsgClass.h"
#include "Types.h"
#include "GetOptClass.h"
#include "Tree.h"
#include "Instance.h"

class TimblExperiment;
class Distribution;

using Timbl::CL_Options;
using Timbl::ClassDistribution;
using Timbl::TargetValue;

enum Algorithm { UNKNOWN_ALG, IB1, IB2, IGTREE, TRIBL, TRIBL2, LOO, CV };
enum Weighting { UNKNOWN_W, UD, NW, GR, IG, X2, SV };

class TimblOpts {
  friend class TimblAPI;
  friend std::ostream& operator<<( std::ostream&, const TimblOpts&  );
 public:
  TimblOpts( const int, const char * const * );
  TimblOpts( const std::string& );
  ~TimblOpts();
  bool Find( char, std::string&, bool& );
  void Add( char, const std::string&, bool );
  bool Delete( char );
 private:
  CL_Options *pimpl;
  TimblOpts( const TimblOpts& );
  TimblOpts& operator=( const TimblOpts& );
};

class TimblAPI {
 public:
  TimblAPI( const TimblOpts *, const std::string& = "" );
  TimblAPI( const std::string&,  const std::string& = "" );
  ~TimblAPI();
  bool Valid() const;
  bool StartServer( const int, const int=10 );
  bool Prepare( const std::string& = "" );
  bool Learn( const std::string& = "" );
  bool Increment( const std::string& );
  bool Decrement( const std::string& );
  bool Expand( const std::string& );
  bool Remove( const std::string& );
  bool Test( const std::string& = "", 
	     const std::string& = "",
	     const std::string& = "" );
  const TargetValue *Classify( const std::string& );
  const TargetValue *Classify( const std::string&, const ClassDistribution *& );
  const TargetValue *Classify( const std::string&, double& );
  const TargetValue *Classify( const std::string&, 
			       const ClassDistribution *&, 
			       double& );
  bool Classify( const std::string&, std::string& );
  bool Classify( const std::string&, std::string&, double& );
  bool Classify( const std::string&, std::string&, 
		       std::string&, double& );
  bool ShowBestNeighbors( std::ostream&, bool ) const;
  std::string ExpName() const;
  static  std::string VersionInfo( bool = false );
  bool SaveWeights( const std::string& = "" );
  bool GetWeights( const std::string& = "", Weighting = UNKNOWN_W  );
  Weighting CurrentWeighting() const;
  Weighting GetCurrentWeights( std::vector<double>& ) const;
  bool WriteInstanceBase( const std::string& = "" );
  bool GetInstanceBase( const std::string& = "" );
  bool WriteArrays( const std::string& = "" );
  bool GetArrays( const std::string& = "" );
  bool WriteNamesFile( const std::string& = "" );
  bool ShowWeights( std::ostream& ) const;
  bool ShowOptions( std::ostream& ) const;
  bool ShowSettings( std::ostream& ) const;
  bool SetOptions( const std::string& );
  bool SetIndirectOptions( const TimblOpts&  );
  bool Set_Single_Threaded();
  Algorithm Algo() const;
  static int Default_Max_Feats();

 private:
  TimblAPI( const TimblAPI& );             // these are private
  TimblAPI& operator=( const TimblAPI& ); // so nobody may use them
  TimblExperiment *pimpl;
  bool i_am_fine;
}; 

const std::string to_string( const Algorithm );
const std::string to_string( const Weighting );
bool string_to( const std::string&, Algorithm& );
bool string_to( const std::string&, Weighting& );
int stoi( const std::string&  );
std::string itos( int );
double stod( const std::string&  );
std::string dtos( double );
int split( const std::string&, std::vector<std::string>& );
int split_at( const std::string&, std::vector<std::string>&, 
	      const std::string& );
int split_at_first_of( const std::string&, std::vector<std::string>&, 
		       const std::string& );

#endif // TIMBLAPI_H
