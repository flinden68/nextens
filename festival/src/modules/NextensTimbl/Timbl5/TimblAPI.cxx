/*
 * TimblAPI.cxx
 *
 * defines all "external" functions of Timbl
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

#include <map>
#include <string>
#include "Common.h"
#include "MsgClass.h"
#include "LogStream.h"
#include "Types.h"
#include "Options.h"
#include "Tree.h"
#include "Instance.h"
#include "MBLClass.h"
#include "GetOptClass.h"
#include "TimblExperiment.h"
#include "TimblAPI.h"

using namespace std;
using namespace Timbl;

TimblOpts::TimblOpts( const int argc, const char * const *argv ):
  pimpl( new CL_Options( argc, argv ) )
{}

TimblOpts::TimblOpts( const string& args ):
  pimpl( new CL_Options( args ) )
{}

TimblOpts::~TimblOpts(){
  delete pimpl;
}

ostream& operator<<( ostream& os, const TimblOpts& opts ){
  os << *opts.pimpl;
  return os;
}

bool TimblOpts::Find( char c, string& opt, bool& mood ){
  return pimpl->Find( c, opt, mood );
}

void TimblOpts::Add( char c, const string& opt, bool mood ){
  pimpl->Add( c, opt, mood );
}

bool TimblOpts::Delete( char c ){
  return pimpl->Delete( c );
}

TimblExperiment *Create_Pimpl( AlgorithmType algo, const string& ex_name,
			       GetOptClass *opt ){
  TimblExperiment *result = NULL;
  switch ( algo ){
  case IB1_a:
    result = new IB1_Experiment( opt->MaxFeatures(), ex_name );
    break;
  case IB2_a:
    result = new IB2_Experiment( opt->MaxFeatures(), ex_name );
    break;
  case IGTREE_a:
    result = new IG_Experiment( opt->MaxFeatures(), ex_name );
    break;
  case TRIBL_a:
    result = new TRIBL_Experiment( opt->MaxFeatures(), ex_name );
    break;
  case TRIBL2_a:
    result = new TRIBL2_Experiment( opt->MaxFeatures(), ex_name );
    break;
  case LOO_a:
    result = new LOO_Experiment( opt->MaxFeatures(), ex_name );
    break;
  case CV_a:
    result = new CV_Experiment( opt->MaxFeatures(), ex_name );
    break;
  default:
    cerr << "wrong algorithm to create TimblAPI" << endl;
    return NULL;
  }
  result->UseOptions( opt );
  return result;
}

TimblAPI::TimblAPI( const TimblOpts *T_Opts,
		    const string& name ):
  pimpl(), i_am_fine(false) {
  if ( T_Opts ){
    GetOptClass *OptPars = new GetOptClass( *T_Opts->pimpl );
    if ( !OptPars->parse_options( *T_Opts->pimpl ) )
      delete OptPars;
    else if ( OptPars->Algo() != Unknown_a ){
      pimpl = Create_Pimpl( OptPars->Algo(), name, OptPars );
    }
    else {
      pimpl = Create_Pimpl( IB1_a, name, OptPars );
    }
  }
  i_am_fine = (pimpl != NULL);
}

TimblAPI::TimblAPI( const string& pars, 
		    const string& name ):
  pimpl( NULL ), i_am_fine(false){
  CL_Options Opts( pars );
  GetOptClass *OptPars = new GetOptClass( Opts );
  if ( OptPars->parse_options( Opts ) ){
    if ( OptPars->Algo() != Unknown_a ){
      pimpl = Create_Pimpl( OptPars->Algo(), name, OptPars );
    }
    else {
      pimpl = Create_Pimpl( IB1_a, name, OptPars );
    }
    if ( pimpl )
      if ( !pimpl->ConfirmOptions() ){
	delete pimpl;
	pimpl = NULL;
      }
  }
  i_am_fine = (pimpl != NULL);
}

TimblAPI::~TimblAPI(){ 
  delete pimpl; 
}


bool TimblAPI::Valid() const {
  return i_am_fine && pimpl && !pimpl->ExpInvalid();
}


const string to_string( const Algorithm A ) {
  string result;
  switch ( A ){
  case IB1:
    result = "IB1";
    break;
  case IB2:
    result = "IB2";
    break;
  case IGTREE:
    result = "IGTREE";
    break;
  case TRIBL:
    result = "TRIBL";
    break;
  case TRIBL2:
    result = "TRIBL2";
    break;
  case LOO:
    result = "LOO";
    break;
  case CV:
    result = "CV";
    break;
  default:
    cerr << "invalid algorithm in switch " << endl;
    result = "Unknown Algorithm";
  }
  return result;
}

bool string_to( const string& s, Algorithm& A ){
  A = UNKNOWN_ALG;
  AlgorithmType tmp;
  if ( string_to( s, tmp ) ){
    switch ( tmp ){
    case IB1_a: A = IB1;
      break;
    case IB2_a: A = IB2;
      break;
    case IGTREE_a: A = IGTREE;
      break;
    case TRIBL_a: A = TRIBL;
      break;
    case TRIBL2_a: A = TRIBL2;
      break;
    case LOO_a: A = LOO;
      break;
    case CV_a: A = CV;
      break;
    default:
      return false;
    }
    return true;
  }
  return false;
}

const string to_string( const Weighting W ) {
  string result;
  switch ( W ){
  case UD:
    result = "ud";
    break;
  case NW:
    result = "nw";
    break;
  case GR:
    result = "gr";
    break;
  case IG:
    result = "ig";
    break;
  case X2:
    result = "x2";
    break;
  case SV:
    result = "sv";
    break;
  default:
    cerr << "invalid Weighting in switch " << endl;
    result = "Unknown Weight";
  }
  return result;
}

inline Weighting WT_to_W( WeightType wt ){
  Weighting w;
  switch ( wt ){
  case UserDefined_w: w = UD;
    break;
  case No_w: w = NW;
    break;
  case GR_w: w = GR;
    break;
  case IG_w: w = IG;
    break;
  case X2_w: w = X2;
    break;
  case SV_w: w = SV;
    break;
  default:
    w = UNKNOWN_W;
  }
  return w;
}

bool string_to( const string& s, Weighting& w ){
  w = UNKNOWN_W;
  WeightType tmp;
  if ( string_to( s, tmp ) ){
    w = WT_to_W( tmp );
    if( w == UNKNOWN_W )
      return false;
    return true;
  }
  return false;
}

Algorithm TimblAPI::Algo() const {
  Algorithm result = UNKNOWN_ALG;
  if ( pimpl ){
    switch ( pimpl->Algorithm() ){
    case IB1_a:
      result = IB1;
      break;
    case IB2_a:
      result = IB2;
      break;
    case IGTREE_a:
      result = IGTREE;
      break;
    case TRIBL_a:
      result = TRIBL;
      break;
    case TRIBL2_a:
      result = TRIBL2;
      break;
    case LOO_a:
      result = LOO;
      break;
    case CV_a:
      result = CV;
      break;
    default:
      cerr << "invalid algorithm in switch " << endl;
      break;
    }
  }
  return result;
}

bool TimblAPI::Learn( const string& s ){
  if ( Valid() )
    return pimpl->Learn( s );
  else 
    return false;
}

bool TimblAPI::Prepare( const string& s ){
  if ( Valid() )
    return pimpl->Prepare( s );
  else
    return false;
}

bool TimblAPI::Increment( const string& s ){
  return Valid() && pimpl->Increment( s );
}

bool TimblAPI::Decrement( const string& s ){
  return Valid() && pimpl->Decrement( s );
}

bool TimblAPI::Expand( const string& s ){
  return Valid() && pimpl->Expand( s );
}

bool TimblAPI::Remove( const string& s ){
  return Valid() && pimpl->Remove( s );
}

bool TimblAPI::Test( const string& in,
		     const string& out,
		     const string& p ){
  if ( !Valid() )
    return false;
  else {
    if ( in.empty() )
      return false;
    if ( out.empty() && Algo() != CV )
      return false;
    return pimpl->Test( in, out, p );
  }
}

const TargetValue *TimblAPI::Classify( const string& s,
				       const ClassDistribution *& db,
				       double& di ){
  if ( Valid() ){
    return pimpl->Classify( s, db, di );
  }
  return NULL;
}

const TargetValue *TimblAPI::Classify( const string& s ){
  if ( Valid() ){
    return pimpl->Classify( s );
  }
  return NULL;
}

const TargetValue *TimblAPI::Classify( const string& s,
				       const ClassDistribution *& db ){
  if ( Valid() ){
    return pimpl->Classify( s, db  );
  }
  return NULL;
}

const TargetValue *TimblAPI::Classify( const string& s,
				       double& di ){
  if ( Valid() ){
    return pimpl->Classify( s, di );
  }
  return NULL;
}

bool TimblAPI::Classify( const string& s, string& d ){
  return Valid() && pimpl->Classify( s, d );
}

bool TimblAPI::Classify( const string& s, string& d, double &f ) {
  return Valid() && pimpl->Classify( s, d, f );
}

bool TimblAPI::Classify( const string& s, string& d, 
			 string& e, double &f ){
  return Valid() && pimpl->Classify( s, d, e, f );
}

bool TimblAPI::SaveWeights( const string& f ){
  if ( Valid() )
    return pimpl->SaveWeights( f );
  else
    return false;
}

bool TimblAPI::GetWeights( const string& f, Weighting w ){
  if ( Valid() ){
    WeightType tmp;
    switch ( w ){
    case UNKNOWN_W: tmp = Unknown_w;
      break;
    case NW: tmp = No_w;
      break;
    case GR: tmp = GR_w;
      break;
    case IG: tmp = IG_w;
      break;
    case X2: tmp = X2_w;
      break;
    case SV: tmp = SV_w;
      break;
    default:
      i_am_fine = false;
      return false;
    }
    if ( !pimpl->GetWeights( f, tmp ) )
      i_am_fine = false;
    return Valid();
  }
  else 
    return false;
}

Weighting TimblAPI::CurrentWeighting() const{
  if ( Valid() )
    return WT_to_W( pimpl->CurrentWeighting() );
  else
    return UNKNOWN_W;
}

Weighting TimblAPI::GetCurrentWeights( std::vector<double>& res ) const {
  res.clear();
  if ( Valid() ){
    if ( pimpl->GetCurrentWeights( res ) )
      return CurrentWeighting();
  }
  return UNKNOWN_W;
}

bool TimblAPI::SetOptions( const string& argv ){
  return Valid() && pimpl->SetOptions( argv );
}

bool TimblAPI::SetIndirectOptions( const TimblOpts& O ){
  return Valid() && pimpl->IndirectOptions( *O.pimpl );
}

string TimblAPI::ExpName() const {
  if ( pimpl ) // return the name, even when !Valid()
    return pimpl->ExpName();
  else
    return "ERROR";
}

bool TimblAPI::WriteNamesFile( const string& f ){
  if ( Valid() ) {
    if ( !pimpl->WriteNamesFile( f ) )
      i_am_fine = false;
    return Valid();
  }
  else
    return false;
}

bool TimblAPI::WriteInstanceBase( const string& f ){
  if ( Valid() ){
    if ( !pimpl->WriteInstanceBase( f ) )
      i_am_fine = false;
    return Valid();
  }
  else
    return false;
}

bool TimblAPI::GetInstanceBase( const string& f ){
  if ( Valid() ){
    if ( !pimpl->ReadInstanceBase( f ) )
      i_am_fine = false;
    return Valid();
  }
  else
    return false;
}

bool TimblAPI::WriteArrays( const string& f ){
  if ( Valid() ){
    if ( !pimpl->WriteArrays( f ) )
      i_am_fine = false;
    return Valid();
  }
  else
    return false;
}

bool TimblAPI::GetArrays( const string& f ){
  if ( Valid() ){
    if ( !pimpl->GetArrays( f ) )
      i_am_fine = false;
    return Valid();
  }
  else
    return false;
}

bool TimblAPI::ShowBestNeighbors( ostream& os, 
				  bool dists_too ) const{
  return Valid() && pimpl->ShowBestNeighbors( os, true, dists_too );
}

bool TimblAPI::ShowWeights( ostream& os ) const{
  return Valid() && pimpl->ShowWeights( os );
}

bool TimblAPI::ShowOptions( ostream& os ) const{
  return Valid() && pimpl->ShowOptions( os );
}

bool TimblAPI::ShowSettings( ostream& os ) const{
  return Valid() && pimpl->ShowSettings( os );
}

bool TimblAPI::StartServer( const int port, const int max_c ){
  return Valid() && pimpl->StartServer( port, max_c );
}

bool TimblAPI::Set_Single_Threaded(){
  return Valid() && pimpl->SetSingleThreaded();
}

string TimblAPI::VersionInfo( bool full ){
  return Common::VersionInfo( full );
}
  
int TimblAPI::Default_Max_Feats() {
  return Common::DEFAULT_MAX_FEATS;
}

int stoi( const string& s ){
  return Common::stoi( s );
}

double stod( const string& s ){
  return Common::stod( s );
}

string itos( int i ){
  return Common::itos( i );
}

string dtos( double d ){
  return Common::dtos( d );
}

int split( const std::string&s, std::vector<std::string>& a ){
  return Common::split( s, a );
}

int split_at( const std::string&s, std::vector<std::string>& a,
	      const std::string& d ){
  return Common::split_at( s, a, d );
}

int split_at_first_of( const std::string&s, std::vector<std::string>& a,
		       const std::string& d ){
  return Common::split_at_first_of( s, a, d );
}
