/*
 * GetOptClass.
 *
 *    A helper class to parse commandlines and temporary store
 *    userdefined options.
 *    Can perform checks en modifications and (at last) store them
 *    in the TimblExperiment.
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
#include <fstream>
#ifdef IRIX64
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#else
#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cstring>
#include <ctime>
#include <cctype>
#endif

#ifdef USE_LOGSTREAMS
#include "LogStream.h"
#else
typedef std::ostream LogStream;
#endif

#include "SocketBasics.h"
#include "Common.h"
#include "MsgClass.h"
#include "Types.h"
#include "Options.h"
#include "Tree.h"
#include "Instance.h"
#include "GetOptClass.h"
#include "MBLClass.h"
#include "TimblExperiment.h"

using namespace std;

namespace Timbl {

  void GetOptClass::set_default_options( int Max ){
    local_algo = IB1_a;
    local_metric = UnknownMetric;
    local_order = UnknownOrdening;
    local_weight = Unknown_w;
    local_decay = Zero;
    local_decay_alfa = 1.0;
    local_decay_beta = 1.0;
    local_normalisation = -1;
    local_norm_factor = 1;
    no_neigh = 1;
    mvd_limit = 1;
    estimate = 0;
    maxbests = 500;
    BinSize = 0;
    clip_freq = 10;
    bootstrap_lines = -1;
    local_progress = 100000;
    seed = -1;
    do_exact = false;
    do_hashed = true;
    min_present = false;
    keep_distributions = false;
    do_sample_weights = false;
    do_ignore_samples = false;
    do_ignore_samples_test = false;
    do_query = false;
    probabilistic = false;
    do_all_weights = false;
    do_sloppy_loo = false;
    if ( MaxFeats == -1 ){
      MaxFeats = Max;
      MaxFeatsSet = true;
      LocalInputFormat = UnknownInputFormat; // InputFormat and verbosity
      MyVerbosity = NO_VERB;   // are not reset!
    }
    if ( !MetricsArray )
      MetricsArray = new MetricType[MaxFeats+1];
    for ( int i=0; i < MaxFeats+1; i++ ){
      MetricsArray[i] = DefaultMetric;
    }
  }
  
  GetOptClass::GetOptClass( CL_Options& Opts ):
    LocalInputFormat( UnknownInputFormat ),
    MaxFeats(-1),
    f_length( 0 ),
    treshold( -1 ),
    MyVerbosity( NO_VERB ),
    MaxFeatsSet( false ),
    opt_init( false ),
    opt_changed( false ),
    N_present( false ),
    MetricsArray( NULL ),
    parent_socket( 0 ) {
    int MaxF = DEFAULT_MAX_FEATS;
    bool the_mood;
    string optie;
    if ( Opts.Find( 'N', optie, the_mood ) ){
      N_present = true;
      MaxF = stoi( optie );
    }
    set_default_options( MaxF );
  }
  
  GetOptClass::~GetOptClass( ){
    if ( MetricsArray )
      delete [] MetricsArray;
  }
  
  GetOptClass::GetOptClass( const GetOptClass& in ):
    MsgClass(in),
    local_algo( in.local_algo ),
    local_metric( in.local_metric ),
    local_order( in.local_order ),
    local_weight( in.local_weight ),
    LocalInputFormat( in.LocalInputFormat ),
    local_decay( in.local_decay ),
    local_decay_alfa( in.local_decay_alfa ),
    local_decay_beta( in.local_decay_beta ),
    local_normalisation( in.local_normalisation ),
    local_norm_factor( in.local_norm_factor ),
    MaxFeats( in.MaxFeats ),
    no_neigh( in.no_neigh ),
    mvd_limit( in.mvd_limit ),
    estimate( in.estimate ),
    maxbests( in.maxbests ),
    clip_freq( in.clip_freq ),
    BinSize( in.BinSize ),
    bootstrap_lines( in.bootstrap_lines ),
    f_length( in.f_length ),
    local_progress( in.local_progress ),
    seed( in.seed ),
    treshold( in.treshold ),
    MyVerbosity( in.MyVerbosity ),
    MaxFeatsSet( in.MaxFeatsSet ),
    opt_init( in.opt_init ),
    opt_changed( in.opt_changed ),
    do_exact( in.do_exact ), 
    do_hashed( in.do_hashed ), 
    min_present( in.min_present ),
    N_present(false),
    keep_distributions( in.keep_distributions ),
    do_sample_weights( in.do_sample_weights ),
    do_ignore_samples( in.do_ignore_samples ), 
    do_ignore_samples_test( in.do_ignore_samples_test ),
    do_query( in.do_query ),
    probabilistic( in.probabilistic ),
    do_all_weights( false ),
    do_sloppy_loo( false ),
    MetricsArray(NULL),
    parent_socket( in.parent_socket )
  {
    MetricsArray = new MetricType[MaxFeats+1];
    for ( int i=0; i < MaxFeats+1; i++ ){
      MetricsArray[i] = in.MetricsArray[i];
    }
  }
  
  GetOptClass *GetOptClass::Clone( int tcp_id ) const{
    GetOptClass *result = new GetOptClass(*this);
    result->parent_socket = tcp_id;
    return result;
  }
  
#ifdef PTHREADS
  using SocketProcs::write_line;
  
  void GetOptClass::Error( const string& out_line ) {
    if ( parent_socket )
      write_line( parent_socket, "ERROR { " ) &&
	write_line( parent_socket, out_line ) &&
	write_line( parent_socket, " }\n" );
    else {
      cerr << "Error:" << out_line << endl;
    }
  }
  
#else
  
  void GetOptClass::Error( const string& out_line ) {
    cerr << "Error:" << out_line << endl;
  }
  
#endif

  inline char *StrDup( const char *s ){
    register unsigned int len = strlen( s );
    char *result = (char *)malloc( len+1 );
    strcpy( result, s );
    return result;
  }
  
  inline bool p_or_m( char k )
  { return ( k == '+' || k == '-' ); }
  

  inline int opt_split( const char *line, char **& new_argv ){ 
    int i, k=0;
    const char *p = line;
    char *tmp = new char[strlen(line)+1]; // maybe a few bits to much, so what
    int argc = 0;
    while ( *p ){
      if ( ( p_or_m(*p) && argc == 0 ) || 
	   ( isspace(*p++) && p_or_m(*p) ) ){
	argc++;
      }
    }
    if ( argc != 0 ){
      new_argv = new char*[argc];
      for ( i=0; i < argc; i++ )
	new_argv[i] = NULL;
      p = line;
      i = 0;
      while ( isspace( *p ) ){ p++; };
      while ( *p ){
	int skip = 0;
	while ( isspace( *p ) ){ p++; skip++; };
	if ( !*p )
	  break;
	if ( skip != 0 && p_or_m(*p) && k != 1 ){
	  if ( i >= 0 ){
	    tmp[k] = '\0';
	    new_argv[i] = StrDup( tmp );
	  }
	  i++;
	  k = 0;
	}
	tmp[k++] = *p++;
      }
      tmp[k] = '\0';
      new_argv[i] = StrDup( tmp );
    }
    delete [] tmp;
    return argc;
  }
  

  CommandLine *Split_Command_Line( const int Argc, 
				   const char * const *Argv ){
    int local_argc = 0;
    char **my_local_argv = NULL;
    const char * const *Pnt;
    char Optchar;
    const char *Option;
    bool Mood = false;
    int arg_ind = 0;
    if ( Argc == 0 ) 
      if ( Argv != NULL &&
	   Argv[0] != NULL ){
	local_argc = opt_split( Argv[0], my_local_argv );
	Pnt = my_local_argv;
      }
      else
	return NULL;
    else {
      local_argc = Argc;
      Pnt = Argv;
      arg_ind++; // to skip the program name
    }
    CommandLine *result = new CommandLine();
    while ( arg_ind < local_argc ){
      Option = Pnt[arg_ind];
      if ( !p_or_m(*Option) ){
	Optchar = '?';
	Mood = false;
      }
      else {
	Mood = *Option++ == '+';
	Optchar = *Option++;
	if ( (!Optchar || !*Option ) && arg_ind+1 < local_argc ) {
	  if ( !p_or_m(*Pnt[arg_ind+1] ) ){
	    Option = Pnt[++arg_ind];
	    if ( !Optchar )
	      Optchar = *Option;
	  }
	}
      }
      CL_item cl( Optchar, Option, Mood );
      result->push_front( cl );
      arg_ind++;
    }
    if ( my_local_argv ){
      int i;
      for( i=0; i < local_argc; i++ )
	free( my_local_argv[i] );
      delete [] my_local_argv;;
    }
    return result;
  }
  
  bool GetOptClass::definitive_options( TimblExperiment *Exp ){
    if ( opt_changed || !opt_init ){
      opt_changed = false;
      bool first = !opt_init;
      if ( !opt_init )
	opt_init = true;
      string optline;
      if ( first ){
	// the following options can only be set once!
	// If you try it anyway, you should get a MblClass warning...
	if ( LocalInputFormat == SparseBin ){
	  if ( !N_present ){
	    Error( "Missing -N option, mandatory for -F Binary" );
	    return false;
	  }
	}
	if ( LocalInputFormat == Sparse ){
	  if ( !N_present ){
	    Error( "Missing -N option, mandatory for -F Sparse" );
	    return false;
	  }
	}
	if ( LocalInputFormat != UnknownInputFormat ){
	  optline = "INPUTFORMAT: " + to_string(LocalInputFormat);
	  if (!Exp->SetOption( optline ))
	    return false;
	}
	if ( keep_distributions ){
	  optline = "KEEP_DISTRIBUTIONS: true";
	  if (!Exp->SetOption( optline ))
	    return false;
	}
	if ( do_sloppy_loo ){
	  if ( local_algo != LOO_a ){
	    Error( "sloppy only valid for LOO algorith" );
	    return false;
	  }
	  else {
	    optline = "DO_SLOPPY_LOO: true";
	    if (!Exp->SetOption( optline ))
	      return false;
	  }
	}
	if ( f_length > 0 ){
	  optline = "FLENGTH: " + to_string(f_length);
	  if (!Exp->SetOption( optline ))
	    return false;
	}
	optline = "MAXBESTS: " + to_string(maxbests);
	Exp->SetOption( optline );
	if ( BinSize > 0 ){
	  optline = "BIN_SIZE: " + to_string(BinSize);
	  Exp->SetOption( optline );
	}
	if ( treshold > 0 ){
	  optline = "TRIBL_OFFSET: " + to_string(treshold);
	  Exp->SetOption( optline );
	}
	if ( local_order != UnknownOrdening ){
	  optline = "TREE_ORDER: " + to_string(local_order);
	  Exp->SetOption( optline );
	}
      }
      if ( estimate < 10 )
	Exp->Estimate( 0 );
      else
	Exp->Estimate( estimate );
      if ( MyVerbosity & DISTRIB ){
	if ( !keep_distributions && local_algo == IGTREE_a ){
	  MyVerbosity &= ~DISTRIB;
	  Info( "Ignoring option +vDB, while +D is missing!" );
	}
      }
      if ( MyVerbosity & CONF_MATRIX ||
	   MyVerbosity & CLASS_STATS )
	MyVerbosity |= ADVANCED_STATS;
      if ( do_exact )
	Exp->SetOption(  "EXACT_MATCH: true" );
      else
	Exp->SetOption(  "EXACT_MATCH: false" );
      if ( do_hashed )
	Exp->SetOption(  "HASHED_TREE: true" );
      else
	Exp->SetOption(  "HASHED_TREE: false" );
      if ( probabilistic )
	Exp->SetOption(  "PROBABILISTIC: true" );
      else
	Exp->SetOption(  "PROBABILISTIC: false" );
      if ( do_sample_weights ){
	Exp->SetOption(  "EXEMPLAR_WEIGHTS: true" );
	if ( do_ignore_samples )
	  Exp->SetOption( "IGNORE_EXEMPLAR_WEIGHTS: true" );
	else
	  Exp->SetOption( "IGNORE_EXEMPLAR_WEIGHTS: false" );
	if ( do_ignore_samples_test )
	  Exp->SetOption( "NO_EXEMPLAR_WEIGHTS_TEST: true" );
	else
	  Exp->SetOption( "NO_EXEMPLAR_WEIGHTS_TEST: false" );
      }
      else
	Exp->SetOption(  "EXEMPLAR_WEIGHTS: false" );
      if ( local_metric != UnknownMetric ){
	if ( local_metric == DotProduct ){
	  optline = "GLOBAL_METRIC: " +  to_string(Numeric);
	  Exp->SetOption( optline );
	  optline = "DO_DOT_PRODUCT: true";
	  Exp->SetOption( optline );
	}
	else {
	  optline = "GLOBAL_METRIC: " + to_string(local_metric);
	  Exp->SetOption( optline );
	}
      }
      if ( local_weight != Unknown_w ){
	optline = "WEIGHTING: " + to_string(local_weight);
	Exp->SetOption( optline );
      }
      if ( do_all_weights ){
	optline = "ALL_WEIGHTS: true";
	Exp->SetOption( optline );
      }
      if ( bootstrap_lines > 0 ){
	optline = "IB2_OFFSET: " + to_string(bootstrap_lines);
	Exp->SetOption( optline );
      }
      if ( local_normalisation >= 0 ){
	optline = "NORMALISATION: " + to_string( local_normalisation );
	Exp->SetOption( optline );
	if ( local_normalisation == 1 ){
	  optline = "NORM_FACTOR: " + dtos( local_norm_factor );
	  Exp->SetOption( optline );
	}
      }
      optline = "MVD_LIMIT: " + to_string(mvd_limit);
      Exp->SetOption( optline );
      optline = "NEIGHBORS: " + to_string(no_neigh);
      if ( Exp->SetOption( optline ) ){
	optline = "DECAY: " + to_string(local_decay);
	if ( Exp->SetOption( optline ) ){
	  optline = "DECAYPARAM_A: " + to_string(local_decay_alfa);
	  if ( Exp->SetOption( optline ) ){
	    optline = "DECAYPARAM_B: " + to_string(local_decay_beta);
	    if ( Exp->SetOption( optline ) ){
	      optline = "CLIP_FACTOR: " + to_string(clip_freq);
	      if ( Exp->SetOption( optline ) ){
		optline = "SEED: " + to_string(seed);
		if ( Exp->SetOption( optline ) ){
		  optline = "PROGRESS: " + to_string(local_progress);
		  if ( Exp->SetOption( optline ) ){
		    optline = "VERBOSITY: " + 
		      to_string(MyVerbosity);
		    if ( Exp->SetOption( optline ) ){
		      for ( int i=0; i < MaxFeats+1; i++ ){
			if ( !first ){
			  if ( MetricsArray[i] == Ignore ){
			    Error( "-M:I is not possible now" );
			    return false;
			  }
			  else if ( MetricsArray[i] == Numeric ){
			    Error( "-M:N is not possible now" );
			    return false;
			  }
			}
			optline = "METRICS: " + to_string(i ) + "=" +
			  to_string(MetricsArray[i]);
			if (!Exp->SetOption( optline ) )
			  return false;
		      }
		      if ( do_query ){
			Exp->ShowSettings( cerr );
			do_query = false;
		      }
		      return true;
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
      return false;
    }
    return true;
  }
  
  inline bool GetOptClass::parse_range( string& line, 
					string::iterator& it,
					MetricType Value, 
					MetricType *info ){
    int k, m;
    string::iterator eit;
    while( it != line.end() && *it != ':' ){
      eit = it;
      while( eit != line.end() && isdigit( *eit ) ) ++eit;
      string tmp = string( it, eit ); 
      k = stoi(tmp);
      if ( k > 0 && k <= MaxFeats ){
	if ( info[k] != DefaultMetric && info[k] != Value ){
	  Error( "metric of feature " + tmp +
		 " is multiply changed!" );
	  return false;
	}
	info[k] = Value;
      }
      else {
	Error( "illegal value in metric description: -m " + line );
	return false;
      }
      it = eit;
      if ( it == line.end() ){
	return true;
      }
      else if ( *it == ',' )
	++it;
      else if ( *it == '-' ){
	++it;
	eit = it;
	while( eit != line.end() && isdigit( *eit ) ) ++eit;
	string tmp = string( it, eit ); 
	m = stoi(tmp);
	if ( m <= 0 || m > MaxFeats ){
	  Error( "illegal value in metric description: -m " + line );
	  return false;
	}
	it = eit;
	if ( it != line.end() && (*it != ',' && *it != ':' ) ){
	  Error( "illegal value in metric description: -m " + line );
	  return false;
	}
	if ( m < k ){
	  Error( "illegal value in metric description: -m " + line );
	  return false;
	}
	else {
	  int j;
	  for ( j=k+1; j <= m && j <= MaxFeats; j++ ){
	    if ( info[j] != DefaultMetric && info[j] != Value ){
	      Error( "metric of feature " + to_string(j) + 
		     " is multiply changed!" );
	      return false;
	    }
	    info[j] = Value;
	  }
	}
	if ( it != line.end() && *it == ',' ) ++it;
      }
    }
    return true;
  }
  
  inline bool GetOptClass::parse_metrics( const string& Mline,
					  MetricType& Def,
					  MetricType *info ){
    string line = Mline;
    uppercase( line );
    string::iterator p = line.begin();
    while ( p != line.end() && isspace( *p ) ) p++;
    if ( p != line.end() ){
      switch ( *p++ ){
      case 'O' : 
	Def = Overlap;
	break;
      case 'J' :
	Def = JeffreyDiv;
	break;
      case 'M' :
	Def = ValueDiff;
	break;
      case 'N' :
	Def = Numeric;
	break;
      case 'D' :
	Def = DotProduct;
	break;
      case 'I' :
	Def = Ignore;
	break;
      default:
	Error( "illegal default value for metric: -m " + Mline );
	return false;
      }
      if ( p == line.end() ){
	if ( Def == Ignore ){
	  Error( "Ignore without further specification for metric: -m " + Mline );
	  return false;
	}
	return true;
      }
      else if ( *p != ':' ){
	Error( "missing ':' after default value in -m option" );
	return false;
      }
      else {
	++p;
	MetricType TmpMT;
	while( p != line.end() ){
	  switch ( *p ){
	  case 'O' : 
	    TmpMT = Overlap;
	    break;
	  case 'J' :
	    TmpMT = JeffreyDiv;
	    break;
	  case 'M' :
	    TmpMT = ValueDiff;
	    break;
	  case 'N' :
	    TmpMT = Numeric;
	    break;
	  case 'I' :
	    TmpMT = Ignore;
	    break;
	  default:
	    Error( "illegal value in metric description: -m " + Mline );
	    return false;
	  }
	  if ( Def == DotProduct && TmpMT != Ignore ){
	    Error( "DotProduct only accepts -I specifications: -m " + Mline );
	    return false;
	  }
	  ++p;
	  if ( !parse_range( line, p, TmpMT, info ) )
	    return false;
	  if ( p == line.end() ){
	    break;
	  }
	  if ( *p != ':' ){
	    Error( "missing ':' in metric description" );
	    return false;
	  }
	  else
	    ++p;
	}
	if ( p != line.end() ){
	  Error( "illegal value in metric description: -m " + Mline );
	  return false;
	}
	else {
	  if ( Def == Ignore ){
	    for ( int k=0; k <= MaxFeats; ++k )
	      if ( info[k] == DefaultMetric )
		info[k] = Ignore;
	  }
	  return true;
	}
      }
      return true;
    }
    else
      return false;
  }
  
  char get_option( CL_item& Opts,  string& arg, bool& mood ){
    arg = Opts.Option();
    mood = Opts.Mood();
    return Opts.OptChar();
  }      

  bool GetOptClass::parse_options( const CL_Options& opts,
				   const int mode ){
    opt_changed = true;
    const char *q;
    list<CL_item>::iterator curr_opt;
    curr_opt = opts.Opts->begin();
    if ( curr_opt == opts.Opts->end() ){
      return true;
    }
    const char *ok_opt;
    switch ( mode ){
    case 0: 
      ok_opt = "a:b:B:c:d:De:F:G:Hk:l:L:m:M:n:N:p:q:QR:st:T:v:w:Wx"; 
      break;
    case 1:
      // limited usage, for @t
      ok_opt = "d:e:G:k:L:m:p:QR:v:x"; 
      break;
    case 2:
      // limited usage, for Server
      ok_opt = "d:G:k:L:m:Qv:x"; 
      break;
    default:
      ok_opt = NULL;
      cerr << "Invalid value '" << mode << "' in switch (" 
	   << __FILE__ << "," << __LINE__ << ")" << endl;
      cerr << "ABORTING now" << endl;
      abort();
    }
    while ( curr_opt != opts.Opts->end()  ) {
      bool mood = false;
      string myoptarg;
      int option = get_option( *curr_opt, myoptarg, mood );
      if ( !strchr( ok_opt, option ) ){
	// invalid option
	switch ( mode ){
	case 1:
	case 2:{
	  string LongLine;
	  q = ok_opt;
	  while ( *q ){
	    if ( *q != ':'  ){
	      LongLine = LongLine + *q + ' ';
	    }
	    q++;
	  }
	  Error( string("Illegal option, -") + (char)option
		 + ", only the following options are allowed:\n"
		 + LongLine );
	}
	break;
	default:
	  break;
	}
	return false;
      };
      
      switch (option) {
      case 'a': 
	if ( !string_to( myoptarg, local_algo ) ){
	  Error( "illegal -a value: " + myoptarg );
	  return false;
	}
	break;

      case 'b':
	bootstrap_lines = stoi( myoptarg );
	if ( bootstrap_lines < 1 ){
	  Error( "illegal value for -b option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'B':
	BinSize = stoi( myoptarg );
	if ( BinSize <= 1 ){
	  Error( "illegal value for -B option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'c':
	clip_freq = stoi( myoptarg );
	if ( clip_freq < 0 ){
	  Error( "illegal value for -c option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'd': {
	string::size_type pos1 = myoptarg.find( ":" );
	if ( pos1 == string::npos ){
	  pos1 = myoptarg.find_first_of( "0123456789" );
	  if ( pos1 != string::npos ){
	    if ( ! ( string_to( string( myoptarg, 0, pos1 ), local_decay ) &&
		     string2double( string( myoptarg, pos1 ), 
				    local_decay_alfa ) ) ){
	      Error( "illegal value for -d option: " + myoptarg );
	      return false;
	    }
	  }
	  else if ( !string_to( myoptarg, local_decay ) ){
	    Error( "illegal value for -d option: " + myoptarg );
	    return false;
	  }
	}
	else {
	  string::size_type pos2 = myoptarg.find( ':', pos1+1 );
	  if ( pos2 == string::npos ){
	    pos2 = myoptarg.find_first_of( "0123456789", pos1+1 );
	    if ( pos2 != string::npos ){
	      if ( ! ( string_to( string( myoptarg, 0, pos1 ),
				  local_decay ) &&
		       string2double( string( myoptarg, pos2 ), 
				      local_decay_alfa ) ) ){
		Error( "illegal value for -d option: " + myoptarg );
		return false;
	      }
	    }
	    else {
	      Error( "illegal value for -d option: " + myoptarg );
	      return false;
	    }
	  }
	  else {
	    if ( ! ( string_to( string( myoptarg, 0, pos1 ), local_decay ) &&
		     string2double( string( myoptarg, pos1+1, pos2-pos1-1 ), 
				    local_decay_alfa ) &&
		     string2double( string( myoptarg, pos2+1 ), 
				    local_decay_beta ) ) ){
	      Error( "illegal value for -d option: " + myoptarg );
	      return false;
	    }
	  }
	}
	break;
      }
      
      case 'D':
	keep_distributions = mood;
	break;
	
      case 'e':
	estimate = stoi( myoptarg );
	if ( estimate < 0 ){
	  Error( "illegal value for -e option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'F':
	if ( !string_to( myoptarg, LocalInputFormat ) ){
	  Error( "illegal value for -F option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'H':
	do_hashed = mood;
	break;
	
      case 'k':
	no_neigh = stoi(myoptarg);
	if ( no_neigh <= 0 ){
	  Error( "illegal value for -k option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'l':
	f_length = stoi( myoptarg );
	if ( f_length <= 0 ){
	  Error( "illegal value for -l option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'L':
	mvd_limit = stoi( myoptarg );
	if ( mvd_limit <= 0 ){
	  Error( "illegal value for -L option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'm': 
	if ( !parse_metrics( myoptarg, local_metric, 
			     MetricsArray) )
	  return false;
	break;
      
      case 'M':
	maxbests = stoi( myoptarg );
	if ( maxbests <= 0 ){
	  Error( "illegal value for -M option: " + myoptarg );
	  return false;
	}
	break;
	
      case 'N':
	// skip previously parsed NumOfFeatures info.
	break;
	
      case 'p':
	local_progress = stoi( myoptarg );
	break;
	
      case 'q':
	treshold = stoi( myoptarg );
	break;
	
      case 'Q':
	do_query = true;
	break;
	
      case 'R':
	if ( myoptarg[0] == 'P' ){
	  probabilistic = true;
	  myoptarg.erase(0,1);
	}
	if ( isdigit(myoptarg[0]) )
	  seed = stoi( myoptarg );
	else {
	  Error( "Integer argument for Random Seed expected (-R option)" );
	  return false;
	}
	break;
	
      case 's':
	if ( myoptarg == "loppy" ){
	  do_sloppy_loo = true;
	}
	else {
	  do_sample_weights = true;
	  if ( !myoptarg.empty() ){
	    if ( isdigit(myoptarg[0]) ){
	      int val = stoi( myoptarg );
	      if ( val == 0 ){
		do_ignore_samples = true;
		do_ignore_samples_test = true;
	      }
	      do_ignore_samples_test = val == 1;
	    }
	  }
	}
	break;
	
      case 'G':
	if ( myoptarg.empty() )
	  local_normalisation = 0;
	else {
	  string::size_type pos1 = myoptarg.find( ":" );
	  if ( pos1 == string::npos ){
	    local_normalisation = stoi( myoptarg );
	    local_norm_factor = 1;
	  }
	  else {
	    local_normalisation = stoi( string( myoptarg, 0, pos1 ) );
	    if ( !string2double( string( myoptarg, pos1+1 ), 
				 local_norm_factor ) ||
		 local_norm_factor < Epsilon ){
	      Error( "illegal value for -G option: " + myoptarg );
	      return false;
	    }
	  }
	  if ( local_normalisation < 0 ||
	       local_normalisation > 1 ){
	    Error( "illegal value for -G option: " + myoptarg );
	    return false;
	  }
	}
	break;
	
      case 't':
	if ( compare_nocase( myoptarg, "leave_one_out" ) )
	  local_algo = LOO_a;
	else if ( compare_nocase( myoptarg, "cross_validate" ) )
	  local_algo = CV_a;
	break;
	
      case 'T': {
	if ( !string_to( myoptarg, local_order ) ){
	  Error( "illegal -T value: " + myoptarg );
	  return false;
	}
      }
      break;
      
      case 'v':{
	VerbosityFlags Flag = NO_VERB;
	if ( !string_to( myoptarg, Flag ) ){
	  Error( "illegal value for +/- v option: " + myoptarg );
	  return false;
	}
	else {
	  if ( mode == 2 &&
	       ( !(Flag & (SILENT|DISTANCE|DISTRIB|NEAR_N|CONF_MATRIX) ) ) )
	    return false;
	  else if ( Flag > 0 )
	    if ( mood ){
	      MyVerbosity |= Flag;
	    }
	    else {
	      MyVerbosity &= ~Flag;
	    }
	  else
	    MyVerbosity = NO_VERB;
	}
      }
      break;
      
      case 'w': {
	if ( !string_to( myoptarg, local_weight ) )
	  return false;
      };
      break;
      
      case 'W': {
	do_all_weights = true;
      };
      break;
      
      case 'x':
	do_exact = mood;
	break;
	
      }
      ++curr_opt;
    }
    return true;
  }
  
  CL_Options::CL_Options( const int argc, const char * const *argv ):
    Opts( Split_Command_Line( argc, argv ) ){
    if ( !Opts )
      exit(99);
  }
  
  CL_Options::CL_Options( const string& args ){
    const char *argstr = args.c_str();
    Opts = Split_Command_Line( 0, &argstr );
    if ( !Opts )
      exit(99);
  }
  
  CL_Options::~CL_Options(){
    delete Opts;
  }
  
  ostream& operator<<( ostream& os, const CL_Options& cl ){
    list<CL_item>::iterator pos;
    for ( pos = cl.Opts->begin(); pos != cl.Opts->end(); ++pos ){
      os << *pos << " ";
    }
    return os;
  }

  bool CL_Options::Present( const char c ){
    list<CL_item>::iterator pos;
    for ( pos = Opts->begin(); pos != Opts->end(); ++pos ){
      if ( pos->OptChar() == c ){
	return true;
      }
    }
    return false;
  }
  
  bool CL_Options::Find( const char c, string &opt, bool& mood ){
    list<CL_item>::iterator pos;
    for ( pos = Opts->begin(); pos != Opts->end(); ++pos ){
      if ( pos->OptChar() == c ){
	opt = pos->Option();
	mood = pos->Mood();
	return true;
      }
    }
    return false;
  }
  
  bool CL_Options::Delete( const char c, bool all ){
    bool result = Present( c );
    if ( result ) {
      do {
	list<CL_item>::iterator pos;
	for ( pos = Opts->begin(); pos != Opts->end(); ++pos ){
	  if ( pos->OptChar() == c ){
	    Opts->erase(pos);
	    break; // for
	  }
	}
      } while ( all && Present(c) );
    }
    return result;
  }
  
  void CL_Options::Add( const char c, const string& line, bool mood ){
    CL_item cl( c, line, mood );
    Opts->push_front( cl );
  }
  
}


