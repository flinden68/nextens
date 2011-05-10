/*
 * MBLClass.cc
 *
 * A Memory Based Learning Class
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
#include <set>
#include <fstream>
#include <string>
#if __GNUC__ < 3
#include <strstream>
#else
#include <limits>
#include <sstream>
#endif
#include <iomanip>
#include <typeinfo>

#ifdef IRIX64
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
#include <assert.h>
#else
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <climits>
#include <cfloat>
#include <cctype>
#include <cassert>
#endif

#include "MsgClass.h"
#include "Common.h"
#include "Types.h"
#include "Options.h"
#include "Tree.h"
#include "Instance.h"
#include "IBtree.h"
#ifdef USE_LOGSTREAMS
#include "LogStream.h"
#else
typedef std::ostream LogStream;
#define Log(X) (X)
#define Dbg(X) (X)
#endif
#include "SocketBasics.h"
#include "MBLClass.h"

#define rint(N) ((float)(int)((N)+0.5))

using namespace std;

namespace Timbl {

  const int DefaultTargetNumber  = 20;
  const int DefaultFeatureNumber = 50;
  const int TargetIncrement      = 50;
  const int FeatureIncrement     = 100;
  

  class bestrec {
  public:
    bestrec();
    ~bestrec();
    int TotalBests() { return AggregateDist.SumFrequencies(); };
    int        CurrPos;  
    double     BestDistance;
    ValueDistribution AggregateDist;
    WValueDistribution *SummedDist;
    double entropy;
    ValueDistribution** BestDistributions;
    FeatureValue*** BestInstances;
  private:
    bestrec( const bestrec& );
    bestrec& operator=( const bestrec& );
  };
  
  bestrec::bestrec():
    CurrPos( 0 ),
    BestDistance( 0.0 ),
    SummedDist( NULL ),
    entropy( 0.0 ),
    BestDistributions( NULL ),
    BestInstances( NULL )
  {}
  
  bestrec::~bestrec(){
    if ( BestInstances ){
      int i;
      for ( i=0; i < CurrPos; i++ )
	delete [] BestInstances[i];
      delete [] BestInstances;
      delete [] BestDistributions;
    }
    delete SummedDist;
  }
  
  static const string DefaultSparseString = "0.0000E-17";


  void MBLClass::fill_table(){
    Options.Add( new IntegerOption( "FLENGTH",
				    &F_length, 0, 1, 32 ) );
    Options.Add( new IntegerOption( "MAXBESTS", 
				    &MaxBests, 500, 10, 100000 ) );
    Options.Add( new IntegerOption( "TRIBL_OFFSET", 
				    &tribl_offset, 0, 0, MaxFeatures ) );
    Options.Add( new InputFormatOption( "INPUTFORMAT",
					&input_format, 
					UnknownInputFormat ) ); 
    Options.Add( new OrdeningOption( "TREE_ORDER",
				     &TreeOrder, UnknownOrdening ) );
    Options.Add( new BoolOption( "ALL_WEIGHTS", 
				 &need_all_weights, false ) );
    Options.Add( new WeightOption( "WEIGHTING",
				   &Weighting, GR_w ) );
    Options.Add( new IntegerOption( "BIN_SIZE", 
				    &Bin_Size, 20, 2, 10000 ) );
    Options.Add( new IntegerOption( "IB2_OFFSET", 
				    &ib2_offset, 0, 1, 10000000 ) );
    Options.Add( new BoolOption( "KEEP_DISTRIBUTIONS", 
				 &keep_distributions, false ) );
    Options.Add( new BoolOption( "DO_SLOPPY_LOO", 
				 &do_sloppy_loo, false ) );
    Options.SetFreezeMark();
    Options.Add( new DecayOption( "DECAY",
				  &decay_flag, Zero ) ); 
    Options.Add( new IntegerOption( "SEED",
				    &random_seed, -1, -1, RAND_MAX ) );
    Options.Add( new RealOption( "DECAYPARAM_A", 
				 &decay_alfa, 1.0, 0.0, DBL_MAX ) );
    Options.Add( new RealOption( "DECAYPARAM_B", 
				 &decay_beta, 1.0, 0.0, DBL_MAX ) );
    Options.Add( new IntegerOption( "NORMALISATION",
                                    &normalisation, -1, 0, 1 ) );
    Options.Add( new RealOption( "NORM_FACTOR",
				 &norm_factor, 1.0, Epsilon, DBL_MAX ) );
    Options.Add( new BoolOption( "EXEMPLAR_WEIGHTS", 
				 &do_sample_weighting, false ) );
    Options.Add( new BoolOption( "IGNORE_EXEMPLAR_WEIGHTS", 
				 &do_ignore_samples, true ) );
    Options.Add( new BoolOption( "NO_EXEMPLAR_WEIGHTS_TEST", 
				 &no_samples_test, true ) );
    Options.Add( new BoolOption( "PROBABILISTIC", 
				 &probabilistic, false ) );
    Options.Add( new VerbosityOption( "VERBOSITY", 
				      &verbosity, NO_VERB ) );
    Options.Add( new BoolOption( "EXACT_MATCH", 
				 &do_exact_match, false ) );
    Options.Add( new BoolOption( "DO_DOT_PRODUCT", 
				 &do_dot_product, false ) );
    Options.Add( new BoolOption( "HASHED_TREE", 
				 &hashed_trees, true ) );
    Options.Add( new MetricOption( "GLOBAL_METRIC", 
				   &GlobalMetric, Overlap ) );
    Options.Add( new MetricArrayOption( "METRICS", 
					UserOptions, DefaultMetric, 
					MaxFeatures+1 ) );
    Options.Add( new IntegerOption( "MVD_LIMIT", 
				    &mvd_treshold, 1, 1, 100000 ) );
    Options.Add( new IntegerOption( "NEIGHBORS", 
				    &num_of_neighbors, 1, 1, 5000 ) );
    Options.Add( new IntegerOption( "PROGRESS", 
				    &progress, 10000, 1, 1000000 ) );
    if ( !Options.Add( new IntegerOption( "CLIP_FACTOR", 
					  &clip_factor, 10, 0, 1000000 ) ) ){
      Error( "Too many options for OptionTable" );
    }
  }

  void MBLClass::InvalidMessage(void) const{
    if ( err_count++ == 1 )
      Warning( "A preceding error prevents any operation on this "
	       "Timbl Object\n"
	       "other experiments might not be influenced" );
    else 
      Warning( "This Experiment is invalid due to errors" );
  }
  
  bool MBLClass::SetOption( const string& line ){ 
    bool result = false;
    if ( !ExpInvalid() ){
      enum SetOptRes opt_res = Options.SetOption( line );
      switch ( opt_res ){
      case Opt_OK: // OK
	MBL_init = false; // To assure redoing initializing stuff
	result = true;
	break;
      case Opt_Frozen:
	Warning( "SetOption '" + line + "' ignored.\nThis option may not "
		 "be changed after an InstanceBase is already created" );
	break;
      case Opt_Unknown:
	Warning( "SetOption '" + line + "' failed.\nOption unknown" );
	break;
      case Opt_Ill_Val:
	Warning( "SetOption '" + line + 
		 "' failed.\nillegal value for this option" );
	break;
      }
    }
    return result;
  }
  
  void MBLClass::InitClass( const int Size ){
    is_copy = false;
    tcp_socket = 0;
    Features  = NULL;
    PermFeatures = NULL;
    Targets   = NULL;
    BestArray = NULL;
    BASize     = 0;
    err_count = 0;
    MBL_init = false;
    need_all_weights = false;
    InstanceBase = NULL;
    TargetStrings = NULL;
    FeatureStrings = NULL;
    num_of_features     = 0;
    mvd_treshold = 1;
    effective_feats = 0;
    num_of_num_features = 0;
    DBEntropy = 0.0;
    ChopInput = 0;
    ChoppedInput = NULL;
    OriginalInput = "";
    CurrInst = NULL;
    MaxFeatures = Size;
    do_sparse = false;
    do_sloppy_loo = false;
    keep_distributions = false;
    Permutation = new int[MaxFeatures];
    int i;
    for ( i=0; i< MaxFeatures; i++ ){
      Permutation[i] = i;
    }
    UserOptions = new MetricType[MaxFeatures+1];
    test_feature_val = new FFIntMemberFn[MaxFeatures];
    fill_table();
#ifndef USE_LOGSTREAMS
    myerr = &cerr;
    mylog = &cout;
    mydebug = &cerr;
#else
    myerr = new LogStream( cerr, NULL, NoStamp );
    mylog = new LogStream( cout, NULL, NoStamp );
    mydebug = new LogStream( cout, "Debug", StampBoth );
    //    mylog->setlevel(LogSilent); 
    //    mydebug->settreshold(LogHeavy); 
#endif
    server_verbosity = &verbosity;
  }

  MBLClass::MBLClass( const string& name ){
    exp_name = name;
  }

  MBLClass &MBLClass::operator=( const MBLClass& m ){
    if ( this != &m ){
      is_copy = true;
      MaxFeatures        = m.MaxFeatures;
      UserOptions  = new MetricType[MaxFeatures+1];
      fill_table();
      F_length           = m.F_length;
      MaxBests           = m.MaxBests;
      TreeOrder          = m.TreeOrder;
      decay_flag         = m.decay_flag;
      input_format       = m.input_format;
      random_seed        = m.random_seed;
      decay_alfa         = m.decay_alfa;
      decay_beta         = m.decay_beta;
      normalisation      = m.normalisation;
      do_sample_weighting = m.do_sample_weighting;
      do_ignore_samples  = m.do_ignore_samples;
      no_samples_test    = m.no_samples_test;
      keep_distributions = m.keep_distributions;
      probabilistic        = m.probabilistic;
      verbosity          = m.verbosity;
      do_exact_match     = m.do_exact_match;
      do_dot_product     = m.do_dot_product;
      GlobalMetric       = m.GlobalMetric;
      for ( int i=0; i < MaxFeatures+1; i++ )
	UserOptions[i]  = m.UserOptions[i];
      mvd_treshold       = m.mvd_treshold;
      num_of_neighbors   = m.num_of_neighbors;
      dynamic_neighbors  = m.dynamic_neighbors;
      num_of_features    = m.num_of_features;
      progress           = m.progress;
      tribl_offset       = m.tribl_offset;
      ib2_offset         = m.ib2_offset;
      Weighting          = m.Weighting;
      do_sparse          = m.do_sparse;
      do_sloppy_loo      = m.do_sloppy_loo;
      Permutation = new int[MaxFeatures];
      test_feature_val = new FFIntMemberFn[MaxFeatures];
      for ( int j=0; j < MaxFeatures; j++ ){
	Permutation[j] = m.Permutation[j];
	test_feature_val[j] = m.test_feature_val[j];
      }
      Features  = m.Features;
      PermFeatures = m.PermFeatures;
      Targets   = m.Targets;
      BestArray = NULL;
      BASize     = 0;
      err_count = 0;
      MBL_init = false;
      need_all_weights = false;
      InstanceBase = m.InstanceBase->Clone();
      TargetStrings = m.TargetStrings;
      FeatureStrings = m.FeatureStrings;
      effective_feats = m.effective_feats;
      num_of_num_features    = m.num_of_num_features;
      DBEntropy = 0.0;
      ChopInput = m.ChopInput;
      ChoppedInput = new string[num_of_features+2];
      //one extra to store the target!
      OriginalInput = "";
      CurrInst = new Instance( num_of_features );
#ifndef USE_LOGSTREAMS
      myerr = m.myerr;
      mylog = m.mylog;
      mydebug = m.mydebug;
#else
      myerr = new LogStream( m.myerr );
      mylog = new LogStream( m.mylog );
      mydebug = new LogStream( m.mydebug );
#endif
      server_verbosity = &m.verbosity;
    }
    return *this;
  }

  MBLClass::~MBLClass(){
    if ( !is_copy ){
      delete InstanceBase;
      delete TargetStrings;
      delete FeatureStrings;
      if ( num_of_features ){
	int i;
	for ( i=0; i < num_of_features; i++ ){
	  delete Features[i];
	}
	delete [] Features;
	delete [] PermFeatures;
	delete Targets;
      }
    }
    else {
      InstanceBase->CleanPartition();
    }
    delete CurrInst;
    delete [] test_feature_val;
    delete [] Permutation;
    if ( BestArray ){
      int i;
      for ( i=0; i < BASize; i++ )
	delete BestArray[i];
      free( BestArray );
    }
    if ( ChoppedInput )
      delete [] ChoppedInput;
    delete [] UserOptions;
#ifndef USE_LOGSTREAMS
#else
    delete mylog;
    delete myerr;
    delete mydebug;
#endif
  }

#ifdef PTHREADS
  using SocketProcs::write_line;
  
  void MBLClass::Info( const string& out_line ) const {
    // Info NEVER to socket !
    if ( exp_name != "" )
      *Log(mylog) << "-" << exp_name << "-" << out_line << endl;
    else
      *Log(mylog) << out_line << endl;
 }
  
  void MBLClass::Warning( const string& out_line ) const {
    if ( Socket() )
      write_line( Socket(), "ERROR { " ) &&
	write_line( Socket(), out_line ) &&
	write_line( Socket(), " }\n" );
    else {
      if ( exp_name != "" )
	*Log(myerr) << "Warning:-" << exp_name << "-" << out_line << endl;
      else 
	*Log(myerr) << "Warning:" << out_line << endl;
    }
  }
  
  void MBLClass::Error( const string& out_line ) const {
    if ( Socket() )
      write_line( Socket(), "ERROR { " ) &&
	write_line( Socket(), out_line ) &&
	write_line( Socket(), " }\n" );
    else {
      if ( exp_name != "" )
	*Log(myerr) << "Error:-" << exp_name << "-" << out_line << endl;
      else
	*Log(myerr) << "Error:" << out_line << endl;
    }
    err_count++;
  }
  
  void MBLClass::FatalError( const string& out_line ) const {
    if ( Socket() )
      write_line( Socket(), "ERROR { " ) &&
	write_line( Socket(), out_line ) &&
	write_line( Socket(), " }\n" );
    else {
      if ( exp_name != "" )
	*Log(myerr) << "-" << exp_name << "-";
      *Log(myerr) << out_line << endl;
      if ( exp_name != "" )
	*Log(myerr) << "Error:-" << exp_name << "-" << out_line
		    << "stopped" << endl;
      else 
	*Log(myerr) << "Error:-" << out_line << "stopped" << endl;
      exit(1);
    }
  }
  
  bool MBLClass::ShowOptions( ostream& os ) const{
    bool result = true;
    if ( is_copy ){
      result = write_line( tcp_socket, "STATUS\n" );
    }
    else
      os << "Possible Experiment Settings (current value between []):" 
	 << endl;
    if ( result ){
#if __GNUC__ < 3
      ostrstream tmp;
      Options.Show_Options( tmp );
      tmp << ends;
      char *pnt = tmp.str();
      if ( is_copy )
	result = write_line( tcp_socket, pnt ) &&
	  write_line( tcp_socket, "ENDSTATUS\n" );
      else
	os << pnt << endl;
      delete [] pnt;
#else
      ostringstream tmp;
      Options.Show_Options( tmp );
      string tmp_s = tmp.str();
      if ( is_copy )
	result = write_line( tcp_socket, tmp_s ) &&
	  write_line( tcp_socket, "ENDSTATUS\n" );
      else
	os << tmp_s << endl;
#endif
    }
    return result;
  }
  
  bool MBLClass::ShowSettings( ostream& os ) const{
    bool result = true;
    if ( is_copy ){
      result = write_line( tcp_socket, "STATUS\n" );
    }
    else
      os << "Current Experiment Settings :" << endl;
    if ( result ){
#if __GNUC__ < 3
      ostrstream tmp;
      Options.Show_Settings( tmp );
      tmp << ends;
      char *pnt = tmp.str();
      if ( is_copy )
	result = write_line( tcp_socket, pnt ) &&
	  write_line( tcp_socket, "ENDSTATUS\n" );
      else
	os << pnt << endl;
      delete [] pnt;
#else
      ostringstream tmp;
      Options.Show_Settings( tmp );
      string tmp_s = tmp.str();
      if ( is_copy )
	result = write_line( tcp_socket, tmp_s ) &&
	  write_line( tcp_socket, "ENDSTATUS\n" );
      else
	os << tmp_s << endl;
#endif
    }
    return result;
  }
  
#else
  
  
  void MBLClass::Info( const string& out_line ) const {
    if ( exp_name != "" )
      *Log(mylog) << "-" << exp_name << "-" << out_line << endl;
    else
      *Log(mylog) << out_line << endl;
  }
  
  void MBLClass::Warning( const string& out_line ) const {
    if ( exp_name != "" )
      *Log(myerr) << "Warning:-" << exp_name << "-" << out_line << endl;
    else 
      *Log(myerr) << "Warning:" << out_line << endl;
  }
  
  void MBLClass::Error( const string& out_line ) const {
    if ( exp_name != "" )
      *Log(myerr) << "Error:-" << exp_name << "-" << out_line << endl;
    else
      *Log(myerr) << "Error:" << out_line << endl;
    err_count++;
  }
  
  void MBLClass::FatalError( const string& out_line ) const {
    if ( exp_name != "" )
      *Log(myerr) << "Error:-" << exp_name << "-" << out_line
		  << "stopped" << endl;
    else 
      *Log(myerr) << "Error:-" << out_line << "stopped" << endl;
    exit(1);
  }
  
  bool MBLClass::ShowOptions( ostream& os ) const {
    os << "Possible Experiment Settings (current value between []):" << endl;
    Options.Show_Options( os );
    os << endl;
    return true;
  }
 
  bool MBLClass::ShowSettings( ostream& os ) const {
    os << "Current Experiment Settings :" << endl;
    Options.Show_Settings( os );
    os << endl;
    return true;
  }
  
#endif // PTHREADS
  
  bool MBLClass::ShowWeights( ostream &os ) const {
    if ( ExpInvalid() )
      return false;
    else { 
      int OldPrec = os.precision(DBL_DIG);
      for ( int i=0; i< num_of_features; i++ ){
	os.precision(DBL_DIG);
	os << "Feature " << i+1 << "\t : " 
	   << Features[i]->Weight() << endl;
      }
      os.precision(OldPrec);
    }
    return true;
  }
  
  bool MBLClass::GetCurrentWeights( vector<double>& res ) {
    res.clear();
    if ( ExpInvalid() )
      return false;
    else { 
      Init_MBL_test( );
      for ( int i=0; i< num_of_features; i++ ){
	res.push_back( Features[i]->Weight() );
      }
    }
    return true;
  }
  
  bool MBLClass::PruneInstanceBase( void ){
    bool result = false;
    if ( !ExpInvalid() ){
      if ( !InstanceBase )
	Warning( "Unable to prune: No tree build yet?" );
      else {
	time_stamp( "Start Pruning:    " );
	InstanceBase->Prune( InstanceBase->TopTarget() );
	time_stamp( "Finished Pruning: " );
	if ( !Verbosity(SILENT) ) {
	  IBInfo( *Log(mylog) );
	}
	result = true;
      }
    }
    return result;
  }
  
  void MBLClass::calc_perm( double *W ){
    double *WR = new double[num_of_features];
    int i,j,k,m;
    for ( i=0; i< num_of_features; i++ ) 
      WR[i] = W[i];
    int IgnoredFeatures = 0;
    for ( j=0; j < num_of_features; j++ ){
      Permutation[j] = j;
      if ( Features[j]->Ignore() ){
	WR[j] = -0.1;         // To be shure that they are placed AFTER
	// those which are realy Zero
	IgnoredFeatures++;
      }
    }
    if ( IgnoredFeatures == num_of_features ){
      Error( "All features seem to be ignored! Nothing to do" );
    }
    else {
      for ( k=0; k < num_of_features; k++){
	int Max = 0;
	for ( m=1; m < num_of_features; m++ ){
	  if ( WR[m] > WR[Max] )
	    Max = m;
	}
	WR[Max] = -1;
	Permutation[k] = Max;
      }
    }
    delete [] WR;
  }
  
  void MBLClass::write_perm( ostream& os ) const {
    int j;
    os << "Feature Permutation based on " 
       << ( Weighting==UserDefined_w?"weightfile":to_string(TreeOrder, true))
       << " :" << endl << "< ";
    for ( j=0; j < num_of_features-1; j++ ){
      os << Permutation[j]+1 << ", ";
    }
    os << Permutation[j]+1 << " >" << endl;
  }
  
  inline void MBLClass::write_perm_special( ostream &os ){
    // write out the permutation and mark the last feature which is
    // NOT to be ignored with an exclamation mark, for instance: 
    // < 5, 2, 3! 1, 4 >
    int j;
    bool excl = false;
    os << "< ";
    for ( j=0; j < num_of_features-1; j++ ){
      if ( !excl && Features[Permutation[j+1]]->Ignore() ){
	excl = true;
	os << Permutation[j]+1 << "! ";
      }
      else
	os << Permutation[j]+1 << ", ";
    }
    os << Permutation[j]+1 << " >" << endl;
  }
  
  inline char *CurTime(){
    time_t lTime;
    struct tm *curtime;
    char *time_string;
    time(&lTime);
    curtime = localtime(&lTime);
    time_string = asctime(curtime);
    time_string[24] = '\0'; // defeat the newline!
    return time_string;
  }
  
  void MBLClass::time_stamp( const char *line, int number, bool nl ) const {
    if ( !Verbosity(SILENT) ){
      if ( nl )
	*Log(mylog) << endl;
#if __GNUC__ < 3
      ostrstream ostr;
      ostr << line;
      if ( number > -1 ){
	ostr.width(6);
	ostr.setf(ios::right, ios::adjustfield);
	ostr << number << " @ ";
      }
      else
	ostr << "        ";
      ostr << CurTime() << ends;
      char *pnt = ostr.str();
      Info( pnt );
      delete [] pnt;
#else
      ostringstream ostr;
      ostr << line;
      if ( number > -1 ){
	ostr.width(6);
	ostr.setf(ios::right, ios::adjustfield);
	ostr << number << " @ ";
      }
      else
	ostr << "        ";
      ostr << CurTime();
      Info( ostr.str() );
#endif
    }
  }
  
  void MBLClass::InitWeights(void){
    int i;
    for ( i=0; i< num_of_features; i++ ){
      if ( Features[i]->Ignore() )
	Features[i]->Weight( 0.0 );
      else
	switch ( Weighting ){
	case IG_w:
	  Features[i]->Weight( Features[i]->InfoGain() );
	  break;
	case GR_w:
	  Features[i]->Weight( Features[i]->GainRatio() );
	  break;
	case X2_w:
	  Features[i]->Weight( Features[i]->ChiSquare() );
	  break;
	case SV_w:
	  Features[i]->Weight( Features[i]->SharedVariance() );
	  break;
	case UserDefined_w:
	  break;
	case No_w:
	  Features[i]->Weight( 1.0 );
	  break;
	case Unknown_w:
	case Max_w:
	  FatalError( "InitWeights: Invalid Weight in switch: " +
		      to_string( Weighting ) );
	  break;
	}
    }
  }

  void MBLClass::default_order(){
    if ( TreeOrder == UnknownOrdening )
      switch ( Weighting ){
      case GR_w:
	TreeOrder = GROrder;
	break;
      case IG_w:
	TreeOrder = IGOrder;
	break;
      case X2_w:
	TreeOrder = X2Order;
	break;
      case SV_w:
	TreeOrder = SVOrder;
	break;
      case No_w:
	TreeOrder = NoOrder;
	break;
      case UserDefined_w:
	TreeOrder = GROrder;
	break;
      default:
	FatalError( "Illegal Weighting Value in Switch: " +
		    to_string( Weighting ) );
	break;
      }
  }
  
  void MBLClass::set_order(){
    calculate_fv_entropy(false);
    double *Order = new double[num_of_features];
    for ( int i=0; i < num_of_features; i++ )
      switch( TreeOrder ){
      case DataFile:
	Order[i] = Features[i]->Weight();
	break;
      case NoOrder:
	Order[i] = num_of_features-i;
	break;
      case IGOrder:
	Order[i] = Features[i]->InfoGain();
	break;
      case GROrder:
	Order[i] = Features[i]->GainRatio();
	break;
      case IGEntropyOrder:
	Order[i] = Features[i]->InfoGain() * Features[i]->SplitInfo();
	break;
      case GREntropyOrder:
	Order[i] = Features[i]->GainRatio() * Features[i]->SplitInfo();
	break;
      case X2Order:
	Order[i] = Features[i]->ChiSquare();
	break;
      case SVOrder:
	Order[i] = Features[i]->SharedVariance();
	break;
      case OneoverFeature:
	Order[i] =  1.0 / Features[i]->ArraySize();
	break;
      case GRoverFeature:
	Order[i] =  Features[i]->GainRatio() / Features[i]->ArraySize();
	break;
      case IGoverFeature:
	Order[i] =  Features[i]->InfoGain() / Features[i]->ArraySize();
	break;
      case X2overFeature:
	Order[i] =  Features[i]->ChiSquare() / Features[i]->ArraySize();
	break;
      case SVoverFeature:
	Order[i] =  Features[i]->SharedVariance() / Features[i]->ArraySize();
	break;
      case OneoverSplitInfo:
	Order[i] =  1.0 / Features[i]->SplitInfo();
	break;
      case UnknownOrdening:
      case MaxOrdening:
	FatalError( "Setorder: Illegal Order Value in Switch: " +
		    to_string( TreeOrder ) );
	break;
      }
    calc_perm( Order );
    delete [] Order;
    if ( !Verbosity(SILENT) )
      write_perm( *Log(mylog) );
    for ( int j=0; j < num_of_features; j++){
      if ( j < effective_feats )
	PermFeatures[j] = Features[Permutation[j]];
      else 
	PermFeatures[j] = NULL;
    }
  }
  
  bool MBLClass::IBAdd( const Instance *I ){ 
    MBL_init = false;
    return InstanceBase->AddInstance( I );
  }

   MBLClass::IB_Stat MBLClass::IBStatus() const {
    if (!InstanceBase ) return Invalid;
    else if (InstanceBase->IsPruned() ) return Pruned;
    else return Normal;
  }
  
  void MBLClass::IBInfo( ostream& os, InstanceBase_base *ib ) const {
    double Compres;
    unsigned long int CurSize;
    unsigned long int CurBytes = ib->GetSizeInfo( CurSize, Compres );
    ios::fmtflags OldFlg = os.setf( ios::fixed, ios::floatfield );
    int OldPrec = os.precision(2);
    os << "\nSize of InstanceBase = " << CurSize << " Nodes, (" << CurBytes
       << " bytes), " << Compres << " % compression" << endl << endl;
    os.precision( OldPrec );
    os.setf( OldFlg );
  }
  
  void MBLClass::MatrixInfo( ostream& os ) const {
    unsigned int TotalCount = 0;
    int f;
    for ( f = 0; f < num_of_features; f++ ){
      if ( !Features[f]->Ignore() &&
	   Features[f]->matrix_present() &&
	   ( Features[f]->Metric() == ValueDiff ||
	     Features[f]->Metric() == DefaultMetric && 
	     GlobalMetric == ValueDiff ) ||
	   ( Features[f]->Metric() == JeffreyDiv ||
	     Features[f]->Metric() == DefaultMetric && 
	     GlobalMetric == JeffreyDiv ) ){
	unsigned int Count = Features[f]->matrix_byte_size();
	os << "Size of value-matrix[" << f+1 << "] = " 
	   << Count << " Bytes " << endl;
	TotalCount += Count;
      }
    }
    if ( TotalCount )
      os << "Total Size of value-matrices " << TotalCount << " Bytes " 
	 << endl << endl;
  }
  
  bool MBLClass::ReadArrays( istream& is ){
    bool result = true;
    int num, index = 1;
    string buf;
    char kar;

    do {
      is >> ws >> buf;
      if ( compare_nocase_n( "feature", buf ) ){
	is >> ws >> kar; // skip #
	if ( kar != '#' ){
	  Error( "Input out-of-sync, a '#' was expected" );
	  result = false; 
	} 
	else {
	  is >> num;
	  if ( num != index ){
	    Error( "Wrong feature number " + to_string(num) +
		   " in file, " + to_string(index) + " expected" );
	    result = false;
	  }
	  else if ( index > num_of_features ){
	    Error( "Too many features matrices in this file " );
	    result = false;
	  }
	  else {
	    is >> ws >> buf;
	    if ( compare_nocase_n( "Ignored", buf ) ){
	      if ( Features[index-1]->Ignore() ){
		++index;
		continue;
	      }
	      else {
		Error( "Feature #" + to_string(index) + 
		       " may not be ignored...");
		result = false;
	      }
	    }
	    else if ( compare_nocase_n( "Numeric", buf ) ){
	      if ( Features[index-1]->Numeric() ){
		++index;
		continue;
	      }
	      else {
		Error( "Feature #" + to_string(index) + " is not Numeric..." );
		result = false;
	      }
	    }
	    else if ( !compare_nocase_n( "Matrix", buf ) ){
	      Error( "Problem in Probability file, missing matrix info" );
	      result = false;
	    }
	    else if ( Features[index-1]->Ignore() ||
		      Features[index-1]->Numeric() ){
	      Warning( "Matrix info found for feature #" + to_string(index) +
		       " (skipped)" );
	      ++index;
	    }
	    else {
#if __GNUC__ < 3
	      is.ignore( INT_MAX, '\n' );
#else
	      is.ignore( std::numeric_limits<std::streamsize>::max(), '\n' );
#endif
	      result = Features[index-1]->read_vc_pb_array( is );
	      ++index;
	    }
	  }
	}
      }
    }
    while ( result && !is.eof() & !is.bad() );
    if ( index < num_of_features+1 ){
      Error( "Not enough features matrices in this file " );
      result = false;
    }
    return result;
  }
  
  
  bool MBLClass::WriteArrays( ostream& os ) {
    if ( ExpInvalid() )
      return false;
    else if ( !pre_divide1( false ) ){
      Warning( "couldn't Calculate probability Arrays's" );
      return false;
    }
    else {
      // Print the possible classes.
      //
      TargetValue *tv;
      os << "Targets : ";
      int t;
      for ( t=0; t < Targets->ArraySize()-1; t++ ) {
	tv = Targets->Value(t);
	os << tv << ", ";
      } 
      tv = Targets->Value(Targets->ArraySize()-1);
      os << tv << "." << endl << endl;
      int i;
      for ( i = 0; i < num_of_features; i++ )
	if ( Features[i]->Ignore() )
	  os << "feature # " << i+1 << " Ignored, (-s option)" << endl;
	else if (Features[i]->Numeric() )
	  os << "feature # " << i+1 << " Numeric, (-N option)" << endl;
	else {
	  os << "feature # " << i+1 << " Matrix: " << endl;
	  Features[i]->print_vc_pb_array( os );
	  os << endl;
	}
      return true;
    }
  }
  
  bool MBLClass::allocate_arrays(){
    int Dim = Targets->ArraySize();
    int j;
    bool result = true;
    for ( j = 0; result && j < num_of_features; j++) {
      if ( !Features[j]->Ignore() &&
	   !Features[j]->Numeric() ) {
	result = Features[j]->AllocSparseArrays( Dim );
      }
    } // j
    return true;
  }
  
  bool MBLClass::pre_divide1( bool force ){
    bool result = true;
    result = allocate_arrays();
    if ( result ){
      for ( int j = 0; j < num_of_features; j++) {
	if ( !Features[j]->Ignore() &&
	     !Features[j]->Numeric() ){
	  Features[j]->ClipFreq( (int)rint(clip_factor * 
					   log((double)Features[j]->EffectiveValues())));
	  if ( force ||
	       !Features[j]->ArrayRead() &&
	       ( ( Features[j]->Metric() == ValueDiff ||
		   (Features[j]->Metric() == DefaultMetric && 
		    GlobalMetric == ValueDiff) ) ||
		 ( Features[j]->Metric() == JeffreyDiv ||
		   (Features[j]->Metric() == DefaultMetric && 
		    GlobalMetric == JeffreyDiv ) ) ) ){
	    Features[j]->InitSparseArrays();
	  } // if force
	} //if !Ignore
      } // j
    }  
    return result;
  }
  
  /*
    For mvd metric.
  */
  bool MBLClass::pre_divide2( bool force ){
    bool result = pre_divide1( force );
    if ( result ){
      int i,j;
      for ( j = tribl_offset; j < effective_feats; j++) {
	if ( PermFeatures[j]->Metric() == ValueDiff ||
	     (PermFeatures[j]->Metric() == DefaultMetric && 
	      GlobalMetric == ValueDiff) ){
	  PermFeatures[j]->store_matrix( ValueDiff, mvd_treshold );
	}
	else if ( PermFeatures[j]->Metric() == JeffreyDiv ||
		  (PermFeatures[j]->Metric() == DefaultMetric && 
		   GlobalMetric == JeffreyDiv) ){
	  PermFeatures[j]->store_matrix( JeffreyDiv, mvd_treshold );
	}
      } // j
      if ( Verbosity(VD_MATRIX) ) 
	for ( i = 0; i < num_of_features; i++ )
	  if ( !Features[i]->Ignore() ){
	    if (Features[i]->matrix_present( ) ){
	      *Log(mylog) << "Value matrix of feature # " 
			  << i+1 << endl;
	      Features[i]->print_matrix();
	      *Log(mylog) << endl;
	    }
	    else {
	      *Log(mylog) << "Value Difference matrix of feature # " 
			  << i+1 << endl << "Not avaliable." << endl;
	    }
	  }
    }
    return result;
  }

  bool MBLClass::chop_C4_5_string( const string& InBuf, 
				   string OutBuf[], int Len ){
    // Function that takes a line InBuf, and chops it up into substrings,
    // which represent the feature-values and the target-value.
    vector<string> splits;
    int res = split_at( InBuf, splits, "," );
    if ( res != Len + 1 )
      return false;
    for( int i=0; i < Len ; ++i ){
       OutBuf[i] = StrToCode( compress(splits[i]) );
    }
    OutBuf[Len] = compress( splits[Len] ); 
    return true;
  }

  bool MBLClass::chop_ARFF_string( const string&InBuf,
				   string OutBuf[], int Len ){
    // Function that takes a line InBuf, and chops it up into substrings,
    // the feature-values and the target-value.
    
    // Lines look like this:
    // one, two,   three , bla.
    // the termination dot is optional
    // WhiteSpace is skipped!
    return chop_C4_5_string( compress( InBuf ), OutBuf, Len );
  }
  
  bool MBLClass::chop_bin_string( const string&InBuf,
				  string OutBuf[], int Len ){
    // Function that takes a line InBuf, and chops it up into substrings,
    // which represent the feature-values and the target-value.
    // Lines look like this:
    // 12, 25, 333, bla.
    // the termination dot is optional
    int m;
    for ( m = 0; m < Len; ++m )
      OutBuf[m] = "0";
    string::size_type s_pos = 0;
    string::size_type e_pos = InBuf.find( ',' );
    while ( e_pos != string::npos ){
      string tmp = string( InBuf, s_pos, e_pos - s_pos );
      int k = stoi( tmp );
      if ( k < 1 || k > Len )
	return false;
      else
	OutBuf[k-1] = "1";
      s_pos = e_pos + 1;
      e_pos = InBuf.find( ',', s_pos );
    }
    OutBuf[Len] = string( InBuf, s_pos );
    return true;
  }
  
  bool MBLClass::chop_Compact_string( const string& InBuf, 
				      string OutBuf[], int Len ){
    int i, j, index, len;
    // Lines look like this:
    // ====AKBVAK
    // v1v2v3v4tt
    // Get & add the target.
    //
    len = InBuf.length();
    if ( len != (Len+1) * F_length ){
      return false;
    }
    for ( i = 0; i <= Len; ++i ) {
      index = i * F_length; 
      // Scan the value.
      //
      OutBuf[i] = "";
      for (j = 0; j < F_length; ++j ) {
	OutBuf[i] += InBuf[index++];
      }
    }
    return ( i == Len+1 ); // Enough?
  }
  
  bool MBLClass::chop_COLUMNS_string( const string& InBuf,
				      string OutBuf[], int Len ){
    // Function that takes a line InBuf, and chops it up into substrings,
    // which represent the feature-values and the target-value.
    
    // Lines look like this:
    // one  two three bla
    int i = 0;
    string::size_type s_pos = 0;
    string::size_type e_pos = InBuf.find_first_of( " \t" );
    while ( e_pos != s_pos && e_pos != string::npos && i <= Len+1 ){
      // stop if a zero length string is found or if too many entries show up
      OutBuf[i++] = string( InBuf, s_pos, e_pos - s_pos );
      s_pos = InBuf.find_first_not_of( " \t", e_pos );
      e_pos = InBuf.find_first_of( " \t", s_pos );
    }
    if ( s_pos != string::npos && i <= Len+1 ){
      OutBuf[i++] = string( InBuf, s_pos );
    }
    return ( i == Len+1 ); // Enough?
  }
  
  bool MBLClass::chop_sparse_string( const string& InBuf, 
				     string OutBuf[], int Len ){
    // Function that takes a line InBuf, and chops it up into substrings,
    // which represent the feature-values and the target-value.
    // Lines look like this:
    // (12,value1) (25,value2) (333,value3) bla.
    // the termination dot is optional

    int k = 0;
    for ( int m = 0; m < Len; m++ )
      OutBuf[m] = DefaultSparseString;
    OutBuf[Len] = "";
    string::size_type s_pos = InBuf.find( "(" );
    if ( s_pos == string::npos )
      OutBuf[Len] = compress(InBuf);
    else {
      string::size_type m_pos, e_pos = InBuf.find( ")" );
      while ( s_pos < e_pos &&
	      s_pos != string::npos  && e_pos != string::npos ){
	m_pos = InBuf.find( ',', s_pos );
	string temp = string( InBuf, s_pos + 1, m_pos - s_pos - 1 );
	k = stoi( temp );
	if ( k < 1 || k > Len ){
	  return false;
	}
	else {
	  OutBuf[k-1] = string( InBuf, m_pos + 1, e_pos - m_pos -1 );
	  OutBuf[k-1] = StrToCode( compress(OutBuf[k-1]) );
	}
	s_pos = InBuf.find( '(', e_pos );
	if ( s_pos == string::npos ){
	  e_pos = InBuf.find_first_not_of( ") \t", e_pos );
	  if ( e_pos != string::npos ){
	    OutBuf[Len] = string( InBuf, e_pos );
	    OutBuf[Len] = compress( OutBuf[Len] );
	  }
	}
	else
	  e_pos = InBuf.find( ')', s_pos );
      }
    }
    return !OutBuf[Len].empty();
  }
  
  const Instance *MBLClass::chopped_to_instance( PhaseType Phase ){
    switch ( Phase  ){
    case LearnWords:
      // Add the target.
      CurrInst->TV = Targets->add_value( ChoppedInput[num_of_features] );
      // Now add the Feature values.
      int i;
      for ( i = 0; i < num_of_features; i++ ){
	// when learning, no need to bother about Permutation
	if ( Features[i]->Ignore() ) // but this might happen, take care!
	  CurrInst->FV[i] = NULL;
	else
	  // Add it to the Instance.
	  CurrInst->FV[i] = Features[i]->add_value( ChoppedInput[i],
						    CurrInst->TV ); 
      } // i
      break;
    case TrainWords:
      // Lookup for TreeBuilding
      // First the Features
      int k;
      for ( k = 0; k < effective_feats; k++ ){
	register int j = Permutation[k];
	CurrInst->FV[k] = Features[j]->Lookup( ChoppedInput[j] );
      } // k
      // and the Target
      CurrInst->TV = Targets->Lookup( ChoppedInput[num_of_features] );
      break;
    case TrainLearnWords:
      // Lookup for Incremental TreeBuilding
      // Assumes that somehow Permutation and effective_feats are known
      // First the Target
      CurrInst->TV = Targets->add_value( ChoppedInput[num_of_features] );
      // Then the Features
      int l;
      for ( l = 0; l < effective_feats; l++ ){
	register int j = Permutation[l];
	CurrInst->FV[l] = Features[j]->add_value( ChoppedInput[j],
						  CurrInst->TV ); 
      } // k
      break;
    case TestWords:
      // Lookup for Testing
      // This might fail for numeric features, then we Create a dummy value
      int m;
      for ( m = 0; m < effective_feats; m++ ){
	register int j = Permutation[m];
	if ( CurrInst->FV[m] && CurrInst->FV[m]->Index() == -1 )
	  delete CurrInst->FV[m];
	CurrInst->FV[m] = Features[j]->Lookup( ChoppedInput[j] );
	if ( !CurrInst->FV[m] && 
	     Features[j]->Numeric() )
	  // For numeric features "unknown" values don't exist so
	  // we have to add a dummy value
	  CurrInst->FV[m] = new FeatureValue( ChoppedInput[j] );
      } // i
      // the last string is the target
      CurrInst->TV = Targets->Lookup( ChoppedInput[num_of_features] );
      break;
    default:
      FatalError( "Wrong value in Switch: " + to_string(Phase) );
    }
    if ( ( Phase != TestWords ) && 
	 do_sample_weighting && !do_ignore_samples ){
      double tmp;
      if ( string2double( ChoppedInput[num_of_features+1], tmp ) )
	CurrInst->ExemplarWeight( tmp );
      else 
	CurrInst->ExemplarWeight( 1.0 );
    }
    return CurrInst;
  }
  
  bool empty_line( const string& Line, const InputFormatType IF ){
    // determine wether Line is empty or a commentline
    bool result = ( Line.empty() || 
		    ( IF == ARFF &&  // ARFF "comment"
		      ( Line[0] == '%' || Line[0] == '@' ) ) ||
		    ( Line.find_first_not_of( " \t" ) == string::npos ) );
    return result;
  }
  
  void MBLClass::show_org_input( ostream& out ) const {
    int i;
    for ( i = 0; i <= num_of_features; i++) {
      switch ( input_format) {
      case C4_5:
      case ARFF:
	out << CodeToStr(ChoppedInput[i]) << ",";
      break;
      case Columns:
	out << ChoppedInput[i] << " ";
	break;
      case SparseBin:
	if ( i == num_of_features )
	  out << ChoppedInput[i] << ",";
	else if ( ChoppedInput[i][0] == '1' )
	  out << i+1 << ",";
	break;
      case Sparse:
	if ( i == num_of_features )
	  out << ChoppedInput[i] << ",";
	else if ( ChoppedInput[i] != DefaultSparseString )
	  out << "(" << i+1 << "," << CodeToStr(ChoppedInput[i]) << ")";
	break;
      default:
	out << ChoppedInput[i];
	break;
      }
    }
  }
  
  void MBLClass::LearningInfo( ostream& os ) {
    if ( !ExpInvalid() ){
      int i;
      calculate_fv_entropy( !MBL_init );
      os.setf(ios::showpoint );
      int OldPrec = os.precision(8);
      os << "DB Entropy        : " << DBEntropy << endl;
      os << "Number of Classes : " << Targets->EffectiveValues() << endl;
      os << endl;
      if ( Verbosity(FEAT_W) ){
	if (  need_all_weights ){
	  os << "Feats\tVals\tX-square\tVariance\tInfoGain\tGainRatio" << endl;
	  for ( i = 0; i < num_of_features; i++) {
	    os << setw(5) << i+1;
	    os.setf(ios::right, ios::adjustfield);
	    if ( Features[i]->Ignore() ){
	      os << " (ignored) " << endl;
	    }
	    else {
	      os.setf(ios::right, ios::adjustfield);
	      os << setw(7) << Features[i]->EffectiveValues()
		 << "\t" << Features[i]->ChiSquare()
		 << "\t" << Features[i]->SharedVariance()
		 << "\t" << Features[i]->InfoGain()
		 << "\t" << Features[i]->GainRatio();
	      if ( Features[i]->Numeric() )
		os << " NUMERIC";
	      os << endl;
	    }
	  }
	  os << endl;
	  os.precision(OldPrec);
	}
	else {
	  os << "Feats\tVals\tInfoGain\tGainRatio" << endl;
	  for ( i = 0; i < num_of_features; i++) {
	    os << setw(5) << i+1;
	    os.setf(ios::right, ios::adjustfield);
	    if ( Features[i]->Ignore() ){
	      os << " (ignored) " << endl;
	    }
	    else {
	      os.setf(ios::right, ios::adjustfield);
	      os << setw(7) << Features[i]->EffectiveValues()
		 << "\t" << Features[i]->InfoGain()
		 << "\t" << Features[i]->GainRatio();
	      if ( Features[i]->Numeric() )
		os << " NUMERIC";
	      os << endl;
	    }
	  }
	  os << endl;
	  os.precision(OldPrec);
	}
      }
    }
  }
  
  bool MBLClass::WriteWeights( ostream& os ) {
    bool result = false;
    if ( !ExpInvalid() ){
      if ( Features == NULL ){
	Warning( "unable to save Weights, nothing learned yet" );
      }
      else {
	os << "# DB Entropy: " << DBEntropy << endl;
	os << "# Classes: " << Targets->ArraySize() << endl;
	os << "# Lines of data: " << Targets->TotalValues() << endl;
	int OldPrec = os.precision(DBL_DIG);
	os << "# " << to_string( No_w ) << endl;
	os << "# Fea." << "\t" << "Weight" << endl;
	for (  int i = 0; i < num_of_features; i++) {
	  os.precision(DBL_DIG);
	  if ( Features[i]->Ignore() )
	    os << i+1 << "\t" << "Ignore" << endl;
	  else
	    os << i+1 << "\t" << 1.0 << endl;
	}
	os << "#" << endl;
	os << "# " << to_string( GR_w ) << endl;
	os << "# Fea." << "\t" << "Weight" << endl;
	for (  int i = 0; i < num_of_features; i++) {
	  os.precision(DBL_DIG);
	  if ( Features[i]->Ignore() )
	    os << i+1 << "\t" << "Ignore" << endl;
	  else
	    os << i+1 << "\t" << Features[i]->GainRatio() << endl;
	}
	os << "#" << endl;
	os << "# " << to_string( IG_w ) << endl;
	os << "# Fea." << "\t" << "Weight" << endl;
	for (  int i = 0; i < num_of_features; i++) {
	  os.precision(DBL_DIG);
	  if ( Features[i]->Ignore() )
	    os << i+1 << "\t" << "Ignore" << endl;
	  else
	    os << i+1 << "\t" << Features[i]->InfoGain() << endl;
	}
	if ( need_all_weights ){
	  os << "#" << endl;
	  os << "# " << to_string( SV_w ) << endl;
	  os << "# Fea." << "\t" << "Weight" << endl;
	  for (  int i = 0; i < num_of_features; i++) {
	    os.precision(DBL_DIG);
	    if ( Features[i]->Ignore() )
	      os << i+1 << "\t" << "Ignore" << endl;
	    else
	      os << i+1 << "\t" << Features[i]->SharedVariance() << endl;
	  }
	  os << "#" << endl;
	  os << "# " << to_string( X2_w ) << endl;
	  os << "# Fea." << "\t" << "Weight" << endl;
	  for (  int i = 0; i < num_of_features; i++) {
	    os.precision(DBL_DIG);
	    if ( Features[i]->Ignore() )
	      os << i+1 << "\t" << "Ignore" << endl;
	    else
	      os << i+1 << "\t" << Features[i]->ChiSquare() << endl;
	  }
	  os << "#" << endl;
	  os.precision(OldPrec);
	}
	result = true;
      }
    }
    return result;
  }
  
  bool MBLClass::read_the_vals( istream& is ){
    bool result = true;
    bool *done = new bool[num_of_features];
    for ( int i=0; i < num_of_features; i++ )
      done[i] = false;
    string Buffer;
    while ( getline( is, Buffer) ){
      if ( !Buffer.empty() ){
	if ( Buffer[0] == '#'){
	  break;
	}
	// Line looks like:
	// 28      0.445481
	// or:
	// 13      Ignore
	//
	vector<string> vals;
	if ( split( Buffer, vals ) == 2 ){
	  int i_f = stoi(vals[0] );
	  if ( i_f > num_of_features ){
	    Error( "in weightsfile, Feature index > Maximum, (" +
		   to_string(num_of_features) + ")" );
	  }
	  else if ( done[i_f-1] ){
	    Error( "in weightsfile, Feature index " + to_string(i_f) +
		   " is mentioned twice" );
	  }
	  else {
	    done[i_f-1] = true;
	    if ( !compare_nocase( vals[1], "Ignore" ) ){
	      double w;
	      if ( !string2double( vals[1], w  ) ){
		Error( "in weightsfile, Feature " + to_string(i_f) +
		       " has illegal value: " + vals[1] );
	      }
	      else {
		Features[i_f-1]->Weight( w );
		if ( Features[i_f-1]->Ignore() )
		  Warning( "in weightsfile, "
			   "Feature " + to_string(i_f) + " has value: " +
			   to_string( w ) +
			   " assigned, but will be ignored" );
	      }
	    }
	    else {
	      Features[i_f-1]->Weight( 0.0 );
	      if ( !Features[i_f-1]->Ignore() )
		Warning( "in weightsfile, Feature " + to_string(i_f) +
			 " has value: 'Ignore', we will use: 0.0 " );
	    }
	  }
	}
      }
    }
    if ( result ){
      for ( int j=0; j < num_of_features; j++ )
	if ( !done[j] ) {
	  Error( "in weightsfile, Feature index " + to_string(j+1) +
		 " is not mentioned" );
	  result = false;
	}
    }
    delete [] done;
    return result;
  }

  bool MBLClass::ReadWeights( istream& is, WeightType wanted ){
    set<WeightType> ret_weights;
    bool result = false;
    bool old_style = true;
    if ( !ExpInvalid() ){
      string Buffer;
      while( getline( is, Buffer ) ) {
	// A comment starts with '#'
	//
	if ( Buffer.empty() )
	  continue;
	else {
	  if ( Buffer[0] == '#'){
	    vector<string> vals;
	    if ( split_at( Buffer, vals, " " ) == 2  ){
	      WeightType tmp_w;
	      if ( !string_to( vals[1], tmp_w ) )
		continue;
	      else {
		old_style = false;
		if ( tmp_w == wanted ){
		  getline( is, Buffer );
		  result = read_the_vals( is );
		  break;
		}
	      }
	    }
	  }
	}
      }
      if ( is.eof() ){
	if ( old_style ){
	  // wanted weighting not found
	  // Old style weightsfile?
	  //	  Warning( "Old Style weightsfile. Please update" );
	  is.clear();
	  is.seekg(0);
	  unsigned pos = 0;
	  while( getline( is, Buffer ) ) {
	    // A comment starts with '#'
	    //
	    if ( Buffer.empty() ){
	      pos = is.tellg();
	      continue;
	    }
	    else {
	      if ( Buffer[0] == '#'){
		pos = is.tellg();
		continue;
	      }
	      is.seekg(pos);
	      result = read_the_vals( is );
	      break;
	    }
	  }
	}
      }
      if ( !result ){
	Warning( "Unable to retrieve " + to_string( wanted ) + " Weights" );
	Warning( "Using 1.0 for all not ignored values" );
	for ( int j=0; j < num_of_features; ++j ){
	  if ( Features[j]->Ignore() )
	    Features[j]->Weight( 0.0 );
	  else
	    Features[j]->Weight( 1.0 );
	}
      }
      // make shure all weights are correct
      // Paranoid?
      for ( int i=0; i< num_of_features; i++ ){
	Features[i]->InfoGain( Features[i]->Weight() );
	Features[i]->GainRatio( Features[i]->Weight() );
	Features[i]->ChiSquare( Features[i]->Weight() );
	Features[i]->SharedVariance( Features[i]->Weight() );
      }
      Weighting = UserDefined_w;
    }
    return true;
  }
  
  bool MBLClass::get_IB_Info( istream& is,
			      bool& Pruned,
			      int& Version,
			      bool& Hashed,
			      string& range_buf ){
    if ( ExpInvalid() )
      return false;
    if ( Options.TableFrozen() ||
	 num_of_features != 0 ){
      Warning( "unable to read an Instance Base while another"
	       " experiment is already loaded" );
      return false;
    }
    
    bool info_ok = true;
    bool more = true;
    int depth = 0;
    int version = -1;
    Hashed = false;
    range_buf = "";
    string buffer;
    vector<string> splits;
    more = ( look_ahead(is) == '#' &&
	     getline( is, buffer ) );
    while ( info_ok && more ){
      int num = split( buffer, splits );
      if ( num > 2 ){
	if ( compare_nocase_n( "Status:", splits[1] ) ){
	  version = 2;
	  if ( splits[2] == "pruned" )
	    Pruned = true;
	  else if ( splits[2] == "complete" )
	    Pruned = false;
	  else {
	    Error( "Unknown Status Information in Instance-Base file." );
	    info_ok = false;
	  }
	}
	else if ( compare_nocase_n( "Algorithm:", splits[1] ) ) {
	  version = 1;
	  if ( compare_nocase( splits[2], "IG-tree" ) )
	    Pruned = true;
	  else if ( compare_nocase( splits[2], "MBL" ) )
	    Pruned = false;
	  else {
	    Error( "Unknown Algorithm Information in Instance-Base file." );
	    info_ok = false;
	  }
	}
	else if ( compare_nocase_n( "Permutation:", splits[1] ) ){
	  if ( splits[2][0] != '<' ){
	    Error( "missing `<` while reading permutation" );
	    info_ok = false;
	  }
	  else {
	    string perms;
	    for ( int i=2; i < num; ++i )
	      perms = perms + splits[i]; // HACK. We could use splits directly
	    bool excl = false;
	    for ( int j=0; j < MaxFeatures; ++j )
	      Permutation[j] = j;
	    effective_feats = 0;
	    int i = 0;
	    int index;
	    string::size_type pos = 1; // skip <
	    while ( info_ok && pos != string::npos &&
		    i < MaxFeatures ){
	      if ( !excl )
		effective_feats++;
	      string tmp = string_tok( perms, pos, ">, !" );
	      if ( pos == string::npos ){
		info_ok = false;
		break; // trouble
	      }
	      index = stoi( tmp );
	      Permutation[i++] = --index;
	      if ( index < 0 || index >= MaxFeatures ){
		Error ( "illegal value " + to_string(index) + 
			" in permutation, not between 1 and " +
			to_string( MaxFeatures ) );
		info_ok = false;
		break;
	      }
	      if ( excl )
		UserOptions[index+1] = Ignore;
	      while ( isspace(perms[pos]) ) ++pos;
	      switch ( perms[pos] ){
	      case '>':
		pos = string::npos;
		break;
	      case ',':
		++pos;
		break;
	      case '!':
		++pos;
		excl = true;
		break;
	      default:
		Error ( "missing `,` while reading permutation" );
		info_ok = false;
	      }
	    }
	    if ( info_ok )
	      if ( pos != string::npos || i > MaxFeatures ){
		Error( "missing `>` while reading permutation" );
		info_ok = false;
	      }
	      else
		depth = i;
	  }
	}
	else if ( compare_nocase_n( "Numeric:", splits[1] ) ){
	  if ( splits[2][0] != '.' ){
	    string::size_type pos = 0;
	    while ( pos != string::npos ){
	      string tmp = string_tok( splits[2], pos, ",. " );
	      if ( tmp != "" ){
		int k = stoi( tmp );
		UserOptions[k] = Numeric;
	      }
	    }
	    getline( is, range_buf );
	  }
	}
	else if ( compare_nocase_n( "Bin_Size:", splits[1] ) ){
	  int siz = stoi( splits[2] );
	  if ( siz < 2 || siz > 1000000 ){
	    Error( "invalid Bin_Size found: " + splits[2] );
	    info_ok = false;
	  }
	  else
	    Bin_Size = siz;
	}
	else if ( compare_nocase_n( "Version", splits[1] ) ){
	  version = stoi( splits[2] );
	  if ( version >= 3  && num > 3 ){
	    if ( compare_nocase_n( "(Hashed)", splits[3] ) )
	      Hashed = true;
	  }
	}
      }
      more = ( look_ahead(is) == '#' && 
	       getline( is, buffer ) );
    }
    if ( version < 0 ) {
      Error( "missing Version information in Instance-Base file" );
      info_ok = false;
    }
    else if ( version < 4 ) {
      Error( "A Version " + to_string(version) + 
	     " type InstanceBase file is found:\n"
	     "        You should recreate it as it is no longer supported"
	     "\n        in this version of the Timbl package" );
    }
    Version = version;
    if ( info_ok ){
      num_of_features = depth;
      return true;
    }
    else {
      num_of_features = 0;
      return false;
    }
#if __GNUC__ < 3
    // stupid compiler...
    return false;
#endif
  }
  
  bool MBLClass::get_ranges( const string& rangeline ){
    if ( NumNumFeatures() == 0 )
      return true;
#if __GNUC__ < 3
    istrstream is( rangeline.c_str(), rangeline.length()+1 );
#else
    istringstream is( rangeline );
#endif
    string buf;
    char kar;
    bool result = false;
    is >> kar; // skip #
    is >> ws >> buf;
    if ( !compare_nocase_n( "Ranges:", buf ) )
      Error( "missing Ranges line in Instance-Base file" );
    else {
      is >> ws;
      int k;
      if ( look_ahead(is) == '.' ){
	result = true;
      }
      else {
	do {
	  is >> k; 
	  if ( UserOptions[k] != Numeric ){
	    Error( "Found range info for feature " + to_string(k) +
		   ", which is Not defined as Numeric!" );
	    result = false;
	  }
	  else {
	    is >> ws >> buf;
	    double min, max;
	    int scancount = sscanf( buf.c_str(), "[%lf-%lf]", &min, &max );
	    if ( scancount == 2 ){
	      Features[k-1]->Min( min );
	      Features[k-1]->Max( max );
	      if ( is ){
		is >> ws >> buf;
		if ( !buf.empty() && (buf[0] == '.' || buf[0] == ',' ) )
		  result = true;
		else
		  result = false;
	      }
	      else {
		buf = ".";
		result = true;
	      }
	    }
	    else
	      result = false;
	  }
	} while ( result && buf[0] != '.' );
      }
    }
    return result;
  }
  
  bool MBLClass::PutInstanceBase( ostream& os ){
    bool result = true;
    if ( ExpInvalid() ){
      result = false;
    }
    else if ( InstanceBase == NULL ){
      Warning( "unable to write an Instance Base, nothing learned yet" );
    }
    else {
      os << "# Status: " 
	      << (InstanceBase->IsPruned()?"pruned":"complete") << endl;
      os << "# Permutation: "; 
      write_perm_special( os );
      os << "# Numeric: "; 
      bool first = true;
      int i;
      for ( i=0; i < num_of_features; i++ )
	if ( !Features[i]->Ignore() &&
	     Features[i]->Numeric() ){
	  if ( !first )
	    os << ", ";
	  else
	    first = false;
	  os << i+1;
	}
      os << '.' << endl;
      if ( NumNumFeatures() > 0 ){
	os << "# Ranges: "; 
	first = true;
	int j;
	for ( j=0; j < num_of_features; j++ )
	  if ( !Features[j]->Ignore() &&
	       Features[j]->Numeric() ){
	    if ( !first )
	      os << " , ";
	    else
	      first = false;
	    os << j+1 << " [" << Features[j]->Min() 
		    << "-" << Features[j]->Max() << "]";
	  }
	os << " ." << endl;
      }
      os << "# Bin_Size: " << Bin_Size << endl;
      if ( hashed_trees ){
	InstanceBase->Save( os,
			    TargetStrings, FeatureStrings, 
			    keep_distributions );
      }
      else
	InstanceBase->Save( os, keep_distributions );
    }
    return result;
  }
  
  void MBLClass::calculate_fv_entropy( bool always ){
    bool realy_first =  DBEntropy == 0.0;
    bool nothing_changed = true;
    if ( always || realy_first ){
      // if it's the first time (DBEntropy == 0 ) or
      // if always, we have to (re)calculate everything
      double Entropy = 0.0, Ratio;
      // first get the Database Entropy
      int i;
      for ( i=0; i < Targets->ArraySize(); i++ ) {
	Ratio = Targets->Value(i)->ValFreq() / 
	  (double)Targets->TotalValues();
	if ( Ratio > 0 )
	  Entropy += Ratio * Log2(Ratio);
      }
      DBEntropy = fabs(-Entropy);
      allocate_arrays(); // create ValueClassProb arrays..
    }
    // Loop over the Features, see if the numerics are non-singular
    // and do the statistics for those features where the metric is changed.
    MetricType TmpMetric;
    FeatVal_Stat *feat_status = new FeatVal_Stat[MaxFeatures];
    for ( int g = 0; g < num_of_features; g++) {
      feat_status[g] = Unknown;
      if ( Features[g]->Ignore() )
	continue;
      TmpMetric = UserOptions[g+1];
      if ( Features[g]->Numeric() ){
	feat_status[g] = Features[g]->prepare_numeric_stats();
	if ( feat_status[g] == SingletonNumeric &&
	     ( input_format == SparseBin && do_dot_product ) ){
	  // ok
	}
	else {
	  if ( feat_status[g] != NumericValue ){
	    Features[g]->Numeric( false );
	    nothing_changed = false;
	    if ( GlobalMetric == Numeric )
	      TmpMetric = Overlap;
	    else
	      TmpMetric = DefaultMetric;
	    Features[g]->Metric(TmpMetric);
	  }
	  else
	    TmpMetric = Numeric;
	} 
      }
      else if ( Features[g]->TotalValues() == 1 )
	feat_status[g] = Singleton;
      if ( always || realy_first ){
	if ( Weighting != UserDefined_w ){
	  if ( TmpMetric == Numeric ){
	    Features[g]->NumStatistics( DBEntropy, Targets, Bin_Size,
					need_all_weights );
	  }
	  else {
	    Features[g]->Statistics( DBEntropy, Targets,
				     need_all_weights );
	  }
	}
	if ( TmpMetric != Numeric && 
	     !( TmpMetric == DefaultMetric && GlobalMetric == Numeric ) &&
	     TmpMetric !=  Features[g]->Metric() ){
	  nothing_changed = false;
	  Features[g]->Metric(TmpMetric);
	}
      }
    } // end g
    if ( do_dot_product && !nothing_changed ){
      // check to see if ALL features are still Numeric.
      // otherwise we can't do Inner product!
      bool first = true;
#if __GNUC__ < 3
      ostrstream ostr1;
      ostrstream ostr2;
#else
      ostringstream ostr1;
      ostringstream ostr2;
#endif
      for ( int ff = 0; ff < num_of_features; ff++ ) 
	if ( feat_status[ff] == Singleton ||
	     feat_status[ff] == SingletonNumeric ){
	  if ( first ){
	    ostr1 << "The following feature(s) have only 1 value: ";
	    first = false;
	  }
	  else
	    ostr1 << ", ";
	  int n = ff;
	  while ( ff < num_of_features-1 && 
		  ( feat_status[ff+1] == Singleton ||
		    feat_status[ff+1] == SingletonNumeric ) )
	    ff++;
	  if ( n != ff ){
	    ostr1 << n+1 << "-" << ff+1;
	  }
	  else
	    ostr1 << ff+1;
	}
      if ( !first  ){
#if __GNUC__ < 3
	ostr1 << ends;
	char *pnt = ostr1.str();
	Error( pnt );
	delete [] pnt;
#else
	Error( ostr1.str() );
#endif
	Error( "These values cannot be treated as Numeric and therefore "
	       "InnerProduct\noperations are impossible" );
      }
    }
    // Give a warning for singular features, except when it's
    // a result of a forced recalculation or when the input format is
    // Sparse ??
    if ( !nothing_changed &&
	 ( input_format != Sparse && input_format != SparseBin ) &&
	 ( realy_first || !always ) ){
      bool first = true;
      int ff;
#if __GNUC__ < 3
      ostrstream ostr1;
      ostrstream ostr2;
#else
      ostringstream ostr1;
      ostringstream ostr2;
#endif
      for ( ff = 0; ff < num_of_features; ff++ ) 
	if ( feat_status[ff] == Singleton ||
	     feat_status[ff] == SingletonNumeric ){
	  if ( first ){
	    ostr1 << "The following feature(s) have only 1 value: ";
	    first = false;
	  }
	  else
	    ostr1 << ", ";
	  int n = ff;
	  while ( ff < num_of_features-1 && 
		  ( feat_status[ff+1] == Singleton ||
		    feat_status[ff+1] == SingletonNumeric ) )
	    ff++;
	  if ( n != ff ){
	    ostr1 << n+1 << "-" << ff+1;
	  }
	  else
	    ostr1 << ff+1;
	}
      if ( !first  ){
#if __GNUC__ < 3
	ostr1 << ends;
	char *pnt = ostr1.str();
	Warning( pnt );
	delete [] pnt;
#else
	Warning( ostr1.str() );
#endif
      }
      first = true;
      for ( ff = 0; ff < num_of_features; ff++ ) 
	if ( feat_status[ff] == NotNumeric ){
	  if ( first ){
	    ostr2 << "The following feature(s) contained non-numeric values and\nwill be treated as NON-Numeric: ";
	    first = false;
	  }
	  else
	    ostr2 << ", ";
	  int n = ff;
	  while ( ff < num_of_features-1 && 
		  feat_status[ff+1] == NotNumeric ) ff++;
	  if ( n != ff ){
	    ostr2 << n+1 << "-" << ff+1;
	  }
	  else
	    ostr2 << ff+1;
	}
      if ( !first  ){
#if __GNUC__ < 3
	ostr2 << ends;
	char *pnt = ostr2.str();
	Warning( pnt );
	delete [] pnt;
#else
	Warning( ostr2.str() );
#endif
      }
    }
    delete [] feat_status;
  }
  
  bool MBLClass::WriteNamesFile( ostream& os ) const {
    bool result = true;
    if ( ExpInvalid() ){
      result = false;
    }
    else {
      TargetValue *tv;
      // Print the possible classes.
      //
      int t;
      for ( t=0; t < Targets->ArraySize()-1; t++ ) {
	tv = Targets->Value(t);
	os << tv << ",";
      } 
      tv = Targets->Value(Targets->ArraySize()-1);
      os << tv << "." << endl << endl;
      
      // Loop over the Features.
      //
      FeatureValue *vf;
      int f;
      for ( f = 0; f < num_of_features; f++) {
	
	// Print the Feature name, and a colon.
	//
	os << "a" << f+1 << ": ";
	if ( Features[f]->Ignore() )
	  os << "Ignore" << endl;
	else if ( Features[f]->Numeric() )
	  os << "Numeric" << endl;
	else {
	  // Loop over the values.
	  //
	  int i;
	  for ( i=0; i< Features[f]->ArraySize()-1; i++ ){
	    vf = Features[f]->Value(i);
	    os << vf << ",";
	  }
	  os << Features[f]->Value(Features[f]->ArraySize()-1) 
	     << "." << endl;
	}
      }
    }
    return result;
  }

  bool MBLClass::ShowBestNeighbors( ostream &outfile, 
				    const bool show_dist,
				    const bool show_distr ) const{
    if ( BestArray &&
	 Verbosity( NEAR_N | ALL_K) ){
      int *InvPerm = new int[num_of_features];
      for ( int i=0; i< num_of_features; i++ )
	InvPerm[Permutation[i]] = i;
      for ( int k = 0; k < num_of_neighbors; k++) {
	bestrec *Best = BestArray[k];
	if ( Best->CurrPos > 0 ){
	  if ( Verbosity( NEAR_N ) ){
	    int TotalBests = Best->TotalBests();
	    outfile << "# k=" << k+1 << ", " << TotalBests 
		    <<  " Neighbor(s) at distance: ";
	    int OldPrec = outfile.precision(DBL_DIG-1);
	    outfile.setf(ios::showpoint);
	    outfile << "\t" << Best->BestDistance;
	    outfile.precision(OldPrec);
	    if ( MaxBests < TotalBests )
	      outfile << " (only " << MaxBests << " shown)";
	    outfile << endl;
	    for ( int m=0; m < Best->CurrPos; m++ ){
	      outfile << "#\t";
	      for ( int j=0; j<num_of_features; j++ ){
		switch ( input_format) {
		case C4_5:
		  // no break
		case ARFF:
		  if ( Features[j]->Ignore() )
		    outfile << "-*-,";
		  else
		    outfile << Best->BestInstances[m][InvPerm[j]] << ",";
		  break;
		case Sparse:
		  if ( Best->BestInstances[m][InvPerm[j]]->Name() != DefaultSparseString )
		    outfile << "(" << j+1 << "," << CodeToStr( Best->BestInstances[m][InvPerm[j]]->Name() ) << ")";
		  break;
		case SparseBin:
		  if ( Best->BestInstances[m][InvPerm[j]]->Name()[0] == '1' )
		    outfile << j+1 << ",";
		  break;
		case Columns:
		  if ( Features[j]->Ignore() )
		    outfile << "-*- ";
		  else
		    outfile << Best->BestInstances[m][InvPerm[j]] << " ";
		  break;
		default:
		  if ( Features[j]->Ignore() )
		    outfile << string( F_length, '*' );
		  else
		    outfile << Best->BestInstances[m][InvPerm[j]];
		  break;
		}
	      }	      
	      if ( show_distr )
		outfile << *Best->BestDistributions[m] << endl;
	      else
		outfile << " -*-" << endl;
	    }
	  }
	  else { //if ( verbosity & ALL_K )
	    bool tie = false;
	    outfile << "# k=" << k+1 << " " 
		    << Best->SummedDist->BestTarget( tie, random_seed >= 0) ;
	    if ( show_distr ){
	      outfile << "\t" << Best->AggregateDist;
	    }
	    if ( show_dist ){
	      int OldPrec = outfile.precision(DBL_DIG-1);
	      outfile.setf(ios::showpoint);
	      outfile << "\t" << Best->BestDistance;
	      outfile.precision(OldPrec);
	    }
	    outfile << endl;
	  }
	}
      }
      delete [] InvPerm;
      return true;
    }
    else
      return false;
  }
  
  
  bool MBLClass::Chop( const string& line ) {
    OriginalInput = line;
    string::iterator it = OriginalInput.end();
    --it;
    // first trim trailing spaces 
    while ( it != OriginalInput.begin() && 
	    isspace(*it) ) --it;
    OriginalInput.erase( ++it , OriginalInput.end() );
    if ( chop_examples ){
      string wght;
      OriginalInput = strip_exemplar_weight( OriginalInput, wght );
      if ( wght.empty() ){
	Warning( "Missing sample weight: " );
	return false;
      }
      else {
	double tmp;
	if ( !string2double( wght, tmp ) ){
	  Warning( "Wrong sample weight: '" + wght + "'" );
	  return false;
	}
	else {
	  ChoppedInput[num_of_features+1] = wght;
	}
      }
    }
    // now trim trailing dot
    it = OriginalInput.end();
    --it;
    if ( it !=  OriginalInput.begin() &&  *it == '.' &&
	 input_format != Compact && input_format != Columns )
      --it;
    // and trailing spaces
    while ( it != OriginalInput.begin() && 
	    isspace(*it) ) --it;
    OriginalInput.erase( ++it , OriginalInput.end() );
    return (this->*ChopInput)( OriginalInput, 
			       ChoppedInput, num_of_features );
  }
  
  bool MBLClass::set_input_format( const InputFormatType IF, 
				   bool test_phase ){
    if ( do_sparse && ( IF != SparseBin && IF != Sparse ) )
      return false;
    chop_examples = do_sample_weighting &&
      !( test_phase && no_samples_test );
    switch ( IF ){
    case C4_5:
      ChopInput = &MBLClass::chop_C4_5_string;
      break;
    case ARFF:
      ChopInput = &MBLClass::chop_ARFF_string;
      break;
    case SparseBin:
      ChopInput = &MBLClass::chop_bin_string;
      do_sparse = true;
      break;
    case Sparse:
      ChopInput = &MBLClass::chop_sparse_string;
      do_sparse = true;
      break;
    case Columns:
      ChopInput = &MBLClass::chop_COLUMNS_string;
      break;
    case Compact:
      ChopInput = &MBLClass::chop_Compact_string;
      break;
    default:
      return false;
    }
    input_format = IF;
    return true;
  }
  
  bool MBLClass::Init_MBL_test( bool all_vd ){ 
    bool result = false;
    if ( !ExpInvalid() ) {
      init_best_array(); // mist be cleared for EVERY test
      BestDistrib.Clear();
      if ( !MBL_init ){  // do this only when necessary
	if ( !is_copy ){
	  calculate_fv_entropy( true );
	  if ( ib2_offset != 0 ){
	    // invalidate MVDM matrices, they might be changing in size
	    for ( int j=0; j < num_of_features; ++j ){
	      if ( !Features[j]->Ignore() ){
		Features[j]->delete_matrix();
	      }
	    }
	  }
	  pre_divide2( all_vd );
	}
	for ( int i=0; i < num_of_features; i++ ){
	  if ( Features[i]->Ignore() )
	    continue;
	  if ( Features[i]->Numeric() ){
	    test_feature_val[i] = &MBLClass::n_test_ov;
	  }
	  else {
	    MetricType TM =  Features[i]->Metric();
	    if ( TM == DefaultMetric )
	      TM = GlobalMetric;
	    switch ( TM ){
	    case Overlap:
	      test_feature_val[i] = &MBLClass::test_ov;
	      break;
	    case ValueDiff:
	      test_feature_val[i] = &MBLClass::test_vd;	  
	      break;
	    case JeffreyDiv:
	      test_feature_val[i] = &MBLClass::test_jd;	  
	      break;
	    default:
	      Warning( "Init_MBL_test: feature " + itos(i+1) +
		       " wrong Metric in switch:" +
		       to_string( TM ) );
	      return false;
	    }
	  }
	}
	if ( !is_copy ){
	  InitWeights( );
	  srand( random_seed );
	}
	MBL_init = true;
      }
      result = true;
    }
    return result;
  }
  
  void MBLClass::NormalizeResult(){
    switch ( normalisation ){
    case 0:
      BestDistrib.Normalize();
      break;
    case 1:
      BestDistrib.Normalize_1( norm_factor, Targets );
      break;
    default:
      break;
    }
  }

  const ValueDistribution *MBLClass::ExactMatch( const Instance *inst,
						 int& status ) const {
    status = 0;
    const ValueDistribution *result = NULL;
    if ( !do_dot_product &&
	 ( do_exact_match || 
	   ( num_of_neighbors == 1 &&
	     !( Verbosity( NEAR_N | ALL_K) ) ) ) ){
      result = InstanceBase->ExactMatch( inst );
      if ( result && do_exact_match )
	status = 1;
    }
    return result;
  }

  bool MBLClass::HideInstance( const Instance *Inst ){
    bool result = true;
    InstanceBase->RemoveInstance( Inst );
    MBL_init = do_sloppy_loo; // must be only true if you are REALY sure
    for ( int i=0; i < effective_feats && result; i++ ){
      PermFeatures[i]->delete_matrix();
      if ( !PermFeatures[i]->decrement_value( Inst->FV[i], 
					      Inst->TV ) ){
	FatalError( "Unable to Hide an Instance!" );
	result = false;
      }
    }
    if ( result )
      Targets->decrement_value( Inst->TV );
    return result;
  }
  
  bool MBLClass::UnHideInstance( const Instance *Inst ){
    bool result = true;
    InstanceBase->AddInstance( Inst );
    MBL_init = do_sloppy_loo; // must be only true if you are REALY sure
    for ( int i=0; i < effective_feats && result; i++ ){
      PermFeatures[i]->delete_matrix();
      if ( !PermFeatures[i]->increment_value( Inst->FV[i], 
					      Inst->TV ) ){
	FatalError( "Unable to UnHide this Instance!" );
	result = false;
      }
    }
    if ( result )
      Targets->increment_value( Inst->TV );
    return result;
  }
  
  void MBLClass::init_best_array(void){ 
    // When necessary, take a larger array. (initialy it has 0 length)
    // Also check if verbosity has changed and a BestInstances array
    // is required.
    //
    if ( BASize < num_of_neighbors ){
      if ( BestArray == NULL )
	BestArray =
	  (bestrec**)malloc( num_of_neighbors * sizeof( bestrec **) );
      else
	BestArray =
	  (bestrec**)realloc( BestArray,
			      num_of_neighbors * sizeof( bestrec **) );
      int k;
      for ( k = BASize; k < num_of_neighbors; k++) {
	BestArray[k] = new bestrec( );
      }
      BASize = num_of_neighbors;
    }
    for ( int i = 0; i < num_of_neighbors; i++) {
      BestArray[i]->BestDistance = (DBL_MAX - num_of_neighbors) + i;
      if ( BestArray[i]->BestInstances == NULL ){
	if ( Verbosity( NEAR_N ) ){
	  BestArray[i]->BestInstances = new FeatureValue **[MaxBests];
	  assert( BestArray[i]->BestInstances != NULL );
	  int k;
	  for ( k=0; k < MaxBests; k++ )
	    BestArray[i]->BestInstances[k] = NULL;
	  BestArray[i]->BestDistributions =
	    new ValueDistribution *[MaxBests];
	  assert( BestArray[i]->BestDistributions != NULL );
	}
      }
      else {
	for ( int j = 0; j < BestArray[i]->CurrPos; j ++ ){
	  if ( BestArray[i]->BestInstances[j] )
	    delete [] BestArray[i]->BestInstances[j];
	  BestArray[i]->BestInstances[j] = NULL;
	  delete BestArray[i]->BestDistributions[j];
	  BestArray[i]->BestDistributions[j] = NULL;
	}
      }
      delete BestArray[i]->SummedDist;
      BestArray[i]->SummedDist = NULL;
      BestArray[i]->entropy = 0.0;
      BestArray[i]->AggregateDist.Clear();
      BestArray[i]->CurrPos = 0;
    }
  }
  
  inline double MBLClass::add_to_best_array( double Distance, 
					     const ValueDistribution *Distr,
					     FeatureValue **neighbor ){
    // We have the similarity in Distance, and a num_of_neighbors
    // dimensional array with best similarities.
    // Check, and add/replace/move/whatever.
    //
    int k;
    for (k = 0; k < num_of_neighbors; k++) {
      bestrec *Best = BestArray[k];
      if (fabs(Distance - Best->BestDistance) < Epsilon) {
	// Equal...just add to the end.
	//
	Best->AggregateDist.Merge( *Distr );
	if ( Verbosity(NEAR_N) && Best->CurrPos < MaxBests ){
	  Best->BestInstances[Best->CurrPos] = neighbor;
	  Best->BestDistributions[Best->CurrPos++] = Distr->clone();
	}
	break;
      }
      // Check if better than Bests[k], insert (or replace if
      // it's the lowest of the k bests).
      //
      /*
	Example (no_n = 3):
	k      distance      number
	0       2             3
	1       4             2
	2       6             1
	
	sim = 1 (dus beste)
      */
      else if (Distance < Best->BestDistance) {
	if (k == num_of_neighbors - 1) {
	  //
	  // Replace.
	  //
	  Best->BestDistance = Distance;
	  if ( Verbosity( NEAR_N ) ){
	    int j;
	    for ( j=0; j < Best->CurrPos; j++ ){
	      delete [] Best->BestInstances[j];
	      Best->BestInstances[j] = NULL;
	      delete Best->BestDistributions[j];
	      Best->BestDistributions[j] = NULL;
	    }
	    Best->BestInstances[0] = neighbor;
	    Best->BestDistributions[0] = Distr->clone();
	  }
	  Best->CurrPos = 1;
	  Best->AggregateDist.Clear();
	  Best->AggregateDist.Merge( *Distr );
	} 
	else {
	  //
	  // Insert. First shift the rest up.
	  //
	  bestrec *keep = BestArray[num_of_neighbors-1];
	  int i;
	  for ( i = num_of_neighbors - 1; i > k; i--) {
	    BestArray[i] = BestArray[i-1]; 
	  } // i
	  //
	  // And now insert.
	  //
	  keep->BestDistance = Distance;
	  if ( Verbosity(NEAR_N) ){
	    int j;
	    for ( j=0; j < keep->CurrPos; j++ ){
	      delete [] keep->BestInstances[j];
	      keep->BestInstances[j] = NULL;
	      delete keep->BestDistributions[j];
	      keep->BestDistributions[j] = NULL;
	    }
	    keep->BestInstances[0] = neighbor;
	    keep->BestDistributions[0] = Distr->clone();
	  }
	  keep->CurrPos = 1;
	  keep->AggregateDist.Clear();
	  keep->AggregateDist.Merge( *Distr );
	  BestArray[k] = keep;
	}
	break;
      } // Distance < fBest
    } // k
    return BestArray[num_of_neighbors-1]->BestDistance;
  }    
  
  inline double MBLClass::RelativeWeight( int j ){
    double result = 1.0;
    double nearest_dist, furthest_dist;
    switch ( decay_flag ){
    case Zero:
      break;
    case InvDist:
      result = 1.0/(BestArray[j]->BestDistance + Epsilon);
      break;
    case InvLinear:
      if ( j > 0 && num_of_neighbors != 1 ){
	nearest_dist = BestArray[0]->BestDistance;
	furthest_dist = BestArray[num_of_neighbors-1]->BestDistance;
	result = (furthest_dist - BestArray[j]->BestDistance) /
	  (furthest_dist-nearest_dist);
      }
      break;
    case ExpDecay:
      result = exp(-decay_alfa*pow(BestArray[j]->BestDistance, decay_beta));
      break;
    default:
      FatalError( "wrong value in switch" );
    }
    return result;
  }

  const ValueDistribution *MBLClass::get_best_dist( double &final_distance ){
    // Analyse BestArray to find THE best solution.
    // Which Target is the most common?
    // Count over the num_of_neighbors arrays.
    // For each neighbor, we loop over the number of bests in that
    // bin, and merge that distribution into Dist
    //
    int stopped = num_of_neighbors-1;
    for ( int k = 0; k < num_of_neighbors; ++k ) {
      if ( k > 0 )
	BestArray[k]->SummedDist = BestArray[k-1]->SummedDist->clone();
      else
	BestArray[0]->SummedDist = new WValueDistribution;
      BestArray[k]->SummedDist->MergeWeight( BestArray[k]->AggregateDist,
					     RelativeWeight(k) );
    }
    final_distance = BestArray[0]->BestDistance;
    WValueDistribution *result = BestArray[stopped]->SummedDist;
    return result;
  }

  const ValueDistribution *MBLClass::GetDistrib( int n ){
    if ( n < num_of_neighbors )
      return BestArray[n]->SummedDist;
    else {
      Warning( "Invalid parameter for GetDistrib " );
      return NULL;
    }
  }

  inline double WeightFun( double D, double W ){
    return D / (W + Epsilon);
  }
  
  inline FeatureValue **fva_copy( FeatureValue **OrgFV,
				  FeatureValue **RedFV,
				  int OffSet,
				  int Size ){
    FeatureValue **result = new FeatureValue*[Size];
    int i,j;
    for ( i=0; i< OffSet; i++ )
      result[i] = OrgFV[i];
    for ( j=OffSet; j< Size; j++ )
      result[j] = RedFV[j-OffSet];
    return result;
  }
  
  const ValueDistribution *MBLClass::test_instance_ex( const Instance *Inst,
						       double &final_distance,
						       InstanceBase_base *IB,
						       int ib_offset ){
    double Distance;
    FeatureValue **TestFV;
    FeatureValue **CurrentFV = new FeatureValue *[num_of_features];
    int EndPos, TmpPos, CurPos = 0;
    int EffFeat;
    double Treshold = DBL_MAX;
    double *distances = new double[num_of_features+1];
    int i;
    for ( i = 0; i <= num_of_features; i++ ){
      distances[i] = 0.0;
    }
    TestFV = (Inst->FV) + ib_offset;
    EffFeat = effective_feats - ib_offset;
    ValueDistribution *best_distrib = IB->InitGraphTest( CurrentFV, TestFV );
    while ( best_distrib->ZeroDist() ){
      // This might happen when doing LOO or CV tests
      TmpPos = EffFeat-1;
      best_distrib = IB->NextGraphTest( CurrentFV, TestFV, TmpPos );
    }
    ValueDistribution::dist_iterator lastpos;
    Vfield *Bpnt = NULL;
    if ( best_distrib ){
      lastpos = best_distrib->Begin();
      if ( lastpos != best_distrib->End() )
	Bpnt = (*lastpos).second;
    }
    double max_product = 0.0;
    if ( do_dot_product )
      max_product = max_inner_product();
    while ( Bpnt ) {
      if ( do_dot_product ){
	EndPos  = test_in_graph_dot_ex( TestFV,
					CurrentFV,
					distances,
					CurPos,
					EffFeat,
					ib_offset,
					Bpnt->Weight(),
					max_product,
					Distance );
      }
      else {
	EndPos  = test_in_graph_ex( TestFV,
				    CurrentFV,
				    distances,
				    CurPos,
				    EffFeat,
				    ib_offset,
				    Bpnt->Weight(),
				    Distance );
      }
      if ( EndPos == EffFeat ){
	// we finished with a certain amount of succes
	ValueDistribution ResultDist;
	ResultDist.SetFreq( Bpnt->Value(), Bpnt->Freq() );
	if ( Verbosity(NEAR_N) ){
	  Treshold = add_to_best_array( Distance, 
					&ResultDist, 
					fva_copy( Inst->FV, CurrentFV, 
						  ib_offset, 
						  num_of_features) );
	}
	else
	  Treshold = add_to_best_array( Distance, &ResultDist );
      }
      else {
	EndPos++; // out of luck, compensate for roll-back
      }
      CurPos = EndPos-1;
      ++lastpos;
      if ( lastpos != best_distrib->End() ){
	Bpnt = (*lastpos).second;
      }
      else {
	best_distrib = IB->NextGraphTest( CurrentFV, TestFV, CurPos );
	while ( best_distrib && best_distrib->ZeroDist() ){
	  // This might happen when doing LOO or CV tests
	  TmpPos = EffFeat-1;
	  best_distrib = IB->NextGraphTest( CurrentFV, TestFV, TmpPos );
	  if ( TmpPos < CurPos ){
	    CurPos = TmpPos;
	  }
	}
	Bpnt = NULL;
	if ( best_distrib ){
	  lastpos = best_distrib->Begin();
	  if ( lastpos != best_distrib->End() ){
	    Bpnt = (*lastpos).second;  
	  }
	}
      }
    }
    delete [] CurrentFV;
    delete [] distances;
    return get_best_dist( final_distance );
  }
  
  inline bool FV_to_real( FeatureValue *FV, double &result ){
    if ( FV ){
      if ( string2double( FV->Name(), result ) )
	return true;
    }
    return false;
  }
  
  double MBLClass::n_test_ov( FeatureValue *FV,
			      FeatureValue *G,
			      int F_pos ) const{
    double r1, r2, result;
    if ( FV_to_real( FV, r1 ) &&
	 FV_to_real( G, r2 ) )
      result = fabs( (r1-r2)/
		     (PermFeatures[F_pos]->Max() - 
		      PermFeatures[F_pos]->Min()));
    else
      result = 1.0;
    result *= PermFeatures[F_pos]->Weight();
    return result;
  }
  
  double MBLClass::test_ov( FeatureValue *FV,
			    FeatureValue *G,
			    int F_pos ) const {
    double result = 0.0;
    if ( FV && FV->ValFreq() == 0 )
      result = 1.0;
    else if ( FV != G ){
      result = PermFeatures[F_pos]->Weight();
    }
    return result;
  }
  
  double MBLClass::test_vd( FeatureValue *F,
			    FeatureValue *G,
			    int F_pos ) const {
    double result = PermFeatures[F_pos]->ValueDistance( F, G, 
							mvd_treshold );
    result *= PermFeatures[F_pos]->Weight();
    return result;
  }

  double MBLClass::test_jd( FeatureValue *F,
			    FeatureValue *G,
			    int F_pos ) const {
    double result = PermFeatures[F_pos]->JeffreyDistance( F, G, 
							  mvd_treshold );
    result *= PermFeatures[F_pos]->Weight();
    return result;
  }
  
  double MBLClass::max_inner_product(){
    double result = 0.0;
    for ( int i = 0; i < effective_feats; ++i ){
      result += PermFeatures[i]->Weight();
    }
    return result;
  }

  double MBLClass::in_prod( FeatureValue *FV,
			    FeatureValue *G,
			    int F_pos ) const {
    double r1, r2, result;
    if ( FV_to_real( FV, r1 ) &&
	 FV_to_real( G, r2 ) ){
      if ( input_format != SparseBin ){
    	r1 = ( r1 + fabs(PermFeatures[F_pos]->Min())) /
	  fabs(PermFeatures[F_pos]->Max() - PermFeatures[F_pos]->Min());
	// force the testvalues to the right range (shabby)
	if ( r1 < 0.0 )
	  r1 = 0.0;
	else if ( r1 > 1.0 )
	  r1 = 1.0;
	r2 = ( r2 + fabs(PermFeatures[F_pos]->Min())) /
	  fabs(PermFeatures[F_pos]->Max() - PermFeatures[F_pos]->Min());
	assert( r1 <= 1.0 );
	assert( r1 >= 0.0 );
      }
      result = r1 * r2;
    }
    else
      result = 0.0;
    result *= PermFeatures[F_pos]->Weight();
    return result;
  }
  
  int MBLClass::test_in_graph_dot( FeatureValue **FV,
				   FeatureValue **G, 
				   double *Distances,
				   int CurPos,
				   int Size,
				   int ib_offset,
				   double Treshold,
				   double max_prod ) const {
    double result;
    int i, TrueF;
    for ( i=CurPos, TrueF = i + ib_offset; i < Size; ++i,++TrueF ){
      result = in_prod( FV[i], G[i], TrueF );
      Distances[i+1] = Distances[i] + result;
      if ( max_prod - Distances[i+1] - Treshold > Epsilon ){   
	return i;
      }
    }
    return Size;
  }
  
  int MBLClass::test_in_graph_dot_ex( FeatureValue **FV,
				      FeatureValue **G, 
				      double *Distances,
				      int CurPos,
				      int Size,
				      int ib_offset,
				      double ExWeight,
				      double max_prod,
				      double& Distance ) const {
    double result;
    int i, TrueF;
    for ( i=CurPos, TrueF = i + ib_offset; i < Size; ++i,++TrueF ){
      result = in_prod( FV[i], G[i], TrueF );
      Distances[i+1] = Distances[i] + result;
    }
    Distance = WeightFun( max_prod - Distances[i], ExWeight );
    return Size;
  }
  
  int MBLClass::test_in_graph( FeatureValue **FV,
			       FeatureValue **G, 
			       double *Distances,
			       int CurPos,
			       int Size,
			       int ib_offset,
			       double Treshold ) const {
    double result;
    int i, TrueF;
    for ( i=CurPos, TrueF = i + ib_offset; i < Size; ++i,++TrueF ){
      result = (this->*test_feature_val[Permutation[TrueF]])( FV[i],
							      G[i],
							      TrueF );
      Distances[i+1] = Distances[i] + result;
      if ( Distances[i+1] - Treshold > Epsilon ){
	return i;
      }
    }
    return Size;
  }

  int MBLClass::test_in_graph_ex( FeatureValue **FV,
				  FeatureValue **G, 
				  double *Distances,
				  int CurPos,
				  int Size,
				  int ib_offset,
				  double ExWeight,
				  double& Distance ) const {
    double result;
    int i, TrueF;
    for ( i=CurPos, TrueF = i + ib_offset; i < Size; ++i,++TrueF ){
      result = (this->*test_feature_val[Permutation[TrueF]])( FV[i],
							      G[i],
							      TrueF );
      Distances[i+1] = Distances[i] + result;
    }
    Distance = WeightFun( Distances[i], ExWeight );
    return Size;
  }
  
  const ValueDistribution *MBLClass::test_instance( const Instance *Inst,
						    double &final_distance,
						    InstanceBase_base *IB,
						    int ib_offset ){
    double Distance;
    FeatureValue **TestFV;
    FeatureValue **CurrentFV = new FeatureValue *[num_of_features];
    int EndPos, TmpPos, CurPos = 0;
    int EffFeat;
    double Treshold = DBL_MAX;
    double *distances = new double[num_of_features+1];
    int i;
    for ( i = 0; i <= num_of_features; i++ ){
      distances[i] = 0.0;
    }
    TestFV = (Inst->FV) + ib_offset;
    EffFeat = effective_feats - ib_offset;
    ValueDistribution *best_distrib = IB->InitGraphTest( CurrentFV, TestFV );
    while ( best_distrib->ZeroDist() ){
      // This might happen when doing LOO or CV tests
      TmpPos = EffFeat-1;
      best_distrib = IB->NextGraphTest( CurrentFV, TestFV, TmpPos );
    }
    double max_product = 0.0;
    if ( do_dot_product )
      max_product = max_inner_product();
    do {
      if ( do_dot_product ){
	EndPos  = test_in_graph_dot( TestFV,
				     CurrentFV,
				     distances,
				     CurPos,
				     EffFeat,
				     ib_offset,
				     Treshold,
				     max_product );
      }
      else {
	EndPos  = test_in_graph( TestFV,
				 CurrentFV,
				 distances,
				 CurPos,
				 EffFeat,
				 ib_offset,
				 Treshold );
      }
      if ( EndPos == EffFeat ){
	// we finished with a certain amount of succes
	if ( do_dot_product )
	  Distance = max_product - distances[EndPos];
	else
	  Distance = distances[EndPos];
	if ( Distance >= 0.0 ){
	  if ( Verbosity(NEAR_N) ){
	    Treshold = add_to_best_array( Distance, 
					  best_distrib, 
					  fva_copy( Inst->FV, CurrentFV, 
						    ib_offset, 
						    num_of_features) );
	  }
	  else
	    Treshold = add_to_best_array( Distance, best_distrib );
	}
	else {
	  Error( "DISTANCE == " + to_string(Distance) );
	  FatalError( "we are dead" );
	  abort();
	}
      }
      else {
	EndPos++; // out of luck, compensate for roll-back
      }
      int pos;
      for ( pos=EndPos-1; pos >= 0; pos-- ){
	if ( distances[pos] <= Treshold ){
	  CurPos = pos;
	  best_distrib = IB->NextGraphTest( CurrentFV, TestFV, CurPos );
	  while ( best_distrib && best_distrib->ZeroDist() ){
	    // This might happen when doing LOO or CV tests
	    TmpPos = EffFeat-1;
	    best_distrib = IB->NextGraphTest( CurrentFV, TestFV, TmpPos );
	    if ( TmpPos < CurPos ){
	      CurPos = TmpPos;
	    }
	  }
	  break;
	}
      }
    } while ( best_distrib );
    delete [] CurrentFV;
    delete [] distances;
    return get_best_dist( final_distance );
  }

  const ValueDistribution *MBLClass::TestInstance( const Instance *Inst, 
					     double& Distance,
					     InstanceBase_base *SubTree,
					     int level ){
    if (  do_sample_weighting && !do_ignore_samples )
      return test_instance_ex( Inst, Distance, SubTree, level );
    else
      return test_instance( Inst, Distance, SubTree, level );
  }

  
  string MBLClass::strip_exemplar_weight( const string& Buffer,
					  string& wght ){
    string::size_type t_pos, e_pos = Buffer.length();
    // first remove trailing whitespace
    e_pos = Buffer.find_last_not_of( " \t", e_pos );
    // now some non-space
    t_pos = Buffer.find_last_of( " \t", e_pos );
    if ( t_pos != string::npos ){
      // found white space
      wght = string( Buffer, t_pos+1, e_pos - t_pos );
    }
    else {
      //      Warning( "missing Exemplar weight in line " + Buffer );
      wght = "";
    }
    // and some more space...
    e_pos = Buffer.find_last_not_of( " \t", t_pos );
    return string( Buffer, 0, e_pos+1 );
  }

  inline int MBLClass::CountFeatures( istream &datafile,
				      const bool test_phase ){
    // Count features assuming a specified format
    if ( do_sparse || input_format == SparseBin || input_format == Sparse )
      return MaxFeatures;
    
    int result = 0;
    string Buffer;
    if ( !getline( datafile, Buffer ) ) {
      Warning( "empty data file" );
    }
    else {
      if ( input_format == ARFF ){
	while ( !compare_nocase_n( "@DATA", Buffer ) ){
	  if ( !getline( datafile, Buffer ) ){
	    Warning( "empty data file" ); 
	    return result;
	  };
	}
	if ( !getline( datafile, Buffer ) ){
	  Warning( "empty data file" ); 
	  return result;
	};
      }
      while ( empty_line( Buffer, input_format ) ){
	if ( !getline( datafile, Buffer ) ){
	  Warning( "empty data file" ); 
	  return result;
	};
      }
      string dummy;
      if ( do_sample_weighting &&  !( test_phase && no_samples_test ) ){
	Buffer = strip_exemplar_weight( Buffer, dummy );
      }
      // now we have a line, analyze it using the User defined input_format
      int len = Buffer.length();
      switch ( input_format ){
      case ARFF:
      case C4_5:
	int i;
	for ( i = 0; i < len; i++) {
	  if (Buffer[i] == ',')
	    result++;
	};
	break;
      case Compact:
	if ( F_length == 0 ){
	  Error( "-F Compact specified, but Feature Length not set."
		 " (-l option)" );
	  return result;
	}
	else
	  result = len / F_length - 1;
	break;
      case Columns:
	int j;
	for ( j = 0; j < len; j++) {
	  if ( isspace(Buffer[j]) ){
	    result++;
	    while ( isspace( Buffer[++j] ) );
	    if ( Buffer[j] == '\0' )
	      result--; // we had some trailing spaces
	  }
	};
	break;
      default:
	FatalError( "CountFeatures: Illegal value in switch:" +
		    to_string(input_format) );
      };
    }
    return result;
  }
  
  int MBLClass::ExamineLine( const string& in_buffer, 
			     InputFormatType &IF,
			     const bool test_phase ){
    if ( do_sparse )
      return MaxFeatures;
    string Buffer;
    if ( do_sample_weighting &&  !( test_phase && no_samples_test ) ){
      string dummy;
      Buffer = strip_exemplar_weight( in_buffer, dummy );
    }
    else
      Buffer = in_buffer;
    unsigned int len = Buffer.length();
    int NumF = 0;
    bool found_some = false;
    for ( unsigned int i = 0; i < len; ) {
      if ( Buffer[i] == ',' ) {
	if ( found_some ) {
	  // first delimiter must be after something significant!
	  if ( IF == UnknownInputFormat ||
	       IF == C4_5 ||
	       IF == ARFF ){
	    ++NumF;
	    if ( IF == UnknownInputFormat )
	      IF = C4_5;
	  }
	}
	else {
	  // so assume it's NOT C4_5 or ARFF
	  found_some = true;
	}
	i++;
      }
      else if ( isspace( Buffer[i++] ) ){
	while ( i < len && isspace( Buffer[i] ) ) ++i;
	if ( i < len ){ // otherwise just trailing spaces!
	  if ( found_some ){
	    // first delimiter must be after something significant!
	    if ( IF == UnknownInputFormat ||
		 IF == Columns ){
	      IF = Columns;
	      ++NumF;
	    }
	  }
	}
	else
	  break;
      }
      else {
	// OK we've found at least something 
	found_some = true;
      }
    }
    if ( !found_some ){
      return 0;
    }
    if ( !NumF ){
      // Zero Features means block of F_length char codes.
      //
      if ( IF != UnknownInputFormat &&
	   IF != Compact ){
	Warning( "Inputformat seems Compact, but something else specified" );
	return 0;
      }
      IF = Compact;
      if ( F_length == 0 ){
	Warning( "InputFormat seems Compact, but Feature Length not set."
		 " (-l option)" );
	return 0;
      }
      NumF = len / F_length - 1;
    }
    return NumF;
  }
  
  inline void MBLClass::show_input_format( ostream& os ){
    if ( !Verbosity(SILENT) ){
      switch ( input_format ){
      case C4_5:
	os << "InputFormat       : C4.5";
	break;
      case SparseBin:
	os << "InputFormat       : Sparse Binary";
	break;
      case Sparse:
	os << "InputFormat       : Sparse";
	break;
      case ARFF:
	os << "InputFormat       : ARFF";
	break;
      case Columns:
	os << "InputFormat       : Columns";
	break;
      case Compact:
	os << "InputFormat       : Compact, (Feature Length = "
	   << F_length << ")";
	break;
      default:
	Warning( "InputFormat unknown\n" );
      }
      os << endl << endl;
    }
  }

  int MBLClass::ExamineData( const string& FileName, const bool test_phase ){
    // Looks at the data files, counts num_of_features.
    // and sets input_format variables.
    //
    int NumF = 0;
    InputFormatType IF = UnknownInputFormat;
    // Open the file.
    //
    if ( FileName == "-" )
      return num_of_features;
    else if ( FileName == "" ) {
      Warning( "couldn't initialize: No FileName specified " );
      return 0;
    }
    else {
      string Buffer;
      ifstream datafile( FileName.c_str(), ios::in);
      if (!datafile) {
	Warning( "can't open DataFile: " + FileName );
	return 0;
      }
      else if ( input_format != UnknownInputFormat ){
	// The format is somehow already known, so use that
	NumF = CountFeatures( datafile, test_phase );
	IF = input_format;
      }
      else if ( !getline( datafile, Buffer ) ){
	Warning( "empty data file: " + FileName );
      }
      // We start by reading the first line so we can figure out the number
      // of Features, and see if the file is comma seperated or not,  etc.
      //
      else { 
	if ( IF == ARFF ){
	  // Remember, we DON't want to auto-detect ARFF
	  while ( !compare_nocase_n( "@DATA", Buffer ) ){
	    if ( !getline( datafile, Buffer ) ) {
	      Warning( "no ARRF data after comments: " + FileName );
	      return 0;
	    }
	  }
	  do {
	    if ( !getline( datafile, Buffer ) ) {
	      Warning( "no ARRF data after comments: " + FileName );
	      return 0;
	    }
	  } while ( empty_line( Buffer, input_format ) );
	}
	else {
	  while ( empty_line( Buffer, input_format ) ) {
	    if ( !getline( datafile, Buffer ) ) {
	      Warning( "no data after comments: " + FileName );
	      return 0;
	    }
	  }
	  // We found a useful line!
	  // Now determine the input_format (if not already known,
	  // and Count Features as well.
	}
	NumF = ExamineLine( Buffer, IF, test_phase );
      }
    }
    if ( NumF > 0 ){
      if ( input_format != UnknownInputFormat &&
	   input_format != IF ){
	Warning( "assumed inputformat differs from specified!" );
	return 0;
      }
      else {
	if ( !Verbosity(SILENT) ){
	  *Log(mylog) << "Examine datafile '" << FileName 
		      << "' gave the following results:"
		      << endl
		      << "Number of Features: " << NumF << endl;
	}
	if ( NumF > MaxFeatures ){
	  Error( "Number of Features exceeds the maximum number. "
		 "(currently " + to_string(MaxFeatures) +
		 ")\nPlease increase.\n" );
	  return 0;
	}
	set_input_format( IF, test_phase );
	show_input_format( *Log(mylog) );
      }
    }
    return NumF;
  }

  double MBLClass::sum_remaining_weights( int level ){
    double result = 0.0;
    int i;
    for ( i = level; i < EffectiveFeatures(); i++ ){
      result += PermFeatures[i]->Weight();
    }
    return result;
  }
  
  void MBLClass::Initialize( int n ){ 
    if ( n > 0 )
      num_of_features = n;
    // Allocate memory. Will be reused again and again ....
    //
    Features = new Feature*[num_of_features];
    PermFeatures = new Feature*[num_of_features];
    FeatureStrings = new StringHash(); // all features share the same hash!
    TargetStrings = new StringHash();
    Targets = new Target( DefaultTargetNumber, 
			  TargetIncrement,
			  TargetStrings );
    int i;
    for ( i=0; i< num_of_features; i++ ){
      Features[i] = new Feature( DefaultFeatureNumber,
				 FeatureIncrement,
				 FeatureStrings );
      PermFeatures[i] = NULL; //Features[i];
    }
    ChoppedInput = new string[num_of_features+2];  
    //one extra to store the target and one for sample-weighting
    CurrInst = new Instance( num_of_features );
    // the user thinks about features running from 1 to Num
    // we know better, so shift one down.
    effective_feats = num_of_features;
    num_of_num_features = 0;
    int j;
    for ( j = 0; j < num_of_features; j++){
      switch (UserOptions[j+1] ){
      case Ignore:
	Features[j]->Ignore( true );
	effective_feats--;
	break;
      case Numeric:
	Features[j]->Numeric( true );
	num_of_num_features++;
	break;
      case DefaultMetric:
	if ( GlobalMetric == Numeric ){
	  Features[j]->Numeric( true );
	  num_of_num_features++;
	}
	break;
      case Overlap:
      case ValueDiff:
      case JeffreyDiv:
	break;
      default:
	FatalError( "Initialize: Illegal value in switch " +
		    to_string( UserOptions[j+1] ) );
      }
    }
    Options.FreezeTable();
    if ( Weighting > IG_w ||
	 TreeOrder >= X2Order )
      need_all_weights = true;
  }
  
} // namespace
