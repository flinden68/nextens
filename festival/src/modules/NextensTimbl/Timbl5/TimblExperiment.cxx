/*
 * TimblExperiment.cc
 *
 * Timbls main Class. 
 * Specialisation of MBLClass for different Timbl experiments...
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
#include <map>
#include <fstream>
#if __GNUC__ < 3
#include <strstream>
#else
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
#include <float.h>
#include <ctype.h>
#include <assert.h>
#include <stdarg.h>
#else
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cfloat>
#include <cctype>
#include <cassert>
#include <cstdarg>
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

#include "MBLClass.h"
#include "GetOptClass.h"
#include "ServerProcs.h"
#include "TimblExperiment.h"

using namespace std;
using namespace Timbl;
using TimblServer::RunServer;

TimblExperiment::TimblExperiment( const AlgorithmType Alg,
				  const string& s ):
  MBLClass( s ),
  Initialized( false ),
  OptParams( NULL ),
  algorithm( Alg ),
  CurrentDataFile( "" ),
  WFileName( "" ),
  estimate( 0 ),
  max_conn( 10 )
{
  Weighting = GR_w;
}

TimblExperiment::~TimblExperiment() {
  delete OptParams;
}

TimblExperiment& TimblExperiment::operator=( const TimblExperiment&in ){
  if ( this != &in ){
    MBLClass::operator=(in);
    Initialized = false;
    OptParams = NULL;
    algorithm = in.algorithm;
    CurrentDataFile = in.CurrentDataFile;
    WFileName = in.WFileName;
    estimate = in.estimate;
    max_conn = in.max_conn;
    Weighting = in.Weighting;
  }
  return *this;
}

TimblExperiment *TimblExperiment::CreateClient( int socketid ){
  TimblExperiment *result = 0;
  switch ( Algorithm() ){
  case IB1_a:
  case TRIBL_a: 
  case TRIBL2_a: 
  case IGTREE_a: 
    result = clone();
    break;
  default:
    FatalError( "You may not Create Clients for Special cases like " + 
		to_string(algorithm) );
  }
  *result = *this;
  string line = "Client on socket: " + to_string( socketid );
#ifdef USE_LOGSTREAMS
  result->myerr->message( line );
  result->mydebug->message( line );
#endif
  if ( OptParams ){
    result->OptParams = OptParams->Clone( socketid );
  }
  result->Socket( socketid );
  result->WFileName = WFileName;
  result->CurrentDataFile = CurrentDataFile;
  *Dbg(mydebug) << "created a new Client" << endl;
  *Dbg(result->mydebug) <<  "I am the new client" << endl;
  return result;
}

bool TimblExperiment::StartServer( const int port, const int max_c ){
  if ( !Init_MBL_test( true ) ){
    FatalError( "Couldn't initialize server" );
    return false;
  }
  RunServer( this, port );
  max_conn = max_c;
  return true;
}

bool TimblExperiment::SetSingleThreaded(){
#ifdef USE_LOGSTREAMS
  if ( !mylog->set_single_threaded_mode() ||
       !myerr->set_single_threaded_mode() ||
       !mydebug->set_single_threaded_mode() ){
    cerr << "problem setting to single threaded mode" << endl;
    return false;
  }
#endif
  return true;
}

inline bool next_line( istream &datafile, string& Line, 
		       const InputFormatType IF, int &SkippedLines ){
  // Function that takes a line from a file, skipping comment
  // returns true if some line is found
  //
  bool found = false;
  while ( !found && getline( datafile, Line ) ){
    if ( empty_line( Line, IF ) ){
      ++SkippedLines;
      continue;
    } else 
      found = true;
  }
  return found;
}  

/*
  First learning Phase:
  Learning of the names of the FeatureValues and TargetValues
  also their distribution etc.
*/
bool TimblExperiment::Prepare( const string& FileName ){
  bool result = false;
  if ( FileName != "" && ConfirmOptions() ){
    int Num;
    if ( !ExpInvalid() ){
      if ( Options.TableFrozen() ||
	   NumOfFeatures() != 0 ){
	Error( "couldn't learn from file '" + FileName +
	       "'\nInstanceBase already filled" );
      }
      else {
	Num = ExamineData( FileName, false );
	if ( Num == 0 ){
	  Error( "Unable to initialize from file :'" + FileName + "'\n" );
	}
	else {
	  if ( NumOfFeatures() == 0 ){
	    Initialize( Num );
	  }
	  CurrentDataFile = FileName;
	  if ( Verbosity(OPTIONS) ){
	    ShowSettings( *Log(mylog) );
	  }
	  // Open the file.
	  //
	  ifstream datafile( FileName.c_str(), ios::in);
	  int DataLines = 0, SkippedLines = 0;
	  string Buffer;
	  if ( InputFormat() == ARFF ){
	    while ( getline( datafile, Buffer ) &&
		    !compare_nocase_n( "@DATA", Buffer ) )
	      ++SkippedLines;
	  }
	  if ( !next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
	    Error( "no useful data in: " + FileName );
	    result = false;
	  }
	  else if ( !Chop( Buffer ) ){
	    ++SkippedLines;
	    Error( "no useful data in: " + FileName );
	    result = false;
	  }
	  else {
	    ++DataLines;
	    bool found;
	    bool go_on = ( IB2_offset() == 0 || DataLines <= IB2_offset() );
	    if ( !Verbosity(SILENT) ){
	      Info( "Phase 1: Reading Datafile: " + FileName );
	      time_stamp( "Start:     ", 0 );
	    }
	    while( go_on ){
	      (void)chopped_to_instance( LearnWords );
	      // Progress update.
	      //
	      if ((DataLines % Progress() ) == 0)
		time_stamp( "Examining: ", DataLines );
	      if ( IB2_offset() > 0 && DataLines >= IB2_offset() )
		go_on = false;
	      else {
		found = false;
		while ( !found && 
			next_line( datafile, Buffer, 
				   InputFormat(), SkippedLines ) ){
		  found = Chop( Buffer );
		  if ( found )
		    ++DataLines;
		  else {
		    ++SkippedLines;
		    Warning( "datafile, skipped line #" + 
			     to_string( DataLines+SkippedLines ) +
			     "\n" + Buffer );
		  }
		}
		go_on = found;
	      }
	    }
	    if ( DataLines < 1 ){
	      Error( "no useful data in: " + FileName );
	    }
	    else {
	      time_stamp( "Finished:  ", DataLines+SkippedLines );
	      time_stamp( "Calculating Entropy " );
	      if ( Verbosity(FEAT_W) ){
		*Log(mylog) << "Lines of data     : " << DataLines << endl;
		if ( SkippedLines != 0 )
		  *Log(mylog) << "SkippedLines      : "
			      << SkippedLines << endl;
		LearningInfo( *Log(mylog) );
	      }
	      result = true;
	    }
	  }
	}
      }
    }
  }
  return result;
}

bool CV_Experiment::Prepare( const string& f ){
  cerr << "CV prepare " << f << endl;
  return true;
}

#ifdef EXPERIMENTAL
bool TimblExperiment::Learn( const string& FileName ){
  //
  // Second Learning Phase:
  // Build an Instance Base
  //
  bool result = true;
  if ( ExpInvalid() ||
       !ConfirmOptions() ){
    result = false;
  }
  else if ( CurrentDataFile == "" ){
    if ( FileName == "" ){
      Warning( "unable to build an InstanceBase: No datafile defined yet" );
      result = false;
    }
    else {
      if ( !Prepare( FileName ) || ExpInvalid() ){
	result = false;
      }
    }
  }
  else if ( FileName != "" &&
	    CurrentDataFile != FileName ){
    Error( "Unable to Learn from file '" + FileName + "'\n"
	   "while previously instantiated from file '" + 
	   CurrentDataFile + "'" );
    result = false;
  }
  if ( result ) {
    InitInstanceBase();
    MultiIndex multi_index;
    result = build_file_index( CurrentDataFile, multi_index );
    if ( result ){
      if ( !Verbosity(SILENT) ) {
	Info( "\nPhase 3: Learning from Datafile: " + CurrentDataFile );
	time_stamp( "Start:     ", 0 );
      }
      string Buffer;
      int dummy1;
      int count;
      const Instance *NewInst;
      IB_InstanceBase *PartInstanceBase = 0;
      // Open the file.
      //
      ifstream datafile( CurrentDataFile.c_str(), ios::in);
      //
      Feature *Feat =  Features[Permutation[0]];
      int lines =0;
      for ( count=0; count< Feat->ArraySize(); ++count ){
	FeatureValue *the_fv = Feat->Value(count);
	//	Info( "start handling feature '" + the_fv->Name() + "'" );
	PartInstanceBase = new IB_InstanceBase( EffectiveFeatures(), 
						(RandomSeed()>=0) );
	typedef MultiIndex::const_iterator mit;
	pair<mit,mit> b = multi_index.equal_range( the_fv );
	for ( mit It=b.first; It!= b.second; ++It ){
	  //	  Info( "seek line " + itos(  (*It).second ) );
	  datafile.seekg( (*It).second );
	  next_line( datafile, Buffer, InputFormat(), dummy1 );
	  // Progress update.
	  //
	  if (( ++lines % Progress() ) == 0) 
	    time_stamp( "Learning:  ", lines );
	  Chop( Buffer );
	  NewInst = chopped_to_instance( TrainWords );
	  PartInstanceBase->AddInstance( NewInst );
	}
	if ( PartInstanceBase ){
	  //	  Info( "finished handling feature " + the_fv->Name() );
	  if ( !InstanceBase->Merge( PartInstanceBase, the_fv ) ){
	    FatalError( string("Merging InstanceBases failed, are you sure")
			+ " that the inputfile is sorted on the most"
			+ " significant feature?" );
	    return false;
	  }
	  delete PartInstanceBase;
	  PartInstanceBase = 0;
	}
      }
      time_stamp( "Finished:  ", lines );
      if ( !Verbosity(SILENT) )
	IBInfo( *Log(mylog) );
    }
  }
  return result;
}
#else
bool TimblExperiment::Learn( const string& FileName ){
  bool result = true;
  if ( ExpInvalid() ||
       !ConfirmOptions() ){
    result = false;
  }
  else if ( CurrentDataFile == "" )
    if ( FileName == "" ){
      Warning( "unable to build an InstanceBase: No datafile defined yet" );
      result = false;
    }
    else {
      if ( !Prepare( FileName ) || ExpInvalid() ){
	result = false;
      }
    }
  else if ( FileName != "" &&
	    CurrentDataFile != FileName ){
    Error( "Unable to Learn from file '" + FileName + "'\n"
	   "while previously instantiated from file '" + 
	   CurrentDataFile + "'" );
    result = false;
  }
  if ( result ) {
    string Buffer;
    int DataLines = 0, SkippedLines = 0;  
    // Open the file.
    //
    ifstream datafile( CurrentDataFile.c_str(), ios::in);
    if ( InputFormat() == ARFF )
      while ( getline( datafile, Buffer ) &&
	      !compare_nocase_n( "@DATA", Buffer ) )
	++SkippedLines;
    if ( !next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
      Error( "cannot start learning from in: " + CurrentDataFile );
      result = false;    // No more input
    }
    else if ( !Chop( Buffer ) ){
      ++SkippedLines;
      Error( "no useful data in: " + CurrentDataFile );
      result = false;    // No more input
    }
    else {
      ++DataLines;
      InitInstanceBase( );
      if ( !Verbosity(SILENT) ) {
	Info( "Phase 2: Learning from Datafile: " + CurrentDataFile );
	time_stamp( "Start:     ", 0 );
      }
      bool found;
      bool go_on = ( IB2_offset() == 0 || DataLines <= IB2_offset() );
      const Instance *NewInst;
      while( go_on ){ 
	// The next Instance to store. 
	NewInst = chopped_to_instance( TrainWords );
	if ( !IBAdd( NewInst ) ){
	  Warning( "deviating exemplar weight in line #" +
		   to_string(DataLines+SkippedLines) + ":\n" +
		   Buffer + "\nIgnoring the new weight" );
	}
	// Progress update.
	//
	if ((DataLines % Progress() ) == 0) 
	  time_stamp( "Learning:  ", DataLines );
	if ( IB2_offset() > 0 && DataLines >= IB2_offset() )
	  go_on = false;
	else {
	  found = false;
	  while ( !found && 
		  next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
	    found = Chop( Buffer );
	    if ( found )
	      ++DataLines;
	    else {
	      ++SkippedLines;
	      Warning( "datafile, skipped line #" + 
		       to_string( DataLines+SkippedLines ) +
		       "\n" + Buffer );
	    }
	  }
	  go_on = found;
	}
      }
      time_stamp( "Finished:  ", DataLines );
      if ( !Verbosity(SILENT) )
	IBInfo( *Log(mylog) );
    }
  }
  return result;
}
#endif

bool CV_Experiment::Learn( const string& f ){
  cerr << "CV Learn " << f << endl;
  return true;
}

IB1_Experiment::IB1_Experiment( const int N,
				const string& s,
				const bool init ):
  TimblExperiment( IB1_a, s ){
  if ( init ) InitClass( N );
  TreeOrder = GRoverFeature;
}

/*
  Increment the Instancebase with one instance (IB1 Class only)
*/
bool IB1_Experiment::Increment( const string& InstanceString ){
  bool result = true;
  if ( ExpInvalid() ){
    result = false;
  }
  else if ( IBStatus() == Invalid ){
    Warning( "unable to Increment, No InstanceBase available" );
    result = false;
  }
  else {
    if ( !Chop( InstanceString ) ){
      Error( "Couldn't convert to Instance: " + InstanceString );
      result = false;    // No more input
    }
    else {
      if ( !IBAdd( chopped_to_instance( TrainLearnWords ) ) ){
	Warning( "deviating exemplar weight in:\n" + 
		 InstanceString + "\nIgnoring the new weight" );
      }
    }
  }
  return result;
}

/*
  Decrement the Instancebase with one instance (IB1 Class only)
*/
bool IB1_Experiment::Decrement( const string& InstanceString ){
  bool result = true;
  if ( ExpInvalid() ){
    result = false;
  }
  else if ( IBStatus() == Invalid ){
    Warning( "unable to Decrement, No InstanceBase available" );
    result = false;
  }
  else {
    if ( !Chop( InstanceString ) ){
      Error( "Couldn't convert to Instance: " + InstanceString );
      result = false;    // No more input
    }
    else {
      HideInstance( chopped_to_instance( TestWords ) );
    }
  }
  return result;
}

/*
  Expand  an Instance Base (IB1 only)
*/
bool IB1_Experiment::Expand( const string& FileName ){
  bool result = true;
  if ( ExpInvalid() ){
    result = false;
  }
  else if ( IBStatus() == Invalid ){
    Warning( "unable to expand the InstanceBase: Not there" );
    result = false;
  }
  else if ( FileName == "" ){
    Warning( "unable to expand the InstanceBase: No inputfile specified" );
    result = false;
  }
  else {
    string Buffer;
    int DataLines = 0, SkippedLines = 0;  
    // Open the file.
    //
    ifstream datafile( FileName.c_str(), ios::in);
    if ( InputFormat() == ARFF )
      while ( getline( datafile, Buffer ) &&
	      !compare_nocase_n( "@DATA", Buffer) )
	++SkippedLines;
    if ( !next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
      Error( "no useful data in: " + FileName );
      result = false;    // No more input
    }
    else if ( !Chop( Buffer ) ){
      ++SkippedLines;
      Error( "no useful data in: " + FileName );
      result = false;    // No more input
    }
    else {
      ++DataLines;
      if ( !Verbosity(SILENT) ) {
	Info( "Phase 2: Expanding from Datafile: " + FileName );
	time_stamp( "Start:     ", 0 );
      }
      bool found;
      do {
	// The next Instance to store. 
	if ( !IBAdd( chopped_to_instance( TrainLearnWords ) ) ){
	  Warning( "deviating exemplar weight in line #" + 
		   to_string(DataLines+SkippedLines ) + ":\n" +
		   Buffer + "\nIgnoring the new weight" );	
}
	// Progress update.
	//
	if ((DataLines % Progress() ) == 0) 
	  time_stamp(  "Learning:  ", DataLines );
	found = false;
	while ( !found && next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
	  found = Chop( Buffer );
	  if ( found )
	    ++DataLines;
	  else {
	    ++SkippedLines;
	    Warning( "datafile, skipped line #" + 
		     to_string( DataLines+SkippedLines ) +
		     "\n" + Buffer );
	  }
	}
      } while( found );
      time_stamp( "Finished:  ", DataLines );
      if ( !Verbosity(SILENT) )
	IBInfo( *Log(mylog) );
    }
  }
  return result;
}

/*
  Remove Instances from an Instance Base (IB1 only )
*/
bool IB1_Experiment::Remove( const string& FileName ){
  bool result = true;
  if ( ExpInvalid() ){
    result = false;
  }
  else if ( IBStatus() == Invalid ){
    Warning( "unable to remove from InstanceBase: Not there" );
    result = false;
  }
  else if ( FileName == "" ){
    Warning( "unable to remove from InstanceBase: No input specified" );
    result = false;
  }
  else {
    string Buffer;
    int DataLines = 0, SkippedLines = 0;  
    // Open the file.
    //
    ifstream datafile( FileName.c_str(), ios::in);
    if ( InputFormat() == ARFF )
      while ( getline( datafile, Buffer ) &&
	      !compare_nocase_n( "@DATA", Buffer ) )
	++SkippedLines;
    if ( !next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
      Error( "no useful data in: " + FileName );
      result = false;    // No more input
    }
    else if ( !Chop( Buffer ) ){
      ++SkippedLines;
      Error( "no useful data in: " + FileName );
      result = false;    // No more input
    }
    else {
      ++DataLines;
      if ( !Verbosity(SILENT) ) {
	Info( "Phase 2: Removing using Datafile: " + FileName );
	time_stamp( "Start:     ", 0 );
      }
      bool found;
      do {
	// The next Instance to remove. 
	HideInstance( chopped_to_instance( TestWords ) );
	// Progress update.
	//
	if ((DataLines % Progress() ) == 0) 
	  time_stamp( "Removing:  ", DataLines );
	found = false;
	while ( !found && 
		next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
	  found = Chop( Buffer );
	  if ( found )
	    ++DataLines;
	  else {
	    ++SkippedLines;
	    Warning( "datafile, skipped line #" + 
		     to_string( DataLines+SkippedLines ) +
		     "\n" + Buffer );
	  }
	}
      } while( found );
      time_stamp( "Finished:  ", DataLines );
      if ( !Verbosity(SILENT) )
	IBInfo( *Log(mylog) );
    }
  }
  return result;
}

inline void TimblExperiment::show_progress( int line, time_t start ){
  char time_string[26];
  struct tm *curtime;
  time_t Time;
  time_t SecsUsed;
  time_t EstimatedTime;
  double Estimated;
  int local_progress = Progress();

  if ( ( (line % local_progress ) == 0) || ( line <= 10 ) ||
       ( line == 100 || line == 1000 || line == 10000 ) ){
    time(&Time);
    if ( line == 1000 ){
      // check if we are slow, if so, change progress value
      if ( Time - start > 120 ) // more then two minutes
	// very slow !
	local_progress = 1000;
    }
    else if ( line == 10000 ){
      if ( Time - start > 600 ) // more then ten minutes
	// quit slow !
	local_progress = 10000;
    }
    curtime = localtime(&Time);
#if __GNUC__ < 3
    ostrstream ostr;
#else
    stringstream ostr;
#endif
    ostr << "Tested: ";
    ostr.width(6);
    ostr.setf(ios::right, ios::adjustfield);
    strcpy( time_string, asctime(curtime));
    time_string[24] = '\0';
    ostr << line << " @ " << time_string;
    
    // Estime time until Estimate.
    //
    if ( Estimate() > 0 ) {
      SecsUsed = Time - start;
      if ( SecsUsed > 0 ) {
	Estimated = (SecsUsed / (float)line) * 
	  (float)Estimate();
	EstimatedTime = (long)Estimated + start;
	ostr << ", ";
	strcpy(time_string, ctime(&EstimatedTime));
	time_string[24] = '\0';
	ostr << Estimate() << ": " << time_string;
      } 
    }
#if __GNUC__ < 3
    ostr << ends;
    char *pnt = ostr.str();
    Info( pnt );
    delete [] pnt;
#else
    Info ( ostr.str() );
#endif
  }
}

class ConfusionMatrix: public MsgClass {
  int size;
  int **mat;
public:
  ConfusionMatrix( int );  
  virtual ~ConfusionMatrix();
  void Increment( const TargetValue*, const TargetValue* );
  void Print( ostream&, Target * );
  void FScore( ostream&, Target *, bool );
};

ConfusionMatrix::ConfusionMatrix( int s): size(s){
  try {
    mat = new int*[size+1];
    for ( int i=0; i <= size; ++i ){
      mat[i] = new int[size];
      for ( int j=0; j < size; ++j )
	mat[i][j] = 0;
    }
  }
  catch( bad_alloc ){
    Error ( "Not enough memory for ConfusionMatrix" );
    throw;

  }
}

ConfusionMatrix::~ConfusionMatrix(){
  for ( int i=0; i <= size; ++i )
    delete [] mat[i];
  delete [] mat;
}

void ConfusionMatrix::Increment( const TargetValue *t1, 
				 const TargetValue *t2 ){
  if ( t2 ){
    if ( t1 )
      ++mat[t1->Index()][t2->Index()];
    else
      ++mat[size][t2->Index()];
  }
  else
    throw std::out_of_range( "ConfusionMatrix, index out of range" );
}

void ConfusionMatrix::Print( ostream& os, Target* tg ){
  os << "\nConfusion Matrix:" << endl;
  os << "        ";
  int i;
  for ( i=0; i < size; i++ ){
    // Print the class names.
    os.width(6);
    os.setf(ios::right, ios::adjustfield);
    os << tg->Value(i) << " ";
  }
  os << endl;
  os << "        ";
  for ( i=0; i < size; i++ )
    os << "-------";
  os << endl;
  for ( i=0; i <= size; i++ ){
    os.width(6);
    os.setf(ios::right, ios::adjustfield);
    if ( i == size )
      os <<  "   -*- | ";
    else
      os << tg->Value(i) << " | ";
    int j;
    for ( j=0; j < size; j++ ){
      os.width(6);
      os.setf(ios::right, ios::adjustfield);
      os << mat[i][j] << " ";
    }
    os << endl;
  }
  os << endl;
}

void pf( ostream&os, double d ){
  if ( d == -DBL_MAX )
    os << " \t(nan)\t";
  else
    os << " \t" << d;
}

void ConfusionMatrix::FScore( ostream& os, Target* tg, bool cs_too ){
  double maf = 0.0;
  double mif = 0.0;
  double maa = 0.0;
  double mia = 0.0;
  if ( cs_too ){
    os << "\nScores per Value Class:" << endl;
    os << "class\tTP\tFP\tTN\tFN\tprecision\trecall(TPR)\tFPR\t\tF-score\t\tAUC" << endl;
  }
  for ( int i=0; i < size; ++i ){
    unsigned int TP = 0;
    unsigned int FP = 0;
    unsigned int FN = 0;
    unsigned int TN = 0;
    for ( int j=0; j < size; ++j ){
      if ( i == j ){
	TP = mat[i][j];
      }
      else
	FN += mat[i][j];
    }
    for ( int j=0; j <= size; ++j ){
      if ( j != i )
	FP += mat[j][i];
    }
    for ( int j=0; j <= size; ++j ){
      if ( j != i )
	for ( int k=0; k < size; ++k ){
	  if ( k != i )
	    TN += mat[j][k];
	}
    }
    double precision;
    if ( TP + FP == 0 )
      precision = -DBL_MAX;
    else
      precision = TP / double(TP + FP);
    double TPR;
    if ( TP + FN == 0 )
      TPR = -DBL_MAX;
    else
      TPR = TP / double(TP + FN);
    double FPR;
    if ( FP + TN == 0 )
      FPR = -DBL_MAX;
    else
      FPR = FP / double(FP + TN);
    double FScore;
    if ( precision == -DBL_MAX || TPR == -DBL_MAX ||
	 precision + TPR == 0 ){
      FScore = -DBL_MAX;
    }
    else {
      FScore = ( 2 * precision * TPR ) / (precision + TPR );
      maf += FScore;
      mif += (FScore * tg->Value(i)->ValFreq());
    }
    double AUC;
    if ( TPR == -DBL_MAX || FPR == -DBL_MAX ){
      AUC = -DBL_MAX;
    }
    else {
      AUC = ( 0.5 * TPR * FPR ) + ( TPR * ( 1.0 - FPR ) ) + 
	( 0.5 * ( ( 1.0 - TPR ) * ( 1.0 - FPR ) ) );
      maa += AUC;
      mia += (AUC * tg->Value(i)->ValFreq());
    }
    if ( cs_too ){
      os << tg->Value(i) << " | "
	 << " \t" << TP << " \t" << FP << " \t" << TN << " \t" << FN;
      pf(os,precision);
      pf(os,TPR);
      pf(os,FPR);
      pf(os,FScore);
      pf(os,AUC);
      os << endl;
    }
  }
  maf = maf / tg->EffectiveValues();
  mif = mif / tg->TotalValues();
  maa = maa / tg->EffectiveValues();
  mia = mia / tg->TotalValues();
  os << "\nF-Score beta=1, microav: " << mif << endl;
  os << "F-Score beta=1, macroav: " << maf << endl;
  os << "AUC, microav:            " << mia << endl;
  os << "AUC, macroav:            " << maa << endl;
}

inline void TimblExperiment::show_summary( ostream& os, int lines, 
					   int correct, int exact,
					   int correct_ties, int wrong_ties,
					   time_t Start,
					   ConfusionMatrix *confusion,
					   const string& PercFileName ){
  time_t Time;
  time_t SecsUsed;

  time(&Time);
  SecsUsed = Time - Start;
  if ( SecsUsed == 0 ) // avoid divide-by-zero!
    ++SecsUsed;
  os.setf( ios::fixed, ios::floatfield );
  os << "Seconds taken: " << SecsUsed << " (";
  os << setprecision(2);
  os << lines / (float)SecsUsed << " p/s)" << endl;
  os << setprecision(6);
  if ( confusion )
    confusion->FScore( *Log(mylog), Targets, Verbosity(CLASS_STATS) );
  os << "overall accuracy:        " << correct/(double) lines << "  (" << correct << "/" << lines  << ")" ;
  if ( exact != 0 )
    os << ", of which " << exact << " exact matches " ;
  os << endl;
  if ( confusion && Verbosity(CONF_MATRIX) )
    confusion->Print( *Log(mylog), Targets );
  int TotalTies =  wrong_ties + correct_ties;
  if ( TotalTies > 0 ){
    os << "\nThere were " << TotalTies << " ties";
    double tie_perc = 100 * (correct_ties / (double)TotalTies);
    os << " of which " << correct_ties 
       << " (" << setprecision(2)
       << tie_perc << setprecision(6) << "%)"
       << " were correctly resolved" << endl;
  }
  if ( PercFileName != "" ) {
    ofstream outfile( PercFileName.c_str(), ios::out | ios::trunc);
    if (!outfile) {
      Warning( "can't open: " + PercFileName );
    }
    else {
      outfile << (correct / (float)lines) * 100.0 << endl
	      << "tested " << lines << " lines " << endl
	      << "correct " << correct << " lines " << endl;
      outfile.close();
    }
  }
}

void TimblExperiment::show_results( ostream &outfile, 
				    const ValueDistribution& Dist,
				    const TargetValue *Best,
				    const double Distance ){
  show_org_input( outfile );
  outfile << Best;
  if ( Verbosity(DISTRIB) ){
    outfile << " " << Dist;
  }
  if ( Verbosity(DISTANCE) ) {
    int OldPrec = outfile.precision(DBL_DIG-1);
    outfile.setf(ios::showpoint);
    outfile.width(8);
    outfile << " " << Distance;
    outfile.precision(OldPrec);
  }
  outfile << endl;
  ShowBestNeighbors( outfile, Verbosity(DISTANCE), Verbosity(DISTRIB) );
}

bool IB2_Experiment::Prepare( const string& FileName  ){
  if ( ConfirmOptions() && IB2_offset() < 0 ){
    Error( "IB2 learning failed, invalid bootstrap option?" );
    return false;
  }
  else
    return TimblExperiment::Prepare( FileName );
}

bool IB2_Experiment::Learn( const string& FileName  ){
  bool result = false;
  if ( IB2_offset() < 0 )
    Error( "IB2 learning failed, invalid bootstrap option?" );
  else if ( TimblExperiment::Learn( FileName ) )
    result = Expand_N( FileName );
  return result;
}

bool IB2_Experiment::Expand( const string& FileName ){
  bool result = false;
  if ( CurrentDataFile == "" ){
    Warning( "IB2, cannot Append data: No datafile bootstrapped yet" );
  }
  else {
    IB2_offset( 0 );
    result = Expand_N( FileName );
  }
  return result;
}

bool IB2_Experiment::Remove( const string& ){
  Warning( "IB2, remove impossible, (ignored) " );
  return false;
}

bool IB2_Experiment::Expand_N( const string& FileName ){
  bool result = true;
  int Added = 0;
  if ( ExpInvalid() ){
    result = false;
  }
  else if ( CurrentDataFile == "" ){
    Warning( "IB2, cannot Append data: No datafile bootstrapped yet" );
    result = false;
  }
  else if ( IBStatus() == Invalid ){
    Warning( "unable to expand the InstanceBase: Not there" );
    result = false;
  }
  else if ( !Init_MBL_test( ) )
    Error( "unable to initialize testing" );
  else {
    string file_name;
    if ( FileName == "" )
      file_name = CurrentDataFile;
    else
      file_name = FileName;
    string Buffer;
    int DataLines = 0, SkippedLines = 0;  
    // Open the file.
    //
    ifstream datafile( file_name.c_str(), ios::in);
    if ( InputFormat() == ARFF )
      while ( getline( datafile, Buffer ) &&
	      !compare_nocase_n( "@DATA", Buffer ) )
	++SkippedLines;
    if ( !next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
      Error( "no useful data in: " + file_name );
      result = false;    // No more input
    }
    else if ( !Chop( Buffer ) ){
      ++SkippedLines;
      Error( "no useful data in: " + file_name );
      result = false;    // No more input
    }
    else {
      ++DataLines;
      while ( DataLines <= IB2_offset() ){
	if ( !next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
	  Error( "not enough lines to skip in " + FileName );
	  result = false;
	  break;
	}
	else if ( Chop( Buffer ) )
	  ++DataLines;
	else {
	  ++SkippedLines;
	  Warning( "datafile, skipped line #" + 
		   to_string( DataLines+SkippedLines ) +
		   "\n" + Buffer );
	}
      }
      if ( result ){
	if ( !Verbosity(SILENT) ) {
	  Info( "Phase 2: Appending from Datafile: " + FileName + 
		" (starting at line " + to_string( DataLines ) + ")" );
	  time_stamp( "Start:     ", DataLines );
	}
	bool found;
	const Instance *NewInst;
	do {
	  // The next Instance to store. 
	  if ( Init_MBL_test( ) )
	    NewInst = chopped_to_instance( TestWords );
	  else {
  	    Error( "problem to continue IB2 expanding " );
  	    result = false;
  	    break;
	  }
	  int status;
	  double final_distance;
	  bool Tie = false;
	  const TargetValue *ResultTarget = LocalClassify( NewInst, 
							   status,
							   final_distance,
							   Tie );
	  if ( ResultTarget != NewInst->TV ) {
	    NewInst = chopped_to_instance( TrainLearnWords );
	    if ( !IBAdd( NewInst ) ){
	      Warning( "deviating exemplar weight in line #" + 
		       to_string(DataLines+SkippedLines ) + ":\n" +
		       Buffer + "\nIgnoring the new weight" );
	    }
	    *Dbg(mydebug) <<"adding " << NewInst << endl;
	    ++Added;
	  }
	  // Progress update.
	  //
	  if ((DataLines % Progress() ) == 0) 
	    time_stamp( "Learning:  ", DataLines );
	  found = false;
	  while ( !found && next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
	    found = Chop( Buffer );
	    if ( found )
	      ++DataLines;
	    else {
	      ++SkippedLines;
	      Warning( "datafile, skipped line #" + 
		       to_string( DataLines+SkippedLines ) +
		       "\n" + Buffer );
	    }
	  }
	} while( found );
	if ( result ){
	  time_stamp( "Finished:  ", DataLines );
	  *Log(mylog) << "added " << Added << " new entries" << endl;
	  if ( !Verbosity(SILENT) ){
	    IBInfo( *Log(mylog) );
	    LearningInfo( *Log(mylog) );
	  }
	}
      }
    }
  }
  return result;
}

bool IB1_Experiment::test_options_ok( const string& FileName ){
  bool result = false;
  if ( !ExpInvalid() &&
       ConfirmOptions() ){
    int i;
    if ( IBStatus() == Invalid )
      Warning( "you tried to apply the " + to_string( algorithm ) +
	       " algorithm, but no Instance Base is available yet" );
    else if ( IBStatus() == Pruned )
      Warning( "you tried to apply the " + to_string( algorithm) +
	       " algorithm on a pruned Instance Base" );
    else if ( FileName != "" &&
	      ( i = ExamineData( FileName, true )) != NumOfFeatures() ){
      if ( i == 0 ){
	Error( "unable to use the data from '" + FileName +
	       "', wrong Format?" );
      }
      else
	Error( "mismatch between number of features in Testfile " +
	        FileName + " and the Instancebase (" +
	       to_string(i) + " vs. " + to_string(NumOfFeatures()) + ")" ); 
    }
    else if ( !Init_MBL_test( ) )
      Warning( to_string( algorithm ) + 
	       " experiment: couldn't proceed due to errors.." );
    else {
      result = true;
    }
  }
  return result;
}

bool IB1_Experiment::classify_options_ok( const string& line ){
  int i;
  InputFormatType IF = InputFormat();
  bool result = false;
  if ( !ExpInvalid() &&
       ConfirmOptions() ) {
    if ( (i = ExamineLine( line, IF, true )) != NumOfFeatures() ){
      if ( i > 0 )
	Warning( "mismatch between number of features in testline " +
		 line + " and the Instancebase (" + to_string(i)
		 + " vs. " + to_string(NumOfFeatures()) + ")" ); 
    }
    else { 
      if ( Initialized ){
	result = true;
      }
      else if ( IBStatus() == Invalid )
	Warning( "no Instance Base is available yet" );
      else if ( IBStatus() == Pruned )
	Warning( "you tried to apply the IB1 algorithm on a pruned"
		 " Instance Base" );
      else if ( TRIBL_offset() != 0 )
	Error( "IB1 algorithm impossible while treshold > 0\n"
	       "Please use TRIBL" );
      else if ( !set_input_format( IF, true ) ){
	Error( "Couldn't set input format to " + to_string( IF ) );
      }
      else {
	if ( Verbosity(NEAR_N) ){
	  Do_Exact( false );
	}
	Initialized = true;
	result = true;
      }
    }
  }
  return result;
}


bool IB2_Experiment::test_options_ok( const string& FileName ){
  if ( IB1_Experiment::test_options_ok( FileName ) )
    if ( IB2_offset() < 0 ){
      Error( "missing bootstrap information for IB2 algorithm." );
      return false;
    }
    else
      return true;
  return false;
}

bool TRIBL_Experiment::test_options_ok( const string& FileName ){
  bool result = false;
  if ( !ExpInvalid() &&
       ConfirmOptions() ){
    int i;
    if ( IBStatus() == Invalid )
      Warning( "you tried to apply the TRIBL algorithm, but no Instance"
	       " Base is available yet" );
    else if ( IBStatus() == Pruned )
      Warning( "you tried to apply the TRIBL algorithm on a pruned"
	       " Instance Base" );
    else if ( TRIBL_offset() < 0 )
      Error( "TRIBL algorithm impossible while treshold < 0\n" );
    else if ( FileName != "" && 
	      ( i = ExamineData( FileName, true )) != NumOfFeatures() ){
      if ( i == 0 ){
	Error( "unable to use the data from '" + FileName +
	       "', wrong Format?" );
      }
      else
	Error( "mismatch between number of features in Testfile " +
	       FileName + " and the Instancebase (" + to_string(i) + "vs. "
	       + to_string(NumOfFeatures()) + ")" ); 
    }
    else {
      if ( !Init_MBL_test( ) )
	Warning( "TRIBL experiment: couldn't proceed due to errors.." );
      else
	result = true;
    }
  }
  return result;
}

bool TRIBL2_Experiment::test_options_ok( const string& FileName ){
  bool result = false;
  if ( !ExpInvalid() &&
       ConfirmOptions() ){
    int i;
    if ( IBStatus() == Invalid )
      Warning( "you tried to apply the TRIBL2 algorithm, but no Instance"
	       " Base is available yet" );
    else if ( IBStatus() == Pruned )
      Warning( "you tried to apply the TRIBL2 algorithm on a pruned"
	       " Instance Base" );
    else if ( FileName != "" && 
	      ( i = ExamineData( FileName, true )) != NumOfFeatures() ){
      if ( i == 0 ){
	Error( "unable to use the data from '" + FileName + 
	       "', wrong Format?" );
      }
      else
	Error( "mismatch between number of features in Testfile " +
	       FileName + " and the Instancebase (" + to_string(i) + "vs. "
	       + to_string(NumOfFeatures()) + ")" ); 
    }
    else {
      if ( !Init_MBL_test( ) )
	Warning( "TRIBL2 experiment: couldn't proceed due to errors.." );
      else
	result = true;
    }
  }
  return result;
}

bool LOO_Experiment::test_options_ok( const string& FileName ){
  if (  Do_Samples() ){
    FatalError( "Cannot Leave One Out on a file with Examplar Weighting" );
    return false;
  }
  else
    return IB1_Experiment::test_options_ok( FileName );
}

bool CV_Experiment::test_options_ok( const string& FileName ){
  if ( Do_Samples() ){
    FatalError( "Cannot Cross validate on a file with Examplar Weighting" );
    return false;
  }
  else {
    bool result = IB1_Experiment::test_options_ok( FileName );
    if ( result && Verbosity(FEAT_W) ){
      LearningInfo( *Log(mylog) );
    }
    return result;
  }
}

bool IG_Experiment::test_options_ok( const string& FileName ){
  int i;
  bool result = false;
  if ( !ExpInvalid() &&
       ConfirmOptions() ){
    if ( num_of_neighbors != 1 ){
      Warning( "number of neighbors set to 1 for IGTree test!" );
      num_of_neighbors = 1;
    }
    if ( decay_flag  != Zero ){
      Warning( "Decay impossible for IGTree test, (k=1)" );
      decay_flag = Zero;
    }
    if ( GlobalMetric != Overlap ){
      Warning( "Metric set to Overlap for IGTree test." );
      GlobalMetric = Overlap;
    }
    if ( IBStatus() == Invalid )
      Warning( "you tried to apply the IGTree algorithm, but "
	       "no Instance Base is available" );
    else if ( IBStatus() != Pruned )
      Warning( "you tried to apply the IGTree algorithm on a complete,"
	       "(non-pruned) Instance Base" );
    else if ( ( i = ExamineData( FileName, true )) != NumOfFeatures() ){
      if ( i == 0 ){
	Error( "unable to use the data from '" + FileName + 
	       "', wrong Format?" );
      }
      else
	Error( "mismatch between number of features in Testfile " +
	       FileName + " and the Instancebase (" + to_string(i) + "vs. "
	       + to_string(NumOfFeatures()) + ")" ); 
    }
    else {
      InitWeights();
      result = true;
    }
  }
  return result;
}


bool IG_Experiment::classify_options_ok( const string& line ){
  int i;
  InputFormatType IF = InputFormat();
  bool result = false;
  if ( !ExpInvalid() &&
       ConfirmOptions() ){
    if ( num_of_neighbors != 1 ){
      Warning( "number of neighbors set to 1 for IGTree test!" );
      num_of_neighbors = 1;
    }
    if ( decay_flag != Zero ){
      Warning( "Decay impossible for IGTree test, (k=1)" );
      decay_flag = Zero;
    }
    if ( GlobalMetric != Overlap ){
      Warning( "Metric set to Overlap for IGTree test." );
      GlobalMetric = Overlap;
    }
    if ( ( i = ExamineLine( line, IF, true )) != NumOfFeatures() ){
      if ( i > 0 )
	Warning( "mismatch between number of features in testline " +
		 line + " and the Instancebase (" + to_string(i) + 
		 " vs. " + to_string(NumOfFeatures()) + ")" ); 
    }
    else {
      if ( Initialized ){
	result = true;
      }
      else if ( IBStatus() == Invalid )
	Warning( "you tried to apply the IGTree algorithm, but "
		 "no Instance Base is available" );
      else if ( IBStatus() != Pruned )
	Warning( "you tried to apply the IGTree algorithm on a complete,"
		 "(non-pruned) Instance Base" );
      else if ( !set_input_format( IF, true ) ){
	Error( "Couldn't set input format to " + to_string( IF ) );
      }
      else {
	InitWeights();
	Initialized = true;
	result = true;
      }
    }
  }
  return result;
}

bool TimblExperiment::Classify( const string& Line, 
				string& Result,
				string& Dist,
				double& Distance ){
  bool Tie;
  const TargetValue *targ = Classify( Line, Distance, Tie );
  if ( targ ){
    Result = targ->Name();
    BestDistrib.DistToString( Dist );
    return true;
  }
  return false;
}

bool TimblExperiment::Classify( const string& Line, 
				string& Result,
				double& Distance ){
  bool Tie;
  const TargetValue *targ = Classify( Line, Distance, Tie );
  if ( targ ){
    Result = targ->Name();
    return true;
  }
  return false;
}

bool TimblExperiment::Classify( const string& Line, 
				string& Result ) {
  double dummy;
  bool Tie;
  const TargetValue *targ = Classify( Line, dummy, Tie );
  if ( targ ){
    Result = targ->Name();
    return true;
  }
  return false;
}

const TargetValue *TimblExperiment::LocalClassify( const Instance *Inst,
						   int &status,
						   double &Distance,
						   bool &Tie ){
  bool recurse = true;
  Tie = false;
  bool flag = false;
  const ValueDistribution *ResultDist = ExactMatch( Inst, status );
  if ( ResultDist ){
    Distance = 0.0;
    recurse = !Do_Exact();
    // no retesting when exact match and the user ASKED for them..
    flag = true;
  }
  else
    ResultDist = TestInstance( Inst, Distance, InstanceBase );
  const TargetValue *Res;
  Res = ( ProbabilisticDraw()?
	  ResultDist->BestTarget():
	  ResultDist->BestTarget( Tie, (RandomSeed() >= 0)) );
  if ( Tie && recurse ){
    bool Tie2 = true;
    double Distance2;
    ++num_of_neighbors;
    Init_MBL_test();
    const TargetValue *Res2;
    const ValueDistribution *ResultDist2 = TestInstance( Inst, Distance2, InstanceBase );
    Res2 = ( ProbabilisticDraw()?
	     ResultDist2->BestTarget():
	     ResultDist2->BestTarget( Tie2, (RandomSeed() >= 0)) );
    --num_of_neighbors;
    if ( !Tie2 ){
      Distance = Distance2;
      Res = Res2;
      ResultDist = ResultDist2; 
    }
    else {
      ResultDist = GetDistrib( num_of_neighbors -1 );
    }
  }
  if ( flag )
    BestDistrib.SpecialVDCopy( *ResultDist );
  else
    BestDistrib.VDCopy( *ResultDist );
  NormalizeResult();
  return Res;
}

const TargetValue *TimblExperiment::Classify( const string& Line, 
					      double &Distance,
					      bool &Tie ){
  Distance = -1.0;
  const TargetValue *BestT = NULL;
  if ( Init_MBL_test( ) &&
       classify_options_ok( Line ) &&
       Chop( Line ) ){
    int status;
    const Instance *NewInst = chopped_to_instance( TestWords );
    Tie = false;
    BestT = LocalClassify( NewInst, status, Distance, Tie );
  }
  return BestT;
}
  
const TargetValue *IG_Experiment::LocalClassify( const Instance *,
						 int &,
						 double &,
						 bool & ){
  FatalError( "IG_Local_Classify may never be called!" );
  return NULL;
}

const TargetValue *IG_Experiment::IG_Classify( const Instance *Inst,
					       double &Distance, 
					       bool &Tie ){
  int level = -1;
  Tie = false;
  const TargetValue *TV = NULL;
  const ValueDistribution *ResultDist 
    = InstanceBase->IG_test( Inst, level, TV );
  if ( level == 0 ){
    // when level 0, ResultDist == TopDistribution
    TV = ( ProbabilisticDraw()?
	   ResultDist->BestTarget():
	   ResultDist->BestTarget( Tie, (RandomSeed() >= 0) ) );
  }
  Distance = sum_remaining_weights(level);
  if ( ResultDist )
    BestDistrib.SpecialVDCopy( *ResultDist );
  NormalizeResult();
  return TV;
}


const TargetValue *IG_Experiment::Classify( const string& Line, 
					    double &Distance,
					    bool &Tie ){
  Distance = -1.0;
  const TargetValue *ResultTarg = NULL;
  const Instance *TestInst;
  BestDistrib.Clear();
  if ( classify_options_ok( Line ) &&
       Chop( Line ) ){
    TestInst = chopped_to_instance( TestWords );
    ResultTarg = IG_Classify( TestInst, Distance, Tie );
  }
  return ResultTarg;
}

const TargetValue *TRIBL_Experiment::LocalClassify( const Instance *Inst,
						    int &status,
						    double &Distance,
						    bool &Tie ){
  const TargetValue *Res = NULL;
  const ValueDistribution *ResultDist = ExactMatch( Inst, status );
  if ( ResultDist ){
    Distance = 0.0;
    Res = ( ProbabilisticDraw()?
	    ResultDist->BestTarget():
	    ResultDist->BestTarget( Tie, (RandomSeed() >= 0)) );
    BestDistrib.SpecialVDCopy( *ResultDist );
  }
  else {
    IB_InstanceBase *SubTree = NULL;
    status = 0;
    int level = 0;
    SubTree = InstanceBase->TRIBL_test( Inst, TRIBL_offset(), 
					Res, ResultDist,
					level );
    if ( !SubTree ){
      Distance = sum_remaining_weights(level);
      if ( ResultDist )
	BestDistrib.SpecialVDCopy( *ResultDist );
    }
    else {
      ResultDist = TestInstance( Inst, Distance, SubTree, TRIBL_offset() );
      Res = ( ProbabilisticDraw()?
	      ResultDist->BestTarget():
	      ResultDist->BestTarget( Tie, (RandomSeed() >= 0)) );
      if ( Tie ){
	++num_of_neighbors;
	double Distance2;
	Init_MBL_test();
	const ValueDistribution *ResultDist2 = TestInstance( Inst,
							     Distance2,
							     SubTree,
							     TRIBL_offset() );
	bool Tie2 = false;
	const TargetValue *Res2 = ( ProbabilisticDraw()?
				    ResultDist2->BestTarget():
				    ResultDist2->BestTarget( Tie2, (RandomSeed() >= 0)) );
	--num_of_neighbors;
	if ( !Tie2 ){
	  ResultDist = ResultDist2;
	  Distance = Distance2;
	  Res = Res2;
	}
	else {
	  ResultDist = GetDistrib( num_of_neighbors -1 );
	}
      }
      SubTree->CleanPartition();
      BestDistrib.VDCopy( *ResultDist );
    }
  }
  NormalizeResult();
  return Res;
}

bool TRIBL_Experiment::classify_options_ok( const string& line ){
  int i;
  InputFormatType IF = InputFormat();
  bool result = false;
  if ( !ExpInvalid() &&
       ConfirmOptions() ){
    if ( ( i = ExamineLine( line, IF, true )) != NumOfFeatures() ){
      if ( i > 0 )
	Warning( "mismatch between number of features in testline " +
		 line + " and the Instancebase (" + to_string(i) + 
		 " vs. " + to_string(NumOfFeatures()) + ")" ); 
    }
    else if ( Initialized ) {
      result = true;
    }
    else if ( IBStatus() == Invalid )
      Warning( "you tried to apply the TRIBL algorithm, but\n"
	       "\t\tno Instance Base is available yet" );
    else if ( IBStatus() == Pruned )
      Warning( "you tried to apply the TRIBL algorithm on a pruned "
	       " Instance Base" );
    else if ( !set_input_format( IF, true ) ){
      Error( "Couldn't set input format to " + to_string( IF ) );
    }
    else {
      if ( Verbosity(NEAR_N) ){
	Do_Exact( false );
      }
      Initialized = true;
      result = true;
    }
  }
  return result;
}

const TargetValue *TRIBL2_Experiment::LocalClassify( const Instance *Inst,
						     int &status,
						     double &Distance,
						     bool &Tie ){
  const TargetValue *Res = NULL;
  const ValueDistribution *ResultDist = ExactMatch( Inst, status );
  if ( ResultDist ){
    Distance = 0.0;
    Res = ( ProbabilisticDraw()?
	    ResultDist->BestTarget():
	    ResultDist->BestTarget( Tie, (RandomSeed() >= 0)) );
    BestDistrib.SpecialVDCopy( *ResultDist );
  }
  else {
    IB_InstanceBase *SubTree = NULL;
    status = 0;
    int level = 0;
    SubTree = InstanceBase->TRIBL2_test( Inst, ResultDist, level );
    if ( SubTree ){
      ResultDist = TestInstance( Inst, Distance, SubTree, level );
      Res = ( ProbabilisticDraw()?
	      ResultDist->BestTarget():
	      ResultDist->BestTarget( Tie, (RandomSeed() >= 0)) );
      if ( Tie ){
	++num_of_neighbors;
	double Distance2;
	Init_MBL_test();
	const ValueDistribution *ResultDist2 = TestInstance( Inst, Distance2,
							     SubTree, level );
	bool Tie2 = false;
	const TargetValue *Res2 = ( ProbabilisticDraw()?
				    ResultDist2->BestTarget():
				    ResultDist2->BestTarget( Tie2, (RandomSeed() >= 0)) );
	--num_of_neighbors;
	if ( !Tie2 ){
	  ResultDist = ResultDist2;
	  Distance = Distance2;
	  Res = Res2;
	}
	else {
	  ResultDist = GetDistrib( num_of_neighbors -1 );
	}
      }
      SubTree->CleanPartition();
      BestDistrib.VDCopy( *ResultDist );
    }
    else {
      // an exact match
      Res = ( ProbabilisticDraw()?
	      ResultDist->BestTarget():
	      ResultDist->BestTarget( Tie, (RandomSeed() >= 0)) );
      BestDistrib.SpecialVDCopy( *ResultDist );
    }
  }
  NormalizeResult();
  return Res;
}

bool TRIBL2_Experiment::classify_options_ok( const string& line ){
  int i;
  InputFormatType IF = InputFormat();
  bool result = false;
  if ( !ExpInvalid() &&
       ConfirmOptions() ){
    if ( ( i = ExamineLine( line, IF, true )) != NumOfFeatures() ){
      if ( i > 0 )
	Warning( "mismatch between number of features in testline " +
		 line  + " and the Instancebase (" + to_string(i) +
		 " vs. " + to_string(NumOfFeatures()) + ")" ); 
    }
    if ( Initialized ) {
      result = true;
    }
    else if ( IBStatus() == Invalid )
      Warning( "you tried to apply the TRIBL2 algorithm, but\n"
	       "\t\tno Instance Base is available yet" );
    else if ( IBStatus() == Pruned )
      Warning( "you tried to apply the TRIBL2 algorithm on a pruned "
	       " Instance Base" );
    else if ( !set_input_format( IF, true ) ){
      Error( "Couldn't set input format to " + to_string( IF ) );
    }
    else {
      if ( Verbosity(NEAR_N) ){
	Do_Exact( false );
      }
      Initialized = true;
      result = true;
    }
  }
  return result;
}

bool TimblExperiment::LocalTest( const Instance *Inst, ostream &outfile, 
				 bool &Exact, bool &Tie, ConfusionMatrix *CM ){
  int status = 0;
  double final_distance = 0.0;
  Init_MBL_test();
  const TargetValue *ResultTarget = LocalClassify( Inst, status, 
						   final_distance, Tie );
  Exact = ( status == 1 || final_distance == 0.0 );
  // Write it to the output file for later analysis.
  show_results( outfile, BestDistrib, ResultTarget, final_distance );
  if ( CM )
    CM->Increment( Inst->TV, ResultTarget );
  return ( Inst->TV && ( ResultTarget == Inst->TV ) );
}

bool IG_Experiment::LocalTest( const Instance *Inst, ostream &outfile,
			       bool &Exact, bool &Tie, ConfusionMatrix *CM ){
  Exact = false; // Always for IG testing!
  double final_distance;
  const TargetValue *ResultTarget = IG_Classify( Inst, final_distance, Tie );
  //
  // Write it to the output file for later analysis.
  //
  show_results( outfile, BestDistrib, ResultTarget, final_distance );
  if ( CM )
    CM->Increment( Inst->TV, ResultTarget );
  return ( Inst->TV && ( ResultTarget == Inst->TV  ) );

}

inline void TimblExperiment::show_metric_info( ostream& os ){
  switch ( CurrentMetric() ){
  case Overlap:
    os << "Global metric : " << to_string(CurrentMetric(), true);
    break;
  case ValueDiff:
  case JeffreyDiv:
    os << "Global metric : " << to_string(CurrentMetric(), true);
    os << ", Prestored matrix";
    break;
  default:
    os << "Global metric : " << to_string(CurrentMetric(), true);
  }
  if ( Do_Exact() )
    os << ", prefering exact matches";
  os << endl;
  os << "Deviant Feature Metrics:";
  int cnt = 0;
  int *InvPerm = new int[NumOfFeatures()];
  for ( int ii=0; ii< NumOfFeatures(); ii++ )
    InvPerm[Permutation[ii]] = ii;
  for ( int i = 0; i < NumOfFeatures(); i++ ){
    if ( !Features[i]->Ignore() &&
	 InvPerm[i]+1 > TRIBL_offset() ){
      if ( Features[i]->Numeric() ){
	if ( CurrentMetric() != Numeric ){
	  cnt++;
	  os << endl << "   Feature[" << i+1 << "] : "
	     << to_string( Numeric, true );
	}
      }
      else if ( Features[i]->Metric() != CurrentMetric() &&
		Features[i]->Metric() != DefaultMetric ){
  	++cnt;
  	os << endl << "   Feature[" << i+1 << "] : "
  	   << to_string( Features[i]->Metric(), true );
  	if ( ( Features[i]->Metric() == ValueDiff ||
	       Features[i]->Metric() == JeffreyDiv ) &&
	     Features[i]->matrix_present() )
  	  os << " (Prestored)";
      }
      else if ( ( Features[i]->Metric() == CurrentMetric() ||
		  Features[i]->Metric() == DefaultMetric ) &&
  		( ( CurrentMetric() == ValueDiff ||
		    CurrentMetric() == JeffreyDiv ) && 
		  !Features[i]->matrix_present( ) ) ){
  	++cnt;
	os << endl << "   Feature[" << i+1 
  	   << "] : " << to_string( CurrentMetric(), true )
	   << " (Not Prestored)";
      }
    }
  }
  delete [] InvPerm;
  if ( cnt )
    os << endl;
  else
    os << "(none)" << endl;
  MatrixInfo( os );
  show_ignore_info( os );
}

inline void TimblExperiment::show_weight_info( ostream& os ){
  os << "Weighting     : " << to_string(CurrentWeighting(), true);
  if ( CurrentWeighting() == UserDefined_w )
    if ( WFileName != "" )
      os << "  (" << WFileName << ")";
    else
      os << " (no weights loaded, using No Weighting)" ;
  os << endl;
  if ( Verbosity( FEAT_W ) && CurrentWeighting() != No_w )
    ShowWeights( os );
}

inline void TimblExperiment::show_decay_info( ostream& os ){
  double par_a, par_b;
  DecayType dc = CurrentDecay( par_a, par_b );
  if ( dc != Zero ){
    os << "Decay         : " << to_string(dc, true);
    if ( dc == ExpDecay )
      os << " a=" << par_a << " b= " << par_b;
    os << endl;
  }
}

inline void TimblExperiment::show_ignore_info( ostream& os ){
  bool first = true;
  int i;
  for ( i=0; i< NumOfFeatures(); i++ ){
    if ( Features[i]->Ignore() ){
      if ( first ){
	first = false;
	os << "Ignored features : { ";
      }
      else
	os << ", ";
      os << i+1;
    }
  }
  if ( !first )
    os << " } " << endl;
}

void IB1_Experiment::testing_info( ostream& os, 
				   const string& FileName, 
				   const string& OutFile ){
  if ( Verbosity(OPTIONS ) )
    ShowSettings( os );
  os << endl << "Starting to test, Testfile: " << FileName << endl
     << "Writing output in:          " << OutFile << endl
     << "Algorithm     : IB1" << endl;
  show_metric_info( os );
  show_weight_info( os );
  show_decay_info( os );
  os << endl;
}

void IB2_Experiment::testing_info( ostream& os,
				   const string& FileName, 
				   const string& OutFile ){
  if ( Verbosity(OPTIONS) )
    ShowSettings( os );
  os << endl << "Starting to test, Testfile: " << FileName << endl
     << "Writing output in:          " << OutFile << endl
     << "Algorithm     : IB2" << endl;
  show_metric_info( os );
  show_weight_info( os );
  show_decay_info( os );
  os << endl;
}

void LOO_Experiment::testing_info( ostream& os,
				   const string& FileName,
				   const string& OutFile ){
  (void)FileName;
  if ( Verbosity(OPTIONS ) )
    ShowSettings( os );
  os << endl << "Starting to test using Leave One Out";
  if ( Do_Sloppy_LOO() )
    os << " using SLOPPY metric calculations" << endl;
  else
    os << endl;
  os   << "Writing output in:          " << OutFile << endl
       << "Algorithm     : LOO" << endl;
  show_metric_info( os );
  show_weight_info( os );
  show_decay_info( os );
  os << endl;
}

void IG_Experiment::testing_info( ostream& os,
				  const string& FileName,
				  const string& OutFile ){
  if ( Verbosity(OPTIONS) )
    ShowSettings( os );
  os << endl << "Starting to test, Testfile: " << FileName << endl
     << "Writing output in:          " << OutFile << endl
     << "Algorithm     : IGTree" << endl;
  show_ignore_info( os );
  show_weight_info( os );
  os << endl;
}


void TRIBL_Experiment::testing_info( ostream& os, 
				     const string& FileName,
				     const string& OutFile ){
  if ( Verbosity(OPTIONS) )
    ShowSettings( os );
  os << endl << "Starting to test, Testfile: " << FileName << endl
     <<	"Writing output in:          " << OutFile << endl
     << "Algorithm     : TRIBL, q = " << TRIBL_offset() << endl;
  show_metric_info( os );
  show_weight_info( os );
  show_decay_info( os );
  os << endl;
}

void TRIBL2_Experiment::testing_info( ostream& os, 
				      const string& FileName,
				      const string& OutFile ){
  if ( Verbosity(OPTIONS) )
    ShowSettings( os );
  os << endl << "Starting to test, Testfile: " << FileName << endl
     <<	"Writing output in:          " << OutFile << endl
     << "Algorithm     : TRIBL2 " << endl;
  show_metric_info( os );
  show_weight_info( os );
  show_decay_info( os );
  os << endl;
}

void CV_Experiment::testing_info( ostream& os, 
				  const string& FileName, 
				  const string& OutFile ){
  if ( Verbosity(OPTIONS) )
    ShowSettings( os );
  os << endl <<  "Starting to test, Testfile: " << FileName << endl
     <<	"Writing output in:          " << OutFile << endl
     << "Algorithm     : CV" << endl;
  show_metric_info( os );
  show_weight_info( os );
  show_decay_info( os );
  os << endl;
}

bool TimblExperiment::Test( const string& FileName,
			    const string& OutFile, 
			    const string& PercFile ){
  bool result = false;
  if ( test_options_ok( FileName ) ){
    // Open the files
    //
    istream *testfile;
    ostream *outfile = NULL;;
    ifstream inp_file;
    ofstream out_file;
    if ( FileName == "-" )
      testfile = &cin;
    else {
      inp_file.open( FileName.c_str(), ios::in);
      testfile = &inp_file;
    }
    if ( OutFile == "-" )
      outfile = &cout;
    else {
      out_file.open( OutFile.c_str(), ios::out | ios::trunc );
      if ( out_file )
	outfile = &out_file;
    }
    if (!outfile) {
      Error( "can't open: " + OutFile );
    }
    else {
      string Buffer;
      int Correct = 0;
      int Exact   = 0;
      int Tested  = 0;
      bool Matched;
      int TieCorrect = 0;
      int TieFailure = 0;
      bool Tie = false;
      time_t lStartTime;
      ConfusionMatrix *Confusion = 0;
      if ( Verbosity(ADVANCED_STATS) )
	Confusion = new ConfusionMatrix( Targets->ArraySize() );
      int DataLines = 0, SkippedLines = 0;
      testing_info( *Log(mylog), FileName, OutFile );
      
      // Start time.
      //
      time(&lStartTime);
      const Instance *CurrentInst;
      if ( InputFormat() == ARFF )
	while ( getline( *testfile, Buffer ) &&
		!compare_nocase_n( "@DATA", Buffer ) )
	  ++SkippedLines;
      while ( next_line( *testfile, Buffer, InputFormat(), SkippedLines ) ){
	if ( !Chop( Buffer ) ) {
	  ++SkippedLines;
	  Warning( "testfile, skipped line #" + 
		   to_string( DataLines+SkippedLines ) +
		   "\n" + Buffer );
	}
	else {
	  ++DataLines;
	  CurrentInst = chopped_to_instance( TestWords );
	  Tie = false;
	  if ( LocalTest( CurrentInst, *outfile, Matched, Tie, Confusion ) ){
	    ++Correct;
	    if ( Tie )
	      ++TieCorrect;
	  }
	  else if ( Tie )
	    ++TieFailure;
	  if ( Matched ){ // remember that a perfect match may be incorrect!
	    ++Exact;
	    if ( Verbosity(EXACT) ) {
	      *Log(mylog) << "Exacte match:\n";
	      show_org_input( *Log(mylog) );
	      *Log(mylog) << endl;
	    }
	  }
	  // Display progress counter.
	  show_progress( ++Tested, lStartTime );
	}
      }// end while.
      if ( OutFile != "-" )
	out_file.close();
      time_stamp( "Ready:  ", Tested );
      show_summary( *Log(mylog), Tested, Correct, Exact, 
		    TieCorrect, TieFailure, lStartTime,  Confusion, PercFile );
      delete Confusion;
      result = true;
    }
  }
  return result;
}

bool IB1_Experiment::LOO_Test( const string& FileName,
			       const string& OutFile, 
			       const string& PercFile ){
  bool result = false;
  if ( test_options_ok( "" ) ){
    // Open the files
    //
    istream *testfile;
    ostream *outfile;
    ifstream inp_file;
    ofstream out_file;
    if ( FileName == "-" )
      testfile = &cin;
    else {
      inp_file.open( FileName.c_str(), ios::in);
      testfile = &inp_file;
    }
    if ( OutFile == "-" )
      outfile = &cout;
    else {
      out_file.open( OutFile.c_str(), ios::out | ios::trunc);
      outfile = &out_file;
    }
    if (!outfile) {
      Error( "can't open: " + OutFile );
    }
    else {
      string Buffer;
      int Correct = 0;
      int Exact   = 0;
      int Tested  = 0;
      bool Matched;
      int TieCorrect = 0;
      int TieFailure = 0;
      bool Tie = false;
      time_t lStartTime;
      int DataLines = 0, SkippedLines = 0;
      testing_info( *Log(mylog), FileName, OutFile );
      const Instance *CurrentInst;
      ConfusionMatrix *Confusion = 0;
      if ( Verbosity(ADVANCED_STATS) )
	Confusion = new ConfusionMatrix( Targets->ArraySize() );
      // Start time.
      //
      time(&lStartTime);
      if ( InputFormat() == ARFF )
	while ( getline( *testfile, Buffer ) &&
		!compare_nocase_n( "@DATA", Buffer ) )
	  ++SkippedLines;
      while ( next_line( *testfile, Buffer, InputFormat(), SkippedLines ) ){
	if ( !Chop( Buffer ) ) {
	  ++SkippedLines;
	  Warning( "testfile, skipped line #" + 
		   to_string( DataLines+SkippedLines ) +
		   "\n" + Buffer );
	}
	else {
	  ++DataLines;
	  CurrentInst = chopped_to_instance( TestWords );
	  Decrement( CurrentInst );
	  Tie = false;
	  if ( LocalTest( CurrentInst, *outfile, Matched, Tie, Confusion ) ){
	    ++Correct;
	    if ( Tie )
	      ++TieCorrect;
	  }
	  else if ( Tie )
	    ++TieFailure;
	  if ( Matched ){ // remember that a perfect match may be incorrect!
	    ++Exact;
	    if ( Verbosity(EXACT) ) {
	      *Log(mylog) << "Exacte match:\n";
	      show_org_input( *Log(mylog) );
	      *Log(mylog) << endl;
	    }
	  }
	  // Display progress counter.
	  show_progress(  ++Tested, lStartTime );
	  Increment( CurrentInst );
	}
      }// end while.
      if ( OutFile != "-" )
	out_file.close();
      time_stamp( "Ready:  ", Tested );
      show_summary( *Log(mylog), Tested, Correct, Exact, 
		    TieCorrect, TieFailure, lStartTime, Confusion, PercFile );
      delete Confusion;
      result = true;
    }
  }
  return result;
}

bool LOO_Experiment::Test( const string& FileName,
			   const string& OutFile, 
			   const string& PercFile ){
  return LOO_Test( FileName, OutFile, PercFile );
}

bool CV_Experiment::get_file_names( const string& FileName ){
  bool result = false;
  if ( !ExpInvalid() ){
    NumOfFiles = 0;
    *Dbg(mydebug) << "get_file_names: '"<< FileName << "'" << endl;
    ifstream file_names( FileName.c_str(), ios::in );
    string name;
    if ( file_names.good() ) {
      while ( getline( file_names, name ) )
	++NumOfFiles;
      file_names.close();
      FileNames = new string[NumOfFiles];
      ifstream file_names2( FileName.c_str(), ios::in );
      int size = -1;
      int pos = 0;
      *Dbg(mydebug) << "get_file_names: ' step 0" << endl;
      while ( getline( file_names2, name ) ){
	*Dbg(mydebug) << "examine : " << name << endl;
	int tmp = ExamineData( name, false );
	if ( tmp != 0 ){
	  FileNames[pos++] = name;
	  if ( size == -1 )
	    size = tmp;
	  else
	    if ( tmp != size ) {
	      Error( "mismatching number of features in file " +
		     name + "of CV filelist " + FileName );
	      return false;
	    }
	}
	else
	  return false;
      }
      if ( pos != NumOfFiles ){
	Error( "Unable to read all " + to_string(NumOfFiles) + 
	       " CV filenames from " + FileName );
	return false;
      }
      else
	result = true;
    }
    else
      Error( "Unable to read CV filenames from " + FileName );
  }
  return result;
}

bool CV_Experiment::Test( const string& FileName,
			  const string& OutFile, 
			  const string& PercFile ){
  if ( !ConfirmOptions() )
    return false;
  (void)OutFile;
  (void)PercFile;
  bool result = false;
  VerbosityFlags keep = get_verbosity();
  set_verbosity( SILENT );
  if ( get_file_names( FileName ) ){
    *Dbg(mydebug) << "looked in: '"<< FileName << "' " << NumOfFiles << endl;
    *Log(mylog) << "Starting Cross validation test on files:" << endl;
    int i;
    for ( i = 0; i < NumOfFiles; i++ )
      *Log(mylog) << FileNames[i] << endl;
    TimblExperiment::Prepare( FileNames[1] );
    TimblExperiment::Learn( FileNames[1] );
    int filenum;
    for ( filenum = 2; filenum < NumOfFiles; filenum++ )
      Expand( FileNames[filenum] );
    int SkipFile;
    string OutName;
    string PercName;
    for ( SkipFile = 0; SkipFile < NumOfFiles-1; SkipFile++ ) {
      OutName = FileNames[SkipFile];
      PercName = FileNames[SkipFile];
      OutName += ".cv";
      PercName += ".cv.%";
      set_verbosity( keep );
      result = TimblExperiment::Test( FileNames[SkipFile], OutName,
				      PercName );
      set_verbosity( SILENT );
      Expand( FileNames[SkipFile] );
      Remove( FileNames[SkipFile+1] );
    }
    OutName = FileNames[NumOfFiles-1];
    PercName = FileNames[NumOfFiles-1];
    OutName += ".cv";
    PercName += ".cv.%";
    set_verbosity( keep );
    result = TimblExperiment::Test( FileNames[NumOfFiles-1], OutName, 
				    PercName ); 
  }
  return result;
}

void TimblExperiment::UseOptions( GetOptClass *gec ){
  OptParams = gec;
}

bool TimblExperiment::SetOptions( int i, const char **argv ){
  CL_Options *Opts = new CL_Options( i, argv );
  bool result = SetOptions( *Opts  );
  delete Opts;
  return result;
}

bool TimblExperiment::SetOptions( const string& arg ){
  CL_Options *Opts = new CL_Options( arg );
  bool result = SetOptions( *Opts  );
  delete Opts;
  return result;
}

bool TimblExperiment::SetOptions( const CL_Options& Opts ){
  bool result;
  if ( IsClone() )
    result = OptParams->parse_options( Opts, 2 );
  else
    result = OptParams->parse_options( Opts, 0 );
  return result;
}

bool TimblExperiment::IndirectOptions( const CL_Options& Opts ){
  OptParams->set_default_options();
  return OptParams->parse_options( Opts, 1 );
}

bool TimblExperiment::ConfirmOptions(){
  return OptParams->definitive_options( this );
}

bool TimblExperiment::DefaultOptions(){
  OptParams->set_default_options();
  return true;
}

bool TimblExperiment::ShowOptions( ostream& os ){
  return ( ConfirmOptions() &&
	   MBLClass::ShowOptions( os ) );
}

bool TimblExperiment::ShowSettings( ostream& os ){
  return ( ConfirmOptions() &&
	   MBLClass::ShowSettings( os ) );
}

bool TimblExperiment::WriteArrays( const std::string& FileName ){
  ofstream out( FileName.c_str(), ios::out | ios::trunc );
  if ( !out ) {
    Warning( "Problem opening Probability file '" + 
	     FileName + "' (not written)" );
    return false;
  }
  else {
    if ( !Verbosity(SILENT) )
      Info( "Saving Probability Arrays in " + FileName );
    return MBLClass::WriteArrays( out );
  }
}
  
bool TimblExperiment::GetArrays( const std::string& FileName ){
  ifstream inf( FileName.c_str(), ios::in );
  if ( !inf ){
    Error( "Problem opening Probability file " + FileName );
    return false;
  }
  else {
    if ( !Verbosity(SILENT) )
      Info( "Reading Probability Arrays from " + FileName );
    if ( !ReadArrays( inf ) ){
      Error( "Errors found in file " + FileName );
      return false;
    }
    else
      return true;
  }
}

bool TimblExperiment::SaveWeights( const std::string& FileName ){
  if ( ConfirmOptions() ){
    // Open the output file.
    //
    ofstream outfile( FileName.c_str(), ios::out | ios::trunc);
    if (!outfile) {
      Warning( "can't open Weightsfile: " + FileName );
      return false;
    }
    else {
      if ( !Verbosity(SILENT) )
	Info( "Saving Weights in " + FileName );
      if ( WriteWeights( outfile ) )
	return true;
      else {
	Error( "errors in Weightsfile " + FileName );
	return false;
      }
    }
  }
  else
    return false;
}

bool TimblExperiment::GetWeights( const std::string& FileName, WeightType w ){
  if ( ConfirmOptions() ){
    WFileName = FileName;
    // Open the file.
    //
    ifstream weightsfile( FileName.c_str(), ios::in);
    if ( !weightsfile) {
      Error( "can't open WeightsFile " + FileName );
      return false;
    }
    else {
      if ( w == Unknown_w ){
	w = GR_w;
// 	Warning( "Unspecified weighting, using default: " 
// 		 + to_string(w) );
      }
      if ( !Verbosity(SILENT) )
	// Info( "Reading " + to_string(w) + " weights from " + FileName );
	Info( "Reading weights from " + FileName );
      if ( ReadWeights( weightsfile, w ) ){
	return true;
      }
      else {
	Error( "Errors in Weightsfile " + FileName );
	return false;
      }
    }
  }
  else
    return false;
}

bool TimblExperiment::WriteInstanceBase( const std::string& FileName ){
  bool result = false;
  if ( ConfirmOptions() ){
    ofstream outfile( FileName.c_str(), ios::out | ios::trunc );
    if (!outfile) {
      Warning( "can't open outputfile: " + FileName );
    }
    else {
      if ( !Verbosity(SILENT) )
	Info( "Writing Instance-Base in: " + FileName );
      if ( PutInstanceBase( outfile ) )
	result = true;
    }
  }
  return result;
}

bool IG_Experiment::WriteInstanceBase( const string& FileName ){
  bool result = false;
  if ( ConfirmOptions() ){
    ofstream outfile( FileName.c_str(), ios::out | ios::trunc );
    if (!outfile) {
      Warning( "can't open outputfile: " + FileName );
    }
    else {
      if ( !Verbosity(SILENT) )
	Info( "Writing Instance-Base in: " + FileName );
      if ( PutInstanceBase( outfile ) ){
	string tmp = FileName;
	tmp += ".wgt";
	ofstream wf( tmp.c_str() );
	if ( !wf ){
	  Error( "can't write default weightfile " + tmp );
	  result = false;
	}
	else if ( !WriteWeights( wf ) )
	  result = false;
	else if ( !Verbosity(SILENT) )
	  Info( "Saving Weights in " + tmp );
	result = true;
      }
    }
  }
  return result;
}

bool IB1_Experiment::GetInstanceBase( istream& is ){
  bool result = false;
  bool Pruned;
  bool Hashed;
  int Version;
  string range_buf;
  if ( !get_IB_Info( is, Pruned, Version, Hashed, range_buf ) ){
    Error( "Can'n retrieve Instance-Base\n" );
  }
  else if ( Pruned ){
    Error( "Instance-base is Pruned!, NOT valid for " + 
	   to_string(algorithm) + " Algorithm" );
  }
  else {
    TreeOrder = DataFile;
    Initialize();
    if ( !get_ranges( range_buf ) ){
      Warning( "couldn't retrieve ranges..." );
    }
    else {
      srand( RandomSeed() );
      InstanceBase = new IB_InstanceBase( EffectiveFeatures(), 
					  (RandomSeed()>=0) );
      int pos=0;
      for ( int i=0; i < NumOfFeatures(); i++ ){
	Features[i]->Weight( 1.0 );
	if ( Features[Permutation[i]]->Ignore() )
	  PermFeatures[i] = NULL;
	else 
	  PermFeatures[pos++] = Features[Permutation[i]];
      }
      if ( Hashed )
	result = InstanceBase->ReadIB( is, PermFeatures,
				       Targets, 
				       TargetStrings, FeatureStrings,
				       Version ); 
      else
	result = InstanceBase->ReadIB( is, PermFeatures, 
				       Targets, 
				       Version ); 
    }
  }
  return result;
}

bool IG_Experiment::GetInstanceBase( istream& is ){
  bool result = false;
  bool Pruned;
  bool Hashed;
  int Version;
  string range_buf;
  if ( !get_IB_Info( is, Pruned, Version, Hashed, range_buf ) ){
    Error( "Can'n retrieve Instance-Base\n" );
  }
  else if ( !Pruned ){
    Error( "Instance-base is NOT Pruned!, invalid for " +
	   to_string(algorithm) + " Algorithm" );
  }
  else {
    TreeOrder = DataFile;
    Initialize();
    if ( !get_ranges( range_buf ) ){
      Warning( "couldn't retrieve ranges..." );
    }
    else {
      srand( RandomSeed() );
      InstanceBase = new IG_InstanceBase( EffectiveFeatures(), 
					  (RandomSeed()>=0), 
					  Pruned,
					  KeepDistributions() );
      int pos=0;
      for ( int i=0; i < NumOfFeatures(); i++ ){
	Features[i]->Weight( 1.0 );
	if ( Features[Permutation[i]]->Ignore() )
	  PermFeatures[i] = NULL;
	else 
	  PermFeatures[pos++] = Features[Permutation[i]];
      }
      if ( Hashed )
	result = InstanceBase->ReadIB( is, PermFeatures,
				       Targets, 
				       TargetStrings, FeatureStrings,
				       Version ); 
      else
	result = InstanceBase->ReadIB( is, PermFeatures, 
				       Targets, 
				       Version ); 
      if ( result &&
	   !InstanceBase->HasDistributions() ){
	if ( Verbosity(DISTRIB) ){
	  Info( "Instance base doesn't contain Distributions, "
		"+vDB option disabled ...."  );
	  ResetVerbosityFlag(DISTRIB);
	}
	KeepDistributions( false );
      }
    }
  }
  return result;
}

bool TRIBL_Experiment::GetInstanceBase( istream& is ){
  bool result = false;
  bool Pruned;
  bool Hashed;
  int Version;
  string range_buf;
  if ( !get_IB_Info( is, Pruned, Version, Hashed, range_buf ) ){
    Error( "Can'n retrieve Instance-Base\n" );
  }
  else if ( Pruned ){
    Error( "Instance-base is Pruned!, NOT valid for " +
	   to_string(algorithm) + " Algorithm" );
  }
  else {
    TreeOrder = DataFile;
    Initialize();
    if ( !get_ranges( range_buf ) ){
      Warning( "couldn't retrieve ranges..." );
    }
    else {
      srand( RandomSeed() );
      InstanceBase = new TRIBL_InstanceBase( EffectiveFeatures(), 
					     (RandomSeed()>=0),
					     KeepDistributions() ); 
      int pos=0;
      for ( int i=0; i < NumOfFeatures(); i++ ){
	Features[i]->Weight( 1.0 );
	if ( Features[Permutation[i]]->Ignore() )
	  PermFeatures[i] = NULL;
	else 
	  PermFeatures[pos++] = Features[Permutation[i]];
      }
      if ( Hashed )
	result = InstanceBase->ReadIB( is, PermFeatures,
				       Targets, 
				       TargetStrings, FeatureStrings,
				       Version ); 
      else
	result = InstanceBase->ReadIB( is, PermFeatures, 
				       Targets, 
				       Version ); 
    }
  }
  return result;
}

bool TRIBL2_Experiment::GetInstanceBase( istream& is ){
  bool result = false;
  bool Pruned;
  bool Hashed;
  int Version;
  string range_buf;
  if ( !get_IB_Info( is, Pruned, Version, Hashed, range_buf ) ){
    Error( "Can'n retrieve Instance-Base\n" );
  }
  else if ( Pruned ){
    Error( "Instance-base is Pruned!, NOT valid for " +
	   to_string(algorithm) + " Algorithm" );
  }
  else {
    TreeOrder = DataFile;
    Initialize();
    if ( !get_ranges( range_buf ) ){
      Warning( "couldn't retrieve ranges..." );
    }
    else {
      srand( RandomSeed() );
      InstanceBase = new TRIBL2_InstanceBase( EffectiveFeatures(), 
					      (RandomSeed()>=0),
					      KeepDistributions() ); 
      int pos=0;
      for ( int i=0; i < NumOfFeatures(); i++ ){
	Features[i]->Weight( 1.0 );
	if ( Features[Permutation[i]]->Ignore() )
	  PermFeatures[i] = NULL;
	else 
	  PermFeatures[pos++] = Features[Permutation[i]];
      }
      if ( Hashed )
	result = InstanceBase->ReadIB( is, PermFeatures,
				       Targets, 
				       TargetStrings, FeatureStrings,
				       Version ); 
      else
	result = InstanceBase->ReadIB( is, PermFeatures, 
				       Targets, 
				       Version ); 
    }
  }
  return result;
}

bool LOO_Experiment::ReadInstanceBase( const string& ){
  Error( "cannot combine Leave One Out with retrieving an Instancebase " );
  return false;
}

bool TimblExperiment::ReadInstanceBase( const string& FileName ){
  bool result = false;
  if ( ConfirmOptions() ){
    ifstream infile( FileName.c_str(), ios::in );
    if ( !infile ) {
      Error( "can't open: " + FileName );
    }
    else {
      if ( !Verbosity(SILENT) )
	Info( "Reading Instance-Base from: " + FileName );
      if ( GetInstanceBase( infile ) ){
	if ( !Verbosity(SILENT) ){
	  write_perm( my_log() );
	}
	result = true;
      }
    }
  }
  return result;
}
  
bool IG_Experiment::ReadInstanceBase( const string& FileName ){
  bool result = false;
  if ( ConfirmOptions() ){
    ifstream infile( FileName.c_str(), ios::in );
    if ( !infile ) {
      Error( "can't open: " + FileName );
    }
    else {
      if ( !Verbosity(SILENT) )
	Info( "Reading Instance-Base from: " + FileName );
      if ( GetInstanceBase( infile ) ){
	if ( !Verbosity(SILENT) ){
	  write_perm( my_log() );
	}
	string tmp = FileName;
	tmp += ".wgt";
	ifstream wf( tmp.c_str() );
	if ( !wf ){
	  Error( "cant't find default weightsfile " + tmp );
	}
	else if ( ReadWeights( wf, CurrentWeighting() ) ){
	  WFileName = tmp;
	  if ( !Verbosity(SILENT) ){
	    Info( "Reading weights from " + tmp );
	  }
	}
	result = true;
      }
    }
  }
  return result;
}
  
bool TimblExperiment::WriteNamesFile( const string& FileName ){
  // Open the file.
  //
  ofstream namesfile( FileName.c_str(), ios::out | ios::trunc);
  if (!namesfile) {
    Warning( "can't open NamesFile: '" +
	     FileName + "' (not written)" );
    return false;
  }
  else {
    if ( !Verbosity(SILENT) )
      Info( "Saving names in " + FileName );
    MBLClass::WriteNamesFile( namesfile );
    return true;
  }
}

void IB1_Experiment::InitInstanceBase(){
  srand( RandomSeed() );
  set_order();
  InstanceBase = new IB_InstanceBase( EffectiveFeatures(), 
				      (RandomSeed()>=0) );
}

void IG_Experiment::InitInstanceBase(){
  srand( RandomSeed() );
  default_order();
  set_order();
  InstanceBase = new IG_InstanceBase( EffectiveFeatures(), 
				      (RandomSeed()>=0), 
				      false, KeepDistributions() );
}

void TRIBL_Experiment::InitInstanceBase(){
  srand( RandomSeed() );
  default_order();
  set_order();
  InstanceBase = new TRIBL_InstanceBase( EffectiveFeatures(), 
					 (RandomSeed()>=0),
					 KeepDistributions() );
}

void TRIBL2_Experiment::InitInstanceBase(){
  srand( RandomSeed() );
  default_order();
  set_order();
  InstanceBase = new TRIBL2_InstanceBase( EffectiveFeatures(), 
					  (RandomSeed()>=0),
					  KeepDistributions() );
}

bool TimblExperiment::build_file_index( const string& file_name, 
					MultiIndex& multi_index ){
  bool result = true;
  string Buffer;
  int DataLines = 0, SkippedLines = 0;  
  streamsize cur_pos = 0;
  // Open the file.
  //
  ifstream datafile( file_name.c_str(), ios::in);
  if ( InputFormat() == ARFF )
    while ( getline( datafile, Buffer ) &&
	    !compare_nocase_n( "@DATA", Buffer ) )
      ++SkippedLines;
  cur_pos = datafile.tellg();
  if ( !next_line( datafile, Buffer, InputFormat(), SkippedLines ) ){
    Error( "cannot start learning from in: " + file_name );
    result = false;    // No more input
  }
  else if ( !Chop( Buffer ) ){
    ++SkippedLines;
    Error( "no useful data in: " + file_name );
    result = false;    // No more input
  }
  else {
    ++DataLines;
    if ( !Verbosity(SILENT) ) {
      Info( "Phase 2: Building index on Datafile: " + file_name );
      time_stamp( "Start:     ", 0 );
    }
    bool found;
    bool go_on = true;
    const Instance *NewInst;
    while( go_on ){ 
      // The next Instance to store. 
      NewInst = chopped_to_instance( TrainWords );
      FeatureValue *fv = NewInst->FV[0];
      multi_index.insert( make_pair( fv, cur_pos ) );
      if ((DataLines % Progress() ) == 0) 
	time_stamp( "indexing:  ", DataLines );
      found = false;
      while ( !found && 
	      ( cur_pos = datafile.tellg(),
		next_line( datafile, Buffer, InputFormat(), SkippedLines ) ) ){
	found = Chop( Buffer );
	if ( found )
	  ++DataLines;
	else {
	  ++SkippedLines;
	  Warning( "datafile, skipped line #" + 
		   to_string( DataLines+SkippedLines ) +
		   "\n" + Buffer );
	}
      }
      go_on = found;
    }
    time_stamp( "Finished:  ", DataLines );
  }
  return result;
}

bool IG_Experiment::Learn( const string& FileName ){
  bool result = true;
  if ( ExpInvalid() ||
       !ConfirmOptions() ){
    result = false;
  }
  else if ( CurrentDataFile == "" ){
    if ( FileName == "" ){
      Warning( "unable to build an InstanceBase: No datafile defined yet" );
      result = false;
    }
    else {
      if ( !Prepare( FileName ) || ExpInvalid() ){
	result = false;
      }
    }
  }
  else if ( FileName != "" &&
	    CurrentDataFile != FileName ){
    Error( "Unable to Learn from file '" + FileName + "'\n"
	   "while previously instantiated from file '" + 
	   CurrentDataFile + "'" );
    result = false;
  }
  if ( result ) {
    InitInstanceBase();
    MultiIndex multi_index;
    result = build_file_index( CurrentDataFile, multi_index );
    if ( result ){
      if ( !Verbosity(SILENT) ) {
	Info( "\nPhase 3: Learning from Datafile: " + CurrentDataFile );
	time_stamp( "Start:     ", 0 );
      }
      string Buffer;
      int dummy1;
      int count;
      const Instance *NewInst;
      IG_InstanceBase *PartInstanceBase = 0;
      TargetValue *TopTarget = (TargetValue*)Targets->MajorityClass();
      // Open the file.
      //
      ifstream datafile( CurrentDataFile.c_str(), ios::in);
      //
      Feature *Feat =  Features[Permutation[0]];
      int lines =0;
      for ( count=0; count< Feat->ArraySize(); ++count ){
	FeatureValue *the_fv = Feat->Value(count);
	//	Info( "start handling feature '" + the_fv->Name() + "'" );
	PartInstanceBase = new IG_InstanceBase( EffectiveFeatures(), 
						(RandomSeed()>=0), 
						false, KeepDistributions() );
	typedef MultiIndex::const_iterator mit;
	pair<mit,mit> b = multi_index.equal_range( the_fv );
	for ( mit It=b.first; It!= b.second; ++It ){
	  datafile.seekg( (*It).second );
	  next_line( datafile, Buffer, InputFormat(), dummy1 );
	  // Progress update.
	  //
	  if (( ++lines % Progress() ) == 0) 
	    time_stamp( "Learning:  ", lines );
	  Chop( Buffer );
	  NewInst = chopped_to_instance( TrainWords );
	  PartInstanceBase->AddInstance( NewInst );
	}
	if ( PartInstanceBase ){
// 	  Info( "finished handling feature " + the_fv->Name() );
// 	  time_stamp( "Start Pruning:    " );
	  PartInstanceBase->Prune( TopTarget );
// 	  time_stamp( "Finished Pruning: " );
	  if ( !InstanceBase->Merge( PartInstanceBase, the_fv ) ){
	    FatalError( string("Merging InstanceBases failed, are you sure")
			+ " that the inputfile is sorted on the most"
			+ " significant feature?" );
	    return false;
	  }
	  delete PartInstanceBase;
	  PartInstanceBase = 0;
	}
      }
      time_stamp( "Finished:  ", lines );
      if ( !Verbosity(SILENT) )
	IBInfo( *Log(mylog) );
    }
  }
  return result;
}

