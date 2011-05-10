/*
 * Timbl.cc
 *
 *    The Timbl main program
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


#include <exception>
#include <iosfwd>
#include <string>
#include <fstream>
#ifdef IRIX64
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#else
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cctype>
#endif
#include "TimblAPI.h"

using namespace std;

static string *ind_lines = NULL;
static int ind_len = 0;

Algorithm algorithm;

bool Do_CV = false;
bool Do_LOO = false;
bool Do_Indirect = false;
bool Do_Server = false;
int ServerPort = -1;
int Max_Connections = 10;
bool Do_Save_Perc = false;

string I_Path = "";
string O_Path = "";
string Q_value = "";
string DataFile = "";
string TestFile = "";
string OutputFile = "";
string PercFile = "";
string TreeInFile = "";
string TreeOutFile = "";
string WgtInFile = "";
Weighting WgtType = UNKNOWN_W;
string WgtOutFile = "";
string ProbInFile = "";
string ProbOutFile = "";
string NamesFile = "";

inline void usage_full(void){
  cerr << "usage: Timbl -f data-file {-t test-file} [options]" << endl;
  cerr << "Algorithm and Metric options:" << endl;
  cerr << "-a n      : algorithm." << endl;
  cerr << "     0 or IB1   : IB1     (default)" << endl;
  cerr << "     1 or IG    : IGTree" << endl;
  cerr << "     2 or TRIBL : TRIBL" << endl;
  cerr << "     3 or IB2   : IB2" << endl;
  cerr << "     4 or TRIBL2 : TRIBL2" << endl;
  cerr << "-m s      : use feature metrics as specified in string s:" 
       << endl
       << "            format: GlobalMetric:MetricRange:MetricRange" 
	 << endl
       << "            e.g.: mO:N3:I2,5-7" << endl;
  cerr << "     D: Dot product. (Global only. numeric features implied)" 
       << endl;
  cerr << "     O: weighted Overlap. (default)" << endl;
  cerr << "     M: Modified value difference." << endl;
  cerr << "     J: Jeffrey Divergence" << endl;
  cerr << "     N: numeric values." << endl;
  cerr << "     I: Ignore named  values." << endl;
  cerr << "-w n      : Weighting." << endl;
  cerr << "     0: No Weighting." << endl;
  cerr << "     1: Weight using GainRatio. (default)" << endl;
  cerr << "     2: Weight using InfoGain" << endl;
  cerr << "     3: Weight using Chi-square" << endl;
  cerr << "     4: Weight using Shared Variance" << endl;
  cerr << "-w f      : read Weights from file 'f'." << endl;
  cerr << "-w f:n    : read Weight n from file 'f'." << endl;
  cerr << "-b n      : number of lines used for bootstrapping (IB2 only)." 
       << endl;
  cerr << "-d val    : weight neighbors as function of their distance:" 
       << endl;
  cerr << "     Z      : all the same weight. (default)" << endl;
  cerr << "     ID     : Inverse Distance." << endl;
  cerr << "     IL     : Inverse Linear." << endl;
  cerr << "     ED:a   : Exponential Decay with factor a. (no whitespace!)"
       << endl;
  cerr << "     ED:a:b : Exponential Decay with factor a and b. (no whitespace!)"
       << endl;
  cerr << "-k n      : k nearest neighbors (default n = 1)." << endl;
  cerr << "-q n      : TRIBL treshold at level n." << endl;
  cerr << "-L n      : MVDM treshold at level n." << endl;
  cerr << "-R n      : solve ties at random with seed n." << endl;
  //  cerr << "-R P n   : solve ties probabilistic with seed n." << endl;
  cerr << "-t  f     : test using file 'f'." << endl;
  cerr << "-t leave_one_out:"
       <<" test with Leave One Out,using IB1." << endl;
  cerr << "-t cross_validate:"
       <<" Cross Validate Test,using IB1." << endl;
  cerr << "   @f     : test using files and options described in file 'f'." 
       << endl;
  cerr << "            Supported options: d e F k m o p q R t u v w x % -" 
       << endl;
  cerr << "            -t <file> is mandatory" << endl;
  cerr << "Input options:" << endl;
  cerr << "-f f      : read from Datafile 'f'." << endl;
  cerr << "-f f      : OR: use filenames from 'f' for CV test" << endl;
  cerr << "-F format : Assume the specified inputformat." << endl;
  cerr << "            (Compact, C4.5, ARFF, Columns, Binary, Sparse )" 
       << endl;
  cerr << "-l n      : length of Features (Compact format only)." << endl;
  cerr << "-i f      : read the InstanceBase from file 'f'. "
       << "(skips phase 1 & 2 )"
       << endl;
  cerr << "-u f      : read value_class probabilities from file 'f'." 
       << endl;
  cerr << "-P d      : read data using path 'd'." << endl;
  cerr << "-s        : use exemplar weights from the input file" << endl;
  cerr << "-s0       : silently ignore the exemplar weights from the input file" << endl;
  cerr << "Output options:" << endl;
  cerr << "-e n      : estimate time until n patterns tested." << endl;
  cerr << "-I f      : dump the InstanceBase in file 'f'." << endl;
  cerr << "-n f      : create names file 'f'." << endl;
  cerr << "-p n      : show progress every n lines. (default p = 100,000)" 
       << endl;
  cerr << "-U f      : save value_class probabilities in file 'f'." << endl;
  cerr << "-V        : Show VERSION." << endl;
  cerr << "+v or -v level : set or unset verbosity level, where level is"
       << endl;
  cerr << "      s:  work silently." << endl;
  cerr << "      o:  show all options set." << endl;
  cerr << "      f:  show Calculated Feature Weights. (default)" 
       << endl;
  cerr << "      p:  show Value Difference matrices." << endl;
  cerr << "      e:  show exact matches." << endl;
  cerr << "      as: show advanced statistics. (memory consuming)" << endl;
  cerr << "      cm: show Confusion Matrix. (implies +vas)" << endl;
  cerr << "      cs: show per Class Statistics. (implies +vas)" << endl;
  cerr << "      di: add distance to output file." << endl;
  cerr << "      db: add distribution of best matched to output file" 
       << endl;
  cerr << "      k:  add a summary for all k neigbors to output file"
       << " (sets -x)" << endl;
  cerr << "      n:  add nearest neigbors to output file. (sets -x)"
       << endl;
  cerr << "  You may combine levels using '+' e.g. +v p+db or -v o+di"
       << endl;
  cerr << "-W f      : calculate and save all Weights in file 'f'." << endl;
  cerr << "+% or -%  : do or don't save test result (%) to file." << endl;
  cerr << "-o s      : use s as output filename." << endl;
  cerr << "-O d      : save output using path 'd'." << endl;
  cerr << "Internal representation options:" << endl;
  cerr << "-B n      : number of bins used for discretization of numeric " 
       << "feature values" << endl;
  cerr << "-c n      : clipping frequency for prestoring MVDM matrices"
       << endl;
  cerr << "+D        : store distributions on all nodes." << endl
       << "            (may waste memory, but enables +vDB option for IG-tree)"
       << endl;
  cerr << "+H or -H  : write hashed trees (default +H)" << endl;
  cerr << "-M n      : size of MaxBests Array" << endl;
  cerr << "-N n      : Number of features (default " 
       << TimblAPI::Default_Max_Feats() << ")" << endl;
  cerr << "-T n      : ordering of the Tree :" << endl;
  cerr << "       DO: none." << endl;
  cerr << "       GRO: using GainRatio" << endl;
  cerr << "       IGO: using InformationGain" << endl;
  cerr << "       1/V: using 1/# of Vals" << endl;
  cerr << "       G/V: using GainRatio/# of Vals" << endl;
  cerr << "       I/V: using InfoGain/# of Vals" << endl;
  cerr << "       X2O: using X-square" << endl;
  cerr << "       X/V: using X-square/# of Vals" << endl;
  cerr << "       SVO: using Shared Variance" << endl;
  cerr << "       S/V: using Shared Variance/# of Vals" << endl;
  cerr << "       GxE: using GainRatio * SplitInfo" << endl;
  cerr << "       IxE: using InformationGain * SplitInfo" << endl;
  cerr << "       1/S: using 1/SplitInfo" << endl;
  cerr << "+x or -x  : Do or don't use the exact match shortcut. " << endl
       << "            (IB only, default is -x)"
       << endl;
}

inline void usage(void){
  cerr << "usage:  Timbl -f data-file {-t test-file}"
       << endl;
  cerr << "or see: Timbl -h" << endl;
  cerr << "        for all possible options" << endl;
  cerr << endl;
}

string *get_command_lines( const string& value ){
  ind_len = 0;
  ifstream ind( value.c_str()+1 ); // skip @ 
  if ( ind.bad() ){
    cerr << "Problem reading command-lines from file '" 
	 << value << "'" << endl;
    exit(3);
  }
  string Buf;
  int lines = 0;
  while ( getline( ind, Buf ) )
    lines++;
  ind.close();
  ind.clear();
  string *result = new string[lines];
  ind.open( value.c_str()+1 );
  ind_len = 0;
  while ( ind && getline( ind, Buf ) ){
    if ( Buf.length() == 0 )
      continue;
    result[ind_len++] = Buf;
  }
  return result;
}

string correct_path( const string& filename,
		     const string& path,
		     bool keep_origpath ){  
  // if filename contains pathinformation, it is replaced with path, except
  // when keep_origpath is true. 
  // if filename contains NO pathinformation, path is always appended.
  // of course we don't append if the filename is empty or just '-' !
  
  if ( path != "" && filename != "" && filename[0] != '-' ){
    bool add_slash = path[path.length()] != '/';
    string tmp;
    string::size_type pos = filename.rfind( '/' );
    if ( pos == string::npos ){
      tmp = path;
      if ( add_slash )
	tmp += "/";
      tmp += filename;
    }
    else { 
      tmp = path;
      if ( add_slash )
	tmp += "/";
      if ( !keep_origpath ){
	tmp += filename.substr( pos+1 );
      }
      else 
	tmp += filename;
    }
    return tmp;
  }
  else
    return filename;
}

void Preset_Values( TimblOpts& Opts ){
  bool mood;
  string value;
  if ( Opts.Find( 'h', value, mood ) ){
    usage_full();
    exit(1);
  }
  if ( Opts.Find( 'V', value, mood ) ){
    cerr << "TiMBL " << TimblAPI::VersionInfo( true ) << endl;
    exit(1);
  }
  if ( Opts.Find( 'a', value, mood ) ){
    // the user gave an algorithm
    if ( !string_to( value, algorithm ) ){
      cerr << "illegal -a value: " << value << endl;
      exit(1); // no chance to proceed
    }
    Opts.Delete( 'a' );
  }
  else
    algorithm = IB1; // general default
  Opts.Add( 'a', to_string( algorithm ), false );
  if ( Opts.Find( 't', value, mood ) ){
    if ( value == "cross_validate" )
      // Special case
      //    running Cross Validation
      Do_CV = true;
    else if ( value == "leave_one_out" )
      // Special case
      //    running Leave_one_out
      Do_LOO = true;
    else if ( value != "" && value[0] == '@' ){
      Do_Indirect = true;
      ind_lines = get_command_lines( value );
      Opts.Delete( 't' );
    }
    if ( Do_LOO || Do_CV )
      if ( algorithm != IB1 ){
	cerr << "Invalid Algorithm: Only IB1 possible for LOO or CV" << endl;
	exit(1);
      }
  }
  if ( Opts.Find( 'P', value, mood ) ){
    I_Path = value;
    Opts.Delete( 'P' );
  }
  if ( Opts.Find( 'O', value, mood ) ){
    O_Path = value;
    Opts.Delete( 'O' );
  }
  if ( Opts.Find( 'f', value, mood ) ){
    DataFile = correct_path( value, I_Path, true );
    Opts.Delete( 'f' );
  }
  if ( Opts.Find( 'q', value, mood ) ){
    Q_value = value;
  }
  Opts.Add( 'v', "F", true );
  Opts.Add( 'v', "S", false );
  if ( Opts.Find( 'S', value, mood ) ){
    if ( Do_LOO || Do_CV || Do_Indirect ){
      cerr << "Cannot run as a server when -t option is also specified.\n" 
	   << "(-S option will be ignored!)" << endl;
    }
    else {
      Do_Server = true;
      ServerPort = stoi( value );
      if ( ServerPort < 1 || ServerPort > 100000 ){
	cerr << "-S option, portnumber invalid: " << ServerPort << endl;
	exit(3);
      }
    }
    Opts.Delete( 'S' );
  }
  if ( Opts.Find( 'C', value, mood ) ){
    Max_Connections = stoi( value );
    if ( Max_Connections < 1 || Max_Connections > 1000 ){
      cerr << "-C options, max number of connection invalid: " 
	   << Max_Connections << endl;
    }
    Opts.Delete( 'C' );
  }
  Weighting W = GR;
  // default Weighting = GainRatio
  if ( Opts.Find( 'w', value, mood ) ){
    // user specified weighting
    if ( !string_to( value, W ) )
      // no valid weight, hopefully a filename
      return;
    else
      // valid Weight, but maybe a number, so replace
      Opts.Delete( 'w' );
  }
  Opts.Add( 'w', to_string(W), false );
}

void Adjust_Default_Values( TimblOpts& Opts ){
  bool mood;
  string value;
  if ( !Opts.Find( 'm', value, mood ) ){
    Opts.Add( 'm', "O", false );
    // Default Metric = Overlap
  }
  else if ( value[0] == 'M' ) {
    if ( algorithm == IGTREE ){
      cerr << "Metric must be Overlap for IGTREE, (use -mO:...)" << endl;
      exit(1);
    }
  }
  else if ( value[0] == 'J' ) {
    if ( algorithm == IGTREE ){
      cerr << "Metric must be Overlap for IGTREE, (use -mO:...)" << endl;
      exit(1);
    }
  }
  if ( Opts.Find( '%', value, mood ) ){
    Do_Save_Perc = true;
    Opts.Delete( '%' );
  }
}

bool next_test( string& line ){
  static bool first = true;
  bool result = false;
  line = "";
  if ( !ind_lines ){
    if ( first ){
      first = false;
      result = true;
    }
  }
  else {
    static int pos = 0;
    if ( pos < ind_len ){
      line = ind_lines[pos++];
      result = true;
    }
  }
  return result;
}

bool get_file_names( TimblOpts& Opts ){
  TestFile = "";
  OutputFile = "";
  PercFile = "";
  TreeInFile = "";
  TreeOutFile = "";
  WgtInFile = "";
  WgtType = UNKNOWN_W;
  WgtOutFile = "";
  ProbInFile = "";
  ProbOutFile = "";
  NamesFile = "";
  string value;
  bool mood;
  if ( Opts.Find( 'P', value, mood ) ||
       Opts.Find( 'O', value, mood ) ||
       Opts.Find( 'f', value, mood ) ){
    cerr << "illegal option, value = " << value << endl;
    return false;
  }
  if ( Do_LOO ){
    if ( DataFile == "" ){
      cerr << "Missing datafile name for Leave One Out test" << endl;
      return false;
    }
    TestFile = DataFile;
  }
  else if ( Do_CV ){
    if ( DataFile == "" ){
      cerr << "Missing datafile name for Cross Validation test" << endl;
      return false;
    }
    TestFile = DataFile;
  }
  else if ( Opts.Find( 't', value, mood ) ){
    TestFile = correct_path( value, I_Path, true );
    Opts.Delete( 't' );
  }
  if ( Opts.Find( 'n', value, mood ) ){
    NamesFile = correct_path( value, O_Path, true );
    Opts.Delete( 'n' );
  }
  if ( Opts.Find( 'o', value, mood ) ){
    OutputFile = correct_path( value, O_Path, true );
    Opts.Delete( 'o' );
  }
  if ( Opts.Find( 'I', value, mood ) ){
    TreeOutFile = correct_path( value, O_Path, true );
    Opts.Delete( 'I' );
  }
  if ( Opts.Find( 'i', value, mood ) ){
    TreeInFile = correct_path( value, I_Path, true );
    Opts.Delete( 'i' );
  }
  if ( Opts.Find( 'U', value, mood ) ){
    ProbOutFile = correct_path( value, O_Path, true );
    Opts.Delete( 'U' );
  }
  if ( Opts.Find( 'u', value, mood ) ){
    ProbInFile = correct_path( value, I_Path, true );
    Opts.Delete( 'u' );
  }
  if ( Opts.Find( 'W', value, mood ) ){
    WgtOutFile = correct_path( value, O_Path, true );
    // leave the option, to signal that we need ALL feature weights
  }
  if ( Opts.Find( 'w', value, mood ) ){
    Weighting W;
    if ( !string_to( value, W ) ){
      // No valid weighting, so assume it also has a filename
      vector<string> parts;
      int num = split_at( value, parts, ":" );
      if ( num == 2 ){
	if ( !string_to( parts[1], W ) ){
	  cerr << "invalid weighting option: " << value << endl;
	  return false;
	}
	WgtInFile = correct_path( parts[0], I_Path, true );
	WgtType = W;
	Opts.Delete( 'w' );
      }
      else if ( num == 1 ){
	WgtInFile = correct_path( value, I_Path, true );
	Opts.Delete( 'w' );
      } 
      else {
	cerr << "invalid weighting option: " << value << endl;
	return false;
      }
    }
  }
  return true;
}

bool Default_Output_Names( TimblOpts& Opts ){
  string value;
  bool mood;
  if ( OutputFile == "" && TestFile != "" ){
    string temp = correct_path( TestFile, O_Path, false );
    temp += ".";
    switch ( algorithm ){
    case IB1:
      if ( Do_LOO )
	temp += "LOO";
      else if ( Do_CV )
	temp += "CV";
      else
	temp += "IB1";
      break;
    case IB2:
      temp +="IB2";
      break;
    case IGTREE:
      temp += "IGTree";
      break;
    case TRIBL:
      temp += "TRIBL";
      if ( Q_value != "" ){
	temp += "-";
	temp += Q_value;
      }
      else
	temp +=  "-0";
      break;
    case TRIBL2:
      temp +=  "TRIBL2";
      break;
    case LOO:
      temp +=  "LOO";
      break;
    case CV:
      temp +=  "CV";
      break;
    default:
      temp +=  "ERROR";
    }
    if ( algorithm != IGTREE ){
      temp +=  ".";
      if ( Opts.Find( 'm', value, mood ) )
	temp +=  value;
      else
	temp +=  "ErRoR";
      if ( Opts.Find( 'L', value, mood ) ){
	temp +=  ".L";
	temp +=  value;
      }
    }
    temp +=  ".";
    if ( Opts.Find( 'w', value, mood ) ){
      temp +=  value;
    }
    else 
      if ( !WgtInFile.empty() )
	temp += "ud";
      else
	temp += "gr";
    if ( algorithm != IGTREE ){
      if ( Opts.Find( 'k', value, mood ) ){
	temp +=  ".k";
	temp +=  value;
      }
      else
	temp +=  ".k1";
      if ( Opts.Find( 'd', value, mood ) ){
	temp +=  ".";
	temp +=  value;
      }
    }
    if ( Opts.Find( 'x', value, mood ) ){
      if ( mood ){
	temp +=  ".X";
      }
    }
    OutputFile = temp + ".out";
    if ( Do_Save_Perc ){
      PercFile = temp + ".%";
    }
  }
  else if ( OutputFile != "" ){
    if ( Do_Save_Perc ){
      PercFile = OutputFile;
      string::size_type pos = PercFile.rfind( '.' );
      if ( pos != string::npos )
	PercFile = PercFile.substr( 0, pos );
      PercFile += ".%";
    }
  }
  return true;
}

void Do_Test( TimblAPI *Run ){
  if ( WgtInFile != "" ) {
    Run->GetWeights( WgtInFile, WgtType );
  }
  if ( !ind_lines ){
    // just one test...
    if ( ProbInFile != "" )
      Run->GetArrays( ProbInFile );
    Run->Test( TestFile,
	       OutputFile,
	       PercFile );
  }
  else {
    // multiple tests from indirect file
    string tmp_line;
    while ( next_test( tmp_line) ){
      TimblOpts Opts( tmp_line );
      Adjust_Default_Values( Opts );
      if ( !get_file_names( Opts ) || TestFile == "" ){
	cerr << "Warning: Skipped a line from indirect testfile:\n'"	
	     << tmp_line << "'" << endl;
	if ( TestFile == "" )
	  cerr << "missing a Testfile name " << endl;
      }
      else if ( Run->SetIndirectOptions( Opts ) ){
	Default_Output_Names( Opts );
	if ( WgtInFile != "" ) {
	  Run->GetWeights( WgtInFile, WgtType );
	}
	if ( ProbInFile != "" )
	  Run->GetArrays( ProbInFile );
	Run->Test( TestFile,
		   OutputFile,
		   PercFile );
      }
      else
	cerr << "Warning: Skipped a line from indirect testfile:\n'"
	     << tmp_line << "'" << endl;
    }
  }
}

int main(int argc, char *argv[]){
  try {
    struct tm *curtime;
    time_t Time;
    // Start.
    //
    cerr << "TiMBL " << TimblAPI::VersionInfo()
	 << " (c) ILK 1998 - 2006.\n" 
	 << "Tilburg Memory Based Learner\n"
	 << "Induction of Linguistic Knowledge Research Group\n"
	 << "Tilburg University / University of Antwerp" << endl;
    time(&Time);
    curtime = localtime(&Time);
    cerr << asctime(curtime) << endl;
    if ( argc <= 1 ){
      usage();
      exit(1);
    }
    TimblOpts Opts( argc, argv );
    Preset_Values( Opts );
    Adjust_Default_Values( Opts );
    if ( !get_file_names( Opts ) )
      exit( 3 );
    TimblAPI *Run = new TimblAPI( &Opts );
    if ( !Run->Valid() ){
      usage();
      exit(3);
    }
    if ( !Do_Server )
      Run->Set_Single_Threaded();
    Default_Output_Names( Opts );
    if ( Do_CV ){
      Run->Test( TestFile, OutputFile );
    }
    else if ( Do_Server ){
      // Special case:   running a Server
      if ( TreeInFile != "" ){
	if ( !Run->GetInstanceBase( TreeInFile ) ){
	  exit( 3 );
	}
      }
      else {
	if ( !Run->Learn( DataFile ) ){
	  exit( 3 );
	}
      }
      if ( WgtInFile != "" ) {
	Run->GetWeights( WgtInFile, WgtType );
      }
      if ( ProbOutFile != "" )
	Run->WriteArrays( ProbOutFile );
      if ( ProbInFile != "" )
	Run->GetArrays( ProbInFile );
      Run->StartServer( ServerPort, Max_Connections );
      exit( 0 );
    }
    else {
      bool do_test = false;
      // normal cases....
      if ( TreeInFile == "" ){
	// normal case
	//   learning and maybe a testing pahse
	if ( WgtOutFile != "" )
	  Run->SetOptions( "ALL_WEIGHTS: true" );
	if ( Run->Prepare( DataFile ) ){
	  if ( WgtOutFile != "" ) {
	    Run->SaveWeights( WgtOutFile );
	  }
	  // If we want to create a namesfile, do it here.
	  //
	  if ( NamesFile != "" ) {
	    Run->WriteNamesFile( NamesFile );
	  }
	  if ( ProbOutFile != "" )
	    Run->WriteArrays( ProbOutFile );
	  
	  do_test = TestFile != "" || Do_Indirect;
	  if ( do_test ||     // something to test ?
	       TreeOutFile != "" ){ // or at least to produce
	    if ( WgtInFile != "" ){
	      if ( Run->GetWeights( WgtInFile, WgtType ) ){
		cerr << "Calculated weights replaced by:" << endl;
		Run->ShowWeights( cerr );
	      }
	      else {
		cerr << "problems reading weights" << endl;
		do_test = false;
	      }
	    }
	    if ( Run->Learn( DataFile ) ){
	      if ( TreeOutFile != "" )
		Run->WriteInstanceBase( TreeOutFile );
	    }
	    else 
	      do_test = false; // no testing because of problems
	  }
	}
      }
      else  {
	// normal case
	//   running a testing phase from recovered tree
	do_test = Run->GetInstanceBase( TreeInFile );
      }
      if ( do_test ){
	Do_Test( Run );
      }
    }
    exit( 0 );
  }
  catch(std::bad_alloc){
    cerr << "ran out of memory somewhere" << endl;
    cerr << "Timbl terminated, Sorry for that" << endl;
  }
  catch(std::exception& e){
    cerr << "a standard exception was raised: '" << e.what() << "'" << endl;
    cerr << "Timbl terminated, Sorry for that" << endl; 
  }
  catch(...){
    cerr << "some exception was raised" << endl;
    cerr << "Timbl terminated, Sorry for that" << endl; 
  }
}
