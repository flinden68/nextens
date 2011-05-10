#ifndef TIMBLEXP_H
#define TIMBLEXP_H

/*
 * TimblExperiment.h
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

using namespace Timbl;

class TimblAPI;
class ConfusionMatrix;

class TimblExperiment: public MBLClass {
  friend class TimblAPI;
 public:
  virtual ~TimblExperiment();
  virtual bool Learn( const std::string& = "" );
  virtual bool Prepare( const std::string& = "" );
  virtual bool Increment( const std::string& )
    { FatalError( "Increment" ); return false; };
  virtual bool Decrement( const std::string& )
    { FatalError( "Decrement" ); return false; };
  virtual bool Expand( const std::string& ){
    FatalError( "Expand" ); return false; };
  virtual bool Remove( const std::string& ){
    FatalError( "Remove" ); return false;};
  virtual bool Test( const std::string&,
		     const std::string&, 
		     const std::string& = "" );
  virtual void InitInstanceBase() = 0;
  virtual bool ReadInstanceBase( const std::string& );
  virtual bool WriteInstanceBase( const std::string& );
  bool WriteNamesFile( const std::string& );
  bool StartServer( const int, const int );
  bool SetSingleThreaded();
  int Estimate() const { return estimate; };
  void Estimate( int e ){ estimate = e; };
  TimblExperiment *CreateClient( int );
  VerbosityFlags ServerVerbosity() { return get_s_verbosity(); };
  int TcpSocket() const { return Socket(); };
  int Max_Connections() const { return max_conn; };
  bool SetOptions( int, const char ** );
  bool SetOptions( const std::string& );
  bool SetOptions( const CL_Options&  );
  bool IndirectOptions( const CL_Options&  );
  bool ConfirmOptions();
  bool DefaultOptions();
  void UseOptions( GetOptClass * );
  bool WriteArrays( const std::string& );
  bool GetArrays( const std::string& );
  bool SaveWeights( const std::string& );
  bool GetWeights( const std::string&, WeightType );
  bool ShowOptions( std::ostream& );
  bool ShowSettings( std::ostream& );
  const std::string& ExpName() const { return exp_name; };
  bool Classify( const std::string& , std::string& );
  bool Classify( const std::string& , std::string&, double & );
  bool Classify( const std::string& , std::string&, std::string&, double & );

  virtual AlgorithmType Algorithm() const = 0;
  const TargetValue *Classify( const std::string& Line, 
			       const ClassDistribution *& db,
			       double & di ){
    bool dummy;
    const TargetValue *res = Classify( Line, di, dummy );
    if ( res )
      db = &BestDistrib;
    return res;
  }
  const TargetValue *Classify( const std::string& Line ){
    bool dum_b;
    double dum_d;
    return Classify( Line, dum_d, dum_b );
  }
 
 const TargetValue *Classify( const std::string& Line, 
			       const ClassDistribution *& db ){
    bool dum_b;
    double dum_d;
    const TargetValue *res = Classify( Line, dum_d, dum_b );
    if ( res )
      db = &BestDistrib;
    return res;
  }
  const TargetValue *Classify( const std::string& Line, 
			       double & di ){
    bool dummy;
    return  Classify( Line, di, dummy );
  }

  typedef std::multimap<FeatureValue*,std::streamsize> MultiIndex;
  bool build_file_index( const std::string&, MultiIndex&  );
  
 protected:
  TimblExperiment( const AlgorithmType, const std::string& = "" );
  virtual TimblExperiment *clone() = 0;
  virtual bool LocalTest( const Instance *, std::ostream&, bool&, bool&, 
			  ConfusionMatrix * );
  virtual bool classify_options_ok( const std::string& ) = 0;
  virtual const TargetValue *Classify( const std::string& , double&,
				       bool& );
  virtual const TargetValue *LocalClassify( const Instance *, 
					    int &, double &,
					    bool & );
  
  virtual bool GetInstanceBase( std::istream& ) = 0;
  virtual void testing_info( std::ostream&, 
			     const std::string&,
			     const std::string& ) = 0;
  virtual bool test_options_ok( const std::string& ) = 0;
  inline void show_results( std::ostream&,
			    const ValueDistribution&,
			    const TargetValue *,
			    const double );
  inline void show_progress( int, time_t );
  inline void show_summary( std::ostream& os,
			    int, int, int, 
			    int, int, time_t, 
			    ConfusionMatrix *,
			    const std::string& = "" );

  inline void show_ignore_info( std::ostream& os );
  inline void show_weight_info( std::ostream& os );
  inline void show_decay_info( std::ostream& os );
  inline void show_metric_info( std::ostream& os );

  bool Initialized;
  GetOptClass *OptParams;
  AlgorithmType algorithm;
  std::string CurrentDataFile;
  std::string WFileName;

 private:
  TimblExperiment( const TimblExperiment& );
  TimblExperiment& operator=( const TimblExperiment& );
  int estimate;
  int max_conn;
}; 

class IB1_Experiment: public TimblExperiment {
 public:
  IB1_Experiment( const int N = DEFAULT_MAX_FEATS, const std::string& s= "",
		  const bool init = true );
  bool Increment( const std::string& );
  bool Decrement( const std::string& );
  bool Expand( const std::string& );
  bool Remove( const std::string& );
  AlgorithmType Algorithm() const { return IB1_a; };
  void InitInstanceBase();
  bool LOO_Test( const std::string&,
		 const std::string&, 
		 const std::string& = "" );
 protected:
  TimblExperiment *clone(){ 
    return new IB1_Experiment( MaxFeats(), "", false ); 
  };
  bool test_options_ok( const std::string& );
  bool classify_options_ok( const std::string& );
  void testing_info( std::ostream&, const std::string&, const std::string& );
  bool Increment( const Instance *I ) { return UnHideInstance( I ); };
  bool Decrement( const Instance *I ) { return HideInstance( I ); };
 private:
  bool GetInstanceBase( std::istream& );
};

class IB2_Experiment: public IB1_Experiment {
 public:
  IB2_Experiment( int N, const std::string& s="" ): 
    IB1_Experiment( N, s ) {
    IB2_offset( -1 );
  }; 
  bool Prepare( const std::string& = "" );
  bool Expand( const std::string& );
  bool Remove( const std::string& );
  bool Learn( const std::string& = "" );
  AlgorithmType Algorithm() const { return IB2_a; };
 protected:
  bool test_options_ok( const std::string& );
  TimblExperiment *clone(){ return new IB2_Experiment( MaxFeats() ); };
  bool Expand_N( const std::string& );
  void testing_info( std::ostream&, const std::string&, const std::string& );
};

class LOO_Experiment: public IB1_Experiment {
 public:
  LOO_Experiment( int N, const std::string& s = "" ): 
    IB1_Experiment( N, s ) {
  };
  bool Test( const std::string&,
	     const std::string&,
	     const std::string& = "" );
  AlgorithmType Algorithm() const { return LOO_a; };
  bool ReadInstanceBase( const std::string& );
 protected:
  bool test_options_ok( const std::string& );
  void testing_info( std::ostream&, const std::string&, const std::string& );
};

class CV_Experiment: public IB1_Experiment {
 public:
  CV_Experiment( int N = DEFAULT_MAX_FEATS, const std::string& s = "" ): 
    IB1_Experiment( N, s ), NumOfFiles( 0 ), FileNames( NULL )
    { };
  ~CV_Experiment(){ delete [] FileNames; };
  bool Learn( const std::string& = "" );
  bool Prepare( const std::string& = "" );
  bool Test( const std::string&,
	     const std::string&,
	     const std::string& = "" );
  AlgorithmType Algorithm() const { return CV_a; };
 protected:
  bool test_options_ok( const std::string& );
  bool get_file_names( const std::string& );
  void testing_info( std::ostream&, const std::string&, const std::string& );
 private:
  CV_Experiment( const CV_Experiment& );
  CV_Experiment& operator=( const CV_Experiment& );
  int NumOfFiles;
  std::string *FileNames;
};

class TRIBL_Experiment: public TimblExperiment {
 public:
  TRIBL_Experiment( const int N = DEFAULT_MAX_FEATS, 
		    const std::string& s = "",
		    const bool init = true ): 
    TimblExperiment( TRIBL_a, s ) {
    if ( init ) InitClass( N );
  };
  void InitInstanceBase();
 protected:
  TimblExperiment *clone(){ 
    return new TRIBL_Experiment( MaxFeats(), "", false ); };
  void testing_info( std::ostream&, const std::string&, const std::string& );
  bool test_options_ok( const std::string& );
  AlgorithmType Algorithm() const { return TRIBL_a; };
  bool classify_options_ok( const std::string& );
  const TargetValue *LocalClassify( const Instance *, int &, double &,
				    bool & );
 private:
  bool GetInstanceBase( std::istream& );
};

class TRIBL2_Experiment: public TimblExperiment {
 public:
  TRIBL2_Experiment( const int N = DEFAULT_MAX_FEATS, 
		     const std::string& s = "",
		     const bool init = true ): 
    TimblExperiment( TRIBL2_a, s ) {
    if ( init ) InitClass( N );
  };
  void InitInstanceBase();
 protected:
  TimblExperiment *clone(){
    return new TRIBL2_Experiment( MaxFeats(), "", false ); };
  void testing_info( std::ostream&, const std::string&, const std::string& );
  bool test_options_ok( const std::string& );
  AlgorithmType Algorithm() const { return TRIBL2_a; };
  bool classify_options_ok( const std::string& );
  const TargetValue *LocalClassify( const Instance *, int &, double &,
				    bool & );
 private:
  bool GetInstanceBase( std::istream& );
};

#ifdef OLD
class IG_Experiment: public TimblExperiment {
 public:
  IG_Experiment( const int N = DEFAULT_MAX_FEATS,
		 const std::string& s = "",
		 const bool init = true ): 
    TimblExperiment( IGTREE_a, s ) { 
    if ( init ) InitClass( N );
  };
  bool Learn( const std::string& f = "" ){
    if ( TimblExperiment::Learn( f ) )
      return PruneInstanceBase( );
    else
      return false;
  };
  AlgorithmType Algorithm() const { return IGTREE_a; };
  void InitInstanceBase();
  bool WriteInstanceBase( const std::string& );
  bool ReadInstanceBase( const std::string& );
 protected:
  TimblExperiment *clone(){ 
    return new IG_Experiment( MaxFeats(), "", false ); };
  bool test_options_ok( const std::string& );
  void testing_info( std::ostream&, const std::string&, const std::string& );
  bool LocalTest( const Instance *, std::ostream&, bool&, bool&, 
		  ConfusionMatrix * );
  bool classify_options_ok( const std::string& );
  const TargetValue *LocalClassify( const Instance *, int &, double &,
				    bool & );
  const TargetValue *IG_Classify( const Instance *, double&, bool& );
  const TargetValue *Classify( const std::string&, double &, bool & );
 private:
  bool GetInstanceBase( std::istream& );
};
#endif

class IG_Experiment: public TimblExperiment {
 public:
  IG_Experiment( const int N = DEFAULT_MAX_FEATS,
		 const std::string& s = "",
		 const bool init = true ): 
    TimblExperiment( IGTREE_a, s ) { 
    if ( init ) InitClass( N );
  };
  bool Learn( const std::string& f = "" );
  AlgorithmType Algorithm() const { return IGTREE_a; };
  void InitInstanceBase();
  bool WriteInstanceBase( const std::string& );
  bool ReadInstanceBase( const std::string& );
 protected:
  TimblExperiment *clone(){ 
    return new IG_Experiment( MaxFeats(), "", false ); };
  bool test_options_ok( const std::string& );
  void testing_info( std::ostream&, const std::string&, const std::string& );
  bool LocalTest( const Instance *, std::ostream&, bool&, bool&, 
		  ConfusionMatrix * );
  bool classify_options_ok( const std::string& );
  const TargetValue *LocalClassify( const Instance *, int &, double &,
				    bool & );
  const TargetValue *IG_Classify( const Instance *, double&, bool& );
  const TargetValue *Classify( const std::string&, double &, bool & );
 private:
  bool GetInstanceBase( std::istream& );
};

#endif // TIMBLEXP_H
