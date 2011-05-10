#ifndef MBLCLASS_H
#define MBLCLASS_H

/*
 * MBLClass.h
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

namespace Timbl {
  using namespace Common;
  using namespace Messages;

  class bestrec;
  class InstanceBase_base;

  class MBLClass {
    typedef double (MBLClass::*FFIntMemberFn)( FeatureValue *, 
					       FeatureValue *, 
					       int ) const;
  public:
    bool SetOption( const std::string& );
    bool ShowSettings( std::ostream& ) const;
    LogStream& my_err() const { return *myerr; };
    LogStream& my_log() const { return *mylog; };
    LogStream& my_debug() const { return *mydebug; };
    bool ShowBestNeighbors( std::ostream &, bool, bool ) const;
    bool Verbosity( VerbosityFlags v ) const { 
      return verbosity & v; };
    void SetVerbosityFlag( VerbosityFlags v ) { verbosity |= v; };
    void ResetVerbosityFlag( VerbosityFlags v ) { verbosity &= ~v; };
    
  protected: 
    enum PhaseType { TrainWords, LearnWords, TestWords, TrainLearnWords };
    enum IB_Stat { Invalid, Normal, Pruned };

    bool WriteArrays( std::ostream& );
    bool ReadArrays( std::istream& );
    bool WriteWeights( std::ostream& );
    bool ReadWeights( std::istream&, WeightType );
    bool WriteNamesFile( std::ostream& ) const;
    bool ShowOptions( std::ostream& ) const;
    void write_perm( std::ostream& ) const;
    void LearningInfo( std::ostream& );
    bool GetCurrentWeights( std::vector<double>& );
    bool ShowWeights( std::ostream& ) const;
    MBLClass( const std::string& = "" );
    virtual ~MBLClass();
    void InitClass( const int );
    MBLClass& operator=( const MBLClass& );
    void Initialize( int = 0 );
    bool PruneInstanceBase( void );
    bool PutInstanceBase( std::ostream& );
    VerbosityFlags get_s_verbosity() const { return *server_verbosity; };
    VerbosityFlags get_verbosity() const { return verbosity; };
    void set_verbosity( VerbosityFlags v ) { verbosity = v; };
    const Instance *chopped_to_instance( PhaseType );
    bool Chop( const std::string& );
    bool HideInstance( const Instance * );
    bool UnHideInstance( const Instance * );
    bool set_input_format( const InputFormatType, const bool );
    void time_stamp( const char *, int =-1, bool=false ) const;
    std::string strip_exemplar_weight( const std::string&, std::string& );
    int ExamineData( const std::string&, const bool );
    int ExamineLine( const std::string&, InputFormatType &, const bool );
    int CountFeatures( std::istream &, const bool );
    bool Init_MBL_test( bool = false );
    const ValueDistribution *TestInstance( const Instance *, double&,
				     InstanceBase_base * = NULL,
				     int = 0 );
    void show_org_input( std::ostream & ) const;
    const ValueDistribution *ExactMatch( const Instance *, int& ) const;
    void NormalizeResult();
    bool ExpInvalid() const { 
      if ( err_count > 0 ){
	InvalidMessage();
	return true;
      }
      else
	return false;
    };
    bool IBAdd( const Instance *);
    IB_Stat IBStatus() const;
    bool get_ranges( const std::string& );
    bool get_IB_Info( std::istream&, bool&, int&, bool&, std::string& );
    int NumOfFeatures() const { return num_of_features; };
    int NumNumFeatures() const { return num_of_num_features; };
    int EffectiveFeatures() const { return effective_feats; };
    double sum_remaining_weights( int );
    void IBInfo( std::ostream&, InstanceBase_base * ) const;
    void IBInfo( std::ostream& os ) const { 
      return IBInfo( os, InstanceBase) ; };
    void MatrixInfo( std::ostream& ) const;
    int RandomSeed() const { return random_seed; };
    void Info( const std::string& ) const;
    void Warning( const std::string& ) const;
    void Error( const std::string& ) const;
    void FatalError( const std::string& ) const;
    int MaxFeats() const { return MaxFeatures; };
    void Socket( int so ){ tcp_socket = so ; };
    int Socket() const { return tcp_socket; };
    int Progress() const { return progress; };
    bool ProbabilisticDraw() const { return probabilistic; };
    bool MBLInit() const { return MBL_init; };
    void MBLInit( bool b ) { MBL_init = b; };
    Target   *Targets;
    Feature **Features;
    Feature **PermFeatures;
    int *Permutation;
    InstanceBase_base *InstanceBase;
    LogStream *mylog;
    LogStream *myerr;
    LogStream *mydebug;
    InputFormatType InputFormat() const { return input_format; };
    int TRIBL_offset() { return tribl_offset; };
    int IB2_offset() { return ib2_offset; };
    void IB2_offset( int n ) { ib2_offset = n; };
    bool Do_Sloppy_LOO() const { return do_sloppy_loo; };
    bool Do_Sparse() const { return do_sparse; };
    bool Do_Samples() const { 
      return do_sample_weighting && !do_ignore_samples; };
    bool Do_Exact() const { return do_exact_match; };
    void Do_Exact( bool b ) { do_exact_match = b; };
    void InitWeights( );
    bool KeepDistributions() const { return keep_distributions; };
    void KeepDistributions( bool f ){ keep_distributions = f; };

    WeightType CurrentWeighting() const { return Weighting; };
    MetricType CurrentMetric() const { return GlobalMetric; };
    DecayType CurrentDecay( double &a, double &b ){
      a = decay_alfa; b = decay_beta; return decay_flag; };
    
    bool IsClone() const { return is_copy; };
    const ValueDistribution *GetDistrib( int );
    void default_order();
    void set_order(void);
    void calc_perm( double * );
    void  calculate_fv_entropy( bool );
    OptionTableClass Options;
    WeightType Weighting;
    MetricType GlobalMetric;
    OrdeningType TreeOrder;
    int num_of_neighbors;
    bool dynamic_neighbors;
    DecayType decay_flag;
    StringHash *TargetStrings;
    StringHash *FeatureStrings;
    std::string exp_name;
    ClassDistribution BestDistrib;
  private:
    int MaxFeatures;
    MetricType *UserOptions;
    InputFormatType input_format;
    double decay_alfa;
    double decay_beta;
    int normalisation;
    double norm_factor;
    const VerbosityFlags *server_verbosity;
    VerbosityFlags verbosity;
    mutable int err_count;
    int num_of_features;
    int num_of_num_features;
    int effective_feats;
    int F_length;
    int random_seed;
    int MaxBests;
    int clip_factor;
    int Bin_Size;
    int BASize;
    int tcp_socket;
    int progress;
    int tribl_offset;
    int ib2_offset;
    int mvd_treshold;
    bool probabilistic;
    bool is_copy;
    bool do_sparse;
    bool do_sloppy_loo;
    bool do_dot_product;
    bool do_exact_match;
    bool hashed_trees;
    bool MBL_init;
    bool need_all_weights;
    bool do_sample_weighting;
    bool do_ignore_samples;
    bool no_samples_test;
    bool chop_examples;
    bool keep_distributions;
    bestrec **BestArray;
    Instance *CurrInst;
    double DBEntropy;
    std::string *ChoppedInput;
    std::string OriginalInput;
    void fill_table();
    void InvalidMessage() const ;
    double calculate_db_entropy( Target * );
    void do_numeric_statistics( );
    void init_best_array(void);
    bool (MBLClass::*ChopInput)( const std::string&, std::string[], int );
    double max_inner_product();
    double in_prod( FeatureValue *, FeatureValue *, int ) const;
    int test_in_graph_dot( FeatureValue **,
			   FeatureValue **, 
			   double *,
			   int,
			   int,
			   int,
			   double,
			   double ) const;

    int test_in_graph_dot_ex( FeatureValue **,
			      FeatureValue **, 
			      double *,
			      int,
			      int,
			      int,
			      double,
			      double,
			      double& ) const;
    
    int test_in_graph( FeatureValue **, 
		       FeatureValue **, 
		       double *,
		       int,
		       int,
		       int,
		       double ) const;

    int test_in_graph_ex( FeatureValue **, 
			  FeatureValue **, 
			  double *,
			  int,
			  int,
			  int,
			  double,
			  double& ) const;

    const ValueDistribution *test_instance( const Instance *, double&,
					    InstanceBase_base * = NULL,
					    int = 0 );
    const ValueDistribution *test_instance_ex( const Instance *, double&,
					       InstanceBase_base * = NULL,
					       int = 0 );
    
    FFIntMemberFn *test_feature_val;
    double add_to_best_array( double, const ValueDistribution *,
			      FeatureValue ** = NULL );
    const ValueDistribution *get_best_dist( double & );
    bool allocate_arrays();
    bool pre_divide1( bool );
    bool pre_divide2( bool );

    double test_value_diff_prestored( FeatureValue **, FeatureValue**,
				      double *, int, int );
    double test_value_diff( FeatureValue **, FeatureValue **,
			    double *, int, int );
    double test_vd( FeatureValue *, FeatureValue *, int ) const;
    double test_jd( FeatureValue *, FeatureValue *, int ) const;
    double test_ov( FeatureValue *, FeatureValue *, int ) const;
    double n_test_ov( FeatureValue *, FeatureValue *, int ) const;
    double RelativeWeight( int );
    void show_input_format( std::ostream& );
    void write_perm_special(std::ostream&);
    bool chop_C4_5_string( const std::string&, std::string[], int );
    bool chop_ARFF_string( const std::string&, std::string[], int );
    bool chop_bin_string( const std::string&, std::string[], int );
    bool chop_Compact_string( const std::string&, std::string[], int );
    bool chop_COLUMNS_string( const std::string&, std::string[], int );
    bool chop_sparse_string( const std::string&, std::string[], int );
    void swap( FeatureValue **, FeatureValue ** );
    bool read_the_vals( std::istream& );
    MBLClass( const MBLClass& );
  };

  bool empty_line( const std::string& , const InputFormatType );

}

#endif // TIMBLCLASS_H
