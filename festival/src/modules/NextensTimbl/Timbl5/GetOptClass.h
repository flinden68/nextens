#ifndef GETOPTCLASS_H
#define GETOPTCLASS_H

/*
 * GetOptClass.h
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

#include <list>
#include <iosfwd>

class TimblExperiment;

namespace Timbl {
  using namespace Messages;
  using namespace Common;

  class CL_item {
    friend std::ostream& operator<<( std::ostream&, CL_item& );
  public:
    CL_item( char c, const std::string o, bool m=false ): 
      opt_char(c), option( o ), mood( m ){};
    CL_item( const CL_item& in ):
      opt_char( in.opt_char ), option(in.option), mood(in.mood){
    };
    CL_item& operator=( const CL_item& );
    bool Mood() const { return mood; };
    char OptChar() const { return opt_char; };
    const std::string& Option() const { return option; };
  private:
    char opt_char;
    std::string option;
    bool mood;
  };

  inline std::ostream& operator<<( std::ostream& os, CL_item& it ){
    os << (it.mood ? " +": "-" ) << it.opt_char << it.option;
    return os;
  }

  typedef std::list<CL_item> CommandLine;

  CommandLine *Split_Command_Line( const int, const char * const * ); 

  class CL_Options;

  class GetOptClass: public MsgClass {
  public:
    GetOptClass( CL_Options&  );
    virtual ~GetOptClass();
    GetOptClass *Clone( int = 0 ) const;
    bool parse_options( const CL_Options&, const int=0 );
    void set_default_options( const int=0 );
    bool definitive_options( TimblExperiment * );
    AlgorithmType Algo() const { return local_algo; };
    int MaxFeatures() const { return MaxFeats; };
    
  private:  
    AlgorithmType local_algo;
    MetricType local_metric;
    OrdeningType local_order;
    WeightType local_weight;
    InputFormatType LocalInputFormat;
    DecayType local_decay;
    double local_decay_alfa;
    double local_decay_beta;
    int local_normalisation;
    double local_norm_factor;
    int MaxFeats;
    int no_neigh;
    int mvd_limit;
    int estimate;
    int maxbests;
    int clip_freq;
    int BinSize;
    int bootstrap_lines;
    int f_length;
    int local_progress;
    int seed;
    int treshold;
    VerbosityFlags MyVerbosity;
    bool MaxFeatsSet;
    bool opt_init;
    bool opt_changed;
    bool do_exact;
    bool do_hashed;
    bool min_present;
    bool N_present;
    bool keep_distributions;
    bool do_sample_weights;
    bool do_ignore_samples;
    bool do_ignore_samples_test;
    bool do_query;
    bool probabilistic;
    bool do_all_weights;
    bool do_sloppy_loo;
    MetricType *MetricsArray;
    int parent_socket;
    void Error( const std::string& );
    inline bool parse_range( std::string&, 
			     std::string::iterator&,
			     MetricType, 
			     MetricType * );
    inline bool parse_metrics( const std::string&,
			       MetricType&, MetricType * );
    GetOptClass( const GetOptClass& );
    GetOptClass& operator=( const GetOptClass& );
  };

  class CL_Options {
    friend bool GetOptClass::parse_options( const CL_Options&,
					    const int ); 
    friend std::ostream& operator<<( std::ostream&, const CL_Options& );
  public:
    CL_Options( const int, const char * const * );
    CL_Options( const std::string& );
    ~CL_Options();
    bool Present( const char );
    bool Find( const char, std::string&, bool& );
    bool Delete( const char, bool = true );
    void Add( const char, const std::string&, bool );
  private:
    CL_Options( const CL_Options& );
    CL_Options& operator=( const CL_Options& );
    CommandLine *Opts;
  };


}
#endif
