#ifndef INSTANCE_H
#define INSTANCE_H

/*
 * Instance.h
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


#include <stdexcept>
#include <list>
#include <vector>
#include <map>

namespace Timbl {
  using namespace Messages;
  using Hash::StringHash;
  
  enum FeatVal_Stat { Unknown, Singleton, SingletonNumeric, NumericValue,
		      NotNumeric };

  class TargetValue;
  
  class Vfield{
  public:
    Vfield( const TargetValue *val ): value(val){};
    Vfield( const Vfield& in ):
      value(in.value) {};
    virtual Vfield *clone() = 0;
    virtual ~Vfield(){};
    virtual std::ostream& put( std::ostream& ) const = 0;
    virtual std::ostream& put_hashed( std::ostream& ) const = 0;
    virtual void Weight( double ) { abort(); };
    virtual double Weight() const { return 1.0; };
    virtual int Freq() const = 0;
    virtual void Freq( int ) = 0;
    const TargetValue *Value() const { return value; };
    void Value( const TargetValue *t ){  value = t; };
    int Index();
  protected:
    const TargetValue *value;
  private:
    Vfield& operator=( const Vfield& );
  };
  
  class WVfield : public Vfield {
  public:
    WVfield( const TargetValue *val, double w = 1.0 ):
      Vfield(val), weight(w) {};
    WVfield( const WVfield& in ):
      Vfield(in), weight(in.weight) {};
    WVfield *clone(){ return new WVfield( *this ); };
    ~WVfield(){};
    std::ostream& put( std::ostream& ) const;
    std::ostream& put_hashed( std::ostream& ) const;
    void Weight( double w ){ weight = w; };
    double Weight() const { return weight; };
    int Freq() const { abort(); };
    void Freq( int ) { abort(); };
  protected:
    double weight;
  private:
    WVfield& operator=( const WVfield& );
  };
  
  class VFfield : public Vfield {
  public:
    VFfield( const TargetValue *val, int freq = 1 ):
      Vfield(val), frequency(freq) {};
    VFfield( const VFfield& in ):
      Vfield( in ), frequency(in.frequency) {};
    virtual VFfield *clone(){ return new VFfield( *this ); };
    ~VFfield(){};
    std::ostream& put( std::ostream& ) const;
    std::ostream& put_hashed( std::ostream& ) const;
    int Freq() const { return frequency; };
    void Freq( const int f ){  frequency = f; };
    void Weight( double ){ abort(); };
    double Weight() const { return frequency; };
  protected:
    int frequency;
  private:
    VFfield& operator=( const VFfield& );
  };
  
  class WVFfield: public VFfield {
  public:
    WVFfield( const TargetValue *val, int freq=1, double sw=1.0 ):
      VFfield( val, freq ), weight(sw) {};
    WVFfield( const WVFfield& in ):
      VFfield( in ), weight(in.weight) {};
    WVFfield *clone(){ return new WVFfield( *this ); };
    void Weight( double w ){ weight = w; };
    double Weight() const { return weight; };
    std::ostream& put( std::ostream& ) const;
    std::ostream& put_hashed( std::ostream& ) const;
  private:
    double weight;
  };
  
  class Target;
  class WValueDistribution;
  
  class ValueDistribution{
    typedef std::map<int, Vfield *> VDlist;
    friend std::ostream& operator<<( std::ostream&, const ValueDistribution& );
    friend std::ostream& operator<<( std::ostream&, const ValueDistribution * );
    friend class WValueDistribution;
    friend class ClassDistribution;
  public:
    typedef VDlist::const_iterator dist_iterator;
    ValueDistribution(): total_items(0) {};
    ValueDistribution( const ValueDistribution& );
    virtual ~ValueDistribution(){ Clear(); };
    virtual ValueDistribution *clone( ) const;
    void SetFreq( const TargetValue *, const int );
    void SetFreq( const TargetValue *, const int, const double );
    void IncFreq( const TargetValue * );
    bool IncFreq( const TargetValue *, const double );
    void DecFreq( const TargetValue * );
    int SumFrequencies() const;
    int TotalItems() const{ return total_items; };
    bool Empty() const{ return distribution.empty(); };
    virtual const TargetValue* BestTarget( bool &, bool = false ) const;
    const TargetValue* BestTarget( ) const;
    void Merge( const ValueDistribution& );
    void Clear();
    VDlist::const_iterator Begin() const { return distribution.begin(); };
    VDlist::const_iterator End() const { return distribution.end(); };
    bool read_distribution( std::istream &, Target *, bool );
    bool read_distribution_hashed( std::istream &, Target *, bool );
    void DistToString( std::string& ) const;
    const std::string DistToString() const;
    std::string Hashed() const;
    bool ZeroDist() const;
    double Entropy() const;
  protected:
    int total_items;
    VDlist distribution;
    virtual ValueDistribution *create() const { return new ValueDistribution; };
  };

  class WValueDistribution: public ValueDistribution {
  public:
    WValueDistribution(): ValueDistribution(){};
    void MergeWeight( const ValueDistribution&, double );
    WValueDistribution *clone( ) const;
    const TargetValue* BestTarget( bool &, bool = false ) const;
  private:
    WValueDistribution *create() const { return new WValueDistribution; };
  }; 

  class ClassDistribution: public ValueDistribution {
  public:
    ClassDistribution(): ValueDistribution(){};
    void VDCopy( const ValueDistribution& VD );
    void SpecialVDCopy( const ValueDistribution& VD );
    void Normalize();
    void Normalize_1( double, Target* );
  private:
    ClassDistribution *create() const { return new ClassDistribution; };
  }; 

  class ValueClass {
  public:
    ValueClass( const std::string& n, int h ):
      name( n ), name_hash( h ), index( 0 ), Frequency( 1 ) {};
    ValueClass( const std::string& n, int h, int I ):
       name( n ), name_hash( h ), index( I ), Frequency( 1 ) {};
    virtual ~ValueClass() {};
    void ValFreq( int f ){ Frequency = f; };
    int ValFreq( ) const { return Frequency; };
    void incr_val_freq(){ Frequency++; };
    void decr_val_freq(){ Frequency--; };
    int Index() const { return index; };
    const std::string& Name() const { return name; };
    unsigned int Hash() const { return name_hash; };
  protected:
    const std::string& name;
    int name_hash;
    int index;
    int Frequency;
    ValueClass( const ValueClass& );
    ValueClass& operator=( const ValueClass& );
  };
  
  class TargetValue: public ValueClass {
    friend std::ostream& operator<<( std::ostream&, const TargetValue* );
  public:
    TargetValue( const std::string&, int, int );
  };
  
  class SparseValueProbClass {
    typedef std::map< int, double > IDmaptype;
    friend std::ostream& operator<< ( std::ostream&, SparseValueProbClass * );
  public:
    SparseValueProbClass( int d ): dimension(d) {};
    void Assign( const int i, const double d ) { vc_map[i] = d; };
    void Clear() { vc_map.clear(); };
    double vd_distance( SparseValueProbClass * );
    double jd_distance( SparseValueProbClass * );
  private:
    IDmaptype vc_map;
    int dimension;
  };
  
  class SymetricMatrixClass {
    typedef std::vector< double > IDvec;
  public:
    SymetricMatrixClass(): size(0){};
    void Clear() { my_mat.clear(); };
    void Assign( int i, int j, double d ){ 
      if ( i < size &&
	   j < size )
	if ( i <=j )
	  my_mat[j][i] = d; 
	else
	  my_mat[i][j] = d; 
      else
	throw std::out_of_range("SymetricMatrix values");
    };
    double Extract( int i, int j ) const { 
      if ( i < size &&
	   j < size )
	if ( i <= j )
	  return my_mat[j][i]; 
	else
	  return my_mat[i][j]; 
      else 
	throw std::out_of_range("SymetricMatrix values");
    };
    void Reserve( int s ){ 
      my_mat.resize( s ); // reserve x and init with empty vectors
      try {
	for ( int i=0; i < s; ++i )
	  my_mat[i].resize( i+1 ); // reserve doubles and init with 0.0
      }
      catch ( ... ){
	my_mat.clear();
	throw;
      }
      size = s;
    };
    unsigned int NumBytes(void){
      return ((size * (size+1) * sizeof(double))/2 ) +
	(size+1) * sizeof( std::vector<double> ); 
    }
  private:
    int size;
    std::vector<IDvec> my_mat;
  };

  class SparseSymetricMatrixClass {
    typedef std::map< int, double > IDmap;
  public:
    void Clear() { my_mat.clear(); };
    void Assign( int i, int j, double d ){ 
      if ( i == j )
	return;
      if ( i <j )
	my_mat[j][i] = d; 
      else
	my_mat[i][j] = d; 
    };
    double Extract( int i, int j ) { 
      if ( i == j ){
	return 0.0;
      }
      if ( i < j ){
	std::map<int, IDmap>::iterator it1 = my_mat.find(j);
	if ( it1 != my_mat.end() ){
	  IDmap::iterator it2 = it1->second.find(i);
	  if ( it2 != it1->second.end() )
	    return it2->second; 
	}
      }
      else {
	std::map<int, IDmap>::iterator it1 = my_mat.find(i);
	if ( it1 != my_mat.end() ){
	  IDmap::iterator it2 = it1->second.find(j);
	  if ( it2 != it1->second.end() )
	    return it2->second; 
	}
      }
      return 0.0;
    };
    unsigned int NumBytes(void){
      unsigned int tot = sizeof(std::map<int, IDmap>);
      std::map<int, IDmap>::iterator it1 = my_mat.begin();
      while ( it1 != my_mat.end() ){
	tot +=  sizeof(IDmap);
	IDmap::iterator it2 = it1->second.begin();
	while ( it2 != (*it1).second.end() ){
	  tot += sizeof(double);
	  ++it2;
	}
	++it1;
      }
      return tot;
    }
  private:
    std::map<int, IDmap> my_mat;
  };

  class FeatureValue: public ValueClass {
    friend class Feature;
    friend struct D_D;
    friend std::ostream& operator<<( std::ostream&, const FeatureValue * );
  public:
    FeatureValue( const std::string& );
    FeatureValue( const std::string&, int, int );
    ~FeatureValue();
    void ReconstructDistribution( const ValueDistribution& vd ) { 
      TargetDist.Merge( vd );
      Frequency = TargetDist.TotalItems();
    };
    double VDDistance( FeatureValue *, int = 1 );
    double JDDistance( FeatureValue *, int = 1 );
  private:
    SparseValueProbClass *ValueClassProb;
    ValueDistribution TargetDist;
    FeatureValue( const FeatureValue& );
    FeatureValue& operator=( const FeatureValue& );
  };
  
  
  class BaseFeatTargClass: public MsgClass {
  public:
    typedef std::map< int, int> IImaptype;
    BaseFeatTargClass( int, int, StringHash * );
    virtual ~BaseFeatTargClass();
    int ArraySize() const { return array_size; };
    int EffectiveValues();
    unsigned int TotalValues() const;
    virtual ValueClass *Lookup( const std::string& ) = 0;
    virtual ValueClass *Value( int i ) = 0;
  protected:
    int CurSize;
    int Increment;
    int array_size;
    StringHash *TokenTree;
    IImaptype Mapping;
    void enlarge_values_array( int );
    int localmapping( int );
    std::vector<ValueClass *>ValuesArrayIntern;
    virtual ValueClass *LookupInternal( const std::string& );
  private:
    BaseFeatTargClass( const BaseFeatTargClass& );
    BaseFeatTargClass& operator=( const BaseFeatTargClass& );
  };
  
  
  class Target: public BaseFeatTargClass {
  public:
    Target( int a, int b, StringHash *T ): BaseFeatTargClass(a,b,T) {};
    TargetValue *add_value( const std::string&, int freq = 1 );
    TargetValue *add_value( int, int freq = 1 );
    TargetValue *Lookup( const std::string& s ){
      return (TargetValue *)LookupInternal( s ); };
    TargetValue *ReverseLookup( int );
    TargetValue *Value( int i ){
      return (TargetValue *)ValuesArrayIntern[i]; };
    bool decrement_value( TargetValue * );
    bool increment_value( TargetValue * );
    TargetValue *MajorityClass() const;
  };
  
  class Feature: public BaseFeatTargClass {
  public:
    Feature( int a, int b, StringHash *T );
    bool Ignore() const { return ignore; };
    void Ignore( const bool val ){ ignore = val; };
    bool Numeric() const { return numeric; };
    void Numeric( const bool val ){ numeric = val; };
    MetricType Metric() const { return metric; };
    void Metric( const MetricType M ){ metric = M; };
    double Weight() const { return weight; };
    void Weight( const double w ) { weight = w; };
    double InfoGain() const { return info_gain; };
    void InfoGain( const double w ){ info_gain = w; };
    double SplitInfo() const { return split_info; };
    void SplitInfo( const double w ){ split_info = w; };
    double GainRatio() const { return gain_ratio; };
    void GainRatio( const double w ){ gain_ratio = w; };
    double ChiSquare() const { return chi_square; };
    void ChiSquare( const double w ){ chi_square = w; };
    double SharedVariance() const { return shared_variance; };
    void SharedVariance( const double w ){ shared_variance = w; };
    double Min() const { return n_min; };
    void Min( const double val ){ n_min = val; };
    double Max() const { return n_max; };
    void Max( const double val ){ n_max = val; };
    ~Feature();
    FeatureValue *add_value( const std::string&, TargetValue * );
    FeatureValue *add_value( int, TargetValue * );
    FeatureValue *Lookup( const std::string& s ) { 
      return (FeatureValue *)LookupInternal( s ); };
    FeatureValue *Value( int i ){ 
      return (FeatureValue *)ValuesArrayIntern[i]; };
    bool decrement_value( FeatureValue *, TargetValue * );
    bool increment_value( FeatureValue *, TargetValue * );
    bool AllocSparseArrays( int );
    void InitSparseArrays();
    bool ArrayRead(){ return vcpb_read; };
    bool matrix_present( );
    unsigned int matrix_byte_size( );
    double ValueDistance( FeatureValue *, FeatureValue *, int );
    double JeffreyDistance( FeatureValue *, FeatureValue *, int );
    bool store_matrix( MetricType, int = 1 );
    void delete_matrix();
    void print_matrix( bool s = false );
    void print_vc_pb_array( std::ostream& );
    bool read_vc_pb_array( std::istream &  );
    FeatVal_Stat prepare_numeric_stats(  );
    void Statistics( double, Target *, bool );
    void NumStatistics( double, Target *, int, bool );
    void ClipFreq( int f ){ matrix_clip_freq = f; };  
    int ClipFreq() const { return matrix_clip_freq; };
 private:
    MetricType metric;
    bool ignore;
    bool numeric;
    bool vcpb_read;
    enum ps_stat{ ps_undef, ps_failed, ps_ok };
    enum ps_stat PrestoreStatus;
    MetricType Prestored_metric;
    double entropy;
    double info_gain;
    double split_info;
    double gain_ratio;
    double chi_square;
    double shared_variance;
    int matrix_clip_freq;
    SparseSymetricMatrixClass metric_matrix;
    long int *n_dot_j;
    long int* n_i_dot;
    double n_min;
    double n_max;
    int SaveSize;
    int SaveNum;
    double weight;
    void Statistics( double );
    void NumStatistics( std::vector<FeatureValue *>&, double, int );
    void ChiSquareStatistics( std::vector<FeatureValue *>&, int, int );
    void ChiSquareStatistics( int, int );
    void SharedVarianceStatistics( Target *, int );
    Feature( const Feature& );
    Feature& operator=( const Feature& );
  };
  
  class Instance {
    //
    // Helper to pass information around. Could just be a struct also.
    friend std::ostream& operator<<(std::ostream&, const Instance * );
  public:
    FeatureValue **FV;
    TargetValue *TV;
    Instance( int );
    ~Instance();
    double ExemplarWeight() const { return sample_weight; }; 
    void ExemplarWeight( const double sw ){ sample_weight = sw; }; 
  private:
    int Size; // length of FV
    double sample_weight; // relative weight
    Instance( const Instance& );
    Instance& operator=( const Instance& );
  };
  
}
#endif
