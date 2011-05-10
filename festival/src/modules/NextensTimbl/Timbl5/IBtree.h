/*
 * IBtree.h
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

#ifndef IBtree_H
#define IBtree_H

namespace Timbl {
  using namespace Messages;
  using Hash::StringHash;

  class IB_InstanceBase;
  class IG_InstanceBase;
  class TRIBL_InstanceBase;
  class TRIBL2_InstanceBase;
  class Feature;
  class FeatureValue;
  class Instance;
  class Target;
  class TargetValue;
  class ValueDistribution;
    
  class IBtree {
    friend class InstanceBase_base;
    friend class IB_InstanceBase;
    friend class IG_InstanceBase;
    friend class TRIBL_InstanceBase;
    friend class TRIBL2_InstanceBase;
    friend std::ostream &operator<<( std::ostream&, const IBtree& );
  private:
    FeatureValue *FValue;
    const TargetValue *TValue;
    ValueDistribution *TDistribution;
    IBtree *link;
    IBtree *next;
    
    IBtree();
    ~IBtree( void );
    inline void show_progress( time_t& );
    IBtree *Reduce( const TargetValue *, time_t& );
    inline IBtree *add_feat_val( FeatureValue *, IBtree ** );
    inline ValueDistribution *sum_distributions( bool );
    inline IBtree *make_unique( const TargetValue * );
    void re_assign_defaults( bool, bool, time_t& ); 
    void assign_defaults( bool, bool, int, time_t& ); 
    void redo_distributions();
    const ValueDistribution *exact_match( const Instance * );
  protected:
    IBtree( const IBtree& );
    IBtree& operator=( const IBtree& );
  };
  
  class InstanceBase_base: public MsgClass {
    friend class IG_InstanceBase;
    friend class TRIBL_InstanceBase;
    friend class TRIBL2_InstanceBase;
    InstanceBase_base( const InstanceBase_base& );
    InstanceBase_base& operator=( const InstanceBase_base& );
  public:
    InstanceBase_base( int, bool, bool );
    virtual ~InstanceBase_base( void );
    void AssignDefaults( void );
    void RedoDistributions();
    bool AddInstance( const Instance * );
    void RemoveInstance( const Instance * );
    virtual bool Merge( InstanceBase_base *, FeatureValue * );
    const ValueDistribution *ExactMatch( const Instance * I ) {
      return InstBase->exact_match( I ); };
    virtual ValueDistribution *InitGraphTest( FeatureValue **, 
					      FeatureValue ** );
    virtual ValueDistribution *NextGraphTest( FeatureValue **, 
					      FeatureValue **, int& );
    unsigned long int GetDistSize( ){ return NumOfTails; };
    virtual ValueDistribution *IG_test( const Instance *, int&,
					const TargetValue *& );
    virtual IB_InstanceBase *TRIBL_test( const Instance *, int,
					 const TargetValue *&,
					 const ValueDistribution *&, int& );
    virtual IB_InstanceBase *TRIBL2_test( const Instance *, 
					  const ValueDistribution *&, int& );
    bool read_hash( std::istream &, StringHash *, StringHash * );
    virtual InstanceBase_base *Clone() = 0;
    virtual void Save( std::ostream &, bool=false ) = 0;
    virtual void Save( std::ostream &, 
		       StringHash *, StringHash *, bool=false ) = 0;
    virtual bool ReadIB( std::istream &, Feature **, Target *, int );
    virtual bool ReadIB( std::istream &, Feature **, Target *,
		 StringHash *, StringHash *, int );
    virtual void Prune( const TargetValue * );
    virtual bool IsPruned() const { return false; };
    void CleanPartition();
    unsigned long int GetSizeInfo( unsigned long int&, double & );
    const ValueDistribution *TopDist(){ return TopDistribution; };
    bool HasDistributions();
    const TargetValue *TopTarget();
  protected:
    bool DefAss;
    bool DefaultsValid;
    bool Random;
    bool PersistentDistributions;
    int Version;
    ValueDistribution *TopDistribution;
    IBtree *InstBase;
    IBtree **RestartSearch;
    IBtree **SkipSearch;
    IBtree **InstPath;
    unsigned long int tree_size;
    
    unsigned int Depth;
    unsigned long int NumOfTails;
    IBtree *read_list( std::istream &, Feature**, Target *, int );
    IBtree *read_local( std::istream &, Feature**, Target *, int );
    IBtree *read_list_hashed( std::istream &, Feature**, Target *, int );
    IBtree *read_local_hashed( std::istream &, Feature**, Target *, int );
    void write_tree( std::ostream &os, IBtree * );
    void write_tree_hashed( std::ostream &os, IBtree * );
    bool read_IB( std::istream &, Feature **, Target *, int );
    bool read_IB( std::istream &, Feature **, Target *,
		  StringHash *, StringHash *, int );
    void SaveIB( std::ostream &, bool );
    void SaveIB( std::ostream &, StringHash *, StringHash *, bool );
  };
  
  class IB_InstanceBase: public InstanceBase_base {
  public:
    IB_InstanceBase( int size, bool rand ):
      InstanceBase_base( size, rand , false ) {
    };
    IB_InstanceBase *Clone();
    void Save( std::ostream &os, bool ){
      SaveIB( os, false );
      };
    void Save( std::ostream &os, StringHash *f, StringHash *t, bool ){
      SaveIB( os, f, t, false );
    };
    ValueDistribution *InitGraphTest( FeatureValue **, 
				      FeatureValue ** );
    ValueDistribution *NextGraphTest( FeatureValue **, 
				      FeatureValue **, int& );
  };

  class IG_InstanceBase: public InstanceBase_base {
  public:
    IG_InstanceBase( int size, bool rand, bool pruned, bool keep_dists ):
      InstanceBase_base( size, rand, keep_dists ), Pruned( pruned ) {};
    IG_InstanceBase *Clone();
    void Save( std::ostream &os, bool keep ){
      SaveIB( os, keep );
      };
    void Save( std::ostream &os, StringHash *f, StringHash *t, bool keep ){
      SaveIB( os, f, t, keep );
    };
    void Prune( const TargetValue * );
    bool IsPruned() const { return Pruned; };
    ValueDistribution *IG_test( const Instance *, int&,
				const TargetValue *& );
    bool ReadIB( std::istream &, Feature **, Target *, int );
    bool ReadIB( std::istream &, Feature **, Target *,
		 StringHash *, StringHash *, int );
    bool Merge( InstanceBase_base *, FeatureValue * );
  protected:
    bool Pruned;
  };

  class TRIBL_InstanceBase: public InstanceBase_base {
  public:
    TRIBL_InstanceBase( int size, bool rand, bool keep_dists ):
      InstanceBase_base( size, rand, keep_dists ), Treshold(-1) {};
    TRIBL_InstanceBase *Clone();
    void Save( std::ostream &os, bool keep_dists ){
      SaveIB( os, keep_dists );
      };
    void Save( std::ostream &os, StringHash *f, StringHash *t, bool keep_dists ){
      SaveIB( os, f, t, keep_dists );
    };
    IB_InstanceBase *TRIBL_test( const Instance *, int,
				 const TargetValue *&,
				 const ValueDistribution *&, int& ); 
  private:
    IB_InstanceBase *IBPartition( IBtree * );
    void AssignDefaults( int );
    int Treshold;
  };

  class TRIBL2_InstanceBase: public InstanceBase_base {
  public:
    TRIBL2_InstanceBase( int size, bool rand, bool keep_dists ):
      InstanceBase_base( size, rand, keep_dists ) {
    };
    TRIBL2_InstanceBase *Clone();
    void Save( std::ostream &os, bool keep ){
      SaveIB( os, keep );
      };
    void Save( std::ostream &os, StringHash *f, StringHash *t, bool keep ){
      SaveIB( os, f, t, keep );
    };
    IB_InstanceBase *TRIBL2_test( const Instance *, 
				  const ValueDistribution *&,
				  int& );
  private:
    IB_InstanceBase *IBPartition( IBtree * );
  };

}
#endif
