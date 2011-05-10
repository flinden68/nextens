/*
 * IBtree.cc
 *
 *    The InstanceTree Class
 *    Building and Maintaining of the InstanceBaseClass.
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
#include <iostream>
#include <iomanip>

#ifdef IRIX64
#include <math.h>
#include <ctype.h>
#include <time.h>
#include <stdio.h>
#else
#include <cmath>
#include <cctype>
#include <ctime>
#include <cstdio>
#endif

#include "Common.h"
#include "MsgClass.h"
#include "Tree.h"
#include "Types.h"
#include "Instance.h"
#include "IBtree.h"

using namespace std;

namespace Timbl {
  using namespace Common;

  static unsigned long int IB_COUNT = 0;
  
  IBtree::IBtree():
    FValue(NULL), TValue( NULL ), TDistribution( NULL ), 
    link(NULL), next(NULL)
  {
    IB_COUNT++;
  }
  
  IBtree::~IBtree(){
    delete TDistribution;
    delete link;
    delete next;
    IB_COUNT--;
  }
  
  inline IBtree *IBtree::add_feat_val( FeatureValue *FV, 
				       IBtree **tree ){
    // Add a Featurevalue to the IB. 
    IBtree **pnt = tree;
    while ( *pnt ){
      if ( (*pnt)->FValue == FV ){
	// already there, so bail out.
	return *pnt;
      }
      else if ( (*pnt)->FValue->Index() < FV->Index() ){
	pnt = &((*pnt)->next);
      }
      else {
	// need to add a new node before the current one
	IBtree *tmp = *pnt;
	*pnt = new IBtree();
	(*pnt)->FValue = FV;
	(*pnt)->next = tmp;
	return *pnt;
      }
    }
    // add at the end.
    *pnt = new IBtree();
    (*pnt)->FValue = FV;
    return *pnt;
  }
  
  static int IBtree_Indent = 0;

  ostream &operator<<( ostream &os, const IBtree& grap ){
    // output an IBtree somewhat orderly.
    const IBtree *pnt = &grap;
    while ( pnt ) {
      if ( pnt->link || pnt->FValue ){
	os << pnt->FValue;
	if ( pnt->TValue )
	  os << "(" << pnt->TValue << ")" ;
	if ( pnt->TDistribution ){
	  os << pnt->TDistribution ;
	}
	if ( pnt->link ){
	  os << "\t";
	  IBtree_Indent++;
	  os << pnt->link;
	  IBtree_Indent--;
	}
	else
	  os << endl;
      }
      else {
	if ( pnt->TValue ){
	  os << "(" << pnt->TValue << ")" ;
	  if ( pnt->link ){
	    os << "\t";
	    IBtree_Indent++;
	    os << pnt->link;
	    IBtree_Indent--;
	  }
	}
	if ( pnt->TDistribution ){
	  os << pnt->TDistribution ;
	}
	os << endl;
      }
      if (pnt->next){
	int j;
	for ( j=0; j<IBtree_Indent; j++ )
	  os << "\t";
      }
      pnt = pnt->next;
    }
    return os;
  }
  
  unsigned long int InstanceBase_base::GetSizeInfo( unsigned long int& CurSize, 
						    double &Compression ){
    unsigned long int MaxSize = (Depth+1) * NumOfTails;
    CurSize = tree_size;
    Compression = 100*(1-(double)CurSize/(double)MaxSize);
    return CurSize * sizeof(IBtree);
  }
  
  void InstanceBase_base::write_tree( ostream &os, IBtree *pnt ){
    // part of saving a tree in a recoverable manner
    os << " (" << pnt->TValue << " ";
    if ( pnt->link ){
      if ( PersistentDistributions && pnt->TDistribution ){
	os << pnt->TDistribution->DistToString();
      }
      pnt = pnt->link;
      if ( pnt->FValue ){
	os << "[";
	while ( pnt ){
	  os << pnt->FValue << " ";
	  write_tree( os, pnt );
	  pnt = pnt->next;
	  if ( pnt )
	    os << ",";
	}
	os << "]\n";
      }
      else if ( !PersistentDistributions && pnt->TDistribution ){
	os << pnt->TDistribution->DistToString();
      }
    }
    else if ( pnt->TDistribution ){
      os << pnt->TDistribution->DistToString();
    }
    os << ")\n";
  }
  
  void InstanceBase_base::write_tree_hashed( ostream &os, IBtree *pnt ){
    // part of saving a tree in a recoverable manner
    os << "(" << pnt->TValue->Hash();
    if ( pnt->link ){
      if ( PersistentDistributions && pnt->TDistribution ){
	os << pnt->TDistribution->Hashed();
      }
      pnt = pnt->link;
      if ( pnt->FValue ){
	os << "[";
	while ( pnt ){
	  os << pnt->FValue->Hash();
	  write_tree_hashed( os, pnt );
	  pnt = pnt->next;
	  if ( pnt )
	    os << ",";
	}
	os << "]\n";
      }
      else if ( pnt->TDistribution && !PersistentDistributions ){
	os << pnt->TDistribution->Hashed();
      }
    }
    else if ( pnt->TDistribution ){
      os << pnt->TDistribution->Hashed();
    }
    os << ")\n";
  }
  
  const TargetValue *InstanceBase_base::TopTarget(){
    bool dummy;
    return TopDistribution->BestTarget( dummy, Random); 
  }

  void InstanceBase_base::SaveIB( ostream &os, bool persist ){
    // save an IBtree for later use.
    bool temp_persist = PersistentDistributions;
    PersistentDistributions = persist;
    AssignDefaults();
    os << "# Version " << Version << "\n#\n(" 
       << TopTarget() << " " << TopDistribution->DistToString();
    IBtree *pnt = InstBase;
    if ( pnt ){
      os << "[";
      while ( pnt ){
	os << pnt->FValue;
	write_tree( os, pnt );
	pnt = pnt->next;
	if ( pnt )
	  os << ",";
      }
      os << "]\n";
    }
    os << ")\n";
    PersistentDistributions = temp_persist;
  }
  
  
  void save_hash( ostream &os, StringHash *cats, StringHash *feats ){
    int Size = cats->NumOfEntries();
    os << "Classes" << endl;
    int i;
    for ( i=1; i <= Size; i++ )
      os << i << "\t" << cats->ReverseLookup( i ) << endl;
    Size = feats->NumOfEntries();
    os << "Features" << endl;
    for ( i=1; i <= Size; i++ )
      os << i << "\t" << feats->ReverseLookup( i ) << endl;
    os << endl;
  }
  
  void InstanceBase_base::SaveIB( ostream &os, 
				  StringHash *cats, 
				  StringHash *feats,
				  bool persist ){
    // save an IBtree for later use.
    bool temp_persist =  PersistentDistributions;
    PersistentDistributions = persist;
    AssignDefaults();
    os << "# Version " << Version << " (Hashed)\n#" << endl;
    save_hash( os , cats, feats );
    os << "(" << TopTarget()->Hash() << TopDistribution->Hashed();
    IBtree *pnt = InstBase;
    if ( pnt ){
      os << "[";
      while ( pnt ){
	os << pnt->FValue->Hash();
	write_tree_hashed( os, pnt );
	pnt = pnt->next;
	if ( pnt )
	  os << ",";
      }
      os << "]\n";
    }
    os << ")\n";
    PersistentDistributions = temp_persist;
  }
  
  IBtree* InstanceBase_base::read_list( istream &is, 
					Feature** Feats,
					Target  *Targ,
					int level ){
    IBtree *result = NULL, **pnt;
    pnt = &result;
    bool goon = true;
    char delim;
    while ( goon ) {
      is >> delim;    // skip the opening `[` or separating ','
      *pnt = read_local( is, Feats, Targ, level );
      if ( !(*pnt) ){
	delete result;
	return NULL;
      }
      pnt = &((*pnt)->next);
      goon = ( look_ahead(is) == ',' );
    }
    is >> delim;    // skip closing `]`
    return result;
  }
  
  IBtree* InstanceBase_base::read_list_hashed( istream &is, 
					       Feature** Feats,
					       Target  *Targ,
					       int level ){
    IBtree *result = NULL, **pnt;
    pnt = &result;
    bool goon = true;
    char delim;
    while ( goon ) {
      is >> delim;    // skip the opening `[` or separating ','
      *pnt = read_local_hashed( is, Feats, Targ, level );
      if ( !(*pnt) ){
	delete result;
	return NULL;
      }
      pnt = &((*pnt)->next);
      goon = ( (look_ahead(is) == ',') );
    }
    is >> delim;    // skip closing `]`
    return result;
  }
  
  IBtree *InstanceBase_base::read_local( istream &is,
					 Feature** Feats,
					 Target *Targ,
					 int level ){
    IBtree *result = new IBtree();
    tree_size++;
    string buf;
    char delim;
    is >> ws >> buf;
    result->FValue = Feats[level]->add_value( buf, NULL );
    is >> delim;
    if ( delim != '(' ){
      Error( "missing `(` in Instance Base file" );
      delete result;
      return NULL;
    }
    is >> ws >> buf;
    result->TValue = Targ->Lookup( buf );
    if ( look_ahead(is) == '{' ){
      result->TDistribution = new ValueDistribution();
      if ( !result->TDistribution->read_distribution( is, Targ, false ) )
	Error( "problems reading a distribution from InstanceBase file" );
      // also we have to update the targetinformation of the featurevalue
      // so we can recalculate the statistics later on.
      if ( result->FValue->ValFreq() > 0 )
	result->FValue->ReconstructDistribution( *(result->TDistribution) );
    }
    if ( look_ahead(is) == '[' ){
      result->link = read_list( is, Feats, Targ, level+1 );
      if ( !(result->link) ){
	delete result;
	return NULL;
      }
    }
    else if ( look_ahead(is) == ')' && result->TDistribution ){
      result->link = new IBtree();
      tree_size++;
      result->link->TValue = result->TValue;
      if ( PersistentDistributions )
	result->link->TDistribution = result->TDistribution->clone();
      else {
	result->link->TDistribution = result->TDistribution;
	result->TDistribution = NULL;
      }
      NumOfTails++;
    }
    is >> delim;
    if ( delim != ')' ){
      Error( "missing `)` in Instance Base file" );
      delete result;
      return NULL;
    }
    return result;
  }
  
  IBtree *InstanceBase_base::read_local_hashed( istream &is,
						Feature** Feats,
						Target *Targ,
						int level ){
    IBtree *result = new IBtree();
    tree_size++;
    char delim;
    int index;
    is >> index;
    result->FValue = Feats[level]->add_value( index, NULL );
    is >> delim;
    if ( delim != '(' ){
      Error( "missing `(` in Instance Base file" );
      delete result;
      return NULL;
    }
    is >> index;
    result->TValue = Targ->ReverseLookup( index );
    if ( look_ahead(is) == '{' ){
      //
      // A distribution is found, must be the last featurevalue
      // (the dummy node is not stored) 
      // OR we have Persistent Distributions
      result->TDistribution = new ValueDistribution();
      if ( !result->TDistribution->read_distribution_hashed( is, Targ,
							     false ) )
	Error( "problems reading a hashed distribution from InstanceBase file" );
    }
    if ( look_ahead(is) == '[' ){
      result->link = read_list_hashed( is, Feats, Targ, level+1 );
      if ( !(result->link) ){
	delete result;
	return NULL;
      }
    }
    else if ( look_ahead(is) == ')' && result->TDistribution ){
      // 
      // make a dummy node for the targetdistributions just read
      //
      result->link = new IBtree();
      tree_size++;
      result->link->TValue = result->TValue;
      if ( PersistentDistributions )
	result->link->TDistribution = result->TDistribution->clone();
      else {
	result->link->TDistribution = result->TDistribution;
	result->TDistribution = NULL;
      }
      NumOfTails++;
    }
    is >> delim;
    if ( delim != ')' ){
      Error( "missing `)` in Instance Base file" );
      delete result;
      return NULL;
    }
    return result;
  }
  
  bool InstanceBase_base::ReadIB( istream &is, 
				  Feature **Feats, Target *Targs,
				  int expected_version ){
    if ( read_IB( is, Feats, Targs, expected_version ) ){
      InstBase->redo_distributions();
      return true;
    }
    else
      return false;
  }
  
  bool IG_InstanceBase::ReadIB( istream &is, 
				Feature **Feats, Target *Targs,
				int expected_version ){
    if ( read_IB( is, Feats, Targs, expected_version ) ){
      return true;
    }
    else
      return false;
  }
  
  bool InstanceBase_base::read_IB( istream &is, 
				   Feature **Feats, Target *Targs,
				   int expected_version ){
    string buf;
    NumOfTails = 0;
    DefAss = true;  // always for a restored tree
    DefaultsValid = true; // always for a restored tree
    Version = expected_version;
    char delim;
    is >> delim;
    if ( delim != '(' ){
      Error( "missing first `(` in Instance Base file" );
    }
    else {
      // first we get the value of the TopTarget. It's in the file
      // for backward compability
      is >> ws >> buf;
      if ( look_ahead(is) == '{' ){
	// Now read the TopDistribution, to get the Targets
	// in the right order in Targ
	if ( !TopDistribution->read_distribution( is, Targs, true ) )
	  Error( "problems reading Top Distribution from Instance Base file" );
      }
      if ( look_ahead( is ) == '[' ){
	InstBase = read_list( is, Feats, Targs, 0 );
      }
      is >> ws >> buf;
      if ( buf.empty() || buf[0] != ')' )
	Error( "missing last `)` in Instance base file, found " + buf );
    }
    return (InstBase != NULL);
  }
  
  bool InstanceBase_base::read_hash( istream &is, 
				     StringHash *cats, StringHash *feats ){
    string line;
    is >> ws;
    is >> line;
    if ( !compare_nocase( line, "Classes" ) ){
      Error( "missing 'Classes' keyword in Hashinfo" );
      return false;
    }
    is >> ws;
    vector<string> vals;
    int i;
    while ( getline( is, line ) ){
      i = split( line, vals );
      if ( i == 2 )
	// just ignore index!
	cats->Hash( vals[1] );
      else
	break;
      is >> ws;
    }
    if ( !compare_nocase( line, "Features" ) ){
      Error( "missing 'Features' keyword in Hashinfo" );
      return false;
    }
    while ( getline( is, line ) ){
      i = split( line, vals );
      if ( i == 2 )
	// just ignore index!
	feats->Hash( vals[1] );
      else
	break;
    }
    return true;
  }
  
  bool InstanceBase_base::ReadIB( istream &is, 
				  Feature **Feats, Target *Targs,
				  StringHash *cats, StringHash *feats,
				  int expected_version ){
    if ( read_IB( is, Feats, Targs, cats, feats, expected_version ) ){
      InstBase->redo_distributions();
      return true;
    }
    else
      return false;
  }
  
  bool IG_InstanceBase::ReadIB( istream &is, 
				Feature **Feats, Target *Targs,
				StringHash *cats, StringHash *feats,
				int expected_version ){
    if ( read_IB( is, Feats, Targs, cats, feats, expected_version ) ){
      return true;
    }
    else
      return false;
  }
  
  bool InstanceBase_base::read_IB( istream &is, 
				   Feature **Feats, Target *Targs,
				   StringHash *cats, StringHash *feats,
				   int expected_version ){
    char delim;
    int dum;
    NumOfTails = 0;
    DefAss = true;  // always for a restored tree
    DefaultsValid = true; // always for a restored tree
    Version = expected_version;
    read_hash( is, cats, feats );
    is >> delim;
    if ( delim != '(' ){
      Error( "missing first `(` in Instance Base file" );
    }
    else {
    // first we get the value of the TopTarget. It's in the file
      // for backward compability
      is >> dum;
      if ( look_ahead(is) == '{' ){
	// Now read the TopDistribution, to get the Targets
	// in the right order in Targ
	if ( !TopDistribution->read_distribution_hashed( is, Targs, 
							 true ) )
	  Error( "problems reading Top Distribution from Instance Base file" );
      }
      else {
	Error( "problems reading Top Distribution from Instance Base file" );
      }
      if ( look_ahead( is ) == '[' ){
	InstBase = read_list_hashed( is, Feats, Targs, 0 );
      }
      is >> delim;
      if ( delim != ')' )
	Error( "missing last `)` in Instance base file, found " + delim );
    }
    return (InstBase != NULL);
  }
  
  bool InstanceBase_base::HasDistributions( ){
    if ( InstBase && InstBase->link )
      return InstBase->link->TDistribution != NULL;
    else
      return false;
  }

  inline ValueDistribution *IBtree::sum_distributions( bool keep ){
    // create a new distribution at this level by summing up the
    // distibutions of all branches.
    ValueDistribution *result;
    if ( !keep ){
      if ( TDistribution )
	if ( FValue ){
	  result = TDistribution;
	  TDistribution = NULL;
	}
	else
	  result = TDistribution->clone();
      else
	result = new ValueDistribution();
      IBtree *pnt = this->next;
      while ( pnt ){
	if ( pnt->TDistribution )
	  result->Merge( *(pnt->TDistribution) );
	if ( FValue ){
	  delete pnt->TDistribution;
	  pnt->TDistribution = NULL;
	}
	pnt = pnt->next;
      }
    }
    else {
      if ( TDistribution )
	result = TDistribution->clone();
      else
	result = new ValueDistribution();
      IBtree *pnt = this->next;
      while ( pnt ){
	if ( pnt->TDistribution ){
	  result->Merge( *(pnt->TDistribution) );
	}
	pnt = pnt->next;
      }
    }
    return result;
  }
  
  void IBtree::assign_defaults( bool Random, bool persist, int level,
				time_t& StartTime ){
    // recursively gather Distribution information up to the top.
    // at each Node we use that info to calculate the Default target.
    // when level > 1 the info might be persistent for IGTREE use
    IBtree *pnt = this;
    bool dummy;
    while ( pnt ){
      if ( pnt->link && !pnt->TDistribution ){
	pnt->link->assign_defaults( Random, persist, level-1, StartTime );
	pnt->TDistribution = pnt->link->sum_distributions( level > 1
							   && persist );
      }
      pnt->TValue = pnt->TDistribution->BestTarget( dummy, Random );
      pnt = pnt->next;
      show_progress( StartTime );
    }
  }
  
  void IBtree::re_assign_defaults( bool Random, 
				   bool persist,
				   time_t& StartTime ){
    // recursively gather Distribution information up to the top.
    // at each Node we use that info to calculate the Default target.
    IBtree *pnt = this;
    bool dummy;
    while ( pnt ){
      if ( pnt->link ){
	delete pnt->TDistribution;
	pnt->link->re_assign_defaults( Random, persist, StartTime );
	pnt->TDistribution = pnt->link->sum_distributions( persist );
      }
      pnt->TValue = pnt->TDistribution->BestTarget( dummy, Random );
      pnt = pnt->next;
      show_progress( StartTime );
    }
  }
  
  void IBtree::redo_distributions(){
    // recursively gather Distribution information up to the top.
    // removing old info...
    // at each node we also Reconstruct Feature distributions
    // we keep the Target value that was given!
    IBtree *pnt = this;
    while ( pnt ){
      if ( pnt->link ){
	pnt->link->redo_distributions();
	delete pnt->TDistribution;
	pnt->TDistribution = pnt->link->sum_distributions( false );
	if ( pnt->FValue->ValFreq() > 0 )
	  pnt->FValue->ReconstructDistribution( *(pnt->TDistribution) );
      }
      pnt = pnt->next;
    }
  }

  inline void IBtree::show_progress( time_t& StartTime ){
    time_t Time;
    time(&Time);
    if ( (Time - StartTime) != 0  &&
	 (Time - StartTime) % 10 == 0 ) { // every 10 seconds
      time(&StartTime);
      cout << "."; cout.flush();
    }
  }
  
  inline IBtree *IBtree::make_unique( const TargetValue *Top ){
    // remove branches with the same target as the Top, except when they
    // still have a subbranch, which means that they are an exception.
    IBtree **tmp, *dead, *result;
    result = this;
    tmp = &result;
    while ( *tmp ){
      if ( (*tmp)->TValue == Top && (*tmp)->link == NULL ){
	dead = *tmp;
	*tmp = (*tmp)->next;
	dead->next=NULL;
	delete dead;
      }
      else
	tmp = &((*tmp)->next);
    }
    return result;
  }
  
  inline IBtree *IBtree::Reduce( const TargetValue *Top,
				 time_t& StartTime ){
    // recursively cut default nodes, (with make unique,) starting at the
    // leaves of the Tree and moving back to the top.
    IBtree *pnt = this;
    while ( pnt ){
      if ( pnt->link != NULL )
	pnt->link = pnt->link->Reduce( pnt->TValue, StartTime );
      show_progress( StartTime );
      pnt = pnt->next;
    }
    return make_unique( Top );
  }
  
  const ValueDistribution *IBtree::exact_match( const Instance *Inst ){
    // Is there an exact match between the Instance and the IB
    // If so, return the best Distribution.
    IBtree *pnt = this;
    int pos = 0;
    while ( pnt ){
      if ( pnt->link == NULL ){
	if ( pnt->TDistribution->ZeroDist() )
	  return NULL;
	else
	  return pnt->TDistribution;
      }
      else if ( pnt->FValue == Inst->FV[pos] ){
	if ( pnt->FValue->ValFreq() == 0 )
	  return NULL;
	else {
	  pnt = pnt->link;
	  pos++;
	}
      }
      else
	pnt = pnt->next;
    }
    return NULL;
  }
  
  InstanceBase_base::InstanceBase_base( int depth, 
					bool Rand, 
					bool persist ):
    DefAss( false ),
    DefaultsValid( false ),
    Random( Rand ),
    PersistentDistributions( persist ),
    Version( 4 ),
    TopDistribution( new ValueDistribution ),
    InstBase( NULL ),
    RestartSearch( new IBtree *[depth] ),
    SkipSearch( new IBtree *[depth] ),
    InstPath( new IBtree *[depth] ),
    tree_size( 0 ),
    Depth( depth ),
    NumOfTails( 0 )
    {}
  
  InstanceBase_base::~InstanceBase_base(){
    if ( InstPath )
      delete [] InstPath;
    if ( SkipSearch )
      delete [] SkipSearch;
    if ( RestartSearch )
      delete [] RestartSearch;
    delete InstBase;
    delete TopDistribution;
  }
  
  IB_InstanceBase *IB_InstanceBase::Clone(){
    IB_InstanceBase *result =
      new IB_InstanceBase( Depth, Random );
    result->DefAss = DefAss;
    result->DefaultsValid = DefaultsValid;
    result->NumOfTails = NumOfTails; // only usefull for Server???
    result->InstBase = InstBase;
    delete result->TopDistribution;
    result->TopDistribution =
      result->InstBase->sum_distributions( false );
    result->tree_size = 0;
    return result;
  }
  
  IG_InstanceBase *IG_InstanceBase::Clone(){
    IG_InstanceBase *result =
      new IG_InstanceBase( Depth, Random, Pruned, PersistentDistributions );
    result->Pruned = Pruned;
    result->DefAss = DefAss;
    result->DefaultsValid = DefaultsValid;
    result->NumOfTails = NumOfTails; // only usefull for Server???
    result->InstBase = InstBase;
    if ( PersistentDistributions ){
      result->TopDistribution =
	result->InstBase->sum_distributions( true );
    }
    else if ( TopDistribution )
      result->TopDistribution = TopDistribution->clone();
    result->tree_size = 0;
    return result;
  }

  TRIBL_InstanceBase *TRIBL_InstanceBase::Clone(){
    TRIBL_InstanceBase *result =
      new TRIBL_InstanceBase( Depth, Random, PersistentDistributions );
    result->Treshold = Treshold;
    result->DefAss = DefAss;
    result->DefaultsValid = DefaultsValid;
    result->NumOfTails = NumOfTails; // only usefull for Server???
    result->InstBase = InstBase;
    delete result->TopDistribution;
    result->TopDistribution =
      result->InstBase->sum_distributions( true );
    result->tree_size = 0;
    return result;
  }
  
  TRIBL2_InstanceBase *TRIBL2_InstanceBase::Clone(){
    TRIBL2_InstanceBase *result =
      new TRIBL2_InstanceBase( Depth, Random, PersistentDistributions );
    result->DefAss = DefAss;
    result->DefaultsValid = DefaultsValid;
    result->NumOfTails = NumOfTails; // only usefull for Server???
    result->InstBase = InstBase;
    delete result->TopDistribution;
    result->TopDistribution =
      result->InstBase->sum_distributions( true );
    result->tree_size = 0;
    return result;
  }
  
  IB_InstanceBase* TRIBL_InstanceBase::IBPartition( IBtree *sub ){
    int i=0;
    IBtree *tmp = sub;
    while ( tmp && tmp->link ){
      i++;
      tmp = tmp->link;
    }
    IB_InstanceBase *result =
      new IB_InstanceBase( i, Random );
    result->DefAss = DefAss;
    result->DefaultsValid = DefaultsValid;
    result->NumOfTails = NumOfTails; // only usefull for Server???
    result->InstBase = sub;
    if ( sub ){
      delete result->TopDistribution;
      result->TopDistribution =
	result->InstBase->sum_distributions( false  );
    }
    result->tree_size = 0;
    return result;
  }

  IB_InstanceBase* TRIBL2_InstanceBase::IBPartition( IBtree *sub ){
    int i=0;
    IBtree *tmp = sub;
    while ( tmp && tmp->link ){
      i++;
      tmp = tmp->link;
    }
    IB_InstanceBase *result =
      new IB_InstanceBase( i, Random );
    result->DefAss = DefAss;
    result->DefaultsValid = DefaultsValid;
    result->NumOfTails = NumOfTails; // only usefull for Server???
    result->InstBase = sub;
    if ( sub ){
      delete result->TopDistribution;
      result->TopDistribution =
	result->InstBase->sum_distributions( false  );
    }
    result->tree_size = 0;
    return result;
  }

  void InstanceBase_base::CleanPartition(){
    InstBase = NULL; // prevent deletion of InstBase in next step!
    delete this;
  }
  
  void InstanceBase_base::AssignDefaults(){
    time_t StartTime;
    time(&StartTime);
    if ( !DefaultsValid ){
      if ( !DefAss ){
	InstBase->assign_defaults( Random,
				   PersistentDistributions,
				   Depth,
				   StartTime );
      }
      else {
	InstBase->re_assign_defaults( Random, PersistentDistributions,
				      StartTime );
      }
      ValueDistribution *Top 
	= InstBase->sum_distributions( PersistentDistributions );
      delete Top; // still a bit silly but the Top Distribution is known
    }
    DefAss = true;
    DefaultsValid = true;
  }

  void TRIBL_InstanceBase::AssignDefaults( int treshold ){
    if ( Treshold < 0 || Treshold != treshold ){
      Treshold = treshold;
      DefaultsValid = false;
    }
    if ( !DefaultsValid ){
      time_t ST;
      time( &ST );
      InstBase->assign_defaults( Random, PersistentDistributions, Treshold, ST );
    }
    DefAss = true;
    DefaultsValid = true;
  }

  void InstanceBase_base::Prune( const TargetValue * ){
    FatalError( "You Cannot Prune this kind of tree! " );
  }
  
  void IG_InstanceBase::Prune( const TargetValue *top ){
    AssignDefaults( );
    if ( !Pruned ) {
      unsigned long int cnt = IB_COUNT;
      time_t StartTime;
      time(&StartTime);
      InstBase = InstBase->Reduce( top, StartTime );
      tree_size = tree_size + IB_COUNT - cnt;
      Pruned = true;
    }
  }  
  
  bool InstanceBase_base::AddInstance( const Instance *Inst ){
    // add one instance to the IB
    unsigned int i;
    IBtree *hlp, **pnt = &InstBase;
    unsigned long int cnt = IB_COUNT;
    if ( !InstBase ){
      for ( i=0; i< Depth; i++ ){
	*pnt = new IBtree();
	(*pnt)->FValue = Inst->FV[i];
	pnt = &((*pnt)->link);
      }
    }
    else
      for ( i=0; i< Depth; i++ ){
	hlp = (*pnt)->add_feat_val( Inst->FV[i], pnt );
	pnt = &(hlp->link);
      }
    if ( *pnt == NULL ){
      *pnt = new IBtree();
      (*pnt)->TDistribution = new ValueDistribution;
      NumOfTails++;
    }
    bool sw_conflict = false;
    if ( Inst->ExemplarWeight() > 0 ){
      sw_conflict = (*pnt)->TDistribution->IncFreq( Inst->TV, 
						    Inst->ExemplarWeight() );
    }
    else
      (*pnt)->TDistribution->IncFreq(Inst->TV);
    TopDistribution->IncFreq(Inst->TV);
    tree_size = tree_size + IB_COUNT - cnt;
    DefaultsValid = false;
    return !sw_conflict;
  }

  bool InstanceBase_base::Merge( InstanceBase_base *ib, FeatureValue *fv ){
    if ( ib->InstBase ){
      if ( InstBase ){
	IBtree **pnt = &InstBase;
	while ( *pnt && (*pnt)->FValue->Index() < fv->Index() ){
	  pnt = &(*pnt)->next;
	}
	if ( *pnt ){
	  if ( (*pnt)->FValue->Index() == fv->Index() ){
	    return false;
	  }
	  cerr << "insert voor " << (*pnt)->next->FValue->Name() << endl;
	  ib->InstBase->next = (*pnt)->next;
	}
	*pnt = ib->InstBase;
      }
      else {
	InstBase = ib->InstBase;
      }
    }
    tree_size += ib->tree_size;
    NumOfTails += ib->NumOfTails;
    TopDistribution->Merge( *ib->TopDistribution );
    ib->InstBase = 0;
    return true;
  }
  
  bool IG_InstanceBase::Merge( InstanceBase_base *ib, FeatureValue *fv ){
    if ( ib->InstBase ){
      if ( InstBase ){
	IBtree **pnt = &InstBase;
	while ( *pnt && (*pnt)->FValue->Index() < fv->Index() ){
	  pnt = &(*pnt)->next;
	}
	if ( *pnt ){
	  if ( (*pnt)->FValue->Index() == fv->Index() ){
	    return false;
	  }
	  cerr << "insert voor " << (*pnt)->next->FValue->Name() << endl;
	  ib->InstBase->next = (*pnt)->next;
	}
	*pnt = ib->InstBase;
      }
      else {
	InstBase = ib->InstBase;
      }
    }
    tree_size += ib->tree_size;
    NumOfTails += ib->NumOfTails;
    TopDistribution->Merge( *ib->TopDistribution );
    Pruned = true;
    DefaultsValid = true;
    DefAss = true;
    ib->InstBase = 0;
    return true;
  }

  void InstanceBase_base::RemoveInstance( const Instance *Inst ){
    // remove an instance from the IB
    int pos = 0;
    IBtree *pnt = InstBase;
    while ( pnt ){
      if ( pnt->link == NULL ){
	pnt->TDistribution->DecFreq(Inst->TV);
	TopDistribution->DecFreq(Inst->TV);
	break;
      }
      else {
	if ( pnt->FValue == Inst->FV[pos] ){
	  pnt = pnt->link;
	  pos++;
	}
	else
	  pnt = pnt->next;
      }
    }
    DefaultsValid = false;
  }
  
  ValueDistribution *InstanceBase_base::InitGraphTest( FeatureValue **P,
						       FeatureValue **T ){
    (void)P; (void)T;
    FatalError( "InitGraphTest" );
    return NULL;
  }

  ValueDistribution *IB_InstanceBase::InitGraphTest( FeatureValue **Path,
						     FeatureValue **Templ ){
    IBtree *pnt;
    ValueDistribution *result = NULL;
    pnt = InstBase;
    unsigned int i;
    for ( i = 0; i < Depth; i++ ){
      InstPath[i] = pnt;
      RestartSearch[i] = pnt;
      while ( pnt && pnt->FValue != Templ[i] ) //search for  exact match
	pnt = pnt->next;
      if ( pnt ){ // found an exact match, so mark restart position
	if ( RestartSearch[i] == pnt )
	  RestartSearch[i] = pnt->next;
	SkipSearch[i] = pnt;
	InstPath[i] = pnt;
      }
      else { // no exact match at this level. Just start with the first....
	RestartSearch[i] = NULL;
	SkipSearch[i] = NULL;
	pnt = InstPath[i];
      }
      Path[i] = pnt->FValue;
      pnt = pnt->link;
      if ( pnt && pnt->link == NULL ){
	result = pnt->TDistribution;
	break;
      }
    }
    return result;
  }
  
  ValueDistribution *InstanceBase_base::NextGraphTest( FeatureValue **P, 
						       FeatureValue **T,
						       int &p ){
    (void)P; (void)T; (void)p;
    FatalError( "NextGraphTest" );
    return NULL;
  }

  ValueDistribution *IB_InstanceBase::NextGraphTest( FeatureValue **Path, 
						     FeatureValue **Templ,
						     int &pos ){
    IBtree *hlp = NULL, *pnt;
    ValueDistribution *result = NULL;
    while ( !hlp && pos >=0 ){
      if ( RestartSearch[pos] == NULL ) {
	// No exact match here, so no real problems
	hlp = InstPath[pos]->next;
      }
      else {
	hlp = RestartSearch[pos];
	RestartSearch[pos] = NULL;
      }
      if ( hlp && hlp == SkipSearch[pos] ){
	hlp = hlp->next;
      }
      if ( !hlp ) {
	pos--;
      }
    }
    if ( pos >= 0 && hlp ) {
      InstPath[pos] = hlp;
      Path[pos] = hlp->FValue;
      pnt = hlp->link;
      unsigned int j;
      for ( j=pos+1; j < Depth; j++ ){
	hlp = pnt;
	while ( pnt && pnt->FValue!= Templ[j] ) // search exact match
	  pnt = pnt->next;
	if ( pnt ){ // we found an exact match, so mark Restart position
	  if ( pnt == hlp )
	    RestartSearch[j] = hlp->next;
	  else
	    RestartSearch[j] = hlp;
	  SkipSearch[j] = pnt;
	}
	else { // no exact match at this level. Just start with the first....
	  RestartSearch[j] = NULL;
	  SkipSearch[j] = NULL;
	  pnt = hlp;
	}
	InstPath[j] = pnt;
	Path[j] = pnt->FValue;
	pnt = pnt->link;
      }
      if ( pnt )
	result = pnt->TDistribution;
    }
    return result;
  }
  
  ValueDistribution *InstanceBase_base::IG_test( const Instance *I, 
						 int &l,
						 const TargetValue *&r ){
    (void)I; (void)l; (void)r;
    FatalError( "IG_test " );
    return NULL;
  }

  ValueDistribution *IG_InstanceBase::IG_test( const Instance *Inst, 
					       int &end_level,
					       const TargetValue *&result ){
    // The Test function for the IG algorithm, returns a pointer to the
    // distribution of the last matching position in the Tree, it's position
    // in the Instance Base and the default TargetValue
    IBtree *pnt = InstBase;
    result = NULL;
    ValueDistribution *Dist = NULL;
    int pos = 0;
    while ( pnt ){
      if ( pnt->FValue == Inst->FV[pos] ){
	result = pnt->TValue;
	Dist = pnt->TDistribution;
	pnt = pnt->link;
	pos++;
	if ( pnt && !pnt->FValue ) 
	  pnt = NULL;
      }
      else
	pnt = pnt->next;
    }
    end_level = pos;
    if ( end_level == 0 ){
      Dist = TopDistribution;
    }
    return Dist;
  }

  IB_InstanceBase *InstanceBase_base::TRIBL_test( const Instance *, int,
						  const TargetValue *&,
						  const ValueDistribution *&,
						  int & ){
    FatalError( "TRIBL_test " );
    return NULL;
  }

  IB_InstanceBase *InstanceBase_base::TRIBL2_test( const Instance *, 
						   const ValueDistribution *&,
						   int & ){
    FatalError( "TRIBL2_test " );
    return NULL;
  }

  IB_InstanceBase *TRIBL_InstanceBase::TRIBL_test( const Instance *Inst, 
						   int treshold,
						   const TargetValue *&TV,
						   const ValueDistribution *&dist,
						   int &level ){
    // The Test function for the TRIBL algorithm, returns a pointer to the
    // Target at the last matching position in the Tree, 
    // or the subtree Instance Base necessary for IB1
    IBtree *pnt = InstBase;
    AssignDefaults( treshold );
    TV = NULL;
    dist = NULL;
    IB_InstanceBase *subt = NULL;
    int pos = 0;
    while ( pnt && pos < treshold ){
      if ( pnt->FValue == Inst->FV[pos] ){
	dist = pnt->TDistribution;
	TV = pnt->TValue;
	pnt = pnt->link;
	if ( pnt && !pnt->FValue ){
	  dist = pnt->TDistribution;
	  pnt = NULL;
	}
	pos++;
      }
      else
	pnt = pnt->next;
    }
    if ( pos == treshold ){
      if ( pnt ){
	subt = IBPartition( pnt );
	dist = NULL;
      }
      else {
	level = pos;
      }
    }      
    else {
      if ( pos == 0 && dist == NULL ){
	dist = TopDistribution;
	TV = TopTarget();
      }
      else
	level = pos;
    }
    return subt;
  }
  
  IB_InstanceBase *TRIBL2_InstanceBase::TRIBL2_test( const Instance *Inst, 
						     const ValueDistribution *& dist,
						     int &level ){
    // The Test function for the TRIBL2 algorithm, returns a pointer to the
    // the subtree Instance Base necessary for IB1
    IBtree *pnt = InstBase;
    dist = NULL;
    AssignDefaults();
    int pos = 0;
    IB_InstanceBase *subtree = NULL;
    IBtree *last_match = pnt;
    while ( pnt ){
      if ( pnt->FValue == Inst->FV[pos] ){
	// a match, go deeper
	pnt = pnt->link;
	last_match = pnt;
	pos++;
	if ( pnt && !pnt->FValue ){
	  // at the end, an exact match
	  dist = pnt->TDistribution;
	  last_match = NULL;
	  break;
	}
      }
      else
	pnt = pnt->next;
    }
    if ( last_match ){
      subtree = IBPartition( last_match );
      level = pos;
    }
    return subtree;
  }
  
} // namespace Timbl
