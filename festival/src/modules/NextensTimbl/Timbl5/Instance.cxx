/*
 * Instance.cc
 *
 *    The classes for Instances, Features, Targets and some helpers
 *    like FeatureValues, TargetValues and ValueDistributions
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
#include <vector>
#include <set>
#include <string>
#include <iostream>
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
#include <ctype.h>
#include <string.h>
#include <float.h>
#include <assert.h>
#else
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <cfloat>
#include <cassert>
#endif

#include "Common.h"
#include "MsgClass.h"
#include "Tree.h"
#include "Types.h"
#include "Instance.h"

#ifdef PTHREADS
#include <pthread.h>
#endif

using namespace std;

namespace Timbl {
  using namespace Common;

  ostream& operator<<(ostream& s, const FeatureValue* fv ){ 
    if ( fv )
      s << fv->Name();
    else
      s << "*FV-NF*";
    return s;
  }
  
  ostream& operator<<(ostream& s, const TargetValue* tv ){ 
    if ( tv )
      s << tv->Name();
    else
      s << "*TV-NF*";
    return s;
  }
  
  int Vfield::Index() { return value->Index(); }
  
  ostream& operator<<(ostream& os, const Vfield *vd ) {
    return vd->put( os );
  }
  
  ostream& VFfield::put( ostream& os ) const {
    os << value << " " << frequency;
    return os;
  }
  
  ostream& WVfield::put( ostream& os ) const {
    os.setf(ios::showpoint);
    os << value << " " << weight;
    return os;
  }
  
  ostream& WVFfield::put( ostream& os ) const {
    if ( frequency > 0 ){
      os.setf(ios::showpoint);
      os << value << " " << frequency << " " << weight;
    }
    return os;
  }
  
  ostream& VFfield::put_hashed( ostream& os ) const {
    if ( frequency > 0 )
      os << value->Hash() << " " << frequency;
    return os;
  }
  
  ostream& WVfield::put_hashed( ostream& os ) const {
    os << value->Hash() << " " << weight;
    return os;
  }

  ostream& WVFfield::put_hashed( ostream& os ) const {
    if ( frequency > 0 )
      os << value->Hash() << " " << frequency << " " << weight;
    return os;
  }
  
  inline int random_number( int Min, int Max ){
    // calculate a random integer within the interval [min,max]
    if ( Min == Max )
      return Min;
    double randnum = (double)rand()/(double)RAND_MAX;
    randnum *= (Max-Min);
    randnum += Min;
    return (int)floor(randnum+0.5);
  }  

  void ValueDistribution::Clear(){ 
    VDlist::iterator it;
    for ( it = distribution.begin(); it != distribution.end(); ++it )
      delete (*it).second;
    distribution.clear(); 
    total_items = 0;
  }
  
  void ValueDistribution::DistToString( string& DistStr ) const {
#if __GNUC__ < 3
    ostrstream oss;
#else
    ostringstream oss;
#endif
    VDlist::const_iterator it = distribution.begin();
    oss << "{ ";
    while ( it != distribution.end() ){
      oss << (*it).second;
      ++it;
      if ( it != distribution.end() )
	oss << ", ";
    }
    oss << " }";
#if __GNUC__ < 3
    oss << ends;
    char *buf = oss.str();
    DistStr = buf;
    delete [] buf;
#else
    DistStr = oss.str();
#endif
  }

  const string ValueDistribution::DistToString() const {
    string result;
    DistToString( result );
    return result;
  }
  
  double ValueDistribution::Entropy() const {
    double Prob = 0.0;
    double entropy = 0.0;
    double TotalVals = total_items;
    if ( TotalVals > 0 ){
      VDlist::const_iterator it = distribution.begin();
      // Loop over the classes in the distibution
      while ( it != distribution.end() ){
	double Freq = (*it).second->Freq();
	if ( Freq > 0 ){
	  Prob = Freq / TotalVals;
	  entropy += Prob * Log2(Prob);
	}
	++it;
      }
    }
    return fabs(entropy);
  }

  WValueDistribution *WValueDistribution::clone( ) const {
    WValueDistribution *result = create();
    VDlist::const_iterator it = distribution.begin();
    Vfield *tmp;
    while ( it != distribution.end() ){
      tmp = ((*it).second)->clone();
      result->distribution[tmp->Index()] = tmp;
      ++it;
    }
    result->total_items =  total_items;
    return result;
  }
  
  void WValueDistribution::MergeWeight( const ValueDistribution& VD, 
					double Weight ){
    int key;
    Vfield *vdf;
    VDlist::const_iterator It = VD.distribution.begin();
    while ( It != VD.distribution.end() ){
      key = (*It).first;
      vdf = (*It).second;
      if ( distribution.find( key ) != distribution.end() ){
	distribution[key]->Weight( distribution[key]->Weight() + vdf->Freq() * Weight );
      }
      else
	distribution[key] = new  WVFfield( vdf->Value(), 
					   1,
					   vdf->Freq() * Weight );
      total_items += vdf->Freq();
      ++It;
    }
  }  

  void ClassDistribution::Normalize() {
    double sum = 0.0;
    VDlist::iterator it = distribution.begin();
    while ( it != distribution.end() ){
      sum += (*it).second->Weight();
      ++it;
    }
    it = distribution.begin();
    while ( it != distribution.end() ){
      (*it).second->Weight( (*it).second->Weight() / sum );
      ++it;
    }
  }
  
  void ClassDistribution::Normalize_1( double factor, Target *targ ) {
    //    cerr << "voor " << this << endl;
    for ( int i = 0; i < targ->ArraySize(); ++i ){
      // search for val, if not there: add entry with frequency factor;
      // otherwise increment the freqency and ExamplarWeight
      // HACK
      TargetValue *val = targ->Value(i);
      VDlist::const_iterator it = distribution.find( val->Index() );
      if ( it != distribution.end() ){
	(*it).second->Weight( (*it).second->Weight() + factor );
      }  
      else {
	distribution[val->Index()] = new WVfield( val, factor );
      }
      total_items += 1;
    }
    Normalize();
  }

  void ClassDistribution::VDCopy( const ValueDistribution& VD ){
    Clear();
    int key;
    Vfield *vdf;
    VDlist::const_iterator It = VD.distribution.begin();
    while ( It != VD.distribution.end() ){
      key = (*It).first;
      vdf = (*It).second;
      distribution[key] = new  WVfield( vdf->Value(), 
					vdf->Weight() );
      total_items += vdf->Freq();
      ++It;
    }
  }

  void ClassDistribution::SpecialVDCopy( const ValueDistribution& VD ){
    Clear();
    int key;
    Vfield *vdf;
    VDlist::const_iterator It = VD.distribution.begin();
    while ( It != VD.distribution.end() ){
      key = (*It).first;
      vdf = (*It).second;
      distribution[key] = new WVfield( vdf->Value(), 
				       vdf->Freq() );
      total_items += vdf->Freq();
      ++It;
    }
  }
  
  string ValueDistribution::Hashed() const{
#if __GNUC__ < 3
    ostrstream oss;
#else
    ostringstream oss;
#endif
    VDlist::const_iterator it = distribution.begin();
    oss << "{ ";
    while ( oss.good() && it != distribution.end() ){
      (*it).second->put_hashed( oss );
      ++it;
      if ( it != distribution.end() )
	oss << ", ";
    }
    oss << " }";
#if __GNUC__ < 3
    oss << ends;
    char *buf = oss.str();
    string result = buf;
    delete [] buf;
#else
    string result = oss.str();
#endif
    return result;
  }

  void ValueDistribution::SetFreq( const TargetValue *val, const int freq,
				   double sw ){
    // search for val, if not there: add entry with frequency freq;
    // otherwise set the freqency
    // also sets the sample_weight
    WVFfield *temp = new  WVFfield( val, freq, sw );
    if ( distribution.find( val->Index() ) != distribution.end() ){
      delete distribution[val->Index()];
    }
    distribution[val->Index()] = temp;
    total_items += freq;
  }
  
  void ValueDistribution::SetFreq( const TargetValue *val, const int freq ){
    // search for val, if not there: add entry with frequency freq;
    // otherwise set the freqency
    VFfield *temp = new VFfield( val, freq );
    if ( distribution.find( val->Index() ) != distribution.end() ){
      delete distribution[val->Index()];
    }
    distribution[val->Index()] = temp;
    total_items += freq;
  }
  
  void ValueDistribution::IncFreq( const TargetValue *val ){
    // search for val, if not there: add entry with frequency 1;
    // otherwise increment the freqency
    if ( distribution.find( val->Index() ) != distribution.end() ){
      distribution[val->Index()]->Freq( distribution[val->Index()]->Freq()+1);
    }
    else
      distribution[val->Index()] = new VFfield( val, 1 );
    total_items += 1;
  }
  
  bool ValueDistribution::IncFreq( const TargetValue *val, double sw ){
    // search for val, if not there: add entry with frequency 1;
    // otherwise increment the freqency
    VDlist::const_iterator it = distribution.find( val->Index() );
    if ( it != distribution.end() ){
      (*it).second->Freq( (*it).second->Freq() + 1 );
    }  
    else {
      distribution[val->Index()] = new WVFfield( val, 1, sw );
    }
    total_items += 1;
    return distribution[val->Index()]->Weight() != sw;
  }
  
  void ValueDistribution::DecFreq( const TargetValue *val ){
    // search for val, if not there, just forget
    // otherwise decrement the freqency
    VDlist::const_iterator it = distribution.find( val->Index() );
    if ( it != distribution.end() ){
      (*it).second->Freq( (*it).second->Freq() - 1 );
      total_items -= 1;
    }  
  }
  
  int ValueDistribution::SumFrequencies( ) const{
    // calculate the total number of items in the distribution
    VDlist::const_iterator it = distribution.begin();
    int result = 0;
    while ( it != distribution.end() ){
      result += (*it).second->Freq();
      ++it;
    }
    return result;
  }
  
  bool ValueDistribution::ZeroDist() const {
    if ( distribution.size() == 1 ){
      VDlist::const_iterator it = distribution.begin();
      return (*it).second->Freq() == 0;
    }
    else
      return false;
  }

  ValueDistribution *ValueDistribution::clone( ) const {
    ValueDistribution *result = create();
    VDlist::const_iterator it = distribution.begin();
    Vfield *tmp;
    while ( it != distribution.end() ){
      tmp = ((*it).second)->clone();
      result->distribution[tmp->Index()] = tmp;
      ++it;
    }
    result->total_items =  total_items;
    return result;
  }
  
  void ValueDistribution::Merge( const ValueDistribution& VD ){
    VDlist::const_iterator It = VD.distribution.begin();
    int key;
    Vfield *vd;
    while ( It != VD.distribution.end() ){
      key = (*It).first;
      vd = (*It).second;
      if ( distribution.find(key) != distribution.end() ){
	distribution[key]->Freq( distribution[key]->Freq() + vd->Freq() );
      }
      else
	distribution[key] = vd->clone();
      ++It;
    }
    total_items += VD.total_items;
  }

  const TargetValue *ValueDistribution::BestTarget( bool& tie,
						    bool do_rand ) const {
    // get the most frequent target from the distribution.
    // In case of a tie take the one which is GLOBALLY the most frequent, 
    // and signal if this ties also!
    // OR (if do_rand) take random one of the most frequents
    const TargetValue *best = NULL;
    tie = false;
    VDlist::const_iterator It = distribution.begin();
    if ( It != distribution.end() ){
      Vfield *pnt = (*It).second;
      int Max = pnt->Freq();
      if ( do_rand ){
	int nof_best=1, pick=1;
	++It;
	while ( It != distribution.end() ){
	  pnt = (*It).second;
	  if ( pnt->Freq() > Max ){
	    Max = pnt->Freq();
	    nof_best = 1;
	  }
	  else
	    if ( pnt->Freq() == Max )
	      nof_best++;
	  ++It;
	}
	tie = nof_best > 1;
	pick = random_number( 1, nof_best );
	It = distribution.begin();
	nof_best = 0;
	while ( It != distribution.end() ){
	  pnt = (*It).second;
	  if ( pnt->Freq() == Max )
	    if ( ++nof_best == pick ){
	      return pnt->Value();
	    }
	  ++It;
	}
	return NULL;
      }
      else {
	best = pnt->Value();
	++It;
	while ( It != distribution.end() ){
	  pnt = (*It).second;
	  if ( pnt->Freq() > Max ){
	    tie = false;
	    best = pnt->Value();
	    Max = pnt->Freq();
	  }
	  else
	    if ( pnt->Freq() == Max ) {
	      tie = true;
	      if ( pnt->Value()->ValFreq() > best->ValFreq() ){
		best = pnt->Value();
	      }
	    }
	  ++It;
	}
	return best;
      }
    }
    return best;
  }
  
  const TargetValue *WValueDistribution::BestTarget( bool& tie,
						     bool do_rand ) const {
    // get the most frequent target from the distribution.
    // In case of a tie take the one which is GLOBALLY the most frequent, 
    // and signal if this ties also!
    // OR (if do_rand) take random one of the most frequents
    const TargetValue *best = NULL;
    VDlist::const_iterator It = distribution.begin();
    tie = false;
    if ( It != distribution.end() ){
      double Max = (*It).second->Weight();
      if ( do_rand ){
	int nof_best=1, pick=1;
	++It;
	while ( It != distribution.end() ){
	  if ( (*It).second->Weight() > Max ){
	    Max = (*It).second->Weight();
	    nof_best = 1;
	  }
	  else
	    if ( abs((*It).second->Weight()- Max) < Epsilon )
	      nof_best++;
	  ++It;
	}
	tie = nof_best > 1;
	tie = nof_best > 1;
	pick = random_number( 1, nof_best );
	It = distribution.begin();
	nof_best = 0;
	while ( It != distribution.end() ){
	  if ( abs((*It).second->Weight() - Max) < Epsilon )
	    if ( ++nof_best == pick ){
	      return (*It).second->Value();
	    }
	  ++It;
	}
	return NULL;
      }
      else { 
	best = (*It).second->Value();
	++It;
	while ( It != distribution.end() ){
	  if ( (*It).second->Weight() > Max ){
	    tie = false;
	    best = (*It).second->Value();
	    Max = (*It).second->Weight();
	  }
	  else 
	    if ( abs((*It).second->Weight() - Max) < Epsilon ) {
	      tie = true;
	      if ( (*It).second->Value()->ValFreq() > best->ValFreq() ){
		best = (*It).second->Value();
	      }
	    }
	  ++It;
	}
	return best;
      }
    }
    return best;
  }

  const TargetValue *ValueDistribution::BestTarget() const {
    // get a target from the distribution, using probabilistics:
    //
    const TargetValue *best = NULL;
    VDlist::const_iterator It = distribution.begin();
    if ( It != distribution.end() ){
      int Max = (*It).second->Freq();
      ++It;
      while ( It != distribution.end() ){
	Max += (*It).second->Freq();
	++It;
      }
      int pick = random_number( 1, Max );
      It = distribution.begin();
      while ( It !=distribution.end()  ){
	pick -= (*It).second->Freq();
	if ( pick <= 0 )
	  return (*It).second->Value();
	++It;
      }
    }
    return best;
  }
  
  Feature::Feature( int a, int b, StringHash *T ): 
    BaseFeatTargClass(a,b,T),
    metric( UnknownMetric ),
    ignore( false ),
    numeric( false ),
    vcpb_read( false ),
    PrestoreStatus(ps_undef),
    Prestored_metric( UnknownMetric ),
    entropy( 0.0 ),
    info_gain (0.0),
    split_info(0.0),
    gain_ratio(0.0),
    chi_square(0.0),
    shared_variance(0.0),
    matrix_clip_freq(10),
    n_dot_j(NULL),
    n_i_dot(NULL),
    n_min (0.0),
    n_max (0.0),
    SaveSize(0),
    SaveNum(0),
    weight(0.0)
  {}
  
  void Feature::InitSparseArrays(){
    // Loop over all values.
    //
    for ( int i=0; i < ArraySize(); i++ ){
      FeatureValue *FV = Value(i);
      double freq = FV->ValFreq();
      FV->ValueClassProb->Clear();
      if ( freq > 0 ){
	// Loop over all present classes.
	//
	ValueDistribution::dist_iterator It = FV->TargetDist.Begin();
	while ( It != FV->TargetDist.End() ){
	  FV->ValueClassProb->Assign( (*It).second->Index(), 
				      (*It).second->Freq()/freq );
	  ++It;
	}
      }
    } // i
  }

  struct D_D {
    D_D(){ dist = NULL; value = 0.0; };
    D_D( FeatureValue *fv ){
      string2double( fv->Name(), value );
      dist = &fv->TargetDist;
    }
    ValueDistribution *dist;
    double value;
  };

  bool dd_less( const D_D* dd1, const D_D* dd2 ){
    return dd1->value < dd2->value;
  }
  
  void Feature::NumStatistics( vector<FeatureValue *>& FVBin,
			       double DBentropy,
			       int BinSize ){
    double Prob, FVEntropy;
    int Freq;
    unsigned int TotalVals = TotalValues();
    entropy = 0.0;
    vector<D_D*> ddv;
    ddv.reserve(ArraySize());
    for ( int i=0; i < ArraySize(); ++i ){
      if ( Value(i)->ValFreq() > 0 ){
	ddv.push_back( new D_D( Value(i) ) );
      }
    }
    sort( ddv.begin(), ddv.end(), dd_less );
    unsigned int dd_len = ddv.size();
    int num_per_bin = (int)floor( (double)dd_len / BinSize);
    int rest = dd_len - num_per_bin * BinSize;
    if ( rest )
      num_per_bin++;
    int j = 0;
    int cnt = 0;
    unsigned int m;
    for ( m = 0; m < dd_len; ++m ){
      FVBin[j]->TargetDist.Merge( *ddv[m]->dist );
      if ( ++cnt >= num_per_bin ){
	j++;
	if ( --rest == 0 )
	  --num_per_bin;
	cnt = 0;
      }
    }
    for ( unsigned int jj=0; jj < dd_len; ++jj ){
      delete ddv[jj];
    }
    int k;
    for ( k=0; k < BinSize; k++ ){
      FeatureValue *pnt = FVBin[k];
      Freq = pnt->TargetDist.SumFrequencies();
      pnt->ValFreq( Freq );
      if ( Freq > 0 ){
	// Entropy for this FV pair.
	//
	FVEntropy = 0.0;
	ValueDistribution::dist_iterator It = pnt->TargetDist.Begin();
	while ( It !=  pnt->TargetDist.End() ){
	  Prob = (*It).second->Freq()/(double)Freq;
	  FVEntropy += Prob * Log2(Prob);
	  ++It;
	}
	entropy += -FVEntropy * Freq / (double)TotalVals;
      }
    }
    entropy = fabs( entropy );
    // Info gain.
    //
    info_gain = DBentropy - entropy;
    
    // And the split info.
    //
    split_info = 0.0;
    for ( int l=0; l < BinSize; ++l ){
      Freq = FVBin[l]->ValFreq();
      if ( Freq > 0 ){
	Prob = Freq / (double)TotalVals;
	split_info += Prob * Log2(Prob);
      } 
    }
    split_info = -split_info;
    // Gain ratio.
    //
    if ( split_info == 0.0 ){
      gain_ratio = 0.0;
      info_gain  = 0.0;
      entropy = DBentropy;
    }
    else
      gain_ratio = info_gain / split_info;
  }
 
  void Feature::Statistics( double DBentropy, Target *Targets, bool full ){
    Statistics( DBentropy );
    if ( full ){
      ChiSquareStatistics( ArraySize(), Targets->ArraySize() );
      SharedVarianceStatistics( Targets, EffectiveValues() );
    }
  }
  
  void Feature::NumStatistics( double DBentropy, Target *Targets,
			       int BinSize, bool full ){
    char dumname[80];
    vector<FeatureValue *> FVBin(BinSize);;
    for ( int i=0; i < BinSize; ++i ){
      sprintf( dumname, "dum%d", i );
      FVBin[i] = new FeatureValue( dumname );
    }
    NumStatistics( FVBin, DBentropy, BinSize );
    if ( full ){
      ChiSquareStatistics( FVBin, BinSize, Targets->ArraySize() );
      int cnt = 0;   // count effective values in Bin 
      for( int i=0; i < BinSize; ++i ){
	if ( FVBin[i]->ValFreq() > 0 )
	  cnt++;
      }
      SharedVarianceStatistics( Targets, cnt );
    }
    for( int i=0; i < BinSize; ++i ){
      delete FVBin[i];
    }
  }

  void Feature::Statistics( double DBentropy ){
    double Prob = 0.0;
    unsigned int TotalVals = TotalValues();
    entropy = 0.0;
    // Loop over the values.
    int i;
    for ( i=0; i < ArraySize(); i++ ){
      FeatureValue *fv = Value(i);
      // Entropy for this FV pair.
      double FVEntropy = 0.0;
      int Freq = fv->ValFreq();
      if ( Freq > 0 ){
	ValueDistribution::dist_iterator It = fv->TargetDist.Begin();
	while ( It != fv->TargetDist.End() ){
	  Prob = (*It).second->Freq() / (double)Freq;
	  FVEntropy += Prob * Log2(Prob);
	  ++It;
	}
	entropy += -FVEntropy * Freq / (double)TotalVals;
      }
    } //end i
    
    entropy = fabs( entropy );
    // Info. gain.
    //
    info_gain = DBentropy - entropy;
    // And the split. info.
    //
    split_info = 0.0;
    int j;
    for ( j=0; j < ArraySize(); j++ ){
      FeatureValue *fv = Value(j);
      Prob = fv->ValFreq() / (double)TotalVals;
      if ( Prob > 0 )
	split_info += Prob * Log2(Prob);
    } 
    split_info = -split_info;
    // Gain ratio.
    //
    if ( split_info == 0.0 )
      gain_ratio = 0.0;
    else
      gain_ratio = info_gain / split_info;
  }

  void Feature::ChiSquareStatistics( vector<FeatureValue *>& FVA, 
				     int Num_Vals,
				     int Size ){
    chi_square = 0.0;
    int start_val = 0;
    long int n_dot_dot = 0;
    double tmp;
    if ( !n_dot_j ) {
      n_dot_j = new long int[Size]; 
      n_i_dot = new long int[Num_Vals]; 
      SaveSize = Size;
      SaveNum = Num_Vals;
    }
    else {
      if ( SaveSize < Size ){
	delete [] n_dot_j;
	n_dot_j = new long int[Size];
	SaveSize = Size;
      }
      if ( SaveNum < Num_Vals ){
	delete [] n_i_dot;
	n_i_dot = new long int[Num_Vals];
	SaveNum = Num_Vals;
      }
    }
    int j;
    for ( j = 0; j < Size; ++j ){
      n_dot_j[j] = 0;
    }
    ValueDistribution::dist_iterator It;
    int i;
    for ( i = start_val; i < Num_Vals; ++i ){
      n_i_dot[i] = 0;
      FeatureValue *fv = FVA[i];
      It = fv->TargetDist.Begin();
      while ( It != fv->TargetDist.End()  ){
	n_dot_j[(*It).second->Index()] += (*It).second->Freq();
	n_i_dot[i] += (*It).second->Freq();
	++It;
      }
      n_dot_dot += n_i_dot[i];
    }
    if ( n_dot_dot != 0 ){
      int m;
      for ( m = start_val; m < Num_Vals; ++m ){
	FeatureValue *fv = FVA[m];
	It = fv->TargetDist.Begin();
	int n = 0;
	while ( It != fv->TargetDist.End() && n < Size ){
	  while ( n < (*It).second->Index() ){
	    tmp = ((double)n_dot_j[n++] * (double)n_i_dot[m]) /
	      (double)n_dot_dot;
	    chi_square += tmp;
	  }
	  if ( n == (*It).second->Index() ){
	    tmp = ((double)n_dot_j[n++] * (double)n_i_dot[m]) /
	      (double)n_dot_dot;
	    if ( tmp ){
	      chi_square += ( (tmp - (*It).second->Freq()) *
			      (tmp - (*It).second->Freq()) ) / tmp;
	    }
	    ++It;
	  }
	  else
	    break;
	}
	while ( n < Size ){
	  tmp = ((double)n_dot_j[n++] * (double)n_i_dot[m]) /
	    (double)n_dot_dot;
	  chi_square += tmp;
	}
      }
    }
  }

  void Feature::ChiSquareStatistics( int Num_Vals, int Size ){
    chi_square = 0.0;
    int start_val = 0;
    long int n_dot_dot = 0;
    double tmp;
    if ( !n_dot_j ) {
      n_dot_j = new long int[Size]; 
      n_i_dot = new long int[Num_Vals]; 
      SaveSize = Size;
      SaveNum = Num_Vals;
    }
    else {
      if ( SaveSize < Size ){
	delete [] n_dot_j;
	n_dot_j = new long int[Size];
	SaveSize = Size;
      }
      if ( SaveNum < Num_Vals ){
	delete [] n_i_dot;
	n_i_dot = new long int[Num_Vals];
	SaveNum = Num_Vals;
      }
    }
    int j;
    for ( j = 0; j < Size; ++j ){
      n_dot_j[j] = 0;
    }
    ValueDistribution::dist_iterator It;
    int i;
    for ( i = start_val; i < Num_Vals; ++i ){
      n_i_dot[i] = 0;
      FeatureValue *fv = Value(i);
      It = fv->TargetDist.Begin();
      while ( It != fv->TargetDist.End()  ){
	long int fr = (*It).second->Freq();
	n_dot_j[(*It).second->Index()] += fr;
	n_i_dot[i] += fr;
	++It;
      }
      n_dot_dot += n_i_dot[i];
    }
    if ( n_dot_dot != 0 ){
      int m;
      for ( m = start_val; m < Num_Vals; ++m ){
	FeatureValue *fv = Value(m);
	It = fv->TargetDist.Begin();
	int n = 0;
	while ( It != fv->TargetDist.End() && n < Size ){
	  int id = (*It).second->Index();
	  long int fr = (*It).second->Freq();
	  while ( n < id ){
	    tmp = ((double)n_dot_j[n++] * (double)n_i_dot[m]) /
	      (double)n_dot_dot;
	    chi_square += tmp;
	  }
	  if ( n == id ){
	    tmp = ((double)n_dot_j[n++] * (double)n_i_dot[m]) /
	      (double)n_dot_dot;
	    if ( tmp ){
	      chi_square += ( (tmp - fr ) * (tmp - fr ) ) / tmp;
	    }
	    ++It;
	  }
	  else
	    break;
	}
	while ( n < Size ){
	  tmp = ((double)n_dot_j[n++] * (double)n_i_dot[m]) /
	    (double)n_dot_dot;
	  chi_square += tmp;
	}
      }
    }
  }

  ostream& operator<<(ostream& os, const ValueDistribution& vd ) {
    string tmp;
    vd.DistToString( tmp );
    os << tmp;
    return os;
  }
  
  ostream& operator<<(ostream& os, const ValueDistribution *vd ) {
    string tmp = "{null}";
    if( vd )
      vd->DistToString( tmp );
    os << tmp;
    return os;
  }
  
  bool ValueDistribution::read_distribution( istream &is, 
					     Target *Targ,
					     bool do_fr ){
    // read a distribution from stream is into Target
    // if do_f we also adjust the value of Frequency of the Target, which is
    // otherwise 1. Special case when reading the TopDistribution.
    //
    bool result = true;
    TargetValue *target;
    string buf;
    char next;
    int freq;
    double sw;
    is >> next;   // skip {
    do {
      is >> ws >> buf;
      is >> freq;
      if ( do_fr ){
	target = Targ->add_value( buf, freq );
      }
      else
	target = Targ->Lookup( buf );
      if ( !target ){
	result = false;
	break;
      }
      next = look_ahead(is);
      if ( next == ',' ){
	is >> next;
	SetFreq( target, freq );
	next = look_ahead(is);
      }
      else if ( next == '}' ){
	SetFreq( target, freq );
      }
      else if ( isdigit(next) ){
	is >> sw;
	SetFreq( target, freq, sw );
	next = look_ahead(is);
	if ( next == ',' ){
	  is >> next;
	  next = look_ahead(is);
	}
      }
    } while ( next != '}' );
    is >> ws >> next;   // skip }
    total_items = SumFrequencies();
    return result;
  }
  
  
  bool ValueDistribution::read_distribution_hashed( istream &is, 
						    Target *Targ,
						    bool do_fr ){
    // read a distribution from stream is into Target
    // if do_f we also adjust the value of Frequency of the Target, which is
    // otherwise 1. Special case when reading the TopDistribution.
    //
    bool result = true;
    TargetValue *target;
    char next;
    int freq;
    int index;
    double sw;
    is >> next;   // skip {
    if ( next != '{' ){
      cerr << "missing '{' " << endl;
      return false;
    }
    do {
      is >> index;
      is >> freq;
      if ( do_fr ){
	target = Targ->add_value( index, freq );
      }
      else
	target = Targ->ReverseLookup( index );
      if ( !target ){
	result = false;
	break;
      }
      next = look_ahead(is);
      if ( next == ',' ){
	is >> next;
	SetFreq( target, freq );
	next = look_ahead(is);
      }
      else if ( next == '}' ){
	SetFreq( target, freq );
      }
      else if ( isdigit(next) ){
	is >> sw;
	SetFreq( target, freq, sw );
	next = look_ahead(is);
	if ( next == ',' ){
	  is >> next;
	  next = look_ahead(is);
	}
      }
    } while ( next != '}' );
    is >> next;   // skip }
    return result;
  }
  
  FeatureValue::FeatureValue( const std::string& value,
			      int value_hash, int I ):
    ValueClass( value, value_hash, I ), ValueClassProb( NULL ) {
  }
  
  FeatureValue::FeatureValue( const string& s ):
    ValueClass( s, -1 , -1 ),
    ValueClassProb(NULL){ Frequency = 0; 
  }
  
  FeatureValue::~FeatureValue( ){
    delete ValueClassProb;
  }
  
  TargetValue::TargetValue( const std::string& value,
			    int value_hash, int I ):
    ValueClass( value, value_hash, I ){}
  
  int BaseFeatTargClass::localmapping( int ID ){
    int index=0;
    IImaptype::const_iterator map_iterator;
    if ((map_iterator = Mapping.find( ID )) == Mapping.end()){
      // Token was not found in map, so we insert a new copy of it here
      //
      Mapping.insert( IImaptype::value_type( ID, array_size ));
      index = array_size++;
    }
    else{
      index = (*map_iterator).second;
    }
    return index;
  }
  
  int BaseFeatTargClass::EffectiveValues() {
    int result = 0;
    for ( int i=0; i < array_size; ++i )
      if ( ValuesArrayIntern[i]->ValFreq() > 0 )
	++result;
    return result;
  }
  
  unsigned int BaseFeatTargClass::TotalValues() const {
    unsigned int result = 0;
    for ( int i=0; i < array_size; ++i ){
      result += ValuesArrayIntern[i]->ValFreq();
    }
    return result;
  }
  
  FeatureValue *Feature::add_value( const string& valstr, 
				    TargetValue *tv ){
    int hash_val = TokenTree->Hash( valstr );
    return add_value( hash_val, tv );
  }
  
  FeatureValue *Feature::add_value( int HashIndex, 
				    TargetValue *tv ){
    const string& value = TokenTree->ReverseLookup( HashIndex );
    int index = localmapping( HashIndex );
    if ( index >= CurSize )
      enlarge_values_array( index );
    if(  ValuesArrayIntern[index] == NULL ){
      ValuesArrayIntern[index] = new FeatureValue( value, HashIndex, index );
    }
    else {
      ValuesArrayIntern[index]->incr_val_freq();
    }
    if ( tv )
      Value(index)->TargetDist.IncFreq(tv);
    return Value(index);
  }
  
  bool Feature::increment_value( FeatureValue *FV, 
				 TargetValue *tv ){
    bool result = false;
    if ( FV ){
      FV->incr_val_freq();
      if ( tv )
	FV->TargetDist.IncFreq(tv);
      result = true;
    }
    return result;
  }
  
  bool Feature::decrement_value( FeatureValue *FV, TargetValue *tv ){
    bool result = false;
    if ( FV ){
      FV->decr_val_freq();
      if ( tv )
	FV->TargetDist.DecFreq(tv);
      result = true;
    }
    return result;
  }
  
  bool Feature::AllocSparseArrays(int Dim ){
    // Loop over all values.
    //
    for ( int i=0; i < ArraySize(); i++ ){
      FeatureValue *FV = Value(i);
      // Loop over all classes.
      if ( FV->ValueClassProb == NULL ){
	if ( !(FV->ValueClassProb = new SparseValueProbClass( Dim )) ){
	  return false;
	}
      }
    } // for i    
    return true;
  }
  
  BaseFeatTargClass::BaseFeatTargClass( int Size, int Inc, StringHash *T ):
    CurSize( Size ),
    Increment( Inc ),
    array_size( 0 ),
    TokenTree( T ),
    ValuesArrayIntern( CurSize ){
    for ( int i=0; i<CurSize; i++)
      ValuesArrayIntern[i] = NULL;
  }
  
  BaseFeatTargClass::~BaseFeatTargClass(){
    int i;
    for ( i=0; i<CurSize; i++)
      if ( ValuesArrayIntern[i] ) {
	delete ValuesArrayIntern[i];
      }
  }
  
  void BaseFeatTargClass::enlarge_values_array( int Minsize ){
    while ( CurSize <= Minsize )
      CurSize += Increment;
    ValuesArrayIntern.resize( CurSize, NULL );
  }
  
  ValueClass *BaseFeatTargClass::LookupInternal( const string& str ){
    ValueClass *result = NULL;
    int index = TokenTree->Lookup( str );
    if ( index ) {
      IImaptype::const_iterator map_iterator;
      if ( (map_iterator = Mapping.find( index )) == Mapping.end() )
	result = NULL;
      else
	result = ValuesArrayIntern[localmapping( index )];
    }
    return result;
  }

  TargetValue *Target::ReverseLookup( int Index ){
    TargetValue *result = NULL;
    IImaptype::const_iterator map_iterator;
    if ( (map_iterator = Mapping.find( Index )) == Mapping.end() )
      result = NULL;
    else
      result =  dynamic_cast<TargetValue *>(ValuesArrayIntern[localmapping( Index )] );
    return result;
  }

  Feature::~Feature(){
    delete_matrix();
    if ( n_dot_j ) {
      delete [] n_dot_j;
      delete [] n_i_dot;
    }
  }

  bool Feature::matrix_present() {
    return PrestoreStatus == ps_ok;
  }
  
  unsigned int Feature::matrix_byte_size() {
    return metric_matrix.NumBytes();
  }
  
  FeatVal_Stat Feature::prepare_numeric_stats( ){
    double tmp; 
    int freq;
    bool first = true;
    int i;
    for ( i=0; i < ArraySize(); i++ ){
      freq = Value(i)->ValFreq();
      if ( freq > 0 ){
	if ( !string2double( Value(i)->Name(), tmp ) ){
	  Warning( "a Non Numeric value '" + 
		   string(Value(i)->Name()) +
		   "' in Numeric Feature!" );
	  return NotNumeric;
	}
	if ( first ){
	  first = false;
	  n_min = tmp;
	  n_max = tmp;
	}
	else if ( tmp < n_min )
	  n_min = tmp;
	else if ( tmp > n_max )
	  n_max = tmp;
      }
    }
    if ( n_max == n_min )
      return SingletonNumeric;
    else
      return NumericValue;
  }

  inline int min( int i1, int i2 ) { return (i1>i2?i2:i1); }

  void Feature::SharedVarianceStatistics( Target *Targ, int eff_cnt ){
    unsigned int NumInst = Targ->TotalValues();
    int NumCats = Targ->EffectiveValues();
    int k = min( NumCats, eff_cnt ) - 1 ;
    if ( k == 0 || NumInst == 0 )
      shared_variance = 0;
    else
      shared_variance = chi_square / (double)( NumInst * k );
  }
  
  double SparseValueProbClass::vd_distance( SparseValueProbClass *s ){
    double result = 0.0;
    IDmaptype::const_iterator p1 = vc_map.begin();
    IDmaptype::const_iterator p2 = s->vc_map.begin();
    while( p1 != vc_map.end() &&
	   p2 != s->vc_map.end() ){
      if ( (*p2).first < (*p1).first ){
	result += (*p2).second;
	++p2;
      }
      else if ( (*p2).first == (*p1).first ){
	result += fabs( (*p1).second - (*p2).second );
	++p1;
	++p2;
      }
      else {
	result += (*p1).second;
	++p1;
      }	  
    }
    while ( p1 != vc_map.end() ){
      result += (*p1).second;
      ++p1;
    }
    while ( p2 != s->vc_map.end() ){
      result += (*p2).second;
      ++p2;
    }
    result = result / 2.0;
    return result;
  }
  
  double FeatureValue::VDDistance( FeatureValue *G, int limit ){
    double result = 0.0;
    if ( G != this ){
      if ( ValFreq() < limit ||
	   G->ValFreq() < limit ){
	result = 1.0;
      }
      else {
	result = ValueClassProb->vd_distance( G->ValueClassProb );
      }
    }
    return result;
  }

  double k_log_k_div_m( double k, double l ){
    return k * Log2( (2.0 * k)/( k + l ) );
  }
  
  double SparseValueProbClass::jd_distance( SparseValueProbClass *s ){
    double result = 0.0;
    IDmaptype::const_iterator p1 = vc_map.begin();
    IDmaptype::const_iterator p2 = s->vc_map.begin();
    while( p1 != vc_map.end() &&
	   p2 != s->vc_map.end() ){
      if ( (*p2).first < (*p1).first ){
	result += (*p2).second;
	++p2;
      }
      else if ( (*p2).first == (*p1).first ){
	result += k_log_k_div_m( (*p1).second, (*p2).second );
	result += k_log_k_div_m( (*p2).second, (*p1).second );
	++p1;
	++p2;
      }
      else {
	result += (*p1).second;
	++p1;
      }	  
    }
    while ( p1 != vc_map.end() ){
      result += (*p1).second;
      ++p1;
    }
    while ( p2 != s->vc_map.end() ){
      result += (*p2).second;
      ++p2;
    }
    result = result / 2.0;
    //    cerr << "JD distance = " << result << endl;
    return result;
  }
  
  double FeatureValue::JDDistance( FeatureValue *G, int limit ){
    double result = 0.0;
    if ( G != this ){
      if ( ValFreq() < limit ||
	   G->ValFreq() < limit ){
	result = 1.0;
      }
      else {
	result = ValueClassProb->jd_distance( G->ValueClassProb );
      }
    }
    return result;
  }
  
  void Feature::delete_matrix(){
    metric_matrix.Clear();
    PrestoreStatus = ps_undef;
  }
  
  bool Feature::store_matrix( MetricType mt, int limit ){
    //
    // Store a complete distance matrix.
    // (Memory? - we have plenty, but let's limit it to say 10 Mb).
    //
    if ( PrestoreStatus != ps_failed && 
	 ( ( mt == ValueDiff || mt == JeffreyDiv ) ) ){
      try {
	for ( int i = 0; i< ArraySize(); i++ ){
	  for ( int j=0; j < ArraySize(); j++ ){
	    if ( Value(i)->ValFreq() >= matrix_clip_freq &&
		 Value(j)->ValFreq() >= matrix_clip_freq &&
		 ( Prestored_metric != mt ||
		   metric_matrix.Extract(i,j) == 0.0 ) ){
	      if ( mt == ValueDiff ){
		metric_matrix.Assign( i, j, 
				      Value(i)->VDDistance( Value(j), limit ) );
	      }
	      else if ( mt == JeffreyDiv ){	
		metric_matrix.Assign( i, j, 
				      Value(i)->JDDistance( Value(j), limit ) );
	      }
	    } // j
	  } // i
	}
      }
      catch( ... ){
	cout << "hit the ground!" << endl;
	PrestoreStatus = ps_failed;
	return false;
      };
      PrestoreStatus = ps_ok;
    }
    if ( PrestoreStatus == ps_ok ){
      Prestored_metric = mt;
    }
    return true;
  }

  double Feature::ValueDistance( FeatureValue *F, FeatureValue *G, 
				 int th ){
    double result = 1.0;
    if ( F ){
      if ( matrix_present() &&
	   F->ValFreq() >= matrix_clip_freq &&
	   G->ValFreq() >= matrix_clip_freq )
	result = metric_matrix.Extract( F->Index(), G->Index() );
      else
	result = F->VDDistance( G, th );
    }
    return result;
  }
  
  double Feature::JeffreyDistance( FeatureValue *F, FeatureValue *G,
				   int th ){
    double result = 1.0;
    if ( F ){
      if ( matrix_present() &&
	   F->ValFreq() >= matrix_clip_freq &&
	   G->ValFreq() >= matrix_clip_freq )
	result = metric_matrix.Extract( F->Index(), G->Index() );
      else
	result = F->JDDistance( G, th );
    }
    return result;
  }
  

  ostream& operator<< (std::ostream& os, SparseValueProbClass *VPC ){
    if ( VPC ) {
      int old_prec = os.precision();
      os.precision(3);
      os.setf( std::ios::fixed );
      SparseValueProbClass::IDmaptype::const_iterator it = VPC->vc_map.begin();
      for ( int k = 0; k < VPC->dimension; k++){
	os.setf(std::ios::right, std::ios::adjustfield);
	if ( it != VPC->vc_map.end() &&
	     (*it).first == k ){
	  os << "\t" << (*it).second;
	  ++it;
	}
	else
	  os << "\t" << 0.0;
      }
      os << setprecision( old_prec );
    }
    else
      os << "(Null SA)";
    return os;
  }
  
  void Feature::print_vc_pb_array( ostream &os ){
    int i;
    for ( i = 0; i < ArraySize(); i++ ){
      FeatureValue *FV = Value(i);
      if ( FV->ValueClassProb ){
	os << FV << FV->ValueClassProb << endl;
      }
    }
  }
  
  bool Feature::read_vc_pb_array( istream &is ){
    const char*p;
    unsigned int Num = 0;
    FeatureValue *FV;
    bool first = true;
    // clear all existing arrays
    for ( int i = 0; i < ArraySize(); i++ ){
      FeatureValue *FV = Value(i);
      if ( FV->ValueClassProb ){
	delete FV->ValueClassProb;
	FV->ValueClassProb = NULL;
      }
    }
    string name;
    string buf;
    while ( getline( is, buf ) ){
      if ( buf.length() < 8 ){ // "empty" line separates matrices
	break;
      }
      if ( first ){
	p = buf.c_str();
	while ( *p && isspace(*p) ) ++p; // skip whitespace
	while ( *p && !isspace(*p) ) ++p; // skip the featurename
	while ( *p && isspace(*p) ){ // move along counting non space items
	  p++;   // found something non-space
	  Num++; // so increment counter
	  while( *p && !isspace(*p) ) p++; // skip it
	}
	first = false;
      }
      p = buf.c_str();  // restart
      name = "";
      while ( *p && isspace(*p) ) ++p; // skip whitespace
      while ( *p && !isspace(*p) )
	name += *p++;
      FV = Lookup( name );
      if ( !FV ){
	Warning( "Unknown FeatureValue '" + name + "' in file, (skipped) " );
	continue;
      }
      else {
	FV->ValueClassProb = new SparseValueProbClass( Num );
	unsigned int ui = 0;
	double value;
	while ( *p && isspace( *p ) ){
	  while ( *p && isspace(*p) ) ++p; // skip trailing whitespace
	  if ( *p ){
	    if ( ui == Num ){
	      FatalError( "Running out range: " + to_string(ui) );
	      return false;
	    }
	    name = "";
	    while( *p && !isspace( *p ) )
	      name += *p++;
	    if ( !string2double( name, value ) ){
	      Error( "Found illegal value '" + name + "'" );
	      return false;
	    }
	    else if ( value > Epsilon ) {
	      FV->ValueClassProb->Assign( ui, value );
	    }
	    ui++;
	  }
	}
      }
    }
    // check if we've got all the values, assign a default if not so
    int j;
    for ( j = 0; j < ArraySize(); j++ ){
      FeatureValue *FV = Value(j);
      if ( FV->ValueClassProb == NULL ){
	FV->ValueClassProb = new SparseValueProbClass( Num );
      }
    }
    vcpb_read = true;
    return true;
  }

  void Feature::print_matrix( bool shrt ){
    //
    // Print the matrix.
    //
    if ( shrt ){
      cout << " a " << ArraySize() << "x" << ArraySize()
	   << " matrix" << endl;
    }
    else {
      int old_prec = cout.precision();
      cout.setf( ios::scientific );
      int i;
      for ( i=0; i < ArraySize(); i++ ){
	cout.width(6);
	cout.setf(ios::left, ios::adjustfield);
	cout << Value(i) << ":";
	cout.width(12); 
	cout.precision(3);
	cout.setf(ios::right, ios::adjustfield);
	int j;
	for ( j = 0; j < ArraySize(); j++) {
	  cout.width(12); 
	  cout.precision(3);
	  cout.setf(ios::right, ios::adjustfield);
	  if ( Value(i)->ValFreq() < matrix_clip_freq ||
	       Value(j)->ValFreq() < matrix_clip_freq )
	    cout << "*";
	  else
	    cout << metric_matrix.Extract(i,j);
	} // j
	cout << endl;
      } 
      cout << setprecision( old_prec );
    }
  }
  
  TargetValue *Target::add_value( const string& valstr, int freq ){
    int hash_val = TokenTree->Hash( valstr );
    return add_value( hash_val, freq );
  }
  
  TargetValue *Target::add_value( int HashIndex, int freq ){
    const string& name = TokenTree->ReverseLookup( HashIndex );
    int index = localmapping( HashIndex );
    if ( index >= CurSize )
      enlarge_values_array( index );
    if(  ValuesArrayIntern[index] == NULL ){
      ValuesArrayIntern[index] = new TargetValue( name,
						  HashIndex,
						  index );
      ValuesArrayIntern[index]->ValFreq( freq );
    }
    else
      ValuesArrayIntern[index]->ValFreq( ValuesArrayIntern[index]->ValFreq() + freq );
    return Value(index);
  }
  
  TargetValue *Target::MajorityClass() const {
    ValueClass *result = 0;
    int freq = 0;
    for ( int i=0; ValuesArrayIntern[i] != 0; ++i ){
      if ( ValuesArrayIntern[i]->ValFreq() > freq ){
	result = ValuesArrayIntern[i];
	freq = ValuesArrayIntern[i]->ValFreq();
      }
    }
    return (TargetValue*)result;
  }

  bool Target::increment_value( TargetValue *TV ){
    bool result = false;
    if ( TV ){
      TV->incr_val_freq();
      result = true;
    }
    return result;
  }
  
  bool Target::decrement_value( TargetValue *TV ){
    bool result = false;
    if ( TV ){
      TV->decr_val_freq();
      result = true;
    }
    return result;
  }
  
  Instance::Instance( int len ):
    FV(NULL), TV(NULL), Size(len), sample_weight(0.0) {
    FV = new FeatureValue*[len];
    int i;
    for ( i=0; i < len; i++ )
      FV[i] = NULL;
  }
  
  Instance::~Instance(){
    delete [] FV;
  }
  
  ostream& operator<<(ostream& os, const Instance *I ){
    if ( I ){
      int i;
      for ( i=0; i < I->Size; i++ )
	os << I->FV[i] << ", ";
      os << I->TV << " " << I->sample_weight;
    }
    else
      os << " Empty Instance";
    return os;
  }
}
