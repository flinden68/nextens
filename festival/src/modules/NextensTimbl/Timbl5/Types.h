#ifndef TYPES_H
#define TYPES_H

/*
 * Types.h
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
  enum InputFormatType { UnknownInputFormat,
			 Compact, C4_5, Columns, ARFF, SparseBin,
			 Sparse,
			 MaxInputFormat };
  
  inline InputFormatType& operator++( InputFormatType &I ){
    return I = ( MaxInputFormat == I ) 
      ? UnknownInputFormat 
      : InputFormatType(I+1);
  }
  
  
  enum WeightType { Unknown_w,
		    No_w, GR_w, IG_w, X2_w, SV_w, UserDefined_w, 
		    Max_w };
  
  inline WeightType& operator++( WeightType &W ){
    return W = ( Max_w == W ) ? Unknown_w : WeightType(W+1);
  }
  
  enum AlgorithmType { Unknown_a,
		       IB1_a, IB2_a, IGTREE_a, TRIBL_a, TRIBL2_a,
		       LOO_a, CV_a,
		       Max_a };
  
  inline AlgorithmType& operator++( AlgorithmType &W ){
    return W = ( Max_a == W ) ? Unknown_a : AlgorithmType(W+1);
  }
  
  
  enum MetricType { UnknownMetric, DefaultMetric, Ignore, 
		    Numeric, DotProduct, Overlap, 
		    ValueDiff, JeffreyDiv, MaxMetric };
  
  inline MetricType& operator++( MetricType &W ){
    return W = ( MaxMetric == W ) ? UnknownMetric : MetricType(W+1);
  }
  
  enum OrdeningType { UnknownOrdening, DataFile, NoOrder,
		      GROrder, IGOrder, 
		      OneoverFeature, OneoverSplitInfo,
		      GRoverFeature, IGoverFeature,
		      GREntropyOrder, IGEntropyOrder,
		      X2Order, SVOrder, 
		      X2overFeature, SVoverFeature,
		      MaxOrdening };
  
  inline OrdeningType& operator++( OrdeningType &W ){
    return W = ( MaxOrdening == W ) ? UnknownOrdening : OrdeningType(W+1);
  }
  
  
  enum VerbosityFlags { NO_VERB=0, SILENT=1, OPTIONS=2, FEAT_W=4,
			VD_MATRIX=8, EXACT=16, DISTANCE=32, DISTRIB=64,
			NEAR_N=128, ADVANCED_STATS=256, CONF_MATRIX=512,
			CLASS_STATS=1024, CLIENTDEBUG=2048, ALL_K=4096,
                        MAX_VERB };

  inline VerbosityFlags operator~( VerbosityFlags V ){
    return (VerbosityFlags)( ~(int)V );
  }

  inline VerbosityFlags operator|( VerbosityFlags V1, VerbosityFlags V2 ){
    return (VerbosityFlags)( (int)V1|(int)V2 );
  }

  inline VerbosityFlags& operator|= ( VerbosityFlags& f, VerbosityFlags g ){
    f = (f | g);
    return f;
  }
  
  inline VerbosityFlags operator& ( VerbosityFlags f, VerbosityFlags g ){
    return (VerbosityFlags)((int)f & (int)g );
}
  
  inline VerbosityFlags& operator&= ( VerbosityFlags& f, VerbosityFlags g ){
    f = (f & g);
    return f;
  }
  
  enum OptType { UnknownOpt,
		 StringT, IntegerT, BooleanT, VerbosityT, IFormatT,
		 AlgoT, MetricT, WeightT, OrdeningT,
		 MaxOpt };
  
  inline OptType& operator++( OptType &W ){
    return W = ( MaxOpt == W ) ? UnknownOpt : OptType(W+1);
  }
  
  enum DecayType { UnknownDecay, 
		   Zero, InvDist, InvLinear, ExpDecay, MaxDecay };
   
  inline DecayType& operator++( DecayType &W ){
    return W = ( MaxDecay == W ) ? UnknownDecay : DecayType(W+1);
  }
  
  enum SmoothingType { UnknownSmoothing, Default, Lidstone, MaxSmoothing };  
  
  inline SmoothingType& operator++( SmoothingType &W ){
    return W = ( MaxSmoothing == W ) ? UnknownSmoothing : SmoothingType(W+1);
  }
  

  bool string_to( const std::string&, bool & );
  bool string_to( const std::string&, int &, int, int );
  bool string_to( const std::string&, double &, double, double );
  bool string_to( const std::string&, AlgorithmType & );
  bool string_to( const std::string&, DecayType & );
  bool string_to( const std::string&, SmoothingType & );
  bool string_to( const std::string&, MetricType & );
  bool string_to( const std::string&, WeightType & );
  bool string_to( const std::string&, InputFormatType & );
  bool string_to( const std::string&, OrdeningType & );
  bool string_to( const std::string&, VerbosityFlags & );
  bool string_to( const std::string&, std::string& );

  std::string to_string( int, bool full = false );
  std::string to_string( unsigned int, bool full = false );
  std::string to_string( double, bool full = false );
  const std::string to_string( VerbosityFlags, bool full = false );
  const std::string to_string( bool, bool full = false );
  const std::string to_string( WeightType, bool full = false );
  const std::string to_string( AlgorithmType, bool full = false );
  const std::string to_string( MetricType, bool full = false );
  const std::string to_string( OrdeningType, bool full = false );
  const std::string to_string( InputFormatType, bool full = false );
  const std::string to_string( DecayType, bool full = false );
  const std::string to_string( SmoothingType, bool full = false );
}
#endif
