#ifndef OPTIONS_H
#define OPTIONS_H

/*
 * Options.h
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
#include <vector>
#ifdef IRIX64
#include <limits.h>
#include <stdio.h>
#else
#include <climits>
#include <cstdio>
#endif

namespace Timbl {
  using namespace Common;
  const int MAX_TABLE_SIZE =  50;
  
  class OptionClass {
  public:
    OptionClass( const std::string& n ): Name( n ) {};
    virtual ~OptionClass() {};
    virtual bool set_option( const std::string& ) = 0;
    virtual std::ostream& show_opt( std::ostream & ) = 0;
    virtual std::ostream& show_full( std::ostream & ) = 0;
    const std::string Name;
  private:
    OptionClass(const OptionClass&);
    OptionClass& operator = (const OptionClass&);
  };
  
  template <class Type>
    class OptionClassT: public OptionClass {
    public:
    OptionClassT( const std::string& n, Type *tp, Type t ):OptionClass(n),
      Content(tp) { *Content = t; };
    virtual bool set_option( const std::string& line ){ 
      Type T;
      bool result = string_to( line, T );
      if ( result ) *Content = T;
      return result;
    };
    virtual std::ostream& show_opt( std::ostream &os ){
      os.width(20);
      os.setf( std::ios::left, std::ios::adjustfield );
      os << Name << " : " << to_string(*Content);
      return os;
    };
    virtual std::ostream& show_full( std::ostream &os ){
      return show_opt( os );
    };
    private:
    Type *Content;
    OptionClassT(const OptionClassT&);
    OptionClassT& operator = (const OptionClassT&);
  };
  
  template <class Type>
    class OptionClassLT: public OptionClass {
    public:
    OptionClassLT( const std::string& n, Type *tp, Type t,
		   Type Min, Type Max ):OptionClass(n),
      Content( tp), MinVal( Min ), MaxVal( Max )
      { *Content = t; };
    
    virtual bool set_option( const std::string& line ){ 
      Type T;
      bool result = string_to( line, T, MinVal, MaxVal );
      if ( result ) *Content = T;
      return result;
    };
    virtual std::ostream& show_opt( std::ostream &os ){
      os.width(20);
      os.setf( std::ios::showpoint );
      os.setf( std::ios::left, std::ios::adjustfield );
      os << Name << " : " << *Content;
      return os;
    };
    virtual std::ostream& show_full( std::ostream &os ){
      os.width(20);
      os.setf( std::ios::showpoint );
      os.setf( std::ios::left, std::ios::adjustfield );
      os << Name << " :  { " 
	 << MinVal << " - " << MaxVal << "}, [" << *Content << "]";
      return os;
    };
    private:
    Type *Content;
    Type MinVal;
    Type MaxVal;
    OptionClassLT(const OptionClassLT&);
    OptionClassLT& operator = (const OptionClassLT&);
  };
  
  typedef OptionClassT<bool> BoolOption;
  
  template <>
    inline std::ostream& OptionClassT<bool>::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " :  - or + [" << to_string( *Content) << "]";
    return os;
  }
  
  typedef OptionClassT<char*> StringOption;
  
  template <>
    inline std::ostream& OptionClassT<char*>::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : a string [" << to_string(*Content) << "]";
    return os;
  }
  
  typedef OptionClassT<VerbosityFlags> VerbosityOption;
  
  template <>
    inline std::ostream& OptionClassT<VerbosityFlags>::show_opt( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : " << to_string(*Content);
    return os;
  }
  
  template <>
    inline std::ostream& OptionClassT<VerbosityFlags>::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : " << to_string(*Content);
    return os;
  }
  
  typedef OptionClassLT<int> IntegerOption;
  
  template <>
    inline bool IntegerOption::set_option( const std::string& line ){ 
    int i;
    bool result = string_to( line, i, MinVal, MaxVal );
    if ( result ) *Content = i;
    return result;
  }
  
  typedef OptionClassLT<double> RealOption;
  
  template <>
    inline bool RealOption::set_option( const std::string& line ){ 
    double d;
    bool result = string_to( line, d, MinVal, MaxVal );
    if ( result ) *Content = d;
    return result;
  }  
  
  typedef OptionClassT<InputFormatType> InputFormatOption;
  
  template <>
    inline std::ostream& InputFormatOption::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : {";
    InputFormatType i = UnknownInputFormat;
    for ( ++i; i < MaxInputFormat-1; ++i )
      os << to_string(i) << ", ";
    os << to_string(i) << "}, [ "
       << to_string(*Content) << "]";
    return os;
  }
  
  
  typedef OptionClassT<MetricType> MetricOption;
  
  template <>
    inline std::ostream& OptionClassT<MetricType>::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : {";
    MetricType i = UnknownMetric;
    for ( ++i; i < MaxMetric-1; ++i )
      os << to_string(i) << ", ";
    os << to_string(i) << "}, [ "
       << to_string(*Content) << "]";
    return os;
  }
  
  template <class Type>
    class OptionArrayClass: public OptionClass {
    public:
    OptionArrayClass( const std::string& n, Type* ta, const int size ): 
      OptionClass( n ), TA(ta), Size(size ){};
    protected:
    Type *TA;
    int Size;
    private:
    OptionArrayClass(const OptionArrayClass&);
    OptionArrayClass& operator = (const OptionArrayClass&);
  };
  
  
  class MetricArrayOption: public OptionArrayClass<MetricType> {
  public:
    MetricArrayOption( const std::string& n, MetricType *mp, MetricType m, int s ):
      OptionArrayClass<MetricType>( n, mp, s ){ 
      for ( int i=0; i < s; i++ )
	TA[i] = m;
    };
    bool set_option( const std::string& line );
    std::ostream& show_opt( std::ostream &os );
    std::ostream& show_full( std::ostream &os );
  };

  inline bool MetricArrayOption::set_option( const std::string& line ){ 
    MetricType m;
    int i;
    std::vector<std::string> res;
    bool result = split_at( line, res, "=" ) == 2 &&
      string_to( res[1], m ) && 
      string_to( res[0], i, 0, Size );
    if ( result ) 
      TA[i] = m;
    return result;
  }
  
    inline std::ostream& MetricArrayOption::show_opt( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : ";
    int i;
    for ( i=0; i < Size; i++ )
      if ( TA[i] != DefaultMetric )
	os << i << ":" << to_string(TA[i]) << ", ";
    return os;
  }
  
  inline std::ostream& MetricArrayOption::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : comma separated metricvalues, [";
    bool frst = true;
    int i;
    for ( i=0; i < Size; i++ ){
      if ( TA[i] != DefaultMetric ){
	if ( !frst )
	  os << ",";
	else
	  frst = false;
	os << i << ":" << to_string(TA[i]);
      }
    }
    os << "]";
    return os;
  }
  

  typedef OptionClassT<AlgorithmType> AlgorithmOption;
  
  template <>
    inline std::ostream& OptionClassT<AlgorithmType>::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : {";
    AlgorithmType i = Unknown_a;
    for ( ++i; i < Max_a-1; ++i )
      os << to_string(i) << ", ";
    os << to_string(i) << "}, [ "
       << to_string(*Content) << "]";
    return os;
  }
  
  typedef OptionClassT<DecayType> DecayOption;
  
  template <>
    inline std::ostream& DecayOption::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : {";
    DecayType i = UnknownDecay;
    for ( ++i; i < MaxDecay-1; ++i )
      os << to_string(i) << ", ";
    os << to_string(i) << "}, [ "
       << to_string(*Content) << "]";
    return os;
  }
  
  typedef OptionClassT<SmoothingType> SmoothOption;
  
  template <>
    inline std::ostream& SmoothOption::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : {";
    SmoothingType i = UnknownSmoothing;
    for ( ++i; i < MaxSmoothing-1; ++i )
      os << to_string(i) << ", ";
    os << to_string(i) << "}, [ "
       << to_string(*Content) << "]";
    return os;
  }
  
  typedef OptionClassT<WeightType> WeightOption;
  
  template <>
    inline std::ostream& OptionClassT<WeightType>::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : {";
    WeightType i = Unknown_w;
    for ( ++i; i < Max_w-1; ++i )
      os << to_string(i) << ", ";
    os << to_string(i) << "}, [ "
       << to_string(*Content) << "]";
    return os;
  }
  
  typedef OptionClassT<OrdeningType> OrdeningOption;
  
  template <>
    inline std::ostream& OptionClassT<OrdeningType>::show_full( std::ostream &os ){
    os.width(20);
    os.setf( std::ios::left, std::ios::adjustfield );
    os << Name << " : {";
    OrdeningType i = UnknownOrdening;
    for ( ++i; i < MaxOrdening-1; ++i )
      os << to_string(i) << ", ";
    os << to_string(i) << "}, [ "
       << to_string(*Content) << "]";
    return os;
  }
  
  
  enum SetOptRes { Opt_OK, Opt_Frozen, Opt_Unknown, Opt_Ill_Val};
  
  class OptionTableClass {
  public:
    bool Add( OptionClass *opt ){
      Table[table_size++] = opt;
      return table_size < MAX_TABLE_SIZE;
    };
    void SetFreezeMark(void){ table_start = table_size; };
    void FreezeTable(void){ table_frozen = true; };
    bool TableFrozen(void){ return table_frozen; };
    SetOptRes SetOption( const std::string& );
    void Show_Settings( std::ostream& ) const;
    void Show_Options( std::ostream& ) const;
    OptionTableClass():
      table_start(0), table_size(0), table_frozen(false),Table(0){
      Table = new OptionClass *[MAX_TABLE_SIZE]; };
    ~OptionTableClass(){
      for ( int i=0; i < table_size; i++ )
	delete Table[i];
      delete [] Table;
    };
  private:
    int table_start;
    int table_size;
    bool table_frozen;
    OptionClass **Table;
    inline OptionClass *look_up( const std::string&, bool & );
    OptionTableClass( const OptionTableClass& );
    OptionTableClass& operator=( const OptionTableClass& );
  };
  
  inline void OptionTableClass::Show_Settings( std::ostream& os ) const{
    for ( int i=0; i <table_size; i++)
      Table[i]->show_opt( os ) << std::endl;
  }
  
  inline void OptionTableClass::Show_Options( std::ostream& os ) const {
    for ( int i=0; i <table_size; i++)
      Table[i]->show_full( os ) << std::endl;
  }

  inline void split_line( const std::string& line, 
			  std::string& name, 
			  std::string& value ){
    std::vector<std::string> results;
    int i = split_at( line, results, ":" );
    switch (i){
    case 2:
      name = compress(results[0]);
    case 1:
      value = compress(results[1]);
    default:
      break;
    }
  }  

  inline OptionClass *OptionTableClass::look_up( const std::string& option_name, 
						 bool &runtime ){
    for ( int i=0; i < table_size; i++ )
      if ( compare_nocase( option_name, Table[i]->Name ) ){
	runtime = (i >= table_start || !table_frozen );
	return Table[i];
      }
    return NULL;
  }
  
  inline SetOptRes OptionTableClass::SetOption( const std::string& line ){ 
    SetOptRes result = Opt_OK;
    bool runtime = false;
    std::string option_name;
    std::string value;
    split_line( line, option_name, value );
    OptionClass *option = look_up( option_name, runtime );
    if ( option ){
      if ( !runtime )
	result = Opt_Frozen; // may not be changed at this stage
      else
	if ( !option->set_option( value ) )
	  result = Opt_Ill_Val; // illegal value
    }
    else 
      result = Opt_Unknown; // What the hell ???
    return result;
  }  


}

#endif

