/*
 * Tree.cc
 *
 *    Based on the generic Trie class we specialize to a StringHash
 *    and and a Lexicon Class
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

#include <iostream>
#ifdef IRIX64
#include <stdlib.h>
#include <string.h>
#else
#include <cstdlib>
#include <cstring>
#endif

#include "Common.h"
#include "Tree.h"

using namespace std;

namespace Hash {
  using namespace Common;
  using namespace Tries;
  
  HashInfo::HashInfo( const string& Tname, int Indx ):
    name(Tname), ID(Indx){}
  
  HashInfo::~HashInfo(){
  }
  
  ostream& operator<<( ostream& os, const HashInfo& tok ){ 
    os << tok.ID << " " << tok.name;
    return os;
  }

  StringHash::StringHash():
    NumOfTokens(0)
  {}

  StringHash::~StringHash( ){
  }

  unsigned int StringHash::Hash( const string& name ){
    HashInfo *info = StringTree.Retrieve( name );
    if ( !info ){
      info = new HashInfo( name, ++NumOfTokens );
      info = (HashInfo *)StringTree.Store( name, info );
    }
    unsigned int idx = info->Index();
    if ( idx >= rev_index.size() ){
      rev_index.resize( rev_index.size() + 1000 );
    }
    rev_index[idx] = info;
    return info->Index();
  }
  
  unsigned int StringHash::Lookup( const string& name ) const {
    HashInfo *info = StringTree.Retrieve( name );
    if ( info )
      return info->Index(); 
    else
      return 0;
  }
  
  const string& StringHash::ReverseLookup( unsigned int index ) {
    return rev_index[index]->Name();
  }
  
  ostream& operator << ( ostream& os, const StringHash& S){
    return os << &S.StringTree; }
  
  LexInfo::LexInfo( const string& Tname, const string& Tran ):
    name(Tname),trans(Tran){}
  
  LexInfo::~LexInfo(){}
  
  ostream& operator<<( ostream& os, const LexInfo& LI ){ 
    os << " " << LI.name << " - " << LI.trans;
    return os;
  }
  
  Lexicon::Lexicon(){}
 
  Lexicon::~Lexicon(){}
  
  LexInfo *Lexicon::Lookup( const string& name ) const {
    return (LexInfo *)LexTree.Retrieve( name ); 
  }
  
  LexInfo *Lexicon::Store( const string& name, const string& translation ){
    LexInfo *info = LexTree.Retrieve( name );
    if ( !info ){
      info = new LexInfo( name, translation );
      return LexTree.Store( name, info );
    }
    return info;
  }
  
  ostream& operator<<( ostream& os, const Lexicon& L )
  { return os << &L.LexTree; }
  
} 
