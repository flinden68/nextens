#ifndef TREE_H
#define TREE_H

/*
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
#include "Trie.h"

namespace Hash {
  using namespace Tries;

// a StringTokenizer. Stores strings and assigns unique numbers
class HashInfo {
  friend std::ostream& operator<< ( std::ostream&, const HashInfo& );
 public:
  HashInfo( const std::string&, const int );
  ~HashInfo();
  const std::string& Name() const { return name; };
  unsigned int Index(){ return ID; };
 private:
  const std::string name;
  unsigned int ID;
  HashInfo( const HashInfo& );
  HashInfo& operator=( const HashInfo& );
};

class StringHash {
  friend std::ostream& operator << ( std::ostream&, const StringHash& );
 public:
  StringHash();
  ~StringHash();
  unsigned int NumOfEntries(){ return NumOfTokens; };
  unsigned int Hash( const std::string& ); 
  unsigned int Lookup( const std::string& ) const;
  const std::string& ReverseLookup( unsigned int );
 private:
  unsigned int NumOfTokens;
  std::vector<HashInfo*> rev_index;
  Trie<HashInfo> StringTree;
  StringHash( const StringHash& );
  StringHash& operator=( const StringHash& );
};

// a Lexion. Stores strings and translations, assigns unique ID's as well
class LexInfo {
  friend std::ostream& operator<<( std::ostream&, const LexInfo& );
 public:
  LexInfo( const std::string&, const std::string& );
  ~LexInfo();
  const std::string& Name() const { return name; };
  const std::string& Trans() const { return trans; };
 private:
  const std::string name;
  const std::string trans;
  LexInfo( const LexInfo& );
  LexInfo& operator=( const LexInfo& );
};

class Lexicon {
  friend std::ostream& operator<< ( std::ostream&, const Lexicon& );
 public:
  Lexicon();
  ~Lexicon();
  LexInfo *Lookup( const std::string& ) const;
  LexInfo *Store( const std::string&, const std::string& );
 private:
  Trie<LexInfo> LexTree;
  Lexicon( const Lexicon& );
  Lexicon& operator=( const Lexicon& );
};

}
#endif
