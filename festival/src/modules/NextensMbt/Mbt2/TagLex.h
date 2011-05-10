/*
 *
 * Copyright (c) 1998-2005
 * ILK  -  Tilburg University
 * CNTS -  University of Antwerp
 *
 * All rights Reserved.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * For questions and suggestions, see:
 *	http://ilk.kub.nl/software.html
 * or send mail to:
 *	Timbl@kub.nl
 */

#ifndef TAGTREE_H
#define TAGTREE_H

#include "Trie.h"
#include "Common.h"

namespace Tagger {
  using Tries::Trie;

const int MAX_TAGS = 200;

class TagFreqList {
  friend std::ostream& operator<<( std::ostream&, TagFreqList * );
 public:
  TagFreqList( const std::string& s ) { tag = s; freq=1; next = NULL; };
  ~TagFreqList();
  std::string tag;
  int freq;
  TagFreqList *next;
};

class TagFreq {
  friend std::ostream& operator<<( std::ostream&, TagFreq * );
 public:
  TagFreq( ) { Tags = NULL; };
  ~TagFreq() { delete Tags; };
  void Update( const std::string&  );
  void Prune( int, int );
  void FreqSort( );
  TagFreqList *Tags;
};

// a Tagged Lexion. Stores strings , frequencies and assigned tags
class TagInfo {
  friend std::ostream& operator<<( std::ostream&, TagInfo * );
 public:
  TagInfo( const std::string& , const std::string&  );
  ~TagInfo();
  void Update( const std::string&  );
  const int Freq() const { return WordFreq; };
  void Prune( int perc ) { TF->Prune( perc, WordFreq ); };
  std::string Word;
  int WordFreq;
  TagFreq *TF;
  std::string StringRepr;
};

class TagLex {
  friend std::ostream& operator<< ( std::ostream&, TagLex * );
 public:
  TagLex();
  ~TagLex();
  TagInfo *Lookup( const std::string& s );
  TagInfo *Store( const std::string&  , const std::string&  );
  TagInfo **CreateSortedArray();
  Trie<TagInfo> *TagTree;
  int NumOfEntries;
};

}
#endif
