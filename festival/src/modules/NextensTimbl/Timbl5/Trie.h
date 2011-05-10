#ifndef TRIE_H
#define TRIE_H

/*
 * Trie.h
 * Template definitions of a generic Trie Class.
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

#if defined __GNUC__
#define LTGT <>
#else
#define LTGT
#endif

namespace Tries {
  // A node in the generic trie.
  template <class Info> class TrieNode;
  template <class Info> std::ostream& operator<< ( std::ostream&, 
						   const TrieNode<Info> * );
  template <class Info> class TrieNode {
    friend std::ostream& operator<< LTGT ( std::ostream&, 
					   const TrieNode<Info> * );
  public:
    TrieNode( char );
    ~TrieNode();
    Info *add_to_tree( Info *, const std::string& );
    Info *scan_tree( const std::string& );
    void Iterate( void(*)( Info *, void * ), void * );
    void Iterate( void(*)( Info * ) );
  private:
    char label;             // the label.
    Info *the_info;         // The information at this pnt.
    TrieNode *next_node;       // Pointer to the next node.
    TrieNode *sub_node;        // Pointer to the sub node.
    TrieNode( const TrieNode& );
    TrieNode& operator=( const TrieNode& );
    Info *add_to_tree( Info *, const char * );
    Info *scan_tree( const char * );
  };
  
  template <class Info> 
    inline Info *TrieNode<Info>::scan_tree( const char *name ){
    //
    // returns the info where it is found in the tree or NULL.
    //
    if ( name[0] == '\0' )
      return the_info;
    else {
      TrieNode *subtree = sub_node;
      while ( subtree ) {
	if ( subtree->label == name[0] )
	  return subtree->scan_tree( &name[1] );
	else if ( subtree->label > name[0] )
	  return NULL;
	subtree = subtree->next_node;
      }
    }
    return NULL;
  }
  
  template <class Info> 
    inline Info *TrieNode<Info>::scan_tree( const std::string& name ){
    return scan_tree( name.c_str() );
  }
  
  template <class Info> 
    inline std::ostream& operator << ( std::ostream& os, 
				       const TrieNode<Info> *tree ){ 
    //
    //  print an TrieNode sorted on Info
    //
    if ( tree ){
      os << tree->sub_node;
      if ( tree->the_info )
	os << tree->the_info << std::endl;
      os << tree->next_node;
    }
    return os;
  }
  
  template <class Info> 
    inline void TrieNode<Info>::Iterate( void F( Info * ) ){ 
    //
    //  Do F on each entry in the Trie
    //
    if ( the_info )
      F( the_info );
    if ( sub_node )
      sub_node->Iterate( F );
    if ( next_node )
      next_node->Iterate( F );
  }
  
  template <class Info> 
    inline void TrieNode<Info>::Iterate( void F( Info *, void * ), 
					 void *arg ){ 
    //
    //  Do F on each entry in the Trie
    //
    if ( the_info )
      F( the_info, arg );
    if ( sub_node )
      sub_node->Iterate( F, arg );
    if ( next_node )
      next_node->Iterate( F, arg );
  }
  
  template <class Info>
    inline TrieNode<Info>::TrieNode( char lab ):
    label(lab),
    the_info(NULL),
    next_node(NULL),
    sub_node(NULL)
    {
    }
  
  template <class Info>
    inline TrieNode<Info>::~TrieNode(){
    delete the_info;
    delete sub_node;
    delete next_node;
  }
  
  template <class Info> 
    inline Info *TrieNode<Info>::add_to_tree( Info *info, 
					      const char *lab ){ 
    // If the lab string is empty, we are at the bottom, and
    // we can store the info.
    //
    if ( lab[0] == '\0') {
      if ( !the_info )
	the_info = info;
      return the_info;
    }
    else {
      // Search all the nodes in the node_list for a
      // fitting sub node. If found, continue with it.
      //
      TrieNode<Info> **subNodePtr = &sub_node; // First one.
      while (*subNodePtr != NULL) {
	if ( (*subNodePtr)->label == lab[0] ) {
	  return (*subNodePtr)->add_to_tree( info, &lab[1] );
	}
	else if ( (*subNodePtr)->label > lab[0] ) {
	  TrieNode<Info> *tmp = *subNodePtr;
	  *subNodePtr = new TrieNode<Info>( lab[0] );
	  (*subNodePtr)->next_node = tmp;
	  return (*subNodePtr)->add_to_tree( info, &lab[1] );
	}
	subNodePtr = &((*subNodePtr)->next_node);
      }
      // We don't, so we create a new one, and continue.
      //
      *subNodePtr = new TrieNode<Info>( lab[0] );
      return (*subNodePtr)->add_to_tree( info, &lab[1] );
    }
  }
  
  template <class Info> 
    inline Info *TrieNode<Info>::add_to_tree( Info *info, 
					      const std::string& lab ){
    return add_to_tree( info, lab.c_str() );
  }
  
  // a generic trie.
  template <class Info> class Trie;
  template <class Info> std::ostream &operator << ( std::ostream &, 
						    const Trie<Info> * );

  template <class Info> class Trie{
    friend std::ostream &operator << LTGT ( std::ostream &, 
					    const Trie<Info> * );
  public:
    Trie():
      Tree( new TrieNode<Info>( '\0' ) )
      {
      };
    ~Trie() {
      delete Tree;
    };
    Info *Store( const std::string& str, Info *info ) {
      return Tree->add_to_tree( info, str );
    };
    Info *Retrieve( const std::string& str ) const{
      return Tree->scan_tree( str ); };
    void ForEachDo( void F( Info *, void * ), void *arg ){
      if ( Tree ) Tree->Iterate( F, arg ); };
    void ForEachDo( void F( Info * ) ) { 
      if ( Tree ) Tree->Iterate( F ); };
  protected:
    TrieNode<Info> *Tree;
    Trie( const Trie& );
    Trie& operator=( const Trie& );
  };
  
  template <class Info>
    inline std::ostream &operator << ( std::ostream &os, 
				       const Trie<Info> *T ){
    if ( T )
      os << T->Tree;
    return os;
  }
  
}
#endif

