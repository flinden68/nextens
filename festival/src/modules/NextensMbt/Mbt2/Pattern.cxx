/*
 * Pattern.cc
 *
 *    The Pattern Class
 *
 * Copyright (c) 1998-2005
 * ILK  -  Tilburg University
 * CNTS -  University of Antwerp
 *
 * All rights Reserved.
 *
 */

#include <cstdlib>
#include <fstream> 
#include <iostream>
#include <cstring>
#include <cctype>
#include <cassert>
#include <string>

#include "Pattern.h"

using std::string;
using std::cerr;
using std::endl;

PatTemplate::PatTemplate()
{  
  numslots = 0;
  wordslots = 0;
  focuspos =-1;
  word_focuspos = 0;
  numsuffix= 0;
  numprefix= 0;
  skipfocus= 0;
  hyphen   = 0;
  capital  = 0;
  numeric  = 0;
  wordfocus = 0;
  templatestring = "";
  word_templatestring = "";
}

bool PatTemplate::set( const string& tempstr ){ 
  // reads a format string and figures out a template for the patterns from 
  // this
  tlen = tempstr.length();
  
  // Find the position of the focus.
  // Seperate string is built for the word-context. 
  int j = 0;
  int k = 0;
  bool focus = false;
  compensation = 0;
  for ( int i = 0; i < tlen; i++) {
    switch(tempstr[i]){
    case 'f':
      if ( focus ){
	cerr << "more than 1 focus position in Pattern! " << tempstr << endl;
	return false;
      }
      focuspos = j;
      skipfocus=0;
      templatestring += tempstr[i];
      word_templatestring += tempstr[i];
      word_focuspos = k;
      numslots++;
      j++;
      ++wordslots;
      ++compensation;
      ++k;
      focus = true;
      break;
    case 'F':
      if ( focus ){
	cerr << "more than 1 focus position in Pattern! " << tempstr << endl;
	return false;
      }
      focuspos = j;
      skipfocus=1;
      templatestring += tempstr[i];
      word_templatestring += tempstr[i];
      word_focuspos = k;
      numslots++;
      j++;
      ++wordslots;
      ++compensation;
      ++k;
      focus = true;
      break;
    case 'd':
      templatestring += tempstr[i];
      numslots++;
      j++;
      break;
    case 'a':
      templatestring += tempstr[i];
      numslots++;
      j++;
      break;
    case 'p':
      numprefix++;
      break;
    case 's':
      numsuffix++;
      break;
    case 'h':
      hyphen=1;
      break;
    case 'c':
      capital = 1;
      break;
    case 'n':
      numeric = 1;
      break;
    case 'w':
      word_templatestring += tempstr[i];
      ++wordslots;
      ++k;
      break;
    case 'W':
      //
      // the W is a modifier which changes the 'f' to a 
      // w.
      //
      if ( focus ){
	if ( word_templatestring[word_focuspos] == 'f' ||
	     word_templatestring[word_focuspos] == 'F' ) {
	  word_templatestring[word_focuspos] = 'w';
	  --compensation;
	  wordfocus = 1;
	}
	else {
	  cerr << "W modifier not directly after 'f' or 'F' in " 
	       << tempstr << endl;
	}
      }
      else {
	// asume W means FW
	focuspos = j;
	skipfocus = 1;
	templatestring += 'F';
	word_templatestring += 'w';
	word_focuspos = k;
	numslots++;
	j++;
	++wordslots;
	++k;
	wordfocus = 1;
	focus = true;
      }
      break;
    default:
      cerr << "ERROR: illegal symbol '" << tempstr[i]
	   << "' in context string'" << endl;
      return false;
    }
  }

  if (focuspos == -1) {
    cerr << "ERROR: no focus in context string." << endl;
    return false;
  }
  return true;
}

int PatTemplate::sprint( string& targetstring) {
  int j=0;
  for( int i=0; i<numprefix; ++i ){
    targetstring[j]='p';
    j++;
  }

  for( int i=0; i<numslots; ++i ){
    if ((templatestring[i] == 'f') || (templatestring[i] == 'F')) {
      for ( int k = 0; k < word_focuspos; ++k ) {
	targetstring[j]=word_templatestring[k];
	++j;
      }
    }
    
    targetstring[j]=templatestring[i];
    j++;
    
    if ((templatestring[i] == 'f') || (templatestring[i] == 'F')) {
      for ( int k = word_focuspos+1; k < wordslots; ++k ) {
	targetstring[j]=word_templatestring[k];
	++j;
      }
    }
  }

  for( int i=0; i<numsuffix; ++i ){
    targetstring[j] = 's';
    j++;
  }
  if(hyphen){
    targetstring[j] = 'h';
    j++;
  }
  if(capital){
    targetstring[j] = 'c';
    j++;
  }
  if( numeric ){
    targetstring[j] = 'n';
    j++;
  }
  if (wordfocus) {
    targetstring[j] = 'W';
    ++j;
  }
  return targetstring.length();
}

int PatTemplate::totalslots()
{
  return ( numslots + numprefix + numsuffix + 
	   hyphen + capital + numeric + word_totalslots());
}


int PatTemplate::word_totalslots()
{
  // Compensate for the 'f'
  // (hm, what if 'F' or 'W'?).
  //
  return (wordslots-compensation);
}

