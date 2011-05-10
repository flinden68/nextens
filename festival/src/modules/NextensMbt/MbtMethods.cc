/*
 * MbtMethods.cpp 
 *
 * This file contains the Scheme to C(++) coupling for the Mbt methods
 *
 * Original by Albert Russel, www.plankton.nl
 *
 * Modified to work with the new Mbt API by Erwin Marsi.
 * Getting the token string and adding the POS tags as features on token items
 * is now handled in Scheme.
 *
 * Copyright (c) 2003, 2004
 * ILK - Tilburg University
 * L&S - University of Nijmegen
 * Stichting Spraaktechnologie
 *
 * All rights Reserved.
 *
 * See the files NEXTENS.COPYING and NEXTENS.LICENSE 
 * for information on usage and redistribution of this file, 
 * and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * $Id: MbtMethods.cc,v 1.3 2004/04/22 12:26:19 emarsi Exp $
 */

#include <iostream>
#include "festival.h"
#include "siod.h"
#include "MbtAPI.h"
#include <string>

// register MbtAPI class in Festival
VAL_REGISTER_CLASS(mbtapi, MbtAPI)
VAL_REGISTER_CLASS_DCLS(mbtapi, MbtAPI)
SIOD_REGISTER_CLASS(mbtapi, MbtAPI)
SIOD_REGISTER_CLASS_DCLS(mbtapi, MbtAPI)

// flag for showing Mbt actions
bool verboseMode = false;


LISP mbtInit(LISP lispOpts) {
  std::string opts = "";
  if (lispOpts != NIL) {
    opts = get_c_string(lispOpts);
  }

  // streambuffer pointers for cout and cerr
  streambuf* outbuf = NULL;
  streambuf* errbuf = NULL;

  // suppress cout and cerr	
  if (!verboseMode) {
    ofstream* nopstream = new ofstream();
    streambuf* nopbuf = nopstream->rdbuf();
    outbuf = cout.rdbuf(nopbuf);
    errbuf = cerr.rdbuf(nopbuf);
  }

  // create a new instance of the Mbt API
  MbtAPI *api = new MbtAPI(opts);

  // repair cout and cerr
  if (!verboseMode) {
    std::cout.rdbuf(outbuf);
    std::cerr.rdbuf(errbuf);
  }

  // give the object handle to the SCHEME world
  return siod(api);
}


LISP tagString(LISP id, LISP lispString) {
  // convert the Scheme id to a MbtAPI object
  MbtAPI *api = mbtapi(id);

  // convert lisp string to c string
  std::string cString = get_c_string(lispString);

  // tag tokens in string
  std::string result = "";
  result = api->Tag(cString);

  // convert to c to lisp string
  return strintern(result.c_str());
}


LISP verbose(LISP boolean) {
	verboseMode = boolean;

	// return to the SCHEME world
	return verboseMode ? cintern("t") : NIL;
}
