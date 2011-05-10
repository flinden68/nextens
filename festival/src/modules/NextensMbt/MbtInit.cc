/*
 * MbtInit.cpp 
 *
 * This file contains the Scheme to C(++) coupling for the Mbt methods
 *
 * Original by Albert Russel, www.plankton.nl
 * Modified to use the new Mbt API by Erwin Marsi
 *
 * Copyright (c) 2003
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
 * $Id: MbtInit.cc,v 1.3 2004/04/22 12:26:19 emarsi Exp $
 */

#include "festival.h"
#include "siod.h"

extern LISP mbtInit(LISP);
extern LISP tagString(LISP, LISP);
extern LISP verbose(LISP);


void festival_NextensMbt_init()
{
   proclaim_module("NextensMbt");

   // calls the MbtAPI method of the Mbt API
   init_subr_1("Mbt.init", mbtInit, 
	       "(Mbt.init OPTIONS)\n"
	       "Create a memory-based tagger with OPTIONS,\n"
	       "which is a string of Mbt command line options.\n"
	       "Returns the tagger as a Scheme object.");

   // calls the Tag method of the Mbt API
   init_subr_2("Mbt.tag", tagString, 
	       "(Mbt.tag MBT STRING)\n"
	       "Tag STRING with taggger MBT.\n"
	       "Returns a tagged string.");

   init_subr_1("Mbt.verbose", verbose, 
	       "If true, puts Mbt in verbose mode");
}
