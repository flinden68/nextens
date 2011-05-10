/*
 * PraatInit.cc 
 *
 * This file contains the Scheme to C(++) coupling for the sendpraat function,
 * which allows sending commands to the Praat program.
 *
 * by Albert Russel, www.plankton.nl
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
 * $Id: PraatInit.cc,v 1.3 2004/01/27 14:51:00 emarsi Exp $
 */

#include "festival.h"
#include "siod.h"

extern char* sendpraat(void *display, const char *programName, long timeOut, const char *text);

/**
 * Send a command to praat
 *
 * Normal usage has par1 = program name, par2 = time out and par3 = command text
 * The first two parameters are optional and default to respective "praat" and 0
 */
LISP NXTSendPraat(LISP par1, LISP par2, LISP par3) {
	const char* progName = "praat";
	long timeOut = 0;
	const char* text = NULL;

	if (par2 == NULL) { // only one parameter given, suppose talking to praat and timeout 0
		text = get_c_string(par1);
	} else if (par3 == NULL) { // only two parameters given, suppose talking to praat
		timeOut = (long) get_c_double(par1);
		text = get_c_string(par2);
	} else { // all parameters given, use them
		progName = get_c_string(par1);
		timeOut = (long) get_c_double(par2);
		text = get_c_string(par3);
	}

	char* result = sendpraat(NULL, progName, timeOut, text);

	if (result != NULL) {
		return strintern(result);
	} else {
		return NIL;
	}
}

/*
 * Binding of the Timbl functions from the Scheme domain to the Timbl methods
 */
void festival_NextensPraat_init(void) {
	proclaim_module("NextensPraat");

	init_subr_3("Sendpraat", NXTSendPraat, "(Sendpraat PROGRAMNAME TIMEOUT COMMAND)\n Send COMMAND to PROGRAMNAME, waiting TIMEOUT seconds for reply.\n The first two parameters are optional and default to 'praat' and 0 respectively.");
}
