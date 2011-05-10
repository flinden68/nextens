/*
 * TimblInit.cpp 
 *
 * This file contains the Scheme to C(++) coupling for the Timbl methods
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
 * $Id: TimblInit.cc,v 1.2 2004/01/27 13:56:32 emarsi Exp $
 */

#include "festival.h"
#include "siod.h"

extern LISP usage(void);
extern LISP timbl(LISP, LISP);
//extern LISP destroy(LISP);
extern LISP valid(LISP);
extern LISP startServer(LISP, LISP);
extern LISP prepare(LISP, LISP);
extern LISP learn(LISP, LISP);
extern LISP increment(LISP, LISP);
extern LISP decrement(LISP, LISP);
extern LISP expand(LISP, LISP);
extern LISP remove(LISP, LISP);
extern LISP classify(LISP, LISP, LISP, LISP);
extern LISP expname(LISP);
extern LISP version(LISP);
//extern LISP revision(LISP);  // GONE IN TIMBL5
//extern LISP revcmnt(LISP);  // GONE IN TIMBL5
extern LISP saveWeights(LISP, LISP);
extern LISP getWeights(LISP, LISP);
extern LISP writeInstanceBase(LISP, LISP);
extern LISP getInstanceBase(LISP, LISP);
extern LISP writeArrays(LISP, LISP);
extern LISP getArrays(LISP, LISP);
extern LISP writeNamesFile(LISP, LISP);
extern LISP test(LISP, LISP, LISP, LISP);
extern LISP showOptions(LISP);
extern LISP showSettings(LISP);
extern LISP setOptions(LISP, LISP);
extern LISP defaultMaxFeats(LISP);
//extern LISP maxFeatNameLen(LISP);  // GONE IN TIMBL5

/*
 * Binding of the Timbl methods from the Scheme domain to the Timbl API
 */
void festival_NextensTimbl_init(void) {
	proclaim_module("NextensTimbl");

	init_subr_0("Timbl.usage", usage, "show the usage of the available methods");
	init_subr_2("Timbl", timbl, "create a new instance of Timbl");
//	init_subr_1("Timbl.destroy", destroy, "destroy the Timbl API for a certain instance");
	init_subr_1("Timbl.valid", valid, "call the Valid method of the Timbl API");
//	init_subr_2("Timbl.start_server", startServer, "call the StartServer method");
	init_subr_2("Timbl.prepare", prepare, "call the Prepare method of the Timbl API");
	init_subr_2("Timbl.learn", learn, "call the Learn method of the Timbl API");
	init_subr_2("Timbl.increment", increment, "call the Increment method of the Timbl API");
	init_subr_2("Timbl.decrement", decrement, "call the Decrement method of the Timbl API");
	init_subr_2("Timbl.expand", expand, "call the Expand method of the Timbl API");
	init_subr_2("Timbl.remove", remove, "call the Remove method of the Timbl API");
	init_subr_4("Timbl.test", test, "call the Test method of the Timbl API");
	init_subr_4("Timbl.classify", classify, "call the Classify method of the Timbl API");
	init_subr_1("Timbl.expname", expname, "call the ExpName method of the Timbl API");
	init_subr_1("Timbl.version", version, "call the Version method of the Timbl API");
//	init_subr_1("Timbl.revision", revision, "call the Revision method of the Timbl API");
//	init_subr_1("Timbl.revcmnt", revcmnt, "call the RevCmnt method of the Timbl API");
	init_subr_2("Timbl.save_weights", saveWeights, "call the SaveWeights method of the Timbl API");
	init_subr_2("Timbl.get_weights", getWeights, "call the GetWeights method of the Timbl API");
	init_subr_2("Timbl.write_instance_base", writeInstanceBase, "call the WriteInstanceBase method of the Timbl API");
	init_subr_2("Timbl.get_instance_base", getInstanceBase, "call the GetInstanceBase method of the Timbl API");
	init_subr_2("Timbl.write_arrays", writeArrays, "call the WriteArrays method of the Timbl API");
	init_subr_2("Timbl.get_arrays", getArrays, "call the GetArrays method of the Timbl API");
	init_subr_2("Timbl.write_names_file", writeNamesFile, "call the WriteNamesFile method of the Timbl API");
	init_subr_1("Timbl.show_options", showOptions, "call the ShowOptions method of the Timbl API");
	init_subr_1("Timbl.show_settings", showSettings, "call the ShowSettings method of the Timbl API");
	init_subr_2("Timbl.set_options", setOptions, "call the SetOptions method of the Timbl API");
	init_subr_1("Timbl.default_max_feats", defaultMaxFeats, "call the Default_max_Feats method of the Timbl API");
//	init_subr_1("Timbl.max_feat_name_len", maxFeatNameLen, "call the Max_Feat_Name_Len method of the Timbl API");
}

