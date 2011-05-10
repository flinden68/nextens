/*
 * TimblMethods.cpp 
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
 * $Id: TimblMethods.cc,v 1.3 2004/01/27 13:56:32 emarsi Exp $
 */

#include <iostream>
#include "festival.h"
#include "siod.h"
#include "Timbl5/TimblAPI.h"
#include <string>

VAL_REGISTER_CLASS(timblapi, TimblAPI)
VAL_REGISTER_CLASS_DCLS(timblapi, TimblAPI)
SIOD_REGISTER_CLASS(timblapi, TimblAPI)
SIOD_REGISTER_CLASS_DCLS(timblapi, TimblAPI)

// show the usage of the Timbl API
LISP usage() {
	cout << "METHOD NAME			RESULT	PARAMETERS [ ] means optional" << endl;
	cout << endl;
	cout << "Timbl				ID	[string] [string]" << endl;
//	cout << "Timbl.destroy			nil	ID"	 << endl;
	cout << "Timbl.valid			bool	ID" << endl;
//	cout << "Timbl.start_server		bool	ID portNumber" << endl;
	cout << "Timbl.prepare			bool	ID fileName" << endl;
	cout << "Timbl.learn			bool	ID fileName" << endl;
	cout << "Timbl.increment			bool	ID instanceString" << endl;
	cout << "Timbl.decrement			bool	ID instanceString" << endl;
	cout << "Timbl.expand			bool	ID fileName" << endl;
	cout << "Timbl.remove			bool	ID fileName" << endl;
	cout << "Timbl.test			bool	ID fileName [outFileName] [percFileName]" << endl;
	cout << "Timbl.classify			string	ID line" << endl;
	cout << "Timbl.classify			string	ID line distance" << endl;
	cout << "Timbl.classify			string	ID line valueDistribution distance" << endl;
	cout << "Timbl.expname			string	ID" << endl;
	cout << "Timbl.version			integer	ID" << endl;
	cout << "Timbl.revision			integer	ID" << endl;
	cout << "Timbl.revcmnt			string	ID" << endl;
	cout << "Timbl.save_weights		bool	ID fileName" << endl;
	cout << "Timbl.get_weights		bool	ID fileName" << endl;
	cout << "Timbl.write_instance_base	bool	ID fileName" << endl;
	cout << "Timbl.get_instance_base		bool	ID fileName" << endl;
	cout << "Timbl.write_arrays		bool	ID fileName" << endl;
	cout << "Timbl.get_arrays		bool	ID fileName" << endl;
	cout << "Timbl.write_names_file		bool	ID fileName" << endl;
	cout << "Timbl.show_options		bool	ID" << endl;
	cout << "Timbl.show_settings		bool	ID" << endl;
	cout << "Timbl.set_options		bool	ID options" << endl;
	cout << "Timbl.default_max_feats		integer	ID" << endl;
	cout << "Timbl.max_feat_name_len		integer	ID" << endl;

	return NIL;
}

LISP timbl(LISP theParameters, LISP theName) {
//	const char *parameters = "";
	std::string parameters = "";
	if (theParameters != NIL) {
		parameters = get_c_string(theParameters);
	}

//	const char *name = NULL;
	std::string name = "";
	if (theName != NIL) {
		name = get_c_string(theName);
	}

	// create a new instance of the Timbl API
	TimblAPI *api = new TimblAPI(parameters, name);


	// give the object handle to the SCHEME world
	return siod(api);
}

/*
 * DISABLED BECAUSE OF PROBLEMS WITH THE SCHEME GARBAGE COLLECTOR
 * THAT ALSO TRIES TO DELETE THE TIMBL OBJECT
 
LISP destroy(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// delete the api object
	delete api;

	// return to the SCHEME world
	return NIL;
}
*/

LISP valid(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
	bool result = api->Valid();

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP startServer(LISP id, LISP thePort) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
	int port = get_c_int(thePort);

	// call the function
	bool result = api->StartServer(port);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}

LISP prepare(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->Prepare(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP learn(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->Learn(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP increment(LISP id, LISP theInstanceString) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* instanceString = NULL;
	std::string instanceString = "";

	if (theInstanceString != NIL) {
		instanceString = get_c_string(theInstanceString);
	}

	// call the function
	bool result = api->Increment(instanceString);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP decrement(LISP id, LISP theInstanceString) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* instanceString = NULL;
	std::string instanceString = "";

	if (theInstanceString != NIL) {
		instanceString = get_c_string(theInstanceString);
	}

	// call the function
	bool result = api->Decrement(instanceString);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}



LISP expand(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->Expand(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP remove(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->Remove(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP test(LISP id, LISP theFileName, LISP theOutFileName, LISP thePercFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameters
//	const char* fileName = NULL;
//	const char* outFileName = NULL;
//	const char* percFileName = NULL;
	std::string fileName = "";
	std::string outFileName = "";
	std::string percFileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	if (theOutFileName != NIL) {
		outFileName = get_c_string(theOutFileName);
	}

	if (thePercFileName != NIL) {
		percFileName = get_c_string(thePercFileName);
	}

	// call the function
	bool result = api->Test(fileName, outFileName, percFileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP classify(LISP id, LISP theLine, LISP par1, LISP par2) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the right version of Classify depending on the available parameters
//	const char* line = NULL;
//	char* valueDistribution = NULL;
//	const char* result;
	double distance;
	std::string line = "";
	std::string valueDistribution = "";
	std::string result;

	if (par1 == NIL) {// variant with only the line parameter
		line = get_c_string(theLine);
		api->Classify(line, result);
	} else if (par2 == NIL) {// variant with line and distance parameters
		line = get_c_string(theLine);
		distance = get_c_double(par1);
		api->Classify(line, result, distance);

	} else {// variant with line, valueDistribution and distance parameters
		line = get_c_string(theLine);
		distance = get_c_double(par2);
		EST_String estString = get_c_string(par1);
		valueDistribution = estString.updatable_str();
		api->Classify(line, result, valueDistribution, distance);
	}

	// return to the SCHEME world
//	if (!result) {
//		result = "";
//	}
//	return strintern(result);

	return strintern(result.c_str());
}


LISP expname(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
//	const char* result = api->ExpName();
	std::string result = api->ExpName();

	// return to the SCHEME world
//	if (!result) {
//		return NIL;
//	} else {
//		return strintern(result);
//	}

	return strintern(result.c_str());
}

LISP version(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
//	int result = api->Version();
	std::string result = api->VersionInfo();

	// return to the SCHEME world
//	return flocons(result);
	return strintern(result.c_str());
}

/*
 * DISABLED, SEEMS TO BE GONE IN TIMBL5
LISP revision(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
	int result = api->Revision();

	// return to the SCHEME world
	return flocons(result);
}
*/


/*
 * DISABLED, SEEMS TO BE GONE IN TIMBL5
LISP revcmnt(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
	const char* result = api->RevCmnt();

	// return to the SCHEME world
	if (!result) {
		result = "";
	}
	return strintern(result);
}
*/

LISP saveWeights(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";
	
	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->SaveWeights(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}

LISP getWeights(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->GetWeights(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP writeInstanceBase(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->WriteInstanceBase(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP getInstanceBase(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->GetInstanceBase(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}

LISP writeArrays(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->WriteArrays(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP getArrays(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->GetArrays(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}

LISP writeNamesFile(LISP id, LISP theFileName) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* fileName = NULL;
	std::string fileName = "";

	if (theFileName != NIL) {
		fileName = get_c_string(theFileName);
	}

	// call the function
	bool result = api->WriteNamesFile(fileName);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}



LISP showOptions(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
	bool result = api->ShowOptions(cout);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}

LISP showSettings(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
	bool result = api->ShowSettings(cout);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP setOptions(LISP id, LISP theOptions) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// get the parameter
//	const char* options = "";
	std::string options = "";
	
	if (theOptions != NIL) {
		options = get_c_string(theOptions);
	}

	// call the function
	bool result = api->SetOptions(options);

	// return to the SCHEME world
	return result ? cintern("t") : NIL;
}


LISP defaultMaxFeats(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
	int result = api->Default_Max_Feats();

	// return to the SCHEME world
	return flocons(result);
}

/*
 * DISABLED, SEEMS TO BE GONE IN TIMBL5
LISP maxFeatNameLen(LISP id) {
	// convert the id to a TimblAPI object
	TimblAPI *api = timblapi(id);

	// call the function
	int result = api->Max_Feat_Name_Len();

	// return to the SCHEME world
	return flocons(result);
}
*/