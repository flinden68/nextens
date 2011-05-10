/*
 * This file contains the Scheme to C(++) coupling for the FonPars function
 *
 * Implemented for the NEXTENS project by www.plankton.nl
 */

#include "festival.h"
#include "siod.h"
#include "siod.h"

extern char* graphemeToPhoneme(char*);


/*
 * Conversion of the char* that comes from fonpars to a LISP cons structure
 *
 * the char* contains the following data:
 *
 * 		(syl-struct syl-struct .... syl-struct)
 *
 * a list of zero or more syl-structs between ()
 *
 *
 * a syl-struct looks like this:
 *
 *		((ph1 ph2 ph3...) 0)
 *
 * (( followed by space delimited phoneme strings followed bij )
 * followed by zero or more spaces followed by 0 or 1 followed by )
 */
LISP makeLisp(char* fonparsResult) {
	LISP result = NIL;
	int length = strlen(fonparsResult);
	// loop over the string and discard the first and last parenthesis
	for (int i = 1; i < length - 1;) {
		// look if another syl-struct starts here
		if (fonparsResult[i] != '(') {
			// no, look at the next character for a starting syl-struct
			i++;
		} else {
			// yes, discard second parenthesis
			int j = i + 2;

			// the LISP structure that will contain the syl-struct
			LISP sylstruct = NIL;

			// the LISP structure that will contain the phoneme list
			LISP pholist = NIL;

			// get all the phonemes that are delimited by space characters
			// this loop destroys the fonparsResult because it inserts '\0'
			// at the positions of the spaces and the closing bracket
			for(;;) {
				// skip the spaces between phonemes
				while (fonparsResult[j] == ' ') {
					j++;
				}

				// the fonemes end at the closing bracket
				if (fonparsResult[j] == ')') {
					break;
				}

				int phon = j;
				while (fonparsResult[j] != ' ' && fonparsResult[j] != ')') {
					j++;
				}

				// remember first non phoneme character (space or closing bracjet)
				char nonpho = fonparsResult[j];

				// place string terminator \0 after the phoneme
				fonparsResult[j] = '\0';

				// add the phoneme to the phoneme list
				pholist = cons(rintern(&fonparsResult[phon]), pholist);

				// continue if needed at the position behind the nonpho character
				if (nonpho != ')') {
					j++;
				} else {
					break;
				}
			}
			// add the phoneme list to the syl-struct
			sylstruct = cons(reverse(pholist), sylstruct);

			// look for 1 or 0, skip all other characters
			while (fonparsResult[j] != '0' && fonparsResult[j] != '1') {
				j++;
			}
			// add the stress info to the sylstruct
			if (fonparsResult[j] == '0') {
				sylstruct = cons(flocons(0), sylstruct);
			} else {
				sylstruct = cons(flocons(1), sylstruct);
			}

			// add the syl-struct to the result object
			result = cons(reverse(sylstruct), result);

			// the next position to look for a syl-struct
			i = j + 2;
		}
	}

	return reverse(result);
}


/*
 * This method is called when the 'FonPars' function is used in the Scheme domain
 */
LISP fonpars(LISP utt) {
	// convert the LISP object to an EST_String
	EST_String estString = get_c_string(utt);

	// get the char* to the actual characters from the string
	char* graphemes = estString.updatable_str();

	// call the function that converts the grapheme form into phoneme form from the FONPARS library
	char* result = graphemeToPhoneme(graphemes);

	return makeLisp(result);
}


/*
 * Binding of the 'FonPars' function from the Scheme domain to the fonpars method
 */
void festival_NextensFonpars_init(void) {
	proclaim_module("NextensFonpars");

	festival_def_utt_module("Fonpars", fonpars, "grapheme to phoneme converter based on FONPARS");
}
