;;; $Id: net_nl_lts_treetalk.scm,v 1.7 2005/10/21 07:03:13 emarsi Exp $
;;;
;;; by Erwin marsi & Joop Kerkhoff
;;; for the NeXTenS project
;;;
;;; Copyright (c) 2003
;;; ILK - Tilburg University
;;; L&S - University of Nijmegen
;;; Stichting Spraaktechnologie
;;;
;;; All rights Reserved.
;;;
;;; See the files NEXTENS.COPYING and NEXTENS.LICENSE 
;;; for information on usage and redistribution of this file, 
;;; and for a DISCLAIMER OF ALL WARRANTIES.


;;; TreeTalk

(require 'net_nl_timbl)


;;; ------------------------------------------------------------
;;; Start Timbl instances for TreeTalk
;;; ------------------------------------------------------------

(define (nl::init_treetalk)
"
\(nl::init_treetalk\)

Initialize TreeTalk by reading the Timbl instance bases
and setting Timbl options
\(see nl::phon_timbl and nl::syl_timbl\)
" 
  ;; Start a new Timbl instance for grapheme-to-phoneme conversion, and read the igtree.
  ;; The previously assigned Timbl instance (if any)
  ;; will be automatically destructed by garbage collection 
  (set! nl::phon_timbl
	(Timbl "-a1 +vs"))
  (Timbl.get_instance_base nl::phon_timbl 
			   (path-append libdir "net_nl/treetalk-ibs/phon.igtree")) 

  ;; Start a new Timbl instance for syllabification, and read the igtree.
  ;; The previously assigned Timbl instance (if any)
  ;; will be automatically destructed by garbage collection 
  (set! nl::syl_timbl
	(Timbl "-a1 +vs"))
  (Timbl.get_instance_base nl::syl_timbl 
			   (path-append libdir "net_nl/treetalk-ibs/syl.igtree")))




;;; ------------------------------------------------------------
;;; TreeTalk core
;;; ------------------------------------------------------------

(define (nl::treetalk_lts word)
"
\(nl::treetalk_lts WORD\)

Performs grapheme-to-phoneme conversion and syllabicifaction for WORD
using the TreeTalk method. Syllabification is taken to include
prosodic word phrasing and stress assignment. Returns a lemma in the
same format as the default function lex.lookup.
"
  (if nl::trace_treetalk
      (begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "nl::treetalk for word \"%s\"\n" word)
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n")))
  (list word nil 
	(nl::syllabification 
	 (nl::graph_to_phon_conversion word))))


(define (nl::graph_to_phon_conversion word)
"
\(nl::graph_to_phon_conversion WORD\)

Perform grapheme-to-phoneme conversion for WORD. Returns a list of
phonemes.
"
;; depends on global vars nl::phon_padding, nl::phon_timbl, and
;; nl::phon_win_size
  (if nl::trace_treetalk
      (format t "*** Starting grapheme-to-phoneme conversion ***\n\n"))
  (let (;; letters is the list of all *lowercased* letters in the word
	(letters (symbolexplode (downcase word)))
	(start 0)
	(features "")
	len
	phon
	phonemes)
    (set! len (length letters)) ; size of the word
    ;; add padding before and after the word, where the padding is a
    ;; list of padding symbols whose lenght depends on the window size
    (set! letters (append nl::phon_padding 
			  letters 
			  nl::phon_padding))
    ;; move a window over the list of letters
    (while (< start len)
	   ;; Features is the string obtained by concatenating all
	   ;; letters/paddings symbols in the window.  A dummy class
	   ;; ('?') is appended, because this expected by Timbl
	   (set! features (string-append (nl::slice letters 
						start (+ start nl::phon_win_size))
					 " ?"))
	   ;; call Timbl with these features, which return the phoneme
	   ;; corresponding to the letter in the center of the window
	   (if nl::trace_treetalk
	       (format t "Calling Timbl with features %s " features))
	   (set! phon (intern (Timbl.classify nl::phon_timbl features)))
	   (if nl::trace_treetalk
	       (format t "returns class %s\n" phon))
	   (set! phonemes (append phonemes (list phon)))
	   (set! start (+ start 1)))
    ;; Finally, expand compressed phonemes to the original pair of
    ;; phonemes (this means an extra loop over all phonemes; can be
    ;; avoided by integrating expand_phonemes in the current function)
    (nl::expand_phonemes phonemes)))


(define (nl::expand_phonemes phonemes)
"
\(nl::expand_phonemes PHONEMES\)

Expands compressed phonemes in the list PHONEMES to the original pair
of phonemes. The mapping is defined is nl::expansion_map.
"
  (let (result)
    (mapcar
     (lambda (phon)
       ;; unless phoneme is empty 
       (if (not (string-equal phon nl::empty_sym))
	   ;; if phoneme has a key in nl::expansion_map, then append
	   ;; its corresponding value (i.e. a pair of phonemes) to the
	   ;; result list, else append the phoneme itself to the
	   ;; result list
	   (set! result (append result
				(or (cdr (assoc_string phon nl::expansion_map))
				    (list phon))))))
     phonemes)
    (if  nl::trace_treetalk
	 (format t "\nExpanding phonemes in %l\ngives %l\n\n" phonemes result)) 
    result))


(define (nl::syllabification phonemes)
"
\(nl::syllabification PHONEMES\)

Perform syllabification, which is taken to include prosodic word
phrasing and stress assignment, on the list of PHONEMES. Returns a
list of one or more prosodic words, which in turn consists of one or
more syllables.
"
;; Example: 
;; (t a f @ l b o m)
;; returns 
;; ((((t a) 1) ((f @ l) 0)) (((b o m) 2)))
;; (Note: this function may be stated more compact by factoring out
;; some repeated code, but it would make things even less clear ...)
  (if nl::trace_treetalk
      (format t "*** Starting syllabification ***\n\n"))
  (let ((start 0)
	padded_phons
	features
	result
	bnd
	syl
	syl_phons
	pwrd
	phon
	vowel
	ill_formed)
    (set! len (length phonemes))
    ;; add padding before and after the phonemes, where the padding is a
    ;; list of padding symbols whose lenght depends on the window size
    (set! padded_phons (append nl::syl_padding phonemes nl::syl_padding))
    ;; move a window over the list of phonemes
    (while phonemes
	   (if nl::trace_treetalk
	       (begin
		 (format t "Stress of current syllable: %l\n" syl)
		 (format t "Phonemes of current syllable: %l\n" syl_phons)
		 (format t "Syllables of prosodic word: %l\n" pwrd)
		 (format t "Current output: %l\n\n" result)))
	   ;; Features is the string obtained by concatenating all
	   ;; phonemes/padding symbols in the window.  A dummy class
	   ;; ('?') is appended, because this expected by Timbl
	   (set! features (string-append (nl::slice padded_phons start 
						(+ start nl::syl_win_size))
					 " ?"))
	   ;; Call Timbl with these features, which returns the type
	   ;; of boundary (see below) corresponding to the letter in
	   ;; the center of the window
	   (if nl::trace_treetalk
	       (format t "Calling Timbl with features %s " features))
	   (set! bnd (Timbl.classify nl::syl_timbl features))
	   (if nl::trace_treetalk
	       (format t "returns class %s,\n" bnd))
	   ;; Unless there is no boundary at all (i.e. '+'),
	   ;; take action according to the type of boundary
	   (if (not (string-equal bnd '+))
	       (begin
		 ;; if we haven't seen a vowel yet,
		 ;; append a '-', so the postprocessing function
		 ;; knows that this is an ill-formed syllable without a vowel,
		 ;; and set the ill_formed flag
		 (if (and syl_phons (not vowel))
		     (begin
		       (set! syl (append syl '(-)))
		       (set! ill_formed t)))
		 ;; Construct a syllable by consing syl_phons (the
		 ;; list of collected phonemes) and syl (the last
		 ;; encountered stress level: 0, 1, or 2)(if vowel
		 (set! syl (cons syl_phons syl)) 
		 (cond
		 
		  ;; 0 --> syllable boundary (without stress)
		  ((string-equal bnd 0)
		   ;; add this syl to the current prosodic word
		   (set! pwrd (append pwrd (list syl)))
		   ;; start a new syl without stress
		   (set! syl '(0))
		   (if nl::trace_treetalk
		       (format t "which is an unstressed syllable boundary.\n")))
		  
		  ;; 1 --> syllable boundary with primary stress
		  ((string-equal bnd 1)
		   ;; add this syl to the current prosodic word
		   (set! pwrd (append pwrd (list syl)))
		   ;; start a new syl with primary stress
		   (set! syl '(1))
		   (if nl::trace_treetalk
		       (format t "which is a primary stressed syllable boundary.\n")))

		  ;; 2 --> syllable boundary with secundary stress
		  ((string-equal bnd 2)
		   ;; add this syl to the current prosodic word
		   (set! pwrd (append pwrd (list syl)))
		   ;; start a new syl with secundary stress
		   (set! syl '(2))
		   (if nl::trace_treetalk
		       (format t "which is a secundary stressed syllable boundary.\n")))

		  ;; 3 --> both prosodic word and syllable boundary (without stress)
		  ((string-equal bnd 3)
		   ;; unless we are at the start of the word (no phonemes yet)
		   (if syl_phons
		       (begin
			 ;; add this syl to the current prosodic word
			 (set! pwrd (append pwrd (list syl)))
			 ;; add this prosodic word to the current result
			 (set! result (append result (list pwrd)))
			 ;; start a new prosodic word
			 (set! pwrd nil)))
		   ;; start a new syl without stress
		   (set! syl '(0))
		   (set! compound t)
		   (if nl::trace_treetalk
		       (begin
			 (format t "which is both an unstressed syllable boundary\n")
			 (format t "and a prosodic word boundary.\n"))))

		  ;; 4 --> both prosodic word and syllable boundary 
		  ;;       with primary stress
		  ((string-equal bnd 4)
		   ;; unless we are at the start of the word (no phonemes yet)
		   (if syl_phons
		       (begin
			 ;; add this syl to the current prosodic word
			 (set! pwrd (append pwrd (list syl)))
			 ;; add this prosodic word to the current result
			 (set! result (append result (list pwrd)))
			 ;; start a new prosodic word
			 (set! pwrd nil)))
		   ;; start a new syl with primary stress
		   (set! syl '(1))
		   (if nl::trace_treetalk
		       (begin
			 (format t "which is both a primary stressed syllable boundary\n")
			 (format t "and a prosodic word boundary.\n"))))

		  ;; 5 --> both prosodic word and syllable boundary 
		  ;;       with secundary stress
		  ((string-equal bnd 5)
		   ;; unless we are at the start of the word (no phonemes yet)
		   (if syl_phons
		       (begin
			 ;; add this syl to the current prosodic word
			 (set! pwrd (append pwrd (list syl)))
			 ;; add this prosodic word to the current result
			 (set! result (append result (list pwrd)))
			 ;; start a new prosodic word
			 (set! pwrd nil)))
		   ;; start a new syllable with secundary stress
		   (set! syl '(2))
		   (if nl::trace_treetalk
		       (begin
			 (format t "which is both a secundary stressed syllable boundary\n")
			 (format t "and a prosodic word boundary.\n")))) 
		  )
		 ;; in any case, empty the list of collected phonemes
		 ;; and reset "I've seen a vowel" flag
		 (set! syl_phons nil)
		 (set! vowel nil)
		 )
	       (if nl::trace_treetalk
		       (format t "which is not a boundary.\n"))
	       )
	   ;; add current phoneme to phonemes of current syllable
	   (set! phon (car phonemes))
	   (if (nl::is_vowel phon)
	       ;; so the current phoneme is a vowel
	       ;; If we have already seen a vowel
	       (if vowel
		   ;; mark the syllable with a '+', so the postprocessing
		   ;; function knows that this is an ill-formed syllable,
		   ;; and set the ill_formed flag
		   (begin
		     (set! syl (append syl '(+)))
		     (set! ill_formed t))
		   ;; otherwise, toggle the "I've seen a vowel" flag
		   (set! vowel t)))
	   (set! phonemes (cdr phonemes))		
	   (set! syl_phons (append syl_phons (list phon)))			      
	   (set! start (+ start 1)))
    ;; Flush remaining stuff: 
    ;; first, construct the final syllable by consing syl_phons (the list of
    ;; collected phonemes) and syl (the last encountered stress level)
    (set! syl (cons syl_phons syl)) 
    ;; next, add final syl to the last prosodic word
    (set! pwrd (append pwrd (list syl)))
    ;; finally, add last prosodic word to the current result
    (set! result (append result (list pwrd)))

    (if nl::trace_treetalk
	(begin
	  (format t "Stress of current syllable: %l\n" syl)
	  (format t "Phonemes of current syllable: %l\n" syl_phons)
	  (format t "Syllables of prosodic word: %l\n\n" pwrd)
	  (format t "Output of syllabification:\n%l\n\n" result)))

    (if nl::treetalk_postproc
	(begin
	  (if ill_formed
	      (set! result (nl::correct_segmentation result)))
	  (nl::correct_stress result))
	result)))



;;; ------------------------------------------------------------
;;; TreeTalk postprocessing
;;; ------------------------------------------------------------

;; Yes, postprocessing can certainly be implemented in a more efficient way.
;; However, that would *really* turn it into spaghetti code :-)


(define (nl::correct_segmentation result)
  (nl::correct_syllables
   (nl::correct_prosodic_words result)))


(define (nl::correct_prosodic_words result)
"
\(nl::correct_prosodic_words RESULT\)

RESULT is an ill-formed output of Treetalk.
The function will correct those cases in which a prosodic word 
consists of a single syllable that lacks a vowel.
If this error occurs in the first prosodic word, 
it will be merged with the next one.
If it occures in an non-initial prosodic word,
it will be merged with previous one.

Example 'klapschaats':

festival> (nl::correct_prosodic_words '\(\(\(\(k l A p\) 1\)\) \(\(\(s\) 2 -\)\) \(\(\(x a t s\) 2\)\)\) \)
\(\(\(\(k l A p\) 1\) \(\(s) 2 -\)\) \(\(\(x a t s\) 2\)\)\)
"
  (let ((new_result '())
	pwrd1
	pwrd2)
    (if nl::trace_treetalk
	(begin
	  (format t "*** Starting prosodic word correction ***\n\n")
	  (format t "Input: %l\n\n" result)))

    ;; iterate over pairs of prosodic words
    (while result
	   (set! pwrd1 (car result))
	   (set! result (cdr result))
	   (set! pwrd2 (car result))
	   (set! result (cdr result))
	   (cond
	    ;; We are at the last prosword, 
	    ;; or we had just one to begin with
	    ((not pwrd2)
	     (set! new_result (append new_result (list pwrd1))))

	    ;; The 1st prosword has just on syllable,
	    ;; and that syllable lacks a vowel ==>
	    ;; merge this prosword with the next one.
	    ;; Note that if the prosword contains 2 syllables, 
	    ;; this type of error will be taken care of by "correct_syllables"
	    ((and (equal? (length pwrd1) 1)
		  (string-equal (car (last (car pwrd1))) "-"))
	     (set! result (cons (append pwrd1 pwrd2) result))
	     (if nl::trace_treetalk
		 (begin
		   (format t "Prosodic word %l has only 1 syllable\n" pwrd1)
		   (format t "and that syllable lacks a vowel.\n")
		   (format t "Merging prosodic words %l and %l\n" pwrd1 pwrd2)
		   (format t "into %l\n\n" (car result)))))

	    ;; The 2nd prosword has just one syllable,
	    ;; and that syllable lacks a vowel ==>
	    ;; merge this prosword with the preceding one.
	    ;; Note that if the prosword contains 2 syllables, 
	    ;; this type of error will be taken care of by "correct_syllables"
	    ((and (equal? (length pwrd2) 1)
		  (string-equal (car (last (car pwrd2))) "-"))
	     (set! result (cons (append pwrd1 pwrd2) result)) 
	     (if nl::trace_treetalk
		 (begin
		   (format t "Prosodic word %l has only 1 syllable\n" pwrd2)
		   (format t "and that syllable lacks a vowel.\n")
		   (format t "Merging prosodic words %l and %l\n" pwrd1 pwrd2)
		   (format t "into %l\n\n" (car result)))))

	    ;; Both proswords are well-formed
	    (t
	     (set! new_result (append new_result (list pwrd1)))
	     (set! result (cons pwrd2 result)))
	    ))

    (if nl::trace_treetalk
	(format t "Output of prosodic word correction: %l\n\n" new_result))

    new_result))



(define (nl::correct_syllables result)
"
\(nl::correct_syllables RESULT\)

RESULT is an ill-formed output of Treetalk.
The function will correct those cases in which a syllable 
either lacks a vowel or contains two vowels.
If the first syllable in the a prosodic word lacks a vowel, 
it will be merged with the next one.
If a non-initial syllable lacks a vowel,
it is merged with the preceding one.
If a syllable contains two vowels,
it will be split in two syllables
\(see nl::split_syllable SYL\)

Example 'ruimtestoet':

\(nl::correct_syllables '\(\(\(\(r Y+ m\) 1\)\) \(\(\(t E s t u t\) 2 +\)\)\) \)
\(\(\(\(r Y+ m\) 1\)\) \(\(\(t E\) 2\) \(\(s t u t\) 0\)\)\)
"
  (let ((new_result '())
	new_pwrd
	new_syl
	pwrd
	syl)

    (if nl::trace_treetalk
	(begin
	  (format t "*** Starting syllable correction ***\n\n")
	  (format t "Input: %l\n\n" result)))

    ;; for all prosodic words in result 
    (while result
	   (set! pwrd (car result))
	   (set! new_pwrd '())

	   ;; iterate over pairs of syllables in the prosodic word
	   (while pwrd
		  (set! syl1 (car pwrd))
		  (set! pwrd (cdr pwrd))
		  (set! syl2 (car pwrd))
		  (set! pwrd (cdr pwrd))
		  (cond

		   ;; the 1st syllable contains two vowels
		   ((string-equal (car (last syl1)) "+")
		    (if nl::trace_treetalk
			(format t "Syllable %l contains two vowels\n\n" syl1))
		    (set! new_pwrd (append new_pwrd 
					   (nl::split_syllable syl1)))
		    (if syl2
			(set! pwrd (cons syl2 pwrd))))

		   ;; the 1st syllable is the last (or only) syllable
		   ;; it can never be lack a vowel, 
		   ;; as this has already been corrected by correct_prosodic_words
		   ((not syl2)
		    (set! new_pwrd (append new_pwrd (list syl1))))

		   ;; the 1st syllable lacks a vowel ==>
		   ;; merge syllables and take stress from 2nd syllable
		   ((string-equal (car (last syl1)) "-")
		    (set! new_syl (list (append (car syl1) (car syl2)) 
					(cadr syl2)))
		    (set! pwrd (cons new_syl pwrd))
		    (if nl::trace_treetalk
			(begin 
			  (format t "Syllable %l lacks a vowel\n" syl1)
			  (format t "Merging syllables %l and %l into %l\n\n" 
				  syl1 syl2 new_syl))))

		   ;; the 2nd syllable lacks a vowel ==>
		   ;; merge syllables and take stress from 1st syllable
		   ((string-equal (car (last syl2)) "-")
		    (set! new_syl (list (append (car syl1) (car syl2))
					(cadr syl1)))
		    (set! pwrd (cons new_syl pwrd))
		    (if nl::trace_treetalk
			(begin 
			  (format t "Syllable %l lacks a vowel\n" syl2)
			  (format t "Merging syllables %l and %l into %l\n\n" 
				  syl1 syl2 new_syl))))

		   ;; both syllables are well-formed
		   (t
		    (set! new_pwrd (append new_pwrd (list syl1)))
		    (set! pwrd (cons syl2 pwrd)))
		   ))

	   (set! new_result (append new_result (list new_pwrd)))
	   (set! result (cdr result)))

    (if nl::trace_treetalk
	(format t "Output of syllable correction: %l\n\n" new_result))

    new_result))


(define (nl::split_syllable syl)
" 
\(nl::split_syllable SYL\)

SYL must be an ill-formed syllable that contains two vowels,
separated by zero or more consonants.
The function returns a split of SYL into a pair of syllables,
where the consonants are optimally divided over the coda of 
the first syllable and the onset of the second syllable,
using a frequency-based score.
\(see also nl::score_onset_coda\)

Example:

festival> \(nl::split_syllable '\(\(r E x t s p l e\) 1\) \)
\(\(\(r E x t s\) 1\) \(\(p l e\) 0\)\)

Remarks:
- The stress of the first syllable is copied from the input;
  the stress of the second syllable is always zero.
- The function doesn't care if the input syllable contains 
  an extra + or - after the stress level, as used for marking 
  ill-formed syllables. 
- Function is not robust against well-formed input :-)
  It will not terminate.
"
  (let ((syl_phons (car syl))
	(syl1_onset_nucl '())
	(syl1_coda '())
	(syl2_onset '())
	(syl2_nucl_coda '())
	(transition '())
	(coda_option '())
	(onset_option '())
	vowel_seen
	score
	syl_pair
	)
    (if nl::trace_treetalk
	(begin
	  (format t "* Starting syllable splitting *\n\n")
	  (format t "Input to syllable splitting: %l\n\n" syl)))

    ;; determine onset and nucleus of 1st syllable
    (while (not vowel_seen)
	   (set! syl1_onset_nucl (append syl1_onset_nucl
					 (list (car syl_phons))))
	   (if (nl::is_vowel (car syl_phons))
	       (set! vowel_seen t))
	   (set! syl_phons (cdr syl_phons)))

    ;; determine consonant transition between the two vowels,
    ;; store in onset_option
    (while (not (nl::is_vowel (car syl_phons)))
	   (set! transition (append transition
				    (list (car syl_phons))))
	   (set! syl_phons (cdr syl_phons)))
 
    ;; determine nucleus and coda of 2nd syllable
    ;; (just for clearity ;-)
    (set! syl2_nucl_coda syl_phons)

    ;; Find the optimal split of the consonants
    ;; into a coda for the 1st syllable and an onset for the 2nd syllable.
    ;; Initialize with all consonants as the coda of 1st syllable
    ;; (thus an empty onset for the 2nd sylable) 
    (set! syl1_coda transition)
    ;; Score this split.
    (set! best_score (nl::score_onset_coda syl1_coda syl2_onset)) 
    ;; Now try all other splits,
    ;; and keep the one with best score
    (set! onset_option transition)
    (while onset_option
	   (set! new_score (nl::score_onset_coda coda_option onset_option))
	   (if (> new_score best_score)
	       (begin
		 (set! syl1_coda coda_option)
		 (set! syl2_onset onset_option)
		 (set! best_score new_score)))
	   (set! coda_option (append coda_option 
				     (list (car onset_option))))
	   (set! onset_option (cdr onset_option)))

    (set! syl_pair 
	  (list 
	   ;; 1st syllable
	   (list (append syl1_onset_nucl syl1_coda) (cadr syl))
	   ;; 2nd syllable
	   ;; we guess that it's stress level is 0
	   (list (append syl2_onset syl2_nucl_coda) '0)))

    (if nl::trace_treetalk
	(format t "\nOutput of syllable splitting: %l\n\n" syl_pair))

    syl_pair))


(define (nl::score_onset_coda coda onset)
"
\(nl::score_onset_coda CODA ONSET\)

Returns a score for the combination of CODA and ONSET.
Both CODA and ONSET are a list of phonemes, possibly empty.
The score is calculated by multiplying the frequency of the CODA 
by the frequency of the ONSET.
Frequencies, as derived from the lexicon, are looked up in
the assoc lists nl::coda_frequencies and nl::onset_frequencies.
If not found, the frequency is asumd to be zero.

Example:

festival> \(nl::score_onset_coda '\(x t s\) '\(p l\)\)
3.06086e+06
"
  (let ((coda_freq (or (cadr (assoc (nl::list_to_string coda) nl::coda_frequencies)) 
		       0))
	(onset_freq (or (cadr (assoc (nl::list_to_string onset) nl::onset_frequencies)) 
			0))
	comb_freq)
    (set! comb_freq (* coda_freq onset_freq 1.0))
    (if nl::trace_treetalk
	(format t "Score for coda %l and onset %l: %d * %d = %.0f\n"
		coda onset coda_freq onset_freq comb_freq))
    comb_freq))



(define (nl::correct_stress result)
  (let ((stress1 0)
	(stress2 0)
	(compounds 0)
	(comp_stress '())
	(comp_no 0)
	syl_no
	stress
	new_syl)

    ;; take a first pass to gather some intelligence:
    ;; find out the total number or primary and secundary stresses,
    ;; as well as the number of compounds
    (mapcar 
     (lambda (comp)
       (mapcar
	(lambda (syl)
	  (set! stress (cadr syl))
	  (if (equal? stress 1)
	      (set! stress1 (+ stress1 1)))
	  (if (equal? stress 2)
	      (set! stress2 (+ stress2 1))))
	comp)
       (cond
	((> stress1 0)
	 (set! comp_stress (append comp_stress (list 1))))
	((> stress2 0)
	 (set! comp_stress (append comp_stress (list 2))))
	(t
	 (set! comp_stress (append comp_stress (list 0)))))
       (set! compounds (+ compounds 1)))
     result)
        
    (cond
     ;; ------------------------------------------------------------
     ;; non-compound with incorrect stress
     ;; ------------------------------------------------------------
     ((and (equal? compounds 1)
	   (or (not (equal? stress1 1))
	       (not (equal? stress2 0))))
      (if nl::trace_treetalk
	  (format t "*** Starting stress correction for non-compound ***\n\n"))
      (set! result 
	    (mapcar
	     (lambda (comp)
	       (set! syl_no 0)
	       (mapcar
		(lambda (syl)
		  (set! syl_no (+ syl_no 1))
		  (set! stress (cadr syl))
		  (cond
		   
		   ;; Rule 1: 
		   ;; IF no primary stress at all
		   ;; THEN put primary stress on the first syllable
		   ((and (equal? stress1 0)          ;; we need a primary stress
			 (equal? stress2 0))         ;; but there are no secundary stress
		    (set! stress1 1)                 ;; so just stress the first syllable 
		    (set! new_syl (list (car syl) 1))
		    (if nl::trace_treetalk
			(format t "Rule 1: %l ==> %l\n" syl new_syl))
		    new_syl)
		   
		   ;; Rule 2:
		   ;; IF no primary stress at all AND
		   ;;    current is secundary stress
		   ;; THEN change current stress to primary stress
		   ((and (equal? stress1 0)          ;; we need a primary stress 
			 (equal? stress 2))          ;; and this syllable has secundary stress
		    (set! stress1 (+ stress1 1))     ;; so we change it to primary stress
		    (set! new_syl (list (car syl) 1))
		    (if nl::trace_treetalk
			(format t "Rule 2: %l ==> %l\n" syl new_syl))
		    new_syl)
		   
		   ;; Rule 3:
		   ;; IF secunday stress
		   ;; THEN change to no stress
		   ((equal? stress 2)                ;; this syllable has secundary stress
		    (set! new_syl (list (car syl) 0));; so we remove it
		    (set! stress2 (- stress2 1))
		    (if nl::trace_treetalk
			(format t "Rule 3: %l ==> %l\n" syl new_syl))
		    new_syl)
		   
		   (t                                ;; keep this primary or none stress
		    syl)))
		comp))
	     result))
      (if nl::trace_treetalk
	  (format t "\nOutput of stress correction:\n%l\n\n" result))
      result)
	
     ;; ------------------------------------------------------------
     ;; compound with incorrect stress
     ;; ------------------------------------------------------------
     ((and (> compounds 1)
	   (or (not (equal? stress1 1))
	       (not (equal? stress2 1))))		    
      (if nl::trace_treetalk
	  (format t "*** Starting stress correction in compound ***\n\n"))
      (set! result
      (mapcar 
       (lambda (comp)
	 (set! comp_no (+ comp_no 1))
	 (mapcar
	  (lambda (syl)
	    (set! stress (cadr syl))
	    (cond
	     
	     ;; Rule 2b
	     ;; IF current syllable has secundary stress AND
	     ;;    word has no primary stress yet AND 
	     ;;    word has more than one secundary stress AND 
	     ;; THEN change secundary to primary on current syllable
	     ((and (equal? stress 2)
		   (equal? stress1 0)
		   (> stress2 1))
	      (set! stress1 1)
	      (set! stress2 (- stress2 1))
	      (set! new_syl (list (car syl) 1))
	      (if nl::trace_treetalk
		  (format t "Rule 2b: %l ==> %l\n" syl new_syl))
	      new_syl)

	     ;; Rule 5
	     ;; IF no secundary stress yet AND 
	     ;;    one primary stress AND 
	     ;;    current compound has no stress
	     ;; THEN put secundary stress on the first syllable of this compound 
	     ((and (equal? stress2 0)
		   (equal? stress1 1)
		   (equal? (nth (- comp_no 1) comp_stress) 0))
	      (set! stress2 1)
	      ;; FIX-ME: updating comp_stress required?
	      (set! new_syl (list (car syl) 2))
	      (if nl::trace_treetalk
		  (format t "Rule 5: %l ==> %l\n" syl new_syl))
	      new_syl)
	     
	     ;; Rule 6 
	     ;; IF no primary stress yet AND
	     ;;    one secundary stress AND
	     ;;    current compound has no stress
	     ;; THEN  put primary stress on the first syllable of this compound
	     ((and (equal? stress1 0)
		   (equal? stress2 1)
		   (equal? (nth (- comp_no 1) comp_stress) 0))
	      (set! stress1 1)
	      ;; FIX-ME: updating comp_stress required?
	      (set! new_syl (list (car syl) 1))
	      (if nl::trace_treetalk
		  (format t "Rule 6: %l ==> %l\n" syl new_syl))
	      new_syl)
	     
	     ;; Rule 7
	     ;; IF current syllable has primary stress AND
	     ;;    two primary stresses in word AND
	     ;;    (no secundary stress yet AND)
	     ;;    remainder of the word contains no primary stress
	     ;; THEN  change primary to secundary on current syllable
	     ((and (equal? stress 1)
		   (equal? stress1 2)
					;(equal? stress2 0)
		   (not (member_string 1 (nth_cdr comp_no comp_stress))))
	      (set! stress1 1)
	      (set! stress2 1)
	      ;; FIX-ME: updating comp_stress required?
	      (set! new_syl (list (car syl) 2))
	      (if nl::trace_treetalk
		  (format t "Rule 7: %l ==> %l\n" syl new_syl))
	      new_syl)
	     
	     ;; Rule 8
	     ;; IF current syllable has secundary stress AND
	     ;;    word has more than one secundary stress
	     ;; THEN delete stress on current syllable
	     ((and (equal? stress 2)
		   (or (> stress1 1)
		       (> stress2 1)))
	      (set! stress2 (- stress2 1))
	      (set! new_syl (list (car syl) 0))
	      (if nl::trace_treetalk
		  (format t "Rule 8: %l ==> %l\n" syl new_syl))
	      new_syl)
	     
	     (t                                ;; keep this syllable
	      syl)))
	  comp))       
       result))

      (if nl::trace_treetalk
	  (format t "\nOutput of stress correction:\n%l\n\n" result))
     result)

     ;; ------------------------------------------------------------
     ;; stress is correct
     ;; ------------------------------------------------------------
     (t result)
     )
    ))
   


;;; ------------------------------------------------------------
;;; Support functions
;;; ------------------------------------------------------------

(define (nl::is_vowel phon)
"
\(nl::is_vowel PHON\)

Return t if phoneme PHON is a vowel, nil oherwise.
"
  (member_string phon
		 '(@ A E I O Y i u y a e o 2 E+ Y+ A+ E: Y: O: E~ A~ O~ Y~)))

;;; ------------------------------------------------------------
;;; Global variables 
;;; ------------------------------------------------------------

;;; (Note: using global vars to speed up processing, hopefully...)

(defvar nl::treetalk_postproc t
"
If true, perform rule-based postprocessing of treetalk output
to correct some common errors. See nl::treetalk.
")


(defvar nl::trace_treetalk nil
"
If true, trace grapheme-to_phoneme conversion and syllabification
in TreeTalk. See nl::treetalk.
")

(defvar nl::expansion_map
      '((1 i j)       ;; malaria /malarija/
	(3 e j)       ;; ateist /atejIst/
	(4 p j)       ;; computer /kOmpjut@r/
	(5 A j)       ;; flamboyant /flAmbwAjAnt/
	(6 d j)       ;; managen /mEn@dj@n/
	(7 y j)       ;; fonduen fOndyj@n
	(8 k j)       ;; barbecuen /bArb@kjuw@n/
	(9 y w)       ;; actuele /Aktywel@/
	(# o w)       ;; asteroide /Ast@rowid@/
	({ u w)       ;; barbecue /bArb@kjuw/
	(} k s)       ;; axon /AksOn/
	([ t s)       ;; martiale /mArts3al@/
	(] t S)       ;; helices /helitSEs/
	(| d Z))
"
Assoc list that defines the mapping from compressed phonemes to the
original pair of phonemes. See nl::expand_phonemes.
")


(defvar nl::empty_sym '-
"
The symbol for empty phones (epsilons). See nl::expand_phonemes.
")


(defvar nl::pad_sym '_
"
The symbol used for padding a window when windowing letters/phonemes
at the begin and end of a word. See nl::phon_padding and
nl::syl_padding.
")


(defvar nl::phon_win_size 9
"
The size of window used for windowing letters during
graheme-to-phoneme conversion. See nl::graph_to_phon_conversion
")

(defvar nl::phon_padding 
  (nl::construct_padding_list nl::phon_win_size
			      nl::pad_sym)
" 
The list of padding symbols to be appended before and after the word
during graheme-to-phoneme conversion. See nl::graph_to_phon_conversion
")


(defvar nl::syl_win_size 9
" 
The size of window used for windowing letters during
syllabification. See nl::syllabification
")

(defvar nl::syl_padding
  (nl::construct_padding_list nl::syl_win_size
			      nl::pad_sym)
"
The list of padding symbols to be appended before and after the word
during syllabification. See nl::syllabification
")


(defvar nl::onset_frequencies '(
   ("Zj" 1)
   ("klw" 1)
   ("pf" 1)
   ("sfr" 1)
   ("tl" 1)
   ("tm" 1)
   ("tsw" 1)
   ("drw" 2)
   ("ft" 2)
   ("fw" 2)
   ("rj" 2)
   ("tSj" 2)
   ("tsj" 2)
   ("Sw" 3)
   ("dz" 3)
   ("skl" 3)
   ("mn" 4)
   ("krw" 5)
   ("pt" 5)
   ("sj" 5)
   ("Sr" 6)
   ("bj" 6)
   ("lj" 6)
   ("nw" 6)
   ("Sp" 8)
   ("Zw" 8)
   ("skw" 9)
   ("frw" 10)
   ("lw" 10)
   ("stj" 10)
   ("Sl" 12)
   ("St" 12)
   ("Gj" 15)
   ("bw" 15)
   ("fn" 15)
   ("rw" 17)
   ("mw" 19)
   ("Sn" 20)
   ("Sm" 27)
   ("mj" 27)
   ("xn" 33)
   ("pw" 34)
   ("pn" 35)
   ("gw" 43)
   ("vj" 43)
   ("vw" 43)
   ("fj" 49)
   ("Gn" 50)
   ("plw" 52)
   ("jn" 55)
   ("gr" 58)
   ("ks" 79)
   ("gl" 86)
   ("sf" 127)
   ("kj" 147)
   ("dj" 191)
   ("skr" 198)
   ("spl" 251)
   ("pj" 349)
   ("nj" 386)
   ("ps" 391)
   ("g" 407)
   ("sw" 452)
   ("Gl" 496)
   ("dw" 559)
   ("wr" 645)
   ("tw" 685)
   ("tS" 808)
   ("sk" 915)
   ("xl" 1096)
   ("sm" 1443)
   ("spr" 1665)
   ("Z" 1896)
   ("zw" 2055)
   ("sn" 2063)
   ("sxr" 2182)
   ("fr" 2227)
   ("kn" 2274)
   ("vl" 2487)
   ("vr" 2545)
   ("kw" 2649)
   ("fl" 2824)
   ("Gr" 3201)
   ("xr" 3786)
   ("S" 3861)
   ("str" 4507)
   ("bl" 4994)
   ("ts" 5135)
   ("sl" 5447)
   ("br" 5634)
   ("pl" 5909)
   ("tj" 6168)
   ("dr" 6408)
   ("sp" 6579)
   ("kl" 7258)
   ("N" 7474)
   ("pr" 7502)
   ("kr" 7559)
   ("tr" 9018)
   ("sx" 12735)
   ("f" 20424)
   ("j" 21186)
   ("st" 24353)
   ("h" 28096)
   ("w" 28559)
   ("x" 30211)
   ("z" 31165)
   ("G" 34065)
   ("p" 37104)
   ("n" 39176)
   ("s" 43651)
   ("b" 46233)
   ("v" 47543)
   ("m" 49049)
   ("k" 64607)
   ("l" 66984)
   ("r" 72008)
   ("" 83056)
   ("d" 98084)
   ("t" 100817))
  "
An assoc list of syllable onsets and their frequencies in the lexicon
"
  )

(defvar nl::coda_frequencies '( 
   ("Ntst" 1)
   ("jks" 1)
   ("jls" 1)
   ("ktt" 1)
   ("ktz" 1)
   ("lfts" 1)
   ("lmtst" 1)
   ("lpst" 1)
   ("lxtst" 1)
   ("ml" 1)
   ("pf" 1)
   ("pz" 1)
   ("rmtst" 1)
   ("rntst" 1)
   ("rwst" 1)
   ("rxst" 1)
   ("sn" 1)
   ("snt" 1)
   ("sps" 1)
   ("sptst" 1)
   ("tl" 1)
   ("tls" 1)
   ("vs" 1)
   ("xtt" 1)
   ("xtz" 1)
   ("Nktst" 2)
   ("bd" 2)
   ("fz" 2)
   ("gs" 2)
   ("jmt" 2)
   ("jnz" 2)
   ("kd" 2)
   ("kn" 2)
   ("knt" 2)
   ("kz" 2)
   ("lftst" 2)
   ("lkst" 2)
   ("lvdz" 2)
   ("mptst" 2)
   ("pts" 2)
   ("rb" 2)
   ("rftst" 2)
   ("rlt" 2)
   ("skt" 2)
   ("jk" 3)
   ("jp" 3)
   ("jz" 3)
   ("lmst" 3)
   ("mSt" 3)
   ("ntr" 3)
   ("rgd" 3)
   ("Gzd" 4)
   ("Nzd" 4)
   ("gdz" 4)
   ("jv" 4)
   ("lps" 4)
   ("lzd" 4)
   ("mpst" 4)
   ("mv" 4)
   ("nstst" 4)
   ("rfz" 4)
   ("rktst" 4)
   ("rwt" 4)
   ("rxtst" 4)
   ("sks" 4)
   ("J" 5)
   ("mzd" 5)
   ("rnst" 5)
   ("wtst" 5)
   ("jfs" 6)
   ("jnts" 6)
   ("lktst" 6)
   ("lvz" 6)
   ("nSt" 6)
   ("rkst" 6)
   ("rls" 6)
   ("rxs" 6)
   ("skst" 6)
   ("wd" 6)
   ("xd" 6)
   ("gzd" 7)
   ("jd" 7)
   ("jf" 7)
   ("jns" 7)
   ("jts" 7)
   ("mS" 7)
   ("mps" 7)
   ("rpst" 7)
   ("rvzd" 7)
   ("St" 8)
   ("jnt" 8)
   ("jtst" 8)
   ("rbz" 8)
   ("zg" 8)
   ("lb" 9)
   ("Ngz" 10)
   ("ldz" 10)
   ("ntS" 10)
   ("mb" 11)
   ("rfs" 11)
   ("jst" 12)
   ("mdz" 12)
   ("ds" 13)
   ("ptst" 13)
   ("wz" 13)
   ("Nkst" 14)
   ("dj" 14)
   ("tSt" 14)
   ("jm" 15)
   ("rl" 15)
   ("rms" 15)
   ("vdz" 15)
   ("md" 16)
   ("rw" 16)
   ("tz" 16)
   ("stst" 17)
   ("jn" 20)
   ("rN" 20)
   ("rmst" 20)
   ("mz" 22)
   ("kts" 23)
   ("mtst" 23)
   ("rzd" 23)
   ("spt" 23)
   ("rtS" 26)
   ("ftst" 27)
   ("gd" 27)
   ("rks" 28)
   ("lxt" 29)
   ("vz" 29)
   ("pst" 31)
   ("rnt" 31)
   ("rS" 33)
   ("sts" 33)
   ("lg" 34)
   ("mf" 35)
   ("fts" 36)
   ("lgz" 38)
   ("lts" 40)
   ("rdz" 40)
   ("lpt" 43)
   ("js" 44)
   ("Gz" 45)
   ("lmt" 45)
   ("mts" 46)
   ("ltst" 49)
   ("wst" 49)
   ("rfst" 50)
   ("lft" 52)
   ("nzd" 55)
   ("sk" 55)
   ("lfs" 58)
   ("rxt" 62)
   ("rpt" 64)
   ("lkt" 67)
   ("nS" 67)
   ("rps" 69)
   ("ktst" 75)
   ("lms" 77)
   ("Nks" 81)
   ("xtst" 82)
   ("tS" 83)
   ("ndz" 85)
   ("ws" 87)
   ("lG" 88)
   ("rmt" 89)
   ("sp" 90)
   ("Gdz" 91)
   ("rns" 96)
   ("mpt" 107)
   ("bz" 108)
   ("lx" 108)
   ("rft" 114)
   ("S" 115)
   ("rg" 125)
   ("gz" 149)
   ("Nst" 156)
   ("Nt" 160)
   ("rv" 167)
   ("fst" 168)
   ("lz" 185)
   ("lks" 191)
   ("Ng" 192)
   ("lv" 193)
   ("rG" 209)
   ("vd" 219)
   ("mst" 220)
   ("zd" 224)
   ("rtst" 233)
   ("Nkt" 286)
   ("rkt" 297)
   ("nz" 305)
   ("fs" 341)
   ("wt" 371)
   ("rz" 375)
   ("lp" 376)
   ("Gd" 386)
   ("Nz" 390)
   ("ld" 392)
   ("rx" 406)
   ("lst" 418)
   ("tst" 437)
   ("rp" 453)
   ("rn" 462)
   ("rf" 481)
   ("xts" 518)
   ("xs" 530)
   ("lm" 538)
   ("jt" 544)
   ("rts" 597)
   ("dz" 624)
   ("ntst" 641)
   ("nts" 691)
   ("rd" 711)
   ("lf" 716)
   ("lk" 753)
   ("rst" 762)
   ("ms" 764)
   ("kst" 772)
   ("mt" 812)
   ("ps" 835)
   ("b" 842)
   ("mp" 899)
   ("g" 1078)
   ("pt" 1087)
   ("rm" 1239)
   ("nst" 1351)
   ("ft" 1483)
   ("nd" 1633)
   ("rk" 1902)
   ("z" 1977)
   ("xst" 2050)
   ("ks" 2075)
   ("Nk" 2132)
   ("v" 2159)
   ("kt" 2470)
   ("j" 2488)
   ("Ns" 2905)
   ("d" 3014)
   ("w" 3015)
   ("ls" 3447)
   ("ts" 3659)
   ("ns" 4106)
   ("G" 4456)
   ("lt" 4678)
   ("st" 4846)
   ("xt" 5576)
   ("rt" 10912)
   ("rs" 11579)
   ("f" 12082)
   ("p" 12518)
   ("m" 18936)
   ("N" 20557)
   ("x" 20832)
   ("nt" 23628)
   ("k" 24998)
   ("t" 28848)
   ("l" 35875)
   ("s" 40399)
   ("r" 102093)
   ("n" 138595)
   ("" 518704))
"
An assoc list of syllable codas and their frequencies in the lexicon
"
)


(provide 'net_nl_lts_treetalk)


