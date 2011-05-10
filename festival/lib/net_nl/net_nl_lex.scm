;;; $Id: net_nl_lex.scm,v 1.9 2005/10/04 12:19:31 emarsi Exp $
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


;;; lexical look up, from words to pronunciations


;;; ----------------------------------------------------------------------------
;;; nl::word
;;; ----------------------------------------------------------------------------

(define (nl::word utt)
"
\(nl::word utt\) 

For each word in the relation Word of utt, build its prosodic
constituent structure, adding nodes to the Segment, SylPart, Syllable,
Foot, and ProsWord relations, using lexical lookup in the addenda, the
lexicon, and LTS conversion.
In addition, intonational events (pitch accents and boundary tones) are
associated with the appropriate syllables.

See also nl::associate_int_to_syls
"
  ;; add new relations for all lower prosodic constituents
  (utt.relation.create utt 'Segment)
  (utt.relation.create utt 'SylPart)
  (utt.relation.create utt 'Syllable)
  (utt.relation.create utt 'Foot)
  (utt.relation.create utt 'ProsWord1)
  (utt.relation.create utt 'ProsWord2)
  (utt.relation.create utt 'Word-Pros)
  ;; add new relation to store the prosodic tree 
  ;; (i.e. mother-daughter relations)
  (utt.relation.create utt 'ProsTree)
  ;; add a new relation to store intonation-to-syllable association
  (utt.relation.create utt 'Syl-Int)
  (let (result)
    ;; for all words in utterance:
    (mapcar
     (lambda (word)
       ;; FIX-ME: prevent lookup of tokens!
       ;; lookup the word plus its part-of-speech in the lexicon
       ;; if not found, the LTS method will be called
       (set! lemma (car (cdr (cdr (lex.lookup
				   (item.name word)
				   ;; hack needed to convert string to symbol :-(
				   (read-from-string 
				    (item.feat word "sense")))))))
       (nl::build_prosword_structure utt word lemma)
       (nl::associate_int_to_syls utt word))
     (utt.relation.items utt 'Word)))
  utt)


;;;-----------------------------------------------------------------------------
;;; Building the prosodic structure of words
;;;-----------------------------------------------------------------------------

(define (nl::build_prosword_structure utt wrd lemma)
"
\(nl::build_prosword_structure utt lemma\)

Build prosodic word structure (and all lower levels) for the given
lemma, e.g. \(\(\(\(w a:\) 1\) \(\(t @ r\) 0\)\) \(\(\(f i t s\) 2\)\)\)
"
  (let ((prosword1 (utt.relation.append utt 
					'ProsWord1  
					'(ProsWord1)))
	prosword2
	s)
    ;; The highest prosodic word becomes a daughter to the graphemic
    ;; word in relation Word-Pros, so we can still link the graphemic
    ;; and the phonological/prosodic form.
    (utt.relation.append utt 'Word-Pros wrd)
    (item.relation.append_daughter wrd 'Word-Pros prosword1)
    ;; Add top prosodic word to the prosodic tree
    (utt.relation.append utt 'ProsTree prosword1)
    ;; for all compounds/prosodic words in lemma
    (mapcar 
     (lambda (s)
       ;; Create a new prosdic word in the ProsWord2 relation
       ;; By default, it's metrically weak, 
       ;; but it may become strong during building of the foot structure
       ;; if we find out that it contains a primary stressed (1) syllable
       (set! prosword2
	     (utt.relation.append utt 'ProsWord2  '(ProsWord2 ((metrical weak)))))
       (item.relation.append_daughter prosword1 'ProsTree prosword2)	
       ;; build foot structure for this prosodic word
       (nl::build_foot_structure utt prosword2 s))
     lemma)))
    



(define (nl::build_foot_structure utt this_prosword_node prosword)
"
\(nl::build_foot_structure utt this_prosword_node prosword\)

Build foot structure (and all lower levels) for the given prosodic word, 
e.g. \(\(\(w a:\) 1\) \(\(t @ r\) 0\)\), and attach to the given node.
"  
  (let (sylstruc_info
	this_syl_node
	stressed_syl_node
	nr_prestress_syl
	foot_node
	app_node)
    ;; build syllable structure first,
    ;; and receive information about syllable structure
    (set! sylstruc_info (nl::build_syllable_structure utt prosword))
    ;; go to the first syllable
    (set! this_syl_node (car sylstruc_info))
    ;; get the stresses syllable node
    (set! stressed_syl_node (cadr sylstruc_info))

    ;; Some dirty side effect that doesn't belong here ;-)
    ;; If this is a primary stressed syllable (1)
    ;; then this prosodic word must become metrically strong
    (if (and stressed_syl_node
	     (equal? (item.feat stressed_syl_node 'stress) 1))
	(item.set_feat this_prosword_node 'metrical 'strong))

    ;; get nr. of sylbles preceding the stressed syllable
    (set! nr_prestress_syl (caddr sylstruc_info))
    ;; ------------------------------------------
    ;; step 1: attach any initial schwa syllables 
    ;; as appendix to prosodic word
    ;; ------------------------------------------
    ;; exception: if schwa is the only vowel in in the word, 
    ;; as in 't, 'n, etc. 
    (while (and this_syl_node
		(equal? (item.feat this_syl_node 'vowel) "@")
		(item.next this_syl_node))
	   (set! app_node 
		 (utt.relation.append utt
				      'Foot
				      (list 'Appendix)))
	   (item.relation.append_daughter this_prosword_node 'ProsTree app_node)
	   (item.relation.append_daughter app_node 'ProsTree this_syl_node)
	   ;; decrement counter of prestress syllables
	   (set! nr_prestress_syl (- nr_prestress_syl 1))
	   (set! this_syl_node (item.next this_syl_node)))
    ;; ----------------------------------------------------
    ;; step 2: build foot structure for all other syllables
    ;; preceding the stressed syllable
    ;; ----------------------------------------------------
    ;; CLEAN-ME: lot of redundancy in the code here
    (cond 
     ;; 2a: pre-stress syllable ==> 
     ;; appendix 
     ((equal? nr_prestress_syl 1)
      (set! app_node 
	    (utt.relation.append utt
				 'Foot
				 (list 'Appendix)))
      (item.relation.append_daughter this_prosword_node 'ProsTree app_node)
      (item.relation.append_daughter app_node 'ProsTree this_syl_node))
     ;; 2b: pre-stress syllables ==> 
     ;; binary foot
     ((equal? nr_prestress_syl 2)
      (begin
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node 
				       'ProsTree 
				       foot_node)
	(item.set_feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree 
				       this_syl_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))))
     ;; 3 pre-stress syllables ==> 
     ;; ternary foot
     ((equal? nr_prestress_syl 3)
      (begin
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(item.set_feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       this_syl_node)		
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next 
					(item.next this_syl_node)))))
     ;; 4 pre-stress syllables ==> 
     ;; 2 binary feet
     ((equal? nr_prestress_syl 4)
      (begin
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(item.set_feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       this_syl_node)			   
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(set! this_syl_node (item.next (item.next this_syl_node)))
	(item.set_feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       this_syl_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))))
     ;; 5 pre-stress syllables ==> 
     ;; appendix and 2 binary feet 
     ((equal? nr_prestress_syl 5)
      (begin
	(set! app_node 
	      (utt.relation.append utt
				   'Foot
				   (list 'Appendix)))
	(item.relation.append_daughter this_prosword_node 'ProsTree app_node)
	(item.relation.append_daughter app_node 'ProsTree this_syl_node)
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next 
					(item.next this_syl_node)))
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(set! this_syl_node (item.next (item.next (item.next this_syl_node))))
	(item.feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree this_syl_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))))
     ;; 6 pre-stress syllables ==> 
     ;; 3 binary feet
     ;; (e.g. "anticonstitutioneel")
     ((equal? nr_prestress_syl 6)
      (begin
	;;; first foot
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       this_syl_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))
	;; second foot
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(set! this_syl_node (item.next (item.next this_syl_node)))
	(item.feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree this_syl_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))
	;; third foot
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(set! this_syl_node (item.next (item.next this_syl_node)))
	(item.feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree this_syl_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))))
     ;; 7 pre-stress syllables ==> 
     ;; 1 appendix and 3 binary feet
     ;; (e.g. "werkgelegenheidsargument")
     ;; This is absurd and only occurs because the grapheme-to-phoneme
     ;; converter fails to generate a compound boundary
     ((equal? nr_prestress_syl 7)
      (begin
	(set! app_node 
	      (utt.relation.append utt
				   'Foot
				   (list 'Appendix)))
	(item.relation.append_daughter this_prosword_node 'ProsTree app_node)
	(item.relation.append_daughter app_node 'ProsTree this_syl_node)
	;; first foot
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next 
					(item.next this_syl_node)))
	;; second foot
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(set! this_syl_node (item.next (item.next this_syl_node)))
	(item.feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree this_syl_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))
	;; third foot
	(set! foot_node
	      (utt.relation.append utt
				   'Foot
				   '(Foot ((metrical weak)))))
	(item.relation.append_daughter this_prosword_node
				       'ProsTree
				       foot_node)
	(set! this_syl_node (item.next (item.next this_syl_node)))
	(item.feat this_syl_node 'metrical 'strong)
	(item.relation.append_daughter foot_node
				       'ProsTree this_syl_node)
	(item.relation.append_daughter foot_node
				       'ProsTree
				       (item.next this_syl_node))))
     )
    ;; --------------------------------------------------
    ;; step3: build foot structure for stressed syllables 
    ;; and others
    ;; --------------------------------------------------
    ;; go to the stressed syllable node
    (set! this_syl_node stressed_syl_node)
    (while this_syl_node
	   ;; -----------------
	   ;; create a new foot
	   ;; -----------------
	   (set! foot_node
		 (utt.relation.append utt 'Foot '(Foot)))
	   ;; if this syllable is the stressed syllable,
	   ;; then this foot node is strong,
	   ;; otherwise it is a weak 
	   (if (equal? this_syl_node stressed_syl_node)
	       (item.set_feat foot_node 'metrical 'strong)
	       (item.set_feat foot_node 'metrical 'weak))
	   (item.relation.append_daughter this_prosword_node
					  'ProsTree
					  foot_node)
	   (item.relation.append_daughter foot_node
					  'ProsTree
					  this_syl_node)
	   ;; make this syllable strong (i.e. first one in foot)
	   (item.set_feat this_syl_node 'metrical 'strong)
	   ;; move on to the next syllable (if any)
	   (set! this_syl_node (item.relation.next this_syl_node 'Syllable))
	   ;; -------------------------------------------------------------
	   ;; Try to attach syllable to current foot:
	   ;; if (1) this syllable is non-schwa and the next is either
	   ;;        non-schwa or empty
	   ;; or (2) this syllable has an /iIy/ vowel and the next is schwa
	   ;; then include this syllable in this foot
	   ;; -------------------------------------------------------------
	   ;; FIX-ME: is it /iy/ or /Iy/, or both?
	   (if (and this_syl_node
		    (or (and (not (equal? (item.feat this_syl_node 'vowel) "@"))
			     (or (not (item.next this_syl_node))
				 (not (equal? (item.feat this_syl_node "n.vowel") "@"))))
			(and (string-matches (item.feat this_syl_node 'vowel) "[iIy]")
			     (item.next this_syl_node)
			     (equal? (item.feat this_syl_node "n.vowel") "@"))))
	       (begin
		 (item.relation.append_daughter foot_node
						'ProsTree
						this_syl_node)
		 ;; move on to next syllable
		 (set! this_syl_node (item.relation.next this_syl_node 'Syllable))))
	   ;; ------------------------------------------------
	   ;; attach all subsequent syllables with schwa vowel
	   ;; to current foot
	   ;; ------------------------------------------------
	   (while (and this_syl_node
		       (equal? (item.feat this_syl_node 'vowel) "@"))
		  (item.relation.append_daughter foot_node
						 'ProsTree
						 this_syl_node)
		  ;; move on to next syllable
		  (set! this_syl_node (item.next this_syl_node))))))




(define (nl::build_syllable_structure utt syllables)
"
\(nl::build_syllable_structure utt syllables\)

Build syllable structure (and all lower levels) for the given list of
syllables, e.g. \(\(\(w a:\) 1\) \(\(t @ r\) 0\)\).
"
  (let ((nr_prestress_syl 0)
	syl_and_stress
	this_syl_node
	stressed_syl_node
	first_syl_node)
    ;; for all syllables in prosodic word
    (while syllables
	   (set! syl_and_stress (car syllables))
	   ;; append syllable node to the Syllable relation
	   ;; map stress level to feature (1=primary, 2=secundary, 0=none)
	   ;; by default, assume a weak syllable
	   (set! this_syl_node 
		 (utt.relation.append utt 
				      'Syllable 
				      (list 'Syllable 
					    (list (cons 'stress (cdr syl_and_stress))
						  '(metrical weak)))))
	   ;; add it to prosodic tree
	   (utt.relation.append utt 'ProsTree this_syl_node)
	   ;; build sub-syllabic structure
	   (nl::build_sub_syllable_structure utt this_syl_node (car syl_and_stress))
	   ;; remember first syllable
	   (if (not first_syl_node)
	       (set! first_syl_node this_syl_node))
	   ;; Keep track of the number of prestress syllables 
	   ;; (for building foot structure)
	   (if (not stressed_syl_node)
	       (if (> (item.feat this_syl_node 'stress) 0)
		   (set! stressed_syl_node this_syl_node)
		   (set! nr_prestress_syl (+ nr_prestress_syl 1))))
	   (set! syllables (cdr syllables)))
    ;; return list with info for foot building
    (list first_syl_node  
	  stressed_syl_node
	  nr_prestress_syl)))



(define (nl::build_sub_syllable_structure utt syl_node segments)
"
\(nl::build_sub_syllable_structure utt syl_node segments\)
 
Builds the sub-syllable structure for the given string of segments.
Appends Onset, Nucleus and Code nodes to the relation SylPart.
Appends Segment nodes to the relation Segment.  Appends the
hierarchical structure to the relation ProsTree under syl_node.
"
  (let (sylpart_node seg)
    ;; -------------------------------------------
    ;; if first segment is a consonant, 
    ;; create onset node and append all consonants
    ;; -------------------------------------------
    (if (string-matches (car segments) "[pbtdkgfvszxGhZSSmnNlrwj]")
	(begin
	  (set! sylpart_node
		(utt.relation.append utt 'SylPart (list 'Onset)))
	  (item.relation.append_daughter syl_node 'ProsTree sylpart_node)
	  (while (string-matches (car segments) "[pbtdkgfvszxGhZSSmnNlrwj]")
		 (set! seg (car segments))
		 (utt.relation.append utt 'Segment (list seg))
		 (item.relation.append_daughter sylpart_node
						'ProsTree
						(utt.relation.last utt 'Segment))
		 (set! segments (cdr segments)))))
    ;; -------------------
    ;; create nucleus node
    ;; -------------------
    ;; FIX-ME: in extremely rare cases, e.g. "'s", "sst!" and "pst!",
    ;; the syllable contains no vowel
    (set! seg (car segments))
    (set! segments (cdr segments))
    (set! sylpart_node
	  (utt.relation.append utt 'SylPart (list 'Nucleus)))
    (item.relation.append_daughter syl_node 'ProsTree sylpart_node)
    (utt.relation.append utt 'Segment (list seg))
    (item.relation.append_daughter sylpart_node
				   'ProsTree
				   (utt.relation.last utt 'Segment))
    ;; add vowel info to syllable node (facilitates building feet)
    (if (string-matches seg "[@iIy]")
	(item.set_feat syl_node 'vowel seg))
    ;; -------------------------------------------
    ;; create coda node for any remaining segments
    ;; -------------------------------------------
    (if segments
	(begin
	  (set! sylpart_node
		(utt.relation.append utt 'SylPart (list 'Coda)))
	  (item.relation.append_daughter syl_node 'ProsTree sylpart_node)
	  (while segments
		 (set! seg (car segments))
		 (utt.relation.append utt 'Segment (list seg))
		 (item.relation.append_daughter sylpart_node
						'ProsTree
						(utt.relation.last utt 'Segment))
		 (set! segments (cdr segments)))))
    )
  )



;;;-----------------------------------------------------------------------------
;;; Associating a word's intonational events with syllables
;;;-----------------------------------------------------------------------------

(define (nl::associate_int_to_syls utt word)
"
\(nl::associate_int_to_syls UTT WORD\)

Associates all intonational events linked to WORD to their appropriate
syllables: an initial boundary is connected to the first syllable of
WORD, a final boundary tone to the last syllable, and a pitch accent
to the main stressed syllable. All associations are stored in the
Syl-Int relation.
"
  (let (todi_symbol)
    (mapcar
     (lambda (int_event)
       (set! todi_symbol (item.name int_event))
       (cond
	((member_string todi_symbol ToDIInitBnd)
	 (nl::set_word_init_bnd utt word int_event))
	((member_string todi_symbol ToDIAcc)
	 (nl::set_word_acc utt word int_event))
	((member_string todi_symbol ToDIFinalBnd)
	 (nl::set_word_final_bnd utt word int_event))))
     (item.relation.daughters word 'Word-Int))))



(define (nl::set_word_init_bnd utt word bnd)
" 
\(nl::set_word_init_bnd UTT WORD BND\)

Connects initial bounary tone BND to the first syllable of WORD by
appending BND as a daughter of this syllable in the relation Syl-Int.
"
(nl::set_syl_inton utt
	     ;; get the first syllable of the word
	     (item.daughter1_to
	      (item.relation
	       (item.relation.daughter1 word 'Word-Pros)
	       'ProsTree)
	      'Syllable)
	     bnd))

    
(define (nl::set_word_final_bnd utt word bnd)
" 
\(nl::set_word_final_bnd UTT WORD BND\)

Connects final bounary tone BND to the last syllable of WORD by
appending BND as a daughter of this syllable in the relation Syl-Int.
"
(nl::set_syl_inton utt
	     ;; get the last syllable of the word
	     (item.daughtern_to
	      (item.relation
	       (item.relation.daughter1 word 'Word-Pros)
	       'ProsTree)
	      'Syllable)
	     bnd))


(define (nl::set_word_acc utt word acc)
" 
\(nl::set_word_acc UTT WORD ACC\)

Connects pitch accent ACC to the stressed (strongest) syllable of
WORD, by appending ACC as a daughter of this syllable in the relation
Syl-Int.
"

(nl::set_syl_inton utt
	     ;; get the main stresses syllable of the word
	     ;; word->prosword1->prosword2->foot->syl
	     (nl::strong_dtr
	      (nl::strong_dtr 
	       (nl::strong_dtr 
		(item.relation.daughter1 word 'Word-Pros))))
	     acc))


(define (nl::set_syl_inton utt syl intev)
"
\(nl::set_syl_inton UTT SYL INTEV\)

Connects intonational event INTEV to sylable SYL in uttance UTT by
appending INTEV as a daughter of SYL in the relation Syl-Int.  Assumes
that INTEV is already in the Intonation relation.If SYL is not in the
Syl-Int relation yet, it will be appended to it first.
"
;; Note: syl MUST be present in Syl-Int relation first! 
(if (not (item.relation syl 'Syl-Int))
    (utt.relation.append utt 'Syl-Int syl))
(item.relation.append_daughter syl
			       'Syl-Int
			       intev))


(define (nl::strong_dtr item)
"
\(nl::strong_dtr ITEM\)
 
Return the metrically strong daughter of ITEM in the relation ProsTree.
"
(let ((dtrs (item.relation.daughters item 'ProsTree)))
  (while (and (> (length dtrs) 1)
	      (not (equal? (item.feat (car dtrs) 'metrical) "strong")))
	 (set! dtrs (cdr dtrs)))
  (car dtrs)))


;;;-----------------------------------------------------------------------------
;;; Synthesizing lexical specifications
;;;-----------------------------------------------------------------------------

(define (Lexical utt)
"
\(Lexical UTT\)

This is the main part of the mode for synthesizing lexical specifications.
It is only used in combination with Flextool, not during normal synthesis. 
"
  (utt.relation.create utt 'Word)
  (utt.relation.create utt 'Word-Int)
  (utt.relation.create utt 'Intonation)
  ;; add new relations for all lower prosodic constituents
  (utt.relation.create utt 'Segment)
  (utt.relation.create utt 'SylPart)
  (utt.relation.create utt 'Syllable)
  (utt.relation.create utt 'Foot)
  (utt.relation.create utt 'ProsWord1)
  (utt.relation.create utt 'ProsWord2)
  (utt.relation.create utt 'Word-Pros)
  ;; add new relation to store the prosodic tree 
  ;; (i.e. mother-daughter relations)
  (utt.relation.create utt 'ProsTree)
  ;; add a new relation to store intonation-to-syllable association
  (utt.relation.create utt 'Syl-Int)
  (let (word)
    ;; for all prosodic words in the input list
    (mapcar
     (lambda (lemma)
       ;; Word relation is filled with dummy words
       (set! word (utt.relation.append utt 'Word '("woord"))) 
       (utt.relation.append utt 'Word-Int word)
       ; associate "%L H*L L%" tune
       (item.relation.append_daughter
	word
	'Word-Int
	(utt.relation.append utt 'Intonation '("%L")))
       (item.relation.append_daughter
	word
	'Word-Int
	(utt.relation.append utt 'Intonation '("H*L")))
       (item.relation.append_daughter
	word
	'Word-Int
	(utt.relation.append utt 'Intonation '("L%")))
       ;; insert a medium break
       (item.set_feat word 'pbreak 'medium)
       (nl::build_prosword_structure utt word lemma)
       (nl::associate_int_to_syls utt word)
       )
     ;; original input form second argument of Uterance function
     ;; is stored as a string in the "iform" feature of an utterance
       (read-from-string (utt.feat utt 'iform))))
  utt)


;;;-----------------------------------------------------------------------------
;;; Setup a lexicon and LTS method
;;;-----------------------------------------------------------------------------


(defvar kunlexdir (path-append lexdir "kunlex"))


(define (nl::setup_kunlex_fonpars)
"
\(setup_kunlex_fonpars\) 

Setup the KUN lexicon with Fonpars as lts method.
"
  (if (not (member_string "kunlex_fonpars" (lex.list)))
      (begin
	(require 'net_nl_lts_fonpars)
	(lex.create "kunlex_fonpars")
	(lex.set.phoneset "net_nl")
	(lex.set.lts.method 'nl::fonpars_lts)
	(lex.set.compile.file (path-append lexdir "kunlex" "kunlex-1.0.out"))
	(nl::addenda))))


(define (nl::setup_kunlex_treetalk)
"
\(setup_kunlex_treetalk\) 

Setup the KUN lexicon with TreeTalk as lts method.
"
  
  (if (not (member_string "kunlex_treetalk" (lex.list)))
      (begin
	(require 'net_nl_lts_treetalk)
        (nl::init_treetalk)
	(lex.create "kunlex_treetalk")
	(lex.set.phoneset "net_nl")
	(lex.set.lts.method 'nl::treetalk_lts)
	(lex.set.compile.file (path-append kunlexdir "kunlex-1.0.out"))
	(nl::addenda))))


(define (nl::setup_kunlex_treetalk_mbma)
"
\(setup_kunlex_treetalk_mbma\) 

Setup the KUN lexicon with TreeTalk as lts method,
and MBMA for morphological analysis.
"
  
  (if (not (member_string "kunlex_treetalk_mbma" (lex.list)))
      (begin
	;; assume all other requirements are in 'net_nl_lts_treetalk_mbma
	(require 'net_nl_lts_treetalk_mbma)
        (nl::init_treetalk_mbma)
        (nl::init_mbma)
	(lex.create "kunlex_treetalk_mbma")
	(lex.set.phoneset "net_nl")
	(lex.set.lts.method 'nl::treetalk_mbma_lts)
	(lex.set.compile.file (path-append kunlexdir "kunlex-1.0.out"))
	(nl::addenda))))

;; FIX-ME:
;; When using TreeTalk, using the full lexicon is actually a waste of memory, 
;; as most words in the lexicon are perfectly reproduced by TreeTalk 
;; (but how many precisely?; also depends on using IB1 or IGTREE). 
;; However, a small lexicon of homographs would still be required, 
;; because TreeTalk does not take POS info into account.
;; Hence, the choice of the lexicon ultimately depends on the lts method used.
;; Although this still needs to be worked out, there are already two separate 
;; functions for setting up kunlex.


(define (compile_kunlex)
"
\(compile_kunlex\)

Concatenate the base and homograph lexicon, and compile them
"
  ;(debug_output t)
  (system (format nil 
		  "cat %s %s %s > %s"
		  (path-append kunlexdir "kunlex-1.0-special.scm")
		  (path-append kunlexdir "kunlex-1.0-homograph.scm")
		  (path-append kunlexdir "kunlex-1.0-base.scm")
		  (path-append kunlexdir "kunlex-1.0.scm")))
  (lex.compile 
   (path-append kunlexdir "kunlex-1.0.scm")
   (path-append kunlexdir "kunlex-1.0.out")))



;;;-----------------------------------------------------------------------------
;;; Debug functions
;;;-----------------------------------------------------------------------------

(define (check_lexicon lex_fn trace_fn)
"
\(define (check_lexicon LEX_FN TRACE_FN\)

Checks lexicon LEX_FN by synthesizing all its words,
writing encountered errors to TRACE_FN.
"
(let (entry lexical 
	    (lexfp (fopen lex_fn "r"))
	    (tracefp (fopen trace_fn "w")))
  (while (not (equal? (set! entry (readfp lexfp)) (eof-val)))
	 (set! lexical (list (car (cdr (cdr entry)))))
	 (format t "Checking: %l\n" entry)
	 ;(format tracefp "Checking: %l\n" entry)
	 (unwind-protect
	  ;; try synthesis, using an utterance of type Lexical
	  (utt.synth (eval (list 'Utterance 'Lexical lexical)))
	  ;; catch error
	  (begin
	    (format t "Encounterd error with: %l\n" entry) 
	    (format tracefp "Encounterd error with: %l\n" entry))))
  (fclose lexfp)
  (fclose tracefp)))


(provide 'net_nl_lex)











