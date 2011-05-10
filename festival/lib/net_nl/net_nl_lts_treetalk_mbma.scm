;;; $Id: net_nl_lts_treetalk_mbma.scm,v 1.2 2005/10/04 21:01:46 emarsi Exp $
;;;
;;; by Erwin marsi & Dieter Van Uytvanck
;;; for the NeXTenS project
;;;
;;; Copyright (c) 2005
;;; ILK - Tilburg University
;;; L&S - University of Nijmegen
;;; Stichting Spraaktechnologie
;;;
;;; All rights Reserved.
;;;
;;; See the files NEXTENS.COPYING and NEXTENS.LICENSE 
;;; for information on usage and redistribution of this file, 
;;; and for a DISCLAIMER OF ALL WARRANTIES.


;;; Experimental code for TreeTalk in combination with morphological
;;; analyses provided by the Memory-based Morphological Analyzer
;;; (MBMA). This should finally be merged with net_nl_lts_treetalk.scm.

(require 'net_nl_lts_treetalk)
(require 'net_nl_timbl)
(require 'net_nl_mbma)

;;; ------------------------------------------------------------
;;; Global variables 
;;; ------------------------------------------------------------


(defvar nl::morph_feat_win_size 5
"
The size of window used for windowing the morphological feature 
during graheme-to-phoneme conversion. 
See nl::graph_to_phon_conversion_wth_mbma.
"
;; NOTE: this is different from nl::mbma_win_size!
)


(defvar nl::morph_feat_padding 
  (nl::construct_padding_list nl::morph_feat_win_size
			      nl::pad_sym)
" 
The list of padding symbols to be appended before and after the 
morphological feature list during graheme-to-phoneme conversion. 
See nl::graph_to_phon_conversion_with_mbma
"
;; NOTE: this is different from nl::mbma_padding!
)


;;; ------------------------------------------------------------
;;; Start Timbl instances for TreeTalk with MBMA
;;; ------------------------------------------------------------

(define (nl::init_treetalk_mbma)
"
\(nl::init_treetalk_mbma\)

Initialize TreeTalk by reading the Timbl instance bases
and setting Timbl options
\(see nl::phon_timbl and nl::syl_timbl\)
" 
  ;; Start a new Timbl instance for grapheme-to-phoneme conversion 
  ;; relying on morphological analysis,  and read the igtree.
  ;; The previously assigned Timbl instance (if any)
  ;; will be automatically destructed by garbage collection 
  (set! nl::phon_timbl
	(Timbl "-a1 +vs"))
  (Timbl.get_instance_base nl::phon_timbl 
			   (path-append libdir "net_nl/treetalk-ibs/phon_mbma.igtree")) 

  ;; Start a new Timbl instance for syllabification, and read the igtree.
  ;; The previously assigned Timbl instance (if any)
  ;; will be automatically destructed by garbage collection.
  ;; NOTE: MBMA is only used for g2p conversion, not for syllabification yet,
  ;; so nothing new here
  (set! nl::syl_timbl
	(Timbl "-a1 +vs"))
  (Timbl.get_instance_base nl::syl_timbl 
			   (path-append libdir "net_nl/treetalk-ibs/syl.igtree")))





;;; ------------------------------------------------------------
;;; TreeTalk with MBMA core
;;; ------------------------------------------------------------

(define (nl::treetalk_mbma_lts word)
"
\(nl::treetalk_mbma_lts WORD\)

Performs grapheme-to-phoneme conversion and syllabicifaction for WORD
using the TreeTalk method, and relying on morphological analysis with
the Memory-Based Morphological Analyzer. Syllabification is taken to
include prosodic word phrasing and stress assignment. Returns a lemma
in the same format as the default function lex.lookup.
"
  (if nl::trace_treetalk
      (begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "nl::treetalk mbma for word \"%s\"\n" word)
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n")))
  (list word nil
	;; NOTE: MBMA is only used for g2p conversion, not for syllabification (yet)
	(nl::syllabification 
	 (nl::graph_to_phon_conversion_with_mbma word))))



(define (nl::graph_to_phon_conversion_with_mbma word)
"
\(nl::graph_to_phon_conversion_with_mbma WORD\)

Perform grapheme-to-phoneme conversion for WORD, relying on
morphological analysis with Memory-Based Morphological
Analyzer. Returns a list of phonemes.
"
   ;; depends on global vars nl::phon_padding, nl::phon_timbl, and
   ;; nl::phon_win_size
  (let (;; letters is the list of all *lowercased* letters in the word
	(letters (symbolexplode (downcase word)))
	(start 0)
	(features "")
	len
	phon
	phonemes
	compounds)
    ;; perform morphological analysis and derive compound
    ;; boundaries as a list of bits
    (set! compounds  (nl::derive_compounds word (nl::mbma word)))
    (if nl::trace_treetalk
	(format t "*** Starting grapheme-to-phoneme conversion ***\n\n"))
    (set! len (length letters)) ; size of the word
    ;; add padding before and after the word, where the padding is a
    ;; list of padding symbols whose lenght depends on the window size
    (set! letters (append nl::phon_padding 
			  letters 
			  nl::phon_padding))
    ;; add padding before and after the compound bit list, where the
    ;; padding is a list of padding symbols whose lenght depends on
    ;; the window size
    (set! compounds (append nl::morph_feat_padding 
			    compounds
			    nl::morph_feat_padding))
    ;; move a window over the list of letters
    (while (< start len)
	   ;; Features is the string obtained by concatenating:
	   ;; 1. all letters/paddings symbols in the word window
	   ;; 2. all compound bits/padding symbols in te compound window
	   ;; 3. a dummy class '?'
	   (set! features (string-append 
			   (nl::slice letters 
				      start (+ start nl::phon_win_size))
			   (nl::slice compounds 
				      start (+ start nl::morph_feat_win_size))
			   " ?"))
	   ;; call Timbl with these features, which returns the phoneme
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




(provide 'net_nl_lts_treetalk_mbma)

