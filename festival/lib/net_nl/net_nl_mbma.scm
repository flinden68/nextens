;;; $Id: net_nl_mbma.scm,v 1.2 2005/10/05 14:57:37 emarsi Exp $
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


;;; Provides morphological analysis of words. 
;;;
;;; This is basically a reimplementation of the Memory-based
;;; Morphological Analyzer (MBMA) described in:
;;;
;;; Van den Bosch, A., and Daelemans, W. (1999). Memory-based
;;; morphological analysis. In Proceedings of the 37th Annual Meeting
;;; of the Association for Computational Linguistics, ACL'99,
;;; University of Maryland, USA, June 20-26, 1999, pp. 285-292.

(require 'net_nl_timbl)


;;; ------------------------------------------------------------
;;; Global variables 
;;; ------------------------------------------------------------

;;; (Note: using global vars to speed up processing, hopefully...)

(defvar nl::trace_mbma nil
"
If true, trace morphological analysis.
See nl::mbma.
")


(defvar nl::mbma_pad_sym '_
"
The symbol used for padding a window when windowing letters
at the begin and end of a word. 
See  nl::mbma.
")


(defvar nl::mbma_win_size 13
"
The size of window used for windowing letters during morphological analysis. 
See nl::mbma.
")


(defvar nl::mbma_padding 
  (nl::construct_padding_list nl::mbma_win_size
			      nl::mbma_pad_sym)
" 
The list of padding symbols to be appended before and after the word
during morphological analysis. 
See nl::mbma.
")


(defvar nl::mbma_instance_base 
  (path-append libdir "net_nl/mbma-ibs/mbma.igtree")
"path to Timbl instance base for MBMA.
Se nl::mbma.")


(defvar nl::mbma_timbl_options
  (if nl::trace_mbma
      ;; a1 = use IGTREE algorithm
      "-a1"
      ;; vs = don't get verbose
      "-a1 +vs"))


;;; ------------------------------------------------------------
;;; Intialization
;;; ------------------------------------------------------------

(define (nl::init_mbma)
"
\(nl::init_mbma\)

Initialize Memory-based Morphological Analyzer by reading the Timbl
instance bases and setting Timbl options.
"

  (set! nl::mbma_timbl
	(Timbl nl::mbma_timbl_options))
  (Timbl.get_instance_base nl::mbma_timbl  nl::mbma_instance_base))


;;; ------------------------------------------------------------
;;; MBMA core
;;; ------------------------------------------------------------

(define (nl::mbma word)
"
\(nl::mbma WORD\)

Perform memory-based morphological analysis for WORD. Returns a list of
morphological operators.
"
;; Currently only return a bit string (well, actually a 'bit list') where 
;; 1 = start of a compound
;; 0 = not the start of a compound
;; Its size is equal to the number of chars in the word.

;; depends on global vars nl::mbma_padding, nl::mbma_timbl, and
;; nl::mbma_win_size
  (if nl::trace_mbma
      (format t "*** Starting morphological analysis ***\n\n"))
  (let (;; letters is the list of all *lowercased* letters in the word
	(letters (symbolexplode (downcase word)))
	(start 0)
	(features "")
	len
	morphop
	morph_operators)
    (set! len (length letters)) ; size of the word
    ;; add padding before and after the word, where the padding is a
    ;; list of padding symbols whose lenght depends on the window size
    (set! letters (append nl::mbma_padding 
			  letters 
			  nl::mbma_padding))
    ;; move a window over the list of letters
    (while (< start len)
	   ;; Features is the string obtained by concatenating all
	   ;; letters/paddings symbols in the window.  A dummy class
	   ;; ('?') is appended, because this expected by Timbl
	   (set! features (string-append (nl::slice letters 
						    start (+ start nl::mbma_win_size))
					 " ?"))
	   ;; call Timbl with these features, which returns the
	   ;; morphological operator corresponding to the letter in
	   ;; the center of the window
	   (set! morphop (intern (Timbl.classify nl::mbma_timbl features)))
	   (if nl::trace_mbma
	       (format t  "Calling Timbl with features %s\nreturns class %s\n" 
		       features morphop))
	   (set! morph_operators (append  morph_operators (list morphop)))
	   (set! start (+ start 1)))
    morph_operators))



(define (nl::derive_compounds word morph_operators)
"
\(nl::derive_compounds WORD MORPH_OPERATORSa\)

Given a word and a list of morphological operators predicted by MBMA,
returns the compounds as a list of bits where 
1 = start of a compound
0 = not the start of a compound
The size of the list is equal to the number of chars in the word.
"
  ;; Example:
  ;; (nl::derive_compounds "omhooggevallen" 
  ;;                       '(B 0 0 0 0 0 pv+Ige 0 V 0 0 pv+Il 0 0))
  ;; ==> ("0" "0" "0" "0" "0" "0" "0" "0" "1" "0" "0" "0" "0" "0")

  (let ((chars (symbolexplode (downcase word)))
	(prefix "") ;; prefix to start of word or last compound boundary
	morphop     ;; morphological operator for current character
	bit         ;; boolean indicating current character starts a compound
	result)     ;; list of bits for the whole word

    (while chars
	   ;; add trailing '|'
	   (set! morphop (string-append (car morph_operators) "|"))
	   (set! bit "0")

	   ;; If the morphological operator contains any of the
	   ;; classes "A" "N" "N+D" "V" "V_*V" and the prefix is is
	   ;; not equal to "", "be", "ge", or "ver", then produce "1",
	   ;; otherwise produce "0".
	   ;; As the prefix cannot be "", the first bit is always "0"
	   (if (not (member prefix '("" "be" "ge" "ver"))) 
	       (while (not (string-equal morphop ""))
		      (if (member (string-before morphop "|")	
				  '("A" "N" "N+D" "V" "V_*V"))
			  (begin
			    (set! bit "1")
			    (set! prefix "")
			    (set! morphop ""))
			  (set! morphop  (string-after morphop "|")))))

	   (set! result (append result (list bit)))
	   (set! morph_operators (cdr morph_operators))
	   (set! prefix (string-append prefix (car chars)))
	   (set! chars (cdr chars)))

    (if nl::trace_mbma
	(format t "compound bit vector: %l\n\n" result))
    result))


(provide 'net_nl_mbma)