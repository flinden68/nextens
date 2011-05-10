;;; $Id: net_nl_postlex.scm,v 1.11 2005/03/08 10:06:31 emarsi Exp $
;;;
;;; by Joop Kerkhoff & Erwin Marsi
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


;;; postlexical rules


;; TODO
;; - add rule description and example to doc strings
;; - add test suite with tests for each rule
;; - rule for /t j/ ==> /c/


(defvar nl::trace_postlex nil
"
If true, trace postlexical rules")


(defvar nl::apply_n_deletion t
"
If true, apply n-deletion rule")


(defvar nl::trace_context 7
"
Size of left and right context shown during tracing of postlexical rule")


(define (nl::postlex_rules utt)
"
\(nl::postlex_rules UTT\)

Apply general postlexical rules to the segments of UTT.
"
  (if nl::trace_postlex
      (begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "postlexical rules\n")
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")))
  (nl::clitics utt)
  (nl::assimilation utt)
  (nl::deletion utt)
  utt)


(define (nl::clitics utt)
"
\(nl::clitics UTT\)

Apply postlexical clitic rules to the words of UTT.
"
  ;; FIXME: when synthesis type is e.g. Word, there is no token relation,
  ;; which means that clitics should be solved in a different way
  (if (utt.relation.present utt 'Token)
      (begin
	(if nl::trace_postlex 
	    (format t "\n*** Clitic rules\n\n"))
	(mapcar
	 (lambda (tok)
	   (nl::quote-s-suffix tok) 
	   (nl::genitive-s-prefix tok) 
      
	   )
	 (utt.relation.items utt 'Token))) ))


(define (nl::quote-s-suffix tok) 
"
\(nl::quote-s-suffix TOK\)

Apply plural /s/ suffixation to a word 
if its corresponding token TOK has the 'plural-s' feature. 
\(see token-to-word rules\)
"
;; examples: camera's, john's
;; FIXME: add trace
  (if (string-equal (item.feat tok 'plural-s) '+)
      ;; get the last segment of the word
      (let ((seg (item.daughtern_to 
		  (item.relation 
		   (item.relation.daughtern 
		    (item.daughtern tok) 
		    'Word-Pros) 
		   'ProsTree) 
		  'Segment))
	    coda) 
	(if (string-equal (item.feat seg "ph_vc") "+")
	    ;; word ends in a vowel => create a coda first
	    (let ((nucl (item.relation.parent seg 'ProsTree)))
	      (set! coda (item.relation.insert nucl 'SylPart '(Coda) 'after))
	      (item.relation.append_daughter 
	       (item.relation.parent nucl 'ProsTree)
	       'ProsTree
	       coda))
	    ;; word ends in a consonent => coda exists
	    (set! coda (item.relation.parent seg 'ProsTree)))
	;; append 's' segment to coda
	(item.relation.append_daughter 
	 coda
	 'ProsTree
	 (item.insert seg '(s))))))


(define (nl::genitive-s-prefix tok) 
"
\(nl::genitive-s-prefix TOK\)

Apply  /s/ prefixation to a word 
if its corresponding token TOK has the 'genitive-s' feature. 
\(see token-to-word rules\)
"
;; examples: 's morgens
;; FIXME: add trace
  (if (string-equal (item.feat tok 'genitive-s) '+)
      ;; get the first segment of the word
      (let ((seg (item.daughter1_to 
		  (item.relation 
		   (item.relation.daughter1 
		    (item.daughter1 tok) 
		    'Word-Pros) 
		   'ProsTree) 
		  'Segment))
	    onset) 
	(if (string-equal (item.feat seg "ph_vc") "+")
	    ;; word start with a vowel => create onset first
	    (let ((nucl (item.relation.parent seg 'ProsTree)))
	      (set! onset (item.relation.insert nucl 'SylPart '(Onset) 'before))
	      (item.prepend_daughter
		(item.relation.parent nucl 'ProsTree)
	       onset))
	    ;; word starts with a consonent => onset exists
	    (set! onset (item.relation.parent seg 'ProsTree)))
	;; prepend 's' segment to onset
	(item.prepend_daughter 
	 (item.relation onset 'ProsTree)
	 (item.insert seg '(s) 'before)))))
  

(define (nl::assimilation utt)
"
\(nl::assimilation UTT\)

Apply postlexical assimilation rules to the segments of UTT.
"
  ;; example "kies zelf"
  (if nl::trace_postlex 
      (format t "\n*** Assimilation rules\n\n"))
  (mapcar
   (lambda (s)
     (if (string-equal (item.feat s "ph_vc") "-")
         (begin
           (nl::fricative_rule s)
           (nl::coda_devoicing s)
           (nl::regressive_assimilation s))))
   ;; skip initial pause
   (cdr (utt.relation.items utt 'Segment))))



(define (nl::deletion utt)
"
\(nl::deletion UTT\)

Apply postlexical deletion rules to the segments of UTT.
"
  ;; can't do deletion rules in a single mapcar
  ;; because deletion appears to screw up the loop's administration
  ;; and the undefined item gives a segfault
  ;; (while-loop doesn't work either)  
  (if nl::trace_postlex 
      (format t "\n*** Deletion rules\n\n"))
  (nl::n_deletion utt)
  (nl::cc_deletion utt) 
  (nl::remove_empty_SylPart utt))


 
(define (nl::fricative_rule s)
"
\(nl::fricative_rule S\)

Apply fricative rule to item S in Segment relation.
"
  (let ((oldseg (item.name s))
	newseg)
    (if (and
	 ;; seg is voiced fricative
	 (string-equal (item.feat s "ph_ctype") "f")
	 (string-equal (item.feat s "ph_cvox") "+")
	 ;; preceded by an unvoiced fricative or an unvoiced plosive
	 (string-equal (item.feat s "p.ph_cvox") "-")
	 (or (string-equal (item.feat s "p.ph_ctype") "f")
	     (string-equal (item.feat s "p.ph_ctype") "p"))
	 (set! newseg 
	       (cond
		((string-equal oldseg "v") "f")
		((string-equal oldseg "z") "s")
		((string-equal oldseg "G") "x") )))
	(nl::apply_assim_rule "Fricative rule" s newseg))))


(define (nl::coda_devoicing s)
"
\(nl::coda_devoicing S\)

Apply coda devoicing rule to item S in Segment relation.
"
;; auslaut verhaertung; final devoicing
;; word final; compound final
  (let ((oldseg (item.name s))
	newseg)
    (if (and
	 (string-equal (item.feat s "R:ProsTree.parent.name") "Coda")
	 (set! newseg
	       (cond
		((string-equal oldseg "b") "p")
		((string-equal oldseg "d") "t")
		((string-equal oldseg "g") "k")
		((string-equal oldseg "G") "x")
		((string-equal oldseg "v") "f")
		((string-equal oldseg "z") "s")
		((string-equal oldseg "Z") "S") )))
	(nl::apply_assim_rule "Coda devoicing rule" s newseg))))


(define (nl::regressive_assimilation s)
"
\(nl::regressive_assimilation S\)

Apply  regressive assimilation rule to item S in Segment relation.
"
;; regressive assimilation (only the conversion to unvoiced segments)
  (let ((oldseg (item.name s))
	newseg)
    (if (and
	 ;; seg is voiced fricative/plosive
	 (or (string-equal (item.feat s "ph_ctype") "f")
	     (string-equal (item.feat s "ph_ctype") "p"))
	 (string-equal (item.feat s "ph_cvox") "+")
	 ;; followed by an unvoiced fricative/plosive
	 (or (string-equal (item.feat s "n.ph_ctype") "f")
	     (string-equal (item.feat s "n.ph_ctype") "p"))
	 (string-equal (item.feat s "n.ph_cvox") "-")
	 (set! newseg
	       (cond
		((string-equal oldseg "b") "p")
		((string-equal oldseg "d") "t")
		((string-equal oldseg "g") "k")
		((string-equal oldseg "G") "x")
		((string-equal oldseg "v") "f")
		((string-equal oldseg "z") "s") )))
	(nl::apply_assim_rule "Regressive assimilation rule" s newseg))))



(define (nl::n_deletion utt)
"
\(nl::n_deletion UTT\)

Apply n-deletion rule to segments of UTT.
"
  (if nl::apply_n_deletion
      (mapcar
       (lambda (seg)
	 (if (and (string-equal (item.name seg) "n")
		  ;; preceded by schwa
		  (string-equal (item.feat seg "p.name") "@")
		  ;; schwa is not first seg in prosodic word: no deletion in word #@n#
		  (not (string-equal 
			(item.feat (item.prev seg) "R:ProsTree.parent.parent.id")
			(item.feat (item.prev seg) "R:ProsTree.parent.parent.parent.daughter1.id")))
		  ;; seg is part of the coda
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Coda")
		  ;; seg is final seg in prosodic word
		  (string-equal
		   (item.feat seg 'id)
		   (item.feat (item.daughtern_to 
			       (item.relation
				(item.parent_to 
				 (item.relation seg 'ProsTree)
				 'ProsWord1)
				'ProsTree)
			       'Segment)
			      'id)))
	     (nl::apply_del_rule "n-deletion rule" seg)))
       ;; skip initial pause
       (cdr (utt.relation.items utt 'Segment)))))


(define (nl::cc_deletion utt)
"
\(nl::cc_deletion UTT\)

Apply cc-deletion rule to segments of UTT
"
  (mapcar
   (lambda (seg)  
     ;; two identical consonants in a row
     (if (and (string-equal (item.feat seg "ph_vc") "-")
	      (string-equal (item.feat seg "name") 
			    (item.feat seg "n.name")))
	 (nl::apply_del_rule "CC-deletion rule" seg)))
   ;; skip initial pause
   (cdr (utt.relation.items utt 'Segment)))) 



(define (nl::remove_empty_SylPart utt)
"
\(nl::remove_empty_SylPart utt\)

Remove all empty syllable parts \(onset, nuclues, or coda\)
\(i.e. without one or more segment daughters\)
"
  (mapcar
   (lambda (sylpart)
     (if (not (item.relation.daughter1 sylpart 'ProsTree))
	 (begin
	   (if nl::trace_postlex 
	       (format t "Deleting empty %s\n" (item.name sylpart)))
	   (item.delete sylpart))))
   (utt.relation.items utt 'SylPart)))




(define (nl::apply_assim_rule rulename seg newname)
  (if nl::trace_postlex
      (format t "%s: %s --> %s  / %s ___ %s\n" 
	      rulename
	      (item.name seg) 
	      newname
	      (nl::left_context seg)
	      (nl::right_context seg)))
  (item.set_name seg newname))


(define (nl::apply_del_rule rulename seg)
  (if nl::trace_postlex
      (format t "%s: %s --> 0  / %s ___ %s\n" 
	      rulename
	      (item.name seg)
	      (nl::left_context seg)
	      (nl::right_context seg)))
  (item.delete seg))


(define (nl::apply_rename_rule seg newname)
  (if nl::trace_postlex
      (format t "Renaming %s --> %s\n" 
	      (item.name seg) 
	      newname))
  (item.set_name seg newname))


(define (nl::apply_cs_rename_rule seg newname)
  (if nl::trace_postlex
      (format t "Renaming %s --> %s  / %s ___ %s\n" 
	      (item.name seg) 
	      newname
	      (nl::left_context seg)
	      (nl::right_context seg)))
  (item.set_name seg newname))


(define (nl::left_context s)
  (let ((i 0)
	(context ""))
    (while (and (<= i nl::trace_context)
		(set! s (item.prev s)))
	   (set! context (string-append " " (item.name s) context))
	   (set! i (+ i 1)))
    context))


(define (nl::right_context s)
  (let ((i 0)
	(context ""))
    (while (and (<= i nl::trace_context)
		(set! s (item.next s)))
	   (set! context (string-append context (item.name s) " "))
	   (set! i (+ i 1)))
    context))



(provide 'net_nl_postlex)


