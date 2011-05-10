;;; $Id: net_nl_ad_postlex.scm,v 1.3 2004/08/16 10:11:19 emarsi Exp $
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


;;; postlexical rules specific to nl2 database


;; TODO:
;; - complete mapping from CGN phonemes to nl2
;; - add rule description and example to doc strings
;; - add test suite with tests for each rule

(define (nl_ad::postlex_rules utt)
"
\(nl_ad::postlex_rules UTT\)

Apply nl2-specific postlexical rules to the segments of UTT.
"   
  (nl_ad::rewrite_diphones utt)
  (nl_ad::rewrite_coda_diphones utt)
  (nl_ad::compose_segments utt))


(define (nl_ad::rewrite_diphones utt)
"
\(nl_ad::rewrite_diphones utt\)

Renames some phonemes because their names are different in the nl2 database,
or because some appropriate allophone is available \(e.g. a dark l\). 
Also rewrites some phoneme pairs, because the corresponding diphone
is lacking in the nl2 database.
"
  (if nl::trace_postlex 
      (format t "\n*** Rewriting rules for ad/nl2\n\n"))
  (let (this next)
    (mapcar
     (lambda (s)
       (set! this (item.name s))
       (set! next (item.feat s "n.name"))
       
       (cond
	;; --- diphtongs ---
	((string-equal this "E+") 
	 (nl::apply_rename_rule s "Ei"))
	((string-equal this "Y+") 
	 (nl::apply_rename_rule s "9y"))
	((string-equal this "A+") 
	 (nl::apply_rename_rule s "Au"))

	;; --- vowels ---e in NL2)
	((string-equal this "Y:")
	 (nl::apply_rename_rule s "Oe"))
	((string-equal this "E:") 
	 (nl::apply_rename_rule s "E"))
	((string-equal this "O:") 
	 (nl::apply_rename_rule s "O"))
	;; colored vowels before r
	((and (string-equal this "e")
	      (member_string next '("r" "R"))) 
	 (nl::apply_rename_rule s "I:"))
	((and (string-equal this "o")
	      (member_string next '("r" "R"))) 
	 (nl::apply_rename_rule s "O:"))
	((and (string-equal this "2")
	      (member_string next '("r" "R"))) 
	 (nl::apply_rename_rule s "Y:"))
	 
	;; --- consonants ---
	 
	((string-equal this "J") 
	 (nl::apply_rename_rule s "nj"))

	;; aanvullen....???

	))
     (utt.relation.items utt 'Segment))))



(define (nl_ad::rewrite_coda_diphones utt)
"
\(nl_ad::rewrite_coda_diphones utt\)

Renames some phonemes ( /l/, /r/, /w/ and /j/) in coda position because
their names are different in the nl2 database.
"
 (if nl::trace_postlex 
      (format t "\n*** Rewrite_coda_diphones for ad/nl2\n\n"))
  (let (this)
    (mapcar
     (lambda (s)
       (set! this (item.name s))
                 
       (if (string-equal (item.feat s "R:ProsTree.parent.name") "Coda")   
	  (cond                                                          
	     ((string-equal this "l")
	      (nl::apply_rename_rule s "L"))               
	     ((string-equal this "r")
	      (nl::apply_rename_rule s "R"))                
	     ((string-equal this "w")
	      (nl::apply_rename_rule s "W"))              
	     ((string-equal this "j")
	      (nl::apply_rename_rule s "J"))          
	)))
    (utt.relation.items utt 'Segment))))


(define (nl_ad::compose_segments utt)
" 
\(nl_ad::compose_segments UTT\)

Rewrite segment pairs to nl2-specific diphtongs:
/t j/ to /tj/ example: ketjap and
/d j/ to /dj/ example: djintan
"
  ;; The nl2 database has a number of special diphones for diphtongs: tj and dj
  ;; In the CGN phoneme set, these are transcribed as two consonants.
  (if nl::trace_postlex 
      (format t "\n*** Segment composition rules for ad/nl2\n\n"))
  (let (prev newname)
    (mapcar
     (lambda (s)
       (set! prev (item.feat s "p.name"))
       (if (and (string-equal (item.name s) "j")
		(member prev '("t" "d"))
		(set! newname
		      (cond
		       ;; /t j/ ==> /tj/
		       ((string-equal prev "t") "tj")
		       ;; /d j/ ==> /dj/
		       ((string-equal prev "d") "dj") )))
	   (begin 
	     (if nl::trace_postlex 
		 (format t "Segment composition rule: %s j --> %s  / %s ___ %s\n" 
			 prev
			 newname
			 (nl::left_context (item.prev s))
			 (nl::right_context s)))	     
	     (item.set_name (item.prev s) newname)
	     (set! coda (item.relation.parent s 'ProsTree)) 
	     (item.delete s)
	     ;; delete the Coda node in SylPart
	     ;; if it becomes daughter-less after the deletion of /j/
	     (if (not (item.relation.daughters coda 'ProsTree))
		 (item.delete coda)) )))
     (utt.relation.items utt 'Segment))))



(provide 'net_nl_ad_postlex)

