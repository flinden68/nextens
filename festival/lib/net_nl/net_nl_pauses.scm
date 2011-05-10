;;; $Id: net_nl_pauses.scm,v 1.2 2004/05/03 16:15:54 emarsi Exp $
;;;
;;; by Joop Kerkhoff & Erwin marsi
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


;;; pause insertion


(define (nl::pauses utt)
"
\(nl::pauses UTT\)

Insert pauses after phrase breaks, and at the start and end of UTT.
"
   (nl::insert_initial_pause utt)
   (nl::insert_phrase_breaks utt)
   (nl::insert_final_pause utt)
utt)


;; create a silence segment according to the definition of silences in
;; the phone set

(set! nl::silence_symbol (car (cadr (car(PhoneSet.description '(silences))))))
(set! nl::silence_seg (list nl::silence_symbol))


(define (nl::insert_initial_pause utt)
"
\(nl::insert_initial_pause UTT\)

Insert a silence segment before the first segment in UTT.
"
  (let ((first_seg (car (utt.relation.items utt 'Segment))))
    (if first_seg
	;; check if pause is already there
	(if (not (string-equal (item.name first_seg) nl::silence_symbol))
	    (item.relation.insert first_seg 
				  'Segment 
				  nl::silence_seg
				  'before))
	;; empty utt with no segs
	(utt.relation.append utt 'Segment  nl::silence_seg))))


(define (nl::insert_phrase_breaks utt)
"
\(nl::insert_break_pauses UTT\)

Insert an appropriate pause segment after every word with a pbreak
feature.
"
  (let (breaklevel last_seg)
    (mapcar
     (lambda (word)
       (set! break_level (item.feat word 'pbreak))
       (if (member_string break_level '(heavy medium light))
         (begin
;      	  ;; (format t "word %s ->> break %s\n" (item.name word) break_level)	   
	  (set! last_seg
	     (item.daughtern_to
	      	(item.relation
	       	  (item.relation.daughter1 word 'Word-Pros) 'ProsTree) 'Segment))
	  ;; being smart: do not insert a second pause
	  (if (not (string-equal (item.feat last_seg "n.name") nl::silence_symbol))
	      (item.insert last_seg nl::silence_seg))
	  ;; for convenience, insert break level as feature at silence segment
	  (item.set_feat (item.next last_seg) 'break break_level))))
     (utt.relation.items utt 'Word))))



(define (nl::insert_final_pause utt)
"
\(nl::insert_final_pause UTT\)

Insert a silence segment after the last segment in UTT.
"
  (let ((last_seg (car (reverse  (utt.relation.items utt 'Segment)))))
    (if (not (string-equal (item.name last_seg) nl::silence_symbol))
	(item.relation.insert last_seg 
			      'Segment 
			      nl::silence_seg
			      'after))))


(provide 'net_nl_pauses)
