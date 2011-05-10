;;; $Id: net_nl_break_herwijnen.scm,v 1.1 2003/10/31 13:42:06 emarsi Exp $
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


;;; phrase break prediction according to the algorithm by Olga van Herwijnen


(define (nl::HerwijnenPhrasing utt)
  (if nl::herwijnen_trace
      (begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "Prosodic phrasing: Herwijnen algorithm\n")
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n")
	(format t "Max. size of medium phrase: %d word(s)\n" nl::max_size_medium_phrase)
	(format t "Max. size of light phrase: %d word(s)\n\n" nl::max_size_light_phrase)))
  ;; removing old phrasing is important when the function is called
  ;; with an existing utterance, as in Nintens
  (nl::remove_phrasing utt)
  (nl::PreparePhrasing utt)
  (nl::PerformPhrasing utt))



(define (nl::PreparePhrasing utt)
"
\(nl::PreparePhrasing UTT\)

In utterance UTT, add optional phrase breaks to items in the Word
relation as a feature 'pbreak' with value 'opt_heavy', 'opt_medium',
or 'opt_light'. Phrase break decisions depend on the punctuation
symbols and the syntactic tree.
"
  (let ((node (utt.relation.first utt 'SynTree)))
    (if nl::herwijnen_trace
	(format t "*** Step 1: inserting optional boundaries ***\n\n"))
    (if node
	(begin
	  (nl::MarkOptionalBreaks (utt.relation.first utt 'SynTree) nil nil nil)
	  (if nl::herwijnen_trace
	      (nl::PrintOptionalPhrasing utt)))
	(if nl::herwijnen_trace
	    (format t "%s %s\n\n" 
		    "cannot prepare phrasing,"
		    "because no syntactic analysis is available")))))



;; Traverse the syntactic tree in a right-to-left, depth-first fashion
;; inserting optional medium and light breaks on the way back
;; according to the syntactic configuration. 

(define (nl::MarkOptionalBreaks node medium light continuous)
"
\(nl::MarkOptionalBreaks NODE MEDIUM LIGHT CONTINUOUS\)

Mark optional medium and light breaks on the words in syntactic phrase NODE. 
If MEDIUM is true,  the last word in the phrase will be marked 
as an optional medium break. 
If LIGHT is true, the last word in the phrase will be marked 
as an optional light break. 
If CONTINUOUS is true, no additional optional breaks will be inserted 
in the phrase.
"
  (cond
   ;; ------------------------------------------------------------
   ;; Case 1: node is a terminal (i.e. a token)
   ;; ------------------------------------------------------------
   ((item.relation node 'Token)
    (begin
      ;; on the rightmost word associated with this token,
      ;; mark a medium or light break
      (cond (medium
	     (item.set_feat (item.relation.daughtern node 'Token) 'pbreak 'opt_medium))
	    (light
	     (item.set_feat (item.relation.daughtern node 'Token) 'pbreak 'opt_light)))
      ;; return true to the caller,
      ;; to indicate that indeed a word was found in this branch
      t))
   ;; ------------------------------------------------------------
   ;; Case 2: node is a non-terminal (i.e. a syntactic phrase)
   ;; that must be continuous (i.e. cannot have internal breaks)
   ;; ------------------------------------------------------------
   ((or continuous
	(member (item.name node) '("TOP" "CL")))
    (let ((dtrs (reverse (item.relation.daughters node 'SynTree)))
	  result)
      ;; continue with searching through the daughters (from right to left)
      ;; until we have found one that contains a word
      (while (and dtrs (not result))
	     (set! result (nl::MarkOptionalBreaks (car dtrs) t nil t))
	     (set! dtrs (cdr dtrs)))
      ;; if none of the daughters contained a word,
      ;; the result remains nil,
      ;; and the search will continue on the sister node preceding
      ;; the current node, or even on its parent node
      ;; until a word is found
      result))
  ;; ------------------------------------------------------------
  ;; Case 3: node is a non-terminal (i.e. a syntactic phrase)
  ;; ------------------------------------------------------------
   (t
    (let (result)
      ;; potential medium break
      (if (member (item.name node) '("SE" "MI" "EX"))
	  (set! medium t))
      ;; potential light breaks
      ;; FIX-ME: requires a better definition
      ;; e.g. what to do with multiple extraposed constituents under EX?
      (if (string-equal (item.feat node "R:SynTree.parent.name") "mi parts")
	  (set! light t))
      ;; continue with traversing *all* daughters (from right to left),
      ;; regardless of whether one of them contains a  word or not,
      ;; as the daughters may trigger breaks themselves
      (mapcar
       (lambda (dtr)
	 (set! result (nl::MarkOptionalBreaks dtr medium light nil))
	 (if result
	     ;; the break triggered by this phrase or one of its parents 
	     ;; is succesfully placed on a word
	     (begin
	       (set! medium nil)
	       (set! light nil))))
       (reverse (item.relation.daughters node 'SynTree)))
      ;; if none of the daughters contained a word,
      ;; the result remains nil,
      ;; and the search will continue on the sister node preceding
      ;; the current node, or even on its parent node
      ;; until a word is found
      result))
   ))
    


(define (nl::PerformPhrasing utt)
  (if nl::herwijnen_trace
      (format t "\n*** Step 2: inserting actual boundaries ***\n\n"))
  (nl::HeavyPhrasing (utt.relation.items utt 'Word))
  (if nl::herwijnen_trace
      (nl::PrintPhrasing utt)))



(define (nl::HeavyPhrasing words)
  (let (phrase wrd)
    (mapcar
     (lambda (wrd)
       (set! phrase (append phrase (list wrd)))
       (if (string-matches (item.feat wrd 'R:Token.parent.punc) ".*[.!?;]+.*")
	   (begin
	     (item.set_feat wrd 'pbreak 'heavy)
	     (nl::MediumPhrasing phrase)
	     (set! phrase nil))))
     words)
    (if phrase
	(nl::MediumPhrasing phrase))))


(define (nl::MediumPhrasing words)
  (let (phrase wrd)
    (mapcar
     (lambda (wrd)
       (set! phrase (append phrase (list wrd)))
       (if (string-matches (item.feat wrd 'R:Token.parent.punc) ".*[,:()-]+.*")
	   (begin
	     (item.set_feat wrd 'pbreak 'medium)
	     (nl::LengthBasedMediumPhrasing phrase)
	     (set! phrase nil))))
     words)
    (if phrase
	(nl::LengthBasedMediumPhrasing phrase))))



;;; FIX-ME: length-based phrasing is a waste of time if no syntax is
;;; available, and thus optional boundaries are lacking

(define (nl::LengthBasedMediumPhrasing words)
  (let ((len (length words)))
    (if (> len nl::max_size_medium_phrase)
	(let ((mid (/ len 2))
	      (offset 0)
	      found)
	  (while (and (not found)
		      (> (- mid offset) 0))
		 (cond
		  ((string-equal (item.feat (nth (- mid offset) words) 'pbreak) 'opt_medium)
		   (set! found (- mid offset)))
		  ((string-equal (item.feat (nth (+ mid offset) words) 'pbreak) 'opt_medium)
		   (set! found (+ mid offset)))
		  (t 
		   (set! offset (+ offset 1)))))
	  (if found
	      (begin
		(item.set_feat (nth found words) 'pbreak 'medium)
		(nl::LengthBasedMediumPhrasing (reverse (nth_cdr (- len found) (reverse words))))
		(nl::LengthBasedMediumPhrasing (nth_cdr (+  found 1) words)))))	
	(nl::LengthBasedLightPhrasing words))))


(define (nl::LengthBasedLightPhrasing words)
  (let ((len (length words)))
    (if (> len nl::max_size_light_phrase)
	(let ((mid (/ len 2))
	      (offset 0)
	      found)
	  (while (and (not found)
		      (> (- mid offset) 0))
		 (cond
		  ((member (item.feat (nth (- mid offset) words) 'pbreak) '("opt_medium" "opt_light"))
		   (set! found (- mid offset)))
		  ((member (item.feat (nth (+ mid offset) words) 'pbreak) '("opt_medium" "opt_light"))
		   (set! found (+ mid offset)))
		  (t 
		   (set! offset (+ offset 1)))))
	  (if found
	      (begin
		(item.set_feat (nth found words) 'pbreak 'light)
		(nl::LengthBasedLightPhrasing (reverse (nth_cdr (- len found) (reverse words))))
		(nl::LengthBasedLightPhrasing (nth_cdr (+  found 1) words))))))))


(define (nl::PrintOptionalPhrasing utt)
  (let ((s ""))
    (mapcar
     (lambda (token)
       ;; ignore the words, which are also in the Token relation
       (if (not (item.relation token 'Word))
	   (begin
	     (set! s (string-append s 
				    (item.feat token 'whitespace)
				    (item.feat token 'prepunctuation)
				    (item.name token)))
	     ;; punc feature is not always present
	     (if (not (string-equal (item.feat token 'punc) "0"))
		 (set! s (string-append s (item.feat token 'punc))))))
       (cond 
	((string-equal (item.feat token 'pbreak) "opt_heavy")
	 (set! s (string-append s " #")))
	((string-equal (item.feat token 'pbreak) "opt_medium")
	 (set! s (string-append s " ||")))
	((string-equal (item.feat token 'pbreak) "opt_light")
	 (set! s (string-append s " |")))
	(t t)))
     (utt.relation.items utt 'Token))
  (format t "%s\n" s)))


(define (nl::PrintPhrasing utt)
  (let ((s ""))
    (mapcar
     (lambda (token)
       ;; ignore the words, which are also in the Token relation
       (if (not (item.relation token 'Word))
	   (begin
	     (set! s (string-append s 
				    (item.feat token 'whitespace)
				    (item.feat token 'prepunctuation)
				    (item.name token)))
	     ;; punc feature is not always present
	     (if (not (string-equal (item.feat token 'punc) "0"))
		 (set! s (string-append s (item.feat token 'punc))))))
       (cond 
	((string-equal (item.feat token 'pbreak) "heavy")
	 (set! s (string-append s " #")))
	((string-equal (item.feat token 'pbreak) "medium")
	 (set! s (string-append s " ||\n")))
	((string-equal (item.feat token 'pbreak) "light")
	 (set! s (string-append s " |")))
	(t t)))
     (utt.relation.items utt 'Token))
  (format t "%s\n" s)))



(define (nl::PrintPhrase words)
  (mapcar
   (lambda (wrd)
     (format t "%s " (item.name wrd)))
   words)
  (format t "\n"))






;;; ------------------------------------------------------------
;;; Global variables 
;;; ------------------------------------------------------------

(set! nl::herwijnen_trace t)

;(defvar nl::herwijnen_trace nil
;"If true, trace phrasing by Herwijnen algorithm.")

(defvar  nl::max_size_medium_phrase 16
"The maximal no. of words allowed in a medium phrase during
applications of the Herwijnen phrasing algorithm")

(defvar  nl::max_size_light_phrase 8
"The maximal no. of words allowed in a light phrase during
applications of the Herwijnen phrasing algorithm")



(provide 'net_nl_break_herwijnen)




  
  









	
	   
	   
	       
	   
	   
	   