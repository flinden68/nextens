;;; $Id: net_nl_timbl.scm,v 1.2 2005/10/04 12:19:31 emarsi Exp $
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


;;; general functions for using Timbl


(define (nl::construct_padding_list win_size pad_sym)
"
\(nl::construct_padding_list WIN_SIZE PAD_SYM\) 

Returns a list of padding symbols PAD_SYM for use with a window with
of size WIN_SIZE.
"
  (let ((half_win_size (/ win_size 2))
	l)
    (while (> half_win_size 1)
	   (set! l (cons pad_sym l))
	   (set! half_win_size (- half_win_size 1))) 
    l))  
  


(define (nl::slice list start end)
"
\(nl::slice LIST START END\)

Return the string obtained by concatenating all elements of LIST from
position START to END.
"
  (let ((l (nth_cdr start list))
	(str ""))
    (while (and l (< start end))
	   (set! str (string-append str (car l) " "))
	   (set! l (cdr l))
	   (set! start (+ start 1)))
    str))



(define (nl::list_to_string list)
" 
\(nl::list_to_string LIST\)

Returns the string obtained from concatenating all atoms in LIST.
\(inverse operation from symbol-explode\)

Example:

festival> \(nl::list_to_string '\(2 r o\)\)
\"2ro\"
"
  (let ((str ""))
    (while list
	   (set! str (string-append str (car list)))
	   (set! list (cdr list)))
    str))



(define (nl::feature-vector item feature-def)
"
\(nl::Feature-vector ITEM FEATURE-DEF\)

A general function for constructing instances for Timbl. Return a
feature vector for ITEM by appending all value for the features
defined in FEATURE-DEF, into a string. FEATURE-DEF must be a list of
pairs, where the first member specifies the feature, and the second
member specifies the value to use in case the feature's value is
undefined \(that is, when item.feat returns a zero\). The vector is
terminated by a question mark, which \(arbitrarily\) represents its
class.
"
  (let ((vector "")
	val)
    (mapcar
     (lambda (pair)
       ;; feature path is specified in the first member of the pair
       (set! val (item.feat item (car pair)))
       (set! vector
	     (string-append 
	      vector
	      ;; if the feature's value is a zero, then append the
	      ;; value specified in the second member of the pair,
	      ;; otherwise append the value itself
	      (if (equal? val 0)
		  (cadr pair)
		  val)
	      " ")))
     ;; feature-def is list of the form 
     ;; ( (path1 val1) ... (pathn valn)) 
     ;; where path defines a feature and string defines a value to use
     ;; if the feature turns out to be undefined
     feature-def)
    ;; finnaly, append a question mark, represensting the unknown
    ;; class of this instance
    (string-append vector "?")))




(provide 'net_nl_timbl)

  
