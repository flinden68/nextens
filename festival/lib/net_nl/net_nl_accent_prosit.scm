;;; $Id: net_nl_accent_prosit.scm,v 1.2 2003/10/31 16:34:49 emarsi Exp $
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


;;; accent placement according to Prosit


;;; ------------------------------------------------------------
;;; Initialization
;;; ------------------------------------------------------------

(require 'net_nl_timbl)

(set! nl::trace-prosit-acc nil)

(add-doc-var  nl::trace-prosit-acc t
"If true, trace accent assignment using the Prosit approach."
)


(set! nl::prosit-acc-feats
  (list 
   (list "R:Token.parent.p.p.name" "___")
   (list "R:Token.parent.p.name" "___")
   (list "R:Token.parent.name" "___")
   (list "R:Token.parent.n.name" "___")
   (list "R:Token.parent.n.n.name" "___")
   (list "R:Token.parent.p.p.pos" "___")
   (list "R:Token.parent.p.pos" "___")
   (list "R:Token.parent.pos" "___")
   (list "R:Token.parent.n.pos" "___")
   (list "R:Token.parent.n.n.pos" "___")
   ))

(add-doc-var 'nl::prosit-acc-feats
"Defines the features of a word that are used for constructing the
instance fed to the Timbl classifier for Prosit accent assignment. It
is a a list of the form

\( \(path-1 val-1\) ... \(path-n val-n\) \) 

where each path defines a feature value, and each string defines the
value to use if the feature turns out to be undefined \(that is, when
item.feat returns a zero\)

See also:
\(nl::Feature-vector ITEM FEATURE-DEF\)"
)


(set! nl::prosit-acc-instance-base
      (path-append libdir "net_nl/prosit-ibs/prosit-acc.igtree"))

(add-doc-var 'nl::prosit-acc-instance-base
"Path to Timbl instance base for Prosit accent assignment")


 (set! nl::prosit-acc-timbl-options
       (if nl::trace-prosit-acc
 	  ;; a1 = use IGTREE algorithm
 	  "-a1"
 	  ;; vs = don't get verbose
 	  "-a1 +vs"))

;(set! nl::prosit-acc-timbl-options
;      (if nl::trace-prosit-acc
;	  "-k 11 -d ID"
;	  ;; vs = don't get verbose
;	  "-k 11 -d ID +vs"))

(add-doc-var 'nl::prosit-acc-timbl-options
"The options passed to the Timbl for Prosit accent assignment")


(define (nl::init-prosit-acc-timbl)
"
\(nl::init-prosit-acc-timbl\)

Initialize a Timbl classifier for Prosit accent assignment. Its
options are defined by nl::prosit-acc-timbl-options and its instance
base by nl::prosit-acc-instance-base.
" 
  ;; Start up a new Timbl instance
  ;; The previously assigned Timbl instance (if any)
  ;; will be automatically destructed by garbage collection 
  (set! nl::prosit-acc-timbl (Timbl nl::prosit-acc-timbl-options))
  ;; Read the igtree and feature weights
  ;; According to the Timbl convention, the weights file has the 
  ;; same name as the instance base plus the suffix ".wgt"
  (Timbl.get_instance_base nl::prosit-acc-timbl 
			   nl::prosit-acc-instance-base)) 


(set! nl::prosit-acc-timbl nil)

;; init on load
(nl::init-prosit-acc-timbl)


;;; ------------------------------------------------------------
;;; Main
;;; ------------------------------------------------------------

(define (nl::prosit_accent_placement utt)
" 
\(nl::prosit_accent_placement UTT\)

Assign accents to utterance UTT using the Prosit approach. Accents are
indicated by the value '+' of the feature 'acc' on items in the Word
relation.
" 
  (if nl::trace-prosit-acc
      (begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "nl::prosit-acc-assignment\n")
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n")
	(format t "instance base: %s\n"  nl::prosit-acc-instance-base)
	(format t "Timbl options: %s\n\n"  nl::prosit-acc-timbl-options)))
  ;; removing old accents is important when the function is called
  ;; with an existing utterance, as in Nintens
  (nl::remove_accents utt)
  (let (instance class)
    (mapcar 
     (lambda (word)
       (set! instance (nl::acc-feature-vector word))
       (set! class  (intern (Timbl.classify nl::prosit-acc-timbl instance)))
       (if (string-equal class "A")
	   (item.set_feat word 'acc '+))
       (if nl::trace-prosit-acc
	   (format t "Word: %s\nInstance: %s\nClassification: %s\n\n" 
		   (item.name word)
		   instance 
		   class)))
     (utt.relation.items utt 'Word))
    utt))



(define (nl::acc-feature-vector word)
"
\(nl::acc-feature-vector WORD\)

Function for constructing Timbl instances for the pupose of accent
assignment. Returns a feature vector for WORD by appending all values
for the features defined in nl::prosit-acc-feats.

See also:
nl::prosit-acc-feat
\(nl::feature-vector ITEM FEATURE-DEF\).
"
  (nl::feature-vector word nl::prosit-acc-feats))



(provide 'net_nl_accent_prosit)