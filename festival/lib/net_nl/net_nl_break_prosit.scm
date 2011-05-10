;;; $Id: net_nl_break_prosit.scm,v 1.5 2005/02/15 11:00:25 emarsi Exp $
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


;;; break assignment according to Prosit


;;; ------------------------------------------------------------
;;; Initialization
;;; ------------------------------------------------------------

(require 'net_nl_timbl)

(set! nl::trace-prosit-break nil)

(add-doc-var  nl::trace-prosit-break t
"If true, trace break assignment using the Prosit approach."
)


(set! nl::prosit-break-feats
  (list 
   (list "R:Token.parent.p.p.name" "___")
   (list "R:Token.parent.p.name" "___")
   (list "R:Token.parent.name" "___")
   (list "R:Token.parent.n.name" "___")
   (list "R:Token.parent.n.n.name" "___")
   (list "R:Token.parent.p.p.lisp_prosit-prepunc" "___")
   (list "R:Token.parent.p.lisp_prosit-prepunc" "___")
   (list "R:Token.parent.lisp_prosit-prepunc" "___")
   (list "R:Token.parent.n.lisp_prosit-prepunc" "___")
   (list "R:Token.parent.n.n.lisp_prosit-prepunc" "___")
   (list "R:Token.parent.p.p.lisp_prosit-postpunc" "___")
   (list "R:Token.parent.p.lisp_prosit-postpunc" "___")
   (list "R:Token.parent.lisp_prosit-postpunc" "___")
   (list "R:Token.parent.n.lisp_prosit-postpunc" "___")
   (list "R:Token.parent.n.n.lisp_prosit-postpunc" "___")
   (list "R:Token.parent.p.p.pos" "___")
   (list "R:Token.parent.p.pos" "___")
   (list "R:Token.parent.pos" "___")
   (list "R:Token.parent.n.pos" "___")
   (list "R:Token.parent.n.n.pos" "___")
   ))

(add-doc-var 'nl::prosit-break-feats
"Defines the features of a word that are used for constructing the
instance fed to the Timbl classifier for Prosit break assignment. It
is a a list of the form

\( \(path-1 val-1\) ... \(path-n val-n\) \) 

where each path defines a feature value, and each string defines the
value to use if the feature turns out to be undefined \(that is, when
item.feat returns a zero\)

See also:
\(nl::feature-vector ITEM FEATURE-DEF\)"
)


(set! nl::prosit-break-instance-base
      (path-append libdir "net_nl/prosit-ibs/prosit-break.igtree"))

(add-doc-var 'nl::prosit-break-instance-base
"Path to Timbl instance base for Prosit break assignment")


(set! nl::prosit-break-timbl-options
      (if nl::trace-prosit-break
	  ;; a1 = use IGTREE algorithm
	  "-a1"
	  ;; vs = don't get verbose
	  "-a1 +vs"))

(add-doc-var 'nl::prosit-break-timbl-options
"The options passed to the Timbl for Prosit break assignment")


(define (nl::init-prosit-break-timbl)
"
\(nl::init-prosit-break-timbl\)

Initialize a Timbl classifier for Prosit break assignment. Its
options are defined by nl::prosit-break-timbl-options and its instance
base by nl::prosit-break-instance-base.
" 
  ;; Start up a new Timbl instance
  ;; The previously assigned Timbl instance (if any)
  ;; will be automatically destructed by garbage collection
  (set! nl::prosit-break-timbl (Timbl nl::prosit-break-timbl-options))
  ;; Read the igtree and feature weights
  ;; According to the Timbl convention, the weights file has the 
  ;; same name as the instance base plus the suffix ".wgt"
  (Timbl.get_instance_base nl::prosit-break-timbl 
			   nl::prosit-break-instance-base)) 

;; init on load
(nl::init-prosit-break-timbl)


;;; ------------------------------------------------------------
;;; Main
;;; ------------------------------------------------------------

(define (nl::prosit-break utt)
" 
\(nl::prosit-break-assignment UTT\)

Assign breaks to utterance UTT using the Prosit approach. Breaks are
indicated by the feature 'pbreak' on items in the Word relation, which
can take the values 'heavy', 'medium', and 'light'.
" (if nl::trace-prosit-break
      (begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "nl::prosit-break-assignment\n")
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n")
	(format t "instance base: %s\n"  nl::prosit-break-instance-base)
	(format t "Timbl options: %s\n\n"  nl::prosit-break-timbl-options)))
  ;; removing old phrasing is important when the function is called
  ;; with an existing utterance, as in Nintens
  (nl::remove_phrasing utt)
  (let (instance class)
    (mapcar 
     (lambda (word)
       (set! instance (nl::break-feature-vector word))
       (set! class  (intern (Timbl.classify nl::prosit-break-timbl instance)))
       (if (string-equal class "B")
	   (item.set_feat word 'pbreak 'medium))
       (if nl::trace-prosit-break
	   (format t "Word: %s\nInstance: %s\nClassification: %s\n\n" 
		   (item.name word)
		   instance 
		   class))
       )
     (utt.relation.items utt 'Word))
    ;; enforce a heavy break at the end of the utterance
    (if (utt.relation.items utt 'Word)
	(item.set_feat (utt.relation.last utt 'Word) 'pbreak 'heavy)))
  (apply_hooks post_phrasing_hooks utt)
  utt)



(define (nl::break-feature-vector word)
"
\(nl::break-feature-vector WORD\)

Function for constructing Timbl instances for the pupose of break
assignment. Returns a feature vector for WORD by appending all values
for the features defined in nl::prosit-break-feats.

See also:
nl::prosit-break-feat
\(nl::Feature-vector ITEM FEATURE-DEF\).
"
  (nl::feature-vector word nl::prosit-break-feats))



;;; ------------------------------------------------------------
;;; Feature functions
;;; ------------------------------------------------------------

(define (prosit-prepunc item)
"
\(prosit-prepunc ITEM\)

Feature function that derives the prepunctuation feature as defined in
Prosit.
"
  ;; In contrast to the punc feature, the prepunctuation feature is
  ;; always present on a token. If it is empty, it is mapped to "=".
  (let ((val (item.feat item 'prepunctuation)))
    (if (string-equal val "")
	"="
	val)))


(define (prosit-postpunc item)
"
\(prosit-postpunc ITEM\)

Feature function that derives the postpunctuation feature as defined
in Prosit.
"
  ;; Contray to prepunctuation, the punc feature is not always
  ;; present. This can either mean that the token is not followed by
  ;; punctuation, which corresponds to Prosit's "=', or that we are
  ;; out of the window, which corresponds to Prosit's "___".  We
  ;; distinguish the between the two cases by checking if it the token
  ;; has a name.
  (cond (;; out of window
	 (eq (item.name item) 0)       
	 0)
	;; no punctuation
	((or (eq (item.feat item 'punc) 0)
	     (string-equal (item.feat item 'punc) ""))
	 "=")
	(;; indeed followed by punctuation
	 t
	 (item.feat item 'punc))))

      




(provide 'net_nl_break_prosit)