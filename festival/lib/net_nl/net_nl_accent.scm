;;; $Id: net_nl_accent.scm,v 1.1 2003/10/31 13:42:06 emarsi Exp $
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


;;; accent assignment


(define (nl::basic_accent_placement utt)
"
\(nl::basic_accent_placement UTT\)

Apply a basic accent placement algorithm that accents all content
words, leaving function words unaccented.
"
  ;; removing old accents is important when the function is called
  ;; with an existing utterance, as in Nintens
  (nl::remove_accents utt)
  (mapcar 
   (lambda (word)
     (set! pos (item.feat word "R:Token.parent.pos"))
     (set! feats (item.feat word "R:Token.parent.pos-attributes"))
     ;; a content word is defined a noun, adjective, or a verb that is
     ;; not an auxiliary
     (if (or
	  (string-equal pos "N")
	  (string-equal pos "Adj")
	  (and (string-equal pos "V")
	       (not (string-matches feats ".*hulp.*"))))
	 (item.set_feat word 'acc '+)))
   (utt.relation.items utt 'Word))
  utt)



(define (nl::remove_accents utt)
"
\(nl::remove_accents UTT\)

Remove all acc features from words \(i.e. item in relation 'Word)
in utterance UTT.
"
  (utt.relation.remove_item_feat utt 'Word 'acc))



;;;-----------------------------------------------------------------------------
;;; Some global variables
;;;-----------------------------------------------------------------------------

(defvar ToDIInitBnd '("%L" "%H" "%HL")
"
The set of all initial boundary tones in ToDI.
")


(defvar ToDIFinalBnd '("%" "L%" "H%")
"
The set of all final boundarys in ToDI.
")


(defvar ToDIAcc '("H*" "L*" "H*L" "H*LH" "H*+L" "L*H" "L*HL" "L*!HL"
"!H*" "H*!H" "!H*L" "!H*LH" "!H*+L")
"
The set of all pitch accents in ToDI.
")





;; FIX-ME doesn't belong here?

(define (nl::clear-accents utt)
  (mapcar
   (lambda (word)
     (item.remove_feature word 'acc))
   (utt.relation.items utt 'Word))
  utt)


(provide 'net_nl_accent)
