;;; $Id: net_nl_break.scm,v 1.2 2004/10/15 16:36:26 emarsi Exp $ 
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


;; simple phrase break prediction on the basis of punctuation
;; (no light breaks are placed)

(set! nl::phrase_cart_tree 
'
((lisp_token_end_punc in ("." "!" "?" ";"))
 ((heavy))
 ((n.R:Token.parent.prepunctuation in ("'" "\"" "("))
  ((medium))
  ((lisp_token_end_punc in ("'" "\"" "," ":" ")"))
   ((medium))
   ((n.name is 0)
    ((heavy))
    ((0))))))
)



(define (nl::remove_phrasing utt)
"
\(nl::remove_phrasing UTT\)

Remove all pbreak features from words \(i.e. item in relation 'Word)
in utterance UTT.
"
  (utt.relation.remove_item_feat utt 'Word 'pbreak))


(define (nl::postphrasing_filter utt)
;; FIX-ME: this is supposed to correct prasing errors
;; in complex tokens such as numbers, email addresses, etc.
"
" 
utt)



(defvar post_phrasing_hooks nil
"
post_phrasing_hooks

A function or list of functions to apply after phrasing.")




(provide 'net_nl_break)