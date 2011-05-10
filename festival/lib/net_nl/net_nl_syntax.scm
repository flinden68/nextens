;;; $Id: net_nl_syntax.scm,v 1.1 2003/10/31 13:42:06 emarsi Exp $ 
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


;;; define a syntax module

(define (Syntax utt)
"(Syntax utt)\n                                
Apply syntactic analysis to Token relation."
  (let ((rval (apply_method 'Syntax_Method utt)))
    (cond
     ;; new style
     (rval rval)
     ;; do nothing
     (t utt)))) 

(provide 'net_nl_syntax)

