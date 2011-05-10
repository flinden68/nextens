;;; $Id: net_nl_lex_addenda.scm,v 1.4 2004/10/15 16:31:53 emarsi Exp $
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


;;; additions to the lexicon


(define (nl::addenda)
"
\(nl::addenda\)

Additions to the lexicon.
"
  (lex.add.entry '("xs4all" nil nil (access for all)))
  (lex.add.entry '("access" nil ((((E k) 1) ((s E s) 0)))))
  (lex.add.entry '("for" nil ((((f O r) 1)))))
  (lex.add.entry '("all" nil ((((O l) 1)))))
 
)


(provide 'net_nl_lex_addenda)