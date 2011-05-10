;;; $Id: net_nl_lts_fonpars.scm,v 1.1 2003/11/08 19:18:26 emarsi Exp $
;;;
;;; by Erwin marsi & Joop Kerkhoff
;;; for the NeXTenS project
;;;
;;; Copyright (c) 2003
;;; ILK - Tilburg University
;;; L&S - University of Nijmegen
;;; Stichting Spraaktechnologie
;;;
;;; All rights reserved.
;;;
;;; See the files NEXTENS.COPYING and NEXTENS.LICENSE 
;;; for information on usage and redistribution of this file, 
;;; and for a DISCLAIMER OF ALL WARRANTIES.


;;; Provides letter-to-sound conversion by Fonpars.
;;; This requires that Festival is compiled including the NextensFonpars module,
;;; which provides to Fonpars function. 
  

(define (nl::fonpars_lts word)
"
\(nl::fonpars_lts WORD\)

Performs rule-based grapheme-to-phoneme conversion and
syllabicifaction for WORD using the Fonpars method. Returns a lemma in
the same format as the default function lex.lookup. In contrast to the
TreeTalk method, it provides no information about compound boundaries,
and always returns a single, non-compound word.
" 
  (list word nil (list (Fonpars word))))

;; Limitations:
;; The fonpars rule set does not provide compound boundaries, 
;; nor secundary stress.
;; Ultimately, the incoming word should be decompounded first,
;; after which each part should  be send to Fonpars seperately.


(provide 'net_nl_lts_fonpars)

