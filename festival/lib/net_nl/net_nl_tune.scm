;;; $Id: net_nl_tune.scm,v 1.6 2005/02/15 10:11:37 emarsi Exp $
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


;;; tune choice


;;; define a tune choice module

(define (Tune utt)
"(Syntax utt)\n                                
Perform tune choice for accents and boundaries."
  (let ((rval (apply_method 'Tune_Method utt)))
    (cond
     ;; new style
     (rval rval)
     ;; do nothing
     (t utt)))) 


;;;-----------------------------------------------------------------------------
;;; basic tune choice
;;;-----------------------------------------------------------------------------

(define (nl::basic_tune_choice utt)
"
\(nl::basic_tune_choice_1 UTT\)

Choose an intonational tune by associating a particular ToDI pitch
accent with every accented word, and particular ToDI boundary tones
with every medium/heavy break. To this end, a new relation called
Intonation is created, which contains the intonational events, as well
as a new Word-Int relation, which contains the assocations between
words and their intonational events.

This tune choice is very simple andd boring:
- phrase starts always become %L
- phrase-initial accents become H*L
- non-phrase-initial accents become !H*L
- non-utterance-final phrase ends become H%
- utterance end becomes L% (unless it is a question)

A typical tune looks like this:
%L H*L !H*L !H*L %H %L H*L !H*L !H*L L%
"
  ;; make a new relation to store the tune as a sequence of
  ;; intonational events (i.e. ToDI pitch accents and boundary tones)
  (utt.relation.create utt 'Intonation)
  ;; make a new relation to to store the association between words and
  ;; intonational events
  (utt.relation.create utt 'Word-Int)
  ;; to force an initial boundary tone at start of utterance we insert
  ;; the tone here, but postpone its association with the 1st word,
  ;; because the word is not append to Word-Int yet
  (utt.relation.append utt 'Intonation '("%L"))
  ;;; mark phrase-initial accents, and the final accents of non-final phrases
  (nl::flag_accents utt)
  (let (acc bnd)
    (mapcar
     (lambda (word)
       ;; take the lazy way: just add every word to Word-Int,
       ;; whether it has intonation or not
       (utt.relation.append utt 'Word-Int word)

       (if (member (item.feat word "p.pbreak") '("medium" "heavy"))
	   ;; if the word is *preceded* by break
	   ;; that is, the previous word has a medium of heavy pbreak
	   (begin
	     ;; associate a final boundary with the preceding word
	     (item.relation.append_daughter
	      (item.prev word)
	      'Word-Int
	      (utt.relation.append utt 'Intonation '("H%")))
	   ;; and an initial boundary tone with the current word	   
	     (item.relation.append_daughter
	      word
	      'Word-Int
	      (utt.relation.append utt 'Intonation '("%L")))))

       (if (string-equal (item.feat word 'acc) "+")
	   (begin
	     (cond 
	      ;; initial accent of phrase
	      ((string-equal (item.feat word 'iaop) "+")
	       (set! acc "H*L"))
	      ;; non-intial accent
	      (t
	       (set! acc "!H*L")))
	     (item.relation.append_daughter word
					    'Word-Int
					    (utt.relation.append utt 'Intonation (list acc)))
	     )))
     (utt.relation.items utt 'Word))
    (if (utt.relation.items utt 'Word)
	;; only if utt is non-empty
	(begin
	  ;; to force initial boundary tone at start of utterance, we
	  ;; associate the first word with the tone inserted earlier
	  (item.prepend_daughter
	   (utt.relation.first utt 'Word-Int)
	   (utt.relation.first utt 'Intonation))
	  ;; final boundary tone at end of utterance
	  (if (string-matches (item.feat (utt.relation.last utt 'Token) 'punc) ".*[?]+.*")
	      ;; if punctuation of last token contains question mark ,then H%
	      (set! bnd "H%")
	      ;; otherwise, L%
	      (set! bnd "L%"))
	  (item.relation.append_daughter
	   (utt.relation.last utt 'Word)
	   'Word-Int
	   (utt.relation.append utt 'Intonation (list bnd))))))
utt)




;;;-----------------------------------------------------------------------------
;;; basic tune choice 2
;;;-----------------------------------------------------------------------------

(define (nl::basic_tune_choice_2 utt)
"
\(nl::basic_tune_choice UTT\)

Choose an intonational tune by associating a particular ToDI pitch
accent with every accented word, and particular ToDI boundary tones
with every medium/heavy break. To this end, a new relation called
Intonation is created, which contains the intonational events, as well
as a new Word-Int relation, which contains the assocations between
words and their intonational events.

This tune choice does not apply downstep, and varies the choice of the
phrase-final accents according to the context.
"
  ;; make a new relation to store the tune as a sequence of
  ;; intonational events (i.e. ToDI pitch accents and boundary tones)
  (utt.relation.create utt 'Intonation)
  ;; make a new relation to to store the association between words and
  ;; intonational events
  (utt.relation.create utt 'Word-Int)
  ;; to force an initial boundary tone at start of utterance we insert
  ;; the tone here, but postpone its association with the 1st word,
  ;; because the word is not append to Word-Int yet
  (utt.relation.append utt 'Intonation '("%L"))
  ;;; mark the final accents of non-final phrases
  (nl::flag_accents utt)
  (let (bnd)
    (mapcar
     (lambda (word)
       ;; take the lazy way: just add every word to Word-Int,
       ;; whether it has intonation or not
       (utt.relation.append utt 'Word-Int word)

       (if (member (item.feat word "p.pbreak") '("medium" "heavy"))
	   ;; if the word is *preceded* by break
	   ;; that is, the previous word has a medium of heavy pbreak
	   (begin
	     ;; associate a final boundary with the preceding word
	     (item.relation.append_daughter
	      (item.prev word)
	      'Word-Int
	      (utt.relation.append utt 'Intonation '("%")))
	     ;; and an initial boundary tone with the current word	   
	     (item.relation.append_daughter
	      word
	      'Word-Int
	      (utt.relation.append utt 'Intonation '("%L")))))

       (if (string-equal (item.feat word 'acc) "+")
	   ;; word is accented
	   (if (string-equal (item.feat word 'faonfp) "+")
	       ;; it is the final accent of non-final phrase
	       (nl::select_tune_non_final_phrase word utt)
	       ;; otherwise,  associate a default pitch accent: H*L	  
	       (item.relation.append_daughter word
					      'Word-Int
					      (utt.relation.append utt 'Intonation '("H*L"))))))
     (utt.relation.items utt 'Word))
    ;; to force initial boundary tone at start of utterance, we
    ;; associate the first word with the tone inserted earlier
    (item.prepend_daughter
     (utt.relation.first utt 'Word-Int)
     (utt.relation.first utt 'Intonation))
    ;; final boundary tone at end of utterance
    (if (string-matches (item.feat (utt.relation.last utt 'Token) 'punc) ".*[?]+.*")
	;; if punctuation of last token contains question mark ,then H%
	(set! bnd "H%")
	;; otherwise, L%
	(set! bnd "L%"))
    (item.relation.append_daughter
     (utt.relation.last utt 'Word)
     'Word-Int
     (utt.relation.append utt 'Intonation (list bnd))))
utt)



(define (nl::select_tune_non_final_phrase word utt)
  """  
  \(nl::select_tune_non_final_phrase word utt\)

  choose type of pitch accent for the final accent of a non-fnal phrase
  """
  ;;; Tune choice, last accent on last accent before a non final boundary
  ;;; accent on last word H*
  ;;; accent on VERB before REL H*LH
  ;;; default L*H
  (let ((pos (item.feat word "R:Token.parent.pos"))
	(nextword (item.feat word "n.name")))

    (cond
     ;;; accent is on the last word in the phrase ==> choose H*
     ((member (item.feat word "pbreak") '("medium" "heavy"))
      (item.relation.append_daughter word
				     'Word-Int
				     (utt.relation.append utt 'Intonation '("H*"))))

     ;;; accent is on verb before a relative clause ==> H*LH
     ((and (string-equal pos "V") 
	   (member nextword '("die" "dat")))
      (item.relation.append_daughter word
				     'Word-Int
				     (utt.relation.append utt 'Intonation '("H*LH"))))

     ;;; otherwise ==> L*H
     (t
      (item.relation.append_daughter word
				     'Word-Int
				     (utt.relation.append utt 'Intonation '("L*H")))) )))




;;;-----------------------------------------------------------------------------
;;; support functions
;;;-----------------------------------------------------------------------------

(define  (nl::flag_accents utt)
  """
  \(nl::flag_accents utt\)

  Flags the first accented word a a phrase with the 'iaop'  feature
  \(= 'initial accent of phrase'\), and accented words in non-final phrases 
   with the 'faonfp' feature \(= 'final accent of non-final phrase')
  """
  (let (uttfinal       ;; flag indicating that the utterance-final accent has been seen 
	phraseinit     ;; flag indicating that the phrase-initial accent has been seen
	phrasefinal)   ;; flag indicating that the phrase-final accent has been seen

    ;; read word from left to right
    (mapcar 
     (lambda (word)
       (if (and (string-equal (item.feat word 'acc) "+")
	       (not phraseinit))
	   (begin
	     (item.set_feat word 'iaop '+)
	     (set! phraseinit t)))
       (if (member (item.feat word "pbreak") '("medium" "heavy"))
	   ;; entering a new phrase, so reset phrase flag
	   (set! phraseinit nil)))
     (utt.relation.items utt 'Word))
    
    ;; read words from right to left
    (mapcar 
     (lambda (word)
      (if (member (item.feat word "pbreak") '("medium" "heavy"))
	  ;; entering a new phrase, so reset phrase final flag
	    (set! phrasefinal nil))
      (if (string-equal (item.feat word 'acc) "+")
	  (if uttfinal
	      ;; this is not the last accent the utterance
	      (if (not phrasefinal)
		  ;; this *is* the phrase-final accent
		  ;; set "final-acc-of-non-final-phrase" feature
		  (and (item.set_feat word 'faonfp '+)
		       (set! phrasefinal t)))
	      ;; this *is* the last accent in the utterance
	      (and (set! uttfinal t)
		   ;;; NB we have seen *a* phrase final break
		   (set! phrasefinal t)))))
     (reverse (utt.relation.items utt 'Word)))))



(define (nl::specified_tune utt)
"
\(nl::specified_tune utt\)

Read the ToDI tune as specified by the ibnd, pacc, and fbnd features
on words in the Word relation. A new relation called Intonation is
created, which contains the intonational events, as well as a new
Word-Int relation, which contains the assocations between words and
their intonational events.
"
  ;; make a new relation to store the tune as a sequence of
  ;; intonational events (i.e. ToDI pitch accents and boundary tones)
  (utt.relation.create utt 'Intonation)
  ;; make a new relation to to store the association between words and
  ;; intonational events
  (utt.relation.create utt 'Word-Int)
  ;; transfer specified pitch accents and boundary tones 
  ;; NB validity of tune spec is not checked
  (mapcar
   (lambda (word)
     (utt.relation.append utt 'Word-Int word)
     ;; initial boundary tone?
     (if (not (equal? (item.feat word 'ibnd) 0))
	 (item.relation.append_daughter word 
					'Word-Int  
					(utt.relation.append utt 
							     'Intonation 
							     (list (item.feat word 'ibnd)))))
       ;; pitch accent?
       (if (not (equal? (item.feat word 'pacc) 0))
	   (begin
	     (item.set_feat word 'acc '+)
	     (item.relation.append_daughter word 
					    'Word-Int 
					    (utt.relation.append utt 
								 'Intonation 
								 (list (item.feat word 'pacc))))))
       ;; final boundary tone?
       (if (not (equal? (item.feat word 'fbnd) 0))
	   (begin
	     (item.set_feat word 'pbreak 'medium)
	     (item.relation.append_daughter word 'Word-Int 
					    (utt.relation.append utt 
					      'Intonation 
					      (list (item.feat word 'fbnd)))))) )
    (utt.relation.items utt 'Word))
  utt)



(provide 'net_nl_tune)
