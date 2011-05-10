;;; $Id: net_nl_nintens.scm,v 1.5 2004/07/01 08:57:04 emarsi Exp $
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


;;; Nintens interface code


;;;-----------------------------------------------------------------------------
;;; Retrieving and modifying the token to word mapping
;;;-----------------------------------------------------------------------------



(define (GetTokenToWords utt)
"
\(GetTokenToWords UTT\)

Retrieves the tokens and words of an utterance as a list of lists,
where each sublist contains a token, followed by zero or more
words associated with this token.
 
Example of output format
(NB all atoms have to be quoted, which cannot be shown in a docstring!):

\(\(formule formule\)
 \(I  een\)\)

See also: SetTokensToWords
"
;; punctuation is left out
(let (result)
  (mapcar
   (lambda (token)
     (if (not (item.relation token 'Word))
	 (set! result 
	       (append result
		       (list (cons (PunctuatedToken token)
				   (mapcar
				    (lambda (word)
				      (item.name word))
				    (item.daughters token))))))))
   (utt.relation.items utt 'Token))
   result))


(define (SetTokenToWords utt map)
"
\(SetTokenToWords UTT MAP\)

Replaces the words of the utterance according the new mapping in
MAP. This MAP must be a list of lists, where each sublist contains a
token, followed by zero or more words associated with this token.

If the tokens in the utterance do no match the words in the mapping,
an error message will be produced, and the function will return
nil. Otherwise, it returns t.

Warning: the existing mapping will be deleted.

Example of MAP format 
(NB all atoms have to be quoted, which cannot be shown in a docstring!):

\(\(formule formule\)
  \(I een\)\)

See also: GetTokensToWords
"
(let ((aligned t))
  ;; delete previous words
  ;; we cannot use utt.relation.clear because that leafs 
  ;; the items in the Token relation untouched
  (mapcar
   (lambda (word)
     (item.delete word))
   (utt.relation.items utt 'Word))
  ;; FIX-ME: if the tokens turn out to be incorrectly aligned
  ;; to the words, the words are already deleted...

  ;; create new words
  (mapcar
   (lambda (token)
     (if aligned 
	 (if (and (not (item.relation token 'Word))
		  (string-equal (PunctuatedToken token)
				(caar map)))
	     ;; tokens are still aligned
	     (mapcar
	      (lambda (word)
		(item.relation.append_daughter
		 token
		 'Token
		 (utt.relation.append utt 'Word (list word))))
	      (cdr (car map)))
	     ;; tokens are not aligned!
	     (begin
	       (set! aligned nil)
	       (format stderr "Error: tokens in utterance and mapping are different!\n")
	       (format stderr "token in utterance: %s\n" (PunctuatedToken token)) 
	       (format stderr "token in mapping: %s\n" (caar map)))))
     (set! map (cdr map)))
   (utt.relation.items utt 'Token))
  aligned))



;;;-----------------------------------------------------------------------------
;;; Retrieving and modifying the intonation of an utterance
;;; word by word
;;;-----------------------------------------------------------------------------


(define (GetIntonByWord utt)
"
\(GetIntonByWord UTT\)
 
Retrieves the intonation of an utterance as a list of lists, where
each sublist contains a word, followed by zero or more intonational
events associated with this word.
 
Example of output format \(NB all atoms have to be quoted!\):

\(\(een %L\)
 \(voorbeeld  H*L L%\)\)
 
See also: SetIntonByWord GetIntonBySyl
"
(let (wordint result)
  ;; for all graphemic words
  (mapcar
   (lambda (word)
     (set! wordint nil)
     ;; make a list of all intonational events associated with this
     ;; word by means of the Word-Int relation 
     ;; NB we assume that the order of boundary tones and pitch
     ;; accents in the Intonation relation is correct
     (mapcar
      (lambda (intev)
	;; (format t "%s\n" (item.name intev))
	(set! wordint (append wordint
			      (list (item.name intev)))))
      (item.relation.daughters word 'Word-Int))
     ;; cons the word to the list of intonation symbols, and append
     ;; this to the result list
     (set! result (append result
			  (list (cons (PunctuatedWord word) wordint)))))
   (utt.relation.items utt 'Word))
  result))



(define (SetIntonByWord utt annot)
"
\(SetIntonByWord UTT ANNOT\)

Replaces the intonation of the utterance according the annotation in
ANNOT.  ANNOT must be a list of lists, where each sublist contains a
word, followed by zero or more intonational events associated with
this word. 

If the words in the utterance do no match the words in the annotation,
an error message will be produced, and the function will return
nil. Otherwise, it returns t.

Warning: the existing intonation will be deleted.

Example of ANNOT format \(NB all atoms have to be quoted!\):

\(\(een %L\)
 \(voorbeeld H*L L%\)\) 

See also: SetIntonByWord GetIntonBySyl
"
(let ((aligned t)
      intev
      final_bnd)
  ;; turn utterance into a flat liner :-)
  (utt.relation.clear utt 'Intonation)
  (utt.relation.clear utt 'Word-Int)
  (mapcar
   (lambda (word)
     ;; take the lazy way: just add every word to Word-Int,
     ;; whether it has intonation or not
     (utt.relation.append utt 'Word-Int word)
     ;; the acc feature will updated according to whether a pitch
     ;; accent is associated with a word; 
     ;; initially, all words are unaccented
     (item.remove_feature word "acc")
     (set! final_bnd nil)
     (if aligned 
	 (if (string-equal (PunctuatedWord word)
			   (caar annot))
	     (begin
	       (mapcar 
		(lambda (intev)
		  (cond
		   ;; an initial boundary tone
		   ((member_string intev ToDIInitBnd)
		    (item.relation.append_daughter
		     word
		     'Word-Int
		     (utt.relation.append utt 'Intonation (list intev))))
		   ;; a pich accent
		   ((member_string intev ToDIAcc)
		     (begin
		       ;; also mark the word as accented
		       (item.set_feat word 'acc '+) 
		       (item.relation.append_daughter
			word
			'Word-Int
			(utt.relation.append utt 'Intonation (list intev)))))
		   ;; a final boundary tone
		   ((member_string intev ToDIFinalBnd)
		     (begin
		       (set! final_bnd t) 
		       (item.relation.append_daughter
			word
			'Word-Int
			(utt.relation.append utt 'Intonation (list intev)))))
		   (t
		    (format stderr 
			    "Warning: %s is not a ToDI symbol, and is ignored!\n" 
			    intev))))
		(cdr (car annot))))
	     (begin
               ;;; words are not aligned!
	       (set! aligned nil)
	       (format stderr "Error: words in utterance and annotion are different!\n")
	       (format stderr "word in utterance: %s\n" (item.name word)) 
	       (format stderr "word in annotion: %s\n" (caar annot)))))

     ;; *** Old approach ***
     ;; And now for some fancy stuff with the breaks:
     ;; - if the break is medium, but is no longer accompanied by a boundary tone,
     ;;   we turn it into a light break
     ;; - if the break is light, but accompanied by a boundary tone,
     ;;   we turn it into a medium break
     ;; In all other cases (e.g. heavy breaks), we don't touch it
     ;;
     ;; This is just patch work, until we have found a solution that more satisfactory.
     ;; It may introduce light breaks, or heavy breaks without any intonational marking
     ;;      (if (not (string-equal (item.feat word "pbreak") "heavy"))
     ;; 	 (if final_bnd
     ;; 	     (item.set_feat word "pbreak" "medium")
     ;; 	     (item.set_feat word "pbreak" "light")))

     ;; *** Current approach ***
     ;; For the time being, we only predict utterance-internal medium breaks, 
     ;; and utterance-final heavy breaks
     ;; The medium breaks correspond one-to-one to the boundary tones.
     ;; This is still far from ideal...
     (if (not (string-equal (item.feat word "pbreak") "heavy"))
	 (if final_bnd
	     (item.set_feat word "pbreak" "medium")
	     (item.remove_feature word "pbreak")))
	 
     (set! annot (cdr annot)))
   ;; check if utterance has too many words?
   (utt.relation.items utt 'Word))
  aligned))


;;;-----------------------------------------------------------------------------
;;; Retrieving and modifying the intonation of an utterance
;;; syllable by syllable
;;;-----------------------------------------------------------------------------

;;; Note: this code is not used by Nintens yet, and is not tested at all


(define (GetIntonBySyl utt)
"
\(GetIntonBySyl UTT\)

Retrieves the intonation of an utterance as a list of lists, where
each sublist contains a word, followed by one or more pairs that
represent the syllables of the word. Each pair consist of list of the
syllable's segments, and a list of intonational events associated with
this syllable.

Example of output format \(NB all atoms have to be quoted!\):

\(\(een 
  \(\(e n\) \(%L\)\)\)
 \(voorbeeld  
  \(\(v o r\) \(H*L\)\)
  \(\(b e l t\) \(L%\)\)\) \) 

See also: SetIntonBySyl GetIntonByWord
"
(let (sylseg sylint pairs result pword1 pword2 foot syl onc seg)
  ;; for all graphemic words
  (mapcar
   (lambda (word)
     (set! wordint nil)
     (set! pairs nil)
     ;; for all daughters of this word, 
     ;; i.e prosodic words of level 1
     (mapcar
      (lambda (pword1)
	;; for all daughters of this prosodic word, 
	;; i.e prosodic words of level 2
	(mapcar
	 (lambda (pword2)
	   ;; for all daughters of the prosodic words, i.e. feet
	   (mapcar
	    (lambda (foot)
	      ;; for all daughters of the foot, i.e. syllables
	      (mapcar
	       (lambda (syl)
		 ;; get a list of all segments in this syllable
		 (set! sylseg nil)
		 (mapcar
		  (lambda (onc)
		    (set! sylseg 
			  (append sylseg
				  (mapcar
				   (lambda (seg)
				     (item.name seg))
				   (item.relation.daughters onc 'ProsTree)))))
		  (item.relation.daughters syl 'ProsTree))
		 ;; get a list of all the intonation symbols 
		 ;; attached to this syllable in the relation Syl-Int
		 (set! sylint (GetIntOfSyl syl))
		 ;; create a pair ( (segments) (intonation) )
		 (set! pairs (append pairs (list (list sylseg sylint)))))
	       (item.relation.daughters foot 'ProsTree)))
	    (item.relation.daughters pword2 'ProsTree)))
	 (item.relation.daughters pword1 'ProsTree)))
      (item.relation.daughters word 'Word-Pros))
     ;; cons the word to the list of pairs,
     ;; and append this to the result list
     (set! result (append result (list (cons (item.name word) pairs)))))
   (utt.relation.items utt 'Word))
  result))


(define (SetIntonBySyl utt annot)
"
\(SetIntonBySyl UTT ANNOT\)

Replaces the intonation of the utterance according the annotation in
ANNOT.  ANNOT must be a list of lists, where each sublist contains a
word, followed by one or more pairs that represent the syllables of
the word. Each pair consist of list of the syllable's segments, and a
list of intonational events associated with this syllable.

If the words in the utterance do no match the words in the annotation,
an error message will be produced, and the function will return
nil. Otherwise, it returns t. The number or syllables and their
segmental content is not checked.

Warning: the existing intonation will be deleted.

Example of ANNOT format (NB all atoms have to be quoted!):

\(\(een 
  \(\(e n\) \(%L\)\)\)
 \(voorbeeld  
  \(\(v o r\) \(H*L\)\)
  \(\(b e l t\) \(L%\)\)\) \) 

See also: GetIntonBySyl SetIntonByWord
"
;; The semantics of the ToDI specification is not checked, so you can
;; e.g. attach an initial boundary tone to final syllable of an word,
;; and all sorts of stupid things :-)
(let ((aligned t)
      (todisyms (append ToDIAcc ToDIInitBnd ToDIFinalBnd))
      (syl (utt.relation.first utt 'Syllable))
      word
      pair)
  (utt.relation.clear utt 'Intonation)
  (utt.relation.clear utt 'Syl-Int)
  ;; for all words in utt
  (mapcar
   (lambda (word)
     (if aligned
	 (if (string-equal (item.name word) (caar annot))
	     ;; There is no further error checking. We assume that the
	     ;; number of syllables in the annotation matches the number
	     ;; of those of in the utterance.
	     ;; We're not going to check if the segments match either,
	     ;; because it doesn't really matter for current purposes.
	     (begin
	       ;; for all syllable + intonation pairs
	       (mapcar 
		(lambda (pair)
		  ;; for all 
		  (mapcar 
		   (lambda (intev)
		     (if (member_string intev todisyms) 
			 (SetSylInton utt syl intev)
			 (format stderr 
				 "Warning: %s is not a ToDI symbol, and is ignored!\n" 
				 intev)))
		   (cadr pair))
		  (set! syl (item.next syl)))
		(cdr (car annot))))
	     (begin
	       (set! aligned nil)
	       (format stderr "Error: words in utterance and annotion are different!\n")
	       (format stderr "word in utterance: %s\n" (item.name word)) 
	       (format stderr "word in annotion: %s\n" (caar annot)))))
     (set! annot (cdr annot)))
   (utt.relation.items utt 'Word))
  aligned))



;;;-----------------------------------------------------------------------------
;;; Checking validity of ToDI intonation
;;;-----------------------------------------------------------------------------


;;; FIX-ME: 
;;; write this function


(define (CheckInton utt)
"
\(CheckInton UTT\)

Check validity of ToDI intonation in UTT.
Return t or a string containing error message.
"
  t)
   

;;;-----------------------------------------------------------------------------
;;; Retrieving F0 targets
;;;-----------------------------------------------------------------------------

(define (GetF0Targets utt)
"
\(GetF0Targets UTT\)

Return a list of all F0 targets of UTT.
"
  (mapcar
   (lambda (i)
     (item.features i))
   (utt.relation.leafs utt 'Target)))


;;;-----------------------------------------------------------------------------
;;; Some utitilty functions
;;;----------------------------------------------------------------------------- 


(define (GetIntOfSyl syl)
"
\(GetIntOfSyl SYL\)

Return a list of strings, representing all boundaries and pitch
accents from the relation Intonation that are attached to SYL over the
relation Syl-Int.
"
  (mapcar
   (lambda (int)
     (item.name int))
   (item.relation.daughters syl 'Syl-Int)))


(define (utt.relation.clear utt rel)
"
\(utt.relation.clear UTT REL\)

Remove all items from relation REL in UTT.
"    
  ;; pretty destructive :-)
  (utt.relation.delete utt rel)
  (utt.relation.create utt rel))


(define (PunctuatedWord word)
"
\(PunctuatedWord WORD\)

Return a word with its prepunctuation and \(post\)punctuation
"
  (let ((s (string-append 
	    (item.feat (item.relation.parent word 'Token) 'prepunctuation)
	    (item.name word)))
	(postpunc (item.feat word "R:Token.parent.punc")))
    (cond
     ;; no postpunctuation
     ((string-equal postpunc "0")
      s)

     ;; word is part of an abbreviation
     ;; that is non-final and not followed by other punctuation
     ;; FIX-ME: this does not cover cases like 
     ;; "niet roken a.u.b., want ..."
     ((and (string-equal postpunc ".")
	   (item.next word)
	   (string-equal (item.feat word "R:Token.parent.tokpos") "abbrev"))
      s)
       
     ;; default to adding the postpunctuation
    (t
     (string-append s postpunc))
    )))



;; FIX-ME: this function is also in net_nl_syntax_amazon.scm

(define (nl::GetTextString utt)
"
\(nl::GetTextString UTT\)

Returns the original input text for utterance UTT. This text is
reconstructed from the tokens, plus the features whitespace,
punctuation, and prepunctuation.
"
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
	       (set! s (string-append s (item.feat token 'punc)))))))
   (utt.relation.items utt 'Token))
  s))



(define (PunctuatedToken token)
"
\(PunctuatedToken TOKEN\)

Return a token with its prepunctuation and \(post\)punctuation,
but without the whitespace
"
  (let ((s (string-append ; (item.feat token 'whitespace)
			  (item.feat token 'prepunctuation)
			  (item.name token))))
	;; punc feature is not always present
	(if (not (string-equal (item.feat token 'punc) "0"))
	    (set! s (string-append s (item.feat token 'punc))))
	s))




(provide 'net_nl_nintens)