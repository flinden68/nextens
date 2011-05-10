;;; $Id: net_nl_token.scm,v 1.9 2005/10/10 13:54:25 emarsi Exp $
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


;;; Token rules to map (tokens to words)


(set! token.unknown_word_name "x")

(define (nl::token_to_words token name)
"
\(nl::token_to_words TOKEN NAME)

Returns a list of words for the NAME from TOKEN.  
"
(let ((entries (lex.lookup_all name)))
  (if entries

      ;; token found in lexicon
      ;; ----------------------
      (cond  
       ;; multiple entries, so word is a homograph
       ((> (length entries) 1)
	(nl::homograph_token_to_words token name entries))

       ;; single entry for a multi-word token
       ;; e.g. '("e.d." nil nil (en dergelijke))'
       ((not (car (cdr (cdr (car entries)))))
	(car (cdr (cdr (cdr (car entries))))))

       ;; single entry with nil sense
       (t
	(list name)))

      ;; unknown token
      ;; -------------
      (cond
       ;; to save time, we check for non-funny words first, that is,
       ;; only alphanumeric symbols with at least one vowel
       ((string-matches name "[A-Za-z]*[AEIOUaeiouÀÁÂÄÈÉÊËÌÍÎÏÒÓÔÖÙÚÛÜáàäâèéëêíìïîóòöôúùüû]+[A-Za-z]*")
	(list name))

       ;; 's plural suffix
       ((string-matches name ".*'s")
	(item.set_feat token 'plural-s '+)
	(append (nl::token_to_words token (string-before name "'"))))

       ;; dash used to form compounds (e.g. "zwart-wit")
       ;; or in hyphenation
       ;; (but not "--" or "-10")
       ((string-matches name "[^-]+-.*")
	(append (nl::token_to_words token 
				    (string-before name "-"))
		(nl::token_to_words token 
				    (string-after name "-")))) 

       ;; url
       ((string-matches name ".*://.*")
	(nl::url token name))

       ;; slash used to form compounds (e.g. "en/of")
       ((string-matches name ".+/.+")
	(append (nl::token_to_words token 
				    (string-before name "/")) 
		(nl::token_to_words_with_sense "/" 'char)
		(nl::token_to_words token 
				    (string-after name "/"))))
       
       ;; abbreviations with dots
       ((and (string-matches (item.feat token 'punc) "\\..*")
	     (lex.lookup_all  (string-append name ".")))
	;; move dot from punc feature to token name
	(set! name  (string-append name "."))
	(item.set_name token name)
	(item.set_feat token 'punc (string-after (item.feat token 'punc) "."))
	(item.set_feat token 'tok_pos 'abbrev)
	;; now try again (lex may contain multiple entries)
	(nl::token_to_words token name))
       
       ;; email address
       ((string-matches name "[^@]+@.[^@]+")
	(nl::email_address token name))
       
       ;; numbers
       ((string-matches name "[-]?[0-9.]+,?[0-9]*")
	(nl::number token name))

       ;; empty string
       ((string-equal name "")
	nil)

       ;; last resort: spell it
       (t
	(format t "Warning: I'm going to spell this word: %s\n\n" name)
	(nl::spell_out_string token name))))))


(define (nl::token_to_words_with_sense name sense)
"
\(nl::token_to_words_with_sense NAME SENSE) 

Returns a list of words for NAME with particular SENSE.  
Assumes that the lexicon contains an entry for this combination 
of name and sense.
"
  (let ((entry (lex.lookup name sense)))
    (cond
     ;; not found in lexicon with this sense
     ((not (string-equal (cadr entry) 'char))
      (format stderr "Warning: unknown token %s\n\n" name)
      (list token.unknown_word_name))
      
     ;; token maped to multiple words
     ;; word senses may be specified in the entry 
     ((not (car (cdr (cdr entry))))
      (car (cdr (cdr (cdr entry)))))

     ;; token mapped to single word
     ;; word sense is added as a feature
     (entry
      (list (list (list 'name name)
		  (list 'sense sense)))) )))


(define (nl::homograph_token_to_words token name entries)
;; FIX-ME: here we should set the correct sense
  (cond
   ;; weird case
   ;; (("/" char nil (schuine streep)) ("/" math nil (gedeeld door)))
   ((string-equal name "/") 
    (car (last (lex.lookup "/" 'char))))
   ;; genitive 's vs. character s 
   ((string-equal name "s")
    (if (string-matches (item.feat token 'prepunctuation) ".*'")
	(begin
	  (item.set_feat (item.next token) 'genitive-s '+)
	  '())
	'(((name s)(sense char)))))
   ;; reduced forms
   ((string-matches name "[kmntr]")
    (if (string-matches (item.feat token 'prepunctuation) ".*'")
	(list (list (list 'name name) (list 'sense 'red)))
	(list (list (list 'name name) (list 'sense 'char)))))
   ;; dash: for the time being, let's assume that an dash 
   ;; surounded by whitespace is similar to a comma, and maps to silence
   ((string-equal name "-")
    ())
   (t
    (list name))))


(define (nl::spell_out_string token name)
  ;; start and end punctuation is not spelled out
  ;; in cases like '#', (!), etc.
  ;; but that is probably the right 
  (let ((punc (item.feat token 'punc)) 
	(words '()))
    (set! name (string-append (item.feat token 'prepunctuation) name))
    (if (not (eq punc '0))
        (set! name (string-append name punc)))
    (mapcar 
     (lambda (char)
       (set! words (append words (nl::token_to_words_with_sense char 'char))))
     (symbolexplode name))
    words))


(define (nl::email_address token name)
  (item.set_feat token 'tok_pos 'email)
  (let ((result '()))
    (mapcar 
     (lambda (w)
       (set! result (append result (nl::token_to_words token w))))
     (nl::parse_dotted_string (string-before name "@")))
    (set! result (append result '(((name @) 
				   (sense email)))))
    (mapcar
     (lambda (w)
      (set! result (append result (nl::token_to_words token w))))
     (nl::parse_dotted_string (string-after name "@")))
    result))


(define (nl::url token name)
  (item.set_feat token 'tok_pos 'url)
  (let ((result (nl::token_to_words token (string-before name "://"))))
    (set! result (append result '(dubbele punt slash slash)))
    (mapcar
     (lambda (w1)
       (mapcar
	(lambda (w2)
	  (if (string-matches w2 "~.*")
	      (set! result (append result 
				   '(tilde)
				   (nl::token_to_words token (string-after w2 "~"))))
	      (set! result (append result (nl::token_to_words token w2)))))
	(nl::parse_dotted_string w1)))
     (nl::parse_slashed_string (string-after name "://")))
    result))


(define (nl::parse_slashed_string s)
  (let (l)
    (if (string-matches s "/.*")
	(begin
	  (set! l '("slash"))
	  (set! s (string-after s "/"))))
    (while (string-matches s ".*/.*")
	   (set! l (append l (list (string-before s "/") "slash")))
	   (set! s (string-after s "/")))
    (if (not (string-equal s ""))
	(set! l (append l (list s))))
    l))

(define (nl::parse_dotted_string s)
  (let (l '())
    (while (string-matches s ".+\\..+")
	   (set! l (append l (list (string-before s ".") ".")))
	   (set! s (string-after s ".")))
    (set! l (append l (list s)))
    l))


(define (nl::number token name)
  (let (words after)
  ;; minus
    (if (string-matches name "[-].*")
	(begin
	  (set! name (string-after name "-"))
	  (set! words '("min"))))
    ;; remove dots
    (while (string-matches name ".*[.].*")
	   (set! name (string-append (string-before name ".")
				     (string-after name "."))))

   (if (string-matches name ".*,.*")
       ;; split on comma
       (begin
	 (set! words (append words
			     (nl::number token (string-before name ","))
			     '("komma")))
	 (set! after (string-after name ","))
	 (if (or (> (length after) 2) 
		 (string-matches after "0.*"))
	     (append words (symbolexplode after))
	     (append words (nl::number_list (symbolexplode after)))))
       ;; else
       (append words (nl::number_list (symbolexplode name))))))
  

(define (nl::number_list digits)
"
\(nl::number_list DIGITS)

Return list of words that pronounce this number
"
(let ((n (length digits)))
  (cond
   ;; 0-9
   ((eq? n 1)
    (list (nl::number_d (car digits))))
   ;; 10-99
   ((eq? n 2)
    (cond
     ;; 10-19
     ((string-equal (car digits) "1")
      (list (nl::number_1d (cadr digits))))
     ;; 20,30,40,..,90
     ((string-equal (cadr digits) "0")
      (list (nl::number_d0 (car digits))))
     ;; 21,22,...29,31,32,...39,... 91,92,...,99
     (t
      (if (string-equal (cadr digits) "1")
	  (list "eenen"
		(nl::number_d0 (car digits)))
	  (list (string-append (nl::number_d (cadr digits)) "en")
		(nl::number_d0 (car digits)))) )))
   ;; 100-999
   ((eq? n 3)
    (if (string-equal (car digits) "1")
	;; 100,101,...,199
	(cons "honderd"
	      (nl::number_list (nl::strip-zeros (cdr digits))))
	;; 200, 201, ..., 999
	(cons (nl::number_d (car digits))
	      (cons "honderd"
		    (nl::number_list (nl::strip-zeros (cdr digits)))))))
   ;; 1000-9999
   ((eq? n 4)
    (if (string-equal (cadr digits) "0")
	(if (string-equal (car digits) "1")
	    ;; 1000-1099
	    (cons "duizend"
		  (nl::number_list (nl::strip-zeros (cdr digits))))
	    ;; 2000-2099,3000-3099,...9000-9099
	    (cons (nl::number_d (car digits))
		  (cons "duizend"
			(nl::number_list (nl::strip-zeros (cdr digits))))))
	;; 1100-1199,2100-2199,...9100-9199
	(append (nl::number_list (list (car digits) (cadr digits)))
		'("honderd")
		(nl::number_list (nl::strip-zeros (cddr digits))))))
   ;; 10000-99999
   ((eq? n 5)
    (append (nl::number_list (list (car digits) (cadr digits)))
	    '("duizend")
	    (nl::number_list (nl::strip-zeros (cddr digits)))))
   ;; 100000-999999
   ((eq? n 6)
    (append (nl::number_list (list (car digits) (cadr digits) (caddr digits)))
	    '("duizend")
	    (nl::number_list (nl::strip-zeros (cdddr digits)))))
   ;; miljoen
   ((eq? n 7)
    (append (nl::number_list (list (car digits)))
	    '("miljoen")
	    (nl::number_list (nl::strip-zeros (cdr digits)))))
   ((eq? n 8)
    (append (nl::number_list (list (car digits) (cadr digits)))
	    '("miljoen")
	    (nl::number_list (nl::strip-zeros (cddr digits)))))
   ((eq? n 9)
    (append (nl::number_list (list (car digits) (cadr digits) (caddr digits)))
	    '("miljoen")
	    (nl::number_list (nl::strip-zeros (cdddr digits)))))
   ;; miljard
   ((eq? n 10)
    (append (nl::number_list (list (car digits)))
	    '("miljard")
	    (nl::number_list (nl::strip-zeros (cdr digits)))))
   ((eq? n 11)
    (append (nl::number_list (list (car digits) (cadr digits)))
	    '("miljard")
	    (nl::number_list (nl::strip-zeros (cddr digits)))))
   ((eq? n 12)
    (append (nl::number_list (list (car digits) (cadr digits) (caddr digits)))
	    '("miljard")
	    (nl::number_list (nl::strip-zeros (cdddr digits)))))
   ;; enumerate
   (t digits)
)))


(define (nl::number_d d)
  (nth (parse-number d)
       '("nul" ((name "een")(sense "Num")) "twee" "drie" "vier" "vijf"  
	 "zes" "zeven" "acht" "negen")))

(define (nl::number_1d d)
  (nth (parse-number d)
       '("tien" "elf" "twaalf" "dertien" "veertien" "vijftien" 
	 "zestien" "zeventien" "achtien" "negentien")))

(define (nl::number_d0 d)
  (nth (- (parse-number d) 2)
       '("twintig" "dertig" "veertig" "vijftig" 
	 "zestig" "zeventig" "tachtig" "negentig")))

(define (nl::strip-zeros digits)
  (if (string-equal (car digits) "0")
      (nl::strip-zeros (cdr digits))
      digits))


(provide 'net_nl_token)
