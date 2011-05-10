;;; $Id: net_nl_pos_mbt.scm,v 1.5 2004/04/21 16:06:28 emarsi Exp $
;;;
;;; by Joop Kerkhoff & Erwin Marsi
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


;;; memory-based part-of-speech tagger for Dutch


(defvar nl::trace_mbt nil
  "
If true, trace the memory-based tagging module.
")


(define (nl::memory_based_tagging utt)
"
\(nl::memory_based_tagging UTT\)

Tag tokens from UTT with the memory-based part-of-speech tagger for Dutch.
"
  (let ((inpstr (nl::token_string utt))
	(token (utt.relation.first utt 'Token))
	tok tagstr toktag) 
    (set! tagstr (Mbt.tag nl::mbt inpstr))
    (if nl::trace_mbt
	(begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "Memory-based tagger\n")
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n")
	(format t "*** Input string:\n\n%s\n\n" inpstr)
	(format t "*** Output string:\n\n%s\n" tagstr)
	(format t "*** Tagged tokens:\n\n")))
    ;; Iterate over the token-tag substring in the tagged string
    ;; returned by Mbt. Example:
    ;; "Hallo/Int mijn/Pron(bez,1,ev,neut,attr) naam/N(soort,ev,neut)
    ;; is/V(hulpofkopp,ott,3,ev) Erwin/N(eigen,ev,neut)12 Marsi//N(eigen,ev,neut)22"
    (while (and token
		(not (string-matches tagstr "<utt>.*")))
	   (set! toktag (string-before tagstr " "))
	   ;; Check if token part of the token-tag substring matches
	   ;; the current token in our utterance. Otherwise, the
	   ;; substring is punctuation, and we skip it.
	   (set! tok (string-before toktag "/"))
	   ;; replace '| by '/' again
	   ;; cf. nl::token_string
	   (while (string-matches tok ".*|.*")
		    (set! tok (string-append (string-before tok "|")
					     "/"
					     (string-after tok "|"))))
	   (if (string-equal tok (item.name token))
	       (begin
		 (nl::parse_tagged_token toktag token)
		 ;; move on to the next token
		 (set! token (item.relation.next token 'Token))))
	   ;; move on to the next substring
	   (set! tagstr (string-after tagstr " "))))
  utt)


(define (nl::parse_tagged_token toktag token) 
"
\(nl::parse_tagged_token TOKTAG TOKEN\) 

Parse token-tag string in TOKTAG and insert the major tag 
and additional attributes as features on TOKEN. 
"
;; Examples of token-tag strings returned by Mbt:
;; Hallo/Int
;; naam/N(soort,ev,neut)
;; Erwin/N(eigen,ev,neut)12
  (let (tag pos posatt)
    ;; optionally, trace mbt output
    (if nl::trace_mbt
	(format t "%s\n" toktag))
    ;; get the tag, which is separated from the word by a double
    ;; slash, and by a single slash for known words
    (if (string-matches toktag ".*//.*")
	(set! tag (string-after toktag "//"))
	(set! tag (string-after toktag "/")))
    ;; check for attributes
    (if (string-matches tag ".*(.*).*")
	(begin 
	  ;; get major POS label
	  (set! pos  (string-before tag "("))
	  (item.set_feat token 'pos pos)
	  (set! tag (string-after tag "("))
	  ;; get POS attributes
	  (set! posatt (string-before tag ")"))
	  (item.set_feat token 'pos-attributes posatt)
	  (set! tag (string-after tag ")"))
	  ;; optionally, get digits that indicate the parts of a
	  ;; proper noun expression (e.g. )
	  (if (not (string-equal tag ""))
	      (item.set_feat token 'part-of-proper tag))
	  )
	;; tag without attributes (e.g. Int)
	(item.set_feat token 'pos tag))))


(define (nl::token_string utt)
"
\(nl::token_string UTT\)

Returns tokens and punctuation from UTT as a string
that can be tagged by Mbt.
"
;; Mbt expects a string of tokens and interpunction,
;; e.g. "Hallo , ik ben een voorbeeldzin ."
  (let ((tokstr ""))    
    (mapcar
     (lambda (token)
       (if (not (item.relation token 'Word))
	   (begin
	     ;; add prepunctuation when present
	     (if (not (string-equal (item.feat token 'prepunctuation) ""))
		 (set! tokstr (string-append tokstr (item.feat token 'prepunctuation) " ")))
	     (set! tokstr (string-append tokstr (item.name token) " "))
	     ;; add postpunctuation when present
	     (if (not (string-equal (item.feat token 'punc) "0"))
		 (set! tokstr (string-append tokstr (item.feat token 'punc))))
	     (set! tokstr (string-append tokstr " "))
	     ;; '/' is used by Mbt as separator, so replace by '|'
	     (while (string-matches tokstr ".*/.*")
		    (set! tokstr (string-append (string-before tokstr "/")
						"|"
						(string-after tokstr "/")))))))
     (utt.relation.items utt 'Token))
    tokstr))


(defvar nl::mbt_settings  '("light" "default" "optimal")
  "
List of Mbt settings, corresponding to directories in
lib/net_nl/mbt-ibs. Current settings are 'light', 'default or
'optimal. The light setting consumes less memory, but is also less
accurate.  The optimal setting consumes a lot of memory, but gives the
best performance.
")


(define (nl::setup_mbt setting)
"
\(nl::setup_mbt SETTING\)

Setup the Memory-Based Tagger for Dutch by reading the settingsfile
and loading the appropriate instance bases. The values allowed for
SETTING are defined in the list nl::mbt_settings.
"
  (if (member_string "NextensMbt" *modules*)
      (if (member_string setting nl::mbt_settings)
	  (begin
	    (Param.set 'POS_Method 'nl::memory_based_tagging)
	    ;; init with new settings and instance bases;
	    ;; by default, MBT searches for its required files in the same
	    ;; directory as where the settingsfile is found 
	    (set! nl::mbt (Mbt.init (string-append "-s " 
						   (path-append libdir 
								"net_nl/mbt-ibs" 
								setting
								"wotan.all.tag.settings")))))
	  (format stderr "Error: unknown Mbt setting: %s\n" setting))
      (format stderr "Error: Festival was not compiled with the NextensMbt module\n")))
