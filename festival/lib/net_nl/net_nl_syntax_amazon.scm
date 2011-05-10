;;; $Id: net_nl_syntax_amazon.scm,v 1.3 2004/08/18 15:12:12 emarsi Exp $

;;; TODO:
;;; 1. -A option for alphabet file is not used, because it doesn't work on linux
;;; 2. when the main Amazon grammar is timed out, we have to call the backup grammar


(define (nl::amazon utt)
"
\(Amazon UTT\)

Syntactic parsing module. Applies the Amazon parser to utterance UTT.
"
(let ((txt (nl::GetTextString utt))
      (inp_fn (make_tmp_filename))
      (out_fn (make_tmp_filename))
      fd
      cmd)
  ;; preprocessing step: lowercase first char unless first word is a proper noun
  (if (not (string-matches (item.feat (utt.relation.first utt 'Token) 'pos-attributes) ".*eigen.*"))
      (set! txt (string-append
		 (downcase (substring txt 0 1))
		 (substring txt 1 99999))))
  (if nl::amazon_trace
      (begin
	(format t "\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
	(format t "Amazon\n")
	(format t "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n")
	(format t "Max. parsing time: %f second(s)\n" nl::amazon_timeout)
	(if nl::amazon_best
	    (format t "Search for best parse is on.\n"))
	(format t "\nInput text to Amazon:\n%s\n\n" txt)))
  (utt.relation.create utt 'SynTree)
  (set! fd (fopen inp_fn "w"))
  ;; write the input text to a temporary file
  ;; add end-of-sentence marker
  (format fd "%s $EOS$" txt)
  (fclose fd)
  (if (string-matches *ostype* ".*CYGWIN.*")
      (set! cmd (format nil "%s -A amazon-alphabet.txt -P1 -b -T%f %s <%s >%s"
			nl::amazon_exec
			nl::amazon_timeout
			(if nl::amazon_best "-B" "")
			inp_fn
			out_fn))
      ;; Amazon's -A option for an alphabet file is not available on linux,
      ;; so we use 'tr' to translate chars with diacritics to bare chars.
      ;; (assuming 'tr' is in the path)
      ;; Translation table (octal):
      ;; ú -> u        372
      ;; ä -> a        344
      ;; è -> e        350
      ;; é -> e        351
      ;; ê -> e        352
      ;; ë -> e        353
      ;; ï -> i        357
      ;; ó -> o        363
      ;; ö -> o        366
      ;; ü -> u        374
      (set! cmd (format nil "tr  '%s' '%s' <%s | %s -P1 -b -T%f %s >%s"
			"\\372\\344\\350\\351\\352\\353\\357\\363\\366\\374"
			"uaeeeeioou"
			inp_fn
			nl::amazon_exec
			nl::amazon_timeout
			(if nl::amazon_best "-B" "")
			out_fn)))
  (if nl::amazon_trace
      (format t "System call to Amazon:\n%s\n\n" cmd))
  (system cmd)
  (nl::BuildTree utt out_fn)
  (delete-file inp_fn)
  (delete-file out_fn)
  ))
    
	       

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
       

(define (nl::BuildTree utt out_fn)
"
\(nl::BuildTree UTT OUT_FN\)

Read the output of the Amazon parser, which is labelled brackets
structure, from the file OUT_FN. Build a corresponding syntactic tree
in the relation SynTree of utterance UTT.
"
(let ((token (utt.relation.first utt 'Token))
      (label "")
      (char " ")
      (output "")
      feats
      fd
      node)
  (set! fd (fopen out_fn "r"))
  ;; read from file, one character at a time
  (while (fread char fd)
	 (set! output (string-append output char))
	 (cond
	  ;; ------------------------------------------------------------
	  ;; Case 1:  we've found the beginning of a new node
	  ;; ------------------------------------------------------------
	  ((string-equal char "{")
	   (begin
	     ;; split features from label
	     (if (string-matches label ".*\(.*\)")
		 (begin
		   (set! feats (string-after (string-before label ")") "("))
		   (set! label (string-before label "(")))
		 (set! feats nil))
	     (if nl::amazon_trace
		 (begin
		   (format t "Creating a new non-terminal with label %s" label)
		   (if feats
		       (format t " and features %s\n" feats)
		       (format t "\n"))))
	     (set! node
		   (if node
		       ;; if we've visited a node before, then this node
		       ;; becomes its daughter in the relation SynTree
		       (item.relation.append_daughter node 'SynTree 
						      (list label))
		       ;; otherwise, this is the root node, and we only have
		       ;; to add it to the relation SynTree
		       (utt.relation.append utt 'SynTree (list label))))
	     (if feats
		 (item.set_feat node 'feats feats))
	     (set! label "")))
	  ;; ------------------------------------------------------------
	  ;; Case 2: we've found the end of the current node
	  ;; ------------------------------------------------------------
	  ((string-equal char "}")
	   (begin
	     (if (string-matches label "\".*\"")
		 ;; we've found a terminal
		 (begin
		   ;; First, some ugly code to remove the
		   ;; surrounding double quotes
		   (cond 
		    ((string-equal label "\"\"-\"")
		     (set! label "\"-"))
		    ((string-equal label "\"-\"\"")
		     (set! label "-\""))
		    (t
		     (set! label (string-before (string-after label "\"") "\""))))

		   (if (and (item.next token)
			    (string-equal (downcase (item.name token))
					  (downcase label)))
		       ;; terminal corresponds to a token
		       (begin
			 (if nl::amazon_trace
			     (format t "Appending existing terminal %s\n" label))
			 ;; append the corresponding token as the terminal 
			 (item.relation.append_daughter node 'SynTree token)
			 ;; and move to the next token
			 (set! token (item.relation.next token 'Token)))
		       ;; otherwise, terminal is a punctuation symbol or end-of-sentence marker,
		       ;; and does not correspond to a token
		       (begin
			 (if nl::amazon_trace
			     (format t "Creating a new terminal %s\n" label))
			 ;; We add it as a new terminal in SynTree
			 (item.append_daughter node 
					       (utt.relation.append utt 'SynTree 
								    (list label)))))))

	     ;; move up to the parent of the current (non-)terminal node
	     (set! node (item.relation.parent node 'SynTree))
	     (set! label "")))
	  ;; ------------------------------------------------------------
	  ;; Case 3: we've another character of a label or terminal
	  ;; ------------------------------------------------------------
	  (t
	   (set! label (string-append label char)))
	  ))
  (fclose fd)
  (if nl::amazon_trace
      (if (string-matches output "START.*")
	  (format t "\nOutput of Amazon:\n%s\n" output)
	  (format t "\nWarning: Amazon produced no output\n"))) 
  ))




;;; ------------------------------------------------------------
;;; Global variables 
;;; ------------------------------------------------------------

(defvar nl::amazon_exec
  (cond 
   ((string-matches *ostype* ".*Linux.*")
    (path-append (cadr etc-path) "amazon")) 
   ((string-matches *ostype* ".*CYGWIN.*")
    (path-append (cadr etc-path) "amazon.exe"))
   (t
    (format t "Error: can't find Amazon executable for this OS type!")))
"Path to Amazon executable, which depends on the OS (Linux or
Windows+Cygwin).")


(defvar nl::amazon_timeout 1.0
"This variable corresponds to the -T option of Amazon. It specifies
the maximum total parse time in seconds.")


(defvar nl::amazon_best nil
"This variable corresponds to the -B option of Amazon. If true, Amazon
will search its parse results parsings for the best penalty level.")


(defvar nl::amazon_trace nil
"If true, trace syntactic parsing by Amazon.")


(provide 'net_nl_syntax_amazon)

