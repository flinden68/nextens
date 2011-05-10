

(define (nl::test_suite name blocks)
"
\(nl::test_suite NAME BLOCKS\)

Run a suite of test blocks under the heading NAME.
BLOCKS should be a list of nl::test_block items.
Returns the number of failed tests.
"
(let ((failures 0))
  (format t "============================================================\n")
  (format t "Testing suite %s\n" name)
  (format t "============================================================\n")
  (mapcar
   (lambda (block)
     (set! failures (+ failures (eval block))))
   blocks)
  (format t "Suite result: %d failure(s)\n\n" failures)
  failures))


(define (nl::test_block name tests)
" 
\(nl::test_block NAME TESTS\)

Run a block of tests under the heading NAME.
TESTS should be a list of nl::test_synth_text items.
Returns the number of failed tests.
"
(let ((failures 0))
  (format t "------------------------------------------------------------\n")
  (format t "Testing block %s\n" name)
  (format t "------------------------------------------------------------\n")
  (mapcar
   (lambda (test)
     (if (eq? (length test) 3)
	 (set! test (append test '(list))))
     (set! failures (+ failures (eval test))))
   tests)
  (format t "Block result: %d failure(s)\n\n" failures)
  failures))


(define (nl::test_synth_text name text check)
"
\(nl::test_synth_text NAME TEXT CHECK\)

Run a test called NAME, which consist of synthesizing TEXT. 
If a Scheme error ocurs, it is caught, and a 'FAILED EVALUATION' is reported.
CHECK must be either nil or a function which takes an utterance as argument.
If applying CHECK to the utterance fails, a 'FAILED CHECK' is reported.
This hook can be used for arbitrary checks on the resulting utterance.
Returns 0 if the test is passed and 0 otherwise.
"
(format t "Testing %s...\n" name)
(format t "festival>(SynthText \"%s\")\n" text)
(unwind-protect
 (let (utt)
   (if nl::test_sound
       (set! utt (SayText text))
       (set! utt (SynthText text)))
   ;; optionally apply check
   (if (and check
	    (not (apply check (list utt))))
       (begin
	 (format t  "*** FAILED CHECK ***\n\n")
	 1)
       (begin
	 (format t "PASSED\n\n")
	 0)))
 ;; catch SIOD error
 (begin
   (format t  "*** FAILED EVALUATION ***\n\n")
   1)))


(define (nl::load_test fn)
  (load (string-append (path-append nl::test_dir fn) ".scm")))

(defvar nl::test_sound t
  "output sound during testing")

(defvar nl::test_dir 
  (path-append libdir "net_nl/tests/")
  "directory containing test files")


(provide 'net_nl_test)