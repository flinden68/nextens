;;; $Id: net_nl_ad_mbrola.scm,v 1.7 2004/04/27 16:26:32 emarsi Exp $
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


;;; A Dutch female voice (Ineke Burema) 
;;; using the nl3 Mbrola diphone database


;;;  Add directory containing shared code for net_nl voices to load-path
(if (probe_file (path-append libdir "net_nl/"))
    (set! load-path (cons (path-append libdir "net_nl/") load-path))
    (begin
      (format stderr "Error: can't find the net_nl scheme files in\n")
      (format stderr "%s\n" (path-append libdir "net_nl/"))
      (format stderr "Either the voice isn't linked into Festival\n")
      (format stderr "or you are starting Festival in the wrong directory\n")
      (error)))

;;; Try to find out where we are
(if (assoc 'net_nl_ad_mbrola voice-locations)
    (defvar net_nl_ad_mbrola_dir 
      (cdr (assoc 'net_nl_ad_mbrola voice-locations)))
    ;;; Not installed in Festival yet so assume running in place
    (defvar net_nl_ad_mbrola_dir (pwd)))

;;;  Add the directory containing code for net_nl_ad_mbrola to load-path
(if (probe_file (path-append net_nl_ad_mbrola_dir "festvox/"))
    (set! load-path (cons (path-append net_nl_ad_mbrola_dir "festvox/") load-path))
    (begin
      (format stderr "#Error: can't find the net_nl_ad_mbrola scheme files in\n")
      (format stderr "%s\n" (path-append net_nl_ad_mbrola_dir "festvox/"))
      (format stderr "Either the voice isn't linked into Festival\n")
      (format stderr "or you are starting Festival in the wrong directory\n")
      (error)))

;;; support for the Nintens GUI
;;; FIX-ME: Nintens should make this require itself 
(require 'net_nl_nintens)


(define (voice_net_nl_ad_mbrola)
"
\(voice_net_nl_ad_mbrola\)

Set speaker to AD in Dutch.
"
  (format t "Initializing voice net_nl_ad_mbrola\nPlease wait...\n")

  (voice_reset)
  (net_nl_ad_voice_backup)

  ;; define some new utterance types
  (require 'net_nl_synthesis)
  (net_nl_utterance_types)

  (Param.set 'Language 'net_nl)

  ;; ------------------------------------------------------------
  ;; Token module: 
  ;; tokenisation
  ;; ------------------------------------------------------------

  (require 'net_nl_token)
  (Param.set 'Token_Method 'Token_Any)
  (set! token_to_words nl::token_to_words)

  ;; ------------------------------------------------------------
  ;; POS module:
  ;; part-of-speech tagging
  ;; ------------------------------------------------------------

  ;; -- option 1 -- guess POS
  ;; use simple heuristics to distinguish  function from content words
  ; (require  'net_nl_pos_guess)
  ; (set! pos_lex_name nil)
  ; (set! guess_pos nl::guess_pos)

  ;; -- option 2 -- MBT
  ;; POS tagging with the Memory-based tagger for Dutch
  (require 'net_nl_pos_mbt)
  ;; setup MBT with default setting 
  ;; (alternatives are 'light and 'optimal
  (nl::setup_mbt 'default) 
  ;; uncomment this to trace the tagger
  ; (nl::trace_mbt t)

  ;; ------------------------------------------------------------
  ;; Syntax module:
  ;; syntactic parsing
  ;; ------------------------------------------------------------
  
  (require 'net_nl_syntax)

  ;; -- Option 1 -- No syntax
  (Param.set 'Syntax_Method nil)

  ;; -- Option 2 -- Amazon syntactic parser
  ; (require 'net_nl_syntax_amazon)
  ;(Param.set 'Syntax_Method 'nl::amazon) 
  ;(set! nl::amazon_trace t)

  ;; ------------------------------------------------------------
  ;; Phrasify module:
  ;; phrase break prediction
  ;; ------------------------------------------------------------

  (require 'net_nl_break)
  
  ;; -- Option 1 -- Phrase break prediction by punctuation
  ;(set! pos_supported nil) ;; well not real pos anyhow
  ;(set! phrase_cart_tree nl::phrase_cart_tree)
  ;(Param.set 'Phrase_Method 'cart_tree)

  ;; -- option 2 -- Prosit accent assignment using a Timbl classifier
  (require 'net_nl_break_prosit)
  (Param.set 'Phrasify_Method 'nl::prosit-break)

  ;; -- Option 3 -- Phrase break prediction by Herwijnen algorithm
  ;; Note: this requires syntactic analysis!
  ; (require 'net_nl_break_herwijnen)
  ; (Param.set 'Phrasify_Method 'nl::HerwijnenPhrasing)

  ;; ------------------------------------------------------------
  ;; Intonation module:
  ;; pitch accent placement and tune choice
  ;; ------------------------------------------------------------

  (require 'net_nl_accent)

  ;; -- option 1 -- Basic accent assignment on the basis C/F word
  ;; uses POS tag to determine if a word is a function or a content
  ;; word, and assigns accents to all content words
  ; (Param.set 'Int_Method 'nl::basic_accent_placement)

  ;; -- option 2 -- Prosit accent assignment using a Timbl classifier
  (require 'net_nl_accent_prosit)
  (Param.set 'Int_Method 'nl::prosit_accent_placement)

  ;; ------------------------------------------------------------
  ;; Tune module:
  ;; tune choice
  ;; ------------------------------------------------------------
  
  (require 'net_nl_tune)

  ;; -- option1 -- Basic tune choice
  (Param.set 'Tune_Method 'nl::basic_tune_choice)

  ;; -----------------------------------------------------------
  ;; Word module:
  ;; lexicon loopkup, grapheme-to-phoneme conversion, and
  ;; building prosodic structures for words 
  ;;------------------------------------------------------------

  ;; Phone set
  (load_library 'net_nl_ad_phones.scm)
  (Param.set 'PhoneSet 'net_nl)
  (PhoneSet.select 'net_nl)

  ;; lexicon
  (require 'net_nl_lex)
  (require 'net_nl_lex_addenda)
  
  (Param.set 'Word_Method 'nl::word)

  ;; -- Option 1 -- TreeTalk memory-based grapheme-to-phoneme conversion
  ; (nl::setup_kunlex_treetalk)
  ; (lex.select "kunlex_treetalk")
  ; (set! nl::trace_treetalk t)
  
  ;; -- Option 2 -- Fonpars context-sensitive rewrite rules
  (nl::setup_kunlex_fonpars)
  (lex.select "kunlex_fonpars")

  ;; ------------------------------------------------------------
  ;; Pauses module:
  ;; pause insertion
  ;; ------------------------------------------------------------

  (require 'net_nl_pauses)
  (Param.set 'Pause_Method 'nl::pauses)

  ;; ------------------------------------------------------------
  ;; Postlex module:
  ;; postlexical rules
  ;; ------------------------------------------------------------

  (require 'net_nl_postlex)
  (require 'net_nl_ad_postlex)
  (set! postlex_rules_hooks (list nl::postlex_rules 
				  nl_ad::postlex_rules))

  ;; ------------------------------------------------------------
  ;; Duration module:
  ;; segment and pause durations
  ;; ------------------------------------------------------------
  
  ;; -- Option 1 -- Fixed durations
  ;; all segments are 100 milliseconds
  ; (Param.set 'Duration_Method 'Default)

  ;; -- Option 2 -- KUN rule-based duration prediction
  ;;; Speaking Rate
  (Parameter.set 'SpeakRate 2)
  (require 'net_nl_dur_kun)
  (load_library 'net_nl_ad_dur_kun.scm)
  (Param.set 'Duration_Method 'KUN_Duration)

  ;; -- Option 3 -- Lucent duration prediction (Esther Klabbers)
  ; (require 'net_nl_ad_dur_lucent)
  ; (Param.set 'Duration_Method 'Lucent_Duration)

  ;; ------------------------------------------------------------
  ;; Int_Target module:
  ;; fundamental frequency control
  ;; ------------------------------------------------------------

  ;; -- Option 1 -- Fixed F0
  ;(set! duffint_params '((start 180) (end 150)))
  ;(Parameter.set 'Int_Method 'DuffInt)
  ;(Parameter.set 'Int_Target_Method Int_Targets_Default)

  ;; -- Option 2 -- ToDI intonation according to the G&R model
  ;;; Phonetic implementation of scaling of F0 targets
  ;;; according to the model descibed in:
  ;;;
  ;;; Rob van den Berg, Carlos Gussenhoven and Toni Rietveld,
  ;;; "Downstep in Dutch: implications for a model".
  ;;; In: G.J. Docherty and D.R. Ladd (eds.),
  ;;; "Papers in Laboratory Phonology II", 
  ;;; Cambridge: Cambridge University Press, 1992, pp. 335-359.  

  ;;; Parameters of the target scaling model
  ;;; Reference frequency
  (Parameter.set 'Fr 60)
  ;;; Range
  (Parameter.set 'N 1.70)
  ;;; Register width
  (Parameter.set 'W 1.70)
  ;;; Accentual downstep factor
  (Parameter.set 'da 0.90)
  ;;; Phrasal downstep factor
  (Parameter.set 'dp 0.85)
  ;;; Timing star position in vowel
  (Parameter.set 'StarPos 0.1)

  (require 'net_nl_int_todi)
  (Param.set 'Int_Target_Method 'ToDI-intonation)

  ;; FIX-ME: reestablish the ToDI manual option
  ;(Param.set 'Int_Method 'ToDI-manual)

  ;; ------------------------------------------------------------
  ;; Wave_Synth module: 
  ;; wave form synthesis
  ;; ------------------------------------------------------------
  
  (require 'mbrola)
  (require 'net_nl_mbrola)
  ;; use MBROLA diphone syntheiszer with nl3 diphone database
  (Param.set 'Synth_Method nl::mbrola_synth)

  (cond
   ;; if on Cygwin, use Mbrola for Cywgin
   ((string-matches *ostype* ".*CYGWIN.*")
    (set! mbrola_progname (path-append (cadr etc-path) "mbrola.exe")))
   ;; if on Linux, use Mbrola for Linux
   ((string-matches *ostype* ".*Linux.*")
    (set! mbrola_progname (path-append (cadr etc-path) "mbrola")))
   ;; if on Darwin, use Mbrola for Darwin
   ((string-matches *ostype* ".*Darwin.*")
    (set! mbrola_progname (path-append (cadr etc-path) "mbrola-darwin-ppc")))
    ;; otherwise, assume it is in the path 
   (t
    (set! mbrola_progname "mbrola")))

  (set! mbrola_database (path-append net_nl_ad_mbrola_dir "nl2" "nl2"))

  ;; ------------------------------------------------------------

  ;; set callback to restore some original values changed by this voice
  (set! current_voice_reset net_nl_ad_voice_reset)

  (set! current-voice 'net_nl_ad_mbrola)
)




(define (net_nl_ad_voice_backup)
"
\(net_nl_ad_voice_backup\)

Save some values of global variables for use in voice reset.
"
  (set! UttTypes_backup UttTypes)
) 


(define (net_nl_ad_voice_reset)
"
\(net_nl_ad_voice_reset\)

Reset global variables back to previous voice.
"
  (set! UttTypes UttTypes_backup)  
)


(proclaim_voice
 'net_nl_ad_mbrola
 '((language Dutch)
   (gender male)
   (dialect standard)
   (description
    "A Dutch male voice using Mbrola with the nl2 database,
developed within the Nextens project" )))


(provide 'net_nl_ad_mbrola)
