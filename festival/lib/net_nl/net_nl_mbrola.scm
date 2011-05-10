;;; $Id: net_nl_mbrola.scm,v 1.2 2004/04/29 14:53:04 joopk Exp $
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


;;; Additional control over Mbrola


(defvar nl::mbrola_ignore_fatal nil
  "mbrola -e command line option 
   = IGNORE fatal errors on unkown diphone.
   Set to t or nil.")

(defvar nl::mbrola_vol_ratio 1
  "mbrola -v command line option
   = VOLUME ratio, float ratio applied to output samples")
;; not very usefull, as you can use the EST methods as well

(defvar nl::mbrola_freq_ratio 1
  "mbrola -f command line option
   = FREQ ratio, float ratio applied to pitch points")
;; reasonable range: 0.1 - 3 

(defvar nl::mbrola_voice_freq 15000
  "mbrola -l command line option
   = VOICE freq, target freq for voice quality")
;; reasonable range: 5000 - 45000  



;;; this is just a extended version of MBROLA_Synth in mbrola.scm

(define (nl::mbrola_synth utt)
  "\(nl::mbrola_synth UTT\)
  A special version of the standard MBROLA_Synth function that also
  passes the -I command line options. 
  Synthesize using MBROLA as external module.  Basically dump the info
  from this utterance. Call MBROLA and reload the waveform into utt.
  [see MBROLA]"
  (let ((filename (make_tmp_filename)))
    (save_segments_mbrola utt filename)
    (system (format nil "%s %s -v %f -f %f -l %f %s \"%s\" \"%s.au\""
		    mbrola_progname
		    (if nl::mbrola_ignore_fatal "-e" "")  
		    nl::mbrola_vol_ratio
		    nl::mbrola_freq_ratio
		    nl::mbrola_voice_freq
		    mbrola_database
		    filename
		    filename))
    (utt.import.wave utt (string-append filename ".au"))
    (apply_hooks after_synth_hooks utt)
    (delete-file filename)
    (delete-file (string-append filename ".au"))
    utt))


(provide 'net_nl_mbrola)