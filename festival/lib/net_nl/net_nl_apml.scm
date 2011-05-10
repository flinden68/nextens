;;; $Id: net_nl_apml.scm,v 1.1 2004/03/05 11:10:55 emarsi Exp $
;;;
;;; by Erwin marsi & Joop Kerkhoff
;;; for the NeXTenS project
;;;
;;; Copyright (c) 2004
;;; ILK - Tilburg University
;;; L&S - University of Nijmegen
;;; Stichting Spraaktechnologie
;;;
;;; All rights Reserved.
;;;
;;; See the files NEXTENS.COPYING and NEXTENS.LICENSE 
;;; for information on usage and redistribution of this file, 
;;; and for a DISCLAIMER OF ALL WARRANTIES.


;; Sets up a Dutch voice to synthesise from APML.


(define (apml_initialise)
  "(apml_initialise)
Set up the current voice for apml use."
  (nl::apml_initialise utt)
  )

(define (nl::apml_initialise)
  "(apml_initialise)
Set up a Dutch voice for apml use."
  (Param.set 'Tune_Method 'nl::apml_tune_choice)
  )


;; start of apml sythesis wrappers, copied from Rob Clark's apml.scm

(define (apml_client_synth apml)
  "(apml_client_synth apml)
Synthesise apml and return waveform(s) to client."
  (utt.send.wave.client (apml_synth apml)))

(define (apml_synth apml)
  "(apml_synth xml)
Synthesis an apml string."
  (let ((tmpfile (make_tmp_filename))
      utt)
    (string_to_file tmpfile apml)
    (set! utt (apml_file_synth tmpfile))
    (delete-file tmpfile)
  utt))

(define (apml_file_synth filename)
  "(apml_file_synth filename)
Synthesis an apml file."
  (let ((utt (Utterance Concept nil)))
    (utt.load utt filename)
    (utt.synth utt)))

(define (string_to_file file s)
  "(string_to_file file string)
 Write string to file."
  (let ((fd))
    (set! fd (fopen file "wb"))
    (format fd "%s" s)
    (fclose fd)))

;; end of apml sythesis wrappers, copied from Rob Clark's apml.scm


;; The ToBI accents and boundary tones in  apml.dtd
;; do not match the ToDI symbols used in Nextens,
;; so we have to translate them.
;; In the long run, we need a dtd modified for Dutch.

(define (nl::tobi2todi_accent acc)
  "\(nl::tobi2todi_accent ACC\)
translate US ToBI pitch accent to nearest Dutch ToDI accent
"
  (cond
   ((string-equal acc "Hstar")
    "H*")
   ((string-equal acc "Lstar")
    "L*")
   ((string-equal acc "LplusHstar")
    "H*")
   ((string-equal acc "LstarplusH")
    "L*H")
   ((string-equal acc "HstarplusL")
    "H*L")
   ;; no good fit
   ((string-equal acc "HplusLstar")
    "L*")
   ))

(define (nl::tobi2todi_boundary bnd)
  "\(nl::tobi2todi_boundary BND\)
translate US ToBI boundary tone to nearest Dutch ToDI boundary"
  (cond
   ((string-equal bnd "L")
    "L%")
   ((string-equal bnd "H")
    "H%")
   ((string-equal bnd "LL")
    "L%")
   ((string-equal bnd "LL")
    "L%")
   ((string-equal bnd "HH")
    "H%")
   ((string-equal bnd "LH")
    "H%")
   ((string-equal bnd "HL")
    "H%")
   ))


(define (nl::apml_tune_choice utt)
  "\(nl::apml_tune_choice UTT\)
Builds a ToDI description of the tune from the APML markup,
as stored in the Emphasis and Boundary relations"
  (let (intev)
    ;; Intonation stores the tune as a sequence of intonational events 
    ;; (i.e. ToDI pitch accents and boundary tones)
    (utt.relation.create utt 'Intonation)
    ;; Word-Int stores the association between words and intonational events
    (utt.relation.create utt 'Word-Int)
    (mapcar
     (lambda (word)
       ;; the lazy way: just add every word to Word-Int,
       ;; regardless whether it has associated intonation or not
       (utt.relation.append utt 'Word-Int word)

       ;; ToDI requires specification of initial boundary tones,
       ;; but APML has no markup for them.
       ;; So, if this is the first word
       ;; or the preceding word has a final boundary tone,
       ;; then associate an initial boundary tone.
       (if (or (not (item.prev word))
	       (item.relation (item.prev word) 'Boundary))	       
	   (item.relation.append_daughter word
					  'Word-Int
					  (utt.relation.append utt 'Intonation '("%L"))))
      ;; transfer APML accents
      (if (item.relation word 'Emphasis)
	  (begin 
	    (set! intev (list (nl::tobi2todi_accent (item.feat word "R:Emphasis.parent.x-pitchaccent"))))
	    (item.relation.append_daughter word
					   'Word-Int
					   (utt.relation.append utt 'Intonation intev))))
       ;; transfer APML boundaries
       (if (item.relation word 'Boundary)	   
	   (begin
	     (set! intev (list (nl::tobi2todi_boundary (item.feat word "R:Boundary.parent.type"))))
	     (item.relation.append_daughter word
					    'Word-Int
					    (utt.relation.append utt 'Intonation intev))
	     ;; mark as heavy break, so a pause will be inserted as well
	     (item.set_feat word 'pbreak 'heavy))))
     (utt.relation.items utt 'Word)) ))

 
(provide 'net_nl_apml)

