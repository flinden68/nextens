;;; $Id: net_nl_ib_postlex.scm,v 1.6 2004/08/16 10:07:46 emarsi Exp $
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


;;; postlexical rules specific to nl3 database


;; TODO:
;; - add rule description and example to doc strings
;; - add test suite with tests for each rule
;; - some foreign words with /j/, e.g. /d j o j n t/, are virtually impossible 
;;   to represent with nl3 diphones; still unsolved are:
;;     j-Oi 
;;     j-_ 
;;     j-d 
;;     j-k 
;;     j-l 
;;     j-m 
;;     j-n 
;;     j-s 
;;     j-t 
;;     j-w 
   

(define (nl_ib::postlex_rules utt)
"
\(nl_ib::postlex_rules UTT\)

Apply nl3-specific postlexical rules to the segments of UTT.
"   
  (nl_ib::compose_diphtongs utt)
  (nl_ib::rewrite_diphones utt)
  (nl_ib::glottal_stop_insertion utt))


(define (nl_ib::compose_diphtongs utt)
" 
\(nl_ib::compose_diphtongs UTT\)

Rewrite segment pairs to nl3-specific diphtongs:
/a j/ to /ai/, 
/o j/ to  /oi/, 
/A j/ to /Ai/, and 
/O j/ to /Oi/
"
  ;; The nl3 database has a number of special diphones for diphtongs: ai, oi, Ai, Oi
  ;; In the CGN phoneme set, these are transcribed as a vowel followed by /j/.
  ;; So here we are rewriting /$ j/ ==> /$i/
  ;; where vowel /$/ is the nucleus and /j/ the coda of the *same* syllable. 
  (if nl::trace_postlex 
      (format t "\n*** Diphtong composition rules for ib/nl3\n\n"))
  (let (vowel newname)
    (mapcar
     (lambda (seg)
       (set! vowel (item.feat seg "p.name"))
       (if (and (string-equal (item.name seg) "j")
		(member vowel '("a" "o" "u" "A" "O"))
		(string-equal (item.feat seg "R:ProsTree.parent.parent.id")
			      (item.feat seg "p.R:ProsTree.parent.parent.id"))
		(set! newname
		      (cond
		       ;; /a j/ ==> /ai/
		       ((string-equal vowel "a") "ai")
		       ;; /o j/ ==> /oi/
		       ((string-equal vowel "o") "oi")
		       ;; /u j/ ==> /ui/
		       ((string-equal vowel "u") "ui")
		       ;; /A j/ ==> /Ai/
		       ((string-equal vowel "A") "Ai")
		       ;; /O j/ ==> /Oi/
		       ((string-equal vowel "O") "Oi") )))
	   (begin 
	     (if nl::trace_postlex 
		 (format t "Diphtong composition rule: %s j --> %s  / %s ___ %s\n" 
			 vowel
			 newname
			 (nl::left_context (item.prev seg))
			 (nl::right_context seg)))	     
	     (item.set_name (item.prev seg) newname)
	     (set! coda (item.relation.parent seg 'ProsTree)) 
	     (item.delete seg)
	     ;; delete the Coda node in SylPart
	     ;; if it becomes daughter-less after the deletion of /j/
	     (if (not (item.relation.daughters coda 'ProsTree))
		 (item.delete coda)) )))
     (utt.relation.items utt 'Segment))))



(define (nl_ib::glottal_stop_insertion utt)
"
\(nl_ib::glottal_stop_insertion UTT\)

Inserts a glottal stop between some diphtong pairs,
because the corresponding diphones are missing in the nl3 database.
"	    
  ;; these are basically hacks because some transitions from and to diphtongs 
  ;; are lacking in nl3
  (if nl::trace_postlex 
      (format t "\n*** Glottal stop insertion rules for ib/nl3\n\n")) 
  (let (newseg)
    (mapcar 
     (lambda (seg)
        (if (or
	     ;; insert glottal stop before 
	     ;; /ai oi ui Ai Oi E: 9: O:/
	     ;; preceded by a vowel
	     (and 
	      (string-equal (item.feat seg "p.ph_vc") "+")
	      (member_string (item.name seg)
			     '("ai" "oi" "ui" "Ai" "Oi" "E:" "9:" "O:")))
	     ;; insert glottal stop after
	     ;; /oi ui Oi E: j: O:/
	     ;; followed by a vowel
	     (and 
	      (string-equal (item.feat seg "ph_vc") "+")
	      (member_string (item.feat seg "p.name") 
			     '("@" "ai" "oi" "ui" "Ai" "Oi" "E:" "9:" "9y" "O:"))))
	    (begin
	      (if nl::trace_postlex
		  (format t "Glottal stop insertion rule: 0 --> ?  / %s  ___ %s %s\n" 
			  (nl::left_context seg)
			  (item.name seg) 
			  (nl::right_context seg)))
	      (item.relation.insert seg 
				    'Segment
				    (list '?)
				    'before))))
     ;; glottal stop is not part of ProsTree, 
     ;; because it's unclear where it should be attached
     (cdr (utt.relation.items utt 'Segment)))))


(define (nl_ib::rewrite_diphones utt)
"
\(nl_ib::rewrite_diphones utt\)

Renames some phonemes because their names are different in the nl3 database,
or because some appropriate allophone is available \(e.g. a dark l\). 
Also rewrites some phoneme pairs, because the corresponding diphone
is lacking in the nl3 database.
"
  (if nl::trace_postlex 
      (format t "\n*** Renaming rules for ib/nl3\n\n"))
  (let (this next)
    (mapcar
     (lambda (s)
       (set! this (item.name s))
       (set! next (item.feat s "n.name"))
       (cond
	;; --- French vowels ---
	;; Y: => 9: (different name in NL3)
	((string-equal this "Y:")
	 (nl::apply_rename_rule s "9:"))
	;; nasal vowels are missing in NL3
	((string-equal this "E~")
	 (nl::apply_rename_rule s "E"))
	((string-equal this "A~")
	 (nl::apply_rename_rule s "A"))
	((string-equal this "O~")
	 (nl::apply_rename_rule s "O"))
	((string-equal this "Y~")
	 (nl::apply_rename_rule s "Y"))
	
	
	;; --- diphtongs --- 
	((string-equal this "E+") 
	 (nl::apply_rename_rule s "Ei"))
	((string-equal this "Y+") 
	 (nl::apply_rename_rule s "9y"))
	((string-equal this "A+") 
	 (nl::apply_rename_rule s "Au"))
	((string-equal this "Y:") 
	 (nl::apply_rename_rule s "9:"))

	;; --- dark l ---
	;; within coda: l => L 
        ;; when preceded by a vowel, /N/, or /J/
	((and (string-equal this "l")
	      (string-equal (item.feat s "R:ProsTree.parent.name") "Coda")
	      (or 
	       (string-equal (item.feat s "p.ph_vc") "+")
	       (member_string (item.feat s "p.name") '("N" "J"))))
	 (nl::apply_assim_rule "Dark l rule" s "L"))
	;; FIX-ME:
	;; the dark l rule doesn't really belong here
	;; Perhaps create a hook in the postlexical assimilation rules
	;; to attach database-specific rules?

	;; --- G ---
	;; next
	;; G-r => x-r (e.g. begrip)	     G-d => x-d (e.g. leegde)
	;; G-l => x-l (e.g. begluren)	     G-n => x-n (e.g. diagnose)
	;; G-b => x-b (e.g. boegbeeld)	     G-m => x-m (e.g. dogma)
	;; G-w => x-w (e.g. oogwenk)	     G-z => x-z (e.g. oorlogsbuit)
	;; G-j => x-j (e.g. brugjaar)	     G-h => x-h (e.g. luchtwaardigheid)
	;; G-s => x-s (e.g. gedragsanalyse)  G-t => x-t (e.g. rechtsdwang)
	;; prev
	;; N-G => N-x (e.g. mongool)	     r-G => r-x (e.g. morgen)
	;; l-G => l-x (e.g. algebra)	     m-G => m-x (e.g. omgaan)
	;; n-G => n-x (e.g. loongrens)	     w-G => w-x (e.g. bouwgrond)
	;; s-G => s-x (e.g. lesgeven)	     S-G => S-x (e.g. puchglas)
	;; g-G => g-x (e.g. druggebruiker)
	((and (string-equal this "G")
	      (or (member_string next '("r" "d" "l" "n" "b" "m" "w" "z" "j" "h" "s" "t"))
		  (member_string (item.feat s "p.name") 
				 '("N" "r" "l" "L" "m" "n" "w" "s" "S" "g"))))
	 (nl::apply_rename_rule s "x"))

	;; --- d ---
	;; d-b => t-b (e.g. aardbei)	     d-z => t-z (e.g. fietsbel)
	;; d-m => t-m (e.g. aardmassa)	     d-l => t-l (e.g. bladluis)
	;; d-n => t-n (e.g. brandnetel)	     d-Z => t-Z (e.g. budget)
	;; d-_ => t-_ (e.g. eindtijd)	     d-h => t-h (e.g. adhesie)
	;; d-s => t-s (e.g. beleidsproces)   d-p => t-p (e.g. overheadprojector)
	;; d-S => t-S (e.g. overheadsheet)   d-f => t-f (e.g. roadfilm)
	;; d-k => t-k (e.g. overheadkosten)
	((and (string-equal this "d")
	      (member_string next '("b" "z" "m" "l" "n" "Z" "_" "h" "s" "p" "S" "f" "k")))
	 (nl::apply_rename_rule s "t"))  

	;; --- b ---
	;; b-d => p-d (e.g. abdij)	     b-z => p-z (e.g gipsbeen )
	;; b-w => p-w (e.g clubwedstrijd)    b-n => p-n (e.g. abnormaal)
	;; b-m => p-m (e.g. schrabmes)	
	((and (string-equal this "b")
	      (member_string next '("d" "z" "w" "n" "m")))
	 (item.set_name s "p"))  

	;; --- z ----
	;; z-b => s-b (e.g. asbak)	     z-d => s-d (e.g. busdienst)
	;; z-g => s-g (e.g. diskdrive)       z-l => s-l (e.g. quizleider)
	;; z-m => s-m (e.g. quizmaster)	     z-p => s-p (e.g. sales-promotion)
	;; z-r => s-r (e.g. gezondheidsreden)
	((and (string-equal this "z")
	      (member_string next '("b" "d" "g" "l" "m" "p" "r")))
	 (nl::apply_rename_rule s "s"))
	
	;; --- v ---
	;; v-d => f-d (e.g. hoofdrol)	      v-b => f-b (e.g. afbreken)
	;; v-z => f-z (e.g. bedrijfsbeleid)   v-w => f-w (e.g. ofwel)
	;; v-k => f-k (e.g. molotovcocktail)  v-g => f-g (e.g. lovegame)
	;; v-s => f-s (e.g. geloofsdwang)     v-x => f-x (e.g. sovchose)
	;; v-G => f-G (e.g. sauvegarde)	      v-n => f-n (e.g. have-nots)
	((and (string-equal this "v")
	      (member_string next '("d" "b" "z" "w" "k" "g" "s" "x" "G" "n")))
	 (nl::apply_rename_rule s "f"))

	;; --- Z ---
	;; Z-w => S-w (e.g. bourgeois)        Z-j => S-j (e.g. lits-jumeaux)
	((and (string-equal this "Z")
	      (string-equal (item.feat s "n.ph_vc") "-"))
	 (nl::apply_rename_rule s "S")) 

	;; --- j ---
	((and (string-equal this "j")
	      (member_string (item.feat s "p.name") '("ai" "oi" "Ai" "Oi")))
	 (nl::apply_rename_rule s "J")) 

	))
     (utt.relation.items utt 'Segment))))


(provide 'net_nl_ib_postlex)




