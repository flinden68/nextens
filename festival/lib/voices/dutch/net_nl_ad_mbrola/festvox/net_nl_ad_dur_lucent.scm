;;; $Id: net_nl_ad_dur_lucent.scm,v 1.1 2003/12/03 13:48:48 joopk Exp $

;;; Lucent duration
;;; by Esther Klabbers

(set! vow_closedsyl_durations
'(
(Ei 0.131)
(Au 0.131)
(9y 0.131)
(E 0.131)
(O 0.131)
(9: 0.131)
(2 0.116)
(a 0.116)
(e 0.116)
(o 0.116)
(A 0.080)
(E 0.080)
(I 0.080)
(O 0.080)
(Y 0.080)
(i 0.099)
(u 0.088)
(y 0.088)
(@ 0.063)
))

(set! vow_opensyl_durations
'(
(Ei 0.161)
(Au 0.161)
(9y 0.161)
(E 0.161)
(O 0.161)
(9: 0.161)
(2 0.147)
(a 0.147)
(e 0.147)
(o 0.147)
(A 0.074)
(E 0.074)
(I 0.074)
(O 0.074)
(Y 0.074)
(i 0.108)
(u 0.116)
(y 0.116)
(@ 0.068)
))

(set! cons_coda_durations
'(
(n 0.078)
(N 0.097)
(m 0.099)
(J 0.105)
(L 0.062)
(l 0.062)
(w 0.038)
(j 0.065)
(x 0.097)
(G 0.097)
(c 0.080)
(f 0.093)
(s 0.116)
(S 0.139)
(p 0.028)
(t 0.040)
(k 0.034)
(tcl 0.029)
(kcl 0.033)
(pcl 0.042)
(r 0.045)
(_ 0.05)
))

(set! cons_onset_durations
'(
(n 0.041)
(N 0.045)
(m 0.051)
(J 0.090)
(l 0.045)
(w 0.048)
(j 0.055)
(x 0.076)
(G 0.076)
(c 0.080)
(f 0.083)
(s 0.088)
(S 0.111)
(p 0.021)
(t 0.033)
(k 0.034)
(ccl 0.030)
(tcl 0.033)
(kcl 0.037)
(pcl 0.048)
(r 0.032)
(Z 0.063)
(z 0.069)
(v 0.075)
(b 0.013)
(d 0.016)
(g 0.020)
(dcl 0.024)
(bcl 0.036)
(gcl 0.036)
(h 0.051)
(? 0.040)
))
  

(define (vowel_previous_phone_class seg c1 c2 c3 c4)
;; c1 = ustop/ufric/utt initial
;; c2 = vstop/vfric
;; c3 = vowel
;; c4 = all other cases

(if (not (equal? (item.prev seg) nil))
    (set! prev_segment (item.prev seg))
    ;; else
    c1)

(cond
((and (string-matches (item.feat prev_segment "ph_ctype") "[sf]")
      (string-equal (item.feat prev_segment "ph_cvox") "-"))
 c1)
((and (string-matches (item.feat seg "ph_ctype") "[sf]")
      (string-equal (item.feat prev_segment "ph_cvox") "+"))
 c2)
((string-equal (item.feat prev_segment "ph_vc") "+")
 c3)
(t
 c4)))

(define (cons_onset_previous_phone_class seg c1 c2 c3 c4 c5 c6 c7 c8 c9)
;; c1 = if in unvoiced 2-el fric/stop cluster
;; c2 = if in unvoiced 3or4-el fric/stop cluster
;; c3 = if in voiced fric/stop cluster
;; c4 = fric in prev syll
;; c5 = stop in prev syll or utt initial
;; c6 = nasal in prev syll
;; c7 = liquid or r in prev syll
;; c8 = lvow/svow 
;; c9 = @

(if (not (equal? (item.prev seg) nil))
    (set! prev_segment (item.prev seg)))

(cond
 ((not (string-equal (item.feat seg "R:ProsTree.parent.daughter1.id") (item.feat seg "id")))
  (if (string-matches (item.feat prev_segment "ph_ctype") "[sf]")
      (if (string-equal (item.feat prev_segment "ph_cvox") "-")
	  (if (string-equal (item.feat seg "R:ProsTree.p.id") (item.feat seg "R:ProsTree.parent.daughter1.id"))
	      c1
	      c2)
	  c3)))
 ((string-equal (item.feat prev_segment "ph_ctype") "f")
  c4)
 ((or (string-equal (item.feat prev_segment "ph_ctype") "s")
      (string-equal (item.feat prev_segment "name") "0"))
  c5)
 ((string-equal (item.feat prev_segment "ph_ctype") "n")
  c6)
 ((string-matches (item.feat prev_segment "ph_ctype") "[lr]")
  c7)
 ((string-equal (item.feat prev_segment "ph_vc") "+")
  (if (string-equal (item.feat prev_segment "name") "@")
      c9
      c8))
 (t
  c5)))

(define (cons_coda_previous_phone_class seg c1 c2 c3 c4)
;; c1 = vowel
;; c2 = nasal
;; c3 = liquid/r/all other cases
;; c4 = fric/stop

(if (not (equal? (item.prev seg) nil))
    (set! prev_segment (item.prev seg))
    ;; else
    c3)

(cond
 ((string-equal (item.feat prev_segment "ph_vc") "+")
  c1)
 ((and (string-equal (item.feat prev_segment "ph_ctype") "n")
       (string-equal (item.feat prev_segment "id") (item.feat seg "R:ProsTree.p.id")))
  c2)
 ((string-matches (item.feat prev_segment "ph_ctype") "[nlr]")
  c3)
 ((string-matches (item.feat prev_segment "ph_ctype") "[sf]")
  c4)
 (t
  c4)))


(define (vowel_next_phone_class seg c1 c2 c3 c4 c5)
;; because seg is in open syll. look at 1st phone in next syll
;; c1 = r
;; c2 = liq/nas
;; c3 = vowel
;; c4 = vstop/vfric
;; c5 = all other cases

(if (not (equal? (item.next seg) nil))
    (begin
      (set! next_segment (item.next seg))
      (cond
       ((string-equal (item.feat next_segment "name") "r")
	c1)
       ((string-matches (item.feat next_segment "ph_ctype") "[ln]")
	c2)
       ((string-equal (item.feat next_segment "ph_vc") "+")
	c3)
       ((and (string-equal (item.feat seg "name") "@")
	     (string-equal (item.feat next_segment "ph_ctype") "f"))
	c3)
       ((and (string-matches (item.feat next_segment "ph_ctype") "[sf]")
	     (string-equal (item.feat next_segment "ph_cvox") "+"))
	c4)
       ((and (string-matches (item.feat next_segment "ph_ctype") "[sf]")
	     (string-equal (item.feat next_segment "ph_cvox") "-"))
	c5)
       (t
	c5)))
    ;; else
    c5))


(define (cons_onset_next_phone_class seg c1 c2 c3 c4 c5 c6 c7 c8)
;; c1 = long vowel
;; c2 = short vowel
;; c3 = @
;; c4 = nasal
;; c5 = liquid
;; c6 = r in 2-el cluster
;; c7 = r in 3/4 el cluster
;; c8 = fric/stop or utt final

(if (not (equal? (item.next seg) nil))
    (begin
      (set! next_segment (item.next seg))
    
    (cond 
     ((string-matches (item.feat next_segment "ph_vlng") "[ld]")
      c1)
     ((string-equal (item.feat next_segment "ph_vlng") "s")
      c2)
     ((string-equal (item.feat next_segment "ph_vlng") "a")
      c3)
     ((string-equal (item.feat next_segment "ph_ctype") "n")
      c4)
     ((string-equal (item.feat next_segment "ph_ctype") "l")
      c5)
     ((string-equal (item.feat next_segment "ph_ctype") "r")
      (if (string-equal (item.feat seg "id") (item.feat seg "R:ProsTree.parent.daughter1.id"))
	  c6
	  c7))
     ((or (string-matches (item.feat next_segment "ph_ctype") "[sf]")
	  (and (string-equal (item.feat seg "R:ProsTree.parent.parent.n.name") "0")
	       (string-equal (item.feat seg "R:ProsTree.parent.parent.parent.parent.n.name") "0")))
       c8)))
    ;; else
    c8))

(define (cons_coda_next_phone_class seg c1 c2 c3 c4 c5 c6 c7 c8)
;; c1 = lvow/svow
;; c2 = @
;; c3 = nasal/liquid/r
;; c4 = vstop / utt final
;; c5 = vfric
;; c6 = ustop cluster
;; c7 = ustop/ufric in next syll
;; c8 = ufric cluster

(if (not (equal? (item.next seg) nil))
    (begin
      (set! next_segment (item.next seg))        
      (cond
       ((string-matches (item.feat next_segment "ph_vlng") "[lds]")
	c1)
       ((string-equal (item.feat next_segment "ph_vlng") "a")
	c2)
       ((string-matches (item.feat next_segment "ph_ctype") "[nlr]")
	c3)
       ((and (string-matches (item.feat next_segment "ph_ctype") "[s]")
	     (string-equal (item.feat next_segment "ph_cvox") "+"))
	c4)
       ((and (string-matches (item.feat next_segment "ph_ctype") "[f]")
	     (string-equal (item.feat next_segment "ph_cvox") "+"))
	c5)
       ((and (string-equal (item.feat next_segment "ph_ctype") "s")
	     (string-equal (item.feat next_segment "ph_cvox") "-"))
	(if (string-equal (item.feat next_segment "R:ProsTree.parent.id") (item.feat seg "R:ProsTree.parent.id"))
	c6
	c7))
       ((and (string-equal (item.feat next_segment "ph_ctype") "f")
	     (string-equal (item.feat next_segment "ph_cvox") "-"))
	(if (string-equal (item.feat next_segment "R:ProsTree.parent.id") (item.feat seg "R:ProsTree.parent.id"))
	c8
	c7))
       (t
	c4)))
    ;; else
    c4))

(define (stress_and_accent seg c1 c2)
  (if (string-equal (item.feat seg "R:ProsTree.parent.parent.stress") "0")
      c1
      c2))

;; if accentuation is defined the stress_and_accent procedure defined above
;; has to be replaced by the following definition

; (define (stress_and_accent seg c1 c2 c3 c4)
;   (if (equal? (item.feat seg "R:ProsTree.parent.parent.stress") "0")
;       (if (equal? (item.feat seg "R:ProsTree.parent.parent.parent.parent.accent") "0")
; 	  c1
; 	  c2))
;   (if (equal? (item.feat seg "R:ProsTree.parent.parent.stress") "1")
;       (if (equal? (item.feat seg "R:ProsTree.parent.parent.parent.parent.accent") "0")    
; 	  c3
; 	  c4)))

(define (left_position utt seg c1 c2 c3 c4 c5)
;; c1 = utt initial syll
;; c2 = word initial
;; c3 = word initial syll
;; c4 = syll initial
;; c5 = all other cases

;; utterance initial syll
;; phrase initial is not yet defined, otherwise it too should get value c1
  (cond
   ((and (string-equal (item.feat seg "R:ProsTree.parent.parent.parent.parent.id") (item.feat (utt.relation.first utt 'ProsTree) "id"))
	   (string-equal (item.feat seg "R:ProsTree.parent.parent.id") (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughter1.daughter1.id")))
    c1)
;; word initial
   ((string-equal (item.feat seg "id") (item.feat seg "R:ProsTree.parent.parent.parent.parent.daugther1.daughter1.daughter1.daughter1.id"))
    c2)
;; word initial syllable
   ((string-equal (item.feat seg "R:ProsTree.parent.parent.id") (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughter1.daughter1.id"))
    c3)
;; syllable initial
   ((string-equal (item.feat seg "id") (item.feat seg "R:ProsTree.parent.parent.daughter1.daughter1.id"))
    c4)
;; all other cases
   (t
    c5)))

(define (right_position utt seg c1 c2 c3 c4 c5)
;; c1 = utt final
;; c2 = utt final syll
;; c3 = word final
;; c4 = word final syll
;; c5 = all other cases

;; utterance final
;; phrase final is not yet defined
(cond
 ((and (string-equal (item.feat seg "R:ProsTree.parent.parent.parent.parent.id") (item.feat (utt.relation.last utt 'ProsTree) "id"))
       (string-equal (item.feat seg "R:ProsTree.parent.parent.id") (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughtern.daughtern.id"))
       (string-equal (item.feat seg "id") (item.feat seg "R:ProsTree.parent.parent.daughtern.daughtern.id")))
  c1)
 ((and (string-equal (item.feat seg "R:ProsTree.parent.parent.parent.parent.id") (item.feat (utt.relation.last utt 'ProsTree) "id"))
       (string-equal (item.feat seg "R:ProsTree.parent.parent.id") (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughtern.daughtern.id"))
       (not (string-equal (item.feat seg "id") (item.feat seg "R:ProsTree.parent.parent.daughtern.daughtern.id"))))
  c2)
;; word final
 ((string-equal (item.feat seg "id") (item.feat seg "R:ProsTree.parent.parent.parent.parent.daugthern.daughtern.daughtern.daughtern.id"))
  c3)
;; word final syllable
 ((string-equal (item.feat seg "R:ProsTree.parent.parent.id") (item.feat seg "R:ProsTree.parent.parent.parent.parent.daughtern.daughtern.id"))
  c4)
;; all other cases
 (t
  c5)))


(define (Lucent_Duration utt)
  (let ((segments (utt.relation.items utt 'Segment))
	(endtime 0)
	duration
	tabname
	prev
	next
	stress
	lpos
	rpos)

;; get the base durations for vowels in open/closed syllables 
;; and for consonants in onset/coda position

    (while segments
	   (set! seg (car segments))	
	   (set! prev 1)
	   (set! next 1)
	   (set! stress 1)
	   (set! lpos 1)
	   (set! rpos 1)
	   (if (string-equal (item.feat seg "ph_vc") "+")
	       (if (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda")
		   (set! tabname vow_closedsyl_durations)
		   (set! tabname vow_opensyl_durations))
	       (if (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset")
		   (set! tabname cons_onset_durations)
		   (set! tabname cons_coda_durations)))
	   ;(format t "seg=%s tabname=%l\n" (item.feat seg 'name) tabname)
	   (set! duration (car (cdr (assoc_string 
				     (item.name seg)  
				     tabname))))
;	   (format t "OLD ==> %s\t%s\n" (item.feat seg 'name) duration)
	   (format t "%s\t%s\t" (item.feat seg "name") duration)

;; long vowels in open syllables

	   (cond 
	    ((and (string-equal (item.feat seg "ph_vc") "+") 
		  (or (string-equal (item.feat seg "ph_vlng") "l")
		      (string-equal (item.feat seg "ph_vlng") "d"))
		  (not (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda"))) 
	     (begin
	       (set! prev (vowel_previous_phone_class seg 0.94 1.05 1.02 1.02))
	       (set! next (vowel_next_phone_class seg 0.92 0.94 0.99 1.04 1.04))
	       (set! stress (stress_and_accent seg 0.83 1.13))
;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.83 0.85 1.13 1.26))
	       (set! lpos (left_position utt seg 1.01 0.94 0.94 1.05 1.01))
	       (set! rpos (right_position utt seg 1.37 1.37 0.97 0.97 0.8))

	       (set! duration (* duration prev next stress lpos rpos))))

;; short vowels in open syllables
 
	    ((and (string-equal (item.feat seg "ph_vc") "+") 
		  (string-equal (item.feat seg "ph_vlng") "s")
		  (not (string-matches (item.feat seg "name") "[iuy]"))
		  (not (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda"))) 
	     (begin
	       (set! prev (vowel_previous_phone_class seg 1.06 1.06 0.94 0.94))
	       (set! next (vowel_next_phone_class seg 1.39 0.86 0.86 0.83 0.83))
	       (set! stress (stress_and_accent seg 0.82 1.05))
;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.82 0.96 1.05 1.22))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 1 1 1 1 1))

	       (set! duration (* duration prev next stress lpos rpos))))

;; iuy in open syllables 
	   
	    ((and (string-equal (item.feat seg "ph_vc") "+") 
		  (string-equal (item.feat seg "ph_vlng") "s")
		  (string-matches (item.feat seg "name") "[iuy]")
		  (not (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda"))) 
	     (begin
	       (set! prev (vowel_previous_phone_class seg 0.92 1.07 0.96 1.07))
	       (set! next (vowel_next_phone_class seg 1.65 0.83 0.97 0.91 0.83))
	       (set! stress (stress_and_accent seg 0.84 1.05))
;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.84 0.89 1.05 1.27))
	       (set! lpos (left_position utt seg 1.03 1.02 1.02 0.94 0.94))
	       (set! rpos (right_position utt seg 1.59 1.59 0.98 0.98 0.64))

	       (set! duration (* duration prev next stress lpos rpos))))

;; schwa in open syllables

	    ((and (string-equal (item.feat seg "ph_vc") "+") 
		 (string-equal (item.feat seg "ph_vlng") "a")
		 (not (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda"))) 
	    (begin
	      (set! prev (vowel_previous_phone_class seg 1.16 1.02 0.84 0.84))
	      (set! next (vowel_next_phone_class seg 1.17 0.9 0.9 0.95 0.95))
	      (set! stress (stress_and_accent seg 0.9 0.9))
;; if accent is defined replace stress call by line below
;	      (set! stress (stress_and_accent seg 0.9 1.12 0.9 1.12))
	      (set! lpos (left_position utt seg 0.9 1.12 1.12 1 0.9))
	      (set! rpos (right_position utt seg 1.68 1.68 0.93 0.93 0.64))

	      (set! duration (* duration prev next stress lpos rpos))))

;; long vowels in closed syllables

	    ((and (string-equal (item.feat seg "ph_vc") "+") 
		  (or (string-equal (item.feat seg "ph_vlng") "l")
		      (string-equal (item.feat seg "ph_vlng") "d"))
		  (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda")) 
	     (begin
	       (set! prev (vowel_previous_phone_class seg 0.97 0.97 1.03 1.03))
	       (set! next (vowel_next_phone_class seg 1.23 0.96 1.05 1.05 1.05))
	       (set! stress (stress_and_accent seg 0.82 1.04))
;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.82 0.97 1.04 1.21))
	       (set! lpos (left_position utt seg 1.03 1.03 0.97 1.03 1.03))
	       (set! rpos (right_position utt seg 1.24 1.24 0.95 0.95 0.85))

	       (set! duration (* duration prev next stress lpos rpos))))

;; short vowels in closed syllables

	    ((and (string-equal (item.feat seg "ph_vc") "+") 
		  (string-equal (item.feat seg "ph_vlng") "s")
		  (not (string-matches (item.feat seg "name") "[iuy]"))
		  (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda"))
	     (begin

	       (set! prev (vowel_previous_phone_class seg 0.9 0.9 0.89 0.98))
	       (set! next (vowel_next_phone_class seg 1.17 0.97 0.99 0.99 0.99))

	       (set! stress (stress_and_accent seg 0.83 1.07))
;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.83 0.98 1.07 1.14))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 1.25 1.25 0.96 0.96 0.84))

	       (set! duration (* duration prev next stress lpos rpos))))

;; iuy in closed syllables 
	   
	    ((and (string-equal (item.feat seg "ph_vc") "+") 
		  (string-equal (item.feat seg "ph_vlng") "s")
		  (string-matches (item.feat seg "name") "[iuy]")
		  (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda"))
	     (begin
	       (set! prev (vowel_previous_phone_class seg 1 1 1 1))
	       (set! next (vowel_next_phone_class seg 1.53 0.78 0.75 0.75 0.75))
	       (set! stress (stress_and_accent seg 0.95 0.81))
;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.95 1.06 0.81 1.21))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 1.23 1.23 0.96 0.96 0.85))

	       (set! duration (* duration prev next stress lpos rpos))))

;; schwa in closed syllables

	    ((and (string-equal (item.feat seg "ph_vc") "+") 
		  (string-equal (item.feat seg "ph_vlng") "a")
		  (string-equal (item.feat seg "R:ProsTree.parent.n.name") "Coda"))
	     (begin
	       (set! prev (vowel_previous_phone_class seg 1.28 1.28 0.76 1.03))
	       (set! next (vowel_next_phone_class seg 1.05 1.05 1.21 0.94 0.94))
	       (set! stress (stress_and_accent seg 0.95 0.95))
;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.95 1.05 0.95 1.05))
	       (set! lpos (left_position utt seg 1.2 0.96 1.27 0.81 0.81))
	       (set! rpos (right_position utt seg 1.39 1.39 0.94 0.94 0.77))

	       (set! duration (* duration prev next stress lpos rpos))))

;; nasals in onsets

	    ((and (string-equal (item.feat seg "ph_ctype") "n")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset")) 
	     (begin
	       (set! prev (cons_onset_previous_phone_class seg 0.78 0.78 0.78 0.99 0.99 0.94 1.32 1.32 1.32))
	       (set! next (cons_onset_next_phone_class seg 1.06 0.95 0.95 0.95 0.95 0.95 0.95 0.95))
	       (set! stress (stress_and_accent seg 0.80 0.95))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.80 0.99 0.95 1.33))
	       (set! lpos (left_position utt seg 0.85 1.18 1.18 0.85 0.85))
	       (set! rpos (right_position utt seg 0.96 0.96 1.04 0.96 0.96))

	       (set! duration (* duration prev next stress lpos rpos))))
	  

;; liquids in onsets

	    ((and (string-equal (item.feat seg "ph_ctype") "l")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
	     (begin
	       (set! prev (cons_onset_previous_phone_class seg 0.72 0.72 0.72 1.01 1.01 1.16 1.16 1.18 1.18))
	       (set! next (cons_onset_next_phone_class seg 1.12 1.03 0.87 1 1 1 1 1))
	       (set! stress (stress_and_accent seg 0.90 0.97))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.90 0.95 0.97 1.19))
	       (set! lpos (left_position utt seg 0.92 1.08 1.08 0.92 0.92))
	       (set! rpos (right_position utt seg 1 1 1 1 1))

	       (set! duration (* duration prev next stress lpos rpos))))

;; unvoiced fricatives in onsets

	    ((and (string-equal (item.feat seg "ph_ctype") "f")
		  (string-equal (item.feat seg "ph_cvox") "-")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
	     (begin
	       (set! prev (cons_onset_previous_phone_class seg 0.77 0.77 0.77 1.03 1.03 1.26 1.21 1.26 1.26))
	       (set! next (cons_onset_next_phone_class seg 1 1 1 1.05 1.05 1.05 1.05 0.95))
	       (set! stress (stress_and_accent seg 0.88 1))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.88 1 1 1.14))
	       (set! lpos (left_position utt seg 0.94 1.07 1.07 0.94 0.94))
	       (set! rpos (right_position utt seg 0.98 0.98 1.02 0.98 0.98))

	       (set! duration (* duration prev next stress lpos rpos))))

;; voiced fricatives in onsets

	    ((and (string-equal (item.feat seg "ph_ctype") "f")
		  (string-equal (item.feat seg "ph_cvox") "+")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
	     (begin
	       (set! prev (cons_onset_previous_phone_class seg 0.44 0.44 0.44 1.19 1.19 1.47 1.47 1.47 1.28))
	       (set! next (cons_onset_next_phone_class seg 0.99 0.99 0.99 1.01 1.01 1.01 1.01 1.01))
	       (set! stress (stress_and_accent seg 0.84 1.07))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.84 0.96 1.07 1.16))
	       (set! lpos (left_position utt seg 0.92 1.09 1.09 0.92 0.92))
	       (set! rpos (right_position utt seg 0.96 0.96 1.04 0.96 0.96))

	       (set! duration (* duration prev next stress lpos rpos))))

;; h in onsets

	    ((and (string-equal (item.feat seg "name") "h")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
	     (begin
	       (set! prev (cons_onset_previous_phone_class seg 1 1 1 1.28 0.96 0.83 0.83 0.97 0.97))
	       (set! next (cons_onset_next_phone_class seg 1 1 1 1 1 1 1 1))
	       (set! stress (stress_and_accent seg 0.89 0.84))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.89 0.88 0.84 1.54))
	       (set! lpos (left_position utt seg 0.97 1.03 1.03 0.97 0.97))
	       (set! rpos (right_position utt seg 1 1 1 1 1))

	       (set! duration (* duration prev next stress lpos rpos))))

;; glottal stop in onsets

	    ((and (string-equal (item.feat seg "name") "?")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
	     (begin
	       (set! prev (cons_onset_previous_phone_class seg 1 1 1 1.28 1.28 0.94 0.94 0.97 0.81))
	       (set! next (cons_onset_next_phone_class seg 1 1 1 1 1 1 1 1))
	       (set! stress (stress_and_accent seg 0.63 1.04))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.63 0.93 1.04 1.64))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 0.92 0.92 1.08 0.92 0.92))

	       (set! duration (* duration prev next stress lpos rpos))))

;; r in onsets

	    ((and (string-equal (item.feat seg "ph_ctype") "r")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
	     (begin
	       (set! prev (cons_onset_previous_phone_class seg 1.04 1.17 0.88 1.04 1.04 0.67 0.67 1.4 1.4))
	       (set! next (cons_onset_next_phone_class seg 1 1 1 1 1 1 1 1))
	       (set! stress (stress_and_accent seg 0.96 1.04))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.96 0.9 1.04 1.11))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 1 1 1 1 1))

	       (set! duration (* duration prev next stress lpos rpos))))

;; unvoiced stops in onsets

	    ((and (string-equal (item.feat seg "ph_ctype") "s")
		  (string-equal (item.feat seg "ph_cvox") "-")
		  (not (string-equal (item.feat seg "name") "?"))
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
	     (begin
;; stop closures
	       (set! prev (cons_onset_previous_phone_class seg 0.69 0.69 0.69 1.02 1.02 0.95 1.5 1.5 1.5))
	       (set! next (cons_onset_next_phone_class seg 1.19 1.19 1.02 1.01 1.01 1.01 1.01 0.82))
	       (set! stress (stress_and_accent seg 0.84 1.07))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.84 0.93 1.07 1.2))
	       (set! lpos (left_position utt seg 0.84 1.19 1.19 0.84 0.84))
	       (set! rpos (right_position utt seg 1 1 1 1 1))

	       (set! dur1 (car (cdr (assoc_string 
				     (string-append (item.feat seg "name") "cl")  
				     tabname))))
	       (set! dur1 (* dur1 prev next stress lpos rpos))
;; stop bursts	       
	       (set! prev (cons_onset_previous_phone_class seg 0.84 0.84 0.84 0.92 1.1 1.17 1.17 1.17 1.17))
	       (set! next (cons_onset_next_phone_class seg 0.99 0.99 0.99 1.26 1.26 0.95 0.84 0.95))
	       (set! stress (stress_and_accent seg 0.95 0.98))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.95 1.05 0.98 1.02))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 0.99 1.07 0.94 0.99 0.99))

	       (set! dur2 (* duration prev next stress lpos rpos))
	       (set! duration (+ dur1 dur2))))

;; voiced stops in onsets

	    ((and (string-equal (item.feat seg "ph_ctype") "s")
		  (string-equal (item.feat seg "ph_cvox") "+")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Onset"))
	     (begin
;; stop closures
	       (set! prev (cons_onset_previous_phone_class seg 1.18 1.18 1.18 1.18 1.18 0.57 1.47 1.47 1.47))
	       (set! next (cons_onset_next_phone_class seg 1.13 1.13 0.88 1.01 1.01 1.01 1.01 1.01))
	       (set! stress (stress_and_accent seg 0.84 0.93))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.84 0.94 0.93 1.36))
	       (set! lpos (left_position utt seg 0.87 1.15 1.15 0.87 0.87))
	       (set! rpos (right_position utt seg 1 1 1 1 1))

	       (set! dur1 (car (cdr (assoc_string 
				     (string-append (item.feat seg "name") "cl")  
				     tabname))))
	       (set! dur1 (* dur1 prev next stress lpos rpos))
;; stop bursts	       
	       (set! prev (cons_onset_previous_phone_class seg 0.89 0.89 0.89 1.12 1.12 0.89 0.89 0.89 0.89))
	       (set! next (cons_onset_next_phone_class seg 1 1 1 1 1 1 1 1))
	       (set! stress (stress_and_accent seg 0.89 1.05))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.89 0.93 1.05 1.15))
	       (set! lpos (left_position utt seg 0.98 1.02 1.02 0.98 0.98))
	       (set! rpos (right_position utt seg 1 1 1 1 1))

	       (set! dur2 (* duration prev next stress lpos rpos))
	       (set! duration (+ dur1 dur2))))

;; nasals in codas

	    ((and (string-equal (item.feat seg "ph_ctype") "n")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Coda")) 
	     (begin
	       (set! prev (cons_coda_previous_phone_class seg 0.97 1.03 1.03 1.03))
	       (set! next (cons_coda_next_phone_class seg 0.93 0.93 1.02 1.17 1.17 1.02 1.02 0.91))
	       (set! stress (stress_and_accent seg 0.71 0.61))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.71 1.10 0.61 1.21))
	       (set! lpos (left_position utt seg 0.83 1.21 1.21 0.83 0.83))
	       (set! rpos (right_position utt seg 1.74 1.74 0.78 0.78 0.62))

	       (set! duration (* duration prev next stress lpos rpos))))
	  

;; liquids in codas

	    ((and (string-equal (item.feat seg "ph_ctype") "l")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Coda"))
	     (begin
	       (set! prev (cons_coda_previous_phone_class seg 1 1 1 1))
	       (set! next (cons_coda_next_phone_class seg 1.28 0.71 1.28 1.28 1.28 1.1 1.28 1.1))
	       (set! stress (stress_and_accent seg 0.82 1.01))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.82 1.05 1.01 1.15))
	       (set! lpos (left_position utt seg 0.92 1.09 1.09 0.92 0.92))
	       (set! rpos (right_position utt seg 1.42 1.42 0.82 0.82 0.68))

	       (set! duration (* duration prev next stress lpos rpos))))

;; unvoiced fricatives in codas

	    ((and (string-equal (item.feat seg "ph_ctype") "f")
		  (string-equal (item.feat seg "ph_cvox") "-")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Coda"))
	     (begin
	       (set! prev (cons_coda_previous_phone_class seg 1.13 1.02 1.02 0.87))
	       (set! next (cons_coda_next_phone_class seg 0.88 0.88 1.13 1.04 1.04 1.04 1.04 1.04))
	       (set! stress (stress_and_accent seg 0.8 1.06))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.8 0.98 1.06 1.19))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 1.67 1.67 0.78 0.78 0.65))

	       (set! duration (* duration prev next stress lpos rpos))))


;; r in codas

	    ((and (string-equal (item.feat seg "ph_ctype") "r")
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Coda"))
	     (begin
	       (set! prev (cons_coda_previous_phone_class seg 1 1 1 1))
	       (set! next (cons_coda_next_phone_class seg 0.75 0.75 1.25 1.25 1.25 1.07 1.25 1.07))
	       (set! stress (stress_and_accent seg 0.74 1.25))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.74 0.82 1.25 1.31))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 1.72 1.72 0.86 1.01 0.67))

	       (set! duration (* duration prev next stress lpos rpos))))

;; unvoiced stops in codas

	    ((and (string-equal (item.feat seg "ph_ctype") "s")
		  (string-equal (item.feat seg "ph_cvox") "-")
		  (not (string-equal (item.feat seg "name") "?"))
		  (string-equal (item.feat seg "R:ProsTree.parent.name") "Coda"))
	     (begin
;; stop closures
	       (set! prev (cons_coda_previous_phone_class seg 1.53 0.65 1.53 1.01))
	       (set! next (cons_coda_next_phone_class seg 1.13 1.13 1.13 0.94 0.94 0.81 1.16 0.81))
	       (set! stress (stress_and_accent seg 0.89 0.92))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.89 1.08 0.92 1.15))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 1.14 1.14 1.02 1.02 0.86))

	       (set! dur1 (car (cdr (assoc_string 
				     (string-append (item.feat seg "name") "cl")  
				     tabname))))
	       (set! dur1 (* dur1 prev next stress lpos rpos))
;; stop bursts	       
	       (set! prev (cons_coda_previous_phone_class seg 1.08 1.12 1.12 0.83))
	       (set! next (cons_coda_next_phone_class seg 1.23 1.23 1.36 0.7 1.36 0.86 1.36 0.86))
	       (set! stress (stress_and_accent seg 0.89 0.88))
;;; if accent is defined replace stress call by line below
;	       (set! stress (stress_and_accent seg 0.89 1.1 0.88 1.19))
	       (set! lpos (left_position utt seg 1 1 1 1 1))
	       (set! rpos (right_position utt seg 2.27 1.49 0.64 0.64 0.46))

	       (set! dur2 (* duration prev next stress lpos rpos))
	       (set! duration (+ dur1 dur2))))

	    (t
	     (set! duration (* duration prev next stress lpos rpos)))
	    )


;	   (format t "NEW ==> %s\t%s\tprev:%s\tnext: %s stress: %s\tlpos: %s\trpos: %s\n" (item.feat seg 'name) duration prev next stress lpos rpos)
	   (format t "%s\n" duration)
	   (set! endtime (+ endtime duration))
	   (item.set_feat seg 'end endtime)
	   (set! segments (cdr segments))
	   )
    )
  utt)


(provide 'net_nl_ib_dur_lucent)