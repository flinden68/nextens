;;; $Id: net_nl_ad_dur_kun.scm,v 1.1 2003/11/28 13:48:42 joopk Exp $
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


;;;  KUN Segment durations


(set! net_nl_ib::phone_data
'(
  ;;;Segments and their initial duration in seconds

  ;; Vowels
   (@ 0.060) ;; schwa, gemak
   (A 0.070) ;; pad
   (E 0.070) ;; pet
   (I 0.070) ;; pit
   (O 0.070) ;; pot
   (Y 0.070) ;; put
   (i 0.070) ;; vier
   (u 0.070) ;; voer
   (y 0.070) ;; vuur
   (a 0.070) ;; laan
   (e 0.070) ;; veer
   (o 0.070) ;; rood
   (2 0.070) ;; deur
  ;; diphthongs
   (Ei  0.075) ;; reis
   (9y  0.075) ;; huis
   (Au  0.075) ;; koud
  ;; foreign vowels
   (E:  0.075) ;; beige
   (9:  0.075) ;; freule

;NOT in AD   (Y:  0.075) ;; freule
;NOT in AD    (O:  0.075) ;; roze
   
   (I:   0.075) ;; AD/NL2 beer
   (Y:   0.075) ;; AD/NL2 deur
   (O:   0.075) ;; AD/NL2 boor
   
   
  ;; consonants
   (p   0.060) ;; pas
   (t   0.060) ;; tas
   (k   0.060) ;; kas
   (b   0.050) ;; bas
   (d   0.050) ;; das
   (g   0.050) ;; goal
   (f   0.075) ;; fiets
   (s   0.075) ;; sap
   (x   0.075) ;; toch
   (h   0.060) ;; hand
   (v   0.060) ;; vaas
   (z   0.060) ;; zeep
   (S   0.075) ;; sjiek
   (Z   0.075) ;; gage
   (G   0.060) ;; regen
   (m   0.060) ;; man
   (n   0.060) ;; nam
   (N   0.060) ;; lang
;;;NOT AD   (J   0.060) ;; oranje
   (r   0.060) ;; rand
   (l   0.060) ;; lief
   (j   0.060) ;; jas
   (w   0.060) ;; wat
;;;   (dZ  0.070) ;; jazz
;; Not in CGN, present in NL3 database
;;;NOT AD   (ai  0.080) ;; draai
;;;NOT AD   (oi  0.080) ;; mooi
;;;NOT AD   (ui  0.080) ;; roei
;;;NOT AD   (Ai  0.080) ;; ai
;;;NOT AD   (Oi  0.080) ;; hoi
;;;NOT AD   (L   0.080) ;; bal
   (J   0.080) ;; AD/NL2 haai
   (R   0.080) ;; AD/NL2 bar
   (W   0.080) ;; AD/NL2 sneeuw
   (L   0.080) ;; AD/NL2 bal   
   (tj	0.075) ;; AD/NL2 ketjap
   (dj	0.075) ;; AD/NL2 djintan
   (nj  0.060) ;; AD/NL2 oranje
  ;; others
;;;NOT AD   (?   0.015) ;; glottal stop
   (_   0.050) ;; pause/glottal stop

))

(provide 'net_nl_ib_dur_kun)
