;;; $Id: net_nl_ad_phones.scm,v 1.1 2003/11/28 13:50:34 joopk Exp $
;;;
;;; by Joop Kerkhoff & Erwin Marsi
;;; for the NeXTenS project
;;;
;;; Copyright (c) 2003
;;; ILK - Tilburg University
;;; L&S - University of Nijmegen
;;; Stichting Spraaktechnologie
;;;
;;; All rights reserved.
;;;
;;; See the files NEXTENS.COPYING and NEXTENS.LICENSE 
;;; for information on usage and redistribution of this file, 
;;; and for a DISCLAIMER OF ALL WARRANTIES.


;;; Definition of phone set and features.
;;; The phone set is that used for broad phonetic transcription 
;;; in the CGN project.  

(defPhoneSet
  net_nl
  ;;; Phone features
  (;; vowel or consonant
   (vc + - 0)  
   ;; vowel length: short or long
   (vlng s l 0)
   ;; vowel type: short long dipthong schwa
   (vtype s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 0)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: plosive fricative nasal liquid
   (ctype p f n l r 0)
   ;; place of articulation: labial alveolar palatal labio-dental
   ;;                        dental velar glottal
   (cplace l a p b d v g 0)
   ;; consonant voicing
   (cvox + - 0)
   )
  ;;; Phone set
  (;; vowels
   (@   +   s   a   2   2   -   0   0   0) ;; de
   (A   +   s   s   3   3   -   0   0   0) ;; pad
   (E   +   s   s   2   1   -   0   0   0) ;; pet
   (I   +   s   s   1   1   -   0   0   0) ;; pit
   (O   +   s   s   3   3   +   0   0   0) ;; pot
   (Y   +   s   s   2   3   +   0   0   0) ;; put
   (i   +   s   s   1   1   -   0   0   0) ;; vier
   (u   +   s   s   1   3   +   0   0   0) ;; voer
   (y   +   s   s   1   3   +   0   0   0) ;; vuur
   (a   +   l   l   3   1   -   0   0   0) ;; laan
   (e   +   l   l   2   1   -   0   0   0) ;; veer
   (o   +   l   l   2   3   +   0   0   0) ;; rood
   (2   +   l   l   2   1   -   0   0   0) ;; deur
   ;; diphtongs
   (E+  +   l   d   3   2   -   0   0   0) ;; CGN reis
   (Y+  +   l   d   3   2   -   0   0   0) ;; CGN huis
   (A+  +   l   d   3   2   -   0   0   0) ;; CGN koud
   (Ei  +   l   d   3   2   -   0   0   0) ;; AD/NL2 reis
   (9y  +   l   d   3   2   -   0   0   0) ;; AD/NL2 huis
   (Au  +   l   d   3   2   -   0   0   0) ;; AD/NL2 koud
   ;; foreign vowels
   (E:  +   l   d   2   1   -   0   0   0) ;; beige
   (Y:  +   l   d   2   1   -   0   0   0) ;; CGN freule
   (9:  +   l   d   2   1   -   0   0   0) ;; IB/NL3 freule
   (O:  +   l   d   2   1   -   0   0   0) ;; roze
   ;; nasal vowels
   (E~  +   l   d   3   2   -   0   0   0) ;; vaccin
   (A~  +   l   d   3   2   -   0   0   0) ;; croissant
   (O~  +   l   d   3   2   -   0   0   0) ;; conge
   (Y~  +   l   d   3   2   -   0   0   0) ;; parfum
   ;; consonants
   (p   -   0   0   0   0   0   p   l   -) ;; pas
   (t   -   0   0   0   0   0   p   a   -) ;; tas
   (k   -   0   0   0   0   0   p   v   -) ;; kas
   (b   -   0   0   0   0   0   p   l   +) ;; bas
   (d   -   0   0   0   0   0   p   a   +) ;; das
   (g   -   0   0   0   0   0   p   v   +) ;; goal
   (f   -   0   0   0   0   0   f   b   -) ;; fiets
   (v   -   0   0   0   0   0   f   b   +) ;; vaas
   (s   -   0   0   0   0   0   f   a   -) ;; sap
   (z   -   0   0   0   0   0   f   a   +) ;; zeep
   (S   -   0   0   0   0   0   f   p   -) ;; sjiek
   (Z   -   0   0   0   0   0   f   p   +) ;; gage
   (x   -   0   0   0   0   0   f   v   -) ;; toch
   (G   -   0   0   0   0   0   f   v   +) ;; regen
   (h   -   0   0   0   0   0   f   v   -) ;; hand
   (m   -   0   0   0   0   0   n   l   +) ;; man
   (n   -   0   0   0   0   0   n   a   +) ;; nam
   (N   -   0   0   0   0   0   n   v   +) ;; lang
;;;NOT AD   (J   -   0   0   0   0   0   n   v   +) ;; oranje
   (r   -   0   0   0   0   0   r   a   +) ;; rand
   (l   -   0   0   0   0   0   l   a   +) ;; lief
   (j   -   0   0   0   0   0   l   p   +) ;; jas
   (w   -   0   0   0   0   0   l   l   +) ;; wat
;; Not in CGN, present in IB/NL3 database
;;;NOT AD   (ai  +   l   d   2   1   -   0   0   0) ;; IB/NL3 haai
;;;NOT AD   (oi  +   l   d   2   1   -   0   0   0) ;; IB/NL3 mooi
;;;NOT AD   (ui  +   l   d   2   1   -   0   0   0) ;; IB/NL3 boei
;;;NOT AD   (Ai  +   l   d   2   1   -   0   0   0) ;; IB/NL3 ai
;;;NOT AD   (Oi  +   l   d   2   1   -   0   0   0) ;; IB/NL3 hoi
;;;NOT AD   (L   -   0   0   0   0   0   l   a   +) ;; IB/NL3 bal
   (J   -   0   0   0   0   0   l   v   +) ;; AD/NL2 aai
   (L   -   0   0   0   0   0   l   a   +) ;; AD/NL2 bal
   (R   -   0   0   0   0   0   r   a   +) ;; AD/NL2 haar
   (W   -   0   0   0   0   0   l   l   +) ;; AD/NL2 wat
   (I:  +   l   l   2   1   -   0   0   0) ;; AD/NL2 beer
   (y:  +   l   l   2   1   -   0   0   0) ;; AD/NL2 deur
   (o:  +   l   l   2   3   +   0   0   0) ;; AD/NL2 boor
   (tj	-   0	0   0   0   0   p   p   -) ;; AD/NL2 ketjap
   (dj	-   0	0   0   0   0   p   p   +) ;; AD/NL2 djintan
   (nj  -   0   0   0   0   0   n   v   +) ;; AD/NL2 oranje 
   ;; other
;;;NOT AD   (?   0   0   0   0   0   -   0   0   -) ;; IB/NL3 glottal stop
   (_   0   0   0   0   0   -   0   0   -) ;; silence
   )
)

(PhoneSet.silences '(_))


(provide 'net_nl_ad_phones)
