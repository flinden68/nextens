;;; $Id: net_nl_pos_guess.scm,v 1.1 2003/11/20 20:54:04 emarsi Exp $
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


;;; guess part-of-speech or Dutch


;; TODO:
;; - add many more function words (pronouns and conjunctions)
;; - distinguish into accentable and non-accentable


;; FIX-ME reduced word forms (e.g. 't) are not handled correctly

(set! nl::guess_pos 
      '( 
	;; determiners
	(Art
	 de het een den der des ener 't 'n )
	;; pronouns
	(Pron
	 ;; personal
	 ik jij je hij ie zij ze wij we jullie hen
	 ;; possive 
	 mijn jouw zijn haar onze hun
	 )
	;; auxiliary verbs
	(Aux 
	 ben bent is zijn was waren geweest
	 word wordt worden werd werden geworden
	 blijf blijft blijven bleef bleven gebleven
	 lijk lijkt lijken leek leken geleken	
	 blijk blijkt blijken bleek bleken gebleken
	 schijn schijnt schijnen scheen schenen
	 heet heten heette heetten geheten
	 dunkt 
	 heb hebt heeft hebben had hadden gehad	 
	 ga gaat gaan ging gingen gegaan
	 zal zult zul zullen zou zouden )
	;; prepositions 
	(Prep 
	 aan ad af achter achterin ad anno 
	 bij beneden binnen blijkens boven bovenaan bovenop buiten
	 conform cum
	 dankzij door
	 gedurende gezien
	 in 
	 langs 
	 met middels
	 na nabij naar naast namens
	 om op over omstreeks omstrent onderaan
	 per
	 rond rondom
	 sedert sinds
	 te ten ter tot tussen tegen tegenover tijdens
	 uit
	 via voor voorts vooraf voorbij vanaf vanachter vanonder 
	   vanuit vanwege volgens vlakbij  
	 wegens
	 zonder )
	;; conjunctions
	(Conj
	 en of)
	))