;; test token to words mapping

;;; there is an unsolved problem with í, ì, Í, Ì, and Â
;;; see http://www.cstr.ed.ac.uk/projects/festival/mailing_lists/festival-talk/msg00954.html

;;  failures DEBÂCLE

(define (nl::test_token)
  (nl::test_suite 
   "TOKEN"
   (list
    '(nl::test_empty)
    '(nl::test_dash)
    '(nl::test_slash) 
    '(nl::test_spell)
    '(nl::test_diacritics)
    '(nl::test_dotted_abbrevs)
    '(nl::test_email)
    '(nl::test_url)
    '(nl::test_acronyms)
    '(nl::test_numbers)
    '(nl::test_quote)
    '(nl::test_garbage)
    )))


(define (nl::test_empty)
  (nl::test_block 
   "EMPTY" 
   (list
    '(nl::test_synth_text 
      "EMPTY STRING"
      "")
   )))


(define (nl::test_dash)
  (nl::test_block 
   "DASH" 
   (list
    '(nl::test_synth_text 
      "SINGLE DASH"
      "twee-derde"
      (lambda (utt) 
	(string-equal (item.name (utt.relation.first utt 'Word)) "twee")))
    '(nl::test_synth_text 
      "MULTIPLE DASH"
      "ons-kent-ons-gevoel")
    '(nl::test_synth_text 
      "HYPHEN DASH"
      "weer- kaart")
    '(nl::test_synth_text 
      "DASH IN ENUMERATION"
      "spier-, pees-, en zenuwklachten")
   )))

(define (nl::test_slash)
  (nl::test_block 
   "SLASH" 
   (list
    '(nl::test_synth_text 
      "SINGLE SLASH"
      "/ en/of")
   ))
  )


(define (nl::test_spell)
  (nl::test_block 
   "SPELL"
   (list
    '(nl::test_synth_text 
      "LOWER CASE LETTERS"
      "a b c d e f g h i j k l m n o p q r s t u v w x y z")
    '(nl::test_synth_text 
      "UPPER CASE LETTERS"
      "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z")
    '(nl::test_synth_text 
      "LOWER CASE DIACRITICS"
      "á à ä â è é ë ê í ì ï î ó ò ö ô ú ù ü û")
    '(nl::test_synth_text 
      "UPPER CASE DIACRITICS"
      "À Á Â Ä È É Ê Ë Ì Í Î Ï Ò Ó Ô Ö Ù Ú Û Ü")
    '(nl::test_synth_text 
      "DIGITS"
      "0 1 2 3 4 5 6 7 8 9")
    '(nl::test_synth_text 
      "NON-ALPHANUMERIC"
      "~ ` ! @ # $ % ^ & * ( ) - _ + = \\ | { [ ] } ; : ' \" < , > . / ?")
   )))

   
(define (nl::test_diacritics)
  (nl::test_block 
   "DIACRITICS"
   (list
    '(nl::test_synth_text 
      "A DIACRITICS"
      "Málaga, voilà, Aufklärung, debâcle, MÁLAGA, VOILÀ, AUFKLÄRUNG, DEBÂCLE")
    '(nl::test_synth_text 
      "E DIACRITICS"
      "derrière, café, efficiënt, enquête, DERRIÈRE, CAFÉ, EFFICIËNT, ENQUÊTE")
    '(nl::test_synth_text 
      "I DIACRITICS"
      "ín, ìn, gereïncarneerd, maîtresse, ÍN, ÌN, GEREÏNCARNEERD, MAÎTRESSE")
    '(nl::test_synth_text 
      "O DIACRITICS"
      "overkómen, coöperatie, compôte, OVERKÓMEN, COÖPERATIE, COMPÔTE")
    '(nl::test_synth_text 
      "U DIACRITICS"
      "vacuüm, croûton, VACUÜM, CROÛTON")
   )))


(define (nl::test_url)
  (nl::test_block 
   "URL"
   (list
    '(nl::test_synth_text 
      "HTTP"
      "http://nextens.uvt.nl")
    '(nl::test_synth_text 
      "HTTP TILDE"
      "http://ilk.uvt.nl/~marsi/index.html")
    '(nl::test_synth_text 
      "FTP"
      "ftp://ilk.uvt.nl/pub/")
   )))


(define (nl::test_email)
  (nl::test_block 
   "EMAIL"
   (list
    '(nl::test_synth_text 
      "EMAIL 1"
      "e.c.marsi@uvt.nl")
    '(nl::test_synth_text 
      "EMAIL 2"
      "J.Kerkhoff@let.ru.nl")
   )))

(define (nl::test_dotted_abbrevs)
  (nl::test_block 
   "DOTTED ABBREVIATIONS"
   (list
    '(nl::test_synth_text 
      "SINGEL DOT"
      "prof. dr. ir. Akkermans")
    '(nl::test_synth_text 
      "MULTI DOT"
      "t.z.t. n.a.v. e.e.a.")
    '(nl::test_synth_text 
      "UNKNOWN"
      "x.y.z.")
   )))


(define (nl::test_acronyms)
  (nl::test_block 
   "ACRONYMS"
   (list
    '(nl::test_synth_text 
      "SPELLED ACRONYM"
      "uvt, UVT")
    '(nl::test_synth_text 
      "NON-SPELLED ACRONYM"
      "nac, NAC")
   )))

(define (nl::test_numbers)
  (nl::test_block 
   "NUMBERS"
   (list
    '(nl::test_synth_text 
      "DIGITS"
      "0, 1, 2, 3, 4, 5, 6, 7, 8, 9")
    '(nl::test_synth_text 
      "9 < INTEGERS < 100"
      "10, 11, 12, 20, 21, 99")
    '(nl::test_synth_text 
      "99 < INTEGERS < 1000"
      "100, 101, 110, 111, 121, 200, 201, 230, 233")
    '(nl::test_synth_text 
      "1000 <= INTEGERS < 1.000.000"
      "1000, 1001, 1010, 1099, 9099, 10.000, 22.222")
    '(nl::test_synth_text 
      "1.000.000 < INTEGERS < 1.000.000.000.000"
      "1.000.000, 1.234.567, 1.000.000.000, 999.999.999.999 ")
    '(nl::test_synth_text 
      "INTEGERS >  999.999.999.999"
      "1.234.456.890.123")
    '(nl::test_synth_text 
      "FLOATS"
      "0,1, 0,01, 0,10, 0,99, 0,001, 0,101, 123456,123456")
    '(nl::test_synth_text 
      "NEGATIVES"
      "-1, -10, -1000, -0,1")
   )))


(define (nl::test_quote)
  (nl::test_block 
   "SINGLE QUOTE PLUS S ('s)"
   (list
    '(nl::test_synth_text 
      "GENITIVE 'S"
      "'s ochtends, 's morgens")
    '(nl::test_synth_text 
      "PLURAL 's"
      "camera's, jan's")
   )))


(define (nl::test_garbage)
  (nl::test_block 
   "GARBAGE" 
   (list
    '(nl::test_synth_text 
      "NON-DUTCH ISO CHARS"
      "Æ, Ændijvie, ç, çaak")
   )))


(provide 'net_nl_test_token)