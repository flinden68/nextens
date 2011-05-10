;;; $Id: nintens.scm,v 1.3 2004/05/03 23:12:29 emarsi Exp $

;;; this is the initialisation file that is read by the nextens gui


;;; use a Dutch voice
(if (not (member current-voice '(net_nl_ib_mbrola net_nl_ad_mbrola)))
    (voice_net_nl_ib_mbrola))

(cond
 ;;; Linux font settings
 ((string-matches *ostype* ".*Linux.*")
  (set! gui::OrthFont "times")
  (set! gui::OrthFontType "medium")
  (set! gui::OrthFontSize "18")
      
  (set! gui::ToDIFont "lucida")
  (set! gui::ToDIFontType "bold")
  (set! gui::ToDIFontSize "14")
      
  (set! gui::F0Font "lucida")
  (set! gui::F0FontType "bold")
  (set! gui::F0FontSize "14"))

 ;;; FIXME: Nintens fails to detect these
 ;;; Win font settings
 ((string-matches *ostype* ".*CYGWIN.*")
  (set! gui::OrthFont "Microsoft Sans Serif")
  (set! gui::OrthFontType "Regular")
  (set! gui::OrthFontSize "12")
      
  (set! gui::ToDIFont "Terminal")
  (set! gui::ToDIFontType "Regular")
  (set! gui::ToDIFontSize "9")
      
  (set! gui::F0Font "Terminal")
  (set! gui::F0FontType "Regular")
  (set! gui::F0FontSize "9"))
 )


(provide 'nintens)






