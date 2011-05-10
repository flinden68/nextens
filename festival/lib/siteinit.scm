;;; default voice for Nextens
(set! voice_default 'voice_net_nl_ib_mbrola)

;;; add net_nl voices, in case none of the English voices are installed
(set! default-voice-priority-list 
      (append  default-voice-priority-list
	       '(net_net_ib_mbrola net_nl_ad_mbrola)))

;;; FIXME: this should happen automatically in the patched init.scm
;;; but somehow 'macosxaudio' doesn't show up in *modules*
(if (string-equal *ostype* "power_macintosh_Darwin")
    (Parameter.set 'Audio_Method 'macosxaudio))