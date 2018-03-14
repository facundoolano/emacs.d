(prelude-require-package 'google-translate)

(require 'google-translate-smooth-ui)

(setq google-translate-translation-directions-alist
      '(("en" . "es") ("es" . "en") ))

(provide 'facundo-dictionary)
