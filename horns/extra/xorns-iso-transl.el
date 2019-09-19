(with-eval-after-load 'iso-transl
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'iso-transl)))

  (define-prefix-command 'arrow-thin-map)
  (define-key iso-transl-ctl-x-8-map "-" 'arrow-thin-map)
  (define-key iso-transl-ctl-x-8-map "->" "→")
  (define-key iso-transl-ctl-x-8-map "-->" "→")
  (define-key iso-transl-ctl-x-8-map "-<" "←")
  (define-key iso-transl-ctl-x-8-map "--<" "←")

  (define-prefix-command 'arrow-thick-map)
  (define-key iso-transl-ctl-x-8-map "=" 'arrow-thick-map)
  (define-key iso-transl-ctl-x-8-map "=>" "⇒")
  (define-key iso-transl-ctl-x-8-map "==>" "⇒")
  (define-key iso-transl-ctl-x-8-map "=<" "⇐")
  (define-key iso-transl-ctl-x-8-map "==<" "⇐")

  (let* ((keys (eval-when-compile
                 (append
                  (mapcar #'string
                          (string-to-list
                           "',-./0123456789;=[\\]`abcdefghijklmnopqrstuvwxyz"))
                  '("<left>" "<right>" "<up>" "<down>"
                    "<return>" "<tab>" "RET" "TAB")))))

    (define-prefix-command 'iso-cm-map)
    (define-prefix-command 'iso-cs-map)
    (define-prefix-command 'iso-ms-map)
    (define-prefix-command 'iso-cms-map)
    (define-key iso-transl-ctl-x-8-map (kbd ";") 'iso-cm-map)
    (define-key iso-transl-ctl-x-8-map (kbd ":") 'iso-cs-map)
    (define-key iso-transl-ctl-x-8-map (kbd "M-;") 'iso-ms-map)
    (define-key iso-transl-ctl-x-8-map (kbd "M-:") 'iso-cms-map)

    (dolist (key keys)
      (define-key iso-transl-ctl-x-8-map
        (kbd (concat "; " key))
        (kbd (concat "C-M-" key)))

      (define-key iso-transl-ctl-x-8-map
        (kbd (concat ": " key))
        (kbd (concat "C-S-" key)))

      (define-key iso-transl-ctl-x-8-map
        (kbd (concat "M-; " key))
        (kbd (concat "M-S-" key)))

      (define-key iso-transl-ctl-x-8-map
        (kbd (concat "M-: " key))
        (kbd (concat "C-M-S-" key))))))


(use-package iso-transl
  :defer 10
  :config
  (progn
    ;; Add custom bindings to "C-x 8" map
    (dolist (binding
             '(;; >
               (">"       . nil) ; First unbind ">" from the map
               (">="      . [?≥]) ; greater than or equal to
               (">>"      . [?≫]) ; much greater than
               (">\""     . [?»]) ; right-pointing double angle quotation mark
               (">'"      . [?›]) ; single right-pointing angle quotation mark
               (">h"      . [?☛]) ; black right pointing index
               ;; <
               ("<"       . nil) ; First unbind "<" from the map
               ("<="      . [?≤]) ; less than or equal to
               ("<<"      . [?≪]) ; much less than
               ("<\""     . [?«]) ; left-pointing double angle quotation mark
               ("<'"      . [?‹]) ; single left-pointing angle quotation mark
               ("<h"      . [?☚]) ; black left pointing index
               ;; "
               ("\"`"     . [?“]) ; left double quotation mark
               ("\"'"     . [?”]) ; right double quotation mark
               ;; ?
               ;; Originally "C-x 8 ?" was bound to insert ¿.
               ;; - But I never used that char, and I would use ‽ more often than
               ;;   that.
               ;; - Also, ¿ can be inserted using "C-x 8 * ?".
               ;; - And unlike other chars, "?" cannot be used in a
               ;;   prefix map, because trying to do so will make it trigger the
               ;;   `help-for-help' command in the `help-map'.  So I'm simply
               ;;   overriding the default "C-x 8 ?" binding here instead of
               ;;   making "?" a prefix map binding (like I do for "!" below).
               ("?"       . [?‽]) ; interrobang
               ;; !
               ("!"       . nil) ; First unbind "!" from the map
               ("!!"      . [?¡]) ; inverted exclamation mark
               ("!?"      . [?‽]) ; interrobang
               ;; arrows
               ("<right>" . [?→]) ; rightwards arrow
               ("<left>"  . [?←]) ; leftwards arrow
               ("<up>"    . [?↑]) ; upwards arrow
               ("<down>"  . [?↓]) ; downwards arrow
               ;; misc
               ("r"       . [?▯]) ; white vertical rectangle
               ("R"       . [?▮]) ; black vertical rectangle
               ("*r"      . [?₹]) ; indian rupee sign
               ("e"       . [?↵]) ; downwards arrow with corner leftwards
               ("E"       . [?⏎]) ; return symbol
               ("1/3"     . [?⅓]) ; fraction one third
               ("0"       . [?​]))) ; zero width space
      (define-key iso-transl-ctl-x-8-map (kbd (car binding)) (cdr binding)))))
