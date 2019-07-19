;;; wk-hydra.el --- `which-key' based `hydra's with less code. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/wk-hydra
;;
;; Version: 0.1
;; Keywords: faces
;; Package-Requires: ((emacs "24.4")(which-key "3.3.0"))

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; `which-key' based `hydra's with less code.

;;; Code:
(require 'which-key)

(declare-function wk-hydra--hide-popup-backup load-file-name)
(defalias #'wk-hydra--hide-popup-backup
  (indirect-function #'which-key--hide-popup))

(defun wk-hydra--disable ()
  "Re-enable `which-key--hide-popup'.
Add it to `pre-command-hook', and restore its original function
definition."
  (add-hook 'pre-command-hook #'which-key--hide-popup)
  (defalias #'which-key--hide-popup
    #'wk-hydra--hide-popup-backup))

(defun wk-hydra--enable ()
  "Disable `which-key--hide-popup'.
Remove it from `pre-command-hook', and set its function
definition to `ignore'.  This is necessary as `which-key' calls
`which-key--hide-popup' in several internal calls that cannot be
disabled."
  (remove-hook 'pre-command-hook #'which-key--hide-popup)
  ;; Called from helper functions I cannot disable.
  ;; Necessary for now.
  (defalias #'which-key--hide-popup
    #'ignore))

(defun wk-hydra--hide (&rest _)
  "Hide `wk-hydra'."
  (wk-hydra--disable)
  (which-key--hide-popup))

(defun wk-hydra--show (keymap &rest _)
  "Show `wk-hydra' showing KEYMAP."
  (wk-hydra--enable)
  (when keymap (which-key-show-keymap keymap)))

(defun wk-hydra--toggle (keymap &rest _)
  "Toggle `wk-hydra' showing KEYMAP."
  (if (which-key--popup-showing-p)
      (wk-hydra--hide)
    (wk-hydra--show keymap)))

(defun wk-hydra--show-funs (funs &optional keymap)
  "Show `wk-hydra' showing KEYMAP when FUNS are called."
  (cl-loop for fun in funs do
            (advice-add fun :after
                        (apply-partially
                        #'wk-hydra--show keymap))))

(defun wk-hydra--hide-funs (funs)
  "Hide `wk-hydra' when FUNS are called."
  (cl-loop for fun in funs do
            (advice-add fun :after
                        #'wk-hydra--hide)))

(defun wk-hydra--toggle-funs (funs &optional keymap)
  "Toggle `wk-hydra' with KEYMAP when FUNS are called."
  (cl-loop for fun in funs do
           (advice-add fun :after
                       (apply-partially
                        #'wk-hydra--toggle keymap))))

;;;###autoload
(cl-defmacro wk-hydra-def (&key toggle-funs
                                show-funs
                                hide-funs
                                keymap
                                pseudo-mode
                                pseudo-mode-fun)
  " The following arguments define entry and exit point functions
for a `wk-hydra':

- TOGGLE-FUNS :: Processed with `wk-hydra--toggle-funs'.
- SHOW-FUNS :: Processed with `wk-hydra--show-funs'.
- HIDE-FUNS :: Processed with `wk-hydra--hide-funs'.

Now to the slightly less obvious ones:

- KEYMAP :: The keymap to display in `wk-hydra'. If it is nil, it is
  assumed that the function you are calling will result in a
  `which-key--show-popup' call. This might be desirable if you wish to
  enable `wk-hydra' for `which-key-show-top-level' or something
  similar. For example, this is what I have in my config so I can
  scroll to the `which-key' page of interest when I'm dealing with
  some fringe Evil commands I kind of forgot. Then I keep it around
  until I feel comfortable enough to kill it with
  `keyboard-quit'. This has the side effect of killing all `wk-hydra's
  on `keyboard-quit', but then again all commands are supposed to obey
  it.
  
 #+BEGIN_SRC emacs-lisp :tangle yes
   (wk-hydra-def
    :show-funs '(which-key-show-top-level)
    :hide-funs '(keyboard-quit keyboard-escape-quit))
 #+END_SRC 

- PSEUDO-MODE :: Whether to create a pseudo-mode by setting a KEYMAP
  as an overriding transient map. This is handy if the function you
  are binding `wk-hydra' to isn't actually a mode, or is fighting for
  keybindings with other minor-modes. The keymap stops taking
  precedence over other keymaps once a key outside of it is
  pressed. See `set-transient-map' for details. To take advantage of
  this capability, it isn't enough to call `wk-hydra-def'. You should
  bind its return value (a symbol) to the key you plan to use to
  enter the PSEUDO-MODE. E.g.:

#+BEGIN_SRC emacs-lisp
  (my:elisp::general-def
    \"m\" '(:ignore t :wk \"macrostep\")
    \"m.\" (wk-hydra-def
          :toggle-funs '(macrostep-mode)
          :keymap 'macrostep-keymap
          :pseudo-mode t
          :pseudo-mode-fun #'macrostep-mode)
    \"me\" #'macrostep-expand
    \"mc\" #'macrostep-collapse
    \"mn\" #'macrostep-next-macro
    \"mp\" #'macrostep-prev-macro)
#+END_SRC
 
- PSEUDO-MODE-FUN :: The command to call when entering
  PSEUDO-MODE. Useful when the function in question is an actual
  mode. You can omit it if you just want to set up the hydra without
  actually doing anything right away.
"
  (let ((keymap-symbol (eval keymap)))
    (wk-hydra--show-funs (eval show-funs) keymap-symbol)
    (wk-hydra--hide-funs (eval hide-funs))
    (wk-hydra--toggle-funs (eval toggle-funs))
    (when pseudo-mode
      (let* ((keymap-name (symbol-name keymap-symbol))
              (func-symbol (intern
                            (format "wk-hydra-%s-pseudo-mode"
                                    keymap-name)))
              (func-doc (format
                        (concat "Pseudo-mode for %s.\n"
                                "Defined by `wk-hydra-def'.")
                                keymap-name)))
        (wk-hydra--show-funs `(,func-symbol) keymap-symbol)
        `(progn
            (defun ,func-symbol ()
              ,func-doc
              (interactive)
              ,(when pseudo-mode-fun
                 `(,(eval pseudo-mode-fun)))
              (set-transient-map ,keymap-symbol t #'wk-hydra--hide))
            ',func-symbol)))))

(provide 'wk-hydra)
;;; wk-hydra.el ends here
