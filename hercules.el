;;; hercules.el --- An auto-magical, which-key-based hydra banisher. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/hercules
;;
;; Version: 0.2
;; Keywords: faces
;; Package-Requires: ((emacs "24.4") (which-key))

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
;; An auto-magical, which-key-based hydra banisher.

;; With almost no set-up code, Hercules lets you call any group of
;; related command sequentially with no prefix keys, while showing a
;; handy popup to remember the bindings for those commands.  It can
;; create both of these (the grouped commands, and the popup) from any
;; keymap.

;;; Code:
(require 'which-key)

(defvar hercules--popup-showing-p nil
  "Whether or not hercules.el has been summoned.
Used in addition to `which-key-persistent-popup' in case other
packages start relying on it.")

(defun hercules--hide (&rest _)
  "Dismiss hercules.el."
  (setq hercules--popup-showing-p nil
        which-key-persistent-popup nil)
  (which-key--hide-popup))

(defun hercules--show (keymap &rest _)
  "Summon hercules.el showing KEYMAP."
  (setq hercules--popup-showing-p t
        which-key-persistent-popup t)
  (when keymap
    (which-key-show-keymap keymap)))

(defun hercules--toggle (keymap &rest _)
  "Toggle hercules.el showing KEYMAP."
  (if hercules--popup-showing-p
      (hercules--hide)
    (hercules--show keymap)))

(defun hercules--enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun hercules--show-funs (funs &optional keymap)
  "Summon hercules.el showing KEYMAP when FUNS are called."
  (cl-loop for fun in (hercules--enlist funs) do
            (advice-add fun :after
                        (apply-partially
                        #'hercules--show keymap))))

(defun hercules--hide-funs (funs)
  "Dismiss hercules.el when FUNS are called."
  (cl-loop for fun in (hercules--enlist funs) do
            (advice-add fun :after
                        #'hercules--hide)))

(defun hercules--toggle-funs (funs &optional keymap)
  "Toggle hercules.el showing KEYMAP when FUNS are called."
  (cl-loop for fun in (hercules--enlist funs) do
           (advice-add fun :after
                       (apply-partially
                        #'hercules--toggle keymap))))

(defun hercules--graylist (keys funs keymap &optional whitelist)
  "Unbind KEYS and keys bound to FUNS from KEYMAP.
If WHITELIST is t, Unbind all keys not in KEYS or bound to FUNS
from KEYMAP."
  (let* ((keymap-val (eval keymap))
         (keys-val (eval keys))
         (funs-val (eval funs))
         (keymap-alist
          (cl-loop for (key . fun)
                   in (which-key--get-keymap-bindings keymap-val)
                   as fun-symbol = (intern fun)
                   when (or (member key (hercules--enlist keys-val))
                            (member fun-symbol (hercules--enlist funs-val)))
                   collect (cons key fun-symbol))))

    (if whitelist
        (progn
          (set keymap (make-sparse-keymap))
          (cl-loop for (key . fun-symbol) in keymap-alist do
                   ;; keymap has to be evaluated again after modification
                   (define-key (eval keymap) (kbd key) fun-symbol)))
      (cl-loop for (key . fun-symbol) in keymap-alist do
               (define-key keymap-val (kbd key) nil)))))

(defun hercules--graylist-after-load (keys funs keymap &optional package whitelist)
  "Call `hercules--graylist' after PACKAGE has been loaded.
Pass KEYS, FUNS, KEYMAP, and WHITELIST directly to it.  If
PACKAGE is nil, simply call `hecules-graylist'."
  (let ((package (eval package)))
    (if package
        (with-eval-after-load package
          (hercules--graylist keys funs keymap whitelist))
      (hercules--graylist keys funs keymap whitelist))))

;;;###autoload
(cl-defmacro hercules-def (&key toggle-funs
                                show-funs
                                hide-funs
                                keymap
                                transient-mode
                                transient-mode-fun
                                blacklist-keys
                                whitelist-keys
                                whitelist-funs
                                blacklist-funs
                                package
                                config)
  "Summon hercules.el to banish your hydras.

In at most 7 lines of set-up code, hercules.el lets you call any
group of related command sequentially with no prefix keys, while
showing a handy popup to remember the bindings for those
commands. He can create both of these (the grouped commands, and
the popup) from any keymap.

The following arguments define entry and exit point functions
that invoke hercules.el (both lists and single functions work):

- TOGGLE-FUNS :: Processed with `hercules--toggle-funs'.
- SHOW-FUNS :: Processed with `hercules--show-funs'.
- HIDE-FUNS :: Processed with `hercules--hide-funs'.

The following mutually arguments provide a shorthand for
whittling down hercules.el pop-ups if you don't want to get your
hands dirty with keymaps and prefer a more minimal UI (both lists
and single keys/functions work, and blacklists take precedence
over whitelists):

- BLACKLIST-KEYS :: Processed with `hercules--graylist-after-load'
- WHITELIST-KEYS :: Processed with `hercules--graylist-after-load'
- BLACKLIST-FUNS :: Processed with `hercules--graylist-after-load'
- WHITELIST-FUNS :: Processed with `hercules--graylist-after-load'


Now to the slightly less obvious options:

- KEYMAP :: The keymap to display in hercules.el pop-ups. If it
  is nil, it is assumed that the function you are calling will
  result in a `which-key--show-popup' call. This might be
  desirable if you wish to summon hercules.el for
  `which-key-show-top-level' or something similar. For example,
  this is what I have in my config so I can scroll to the
  which-key page of interest when I'm dealing with some fringe
  Evil commands I kind of forgot. Then I keep it around until I
  feel comfortable enough to kill it with `keyboard-quit'. This
  has the side effect of killing all hercules.el pop-ups on
  `keyboard-quit', but then again all commands are supposed to
  obey it.

 #+BEGIN_SRC emacs-lisp :tangle yes
   (hercules-def
    :show-funs #'which-key-show-top-level
    :hide-funs '(keyboard-quit keyboard-escape-quit))
 #+END_SRC

- PACKAGE :: If you are using any of BLACKLIST-KEYS,
  WHITELIST-KEYS, BLACKLIST-FUNS, or WHITELIST-FUNS, and the
  KEYMAP you're dealing is in a lazy-loaded package, you must
  also specify the package it belongs to as a quotes symbol using
  this argument.

- TRANSIENT-MODE :: Whether to create a transient-mode by setting
  a KEYMAP as an overriding transient map. This is handy if the
  function you are summoning hercules.el with isn't actually a
  mode, or is fighting for keybindings with other
  minor-modes. The keymap stops taking precedence over other
  keymaps once a key outside of it is pressed. See
  `set-transient-map' for details. To take advantage of this
  capability, it isn't enough to call `hercules-def'. You should
  bind its return value (a symbol) to the key you plan to use to
  enter the TRANSIENT-MODE. E.g.:

#+BEGIN_SRC emacs-lisp
  (my:elisp::general-def
    \"m\" '(:ignore t :wk \"macrostep\")
    \"m.\" (hercules-def
          :toggle-funs '(macrostep-mode)
          :keymap 'macrostep-keymap
          :transient-mode-fun #'macrostep-mode)
    \"me\" #'macrostep-expand
    \"mc\" #'macrostep-collapse
    \"mn\" #'macrostep-next-macro
    \"mp\" #'macrostep-prev-macro)
#+END_SRC

- TRANSIENT-MODE-FUN :: The command to call when entering
  TRANSIENT-MODE.  You can omit it if you just want to summon
  hercules.el without actually doing anything right away.

- CONFIG :: A dummy argument the pedantic among us can use to
  execute hercules.el related configuration code in the same
  place as `hercules-def'.  The most common use case will most
  likely be to tweak keymaps with more precision than
  the (BLACK|WHITE)LIST-(KEYS|FUNS) arguments allow, or to define
  new keymaps from scratch for complete control. At that point
  you'd basically be building a hydra, but you'd still be saving
  effort in my opinion. You could leverage the power of whichever
  keybinding engine you prefer, and bind nested keymaps to keys
  without worrying about *-hydra/body abstractions or manually
  specifying anything beyond what you ECD (Emacs Compulsive
  Disorder) compels you to. hercules.el will simply follow you
  where needed. Here's a pseudo-example:

#+BEGIN_SRC emacs-lisp
   (hercules-def
    :show-funs #'my-show-fun
    :hide-funs #'my-hide-fun
    :keymap 'my-map
    :config (general-def
              :prefix-map 'my-map
              \"h\" #'my-hide-fun
              \"s\" #'my-show-fun
              \"m\" 'my-keymap-1
              \"n\" 'my-keymap-2
              ;; +++
              ))
#+END_SRC
"
  (let ((keymap-symbol (eval keymap)))
    ;; tweak keymaps
    (when keymap
      (when (or whitelist-keys whitelist-funs)
        (hercules--graylist-after-load whitelist-keys whitelist-funs
                                       keymap-symbol package t)))
      (when (or blacklist-keys blacklist-funs)
        (hercules--graylist-after-load blacklist-keys blacklist-funs
                                       keymap-symbol package nil))

    ;; define entry points
    (hercules--toggle-funs (eval toggle-funs) keymap-symbol)
    (hercules--show-funs (eval show-funs) keymap-symbol)
    (hercules--hide-funs (eval hide-funs))

    ;; user config
    (eval config)

    ;; create transient-mode
    (when (or transient-mode transient-mode-fun)
      (let* ((keymap-name (symbol-name keymap-symbol))
              (func-symbol (intern
                            (format "hercules-%s-transient-mode"
                                    keymap-name)))
              (func-doc (format
                        (concat "transient-mode for %s.\n"
                                "Defined by `hercules-def'.")
                                keymap-name)))
        (hercules--show-funs `(,func-symbol) keymap-symbol)
        `(progn
            (defun ,func-symbol ()
              ,func-doc
              (interactive)
              ,(when transient-mode-fun
                 `(,(eval transient-mode-fun)))
              (set-transient-map ,keymap-symbol t #'hercules--hide))
            ',func-symbol)))))

(provide 'hercules)
;;; hercules.el ends here
