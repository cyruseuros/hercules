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

(defun hercules-hide (&rest _)
  "Dismiss hercules.el."
  (interactive)
  (setq hercules--popup-showing-p nil
        which-key-persistent-popup nil)
  (which-key--hide-popup))

(defun hercules-show (&optional keymap &rest _)
  "Summon hercules.el showing KEYMAP."
  (interactive (list (which-key--read-keymap)))
  (setq hercules--popup-showing-p t
        which-key-persistent-popup t)
  (when keymap
    (which-key-show-keymap keymap)
    (set-transient-map (eval keymap) t #'hercules-hide)))

(defun hercules-toggle (&optional keymap &rest _)
  "Toggle hercules.el showing KEYMAP."
  (interactive)
  (if hercules--popup-showing-p
      (hercules-hide)
    (hercules-show keymap)))

(defun hercules--enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun hercules--advise (funs hst &optional keymap)
  "Either `hide', `show' or `toggle' hercules.el depending on HST.
Do so when calling FUNS showing KEYMAP."
  (cl-loop
   for fun in (hercules--enlist funs) do
   (progn
     (unless (symbol-function fun) (fset fun #'ignore))
     (advice-add fun :after
                 (pcase hst
                   ('toggle (apply-partially #'hercules-toggle keymap))
                   ('show (apply-partially #'hercules-show keymap))
                   ('hide #'hercules-hide))))))

(defun hercules--graylist (keys funs keymap &optional whitelist)
  "Unbind KEYS and keys bound to FUNS from KEYMAP.
If WHITELIST is t, Unbind all keys not in KEYS or bound to FUNS
from KEYMAP."
  (let ((keymap-alist
         (cl-loop for (key . fun-name)
                  in (which-key--get-keymap-bindings keymap)
                  as fun = (intern fun-name)
                  when
                  (or (member key (hercules--enlist keys))
                      (member fun (hercules--enlist funs)))
                  collect (cons key fun))))

    (if whitelist
        (progn
          (set keymap (make-sparse-keymap))
          (cl-loop for (key . fun) in keymap-alist do
                   ;; keymap has to be evaluated again after modification
                   (define-key (eval keymap) (kbd key) fun)))
      (cl-loop for (key . fun) in keymap-alist do
               (define-key keymap (kbd key) nil)))))

(defun hercules--graylist-after-load (keys funs keymap &optional package whitelist)
  "Call `hercules--graylist' after PACKAGE has been loaded.
Pass KEYS, FUNS, KEYMAP, and WHITELIST directly to it.  If
PACKAGE is nil, simply call `hecules-graylist'."
  (if package
      (with-eval-after-load package
        (hercules--graylist keys funs keymap whitelist))
    (hercules--graylist keys funs keymap whitelist)))

;;;###autoload
(cl-defun hercules-def
    (&key toggle-funs
          show-funs
          hide-funs
          keymap
          blacklist-keys
          whitelist-keys
          blacklist-funs
          whitelist-funs
          package
          config)
  ""
  ;; tweak keymaps
  (when keymap
    (when (or whitelist-keys whitelist-funs)
      (hercules--graylist-after-load
       whitelist-keys whitelist-funs
       keymap package t)))
  (when (or blacklist-keys blacklist-funs)
    (hercules--graylist-after-load
     blacklist-keys blacklist-funs
     keymap package nil))
  ;; define entry points
  (hercules--advise toggle-funs 'toggle keymap)
  (hercules--advise show-funs 'show keymap)
  (hercules--advise hide-funs 'hide)
  ;; user config
  (eval config))

(provide 'hercules)
;;; hercules.el ends here
