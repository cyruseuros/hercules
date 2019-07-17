;;; wk-hydra.el --- `which-key' based `hydra's with less code. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/wk-hydra
;;
;; Version: 0.1
;; Keywords: faces
;; Package-Requires: ((emacs "24.4")(which-key))

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
  (add-hook 'pre-command-hook #'which-key--hide-popup)
  (defalias #'which-key--hide-popup
    #'wk-hydra--hide-popup-backup))

(defun wk-hydra--enable ()
  (remove-hook 'pre-command-hook #'which-key--hide-popup)
  ;; Called from helper functions I cannot disable.
  ;; Necessary for now.
  (defalias #'which-key--hide-popup
    #'ignore))

(defun wk-hydra--hide (&rest _)
  (wk-hydra--disable)
  (which-key--hide-popup))

(defun wk-hydra--show (keymap &rest _)
  (wk-hydra--enable)
  (when keymap (which-key-show-keymap keymap)))

(defun wk-hydra--hide-show (keymap &rest _)
  (if (which-key--popup-showing-p)
      (wk-hydra--hide)
    (wk-hydra--show keymap)))

(defun wk-hydra--show-funs (funs &optional keymap)
  (cl-loop for fun in funs do
            (advice-add fun :before
                        (apply-partially
                        #'wk-hydra--show keymap))))

(defun wk-hydra--hide-funs (funs)
  (cl-loop for fun in funs do
            (advice-add fun :before
                        #'wk-hydra--hide)))

(defun wk-hydra--hide-show-funs (funs &optional keymap)
  (cl-loop for fun in funs do
           (advice-add fun :before
                       (apply-partially
                        #'wk-hydra--hide-show keymap))))

;;;###autoload
(cl-defmacro wk-hydra-def (&key hide-show-funs
                                show-funs hide-funs
                                keymap pseudo-mode
                                package)
  (let ((keymap-symbol (eval keymap)))
    (wk-hydra--show-funs (eval show-funs) keymap-symbol)
    (wk-hydra--hide-funs (eval hide-funs))
    (wk-hydra--hide-show-funs (eval hide-show-funs))
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
              ,(when package
                `(require ,package))
              (set-transient-map (eval ,keymap-symbol)
                                  t #'wk-hydra--hide))
            ',func-symbol)))))

(provide 'wk-hydra)
;;; wk-hydra.el ends here
