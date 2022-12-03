;;; lispy-inline.el --- inline arglist and documentation. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Oleh Krehel

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display current function arguments or docstring in an in-place
;; overlay.

;;; Code:

(require 'thingatpt)

(require 'subr-x)

(defgroup lispy-faces nil
  "Font-lock faces for `lispy'."
  :group 'lispy
  :prefix "lispy-face-")

(defface lispy-face-hint
  '((((class color) (background light))
     :background "#fff3bc" :foreground "black")
    (((class color) (background dark))
     :background "black" :foreground "#fff3bc"))
  "Basic hint face."
  :group 'lispy-faces)

(defface lispy-face-req-nosel
  '((t (:inherit lispy-face-hint)))
  "Face for required unselected args."
  :group 'lispy-faces)

(defface lispy-face-req-sel
  '((t (:inherit lispy-face-req-nosel :bold t)))
  "Face for required selected args."
  :group 'lispy-faces)

(defface lispy-face-opt-nosel
  '((t (:inherit lispy-face-hint :slant italic)))
  "Face for optional unselected args."
  :group 'lispy-faces)

(defface lispy-face-key-nosel
  '((t (:inherit lispy-face-hint :slant italic)))
  "Face for keyword unselected args."
  :group 'lispy-faces)

(defface lispy-face-opt-sel
  '((t (:inherit lispy-face-opt-nosel :bold t)))
  "Face for optional selected args."
  :group 'lispy-faces)

(defface lispy-face-key-sel
  '((t (:inherit lispy-face-opt-nosel :bold t)))
  "Face for keyword selected args."
  :group 'lispy-faces)

(defface lispy-face-rst-nosel
  '((t (:inherit lispy-face-hint)))
  "Face for rest unselected args."
  :group 'lispy-faces)

(defface lispy-face-rst-sel
  '((t (:inherit lispy-face-rst-nosel :bold t)))
  "Face for rest selected args."
  :group 'lispy-faces)

(defcustom lispy-window-height-ratio 0.65
  "`lispy--show' will fail with string taller than window height times this.
The caller of `lispy--show' might use a substitute e.g. `describe-function'."
  :type 'float
  :group 'lispy)

(defvar lispy-elisp-modes
  '(emacs-lisp-mode lisp-interaction-mode eltex-mode minibuffer-inactive-mode
                    suggest-mode)
  "Modes for which `lispy--eval-elisp' and related functions are appropriate.")

(defvar lispy-clojure-modes
  '(clojure-mode clojurescript-mode clojurex-mode clojurec-mode)
  "Modes for which clojure related functions are appropriate.")

(defvar lispy-overlay nil
  "Hint overlay instance.")

(defvar lispy-hint-pos nil
  "Point position where the hint should be (re-) displayed.")

(declare-function lispy--eval-clojure-cider "le-clojure")
(declare-function lispy--clojure-args "le-clojure")
(declare-function lispy--clojure-resolve "le-clojure")
(declare-function lispy--describe-clojure-java "le-clojure")
(declare-function lispy--eval-scheme "le-scheme")
(declare-function lispy--eval-lisp "le-lisp")
(declare-function lispy--lisp-args "le-lisp")
(declare-function lispy--lisp-describe "le-lisp")
(declare-function lispy--back-to-paren "lispy")
(declare-function lispy--current-function "lispy")
(declare-function lispy--in-comment-p "lispy")
(declare-function lispy--bounds-string "lispy")

;; ——— Commands ————————————————————————————————————————————————————————————————

(defun lispy--cleanup-overlay ()
  "Delete `lispy-overlay' if it's valid and return t."
  (when (overlayp lispy-overlay)
    (delete-overlay lispy-overlay)
    (setq lispy-overlay nil)
    t))

(declare-function geiser-doc-symbol-at-point "geiser-doc")

(defun lispy--describe-inline (str pos)
  "Toggle the overlay hint."
  (condition-case nil
      (save-excursion
        (when (= 0 (count-lines (window-start) (point)))
          (recenter 1))
        (setq lispy-hint-pos pos)
        (goto-char lispy-hint-pos)
        (lispy--show (propertize str 'face 'lispy-face-hint)))
    (error
     (lispy--cleanup-overlay))))

(declare-function cider-nrepl-op-supported-p "ext:cider-client")
(declare-function cider-sync-request:info "ext:cider-client")
(declare-function nrepl-dict-get "ext:nrepl-dict")

(declare-function semantic-ctxt-current-symbol "ctxt")

;; ——— Utilities ———————————————————————————————————————————————————————————————
(defun lispy--arglist (symbol)
  "Get arglist for SYMBOL."
  (let (doc)
    (if (setq doc (help-split-fundoc (documentation symbol t) symbol))
        (car doc)
      (prin1-to-string
       (cons symbol (help-function-arglist symbol t))))))

(defun lispy--join-pad (strs width)
  "Join STRS padding each line with WIDTH spaces."
  (let* ((maxw (apply #'max (mapcar #'length strs)))
         (padding (make-string width ?\ ))
         (fstring (format "%%- %ds" maxw)))
    (mapconcat
     (lambda (x)
       (concat
        padding
        (let ((str (format fstring x)))
          (font-lock-append-text-property
           0 (length str) 'face 'lispy-face-hint str)
          str)))
     strs
     "\n")))

(defun lispy--show-fits-p (str)
  "Return nil if window isn't large enough to display STR whole."
  (let ((strs (split-string str "\n")))
    (when (or (< (length strs) (* lispy-window-height-ratio (window-height)))
              (window-minibuffer-p))
      strs)))

(defun lispy--show (str)
  "Show STR hint when `lispy--show-fits-p' is t."
  (let ((last-point (point))
        (strs (lispy--show-fits-p str)))
    (if strs
        (progn
          (setq str (lispy--join-pad
                     strs
                     (+ (if (window-minibuffer-p)
                            (- (minibuffer-prompt-end) (point-min))
                          0)
                        (string-width (buffer-substring
                                       (line-beginning-position)
                                       (point))))))
          (save-excursion
            (goto-char lispy-hint-pos)
            (if (= -1 (forward-line -1))
                (setq str (concat str "\n"))
              (end-of-line)
              (setq str (concat "\n" str)))
            (setq str (concat str
                              (buffer-substring (point) (1+ (point)))))
            (if lispy-overlay
                (progn
                  (move-overlay lispy-overlay (point) (+ (point) 1))
                  (overlay-put lispy-overlay 'invisible nil))
              (setq lispy-overlay (make-overlay (point) (+ (point) 1)))
              (overlay-put lispy-overlay 'priority 9999))
            (overlay-put lispy-overlay 'display str)
            (overlay-put lispy-overlay 'after-string "")
            (put 'lispy-overlay 'last-point last-point)))
      (save-selected-window
        (pop-to-buffer (get-buffer-create "*lispy-help*"))
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))
          (insert str)
          (goto-char (point-min))
          (help-mode))))))

(defun lispy--pretty-args (symbol)
  "Return a vector of fontified strings for function SYMBOL."
  (let* ((args (cdr (read (lispy--arglist symbol))))
         (p-opt (cl-position '&optional args :test 'equal))
         (p-rst (or (cl-position '&rest args :test 'equal)
                    (cl-position-if (lambda (x)
                                      (and (symbolp x)
                                           (string-match
                                            "\\.\\.\\.\\'"
                                            (symbol-name x))))
                                    args)))
         (a-req (cl-subseq args 0 (or p-opt p-rst (length args))))
         (a-opt (and p-opt
                     (cl-subseq args (1+ p-opt) (or p-rst (length args)))))
         (a-rst (and p-rst (last args))))
    (format
     "(%s)"
     (mapconcat
      #'identity
      (append
       (list (propertize (symbol-name symbol) 'face 'lispy-face-hint))
       (mapcar
        (lambda (x)
          (propertize (downcase (prin1-to-string x)) 'face 'lispy-face-req-nosel))
        a-req)
       (mapcar
        (lambda (x)
          (propertize (downcase (prin1-to-string x)) 'face 'lispy-face-opt-nosel))
        a-opt)
       (mapcar
        (lambda (x)
          (setq x (downcase (symbol-name x)))
          (unless (string-match "\\.\\.\\.$" x)
            (setq x (concat x "...")))
          (propertize x 'face 'lispy-face-rst-nosel))
        a-rst))
      " "))))

(provide 'lispy-inline)

;;; Local Variables:
;;; End:

;;; lispy-inline.el ends here
