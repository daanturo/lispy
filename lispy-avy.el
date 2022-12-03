;; -*- lexical-binding: t; -*-

(require 'avy)

(require 'lispy)

;;;###autoload
(defun lispy-ace-subword (arg)
  "Mark sub-word within a sexp.
Sexp is obtained by exiting list ARG times."
  (interactive "p")
  (if (and (region-active-p)
           (string-match "\\`\\(\\sw+\\)\\s_"
                         (lispy--string-dwim)))
      (lispy--mark (cons (region-beginning)
                         (+ (region-beginning) (match-end 1))))
    (lispy--out-forward
     (if (region-active-p)
         (progn (deactivate-mark) arg)
       (1- arg)))
    (let* ((avy-keys lispy-avy-keys)
           (res (avy-with 'lispy-ace-subword
                  (lispy--avy-do
                   "[([{ -/]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
                   (lispy--bounds-dwim)
                   (lambda () (or (not (lispy--in-string-or-comment-p))
                                  (lispy-looking-back ".\"")))
                   lispy-avy-style-symbol))))
      (unless (memq res '(t nil))
        (skip-chars-forward "-([{ `'#")
        (mark-word)))))

;;;###autoload
(defun lispy-ace-symbol-replace (arg)
  "Jump to a symbol within the current sexp and delete it.
Sexp is obtained by exiting the list ARG times."
  (interactive "p")
  (lispy-ace-symbol arg)
  (when (region-active-p)
    (lispy-delete 1)))

(defun lispy--avy-do (regex bnd filter style &optional group)
  "Visually select a match to REGEX within BND.
Filter out the matches that don't match FILTER.
Use STYLE function to update the overlays."
  (lispy--recenter-bounds bnd)
  (let* ((avy-all-windows nil)
         (cands (avy--regex-candidates
                 regex
                 (car bnd) (cdr bnd)
                 filter
                 group)))
    (dolist (x cands)
      (when (> (- (cdar x) (caar x)) 1)
        (cl-incf (caar x))))
    (avy-process
     cands
     (cl-case style
       (pre #'avy--overlay-pre)
       (at #'avy--overlay-at)
       (at-full #'avy--overlay-at-full)
       (post #'avy--overlay-post)))))

;;;###autoload
(defun lispy-ace-char ()
  "Visually select a char within the current defun."
  (interactive)
  (let ((avy-keys lispy-avy-keys))
    (avy-with lispy-ace-char
      (lispy--avy-do
       (string (read-char "Char: "))
       (save-excursion
         ;; `beginning-of-defun' won't work, since it can change sexp
         (lispy--out-backward 50)
         (lispy--bounds-dwim))
       (lambda () t)
       lispy-avy-style-char))))

;;;###autoload
(defun lispy-ace-paren (&optional arg)
  "Jump to an open paren within the current defun.
ARG can extend the bounds beyond the current defun."
  (interactive "p")
  (setq arg (or arg 1))
  (lispy--remember)
  (deactivate-mark)
  (let ((avy-keys lispy-avy-keys)
        (bnd (if (eq arg 1)
                 (save-excursion
                   (lispy--out-backward 50)
                   (lispy--bounds-dwim))
               (cons (window-start)
                     (window-end nil t)))))
    (avy-with lispy-ace-paren
      (lispy--avy-do
       lispy-left
       bnd
       (lambda () (not (lispy--in-string-or-comment-p)))
       lispy-avy-style-paren))))

;;;###autoload
(defun lispy-ace-symbol (arg)
  "Jump to a symbol within the current sexp and mark it.
Sexp is obtained by exiting the list ARG times."
  (interactive "p")
  (lispy--out-forward
   (if (region-active-p)
       (progn (deactivate-mark) arg)
     (1- arg)))
  (let ((avy-keys lispy-avy-keys)
        res)
    (avy-with lispy-ace-symbol
      (let ((avy--overlay-offset (if (eq lispy-avy-style-symbol 'at) -1 0)))
        (setq res (lispy--avy-do
                   "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)"
                   (lispy--bounds-dwim)
                   (lambda ()
                     (not (save-excursion
                            (forward-char -1)
                            (lispy--in-string-or-comment-p))))
                   lispy-avy-style-symbol))))
    (unless (memq res '(t nil))
      (unless (or (eq (char-after) ?\")
                  (looking-at ". "))
        (forward-char 1))
      (lispy-mark-symbol))))


(provide 'lispy-ace)
