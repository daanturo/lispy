;; -*- lexical-binding: t; -*-

(require 'hydra)

(require 'lispy)

(defhydra hydra-lispy-x (:exit t
                         :hint nil
                         :columns 3)
  "x"
  ;; ("a" nil)
  ("b" lispy-bind-variable "bind variable")
  ("c" lispy-to-cond "to cond")
  ("C" lispy-cleanup "cleanup")
  ("d" lispy-to-defun "to defun")
  ("D" lispy-extract-defun "extract defun")
  ("e" lispy-edebug "edebug")
  ("f" lispy-flatten "flatten")
  ("F" lispy-let-flatten "let-flatten")
  ;; ("g" nil)
  ("h" lispy-describe "describe")
  ("i" lispy-to-ifs "to ifs")
  ("j" lispy-debug-step-in "debug step in")
  ("k" lispy-extract-block "extract block")
  ("l" lispy-to-lambda "to lambda")
  ("m" lispy-cursor-ace "multi cursor")
  ("n" lispy-cd)
  ;; ("o" nil)
  ("p" lispy-set-python-process "process")
  ;; ("q" nil)
  ("r" lispy-eval-and-replace "eval and replace")
  ("s" save-buffer)
  ("t" lispy-view-test "view test")
  ("v" lispy-eval-expression "eval")
  ("w" lispy-show-top-level "where")
  ;; ("x" nil)
  ;; ("y" nil)
  ;; ("z" nil)
  ("B" lispy-store-region-and-buffer "store list bounds")
  ("R" lispy-reverse "reverse")
  ("T" lispy-ert "ert")
  (">" lispy-toggle-thread-last "toggle last-threaded form")
  ("" lispy-x-more-verbosity :exit nil)
  ("?" lispy-x-more-verbosity "help" :exit nil))

;;;###autoload
(autoload #'hydra-lispy-x/body "lispy-hydra")

(defhydra lh-knight ()
  "knight"
  ("j" lispy-knight-down)
  ("k" lispy-knight-up)
  ("z" nil))

;;;###autoload
(autoload #'lh-knight/body "lispy-hydra")

(provide 'lispy-hydra)
