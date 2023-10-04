;; get this from
;; https://github.com/rorokimdim/bb-pod
(require 'bb-pod)

;; execute this to start tl pod
;;
;; all functions will be loaded under 'tl/' prefix.
(setq pod (bb-pod/start "tl" '("tl") "pod.textlambda.tl"))

(defun tl/load-notebook (pod path)
  "Loads a notebook from PATH.

  If notebook file does not exist at PATH, it will be created."
  (let ((notebook-path (if (file-name-absolute-p path)
                           path
                         (expand-file-name path)))
        (passphrase (read-passwd "Enter passphrase: ")))
    (tl/init pod (list (cons :passphrase passphrase)
                       (cons :notebook-path notebook-path)
                       (cons :create-notebook-if-missing t)))))

;; these two don't require a notebook
(tl/version pod)
(tl/suggest-passphrase pod)

;; Everything else requires a notebook, which can be loaded using tl/load-notebook function

(tl/load-notebook pod "demo.lnb")

;;
;; Look up description of tl/* functions using describe-function (C-h f)
;;

;;; shutdown
(bb-pod/shutdown pod)
