(in-package :tl)

;;
;; First, make sure to symlink this
;; code to your quicklisp projects folder:
;;
;; ln -s {path-to-folder-where-this-file-is-in} tl
;;
;; Next, do this in repl
;;
;; (ql:quickload :tl)
;; (in-package :tl)
;;
;; Then call tl/start to get started.
;;
;; (tl/start)

(defparameter *tl-command-path* "tl"
  "Full path to tl command.")

(defvar *tl-pod* nil
  "Active tl pod process.")

(defun pod/start (command-path &key (environment nil))
  "Starts a pod process."
  (external-program:start command-path
                          nil
                          :environment environment
                          :output :stream
                          :input :stream))

(defun pod/describe (pod)
  "Describes a pod."
  (let ((msg (make-hash-table)))
    (setf (gethash "op" msg) "describe")
    (pod/write pod msg)
    (pod/read pod)))

(defun pod/shutdown (pod)
  "Shutsdown a pod process."
  (let ((msg (make-hash-table)))
    (setf (gethash "op" msg) "shutdown")
    (pod/write pod msg)
    (close (external-program:process-input-stream pod))
    (close (external-program:process-output-stream pod))))

(defun pod/write (pod object)
  "Writes object to a pod process."
  (let ((ostream (external-program:process-input-stream pod)))
    (bencode:encode object ostream)
    (force-output ostream)))

(defun pod/read (pod)
  "Reads from a pod process."
  (bencode:decode (external-program:process-output-stream pod)))

(defun pod/load-functions (pod ns)
  "Loads all functions in given namespace from a pod process."
  (let* ((description (pod/describe pod))
         (namespaces (remove-if-not
                      (lambda (ht)
                        (equalp (gethash "name" ht) ns))
                      (gethash "namespaces" description)))
         (vars (gethash "vars" (first namespaces)))
         (parse-meta (lambda (meta)
                       ;; baby's first edn parser using regex
                       (list
                        (ppcre:register-groups-bind (docstring)
                            (".*:doc\\s+\"(.*?)\"" meta :sharedp t)
                          (ppcre:regex-replace-all "\\\\n" docstring (format nil "~%")))
                        (ppcre:register-groups-bind (arglists)
                            (".*?:arglists\\s+\\((.*?)\\)" meta :sharedp t)
                          (ppcre:regex-replace-all "&" arglists (format nil "&rest")))))))
    (loop for x in vars do
      (let* ((name (gethash "name" x))
             (meta (funcall parse-meta (gethash "meta" x)))
             (docstring (first meta))
             (arglists (second meta))
             (fn-symbol (intern (concatenate 'string "TL/" (string-upcase name)))))
        (setf (symbol-function fn-symbol)
              (lambda (&rest args)
                (apply #'tl/invoke name args)))
        (setf (documentation fn-symbol 'function) (format nil "~a~% args: ~a" docstring arglists))))))

(defun tl/start ()
  "Starts tl process as a babashka pod."
  (setf *tl-pod* (pod/start *tl-command-path* :environment '(("BABASHKA_POD" . "true"))))
  (pod/load-functions *tl-pod* "pod.textlambda.tl")
  (tl/help)
  (format t "▸ Use (describe 'tl/FUNCTION-NAME) for more on any particular function.~%")
  (format t "▸ Use (load-tl FILE-PATH ENCRYPTION-KEY) to load a tl file.~%"))

(defun tl/running? ()
  "Checks if tl process is running."
  (and *tl-pod*
       (eq (external-program:process-status *tl-pod*) :RUNNING)))

(defun tl/stop ()
  "Stops tl process if it is running."
  (when (tl/running?)
    (pod/shutdown *tl-pod*)))

(defun tl/invoke (name &rest args)
  "Invokes a tl command by name."
  (unless (tl/running?)
    (tl/start))
  (clear-input (external-program:process-output-stream *tl-pod*))
  (let ((msg (make-hash-table)))
    (setf (gethash "op" msg) "invoke"
          (gethash "id" msg) (format nil "~a-~a" name (uuid:make-v4-uuid))
          (gethash "var" msg) (format nil "pod.textlambda.tl/~a" name)
          (gethash "args" msg) (jonathan:to-json args))
    (pod/write *tl-pod* msg)
    (let* ((response (pod/read *tl-pod*))
           (ex-message (gethash "ex-message" response))
           (value (gethash "value" response)))
      (if ex-message
          (error ex-message)
          (jonathan:parse value :as :hash-table)))))

(defun load-tl (path encryption-key)
  "Loads tl file from given path.

  If PATH does not exist, it will be created."
  (let ((msg (make-hash-table)))
    (setf (gethash "passphrase" msg) encryption-key
          (gethash "notebook-path" msg) path
          (gethash "create-notebook-if-missing" msg) t)
    (tl/invoke "init" msg)))

(defun short-function-documentation (fn)
  (first (uiop:split-string (documentation fn 'function)
                            :separator '(#\Newline))))

(defun tl/help ()
  "Print some help on tl functions."
  (let ((tl-functions (remove-if-not
                          (curry #'uiop:string-prefix-p "TL/")
                          (all-function-symbols :tl)))
        (table (t:make-table '(function description) :header "tl")))
    (loop for fn in tl-functions
          do (t:add-row table (list fn (short-function-documentation fn))))
    (t:display table)))

(defun curry (fn &rest init-args)
  (lambda (&rest args)
    (apply fn (append init-args args))))

(defun all-function-symbols (package-name)
  "Retrieves all function symbols from a package.

  Taken from http://reference-error.org/2015/08/30/common-lisp-finding-all-functions-in-a-package.html"
  (declare ((or package string symbol) package-name))
  (the list
       (let ((lst (list))
             (package (find-package package-name)))
         (cond (package
                (do-all-symbols (symb package)
                  (when (and (fboundp symb)
                             (eql (symbol-package symb) package))
                    (push symb lst)))
                lst)
               (t
                (error "~S does not designate a package" package-name))))))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
                using (hash-value value)
                collect (list key value))))
