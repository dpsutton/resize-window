;;; test-helper --- Test helper for resize-window

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:
(require 'undercover)
(undercover "*.el" "resize-window/*.el"
            (:exclude "*-test.el")
            (:send-report nil)
            (:report-file "/tmp/undercover-report.json"))
(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar resize-window-test-path
  (f-dirname (f-this-file)))

(defvar resize-window-root-path
  (f-parent resize-window-test-path))

(defvar resize-window-sandbox-path
  (f-expand "sandbox" resize-window-test-path))

(when (f-exists? resize-window-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" resize-window-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory resize-window-sandbox-path))
     (when (f-exists? resize-window-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir resize-window-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'resize-window)

(provide 'test-helper)
;;; test-helper.el ends here
