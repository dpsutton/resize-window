(require 'f)

(defvar resize-window-support-path
  (f-dirname load-file-name))

(defvar resize-window-features-path
  (f-parent resize-window-support-path))

(defvar resize-window-root-path
  (f-parent resize-window-features-path))

(add-to-list 'load-path resize-window-root-path)

(require 'resize-window)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
