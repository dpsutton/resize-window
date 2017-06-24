;;; Test for `resize-window'

;;; Commentary:
;; These are the tests for `resize-window'

;;; Code:

(defmacro aliases=> (alias original)
  `(should (equal ,original (resize-window--match-alias ,alias))))

(ert-deftest should-match-aliases ()
  (aliases=> 'right ?f)
  (aliases=> 'left ?b)
  (aliases=> 'up ?n)
  (aliases=> 'down ?p))

(ert-deftest should-return-original-if-no-alias ()
  (aliases=> ?d ?d))

(defvar choice-no-capital '(?n 'function "documentation" nil))
(defvar choice-capital '(?n 'function "documentation" t))

(ert-deftest should-create-documentation-from-alist ()
  (should (equal "n: documentation "
                 (resize-window--display-choice choice-no-capital)))
  (should (equal "n|N: documentation "
                 (resize-window--display-choice choice-capital))))

(ert-deftest should-execute-and-display-message ()
  (let ((choice '(?n (lambda () (setq executed t)) "doc" nil))
        resize-window-notify-with-messages             ;suppress messages
        (executed))
    (resize-window--execute-action choice)
    (should executed)))

(ert-deftest should-identify-which-allow-capital-matching ()
  (should (resize-window--allows-capitals choice-capital))
  (should-not (resize-window--allows-capitals choice-no-capital)))

;;; tests for swapping uppercase and lowercase behavior
(ert-deftest resize-window-swap-capital-and-lowercase-behavior-swaps ()
  (let ((resize-window-swap-capital-and-lowercase-behavior t))
    (should (equal resize-window-coarse-argument
                   (resize-window-lowercase-argument)))
    (should (equal resize-window-fine-argument
                   (resize-window-uppercase-argument)))))

(ert-deftest resize-window-swap-capital-and-lowercase-behavior-ignored ()
  (let ((resize-window-swap-capital-and-lowercase-behavior))
    (should (equal resize-window-coarse-argument
                   (resize-window-uppercase-argument)))
    (should (equal resize-window-fine-argument
                   (resize-window-lowercase-argument)))))

(ert-deftest resize-window--key-already-used-tests ()
  (should (resize-window--key-available? ?e))
  (should-not (resize-window--key-available? ?n)))
