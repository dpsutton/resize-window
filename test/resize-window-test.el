;;; Test for `resize-window'

;;; Commentary:
;; These are the tests for `resize-window'

;;; Code:

(defvar choice-no-capital '(?n 'function "documentation" nil))
(defvar choice-capital '(?n 'function "documentation" t))

(ert-deftest should-match-aliases ()
  (should (equal ?f (resize-window-match-alias 'right)))
  (should (equal ?b (resize-window-match-alias 'left)))
  (should (equal ?n (resize-window-match-alias 'up)))
  (should (equal ?p (resize-window-match-alias 'down))))

(ert-deftest should-return-original-if-no-alias ()
  (should (equal ?d (resize-window-match-alias ?d))))

(ert-deftest should-create-documentation-from-alist ()
  (should (equal "n: documentation "
                 (resize-window-display-choice choice-no-capital)))
  (should (equal "n|N: documentation "
                 (resize-window-display-choice choice-capital))))

(ert-deftest should-execute-and-display-message ()
  (let ((choice '(?n (lambda () (setq executed t)) "doc" nil))
        resize-window-notify-with-messages             ;suppress messages
        (resize-window-notify)
        (message-written)
        (executed))
    (resize-window-execute-action choice)
    (should (equal executed t))))

(ert-deftest should-identify-which-allow-capital-matching ()
  (should (resize-window-allows-capitals choice-capital))
  (should-not (resize-window-allows-capitals choice-no-capital)))

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
