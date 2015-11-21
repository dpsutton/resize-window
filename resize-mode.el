;;; package -- Summary
;;; Commentary:

;;; Code:
;; (defvar resize-mode nil
;;   "Minor mode for resizing windows.")

(defgroup resize-window nil
  "Quickly resize current window"
  :group 'convenience
  :prefix "rm-")

(defvar rm-dispatch-alist ()
  "List of actions for `rm-dispatch-default.")

(defvar rm-capital-argument ()
  "Set how big a capital letter movement is.")

(setq rm-capital-argument 5)

(defvar rm-default-argument ()
  "Set how big the default movement should be.")

(defvar rm-allow-backgrounds ()
  "Allow resize mode to set a background.
This is also valuable to see that you are in resize mode.")

(setq rm-allow-backgrounds t)

(defvar rm-background-overlay ()
  "Holder for background overlay.")

(defface rm-background-face
  '((t (:foreground "gray40")))
  "Face for when resizing window.")

(defun rm-make-background ()
    "Place a background over the current window."
  (when rm-allow-backgrounds
    (let ((ol (make-overlay
               (point-min)
               (point-max)
               (window-buffer))))
      (overlay-put ol 'face 'rm-background-face)
      ol)))

(setq rm-default-argument 1)

(setq rm-dispatch-alist
      ;; key function description allow-caps-for-scaled
      '((?n rm-enlarge-down          " Resize - Expand down" t)
        (?p rm-enlarge-up            " Resize - Expand up" t)
        (?f rm-enlarge-horizontally  " Resize - horizontally" t)
        (?b rm-shrink-horizontally   " Resize - shrink horizontally" t)
        (?r rm-reset-windows         " Resize -- reset window layour" nil)
        (?w rm-cycle-window-positive " Resize - cycle window" nil)
        (?W rm-cycle-window-negative " Resize - cycle window" nil)))

(defun rm-execute-action (choice &optional scaled)
  "Given a CHOICE, grab values out of the alist.
If SCALED, then call action with the rm-capital-argument."
  ;; (char function description)
  (let ((action (cadr choice))
        (description (car (cdr (cdr choice)))))
    (progn
      (if scaled
          (funcall action rm-capital-argument)
        (funcall action))
      (message "%s" description))))

(defun rm-allows-capitals (choice)
  "To save time typing, we will tell whether we allow capitals for scaling.
To do so, we check to see whether CHOICE allows for capitals by
checking its last spot in the list for whether it is true or
nil."
  (car (last choice)))

;;;###autoload
(defun resize-window ()
  "Resize the window.
Press n to enlarge down, p to enlarge up, b to enlarge left and f
to enlarge right.  Current ARG is not supported."
  (interactive)
  (setq rm-background-overlay (rm-make-background))
  (let ((rm-active t))
    (while rm-active
      (let* ((char (read-char-exclusive))
             (choice (assoc char rm-dispatch-alist))
             (capital (assoc (+ char 32) rm-dispatch-alist)))
        (if choice
            (rm-execute-action choice)
          (if (and capital (rm-allows-capitals capital))
              (rm-execute-action capital t)
            (progn
              (setq rm-active nil)
              (delete-overlay rm-background-overlay))))))))

(defun rm-enlarge-down (&optional size)
  "Extend the current window downwards by optional SIZE.
If no SIZE is given, extend by `rm-default-argument`"
  (let ((size (or size rm-default-argument)))
    (enlarge-window size)))

(defun rm-enlarge-up (&optional size)
  "Bring bottom edge back up by one or optional SIZE."
  (let ((size (or size rm-default-argument)))
    (enlarge-window (- size))))

(defun rm-enlarge-horizontally (&optional size)
  "Enlarge the window horizontally by one or optional SIZE."
  (let ((size (or size rm-default-argument)))
    (enlarge-window size t)))

(defun rm-shrink-horizontally (&optional size)
  "Shrink the window horizontally by one or optional SIZE."
  (let ((size (or size rm-default-argument)))
    (enlarge-window (- size) t)))

(defun rm-reset-windows ()
  "Reset window layout to even spread."
  (balance-windows))

(defun rm-cycle-window-positive ()
  "Cycle windows."
  (delete-overlay rm-background-overlay)
  (other-window 1)
  (setq rm-background-overlay (rm-make-background)))

(defun rm-cycle-window-negative ()
  "Cycle windows negative."
  (delete-overlay rm-background-overlay)
  (other-window -1)
  (setq rm-background-overlay (rm-make-background)))

(provide 'resize-mode)
;;; resize-mode ends here
