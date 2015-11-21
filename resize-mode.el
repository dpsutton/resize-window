;;; package -- Summary

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Dan Sutton  <danielsutton01@gmail.com>
;; Maintainer: Dan Sutton  <danielsutton01@gmail.com>
;; URL: https://github.com/dpsutton/resize-mode
;; Package-Version: 20150803.837
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: window, resize

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Easily allows you to resize windows. Rather than guessing that you
;; want `C-u 17 C-x {`, you could just press FFff, which enlarges 5
;; lines, then 5 lines, then one and then one. The idea is that the
;; normal motions n,p,f,b along with r for reset and w for cycling
;; windows allows for super simple resizing of windows. All of this is
;; inside of a while loop so that you don't have to invoke more chords
;; to resize again, but just keep using standard motions until you are
;; happy.

;; All of the work is done inside of resize-window. Its just a while
;; loop that keeps looping over character input until it doesn't
;; recognize an option or an allowable capital. The dispatch alist has
;; a character code to look for, a function to invoke, a string for
;; display and whether to match against capital letters. If so, it is
;; invoked with the default capital argument rather than the default
;; argument.

;;; Code:

(defgroup resize-window nil
  "Quickly resize current window"
  :group 'convenience
  :prefix "rm-")

(defcustom rm-capital-argument 5
  "Set how big a capital letter movement is."
  :type 'integer)

(defcustom rm-default-argument 1
  "Set how big the default movement should be."
  :type 'integer)

(defcustom rm-allow-backgrounds t
  "Allow resize mode to set a background.
This is also valuable to see that you are in resize mode."
  :type 'boolean)

(defvar rm-background-overlay ()
  "Holder for background overlay.")

(defface rm-background-face
  '((t (:foreground "gray40")))
  "Face for when resizing window.")

(defvar rm-dispatch-alist
  ;; key function description allow-caps-for-scaled
  '((?n rm-enlarge-down          " Resize - Expand down" t)
    (?p rm-enlarge-up            " Resize - Expand up" t)
    (?f rm-enlarge-horizontally  " Resize - horizontally" t)
    (?b rm-shrink-horizontally   " Resize - shrink horizontally" t)
    (?r rm-reset-windows         " Resize - reset window layour" nil)
    (?w rm-cycle-window-positive " Resize - cycle window" nil)
    (?W rm-cycle-window-negative " Resize - cycle window" nil)
    (?? rm-display-menu          " Resize - display menu" nil))
  "List of actions for `rm-dispatch-default.")

(defun rm-display-choice (choice)
  "Formats screen message about CHOICE.
CHOICE is a (key function description allows-capital."
  (format "%s: %s " (if (rm-allows-capitals choice)
                        (format "%s|%s" (string (car choice)) (string (- (car choice) 32)))
                      (string (car choice)))
          (car (cdr (cdr choice)))))

(defun rm-get-documentation-strings ()
  "Get all documentation strings for display."
  (let ((documentation ""))
    (dolist (choice rm-dispatch-alist)
      (setq documentation
            (concat (rm-display-choice choice) "\n" documentation)))
    documentation))

(defun rm-make-background ()
  "Place a background over the current window."
  (when rm-allow-backgrounds
    (let ((ol (make-overlay
               (point-min)
               (point-max)
               (window-buffer))))
      (overlay-put ol 'face 'rm-background-face)
      ol)))

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
      (unless (equalp (car choice) ??)
        (message "%s" description)))))

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
  (message "Resize mode: enter character, ? for help")
  (let ((reading-characters t)
        ;; allow mini-buffer to collapse after displaying menu
        (resize-mini-windows t))
    (while reading-characters
      (let* ((char (read-char-exclusive))
             (choice (assoc char rm-dispatch-alist))
             (capital (assoc (+ char 32) rm-dispatch-alist)))
        (if choice
            (rm-execute-action choice)
          (if (and capital (rm-allows-capitals capital))
              (rm-execute-action capital t)
            (progn
              (setq reading-characters nil)
              (delete-overlay rm-background-overlay))))))))

;;; Function Handlers
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

(defun rm-display-menu ()
  "Display menu in minibuffer."
  (message "%s" (rm-get-documentation-strings)))

(provide 'resize-mode)
;;; resize-mode ends here
