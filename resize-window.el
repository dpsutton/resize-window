;;; resize-window.el --- easily resize windows          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Dan Sutton  <danielsutton01@gmail.com>
;; Maintainer: Dan Sutton  <danielsutton01@gmail.com>
;; URL: https://github.com/dpsutton/resize-mode

;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
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
  :prefix "rw-")

(defcustom rw-coarse-argument 5
  "Set how big a capital letter movement is."
  :type 'integer)

(defcustom rw-fine-argument 1
  "Set how big the default movement should be."
  :type 'integer)

(defcustom rw-allow-backgrounds t
  "Allow resize mode to set a background.
This is also valuable to see that you are in resize mode."
  :type 'boolean)

(defcustom rw-swap-capital-and-lowercase-behavior nil
  "Reverse default behavior of lower case and uppercase arguments.")

(defcustom rw-notify-with-messages t
  "Show notifications in message bar."
  :type 'boolean)

(defvar rw-background-overlay ()
  "Holder for background overlay.")

(defface rw-background
  '((t (:foreground "gray40")))
  "Face for when resizing window.")

(defun rw-lowercase-argument ()
  "Return the behavior for lowercase entries.
Example, normally n maps to enlarge vertically by 1. However,
if you have swapped capital and lowercase behavior, then
this should return the coarse adjustment."
  (if rw-swap-capital-and-lowercase-behavior
      rw-coarse-argument
    rw-fine-argument))

(defun rw-uppercase-argument ()
  "Return the behavior for uppercase entries.
Example, normally N maps to enlarge vertically by 5. However,
if you have swapped capital and lowercase behavior, then this
should return the fine adjustment (default 1)."
  (if rw-swap-capital-and-lowercase-behavior
      rw-fine-argument
    rw-coarse-argument))

(defvar rw-dispatch-alist
  '((?n rw-enlarge-down          " Resize - Expand down" t)
    (?p rw-enlarge-up            " Resize - Expand up" t)
    (?f rw-enlarge-horizontally  " Resize - horizontally" t)
    (?b rw-shrink-horizontally   " Resize - shrink horizontally" t)
    (?r rw-reset-windows         " Resize - reset window layour" nil)
    (?w rw-cycle-window-positive " Resize - cycle window" nil)
    (?W rw-cycle-window-negative " Resize - cycle window" nil)
    (?? rw-display-menu          " Resize - display menu" nil))
  "List of actions for `rw-dispatch-default.
Main data structure of the dispatcher with the form:
\(char function documentation match-capitals\)")

(defvar rw-alias-list
  '((right ?f)
    (up ?n)
    (left ?b)
    (down ?p))
  "List of aliases for commands.
Rather than have to use n, etc, you can alias keys for others.")

(defun rw-notify (&rest info)
  "Notify with INFO, a string.
This is just a pass through to message usually.  However, it can be
overridden in tests to test the output of message."
  (when rw-notify-with-messages (apply #'message info)))

(defun rw-match-alias (key)
  "Taken the KEY or keyboard selection from `read-key` check for alias.
Match the KEY against the alias table.  If found, return the value that it
points to, which should be a key in the rw-dispatch-alist.
Otherwise, return the key."
  (let ((alias (assoc key rw-alias-list)))
    (if alias
        (car (cdr alias))
      key)))

(defun rw-display-choice (choice)
  "Formats screen message about CHOICE.
CHOICE is a \(key function description allows-capital\)."
  (format "%s: %s " (if (rw-allows-capitals choice)
                        (format "%s|%s"
                                (string (car choice))
                                (string (- (car choice) 32)))
                      (string (car choice)))
          (car (cdr (cdr choice)))))

(defun rw-get-documentation-strings ()
  "Get all documentation strings for display."
  (let ((documentation ""))
    (dolist (choice rw-dispatch-alist)
      (setq documentation
            (concat (rw-display-choice choice) "\n" documentation)))
    documentation))

(defun rw-make-background ()
  "Place a background over the current window."
  (when rw-allow-backgrounds
    (let ((ol (make-overlay
               (point-min)
               (point-max)
               (window-buffer))))
      (overlay-put ol 'face 'rw-background)
      ol)))

(defun rw-execute-action (choice &optional scaled)
  "Given a CHOICE, grab values out of the alist.
If SCALED, then call action with the rw-capital-argument."
  ;; (char function description)
  (let ((action (cadr choice))
        (description (car (cdr (cdr choice)))))
    (if scaled
        (funcall action (rw-uppercase-argument))
      (funcall action))
    (unless (equal (car choice) ??)
      (rw-notify "%s" description))))

(defun rw-allows-capitals (choice)
  "To save time typing, we will tell whether we allow capitals for scaling.
To do so, we check to see whether CHOICE allows for capitals by
checking its last spot in the list for whether it is true or
nil."
  (car (last choice)))

;;;###autoload
(defun resize-window ()
  "Resize the window.
Press n to enlarge down, p to enlarge up, b to enlarge left and f
to enlarge right."
  (interactive)
  (setq rw-background-overlay (rw-make-background))
  (rw-notify "Resize mode: enter character, ? for help")
  (let ((reading-characters t)
        ;; allow mini-buffer to collapse after displaying menu
        (resize-mini-windows t))
    (while reading-characters
      (let* ((char (rw-match-alias (read-key)))
             (choice (assoc char rw-dispatch-alist))
             (capital (assoc (+ char 32) rw-dispatch-alist)))
        (cond
         (choice (rw-execute-action choice))
         ((and capital (rw-allows-capitals capital))
          ;; rather than pass an argument, we tell it to "scale" it
          ;; with t and that method can worry about how to get that
          ;; action
          (rw-execute-action capital t))
         (t (setq reading-characters nil)
            (delete-overlay rw-background-overlay)))))))

;;; Function Handlers
(defun rw-enlarge-down (&optional size)
  "Extend the current window downwards by optional SIZE.
If no SIZE is given, extend by `rw-default-argument`"
  (let ((size (or size (rw-lowercase-argument))))
    (enlarge-window size)))

(defun rw-enlarge-up (&optional size)
  "Bring bottom edge back up by one or optional SIZE."
  (let ((size (or size (rw-lowercase-argument))))
    (enlarge-window (- size))))

(defun rw-enlarge-horizontally (&optional size)
  "Enlarge the window horizontally by one or optional SIZE."
  (let ((size (or size (rw-lowercase-argument))))
    (enlarge-window size t)))

(defun rw-shrink-horizontally (&optional size)
  "Shrink the window horizontally by one or optional SIZE."
  (let ((size (or size (rw-lowercase-argument))))
    (enlarge-window (- size) t)))

(defun rw-reset-windows ()
  "Reset window layout to even spread."
  (balance-windows))

(defun rw-cycle-window-positive ()
  "Cycle windows."
  (delete-overlay rw-background-overlay)
  (other-window 1)
  (setq rw-background-overlay (rw-make-background)))

(defun rw-cycle-window-negative ()
  "Cycle windows negative."
  (delete-overlay rw-background-overlay)
  (other-window -1)
  (setq rw-background-overlay (rw-make-background)))

(defun rw-display-menu ()
  "Display menu in minibuffer."
  (rw-notify "%s" (rw-get-documentation-strings)))

(provide 'resize-window)
;;; resize-window.el ends here
