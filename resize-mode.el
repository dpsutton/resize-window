;;; package -- Summary -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Dan Sutton  <danielsutton01@gmail.com>
;; Maintainer: Dan Sutton  <danielsutton01@gmail.com>
;; URL: https://github.com/dpsutton/resize-mode

;; Version: 0.1.0
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
  :prefix "rw-")

(defcustom rw-capital-argument 5
  "Set how big a capital letter movement is."
  :type 'integer)

(defcustom rw-default-argument 1
  "Set how big the default movement should be."
  :type 'integer)

(defcustom rw-allow-backgrounds t
  "Allow resize mode to set a background.
This is also valuable to see that you are in resize mode."
  :type 'boolean)

(defvar rw-background-overlay ()
  "Holder for background overlay.")

(defface rw-background-face
  '((t (:foreground "gray40")))
  "Face for when resizing window.")

(defvar rw-dispatch-alist
  ;; (key function description allow-caps-for-scaled)
  ()
  "List of actions for `rw-dispatch-default.")

(setq rw-dispatch-alist
      '((?n rw-enlarge-down          " Resize - Expand down" t)
        (?p rw-enlarge-up            " Resize - Expand up" t)
        (?f rw-enlarge-horizontally  " Resize - horizontally" t)
        (?b rw-shrink-horizontally   " Resize - shrink horizontally" t)
        (?r rw-reset-windows         " Resize - reset window layour" nil)
        (?w rw-cycle-window-positive " Resize - cycle window" nil)
        (?W rw-cycle-window-negative " Resize - cycle window" nil)
        (?? rw-display-menu          " Resize - display menu" nil)))

(defun rw-display-choice (choice)
  "Formats screen message about CHOICE.
CHOICE is a (key function description allows-capital."
  (format "%s: %s " (if (rw-allows-capitals choice)
                        (format "%s|%s" (string (car choice)) (string (- (car choice) 32)))
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
      (overlay-put ol 'face 'rw-background-face)
      ol)))

(defun rw-execute-action (choice &optional scaled)
  "Given a CHOICE, grab values out of the alist.
If SCALED, then call action with the rw-capital-argument."
  ;; (char function description)
  (let ((action (cadr choice))
        (description (car (cdr (cdr choice)))))
    (progn
      (if scaled
          (funcall action rw-capital-argument)
        (funcall action))
      (unless (equalp (car choice) ??)
        (message "%s" description)))))

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
to enlarge right.  Current ARG is not supported."
  (interactive)
  (setq rw-background-overlay (rw-make-background))
  (message "Resize mode: enter character, ? for help")
  (let ((reading-characters t)
        ;; allow mini-buffer to collapse after displaying menu
        (resize-mini-windows t))
    (while reading-characters
      (let* ((char (read-char-exclusive))
             (choice (assoc char rw-dispatch-alist))
             (capital (assoc (+ char 32) rw-dispatch-alist)))
        (if choice
            (rw-execute-action choice)
          (if (and capital (rw-allows-capitals capital))
              (rw-execute-action capital t)
            (progn
              (setq reading-characters nil)
              (delete-overlay rw-background-overlay))))))))

;;; Function Handlers
(defun rw-enlarge-down (&optional size)
  "Extend the current window downwards by optional SIZE.
If no SIZE is given, extend by `rw-default-argument`"
  (let ((size (or size rw-default-argument)))
    (enlarge-window size)))

(defun rw-enlarge-up (&optional size)
  "Bring bottom edge back up by one or optional SIZE."
  (let ((size (or size rw-default-argument)))
    (enlarge-window (- size))))

(defun rw-enlarge-horizontally (&optional size)
  "Enlarge the window horizontally by one or optional SIZE."
  (let ((size (or size rw-default-argument)))
    (enlarge-window size t)))

(defun rw-shrink-horizontally (&optional size)
  "Shrink the window horizontally by one or optional SIZE."
  (let ((size (or size rw-default-argument)))
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
  (message "%s" (rw-get-documentation-strings)))

(provide 'resize-mode)
;;; resize-mode ends here
