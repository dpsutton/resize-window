;;; resize-window.el --- Easily resize windows          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Dan Sutton  <danielsutton01@gmail.com>
;; Maintainer: Dan Sutton  <danielsutton01@gmail.com>
;; URL: https://github.com/dpsutton/resize-mode

;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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
;; Easily allows you to resize windows.  Rather than guessing that you
;; want `C-u 17 C-x {`, you could just press FFff, which resizes 5
;; lines, then 5 lines, then one and then one.  The idea is that the
;; normal motions n,p,f,b along with e for even and w for cycling
;; windows allows for super simple resizing of windows.  All of this is
;; inside of a while loop so that you don't have to invoke more chords
;; to resize again, but just keep using standard motions until you are
;; happy.

;; Just run `M-x resize-window`. There are only a few commands to learn,
;; and they mimic the normal motions in emacs.

;;   n : Resize the window vertically like scrolling down.
;;        N  for 5 lines at once.
;;   p : Resize the window vertically like scrolling up.
;;        P  for 5 lines at once.
;;   f : Resize the window horizontally like scrolling forward.
;;        F  for 5 lines at once.
;;   b : Resize the window horizontally like scrolling backward.
;;        B  for 5 lines at once.
;;   w : Cycle through windows so that you can adjust other window panes.
;;        W  cycle in the opposite direction.
;;   e : Even the size of the windows.
;;   2 : Split the window horizontally.
;;   3 : Split the window vertically.
;;   0 : Delete the current window.
;;   k : Delete other windows and save the state on the stack.
;;   s : Save the state on the stack so you may restore it later.
;;   y : Restore to a previous saved state.
;;   ? : Display the help menu listing commands.


;;; Code:

(require 'cl-lib)

(defgroup resize-window nil
  "Quickly resize windows."
  :group 'convenience
  :prefix "rw-")

(defcustom resize-window-coarse-argument 5
  "Set how big a capital letter movement is."
  :type 'integer
  :group 'resize-window)

(defcustom resize-window-fine-argument 1
  "Set how big the default movement should be."
  :type 'integer
  :group 'resize-window)

(defcustom resize-window-allow-backgrounds t
  "Allow resize mode to set backgrounds.
This is also valuable to see that you are in resize mode."
  :type 'boolean
  :group 'resize-window)

(defcustom resize-window-unregistered-key-quit nil
  "Quit when an unregistered key is pressed.
If nil do not quit and notify the unregistered key pressed."
  :type 'boolean
  :group 'resize-window)

(defcustom resize-window-stack-size 16
  "Size of the stack for holding window configurations."
  :type 'integer
  :group 'resize-window)

(defcustom resize-window-swap-capital-and-lowercase-behavior nil
  "Reverse default behavior of lower case and uppercase arguments."
  :type 'boolean
  :group 'resize-window)

(defcustom resize-window-notify-with-messages t
  "Show notifications in message bar."
  :type 'boolean
  :group 'resize-window)

(defvar resize-window--background-overlays ()
  "List of background overlays.")

(defvar resize-window--window-stack ()
  "Stack for holding window configurations.

It is an alist of format ((configuration . time)...), where time
is the time when the configuration was saved/visited.")

(defface resize-window-background
  '((t (:foreground "gray40")))
  "Face for when resizing window.")

(defun resize-window-lowercase-argument ()
  "Return the behavior for lowercase entries.
Example, normally n maps to resize vertically by 1. However,
if you have swapped capital and lowercase behavior, then
this should return the coarse adjustment."
  (if resize-window-swap-capital-and-lowercase-behavior
      resize-window-coarse-argument
    resize-window-fine-argument))

(defun resize-window-uppercase-argument ()
  "Return the behavior for uppercase entries.
Example, normally N maps to resize vertically by 5. However,
if you have swapped capital and lowercase behavior, then this
should return the fine adjustment (default 1)."
  (if resize-window-swap-capital-and-lowercase-behavior
      resize-window-fine-argument
    resize-window-coarse-argument))

(defvar resize-window-dispatch-alist
  '((?n resize-window--resize-downward       "Resize downward" t)
    (?p resize-window--resize-upward         "Resize upward" t)
    (?f resize-window--resize-forward        "Resize forward" t)
    (?b resize-window--resize-backward       "Resize backward" t)
    (?w resize-window--cycle-window-positive "Next window" nil)
    (?W resize-window--cycle-window-negative "Previous window" nil)
    (?e resize-window--reset-windows         "Even layout (save state)" nil)
    (?2 resize-window--split-window-below    "Split below (save state)" nil)
    (?3 resize-window--split-window-right    "Split right (save state)" nil)
    (?0 resize-window--delete-window         "Delete window (save state)" nil)
    (?k resize-window--kill-other-windows    "Delete other windows (save state)" nil)
    (?s resize-window--window-push           "Save state" nil)
    (?y resize-window--restore-windows       "Restore state (save state)" nil)
    (?? resize-window--display-menu          "Toggle help menu" nil))
  "List of resize mode bindings.
Main data structure of the dispatcher with the form:
\(key function documentation allows-capitals\)")

(defvar resize-window-alias-list
  '((right ?f)
    (down ?n)
    (left ?b)
    (up ?p))
  "List of aliases for commands.
Rather than have to use n, etc, you can alias keys for others.")

(defun resize-window--notify (&rest info)
  "Notify with INFO, a string or list (format-string object...).
This is just a pass through to message usually.  However, it can be
overridden in tests to test the output of message."
  (when resize-window-notify-with-messages (apply #'message info)))

(defun resize-window--key-str (key)
  "Return the string representation of KEY.
KEY is a symbol, character (integer), key text, or key sequence.

For instance, ?n \"n\" [?n] [(?n)] are considered the same, and
?\\C-n \"C-n\" \"\\C-n\" [?\\C-n] [(?\\C-n)] [(control ?n)] too."
  ;; NOTE: Fail loudly when KEY is wrong to help debugging.
  (key-description
   (cond
    ((and (not (booleanp key))
          (or (symbolp key) (integerp key)))
     (vector key))
    ((stringp key)
     (kbd key))
    ((vectorp key)
     key)
    (t
     (signal 'wrong-type-argument
             `((symbolp integerp stringp vectorp) ,key))))))

(defun resize-window--keys-equal (&rest keys)
  "Return non-nil if KEYS are considered equal.
If there is only one key return non-nil."
  (let ((key-str (resize-window--key-str (car keys))))
    (not (cl-find-if-not
          (lambda (k)
            (string= key-str (resize-window--key-str k)))
          (cdr keys)))))

(defun resize-window--key-to-lower (key)
  "Return the lowercase key sequence of KEY.
Return nil if KEY isn't an uppercase letter."
  (let* ((key-str (resize-window--key-str key))
         (char (if (= (length key-str) 1) (string-to-char key-str))))
    (and char
         (member char resize-window--capital-letters)
         (vector (+ char 32)))))

(defun resize-window--key-to-upper (key)
  "Return the uppercase key sequence of KEY.
Return nil if KEY isn't an lowercase letter."
  (let* ((key-str (resize-window--key-str key))
         (char (if (= (length key-str) 1) (string-to-char key-str))))
    (and char
         (member char resize-window--lower-letters)
         (vector (- char 32)))))

(defun resize-window--key-element (key sequence)
  "Return the first element in SEQUENCE whose car equals KEY."
  (let ((key-str (resize-window--key-str key)))
    (cl-assoc-if
     (lambda (k)
       (string= key-str (resize-window--key-str k)))
     sequence)))

(defun resize-window--match-alias (key)
  "Taken the KEY or keyboard selection check for alias.
Match the KEY against the alias table.  If found, return the value that it
points to, which should be a key in the `resize-window-dispatch-alist'.
Otherwise, return the KEY."
  (let ((alias (resize-window--key-element
                key resize-window-alias-list)))
    (if alias
        (car (cdr alias))
      key)))

(defun resize-window--match-dispatch (key)
  "Taken the KEY or keyboard selection check for an action.
Match the KEY against the alias table `resize-window-dispatch-alist'."
  (resize-window--key-element
   key resize-window-dispatch-alist))

(defun resize-window--choice-keybinding (choice)
  "Get the keybinding associated with CHOICE."
  (car choice))

(defun resize-window--choice-documentation (choice)
  "Get the documentation associated with CHOICE."
  (car (cdr (cdr choice))))

(defun resize-window--choice-lambda (choice)
  "Get the lambda associated with CHOICE."
  (car (cdr choice)))

(defun resize-window--allows-capitals (choice)
  "To save time typing, we will tell whether we allow capitals for scaling.
To do so, we check to see whether CHOICE allows for capitals by
checking its last spot in the list for whether it is true or
nil."
  (car (last choice)))

(defun resize-window--display-choice (choice)
  "Formats screen message about CHOICE.
CHOICE is a \(key function documentation allows-capitals\)."
  (let ((key (resize-window--choice-keybinding choice)))
    (concat (if (resize-window--allows-capitals choice)
                (format "%s|%s"
                        (resize-window--key-str key)
                        (resize-window--key-str
                         (resize-window--key-to-upper key)))
              (format " %s "
                      (resize-window--key-str key)))
            " : "
            (resize-window--choice-documentation choice))))

(defun resize-window--get-documentation-strings ()
  "Return documented keybindings as a multiline string."
  (mapconcat #'identity (mapcar 'resize-window--display-choice
                                resize-window-dispatch-alist)
             "\n"))

(defun resize-window--add-backgrounds ()
  "Place an overlay background over other windows."
  (resize-window--remove-backgrounds)
  (when resize-window-allow-backgrounds
    (let ((windows (remq (selected-window) (window-list nil -1))))
      (dolist (window windows)
        (with-current-buffer (window-buffer window)
          (let ((ol (make-overlay
                     (point-min)
                     (point-max)
                     (current-buffer))))
            (overlay-put ol 'face 'resize-window-background)
            (overlay-put ol 'window window)
            (push ol resize-window--background-overlays)))))))

(defun resize-window--remove-backgrounds ()
  "Remove the overlay backgrounds."
  (mapc 'delete-overlay resize-window--background-overlays)
  (setq resize-window--background-overlays nil))

(defun resize-window--execute-action (choice &optional scaled)
  "Given a CHOICE, grab values out of the alist.
CHOICE is a \(key function documentation allows-capitals\).
If SCALED, then call action with the `resize-window-uppercase-argument'."
  (let ((action (resize-window--choice-lambda choice))
        (description (resize-window--choice-documentation choice)))
    (unless (resize-window--keys-equal
             (resize-window--choice-keybinding choice) [??])
      (resize-window--notify "%s" description))
    (condition-case nil
        (if scaled
            (funcall action (resize-window-uppercase-argument))
          (funcall action))
      (wrong-number-of-arguments
       (resize-window--notify
        "Invalid arity in function for %s"
        (resize-window--key-str
         (resize-window--choice-keybinding choice)))))))

;;;###autoload
(defun resize-window ()
  "Resize the window.
Press n to resize down, p to resize up, b to resize left and f
to resize right."
  (interactive)
  (resize-window--refresh-stack)
  ;; NOTE: Do not trim the stack here. Let stack requests to handle
  ;; window configurations in excess.
  (resize-window--add-backgrounds)
  (resize-window--notify "Resize mode: insert KEY, ? for help")
  (condition-case nil
      (let ((reading-keys t)
            ;; allow mini-buffer to collapse after displaying menu
            (resize-mini-windows t))
        (while reading-keys
          (let* ((kin (read-key-sequence-vector nil nil t))
                 (key (and kin (resize-window--match-alias kin)))
                 (choice (and key (resize-window--match-dispatch key)))
                 (lower (and key (resize-window--key-to-lower key)))
                 (capital (and lower (resize-window--match-dispatch lower))))
            (cond
             (choice (resize-window--execute-action choice))
             ((and capital (resize-window--allows-capitals capital))
              ;; rather than pass an argument, we tell it to "scale" it
              ;; with t and that method can worry about how to get that
              ;; action
              (resize-window--execute-action capital t))
             ((or resize-window-unregistered-key-quit
                  (resize-window--keys-equal key [?q])
                  (resize-window--keys-equal key [?Q])
                  (resize-window--keys-equal key [? ])
                  (resize-window--keys-equal key "C-g"))
              (setq reading-keys nil)
              (message nil)
              (resize-window--display-menu 'kill)
              (resize-window--remove-backgrounds))
             (t
              (resize-window--notify
               (format
                "Unregistered key: %s -> %s"
                key (resize-window--key-str key))))))))
    (quit
     (message nil)
     (resize-window--display-menu 'kill)
     (resize-window--remove-backgrounds))))

;;; Function Handlers
(defun resize-window--resize-downward (&optional size)
  "Resize the window vertically downward by optional SIZE.
If no SIZE is given, modify by `resize-window-default-argument'"
  (unless (frame-root-window-p (selected-window))
    (let ((size (or size (resize-window-lowercase-argument)))
          (direction (if (window-in-direction 'below) 1 -1)))
      (enlarge-window (* size direction)))))

(defun resize-window--resize-upward (&optional size)
  "Resize the window vertically upward by optional SIZE.
If no SIZE is given, modify by `resize-window-default-argument'"
  (unless (frame-root-window-p (selected-window))
    (let ((size (or size (resize-window-lowercase-argument)))
          (direction (if (window-in-direction 'below) -1 1)))
      (enlarge-window (* size direction)))))

(defun resize-window--resize-forward (&optional size)
  "Resize the window horizontally forward by optional SIZE.
If no SIZE is given, modify by `resize-window-default-argument'"
  (unless (frame-root-window-p (selected-window))
    (let ((size (or size (resize-window-lowercase-argument)))
          (direction (if (window-in-direction 'right) 1 -1)))
      (enlarge-window (* size direction) t))))

(defun resize-window--resize-backward (&optional size)
  "Resize the window horizontally backward by optional SIZE.
If no SIZE is given, modify by `resize-window-default-argument'"
  (unless (frame-root-window-p (selected-window))
    (let ((size (or size (resize-window-lowercase-argument)))
          (direction (if (window-in-direction 'right) -1 1)))
      (enlarge-window (* size direction) t))))

(defun resize-window--reset-windows ()
  "Reset window layout to even spread."
  (resize-window--window-push)
  (balance-windows))

(defun resize-window--cycle-window-positive ()
  "Cycle windows."
  (other-window 1)
  (resize-window--add-backgrounds))

(defun resize-window--cycle-window-negative ()
  "Cycle windows negative."
  (other-window -1)
  (resize-window--add-backgrounds))

(defun resize-window--display-menu (&optional action)
  "Toggle help menu side window or perform ACTION if non-nil.
ACTION is a symbol of value 'kill or 'open."
  (let* ((buffer (get-buffer-create "*Resize-Window-Help*"))
         (window (get-buffer-window buffer))
         (add-backgrounds nil))
    (cond
     ((and window (or (not action) (eq action 'kill)))
      (quit-window t window)
      (setq add-backgrounds t))
     ((and (not window) (or (not action) (eq action 'open)))
      (setq window (display-buffer-in-side-window buffer nil))
      (set-window-parameter window 'no-other-window t)
      (set-window-parameter window 'no-delete-other-windows t)
      (with-current-buffer buffer
        (setq buffer-read-only t)
        (setq window-size-fixed t)
        (let ((inhibit-read-only t)
              (window-size-fixed nil))
          (erase-buffer)
          (insert (resize-window--get-documentation-strings))
          (fit-window-to-buffer window)))
      (setq add-backgrounds t)))
    ;; NOTE: Just in case the help menu was selected (it shouldn't)
    ;; refresh the backgrounds even when the help menu is killed.
    (when add-backgrounds
      (resize-window--add-backgrounds))))

(defun resize-window--split-window-below ()
  "Split the window vertically."
  (resize-window--window-push)
  (split-window-below)
  (resize-window--add-backgrounds))

(defun resize-window--split-window-right ()
  "Split the window horizontally."
  (resize-window--window-push)
  (split-window-right)
  (resize-window--add-backgrounds))

(defun resize-window--delete-window ()
  "Delete the current window."
  (unless (eq (selected-window) (window-main-window))
    (resize-window--window-push)
    (delete-window)
    (resize-window--add-backgrounds)))

(defun resize-window--window-config ()
  "Return the current window configuration.
Exclude the help menu from the configuration."
  (let ((display-menu (get-buffer-window "*Resize-Window-Help*")))
    (resize-window--display-menu 'kill)
    (prog2
        ;; WORKAROUND: Calling `current-buffer' or `get-buffer-window'
        ;; soon after `ivy-switch-buffer' references the old buffer.
        ;; This forces to update to the buffer switched to.  It also
        ;; allows `current-window-configuration' to capture a proper
        ;; configuration updating the values of all current buffers.
        ;; See also https://github.com/abo-abo/swiper/issues/1766
        (let ((curr-window (selected-window)))
          (mapc (lambda (w) (select-window w)) (window-list))
          (select-window curr-window))
        (current-window-configuration)
      (when display-menu
        (resize-window--display-menu 'open)))))

(defun resize-window--restore-config (config)
  "Restore the window configuration CONFIG then return it.
Restore the help menu only if it is currently open."
  (let ((display-menu (get-buffer-window "*Resize-Window-Help*")))
    (set-window-configuration config)
    ;; NOTE: If `resize-window--window-config' was used to save the
    ;; CONFIG there is no help menu to kill. Keep this just in case.
    (resize-window--display-menu 'kill)
    (prog1
        (current-window-configuration)
      (if display-menu
          (resize-window--display-menu 'open)
        (resize-window--add-backgrounds)))))

(defun resize-window--apply-config (config)
  "Return the window configuration CONFIG after applying it.
Return nil if CONFIG isn't a proper window configuration.
Do not change the current window configuration."
  (when (window-configuration-p config)
    (let ((curr-frame (selected-frame))
          (some-frame (window-configuration-frame config))
          (some-config config))
      (when (frame-live-p some-frame)
        (select-frame some-frame)
        (save-excursion
          (save-window-excursion
            (set-window-configuration config)
            (setq some-config (resize-window--window-config))))
        (select-frame curr-frame))
      some-config)))

(defun resize-window--refresh-stack ()
  "Refresh the stack and remove adjacent duplicates.
Each window configuration is restored and saved again.

The configurations saved time is not changed. Always remove the
older configuration when a duplicate is found.

A refresh reveals duplicate configurations. When a configuration
is restored that takes account of the current state of the frame.
Since killed buffers cannot be dug up, applying a state will use
what it finds, and so two configurations may end up the same."
  (let (stack-buffer)
    (dotimes (n (length resize-window--window-stack))
      (let* ((this-member (nth n resize-window--window-stack))
             (this-config (resize-window--apply-config (car this-member)))
             (this-svtime (cdr this-member))
             (prev-config (caar stack-buffer))
             (prev-svtime (cdar stack-buffer)))
        (if (and this-config prev-config
                 (compare-window-configurations this-config prev-config))
            (when (time-less-p prev-svtime this-svtime)
              (setcar stack-buffer this-member))
          (when this-config
            (push this-member stack-buffer)))))
    (setq resize-window--window-stack (nreverse stack-buffer))))

(defun resize-window--window-trim ()
  "Trim the oldest window configurations in excess from the stack.
Return the removed stack members."
  (let* ((size (length resize-window--window-stack))
         (trim (- size resize-window-stack-size)))
    (when (> trim 0)
      (let ((oldest-members
             (sort (copy-sequence resize-window--window-stack)
                   (lambda (a b)
                     (time-less-p (cdr a) (cdr b))))))
        (setq oldest-members
              (nbutlast oldest-members (- size trim)))
        (dotimes (n (length oldest-members))
          (let ((old-member (nth n oldest-members)))
            (setq resize-window--window-stack
                  (delq old-member resize-window--window-stack))))
        oldest-members))))

(defun resize-window--window-push ()
  "Save the current window configuration in the stack.
Return its stack member if the configuration is saved.

Trim adjacent duplicates and old configurations when necessary.

See also `resize-window-stack-size'."
  (let* ((curr-config (resize-window--window-config))
         (curr-member (cons curr-config (current-time)))
         (prev-config nil))
    ;; trim duplicates from the tail
    (while (and (setq prev-config (caar (last resize-window--window-stack)))
                (setq prev-config (resize-window--apply-config prev-config))
                (compare-window-configurations curr-config prev-config)
                (setq resize-window--window-stack
                      (nbutlast resize-window--window-stack))))
    ;; trim duplicates from the head
    (while (and (setq prev-config (caar resize-window--window-stack))
                (setq prev-config (resize-window--apply-config prev-config))
                (compare-window-configurations curr-config prev-config)
                (pop resize-window--window-stack)))
    (push curr-member resize-window--window-stack)
    (resize-window--window-trim)
    (when resize-window--window-stack
      curr-member)))

(defun resize-window--window-pop ()
  "Shift to a previous window configuration.
Return the configuration shifted to if any.

Save the current configuration to the tail of the stack.

Trim adjacent duplicates and old configurations when necessary.

See also `resize-window-stack-size'."
  (resize-window--window-trim)
  (let* ((curr-config (resize-window--window-config))
         (curr-member (cons curr-config (current-time)))
         (prev-config nil))
    ;; trim duplicates from the tail
    (while (and (setq prev-config (caar (last resize-window--window-stack)))
                (setq prev-config (resize-window--apply-config prev-config))
                (compare-window-configurations curr-config prev-config)
                (setq resize-window--window-stack
                      (nbutlast resize-window--window-stack))))
    ;; trim duplicates from the head
    (while (and (setq prev-config (car (pop resize-window--window-stack)))
                (setq prev-config (resize-window--apply-config prev-config))
                (compare-window-configurations curr-config prev-config)))
    (when prev-config
      (setq resize-window--window-stack
            (append resize-window--window-stack (list curr-member)))
      (resize-window--restore-config prev-config)
      prev-config)))

(defun resize-window--kill-other-windows ()
  "Delete other windows."
  (resize-window--window-push)
  (delete-other-windows)
  (resize-window--add-backgrounds))

(defun resize-window--restore-windows ()
  "Restore the previous state."
  (let ((config (resize-window--window-pop)))
    (when config
      (resize-window--restore-config config))))

(defvar resize-window--capital-letters (number-sequence ?A ?Z)
  "List of uppercase letters as characters.")
(defvar resize-window--lower-letters (number-sequence ?a ?z)
  "List of lowercase letters as characters.")

(defun resize-window--key-available? (key)
  "Return non-nil if KEY is bound, otherwise return nil."
  (and (not (resize-window--key-element
             key resize-window-alias-list))
       (not (resize-window--key-element
             key resize-window-dispatch-alist))))

(defun resize-window-add-choice (key func doc &optional allows-capitals)
  "Register a new binding for `resize-window'.
Refuses to replace an already taken key.

KEY is the key (e.g. ?c) that invokes the function FUNC. DOC is a
docstring for the help menu. A non-nil ALLOWS-CAPITALS tells FUNC
accepts capital letters. FUNC should be of zero arity if does not
allow capitals, otherwise to allow capitals should be of optional
single arity so a capital KEY may be passed to FUNC when pressed.

See also `resize-window--key-str'."
  (if (resize-window--key-available? key)
      (push (list key func doc allows-capitals)
            resize-window-dispatch-alist)
    (message "The `%s` key is already taken for resize-window."
             (resize-window--key-str key))))

(provide 'resize-window)
;;; resize-window.el ends here
