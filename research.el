;;; research.el --- Run shell searches and store them reproducibly in buffers -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou General Issues <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/research
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WORK-IN-PROGRESS.
;;
;; The goal of `research.el' ("re-search" is a shorthand for
;; "repeatable search") is to provide a thin wrapper for shell
;; invocations that search for file names or file contents.  The
;; output is stored in an Emacs buffer which retains the parameters
;; that generated it.  This buffer can then repeat the command that
;; created it by means of `revert-buffer' (bound to `g' by default).
;; File paths are automatically buttonised so that they can be used as
;; links to the file they reference.
;;
;; RESEARCH provides the means to write the Emacs Lisp that wraps
;; around the desired shell invocation.  We call the resulting
;; commands and the buffers they generate "re-searchable".
;;
;; Backronym: Repeat Ephemeral Searches in Emacs via Access to
;; Reproducible Command Hubs.

;;; Code:

(defgroup research nil
  "Repeat shell searches and store them in buffers."
  :group 'shell
  :group 'unix)

(defcustom research-hook nil
  "Normal hook that runs at the end of `research'."
  :type 'hook
  :package-version '(research . "0.1.0")
  :group 'research)

;; TODO 2023-04-24: Mention here the command that buttonizes on
;; demand.  See tasks close to the `research-mode'.
(defcustom research-buttonize-absolute-file-paths nil
  "When non-nil render absolute file paths as buttons.
Buttonization is done in buffers whose major mode is
`research-mode'."
  :type 'boolean
  :package-version '(research . "0.1.0")
  :group 'research)

(defcustom research-find-file-command #'find-file-other-window
  "A `find-file' command to act on absolute file path buttons."
  :type '(choice
          (const :tag "find-file" find-file)
          (const :tag "find-file-other-window" find-file-other-window)
          (const :tag "find-file-other-tab" find-file-other-tab)
          (const :tag "find-file-other-frame" find-file-other-frame)
          (function :tag "Custom function"))
  :package-version '(research . "0.1.0")
  :group 'research)

(defcustom research-timestamp-format "%F %T %z"
  "Format of timestamp appended to Research buffers.
The timestamp references the return value of `current-time' at
the moment the process was concluded.

The format must be a set of specifiers as defined in the
documentation of `format-time-string'."
  :type 'string
  :group 'research)

;;;; Core functionality

(defvar research-stdout-buffer "*research*"
  "Buffer name for RESEARCH commands showing their stdout.")

(defconst research-spacing "[\s\f\t\n\r\v]+"
  "Spacing characters that delimit strings.")

(defun research--return-arguments-as-list (arguments)
  "Return ARGUMENTS as a list of strings.
If ARGUMENTS is a list, return it as-is.  If it is a string,
split it at `research-spacing'."
  (cond
   ((listp arguments)
    arguments)
   ((stringp arguments)
    (split-string arguments research-spacing :omit-nulls research-spacing))
   (t
    (error "ARGUMENTS is neither a list nor a string"))))

(defun research--prepare-shell-invocation (arguments)
  "Prepare ARGUMENTS for use in `make-process'.
ARGUMENTS is either a string that represents a shell
invocation (e.g. \"find . type -d\") or a list of strings."
  (let ((args (research--return-arguments-as-list arguments)))
    `(,(car args) ,@(cdr args))))

(defun research-make-process (arguments &optional buffer-name)
  "Define a `make-process' that invokes ARGUMENTS.
ARGUMENTS are used to construct the subprocess.  They are passed
directly to `research--prepare-shell-invocation' and then used as
the :command of `make-process'.

With optional BUFFER-NAME, use it as a component for the
`research-stdout-buffer' once it collects the output and is
subsequently renamed to include BUFFER-NAME and a timestamp."
  (let ((stdout-buffer (get-buffer-create research-stdout-buffer))
        (start-time (research--format-time)))
    ;; FIXME 2023-04-23: Make it asynchronous.
    (make-process
     :name "research"
     :buffer stdout-buffer
     :command (research--prepare-shell-invocation arguments)
     ;; FIXME 2023-04-23: Make the sentinel its own function.
     ;; TODO 2023-04-23: Keep record of start time.
     :sentinel (lambda (process _)
                 (unless (process-live-p process)
                   (when (buffer-live-p stdout-buffer)
                     (with-current-buffer stdout-buffer
                       (goto-char (point-max))
                       (research--insert-timestamp (research--format-time))
                       (research--display-stdout)
                       (research--rename-buffer start-time buffer-name)
                       ;; TODO 2023-04-23: Consider adding a
                       ;; `run-hook-with-args' which the user can set
                       ;; up to, for example, receive a notification
                       ;; that the current buffer no longer receives
                       ;; process output.
                       (research-mode))))))))

(defun research--format-time ()
  "Format TIME using `research-timestamp-format'."
  (format-time-string research-timestamp-format (current-time)))

(defun research--insert-timestamp (time)
  "Insert TIME using `research-timestamp-format'."
  (insert "\nProcess finished at: " time))

(defun research--insert-revert-buffer-function (command)
  "Insert `revert-buffer-function' for COMMAND.
See `research--add-buffer-variables' for how this is used."
  (insert
   "-*- mode: research"
   (format
    "; revert-buffer-function: %S "
    `(lambda (_ignore-auto _noconfirm) ,command))
   "-*-\n\n")
  (setq-local revert-buffer-function
              `(lambda (_ignore-auto _noconfirm)
                 ,command)))

(defun research--add-buffer-variables (command)
  "Store COMMAND in `research-stdout-buffer' local variables.
BUFFER is either an object that satisfies `bufferp' or a buffer
name."
  (if-let ((buffer (get-buffer research-stdout-buffer)))
      (with-current-buffer buffer
        ;; Wait for process to finish before writing anything,
        ;; otherwise what we insert will appear at the end.
        (while (accept-process-output (get-buffer-process buffer)))
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (unless (looking-at "-\\*-")
              (research--insert-revert-buffer-function command)))))
    (error "Cannot find `%s' as a buffer to store parameters" buffer)))

(defun research--clear-buffer ()
  "Delete the contents of BUFFER."
  (when-let* ((buf (get-buffer research-stdout-buffer))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (erase-buffer))))

(defun research--rename-buffer (time &optional buffer-name)
  "Rename `research-stdout-buffer' uniquely with TIME suffix.
Optional BUFFER-NAME has the meaning describe in the
documentation string of `research-make-process'."
  (when-let* ((buf (get-buffer research-stdout-buffer))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (rename-buffer
       (format "%s %s %s" research-stdout-buffer (or buffer-name "") time)
       :unique))))

;;;###autoload
(defun research (arguments &optional buffer-name)
  "Make a subprocess that consists of ARGUMENTS.
ARGUMENTS is either a string or list of strings that represent a
shell invocation.  It may also be a function that returns such
value.  ARGUMENTS is used by `research-make-process'.

To make ARGUMENTS contain non-constant parts, use the present
function like this:

    (let* ((dir (expand-file-name \"~/Git/emacs-sources/emacs.git/\"))
           (default-directory dir))
      (research
       `(\"find\" ,dir \"-iregex\" ,(read-string \"Prompt: \") \"-ls\")))

In the above example, the can provide at the prompt an
Emacs-style regular expression like: .*\.\(el\|c\)

The standard output of the eventual shell invocation is stored in
the buffer `research-stdout-buffer'.

Research buffers store local variables about their state and the
parameters used to produce them.  They can be generated anew
using those variables.

Optional BUFFER-NAME is a string that is used as part of the
buffer that contains the standard output of the aforementioned."
  (research--clear-buffer)
  (research-make-process arguments buffer-name)
  (research--add-buffer-variables `(research ',arguments))
  (run-hooks 'research-hook))

(defun research--display-stdout ()
  "Display the `research-stdout-buffer' if it is available."
  (when-let ((buf (get-buffer research-stdout-buffer)))
    (display-buffer buf)))

;;;; Major mode declaration (buttonize paths)

(defun research-find-file (button)
  "Run `research-find-file-command' on BUTTON text.
BUTTON is assumed to be an absolute file path.  It is buttonized
by the function `research-buttonize-absolute-file-paths'."
  (when-let* ((file (buffer-substring (button-start button) (button-end button)))
              ((file-exists-p file)))
    (funcall research-find-file-command file)))

(define-button-type 'research-file-button
  'follow-link t
  'action #'research-find-file
  'face 'button)

;; FIXME 2023-04-17: This is not a reliable regexp, but we just need
;; something to get started.
(defconst research-absolute-file-path-regexp "[/:.~[:alpha:]]+/\\|@[[:alpha:]][-[:alnum:]]+\\."
  "Regular expression to match absolute file paths.
Variant of `ffap-next-regexp'.")

(defun research-buttonize-absolute-file-paths (&optional force beginning end)
  "Find absolute file paths in the current buffer and buttonize them.
Buttonization is done only if the user option
`research-buttonize-absolute-file-paths' is non-nil.  With
optional non-nil FORCE argument, buttonization is done
regardless.

With optional BEGINNING and END as buffer positions, limit the
effect to the region delineated therein.  Otherwise operate on
the entirety of the buffer.

Buttons call the `research-find-file-command'."
  (when (or force research-buttonize-absolute-file-paths)
    (save-excursion
      (goto-char (or beginning (point-min)))
      (while (re-search-forward research-absolute-file-path-regexp end :no-error 1)
        (when-let (((thing-at-point 'filename))
                   ((not (thing-at-point 'url)))
                   (bounds (bounds-of-thing-at-point 'filename)))
          (make-button (car bounds) (cdr bounds) :type 'research-file-button))))))

(defun research-buttonize-paths (&optional beginning end)
  "Buttonize full paths, subject to confirmation.

With an active region between BEGINNING and END buffer positions,
operate within the boundaries of that area.  Else consider the
entire buffer.

If the entire buffer is the target, prompt for confirmation as
the extraction process may be slow depending on the size of the
buffer."
  (interactive
   (when (region-active-p)
     (list
      (region-beginning)
      (region-end))))
  (when (or (and beginning end)
            (yes-or-no-p "Buttonize full paths (may be slow)?"))
    (research-buttonize-absolute-file-paths :force beginning end)))

(defun research-dired-collect-absolute-file-paths (&optional beginning end)
  "Collect absolute paths and show them in `dired'.
With optional BEGINNING and END as buffer positions, limit the
effect to the region delineated therein.  Otherwise operate on
the entirety of the buffer."
  (let (paths)
    ;; TODO 2023-04-30: Incorporate for `save-restriction'.
    (save-excursion
      (goto-char (or beginning (point-min)))
      (while (re-search-forward research-absolute-file-path-regexp end :no-error 1)
        (when-let (((thing-at-point 'filename))
                   ((not (thing-at-point 'url)))
                   (bounds (bounds-of-thing-at-point 'filename)))
          (push (buffer-substring-no-properties (car bounds) (cdr bounds)) paths)))
      (dired (cons "*research paths*" (delete-dups paths))))))

(defun research-dired-collect-paths (&optional beginning end)
  "Collect absolute paths and show them in `dired'.

With an active region between BEGINNING and END buffer positions,
operate within the boundaries of that area.  Else consider the
entire buffer.

If the entire buffer is the target, prompt for confirmation as
the extraction process may be slow depending on the size of the
buffer."
  (interactive
   (when (region-active-p)
     (list
      (region-beginning)
      (region-end))))
  (when (or (and beginning end)
            (yes-or-no-p "Collect full paths with `dired' (may be slow)?"))
    (research-dired-collect-absolute-file-paths beginning end)))

(defvar research-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "B") #'research-buttonize-paths)
    (define-key map (kbd "D") #'research-dired-collect-paths)
    map)
  "Keymap for `research-mode'.")

;;;###autoload
(define-derived-mode research-mode special-mode "RESEARCH"
  "Major mode for RESEARCH buffers."
  :keymap research-mode-map
  (setq-local buffer-read-only t)
  (research-buttonize-absolute-file-paths))

(provide 'research)
;;; research.el ends here
