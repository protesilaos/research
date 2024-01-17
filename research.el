;;; research.el --- Run shell searches and store them reproducibly in buffers -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/research
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

(require 'ansi-color) ; NOTE 2023-05-28: Experimental

(defgroup research nil
  "Repeat shell searches and store them in buffers."
  :group 'shell
  :group 'unix)

(defcustom research-directory (expand-file-name "~/Documents/research-buffers")
  "Default directory for storing research buffers.
This is the default value read by the command `write-file' when
it prompts for a file name."
  :type 'directory
  :package-version '(research . "0.1.0")
  :group 'research)

(defcustom research-buffer-description-function #'research-buffer-description-as-hash
  "Function to provide a description for research buffers.

Research buffers are named according to this pattern:

    *research: COMMAND DESCRIPTION*

The DESCRIPTION part is affected by this user option.  The rest
are constant.

The function assigned to this user option must read a list of
strings and return a single string.  The list of strings
corresponds to the ARGUMENTS given to the `research' function.

The `research-buffer-description-as-hash' function returns the
list of strings it receives as a hash.

The `research-buffer-description-as-string' takes the list of
strings and returns its `cdr' as a single string.  It omits the
`car' as that is already part of the buffer's name."
  :package-version '(research . "0.1.0")
  :type '(choice
          (function :tag "Show list of arguments as a string" #'research-buffer-description-as-string)
          (function :tag "Show list of arguments as a hash" #'research-buffer-description-as-hash)
          (function :tag "Custom description function"))
  :group 'research)

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

(defcustom research-process-time-identifier "%H%M%S%3N"
  "Format of time that uniquifies a research process object.
A research process is named research-TIME where time is the value
of `format-time-string' with the specifiers provided as the value
of this variable.

Depending on one's requirements, a high degree of precision is
mandatory to avoid conflicts between multiple processes."
  :type 'string
  :group 'research)

;;;; Core functionality

(defconst research-spacing "[\s\f\t\n\r\v]+"
  "Spacing characters that delimit strings.")

(defun research-return-arguments-as-list (arguments)
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
  (let ((args (research-return-arguments-as-list arguments)))
    `(,(car args) ,@(cdr args))))

(defun research--make-process-name-unique ()
  "Return a unique name for `research-make-process'."
  (concat "research-" (format-time-string research-process-time-identifier)))

(defun research--convert-list-to-string (list)
  "Convert LIST to a string.
Separate each element of LIST by a space."
  (mapconcat #'identity list " "))

(defun research-buffer-description-as-string (list)
  "Return the concatenated LIST of strings.

Omit the `car' of LIST because it is the name of the command
which already forms part of the buffer's name, per the function
`research-get-or-create-buffer'.

This function can be used as the value of the user option
`research-buffer-description-function'."
  (research--convert-list-to-string (cdr list)))

(defun research-buffer-description-as-hash (list)
  "Return the concatenated LIST of strings as a hash.
This function can be used as the value of the user option
`research-buffer-description-function'."
  (secure-hash 'sha1 (research--convert-list-to-string list)))

(defun research--normalize-buffer-description-function ()
  "Return `research-buffer-description-function' if appropriate.
Else return `research-buffer-description-as-hash'."
  (if (and research-buffer-description-function
           (functionp research-buffer-description-function))
      research-buffer-description-function
    #'research-buffer-description-as-hash))

(defun research--buffer-description (list)
  "Return a description by parsing the LIST of strings.
This description is used to expand the name of the research
buffer.  The function must return a string."
  (funcall (research--normalize-buffer-description-function) list))

(defun research-get-or-create-buffer (name description)
  "Get buffer with NAME and DESCRIPTION for `research-make-process'.
DESCRIPTION is ultimately subject to the user option
`research-buffer-description-function'."
  (get-buffer-create
   (format "*research: %s %s*" name description)))

(defun research--erase-buffer-contents (buffer)
  "Erase the contents of BUFFER.
Do this before the call to `make-process' (per `research-make-process')."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun research-make-process (arguments &optional buffer-name)
  "Define a `make-process' that invokes ARGUMENTS.

The process is asynchronous and runs the command consisting of
ARGUMENTS.  The `car' of ARGUMENTS is the name of the program,
while the `cdr' are its arguments as a list of strings.

ARGUMENTS are used to construct the shell command, which is a
subprocess of Emacs.  See `research--prepare-shell-invocation'
for how ARGUMENTS are prepared.

With optional BUFFER-NAME, use it as the process standard output
buffer.  BUFFER is an object that satisfies `bufferp', such as
the return value of `research-get-or-create-buffer'.

BUFFER is used to perform the following:

1. Store the output of the subprocess running ARGUMENTS.
2. Capture the start and end timestamps of the subprocess.
3. Define a `revert-buffer' value that makes the subprocess with
   ARGUMENTS capable of repeating itself.
4. Store the value of the aforementioned `revert-buffer' in the
   first line of the buffer (i.e. the mode line), which means
   that `write-file' will preserve the BUFFER's repeatable
   subprocess."
  (let* ((args (research--prepare-shell-invocation arguments))
         (stdout-buffer (research-get-or-create-buffer
                         (or buffer-name (car args))
                         (research--buffer-description args)))
         (start-time (research--format-time)))
    (research--erase-buffer-contents stdout-buffer)
    (display-buffer stdout-buffer)
    (make-process
     :name (research--make-process-name-unique)
     :buffer stdout-buffer
     :command args
     :filter
     (lambda (process string)
       (when (buffer-live-p (process-buffer process))
         (with-current-buffer (process-buffer process)
           (setq mode-line-process
                 (propertize " Running research..."
                             'face 'mode-line-emphasis))
           (let ((inhibit-read-only t))
             (save-excursion
               ;; Insert the text, advancing the process marker.
               (goto-char (process-mark process))
               (insert (replace-regexp-in-string "" "\n" string))
               (ansi-color-apply-on-region (point-min) (point))
               (set-marker (process-mark process) (point)))))))
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (when (buffer-live-p stdout-buffer)
           (with-current-buffer stdout-buffer
             (setq mode-line-process nil)
             (let ((inhibit-read-only t))
               (research--add-buffer-variables `(research ',arguments))
               (goto-char (point-max))
               (research--insert-timestamp "started" start-time)
               (research--insert-timestamp "finished" (research--format-time)))
             (research-mode))))))))

(defun research--format-time ()
  "Format TIME using `research-timestamp-format'."
  (format-time-string research-timestamp-format))

(defun research--insert-timestamp (string time)
  "Insert STRING describing TIME using `research-timestamp-format'."
  (insert (format "\nProcess %s at: %s" string time)))

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
  "Store COMMAND as a local variable."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at "-\\*-")
        (research--insert-revert-buffer-function command)))))

;;;###autoload
(defun research (arguments &optional buffer-name)
  "Make a subprocess that consists of ARGUMENTS.
ARGUMENTS is either a string or list of strings that represent a
shell invocation.  It may also be a function that returns such
value.  ARGUMENTS is used by `research-make-process'.
Internally, ARGUMENTS is always converted to a list of strings,
per `research-return-arguments-as-list'.

Example:

    (research
     '(\"find\" \"/home/prot/Git/emacs.git/\" \"-iregex\" \".*\\.el\" \"-ls\"))

To make ARGUMENTS contain non-constant parts, use the present
function like this:

    (let* ((dir (expand-file-name \"~/Git/emacs-sources/emacs.git/\"))
           (default-directory dir))
      (research
       `(\"find\" ,dir \"-iregex\" ,(read-string \"Prompt: \") \"-ls\")))

In the above example, the can provide at the prompt an
Emacs-style regular expression like: .*\.\(el\|c\)

The standard output of the eventual shell invocation is stored in
a buffer named after research, the name of the shell command (the
`car' of ARGUMENTS), and whatever the description is per the user
option `research-buffer-description-function'.

Research buffers store local variables about their state and the
parameters used to produce them.  They can be generated anew
using those variables.

Optional BUFFER-NAME is a string that is used as part of the
buffer that contains the standard output of the aforementioned."
  (research-make-process arguments buffer-name)
  (run-hooks 'research-hook))

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

(defun research--make-directory ()
  "Make the variable `research-directory' and its parents, if needed."
  (when (and (stringp research-directory)
             (not (file-directory-p research-directory)))
    (make-directory research-directory :parents)))

(defun research--directory ()
  "Normalize variable `research-directory'."
  (let ((path (or (research--make-directory) research-directory)))
    (file-name-as-directory (expand-file-name path))))

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
  (setq-local default-directory (research--directory))
  (setq-local buffer-read-only t)
  (research-buttonize-absolute-file-paths))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.research\\'" . research-mode))

(provide 'research)
;;; research.el ends here
