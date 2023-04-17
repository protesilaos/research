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
;; The goal of `research.el' is to provide a thin wrapper for shell
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
;; Reproducible Command History.

;;; Code:

(defgroup research nil
  "Repeat shell searches and store them in buffers."
  :group 'shell
  :group 'unix)

(defcustom research-buttonize-absolute-file-paths t
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

;;;; Core functionality

(defvar research-buffer-stdout "*research*"
  "Buffer name for RESEARCH commands showing their stdout.")

(defvar research-buffer-stderr "*research-errors*"
  "Buffer name for RESEARCH commands showing their stderr.")

;; FIXME 2023-04-17: This is not a reliable regexp, but we just need
;; something to get started.
(defconst research-absolute-file-path-regexp "^.*?\\(/.*/?\\).*?$"
  "Regular expression to match absolute file paths.")

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
  "Prepare shell invocation that consists of ARGUMENTS.
ARGUMENTS is either a string that represents a shell
invocation (e.g. \"find . type -d\") or a list of strings."
  (let ((args (research--return-arguments-as-list arguments)))
    `(,(car args) ,@(cdr args))))

(defun research-make-process (arguments)
  "Define a `make-process' that invokes a shell with ARGUMENTS.
ARGUMENTS are used to construct the shell invocation.  They are
passed directly to `research--prepare-shell-invocation'."
  (make-process
   :name "research"
   :buffer (get-buffer-create research-buffer-stdout)
   :command (research--prepare-shell-invocation arguments)
   :stderr (get-buffer-create research-buffer-stderr)))

(defmacro research-create-command (name arguments &optional doc)
  "Create command with NAME and DOC that invokes ARGUMENTS.

NAME is an interactive function that accepts no arguments.  DOC
is its documentation string, which is optional.

ARGUMENTS is either a string or list of strings that represent a
shell invocation.  It may also be a function that returns such
value.  The value is used by `research-make-process'.

To make ARGUMENTS contain non-constant parts, use the present
macro like this:

    (research-create-command
     test-find
     `(\"find\" \".\" \"-type\" \"f\" \"-iname\" ,(read-string \"A test prompt: \")))

The standard output of the eventual shell invocation is stored in
the buffer `research-buffer-stdout', while errors go to
`research-buffer-stderr'.

Research buffers store local variables about their state and the
parameters used to produce them.  They can be generated anew
using those variables.  WORK-IN-PROGRESS: this is not done yet."
  `(defun ,name ()
     ,doc
     (interactive)
     (research-make-process ,arguments)))

;;;; Major mode declaration (buttonize paths)

(defun research-find-file (button)
  "Run `research-find-file-command' on BUTTON text.
BUTTON is assumed to be an absolute file path.  It was buttonized
by `research-buttonize-absolute-file-paths'."
  (funcall
   research-find-file-command
   (buffer-substring (button-start button) (button-end button))))

(define-button-type 'research-file-button
  'follow-link t
  'action #'research-find-file
  'face 'button)

(defun research-buttonize-absolute-file-paths ()
  "Find absolute file paths in the current buffer and buttonize them."
  (when research-buttonize-absolute-file-paths
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward research-absolute-file-path-regexp nil :no-error)
        (make-button (match-beginning 1) (match-end 1) :type 'research-file-button)))))

;; TODO 2023-04-17: Define the procedure of adding variables to the
;; buffer.

;; TODO 2023-04-17: Substantiate the major-mode.

;;;###autoload
(define-derived-mode research-mode special-mode "RESEARCH"
  "Major mode for RESEARCH buffers."
  (research-buttonize-absolute-file-paths))

(provide 'research)
;;; research.el ends here