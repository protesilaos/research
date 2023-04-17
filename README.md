# Research (research.el) for GNU Emacs

Repeat shell searches and store them in buffers.  ⚠️ WORK-IN-PROGRESS

The goal of `research.el` is to provide a thin wrapper for shell
invocations that search for file names or file contents.  The output
is stored in an Emacs buffer which retains the parameters that
generated it.  This buffer can then repeat the command that created it
by means of `revert-buffer` (bound to `g` by default).  File paths are
automatically buttonised so that they can be used as links to the file
they reference.  RESEARCH provides the means to write the Emacs Lisp
that wraps around the desired shell invocation.

+ Package name (GNU ELPA): `research` ⛔ (not available yet)
+ Official manual: <https://protesilaos.com/emacs/research>
+ Git repo on SourceHut: <https://git.sr.ht/~protesilaos/research>
  - Mirrors:
    + GitHub: <https://github.com/protesilaos/research>
    + GitLab: <https://gitlab.com/protesilaos/research>
+ Mailing list: <https://lists.sr.ht/~protesilaos/general-issues>
+ Backronym: Repeat Ephemeral Searches in Emacs via Access to
  Reproducible Command History.
