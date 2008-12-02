;;;; Upstream is a set of command-line tools to fetch CVS modules from various
;;;; repositories without needing to remember magic incantations.
;;;;
;;;; Upstream is written by Nikodemus Siivola <nikodemus@random-state.net>, with
;;;; additional contributions from:
;;;;
;;;;    - Martin Kuehl <martin.kuehl@gmail.com>: Gna! support
;;;;
;;;; Upstream has been placed in Public Domain by the author, and comes with no warranty.
;;;;
;;;; This file contains the Common Lisp source from which the command-line tools are
;;;; generated. Though this should be fully portable, it has been tested on SBCL only.

(defpackage :upstream
  (:use :cl)
  (:export "BUILD-ALL"))

(in-package :upstream)

;;;; Output

(defvar *output* *standard-output*)
(defvar *indent* 0)

(defmacro with-indentation ((&optional (n 2)) &body forms)
  `(let ((*indent* (+ ,n *indent*)))
     ,@forms))

(defmacro with-inline-output (form)
  `(let ((*indent* -1))
     ,form))

(defun out (control &rest arguments)
  (cond ((plusp *indent*)
         (format *output* "~VT~?~%" *indent* control arguments))
        ((zerop *indent*)
         (format *output* "~?~%" control arguments))
        (t
         (format nil "~?" control arguments))))

;;;; Functions wrapping programs and shell commands

(defun echo (&optional control &rest arguments)
  (out "echo~@[ \"~?\"~]" control arguments))

(defun exit (&optional (code 0))
  (out "exit ~A" code))

(defun test (type target)
  (out "test -~A ~S" (string-downcase type) target))

(defun checkout (user directory)
  ;; FIXME: This is a bad abstraction, but figuring out how to wrap CVS
  ;; nicely was more bother then I wanted.
  (out "cvs -z3 -d ~A@~A co -d $directory $module" user directory))

;;;; Symbols to strings for shell
;;;;
;;;; We do not convert - to _, as we whish to pun normal shell
;;;; string expansion, and keep the variable names constant.

(defun sh-name (name)
  ;; No EVAL-WHEN needed as we don't bother to compile this
  (if (integerp name)
      name
      (string-downcase name)))

;;;; Macros implementing shell operations
;;;;
;;;; Plenty of names colliding with lisp symbols here. Prefixed with SH-, with
;;;; the exception of SET!.

(defvar *sh-functions*)

(defmacro sh-function (name &body forms)
  (push name *sh-functions*)
  `(progn
     (out "~A() {" ,(sh-name name))
     (with-indentation ()
       ,@forms)
     (out "}")
     ',name))

(defmacro $ (name)
  `(format nil "$~A" ,(sh-name name)))

(defmacro $- (name default)
  `(format nil "${~A:-~A}" ,(sh-name name) ,default))

(defmacro set! (&rest things)
  (assert (evenp (length things)))
  `(progn
     ,@(loop
          for name = (pop things)
          for value = (pop things)
          while name
          collect `(out "~A=~S" ,(sh-name name) ,value))))

(defmacro sh-if (test &body forms)
  `(progn
     (out "if ~A" (with-inline-output ,test))
     ,@(mapcar (lambda (form)
                 (ecase (car form)
                   (then
                    `(progn
                       (out "then")
                       (with-indentation ()
                         ,@(cdr form))))
                   (else
                    `(progn
                       (out "else")
                       (with-indentation ()
                         ,@(cdr form))))))
               forms)
     (out "fi")))

(defmacro sh-case (thing &body clauses)
  `(progn
     (out "case ~A in" ,thing)
     (with-indentation ()
       ,@(mapcar (lambda (clause)
                   (destructuring-bind (pattern &body forms) clause
                     `(progn
                        ,(if (stringp pattern)
                             `(out "\"~A\")" ,pattern)
                             `(out "~A)" ',pattern))
                        (with-indentation ()
                          ,@forms
                          (out ";;")))))
                 clauses))
     (out "esac")
     nil))

;;;; The head honcho: DEFINE-PROGRAM defines a shell-program as a template that
;;;; can be instantiated in various ways -- like we do for UPSTREAM.

(defmacro define-program (name lambda-list &body forms)
  (let* ((*sh-functions* nil)
         (expanded (mapcar #'macroexpand forms)))
    `(defun ,name (pathname ,@lambda-list)
       (let ((name (file-namestring pathname)))
         (with-open-file (*output* pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :external-format :ascii)
           (out "#!/bin/sh")
           (out "# MACHINE GENERATED SOURCE: DO NOT EDIT BY HAND!")
           (out "# build-host: ~A ~A"
                (lisp-implementation-type) (lisp-implementation-version))
           (flet ,(mapcar
                   (lambda (name)
                     `(,name (&rest arguments) (out "~A~{ ~S~}" ,(sh-name name) arguments)))
                   *sh-functions*)
             ,@expanded))))))

;;;; The UPSTREAM program definition

(defvar *upstream-version* nil)

(defun upstream-version ()
  (or *upstream-version* "<experimental version>"))

(define-program upstream (repository anon-source developer-source checkout-directory)

  (out "# In Public Domain")
  (when *upstream-version*
    (out "# Original available as: http://random-state.net/files/upstream-~A.tar.gz"
         (upstream-version)))
           
  (set! project_or_option ($ 1)
        module ($- 2 ($ project_or_option))
        directory ($- 3 ($ module))
        upstream_user ($ 4))
  
  (sh-function version
    (echo "~A (Upstream ~A)" name (upstream-version))
    (echo "~A is in Public Domain." name))

  (sh-function usage
    (echo "usage: ~A [--help | --version] project [module] [directory] [username]" name))

  (sh-function help
    (usage)
    (echo)
    (echo "Check out a ~A project from developer or anonymous CVS." repository)
    (echo)
    (echo "Options:")
    (echo "  --help      Prints this message and exits.")
    (echo "  --version   Prints version string and exits.")
    (echo)
    (echo "Arguments:")
    (echo "  project     Name of the project to check out.")
    (echo "  module      Name of the CVS module the check out. Defaults to project.")
    (echo "  directory   Local directory to check out to. Defaults to module.")
    (echo "  username    ~A username to use for developer checkout." repository)
    (echo)
    (echo "Uses the developer CVS if username is given, otherwise uses anonymous CVS.")
    (echo "Aborts if a file or directory with the same name as the local checkout")
    (echo "directory already exists."))

  (sh-case ($ project_or_option)
    (""
     (usage)
     (exit 1))
    ("--help"
     (help)
     (exit 0))
    ("--version"
     (version)
     (exit 0))
    (-*
     (usage)
     (exit 1))
    (*
     (set! project ($ project_or_option))))
  
  (sh-if (test :e ($ directory))
         (then
          (echo "File or directory exists already: $directory")
          (exit 1)))

  (sh-function run
    (set! what "$project/$module -> $directory")
    (echo "Checking out $what $2")
    (sh-if (checkout ($ 1) checkout-directory)
           (then
            (echo "$what $2 checked out"))
           (else
            (echo "$what $2 CHECKOUT FAILED"))))

  (sh-if (test :n ($ upstream_user))
         (then
          (run developer-source (format nil "(from ~A developer cvs)" repository)))
         (else
          (run anon-source (format nil "(from ~A anonymous cvs)" repository)))))

;;;; Generate the shell-scripts


(defun build-all (version)
  (let ((*upstream-version* version))
    (upstream "clnet" "Common-lisp.net"
              ":pserver:anonymous:anonymous" ":ext:$upstream_user"
              "common-lisp.net:/project/$project/cvsroot")
    (upstream "gna" "Gna!"
              ":pserver:anonymous" ":ext$upstream_user"
              "cvs.gna.org:/cvs/$project")
    (upstream "savannah" "Savannah"
              ":pserver:anonymous" "$upstream_user"
              "cvs.sv.gnu.org:/sources/$project")
    (upstream "sfnet" "SourceForge"
              ":pserver:anonymous" ":ext:$upstream_user"
              "$project.cvs.sourceforge.net:/cvsroot/$project")))
