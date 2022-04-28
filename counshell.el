;;; counshell.el --- Ivy interface for grep-like shell commands -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Leandro Lisboa Penz

;; Author: Leandro Lisboa Penz <lpenz@lpenz.org>
;; Keywords: lisp, bash
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (swiper "0.10.0") (counsel "0.10.0") (projectile "0.14.0"))
;; URL: https://github.com/lpenz/counshell

;; This file is subject to the terms and conditions defined in
;; file 'LICENSE', which is part of this source code package.

;;; Commentary:

;; counshell provides an ivy interface to the shell through the following
;; functions:
;; - counshell-sh: starts ivy, runs your input as a bash command and interprets
;; the output as file:line:msg (grep-like).
;; - counshell-gnuglobal: interprets ivy input as arguments to GNU global,
;; effectively prefixing the input with "global --results=grep ".
;; - counshell-rg: interprets ivy input as arguments to ripgrep.  It allows you
;; to use ripgrep's arguments to limit the search to a file/directory, a
;; specific file type, etc.
;; - counshell-projectile-*: counshell-sh-* in the project's directory.

;;; Code:

(require 'ivy)
(require 'counsel)
(require 'projectile)

(provide 'counshell)


;; Options

(defgroup counshell nil
  "Ivy interface for grep-like shell commands"
  :group 'external)

(defcustom counshell-wait-for-space nil
  "Only run the shell command when a space is the last character in the input"
  :type 'boolean
  :group 'counshell)

;; Misc functions

(defun counshell--filepath (filename)
  "Figure out the path of the FILENAME by checking for projectile.
Returns nil if FILENAME doesn't exist."
  (let ((f (if (projectile-project-p)
               (projectile-expand-root filename)
             filename)))
    (if (file-exists-p f)
        f
      nil)))


;; Regex handling

(defvar counshell--default-regexes
  (list '("\\`\\([^:]+\\):\\([0-9]+\\):" . 2)
        '("\\`\\([^:]+\\):" . 1)
        '("\\`\\(.+\\)$" . 1))
  "Default list of regexes that are matched against the shell output.")

(defun counshell--match-regexes (str &optional regexes)
  "Try to match STR against each of the REGEXES in turn.
If the regex has 2 elements, return a list with file name and line number;
if the regex has 1 element, return a list with the file name only;
if there is no match, return nil."
  (let* ((res (or regexes counshell--default-regexes))
         (re-num (cl-member-if (lambda (regex-num)
                                 (and (string-match (car regex-num) str)
                                      (counshell--filepath (match-string 1 str))))
                               res))
         (num (cdr (car re-num))))
    (when num
      (string-match (car (car re-num)) str) ;; match again because of projectile
      (cond
       ((= num 2) (list
                   (match-string 1 str)
                   (string-to-number (match-string 2 str))))
       ((= num 1) (list
                   (match-string 1 str)))))))


;; Format functions

(defun counshell--format-str (str)
  "Format STR if format is known."
  (if (string= str "EOF")
      (ivy--add-face str 'file-name-shadow)
    (let ((matches (or (length (counshell--match-regexes str)) 0)))
      (when (> matches 0)
        (add-face-text-property
         (match-beginning 1) (match-end 1)
         'compilation-info
         nil str))
      (when (> matches 1)
        (add-face-text-property
         (match-beginning 2) (match-end 2)
         'compilation-line-number
         nil str)))
    str))

(defun counshell--format-str-current (str)
  "Format STR when it is the current selection."
  (ivy--add-face (counshell--format-str str) 'ivy-current-match))

(defun counshell--format (cands)
  "Format CANDS if format is known by using ‘counshell--format-str’."
  (ivy--format-function-generic
   #'counshell--format-str-current
   #'counshell--format-str
   cands
   "\n"))


;; Collection functions

(defun counshell--create-script (scriptfile dir cmdline)
  "Write to SCRIPTFILE the command to change the directory to DIR, followed by
CMDLINE."
  (write-region "" nil scriptfile nil 0)
  (when dir (write-region (format "cd %s\n" dir) nil scriptfile t 0))
  (write-region (format "%s\n" cmdline) nil scriptfile t 0)
  (write-region "echo EOF\n" nil scriptfile t 0)
  scriptfile)

(defun counshell--function (projectile scriptfile prefix str)
  "If PROJECTILE, go to project directory before writting SCRIPTFILE with PREFIX
STR and running it using the shell - otherwise, keep dir unchanged.  If str size
is <=2, do nothing."
  (let ((dir (if (and projectile (projectile-project-p))
                 (projectile-project-root)
               nil)))
    (if (or (< (length str) 2)
            (and
             counshell-wait-for-space
             (not (string= " " (substring str -1 nil)))))
    ;; (if (< (length str) 2)
        (ivy-more-chars)
      (progn
        (counshell--create-script scriptfile dir (format "%s %s" prefix str))
        (counsel--async-command (format "bash %s </dev/null | cat" scriptfile))
        '("" "working...")))))

(defun counshell--function-wrapper (projectile scriptfile prefix)
  "Return ‘counshell--function’ as a closure with pre-defined arguments
PROJECTILE, SCRIPTFILE and PREFIX."
  `(lambda (str)
     (counshell--function ,projectile ,scriptfile ,prefix str)))

;; Action functions - return nil if no action taken

(defun counshell--action (str)
  "Open file in line number if STR format is recognized."
  (when str
    (let* ((matches (counshell--match-regexes str))
           (num (length matches)))
      (if matches
          (with-ivy-window
            (find-file (counshell--filepath (car matches)))
            (when (> num 1)
              (let ((linenum (nth 1 matches)))
                (goto-char (point-min))
                (forward-line (- linenum 1)))))
        (message (format "File not found or unable to parse [%s]" str))))))

;; Main function

(defun counshell--unwind (scriptfile)
  "Create a closure that removes SCRIPTFILE and kills counshell process."
  `(lambda () (progn (delete-file ,scriptfile)
                     (counsel-delete-process))))

(defun counshell-sh-read (&optional projectile prefix initial)
  "Invoke a subprocess through the shell.
If PROJECTILE, run command in project directory.
PREFIX is pre-pended to the ‘command-line’; allows pre-defined commands.
INITIAL is used to pre-populate the ‘command-line’."
  (let ((scriptfile (make-temp-file "counshell-command.sh.")))
    (ivy-read (format "$ %s" (or prefix ""))
              (counshell--function-wrapper projectile scriptfile (or prefix ""))
              :initial-input (or initial "")
              :dynamic-collection t
              :history 'counshell-history
              :action #'counshell--action
              :unwind (counshell--unwind scriptfile)
              :caller 'counshell)))

(add-to-list 'ivy-format-functions-alist '(counshell . counshell--format))

;; API

;;;###autoload
(defun counshell-sh ()
  "Invoke a subprocess through the shell."
  (interactive)
  (counshell-sh-read))

;;;###autoload
(defun counshell-projectile-sh ()
  "Invoke a subprocess through the shell in the project directory."
  (interactive)
  (counshell-sh-read t))

;;;###autoload
(defun counshell-ls ()
  "Invoke ls in a subshell."
  (interactive)
  (counshell-sh-read nil "ls "))

;;;###autoload
(defun counshell-projectile-ls ()
  "Invoke ls in a subshell in the project directory."
  (interactive)
  (counshell-sh-read t "ls "))

;;;###autoload
(defun counshell-grep ()
  "Invoke grep in a subshell."
  (interactive)
  (counshell-sh-read nil "grep -n "))

;;;###autoload
(defun counshell-projectile-grep ()
  "Invoke grep in a subshell in the project directory."
  (interactive)
  (counshell-sh-read t "grep -n "))

;;;###autoload
(defun counshell-find ()
  "Invoke find in a subshell."
  (interactive)
  (counshell-sh-read nil "find "))

;;;###autoload
(defun counshell-projectile-find ()
  "Invoke find in a subshell in the project directory."
  (interactive)
  (counshell-sh-read t "find "))

;;;###autoload
(defun counshell-find-grep ()
  "Invoke find for files piped to grep in a subshell."
  (interactive)
  (counshell-sh-read nil "find . -type f | grep "))

;;;###autoload
(defun counshell-projectile-find-grep ()
  "Invoke find for files piped to grep in a subshell in the project directory."
  (interactive)
  (counshell-sh-read t "find . -type f | grep "))

;;;###autoload
(defun counshell-rg ()
  "Invoke rg in a subshell."
  (interactive)
  (counshell-sh-read nil "rg -n "))

;;;###autoload
(defun counshell-projectile-rg ()
  "Invoke rg in a subshell."
  (interactive)
  (counshell-sh-read t "rg -n "))

;;;###autoload
(defun counshell-fd ()
  "Invoke fd in a subshell."
  (interactive)
  (counshell-sh-read nil "fd "))

;;;###autoload
(defun counshell-projectile-fd ()
  "Invoke fd in a subshell."
  (interactive)
  (counshell-sh-read t "fd "))

;;;###autoload
(defun counshell-gnuglobal ()
  "Invoke GNU global in a subshell."
  (interactive)
  (counshell-sh-read nil "global --result=grep " (ivy-thing-at-point)))

;;;###autoload
(defun counshell-projectile-gnuglobal ()
  "Invoke GNU global in a subshell in the project directory."
  (interactive)
  (counshell-sh-read t "global --result=grep " (ivy-thing-at-point)))

;;;###autoload
(defun counshell-idutils-gid ()
  "Invoke idutils' gid in a subshell."
  (interactive)
  (counshell-sh-read nil "gid " (ivy-thing-at-point)))

;;;###autoload
(defun counshell-projectile-idutils-gid ()
  "Invoke idutils' gid in a subshell in the project directory."
  (interactive)
  (counshell-sh-read t "gid " (ivy-thing-at-point)))

;;; counshell.el ends here
