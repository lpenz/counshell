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
;; - counshell-rg: interprets ivy input as arguments to ripgrep. It allows you
;; to use ripgrep's arguments to limit the search to a file/directory, a
;; specific file type, etc.
;; - counshell-projectile-*: counshell-sh-* in the project's directory.

;;; Code:

(require 'ivy)
(require 'counsel)
(require 'projectile)

(provide 'counshell)


;; Misc functions

(defun counshell-filepath (filename)
  "Figure out the path of the file by checking for projectile"
  (if (projectile-project-p) (projectile-expand-root filename) filename))

(defun trim-left (str)
  (replace-regexp-in-string "^[ \t\n\r]*" "" str))


;; Regex handling

(defvar counshell--default-regexes
  (list '("\\`\\([^:]+\\):\\([0-9]+\\):" . 2)
        '("\\`\\([^:]+\\):" . 1)
        '("\\`\\(.+\\)$" . 1))
  "Default list of regexes that are matched against the shell output")

(defun counshell--matched-str (str match)
  (substring str (match-beginning match) (match-end match)))

(defun counshell--match-regexes (str &optional regexes)
  (let ((res (or regexes counshell--default-regexes)))
    (cdr (car (cl-member-if (lambda (regex-num)
                              (and (string-match (car regex-num) str)
                                   (file-exists-p
                                    (counshell-filepath
                                     (counshell--matched-str str 1)))))
                            res)))))

;; Format functions

(defun counshell-format-str (str)
  "Format str if format is known"
  (let ((matches (or (counshell--match-regexes str) 0)))
    (when (> matches 0)
      (ivy-add-face-text-property (match-beginning 1) (match-end 1)
                                  'compilation-info
                                  str))
    (when (> matches 1)
      (ivy-add-face-text-property (match-beginning 2) (match-end 2)
                                  'compilation-line-number
                                  str)))
  str)

(defun counshell-format-function (cands)
  "Format candidates if format is known by using counshell-format-str"
  (ivy--format-function-generic
   (lambda (str)
     (ivy--add-face (counshell-format-str str) 'ivy-current-match))
   #'counshell-format-str
   cands
   "\n"))


;; Collection functions

(defun counshell-create-script (scriptfile dir cmdline)
  "Write the commands to execute in the provided scriptfile"
  (when dir (write-region (format "cd %s\n" dir) nil scriptfile nil 0))
  (write-region (format "%s\n" cmdline) nil scriptfile t 0)
  (write-region "echo EOF\n" nil scriptfile t 0)
  scriptfile)

(defun counshell-function (projectile scriptfile prefix str)
  "Run prefix+str using the shell if str size is > 2"
  (let ((dir (if (and projectile (projectile-project-p)) (projectile-project-root) nil)))
    (if (< (length str) 2)
        (counsel-more-chars 2)
      (progn
        (counshell-create-script scriptfile dir (format "%s %s" prefix str))
        (counsel--async-command
         (format "bash %s </dev/null | cat" scriptfile)))
      '("" "working..."))))


;; Action functions - return nil if no action taken

(defun counshell-action-file (filename)
  "Open filename if it is an existing file"
  (let ((filepath (counshell-filepath filename)))
    (when (file-exists-p filepath)
      (with-ivy-window
        (find-file filepath)))))

(defun counshell-action-file-linenum (filename linenum)
  "Open filename and go to linenum if filename is an existing file"
  (let ((filepath (counshell-filepath filename)))
    (when (file-exists-p filepath)
      (with-ivy-window
        (progn
          (find-file filepath)
          (goto-char (point-min))
          (forward-line (- linenum 1)))))))


;; Main function

(defun counshell-sh-read (projectile prefix initial)
  "Invoke a subprocess through the shell"
  (let ((scriptfile (make-temp-file "counshell-command.sh."))
        (ivy-format-function #'counshell-format-function))
    (ivy-read (format "$ %s" prefix)
              (lambda (str) (counshell-function projectile scriptfile prefix str))
              :initial-input initial
              :dynamic-collection t
              :history 'counshell-history
              :action (lambda (str)
                        (when str
                          (cond
                           ((counshell-action-file str) ())
                           ((counshell-action-file (trim-left str)) ())
                           ((counshell-action-file-linenum (replace-regexp-in-string ":.*$" "" str)
                                                           (string-to-number (replace-regexp-in-string "^[^:]+:\\([0-9]+\\):.*" "\\1" str))) ())
                           ((counshell-action-file (replace-regexp-in-string ":.*$" "" str)) ())
                           (t (message (format "File not found or unable to parse [%s]" str))))))
              :unwind (lambda () (progn (delete-file scriptfile)
                                        (counsel-delete-process)))
              :caller 'counshell)))

;; API

;;;###autoload
(defun counshell-projectile-sh ()
  "Invoke a subprocess through the shell"
  (interactive)
  (counshell-sh-read t "" ""))

;;;###autoload
(defun counshell-sh ()
  "Invoke a subprocess through the shell"
  (interactive)
  (counshell-sh-read nil "" ""))

;;;###autoload
(defun counshell-projectile-gnuglobal ()
  "Invoke GNU global in a subshell"
  (interactive)
  (counshell-sh-read t "global --result=grep " (ivy-thing-at-point)))

;;;###autoload
(defun counshell-projectile-rg ()
  "Invoke rg in a subshell"
  (interactive)
  (counshell-sh-read t "rg -n " ""))

;;; counshell.el ends here
