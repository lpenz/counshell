;;; counshell-test.el --- Tests for counshell -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Leandro Lisboa Penz

;; Author: Leandro Lisboa Penz <lpenz@lpenz.org>

;; This file is subject to the terms and conditions defined in
;; file 'LICENSE', which is part of this source code package.

(require 'undercover)
(undercover "*.el" (:report-type :codecov))

(require 'el-mock)

(add-to-list 'load-path (f-parent (f-dirname load-file-name)))

(require 'counshell)


;; Test counshell--filepath

(ert-deftest counshell--filepath-noproject ()
  (with-mock
   (stub projectile-project-p => nil)
   (should (equal (counshell--filepath "counshell.el") "counshell.el"))))

(ert-deftest counshell--filepath-inproject ()
  (with-mock
   (stub projectile-project-p => t)
   (stub projectile-expand-root => "counshell.el")
   (should (equal (counshell--filepath "counshell.el") "counshell.el"))))

(ert-deftest counshell--filepath-nofile ()
  (with-mock
   (stub projectile-project-p => nil)
   (should (equal (counshell--filepath "nofile") nil))))

(ert-deftest counshell--create-script-dir ()
  (let ((scriptfile (make-temp-file "counshell-test.sh.")))
    (should (equal t (file-readable-p (counshell--create-script scriptfile "/tmp" "ls /"))))
    (let ((size (nth 7 (file-attributes scriptfile))))
      (should (equal t (file-readable-p (counshell--create-script scriptfile "/tmp" "ls /"))))
      (should (equal size (nth 7 (file-attributes scriptfile)))))
    (delete-file scriptfile)))

(ert-deftest counshell--create-script-nodir ()
  "Also check that the script is not appended to when created multiple times"
  (let ((scriptfile (make-temp-file "counshell-test.sh.")))
    (should (equal t (file-readable-p (counshell--create-script scriptfile nil "ls /"))))
    (let ((size (nth 7 (file-attributes scriptfile))))
      (should (equal t (file-readable-p (counshell--create-script scriptfile nil "ls /"))))
      (should (equal size (nth 7 (file-attributes scriptfile)))))
    (delete-file scriptfile)))


;; Test top-level functions

(ert-deftest counshell-top-projectile ()
  (with-mock
   (mock (counshell-sh-read t * *))
   (counshell-projectile-sh)
   (counshell-projectile-gnuglobal)
   (counshell-projectile-rg)))

(ert-deftest counshell-top-noprojectile ()
  (with-mock
   (mock (counshell-sh-read nil * *))
   (counshell-sh)))


;; Test regex functions

(ert-deftest counshell-regexes ()
  (should (equal
           (counshell--match-regexes "counshell.el:5: ok")
           (list "counshell.el" 5)))
  (should (equal
           (counshell--match-regexes "counshell.el: ok")
           (list "counshell.el")))
  (should (equal
           (counshell--match-regexes "counshell.el")
           (list "counshell.el")))
  (should (equal
           (counshell--match-regexes "zxcv:5: no file")
           nil)))


;; Test format functions

(ert-deftest counshell--format-str-eof ()
  (with-mock
   (mock (ivy--add-face "EOF" 'file-name-shadow) => nil)
   (should (equal (counshell--format-str "EOF") nil))))

(ert-deftest counshell--format-str-fileline ()
  (with-mock
   (mock (ivy-add-face-text-property * * * "counshell.el:5: ok") => nil)
   (should (equal
            (counshell--format-str "counshell.el:5: ok")
            "counshell.el:5: ok"))))

(ert-deftest counshell--format-str-fileonly ()
  (with-mock
   (mock (ivy-add-face-text-property * * * "counshell.el: ok") => nil)
   (should (equal
            (counshell--format-str "counshell.el: ok")
            "counshell.el: ok"))))

(ert-deftest counshell--format-str-current ()
  (with-mock
   (mock (ivy--add-face nil 'ivy-current-match) => nil)
   (mock (counshell--format-str "") => nil)
   (should (equal (counshell--format-str-current "") nil))))

(ert-deftest counshell--format ()
  (with-mock
   (mock (ivy--format-function-generic
          #'counshell--format-str-current
          #'counshell--format-str
          nil
          "\n") => nil)
   (should (equal (counshell--format nil) nil))))


;; Test counshell--action

(ert-deftest counshell--action-noline ()
  (with-mock
   (stub with-ivy-window => nil)
   (mock (find-file (expand-file-name "counshell.el")) => nil)
   (should (equal (counshell--action "counshell.el") nil))))

(ert-deftest counshell--action-line ()
  (with-mock
   (stub with-ivy-window => nil)
   (mock (find-file (expand-file-name "counshell.el")) => nil)
   (mock (goto-char 1) => nil)
   (mock (forward-line 4) => nil)
   (should (equal (counshell--action "counshell.el:5: ok") nil))))

(ert-deftest counshell--action-noline ()
  (with-mock
   (stub with-ivy-window => nil)
   (mock (message "File not found or unable to parse [x.el]") => nil)
   (should (equal (counshell--action "x.el") nil))))


;; Test counshell--function

(ert-deftest counshell--function-wrapper ()
  (with-mock
   (mock (projectile-project-p) => nil)
   (mock (counsel-more-chars 2))
   (should (equal (funcall (counshell--function-wrapper t "" "") "") nil))))

(ert-deftest counshell--function ()
  (with-mock
   (mock (projectile-project-p) => t)
   (mock (projectile-project-root) => "/")
   (mock (counshell--create-script "" "/" "ls counshell.el") => nil)
   (mock (counsel--async-command "bash  </dev/null | cat") => nil)
   (should (equal (counshell--function t "" "ls" "counshell.el") '("" "working...")))))


;; Test other

(ert-deftest counshell-sh-read ()
  (with-mock
   (stub ivy-read => nil)
   (stub make-temp-file => "")
   (should (equal (counshell-sh-read t "" "") nil))))

