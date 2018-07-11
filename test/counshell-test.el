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


(ert-deftest counshell-filepath-noproject ()
  (with-mock
   (stub projectile-project-p => nil)
   (should (equal (counshell--filepath "filepath") "filepath"))))

(ert-deftest counshell-filepath-inproject ()
  (with-mock
   (stub projectile-project-p => t)
   (stub projectile-expand-root => "fileinproject")
   (should (equal (counshell--filepath "filepath") "fileinproject"))))

(ert-deftest counshell-create-script ()
  (let ((scriptfile (make-temp-file "counshell-test.sh.")))
    (should (equal t (file-readable-p (counshell--create-script scriptfile "/tmp" "ls /"))))
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
   (should (equal (counshell--match-regexes "counshell.el:5: ok") 2))
   (should (equal (counshell--match-regexes "counshell.el: ok") 1))
   (should (equal (counshell--match-regexes "counshell.el") 1))
   (should (equal (counshell--match-regexes "zxcv:5: no file") nil)))
