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

(ert-deftest counshell-trim-left ()
  (should (equal (trim-left "string") "string"))
  (should (equal (trim-left "string ") "string "))
  (should (equal (trim-left " string") "string")))

(ert-deftest counshell-filepath-noproject ()
  (with-mock
   (stub projectile-project-p => nil)
   (should (equal (counshell-filepath "filepath") "filepath"))))

(ert-deftest counshell-filepath-inproject ()
  (with-mock
   (stub projectile-project-p => t)
   (stub projectile-expand-root => "fileinproject")
   (should (equal (counshell-filepath "filepath") "fileinproject"))))
