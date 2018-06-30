;;; counshell-test.el --- Tests for counshell -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Leandro Lisboa Penz

;; Author: Leandro Lisboa Penz <lpenz@lpenz.org>

;; This file is subject to the terms and conditions defined in
;; file 'LICENSE', which is part of this source code package.

(require 'undercover)
(undercover "*.el" (:report-type :codecov))

(add-to-list 'load-path (f-parent (f-dirname load-file-name)))

(require 'counshell)

(ert-deftest counshell-trim-left ()
  (should (equal (trim-left "string") "string"))
  (should (equal (trim-left "string ") "string "))
  (should (equal (trim-left " string") "string")))
