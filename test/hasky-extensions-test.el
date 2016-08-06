;;; hasky-extensions-test.el --- Tests for Hasky Extensions -*- lexical-binding: t; -*-
;;
;; Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/hasky-mode/hasky-extensions
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'undercover)

(undercover "hasky-extensions.el")

(require 'cl-lib)
(require 'hasky-extensions)

(defmacro run-with-file (filename &rest body)
  "Load FILENAME in a temporary buffer and evaluate BODY."
  `(with-temp-buffer
     (insert-file-contents ,filename)
     ,@body))

(defun file-string (filename)
  "Return contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; `hasky-extensions-list'

(ert-deftest hasky-extensions-list/empty ()
  (should
   (equal
    (run-with-file
     "test/data/Empty.hs"
     (hasky-extensions-list))
    nil)))

(ert-deftest hasky-extensions-list/empty-with-header ()
  (should
   (equal
    (run-with-file
     "test/data/EmptyWithHeader.hs"
     (hasky-extensions-list))
    nil)))

(ert-deftest hasky-extensions-list/empty-with-header-ext ()
  (should
   (equal
    (run-with-file
     "test/data/EmptyWithHeaderExt.hs"
     (hasky-extensions-list))
    '("CPP"))))

(ert-deftest hasky-extensions-list/no-header ()
  (should
   (equal
    (run-with-file
     "test/data/NoHeader.hs"
     (hasky-extensions-list))
    '("PolyKinds"
      "GADTs"
      "DataKinds"))))

(ert-deftest hasky-extensions-list/with-header ()
  (should
   (equal
    (run-with-file
     "test/data/WithHeader.hs"
     (hasky-extensions-list))
    '("TupleSections"
      "OverloadedStrings"
      "DataKinds"))))

(ert-deftest hasky-extensions-list/tricky-space ()
  (should
   (equal
    (run-with-file
     "test/data/TrickySpace.hs"
     (hasky-extensions-list))
    '("CPP"
      "DeriveDataTypeable"
      "OverloadedStrings"
      "DataKinds"))))

;; `hasky-extensions-add'

(ert-deftest hasky-extensions-add/empty ()
  (should
   (equal
    (run-with-file
     "test/data/Empty.hs"
     (hasky-extensions-add "CPP")
     (hasky-extensions-add "DataKinds")
     (hasky-extensions-add "Arrows")
     (buffer-string))
    (file-string "test/data/add/Empty.hs"))))

(ert-deftest hasky-extensions-add/empty-with-header ()
  (should
   (equal
    (run-with-file
     "test/data/EmptyWithHeader.hs"
     (hasky-extensions-add "CPP")
     (hasky-extensions-add "OverloadedStrings")
     (hasky-extensions-add "DataKinds")
     (buffer-string))
    (file-string "test/data/add/EmptyWithHeader.hs"))))

(ert-deftest hasky-extensions-add/empty-with-header-ext ()
  (should
   (equal
    (run-with-file
     "test/data/EmptyWithHeaderExt.hs"
     (hasky-extensions-add "Arrows")
     (hasky-extensions-add "FlexibleContexts")
     (hasky-extensions-add "FlexibleInstances")
     (buffer-string))
    (file-string "test/data/add/EmptyWithHeaderExt.hs"))))

(ert-deftest hasky-extensions-add/no-header ()
  (should
   (equal
    (run-with-file
     "test/data/NoHeader.hs"
     (hasky-extensions-add "OverloadedStrings")
     (hasky-extensions-add "Arrows")
     (hasky-extensions-add "TemplateHaskell")
     (buffer-string))
    (file-string "test/data/add/NoHeader.hs"))))

(ert-deftest hasky-extensions-add/with-header ()
  (should
   (equal
    (run-with-file
     "test/data/WithHeader.hs"
     (hasky-extensions-add "CPP")
     (hasky-extensions-add "BangPatterns")
     (hasky-extensions-add "UndecidableInstances")
     (buffer-string))
    (file-string "test/data/add/WithHeader.hs"))))

(ert-deftest hasky-extensions-add/tricky-space ()
  (should
   (equal
    (run-with-file
     "test/data/TrickySpace.hs"
     (hasky-extensions-add "Arrows")
     (buffer-string))
    (file-string "test/data/add/TrickySpace.hs"))))

;; `hasky-extensions-remove'

(ert-deftest hasky-extensions-remove/empty ()
  (should
   (equal
    (run-with-file
     "test/data/Empty.hs"
     (hasky-extensions-remove "CPP")
     (hasky-extensions-remove "DataKinds")
     (hasky-extensions-remove "Arrows")
     (buffer-string))
    (file-string "test/data/remove/Empty.hs"))))

(ert-deftest hasky-extensions-remove/empty-with-header ()
  (should
   (equal
    (run-with-file
     "test/data/EmptyWithHeader.hs"
     (hasky-extensions-remove "CPP")
     (hasky-extensions-remove "OverloadedStrings")
     (hasky-extensions-remove "DataKinds")
     (buffer-string))
    (file-string "test/data/remove/EmptyWithHeader.hs"))))

(ert-deftest hasky-extensions-remove/empty-with-header-ext ()
  (should
   (equal
    (run-with-file
     "test/data/EmptyWithHeaderExt.hs"
     (hasky-extensions-remove "Arrows")
     (hasky-extensions-remove "CPP")
     (buffer-string))
    (file-string "test/data/remove/EmptyWithHeaderExt.hs"))))

(ert-deftest hasky-extensions-remove/no-header ()
  (should
   (equal
    (run-with-file
     "test/data/NoHeader.hs"
     (hasky-extensions-remove "DataKinds")
     (hasky-extensions-remove "PolyKinds")
     (buffer-string))
    (file-string "test/data/remove/NoHeader.hs"))))

(ert-deftest hasky-extensions-remove/with-header ()
  (should
   (equal
    (run-with-file
     "test/data/WithHeader.hs"
     (hasky-extensions-remove "OverloadedStrings")
     (buffer-string))
    (file-string "test/data/remove/WithHeader.hs"))))

(ert-deftest hasky-extensions-remove/tricky-space ()
  (should
   (equal
    (run-with-file
     "test/data/TrickySpace.hs"
     (hasky-extensions-remove "DeriveDataTypeable")
     (buffer-string))
    (file-string "test/data/remove/TrickySpace.hs"))))

(provide 'hasky-extensions-test)

;;; hasky-extensions-test.el ends here
