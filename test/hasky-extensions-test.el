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

;; `hasky-extensions-list'

(ert-deftest hasky-extensions-list/empty ()
  (should (equal (run-with-file "test/data/Empty.hs"
                                (hasky-extensions-list))
                 nil)))

(ert-deftest hasky-extensions-list/empty-with-header ()
  (should (equal (run-with-file "test/data/EmptyWithHeader.hs"
                                (hasky-extensions-list))
                 nil)))

(ert-deftest hasky-extensions-list/empty-with-header-ext ()
  (should (equal (run-with-file "test/data/EmptyWithHeaderExt.hs"
                                (hasky-extensions-list))
                 '("CPP"))))

(ert-deftest hasky-extensions-list/no-header ()
  (should (equal (run-with-file "test/data/NoHeader.hs"
                                (hasky-extensions-list))
                 '("PolyKinds"
                   "GADTs"
                   "DataKinds"))))

(ert-deftest hasky-extensions-list/with-header ()
  (should (equal (run-with-file "test/data/WithHeader.hs"
                                (hasky-extensions-list))
                 '("TupleSections"
                   "OverloadedStrings"
                   "DataKinds"))))

(ert-deftest hasky-extensions-list/tricky-space ()
  (should (equal (run-with-file "test/data/TrickySpace.hs"
                                (hasky-extensions-list))
                 '("CPP"
                   "DeriveDataTypeable"
                   "OverloadedStrings"
                   "DataKinds"))))

(provide 'hasky-extensions-test)

;;; hasky-extensions-test.el ends here
