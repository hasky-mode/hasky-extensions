;;; hasky-extensions.el --- Toggle Haskell language extensions -*- lexical-binding: t; -*-
;;
;; Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/hasky-mode/hasky-extensions
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (avy-menu "0.2"))
;; Keywords: programming
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

;; This package allows to switch language extensions easily and lookup
;; information about them.  It's part of Hasky Mode.

;;; Code:

(require 'avy-menu)
(require 'cl-lib)

(defgroup hasky-extensions nil
  "Toggle Haskell language extensions."
  :group  'programming
  :tag    "Hasky Extensions"
  :prefix "hasky-extensions-"
  :link   '(url-link :tag "GitHub"
                     "https://github.com/hasky-mode/hasky-extensions"))

(defface hasky-extensions-disabled
  '((t (:inherit font-lock-comment-face)))
  "Face used to print disabled Haskell extensions in the menu.")

(defface hasky-extensions-enabled
  '((t (:inherit font-lock-keyword-face)))
  "Face used to print enabled Haskell extensions in the menu.")

(defvar hasky-extensions ;; TODO work on right order here
  '("Arrows"
    "AutoDeriveTypeable"
    "BangPatterns"
    "CPP"
    "DataKinds"
    "DeriveAnyClass"
    "DeriveDataTypeable"
    "DeriveFoldable"
    "DeriveFunctor"
    "DeriveGeneric"
    "DeriveTraversable"
    "EmptyDataDecls"
    "ExistentialQuantification"
    "ExplicitForAll"
    "FlexibleContexts"
    "FlexibleInstances"
    "ForeignFunctionInterface"
    "FunctionalDependencies"
    "GADTs"
    "GeneralizedNewtypeDeriving"
    "InstanceSigs"
    "KindSignatures"
    "MagicHash"
    "MultiParamTypeClasses"
    "MultiWayIf"
    "NoImplicitPrelude"
    "OverloadedLists"
    "OverloadedStrings"
    "PolyKinds"
    "QuasiQuotes"
    "RankNTypes"
    "RecordWildCards"
    "RecursiveDo"
    "ScopedTypeVariables"
    "StandaloneDeriving"
    "TemplateHaskell"
    "TupleSections"
    "TypeFamilies"
    "TypeOperators"
    "TypeSynonymInstances"
    "UndecidableInstances"
    "ViewPatterns")
  "List of commonly used Haskell extensions.")

(defvar hasky-extensions-reach 5000
  "Max number of characters from beginning of file to search.

Very large files can either slow down the process of extensions
detection or cause stack overflows, thus we limit number of
characters the package will traverse.  The default value should
be appropriate for most users since language extension pragmas
are typically placed in the beginning of file.  If you wish to
disable the limitation, set this value to NIL (not recommended).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The editing itself

(defun hasky-extensions-list ()
  "List all active Haskell extensions in current file.

Returned list is always a fresh one (you can perform destructive
operations on it without fear).

This does not take into account extensions enabled in Cabal file
with “default-extensions” or similar settings."
  (let (exts)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^\\s-*{-#\\s-*LANGUAGE\\s-+\\([[:alnum:]]+\\)\\s-*#-}\\s-*$"
              hasky-extensions-reach t)
        (push (match-string-no-properties 1) exts)))
    exts))

(defun hasky-extensions-add (extension)
  "Insert EXTENSION into appropriate place in current file."
  (message "I'm adding %s, honest!" extension)) ;; TODO

(defun hasky-extensions-remove (extension)
  "Remove EXTENSION from current file (if present)."
  (message "I'm removing %s, honest!" extension)) ;; TODO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface

(defun hasky-extensions ()
  "Invoke the menu that allows to add and remove Haskell language extensions."
  (interactive)
  (let ((exts (hasky-extensions-list))
        (selected t))
    (while selected
      (setq
       selected
       (avy-menu
        "*hasky-extensions*"
        (cons
         "Haskell Extensions"
         (list
          (cons
           "Pane"
           (mapcar
            (lambda (x)
              (let ((active (cl-find x exts :test #'string=)))
                (cons
                 (propertize
                  x
                  'face
                  (if active
                      'hasky-extensions-enabled
                    'hasky-extensions-disabled))
                 (cons x active))))
            hasky-extensions))))))
      (when selected
        (cl-destructuring-bind (ext . active) selected
          (if active
              (progn
                (hasky-extensions-remove ext)
                (setq exts (cl-delete ext exts :test #'string=)))
            (hasky-extensions-add ext)
            (cl-pushnew ext exts :test #'string=)))))))

(provide 'hasky-extensions)

;;; hasky-extensions.el ends here
