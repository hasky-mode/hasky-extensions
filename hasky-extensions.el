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

(provide 'hasky-extensions)

;;; hasky-extensions.el ends here
