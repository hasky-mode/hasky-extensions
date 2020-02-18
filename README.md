# Hasky Extensions

*This project is in “limited-maintenance” mode. I will not spend any of my
time supporting it. You can still open PRs if you must, or you can take over
if you wish. I'll mark the project as deprecated and stop supporting it
altogether in some months.*

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/hasky-extensions-badge.svg)](https://melpa.org/#/hasky-extensions)
[![CircleCI](https://circleci.com/gh/hasky-mode/hasky-extensions/tree/master.svg?style=svg)](https://circleci.com/gh/hasky-mode/hasky-extensions/tree/master)

The package provides a way to add and remove Haskell language extensions
easily from any place in a file without moving the point. This is done with
the help of a menu where the most popular language extensions are assigned
just one letter to toggle, while the others require two key strokes:

![Hasky extensions](https://raw.githubusercontent.com/hasky-mode/hasky-extensions/gh-pages/hasky-extensions.png)

Naturally, when performing toggling of the extensions, they are kept sorted
and aligned automatically for you.

## Installation

Download this package and place it somewhere, so Emacs can see it. Then put
`(require 'hasky-extensions)` into your configuration file. Done!

It's available via MELPA, so you can just <kbd>M-x package-install RET
hasky-extensions RET</kbd>.

## Usage

Just bind the `hasky-extensions` and `hasky-extensions-browse-docs`
commands, for example:

```emacs-lisp
(global-set-key (kbd "C-c y")     #'hasky-extensions)
(global-set-key (kbd "<next> h x" #'hasky-extensions-browse-docs))
```

When the menu shows up, type letters assigned to the language extension you
wish to toggle. To exit the menu press <kbd>C-g</kbd>.

## Customization

To view available customization options, type <kbd>M-x customize-group RET
hasky-extensions RET</kbd>. Most important variables are:

* `hasky-extensions`—list of all extensions to show in the menu.

* `haksy-extensions-docs`—A collection of extensions with links to GHC user
  guide.

* `hasky-extensions-reach`—how many characters from the beginning of file to
  scan. We cannot always scan entire files because they can be quite big,
  the default value, however, should be OK in 99.99% of cases.

* `hasky-extensions-sorting`—whether to keep the collection of extensions
  sorted, default is `t`.

* `hasky-extensions-aligning`—whether to keep closing braces of extension
  pragmas aligned, default is `t`.

* `hasky-extensions-prettifying-hook`—the hook to run after prettifying the
  extension list. For example, you could run `whitespace-cleanup` there.

The package in powered by the
[`avy-menu`](https://github.com/mrkkrp/avy-menu) library, which implements
this handy Avy-powered popup menu. To control appearance of the menu, use
<kbd>M-x customize-group avy-menu RET</kbd>.

## License

Copyright © 2016–2019 Mark Karpov

Distributed under GNU GPL, version 3.
