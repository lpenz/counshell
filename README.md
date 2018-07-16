[![Build Status](https://travis-ci.org/lpenz/counshell.png?branch=master)](https://travis-ci.org/lpenz/counshell)
[![codecov](https://codecov.io/gh/lpenz/counshell/branch/master/graph/badge.svg)](https://codecov.io/gh/lpenz/counshell)

# counshell

**counshell** provides an interactive command line where you can
navigate the resulting file names and line numbers
using [ivy](https://github.com/abo-abo/swiper).

![counshell demo](counshell-demo.gif "counshell demo")


## Installation

No package installation is available yet. Just clone this repository
in your installation path.

You should also create bindings to the top-level function you want to
use. For example:

```elisp
(global-set-key (kbd "<C-C> e b") 'counshell-sh)
(global-set-key (kbd "<C-C> e e") 'counshell-projectile-sh)
(global-set-key (kbd "<C-C> e f") 'counshell-projectile-find-grep)
(global-set-key (kbd "<C-C> e s") 'counshell-projectile-rg)
(global-set-key (kbd "<C-C> e g") 'counshell-projectile-grep)
(global-set-key (kbd "<C-C> e b") 'counshell-projectile-gnuglobal)
(global-set-key (kbd "<C-C> e i") 'counshell-projectile-idutils-gid)
```

`counshell-sh` is the function that lets you type the whole shell
command. The other functions will add a template command-line and
allow you to complete it interactively.


## Usage

When in *counshell*, whatever you type ends up being executed using
the shell. The output of that is captured and presented to the user
as [ivy] completions for file names and line numbers (if possible).
All [ivy keys] work as expected.

Be aware that several partial commands will be executed while you
type, which might get "interesting" if your command has
side-effects. For instance, if the command requires you to type the
name of a file that is created, files with partial names can end up
being created.


[ivy]: https://github.com/abo-abo/swiper
[ivy keys]: http://oremacs.com/swiper/#key-bindings
