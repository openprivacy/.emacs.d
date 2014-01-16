# .emacs.d
_This configuration requires emacs 24+_

### To install
* Remove (or rename) any existing `.emacs` file and/or `.emacs.d` directory that currently exists in your home directory
* Clone this repository into your home directory
* Restart emacs

Note that you'll likely want to fork this so that you can manage your own packages and key bindings.

### Notable features
* `drupal-mode` supports `flymake-phpcs` for on-the-fly `PHP_CodeSniffer` editing
** In s `drupal-mode` buffer, hit Shift-Meta-RightArrow to start `flymake-phpcs`
** Use Shift-Meta-DownArrow to go to next flymake error
* `clojure-mode` is built on `nREPL` and `Cider`
