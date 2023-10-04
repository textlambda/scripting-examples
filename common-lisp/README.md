# Demo

Symlink this directory in your quicklisp projects folder, which is probably `~/quicklisp/local-projects` or `~/.roswell/lisp/quicklisp/local-projects`:

```
ln -s {path-to-this-folder} tl
```

In a repl,

```lisp
(ql:quickload :tl)
(in-package :tl)
(tl/start)
```
