# Paul M. Winkler’s Emacs config

Personal Emacs setup: a thin `~/.emacs` that loads modular files from
`~/.emacs.d/`. Packages are managed with
[straight.el](https://github.com/radian-software/straight.el) and
`use-package`.

## Layout

| Path | Role |
|------|------|
| `.emacs` | Entry point: load order and `custom-set-*` |
| `.emacs.d/early-init.el` | Disable built-in `package.el` (straight) |
| `.emacs.d/*.el` | Modular config loaded by `.emacs` |
| `.emacs.d/site-lisp/` | Local Lisp on `load-path` |
| `.emacs.d/straight/versions/` | Frozen package versions |

Runtime caches (`elpa/`, `straight/repos/`, `straight/build/`,
`eln-cache/`, desktop/recentf, etc.) are gitignored.

## Package maintenance

```elisp
;; After Emacs upgrade or new machine:
M-x straight-pull-all

;; Freeze and commit lockfiles:
M-x straight-freeze-versions
;; then commit .emacs.d/straight/versions/
```
