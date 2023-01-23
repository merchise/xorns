# Merchise Xorns

## An improved way to use Emacs

The initial goal of Xorns is to automate the Emacs configuration for Merchise
members.  It has now become a set of packages that could be used generally by
any Emacs user.

To develop Xorns, tutorials and other similar packages are studied: [David
Wilson's video series](https://www.youtube.com/@SystemCrafters) and the
[Spacemacs package](https://github.com/syl20bnr/spacemacs) deserve special
mention.


## Xorns stages

You have two options to use Xorns:

  1. installing it as a standard package or
  2. cloning the repository to the User Emacs Directory `"~/.emacs.d"`.

**Note**: Option (1) is deprecated now, option (2) is preferred.

To install Xorns using option (1) you must have the User Emacs Directory
prepared with all dependencies already installed.  Use:

```shell
  git clone https://github.com/merchise/xorns.git
  cd xorns
  make local-install
```

To use option (2), just:

```shell
  git clone https://github.com/merchise/xorns.git ~/.emacs.d
```


## Conventions

We prefixes most symbols, next are some conventions:

- `>>=` for general definitions,
- `>>=!` for constants,
- `>>=|` for configuration variables,
- `>>=+` for building blocks configuration variables,
- `>>=-` or `>>-` for module internals, and
- `>>:` for declarative functions in package configuration options.

Standard modules will use "-" separator in its names (for example
"xorns-startup.el").  Building-block modules (units) will use "+" instead (for
example "xorns+python.el", "").  Sub-levels are joined with "/" (for example
">>=+base/extra-packages-to-configure").


## Backlog

### Misc

  - Integrate `xorns-setup` and `xorns-packages` modules.
  - Deft is too slow after Emacs 28.  Function `deft-parse-summary` was
    replaced.  Check after Emacs or Deft are updated.

### Fix loss of focus for Brave when using EXWM

It happens when changing the workspace.  Below is an experimental code being
tested:

```lisp
(add-hook
  'exwm-workspace-switch-hook
  (defun >>-workspace-switch ()
    (when (eq exwm-workspace-current-index 2)
      (let* ((frame (selected-frame))
             (buf (current-buffer))
             (win (frame-selected-window frame)) )
        (message ">>= current buffer %s." buf)
        (next-window)
        (switch-to-buffer buf nil 'force)
        )
      ))
  )

(setq exwm-workspace-switch-hook '(exwm-systemtray--on-workspace-switch))
```
