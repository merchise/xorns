# Merchise Xorns

## An improved way to use Emacs

Xorns automates Emacs configurations for those packages commonly used for
Merchise members in development tasks.

## Xorns stages

Xorns has two stages: (1) as an Emacs Lisp Package, and (2) as the User Emacs
Directory.

The first stage will be deprecated soon, its maximum version number is
"0.4.9", second stage will start at "0.5.0".

To install locally in (1):

```shell
git clone https://github.com/merchise/xorns.git
cd xorns
make local-install
```

You must have a User Emacs Directory prepared with all dependencies installed.

To install a stage two version:

```shell
git clone https://github.com/merchise/xorns.git ~/.emacs.d
```

Remember backup all your previous Emacs files and directories from the user
home.

A version of stage two has not yet been released in the "stable" branch.
Meanwhile, execute:

```shell
git clone -b feature-new-age https://github.com/merchise/xorns.git ~/.emacs.d
```

## Conventions

Most structures are prefixed, next are some conventions:

- `>>=` for general definitions,
- `>>=!` for constants,
- `>>=|` for configuration variables,
- `>>=+` for building blocks configuration variables,
- `>>=-` for module internals, and
- `>>:` for declarative functions in package configuration options.

Standard modules will use "-" separator in its names (for example
"xorns-startup.el").  Building-block modules (units) will use "+" instead (for
example "xorns+python.el", "").  Sub-levels are joined with "/" (for example
">>=+base/extra-packages-to-configure").


## Backlog

### Avoid compiler errors

Sometimes, there are errors depending on module load; for example, using
constant `>>=!home-dir` in other module different to where it is defined (see
`xorns-pim`).  Maybe the solution is to use `eval-when-compile`, or
`eval-and-compile`, function.

### Use 'lexical-binding' in xorns modules

Research how to use `lexical-binding` in most `xorns ` modules.

### Improve 'multiple-cursors'

- Integrate 'multiple-cursors' with 'mode-line'. Show the active cursors count
  in the mode-line.
- Check for alternative packages: `ace-mc`, `evil-mc`, `evil-multiedit`,
  `smart-region`.


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
