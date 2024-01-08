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

We use prefixes for most symbols, next are some conventions:

- `>>=` for general definitions,
- `>>=!` for constants,
- `>>=|` for configuration variables,
- `>>=+` for building blocks configuration variables,
- `>>-` for module internals, and
- `>>:` for declarative functions in package configuration options.

Standard modules will use "-" separator in its names (for example
"xorns-startup.el").  Building-block modules (units) will use "+" instead (for
example "xorns+python.el", "").  Sub-levels are joined with "/" (for example
">>=+base/extra-packages-to-configure").
