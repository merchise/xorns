# Xorns - an improved way to use Emacs


## Introduction

Xorns is a configuration framework for GNU [Emacs].  It was initially intended
to be used by [Merchise] members who wanted to take it to the next level by
using Emacs.

[Emacs]: https://www.gnu.org/software/emacs/
[Merchise]: https://github.com/merchise#what-is-merchise


## Install

1. Select a target folder (you can use `~/.emacs.d` or `~/.config/emacs`):

   ```shell
   TARGET_FOLDER=~/.emacs.d
   ```

2. Backup any files you have in your `$TARGET_FOLDER`.

   ```shell
   [ -d $TARGET_FOLDER ] && mv $TARGET_FOLDER ${TARGET_FOLDER}.bak
   ```

3. Clone the repository:

   ```shell
   git clone --depth 1 https://github.com/merchise/xorns.git $TARGET_FOLDER
   ```

4. Xorns is now ready to run, but it is advisable to first create a user
   configuration folder:

   ```shell
   CONFIG_HOME=${XDG_CONFIG_HOME:~/.config}
   mkdir $CONFIG_HOME/xorns
   ```

   Inside this folder there must be at least two files:

   - `user-config.el`: Xorns local user configuration file.  There is a
      template to create this file (`templates/user-config`).  Copy it, and
      rename it to `user-config.el`, and customize the user experience
      according to your preferences.
   - `custom.el`: File used for storing standard Emacs customization
     information.  See the Emacs variable `custom-file` for more information.


## Documentation

Check the [Xorns User Manual][docs/xorns.org].


## Prerequisites

- Emacs 28.1+ (recommended: 29.1+ with native compilation enabled)

- Git: If you need to be told this, this package is not for you. ;)

- The fonts you will customize.

  Recommendations:
  - [Source Code Pro][]
  - [RobotoMono Nerd Font][], part of Nerd Fonts, project that patches
    developer targeted fonts with a high number of icons.

  You can also use fallback fonts to ensure that certain Unicode symbols
  appear correctly (If you don't do this you may have problems with certain
  types of `mode-line`):
  - For example: [Nanum Gothic][]

  You can search fonts on:
  - https://www.wfonts.com/
  - https://sharefonts.net

  Maybe you can install some fonts using your OS package manager.

  There could be packages using [all-the-icons][] and [nerd-icons][].  If any
  of those are installed, please execute `all-the-icons-install-fonts` and
  `nerd-icons-install-fonts` helper functions.

- Several commands use operating system programs.  In the XS module you can
  see how to configure these options.  For example:
  - `grep`: very slow but widely available on most systems.
  - [rg][] (ripgrep): line-oriented search tool that recursively searches the
    current directory for a regular expression pattern (strongly recommend).
  - [fzf][]: general-purpose command-line fuzzy finder.

[Source Code Pro]: https://adobe-fonts.github.io/source-code-pro/
[RobotoMono Nerd Font]: https://github.com/ryanoasis/nerd-fonts
[Nanum Gothic]: https://fonts.google.com/specimen/Nanum+Gothic
[rg]: https://github.com/BurntSushi/ripgrep
[fzf]: https://github.com/junegunn/fzf
[all-the-icons]: https://github.com/domtronn/all-the-icons.el
[nerd-icons]: https://github.com/rainstormstudio/nerd-icons.el


## Known Issues

- When the module containing the `vterm` configuration is compiled to native
  code, although this configuration is optional, it is installed from ELPA and
  the `vterm-module` package is compiled.  Sometimes I needed to manually
  install the ELPA =vterm= package to avoid some errors when compiling the
  modules.  See `backlog` file for more information.


## Contributions

This package is currently experimental, but the plan is that in the future it
can be used by anyone.  From now on, any [contribution][] and suggestion will
be welcome.

[contribution]: https://docs.github.com/en/get-started/exploring-projects-on-github/contributing-to-a-project


## References

To make `xorns`, the most important references that we have used are:
- [David Wilson's videos](https://www.youtube.com/@SystemCrafters)
- [Mastering Emacs](https://www.masteringemacs.org)
- [The Spacemacs package](https://github.com/syl20bnr/spacemacs)
- [Doom Emacs](https://github.com/doomemacs/doomemacs)
