# dotfiles

## Notes on zsh on macos

Sources:
 - https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout
 - https://apple.stackexchange.com/questions/388622/zsh-zprofile-zshrc-zlogin-what-goes-where

A login shell is simply a shell, whether local or remote, that allows a user to authenticate to the system. These shells are typically interactive shells.
After ~/.zshenv is sourced, the next file that is loaded is /etc/zprofile, which is provided by macOS. This script executes

```
eval `/usr/libexec/path_helper -s`
```
Note ``path_helper`` appears to rebuild PATH in the following order:

1. Directories listed in the file /etc/paths
2. Directories listed in the files in /etc/paths.d (note: files are not enumerated in sorted order)
3. Any other directories that were previously in the PATH variable, excluding those that appear in the above lists

The solution is to do path initialization in ~/.zprofile on macOS. This file is loaded after /etc/zprofile and before ~/.zshrc.

### .zprofile (login shell)

``.zlogin`` and ``.zprofile`` are basically the same thing - they set the environment for login shells1; they just get loaded at different times (see below).  ``.zprofile`` is based on the Bash's .bash_profile while .zlogin is a derivative of CSH's .login. Since Bash was the default shell for everything up to Mojave, stick with .zprofile.

### .zshrc (interactive shell)

This sets the environment for interactive shells2. This gets loaded after .zprofile. It's typically a place where you "set it and forget it" type of parameters like $PATH, $PROMPT, aliases, and functions you would like to have in both login and interactive shells.

### .zshenv (environment variables - Optional)

This is read first and read every time. This is where you set environment variables. I say this is optional because is geared more toward advanced users where having your $PATH, $PAGER, or $EDITOR variables may be important for things like scripts that get called by launchd. Those run under a non-interactive shell 3 so anything in .zprofile or .zshrc won't get loaded. Personally, I don't use this one because I set the PATH variable in my script itself to ensure portability.

### .zlogin (login shell - Optional)


### .zlogout (when the shell exits - Optional)

But very useful! This is read when you log out of a session and is very good for cleaning things up when you leave (like resetting the Terminal Window Title)

For an excellent, in-depth explanation of what these files do, see [What should/shouldn't go in .zshenv, .zshrc, .zlogin, .zprofile, on Unix/Linux](https://unix.stackexchange.com/q/71253/107777)

### Order of Operations

This is the order in which these files get read. Keep in mind that it reads first from the system-wide file (i.e. /etc/zshenv) then from the file in your home directory (~/.zshenv) as it goes through the following order.

.zshenv → .zprofile → .zshrc → .zlogin → .zlogout


