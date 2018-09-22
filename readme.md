# Resize Mode #

[![Build Status](https://travis-ci.org/dpsutton/resize-window.svg)](https://travis-ci.org/dpsutton/resize-window)
[![Coverage Status](https://coveralls.io/repos/dpsutton/resize-window/badge.svg?branch=master&service=github)](https://coveralls.io/github/dpsutton/resize-window?branch=master)
[![MELPA](http://melpa.org/packages/resize-window-badge.svg)](http://melpa.org/#/resize-window)
[![MELPA stable](http://stable.melpa.org/packages/resize-window-badge.svg)](http://stable.melpa.org/#/resize-window)
[![Tag Version](https://img.shields.io/github/tag/dpsutton/resize-window.svg)](https://github.com/dpsutton/resize-window/tags)
[![License](https://img.shields.io/:license-gpl3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

## What it is ##

Resizing windows is a pain in emacs. You have to do an uncomfortable
chord `C-x {`, `C-x ^`. Giving prefixes to make the jump larger than 1
line means remembering `C-u <number> <terrible chord>`. I always
forget the number, making it difficult to do this. So this is a simple
minor mode to easily adjust window sizes with familiar keys and
without chords.

## How to use it ##

As soon as it gets polished a little further, it will hopefully be
accepted to ELPA or something (I'm not too sure about all of the
differences, ELPA, MELPA, marmalade, etc.)

I've submitted for elpa and have a pending request to have copyright
assigned. Until then, just drop it into your load path. I've found the
following keybinding to be really nice:
`(global-set-key (kbd "C-c ;") 'resize-window)`.
This keeps your fingers on the home row, especially if you've remapped
the caps lock key to control.

But, just run `M-x resize-window`. There are only a few commands to learn,
and they mimic the normal motions in emacs.

- `n`: Resize the window vertically like scrolling down. Use `N` for 5
lines at once.
- `p`: Resize the window vertically like scrolling up. Use `P` for 5
lines at once.
- `f`: Resize the window horizontally like scrolling forward. Use `F`
for 5 lines at once.
- `b`: Resize the window horizontally like scrolling backward. Use `B`
for 5 lines at once.
- `w`: Cycle through windows so that you can adjust other window
panes. Use `W` to cycle in the opposite direction.
- `e`: Even the size of the windows.
- `2`: Split the window horizontally.
- `3`: Split the window vertically.
- `0`: Delete the current window.
- `k`: Delete other windows and save the state on the stack.
- `s`: Save the state on the stack so you may restore it later.
- `>`: Restore to a previous saved state. Use `<` to restore in the
opposite direction.
- `?`: Display the help menu listing commands.

The best part of this is that resize-window keeps listening for more
keystrokes until it doesn't recognize input. So you don't have to make
convulted chords to resize things two lines, or prefix a negative
number because you can't guess how far `C-u 17 C-x ^` will adjust your
view right away. Just invoke resize-window and just press your normal
motions and cycle windows until everything is adjusted to how you like
it.

## How to extend it ##

There are a few things that you can do. There are customizable variables:
- resize-window-coarse-argument (default: 5)
- resize-window-fine-argument (default: 1)
- resize-window-allow-backgrounds (default: t)
- resize-window-unregistered-key-quit (default: nil)
- resize-window-stack-size (default: 16)
- resize-window-swap-capital-and-lowercase-behavior (default: nil)
- resize-window-notify-with-messages (default: t)

Any of these can be customized by using `customize-group RET
resize-window` or by setting the appropriate variable in your init.el
file as normal: `(setq <var> <val>)`.

## What's even cooler ##

At the end of the day, this is really just a function dispatcher
listening for key presses. So i've found a really nice workflow. I've
bound resize-window to `C-c ;` and i've also added a new dispatch:

    (push '(?l helm-mini "run helm-mini" nil) resize-window-dispatch-alist)

This allows for a really nice workflow. Now with `w` it is really easy
to bounce around windows, use keys to enlarge them, and then hit l to
run helm-mini. Its trivial now to bounce around, resize windows and
reset their sources. And since the help menu is dynamically generated,
pressing ? displays this new choice automatically.

For convenience sake, you can use the helper method `resize-window-add-choice`
to register your function without having to remember the structure of the list
it will end up in. For example:

    (push '(?h (lambda () (dired "~/projects/clojure")) "Clojure home" nil))
    ; is equivalent to

    (resize-window-add-choice ?h (lambda () (dired "~/projects/clojure")) "Clojure home")

    ; the allows-capitals argument is optional.

Further, there are aliases, held in the `resize-window-alias-list` alist. It is
currently defined as

    (defvar resize-windown-alias-list
      '((right ?f)
        (up ?n)
        (left ?b)
        (down ?p))
      "List of aliases for commands.
    Rather than have to use n, etc, you can alias keys for others.")

However, you can easily add your own. For instance, to alias h to ?,
the help command, just add `(push '(?h ??) resize-window-alias-list)` in your init.el.

### Kill and restore windows ##

![usage gif](images/kill_windows.gif)

In this example, we can bounce back and forth between the test and
code of resize-window. When we want to work in one exclusively, we
call up resize-window (bound with `C-c ;` and then hit `k` for kill
all the other windows. We edit our tests and then call up
resize-window and hit `>` restore to a succeding saved state or `<`
for a preceding one. Think that we just put them into a ring buffer,
but they are actually in a stack.

## The window configurations stack ##

The stack is a customizable size holder for window configurations. It
folds over. Moving after the end restarts from the beginning and vice
versa. Old configurations are dropped due to a chosen reduction in its
size or an exceding number of configurations saved.

Move forward/backward via `>` and `<` (to avoid pressing a modifier
key, you may consider `,` and `.` as possible alternatives).
Originally I was using `r` and `R` to move in the stack...

Special flags give hints about the direction followed, forward `>` or
backward `<`, and if the current window configuration is modified `*`
or not `=` (aka saved in the stack at the current position).

When a configuration is modified, adjacent positions in the stack are
considered to see if such new configuration is already there. In such
a case, modification flag and direction followed are set accordingly.

## Create windows ##

Here, we want to create a bunch of windows. We can use `2` and `3` to
make splits like their native emacs commands `C-x 2` and `C-x 3`. Use
`0` to kill the split. If you want to go down to a single, use the
example above to hit `k` to kill all and then `>` or `<` to restore.
Again, all of the buffer resizing commands work (`f`, `p`, `b`, `n`)
to resize these buffers.

![usage gif](images/navigate.gif)

## Bugs ##

Working to spot one to fix...!

## Hopeful upcoming features ##

I can't promise any of these but these are the things I'm hoping to
do:

- DONE: put overlays over *other* buffers rather than current one. This
greys out other workspaces and leaves yours with full color,
bringing your eye there. Just seems like a nice ergonomic touch.
- DONE: allow customization to invert capital/non-capital behavior. Right
now, lower case selections move the window size by 1 and upper-case
moves by 5. These should both be easy to customize and easy to
*flip*. Ie, make the lowercase `n` make it bigger by 5 and the
upper-case `N` increase by 1.

## Shout out ##

This is my first attempt at really doing anything in elisp and to get
there, I drew lots of inspiration and organizational ideas and almost
verbatim code from `ace-mode`. Ace mode is super super nice and my aim
is to make this more or less an ace mode for resizing windows rather
than jumping. In fact, this might actually be better as a pull request
to their package.

## How it works ##

Super simple architecture. There's an associative list called
`resize-window-dispatch-alist` (the rm is prefix for resize-mode) that
holds a simple data structure with the invocation-key, the function to
call, documentation string to be shown as a message, and whether it
should accept a capital letter as well as the lower-case letter.

The function `resize-window` is just a while loop listening for
characters, checking if that character is associated to anything in
the list and checking if `(+ char 32)` is associated to anything in
the list (which is just the uppercase version (or is it? now its a
bug)). If lower case matches, do it, if uppercase matches something,
then make sure that's ok and do it but with the
`resize-window-uppercase-argument` rather than
`resize-window-lowercase-argument`.
