+++
title = "1337 fake Screensaver"
author = ["mitchmarq42"]
date = 2022-12-06
categories = ["shell"]
draft = false
+++

It's 2022. Screens don't really need saving anymore - in fact, when there's talk
of a "screensaver", it's usually a lock-screen and/or just some animation that
would be cool if "screensavers" were still a thing. What I'm about to present is
no different.


## The vision {#the-vision}

_One Vision_

```text
one flesh, one bone, one true religion
one voice, one hope, one real decision
gimme gimme gimme gimme
...fried chicken
```

I want each of my monitors to display a fullscreen Matrix-like text crawl, and
then when I quit one of them it quits all of them. That's what I want. Should be
pretty simple, right?


## The obvious solution {#the-obvious-solution}

We should be able to just open one big Cmatrix window that covers all three
screens (yes I have three screens. Only two monitors though - the other one is a
laptop). That way there's only one to quit. Elementary, Watson-kun.


## Problem 1: resolution {#problem-1-resolution}

Here's how my monitors look physically:

```text
+---------------+---------------+    |   | |  +----------+
|               |               |    pillar|  |          |
|   1920x1080   |    1366x760   |    |   | |  | 1920x1080|
|               |               | <--cable--> +----------+
+---------------+---------------+    |___|/   \___________\
```

But here's how they look to the computer:

```text
+---------------+----------+---------------+
|               |          |               |
|   1920x1080   | 1366x760 |   1920x1080   |
|               |----------|               |
+---------------+          +---------------+
```

So a window that looks fine on the left monitor will be zoomed in on the middle
monitor, and small on the right ~~monitor~~​laptop.

This means that having one big window stretching over all three screens will
look all wonky. We need separate windows with different zoom levels.


## Problem 2: Separate windows {#problem-2-separate-windows}

Opening multiple terminal windows is fine - just call them with `&` and call it
a day.

Closing them is another matter.

Thankfully, things like `kill` and `killall` exist. If we have the PIDs of each
terminal process, it's killable. There are various shell-scripting tricks for
getting the PID straight off the process invocation, but I decided not to bother
with anything fancy. Instead, I'll just use a terminal I don't normally --
`Xterm` -- and kill all of those when one exits.


### Problem 2.5: Window Placement {#problem-2-dot-5-window-placement}

I'm using ubuntu on this computer because I don't care. Snaps aren't that bad,
Gnome isn't that bad, and all the server things I want to run, run. However,
Gnome is also a Wayland compositor, making ordinary Xorg tools insufficient for
automation - specifically stuff like `xdotool` for sending keystrokes and mouse
events. In addition, XWayland+Gnome doesn't seem to honor a window's request to
be placed on a specific monitor.

So behold! `Ydotool`​! An Xdotool replacement for Wayland!

... which doesn't work on gnome either. At least, I couldn't get it to work. No
idea why, it's been a week since this part.

Okay, for real this time. Behold [dotool](https://git.sr.ht/~geb/dotool). A display-server-agnostic desktop
automator and event simulator. Works sort of like xdotool or ydotool but with
pipe syntax for reasons.

Once it's installed and working, we can run basic commands like

```shell
echo "type twelve" | dotool
```

resulting in

```text
twelve
```

getting typed to the current window.

On my gnome setup which is hacky and probably non-standard, the keybinding to
move a window to the right by one monitor is super + shift + right. Let's define
this as a function in bash:

```shell
moveright () {
    # move a window right one workspace
    echo "key super+shift+right" | dotool
}
```

now running `moveright` in the terminal will move the currently selected window
to the right. It leaves the mouse in the same place because that's what the
keybinding does.

But I want to be able to move it twice. If we leave the mouse, another window
might get selected (this is a setting that I enable because it's very convenient
on tiling WMs). So another function

```shell
mouseright () {
    # move mouse one monitor right (ish)
    echo "mousemove 1300 0" | dotool
}
```

1300 isn't quite right, but it's close enough for now.

We should also have one for moving it back to the left:

```shell
mouseleft () {
    # move mouse one monitor left (ish)
    echo "mousemove -1300 0" | dotool
}
```


### Problem 2.5.1: Actually throwing windows onto the screen an moving them around {#problem-2-dot-5-dot-1-actually-throwing-windows-onto-the-screen-an-moving-them-around}

Assuming we start with the mouse on the left monitor, here's the basic control
flow:

-   Spawn the window that will be on the right monitor
    -   Move it right (to middle monitor)
    -   Move the mouse to follow it
    -   Move it right (to right monitor)
-   Move mouse left (back to left monitor)
-   Spawn the window that will be on the middle monitor
    -   Move it right (to middle monitor)
-   Spawn the window that will be on the left monitor

Also, the font sizes. The rightmost screen is small but with a high resolution,
so the text should be larger. The middle screen is large with low resolution, so
small text. Then somewhere in between for the left monitor.

As previously mentioned, we're using Xterm. Xterm's (relevant) command-line
options include:

```text
-fullscreen                 start in fullscreen
-bg COLOR                   background color, very intuitive
-fa FONT                    font, semi-intuitive
-e COMMAND                  command to run, in quotes
```

So to asynchronously launch a fullscreen Xterm with a black background and (not
my) Meslo font in size 12 running Cmatrix in screensaver mode, we can do:

```shell
xterm -fullscreen -bg black -fa "MesloLGS NF:size=12" -e "cmatrix" &
```

Sprucing that up into a function taking an argument for the font size:

```sh
COLOR=blue
FONT=MeseloLGS\ NF:size=
matrixterm () {
    # spawn terminal with fun matrix in it
    # $1 = the size of font
    xterm -fullscreen -bg black -fa "${FONT}$1" -e "cmatrix -abC$COLOR" &
}
```

Notice the expanded variable $FONT, and $COLOR. Cmatrix takes different colors
and at the moment I'm partial to blue.

So the control flow including fonts can go something like

```shell
openonall () {
    # Open $1 on all monitors
    $1 14
    moveright
    mouseright
    moveright
    mouseleft
    $1 9
    moveright
    $1 12
}
openall matrixterm
```


## Problem 3: on opening and closing {#problem-3-on-opening-and-closing}

As previously mentioned, processes are okay. If we know the PIDs we can just
kill them. [Here's](https://www.howtogeek.com/devops/bash-process-termination-hacks/) an article on finding and killing processes. I personally
don't believe in Xargs, so we're going to do this in multiple steps.

```shell
XTERMS=$(ps -ef | grep xterm | grep -v 'grep' | awk '{print $2}')
```

So here's an array of all the PIDs of running `xterm`​s. I want to streamline
checking if each of these are still running and signal an error if any aren't:

```shell
isrunning () {
    # $1 = process to check
    ps -p $1 >/dev/null
}
checkallprocs () {
    for proc in $*
    do
      isrunning $proc || return 1
    done
}
```

So any run of `checkallprocs $XTERMS` will succeed as long as all of the
original xterms are running, but will fail once one is manually terminated. This
is perfect for a While loop:

```shell
INTERVAL=0.5
while checkallprocs $XTERMS
do
    sleep $INTERVAL
done
```

The interpreter doesn't move on until this loop is broken, meaning we can simply
put the final Kill right after it.

```shell
for proc in $XTERMS
do
    kill -9 $proc 2>/dev/null
done
```

One of those iterations will fail because that xterm is already closed. But oh
well.


## Wrapping it up in a script with a hotkey {#wrapping-it-up-in-a-script-with-a-hotkey}

-   [Here's](https://git.mitchmarq42.xyz/mitch/dotfiles/src/branch/main/.local/bin/trimatrix) the whole script

And I bound it to super+z because nothing else appeared to be bound to that key
and it's easy to hit when leaving the computer.

There's definitely improvements that can be made to this, but I found it to be a
positive learning experience and wanted to share it with thou, the void.
