+++
title = "Making Neofetch aware of Emacs"
author = ["mitchmarq42"]
date = 2022-12-02
categories = ["emacs"]
draft = false
+++

## Introduction {#introduction}

I have a confession to make. I was, and still technically am, an Arch user (by
the way). On my main machines I use ubuntu or fedora, but that's only because
I'm tired of updates and reboots. Computers have always been toys to me, but the
OS itself was becoming more of a tool or platform.

Enter Emacs.

If you're reading this, you probably know how freakin' _cool_ Emacs is. A few days
ago, the [EAT terminal](https://codeberg.org/akib/emacs-eat) went public, and it's great. You can even use it inside
Eshell. And with this, it feels like Emacs is my true OS more than the
underlying OS itself, I felt it would be a good exercise to have blingy terminal
tools reflect that.

Enter Neofetch.

If you somehow haven't come across Neofetch yet, Kamisama bless your sweet
summer soul. It's the thing that looks like this:

{{&lt; figure
src=![](https://linoxide.com/wp-content/uploads/2018/07/neofetch-ArchLinux.png) &gt;}}

The challenge: **Make Neofetch display the Emacs logo**


## Part 1: how Neofetch works {#part-1-how-neofetch-works}

Neofetch is written in Bash 3.2+, which makes it compatible with even the
ancient bash that MacOS ships with. It has a config file which it creates if not
present, and will read from at startup. This config file is also written in
Bash, and thus you can define custom functions in it that follow the standard
format of the built-in ones. For example:

```bash
get_pane() {
    pane=$(
	[ -n ${TMUX} ] &&
	    echo "${TMUX//%//}"
	[ -z ${TMUX} ] &&
	    echo 'N/A'
    )
prin "Tmux Pane" "$pane"
}
```

This displays the Tmux pane that you're in, or else "N/A". I used to use tmux
for everything because I wanted to keep processes running despite closing and
opening terminals, and splitting windows arbitrarily. Thankfully now Emacs can
handle most of that!

Anyway, snippets like the above will get recognized by neofetch when it prints
information on the right of the image. It scans for all the "get_\*" functions
and runs them in the order described in the function `print_info` (defined at the
top of the config). So for tmux all I have to do to enable it is put something
like

```bash
info "Tmux Pane" pane
```

before the end of that function.

Problem:


### Logos do not work like info lines {#logos-do-not-work-like-info-lines}

All of the ascii art that Neofetch displays happens through a single function
called `get_distro_ascii` which consists of a single **5226**-line Case statement,
each of which contains a color setting and a mangled-looking heredoc to dump to
the terminal.

This is not extensible at all.

The standard advice is to submit a pull request and add the distro/image you
want, but the maintainer seems to no longer be active. There are forks, but I
really don't want to rely on that kind of thing. As long as the original
Neofetch works, I will use it and only modify my own config file. It's just
simpler that way.

So now, we have a puzzle: How do we ~~advise~~ add to a function in Bash without
wiping out the original?


## Part 2: Adding to a function in Bash {#part-2-adding-to-a-function-in-bash}

If this were Elisp, I would just do something like

```elisp
(setq old-get_distro_ascii (symbol-function 'get_distro_ascii))
(defun get_distro_ascii ()
  (if (<test-are-we-in-emacs>)
      (<display-emacs-things>)
    (funcall old-get_distro_ascii)))
```

I _really_ didn't want to bother figuring this out in bash. So I did a web
search. And what I discovered, may shock you.

[Renaming a function](https://www.appsloveworld.com/bash/100/210/save-the-old-value-of-a-function-bash-so-that-it-can-be-called-later) is totally a thing!

So all I would have to do in Bash is

```bash
renameFunction get_distro_ascii old--get_distro_ascii
get_distro_ascii ()
if <check-if-emacs>; then
    <display-emacs-things>
else
    old--get_distro_ascii
fi
```

Now with the technical aspect out of the way, time to do Art.


## Part 3: Art {#part-3-art}

There's this really handy site
<https://www.text-image.com/convert/ascii.html>. You give it an image and a pixel
width, and it spits out ascii art of the image. Good start.

```text
@@@@@@@@@@@@@@@@@&&&&&&@@@@@@@@@@@@@@@@@
@@@@@@@@@@@&#GPYYJJJJJJY5PB#&@@@@@@@@@@@
@@@@@@@@&GYJ????????J?????JJYPB&@@@@@@@@
@@@@@@#5J?????????7!^:..  .7JJJYG&@@@@@@
@@@@&PJ????J?????7!!~~^.    JYYYY5B@@@@@
@@@&5??????^.           ...!YYYYYYYG&@@@
@@&5??????J^    .!7?JJJYYYYYYYYYYYYYG@@@
@@G????JJJJJ?^.  .~7JYYYYYYYYYYY55555#@@
@&5?JJJJJJJYYYY?~.   .:!?YYYY55555555G@@
@&Y?JJJJJY?!^..       .:~?55555555555G&@
@&5JJJJJ7.      .^!?JY555555555555555B@@
@@GJJJY?      :Y555555555555555555555#@@
@@&5JYYY~     .~7?JJYYYYY5555P555555B@@@
@@@&PJYYYY?~:.         .    .:JP555B&@@@
@@@@&GYYYY5555YYJ?7!~^:.   .^7555P#@@@@@
@@@@@@&G5YY555Y?7!!~~!7?JY5PPP5PB&@@@@@@
@@@@@@@@&BP555555555PPPP5555PG#&@@@@@@@@
@@@@@@@@@@@&#BGGPPPPPPPPGB#&@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@&&&&@@@@@@@@@@@@@@@@@@
```

(the above is the Emacs logo at
![](https://www.gnu.org/software/emacs/images/emacs.png) with a width of 40.)

This is nice, but there's two problems:

1.  `@` Background
2.  Monochrome

Let's tackle the `@` background first:

```text
                 &&&&&&
           &#GPYYJJJJJJY5PB#&
        &GYJ????????J?????JJYPB&
      #5J?????????7!*./eEe.7JJJYG&
    &PJ????J?????7!!~~^/eeeeJYYYY5B
   &5??????^.eeeeeeeeeee/^/!YYYYYYYG&
  &5??????J^eeee.!7?JJJYYYYYYYYYYYYYG
  G????JJJJJ?^\ee\~7JYYYYYYYYYYY55555#
 &5?JJJJJJJYYYY?~\eee.:!?YYYY55555555G
 &Y?JJJJJY?!^./eeeeeee.:~?55555555555G&
 &5JJJJJ7.eeeeee/^!?JY555555555555555B
  GJJJY?eeeeee:Y555555555555555555555#
  &5JYYY~eeeee\.7?JJYYYYY5555P555555B
   &PJYYYY?~:\eeeeeeeeeEeee\.:JP555B&
    &GYYYY5555YYJ?7!~\.>eee/^7555P#
      &G5YY555Y?7!.~~^7?JY5PPP5PB&
        &BP555555555PPPP5555PG#&
           &#BGGPPPPPPPPGB#&
                  &&&&
```

The `@`​s were pretty easy to get rid of. I used a _lot_ of evil-Ex commands. Also,
you may notice that the giant E has been filled in with little `e`​s.

Next, the coloring.


### How Neofetch works part 2: the coloring {#how-neofetch-works-part-2-the-coloring}

Each clause in that giant Case statement has a `set-colors x y` line above the art
itself. `x` and `y` (and posssibly more) are simply the 8/16 terminal colors.

```text
0. Black
1. Red
2. Green

etc.
```

Color 5 is magenta and color 7 is white. If we allow the "dark/light" colors
(8-15) we should probably use 13 (darkmagenta) and 15 (darkwhite), but I don't
particularly want to rely on that, and my theme's colors may be backwards so
we'll stick to 5 and 7 for now.

Neofetch embeds color escape codes in the ascii art with sequences like `${cX}`
where X is the slot after `set-colors`. Thus we can `set-colors 5 7` and then access
color 5 with `${c1}` and color 7 with `${c2}`.

Let's can put `${c1}` right before the start of the art. Then when the white E
would start we put a `${c2}`, and when each line of the E ends another `${c1}`. This
looks gross:

```text
${c1}                &&&&&&
	  &#GPYYJJJJJJY5PB#&
       &GYJ????????J?????JJYPB&
     #5J?????????7!*${c2}./eEe${c1}.7JJJYG&
   &PJ????J?????7!!~~^${c2}/eeee${c1}JYYYY5B
  &5??????^${c2}.eeeeeeeeeee/^${c1}/!YYYYYYYG&
 &5??????J^${c2}eeee.${c1}!7?JJJYYYYYYYYYYYYYG
 G????JJJJJ?^${c2}\\ee\${c1}~7JYYYYYYYYYYY55555#
&5?JJJJJJJYYYY?~${c2}\\eee.${c1}:!?YYYY55555555G
&Y?JJJJJY?!^${c2}./eeeeeee.${c1}:~?55555555555G&
&5JJJJJ7${c2}.eeeeee/${c1}^!?JY555555555555555B
 GJJJY?${c2}eeeeee${c1}:Y555555555555555555555#
 &5JYYY~${c2}eeeee\.${c1}7?JJYYYYY5555P555555B
  &PJYYYY?~:${c2}\\eeeeeeeeeEeee\.${c1}:JP555B&
   &GYYYY5555YYJ?7!~\${c2}.>eee/${c1}^7555P#
     &G5YY555Y?7!${c2}.~~^${c1}7?JY5PPP5PB&
       &BP555555555PPPP5555PG#&
	  &#BGGPPPPPPPPGB#&
		 &&&&
```

But the screenshot doesn't:

{{< figure src="https://mitchmarq42.xyz/eofetch.png" >}}


## Part 4: Putting it all together {#part-4-putting-it-all-together}

Problem: How do we tell neofetch that it's inside Emacs? Well, there is the
variable `$INSIDE_EMACS`​. The problem is how and where to use it.

I actually didn't have a perfect solution when I tooted out that screenshot. Nor
when I started this article. But I've done a minor hack and now have a solution.

In the default config file there's a line: `ascii_distro="auto"`​. Neofetch by
default auto-detects your distro from various things like `uname` and
​/​etc​/​issue. It overrides this if you pass the command-line option
`--ascii-distro`​. This all happens _before_ `get_distro_ascii` is run. Which of
course, but that also means we can't detect whether there was a command-line
override within our new `get_distro_ascii`​.

Looking at it with fresh eyes, we can clearly just check when `ascii_distro` would
get set to auto. So first replace

```bash
ascii_distro="auto"
```

with

```bash
ascii_distro=$(
    if [[ $INSIDE_EMACS ]]; then
	echo "Emacs"
    else
	echo "auto"
    fi
	    )
```

Then, below the initial `print_info()` definition but above everything else, paste
the code from [that article](https://www.appsloveworld.com/bash/100/210/save-the-old-value-of-a-function-bash-so-that-it-can-be-called-later) to rename bash functions. Then use it:

```bash
renameFunction get_distro_ascii old--get_distro_ascii
```

And now we can **finally** get to the good bit. I'm using a case statement just like
the original, since we're setting `$ascii_distro` beforehand.

```bash
get_distro_ascii() {
    case $(trim "$ascii_distro") in
	"Emacs")
	    # set_colors 13 15
	    set_colors 5 7
	    read -rd '' ascii_data <<'EOF'
${c1}                &&&&&&
	  &#GPYYJJJJJJY5PB#&
       &GYJ????????J?????JJYPB&
     #5J?????????7!*${c2}./eEe${c1}.7JJJYG&
   &PJ????J?????7!!~~^${c2}/eeee${c1}JYYYY5B
  &5??????^${c2}.eeeeeeeeeee/^${c1}/!YYYYYYYG&
 &5??????J^${c2}eeee.${c1}!7?JJJYYYYYYYYYYYYYG
 G????JJJJJ?^${c2}\\ee\${c1}~7JYYYYYYYYYYY55555#
&5?JJJJJJJYYYY?~${c2}\\eee.${c1}:!?YYYY55555555G
&Y?JJJJJY?!^${c2}./eeeeeee.${c1}:~?55555555555G&
&5JJJJJ7${c2}.eeeeee/${c1}^!?JY555555555555555B
 GJJJY?${c2}eeeeee${c1}:Y555555555555555555555#
 &5JYYY~${c2}eeeee\.${c1}7?JJYYYYY5555P555555B
  &PJYYYY?~:${c2}\\eeeeeeeeeEeee\.${c1}:JP555B&
   &GYYYY5555YYJ?7!~\${c2}.>eee/${c1}^7555P#
     &G5YY555Y?7!${c2}.~~^${c1}7?JY5PPP5PB&
       &BP555555555PPPP5555PG#&
	  &#BGGPPPPPPPPGB#&
		 &&&&
EOF
	    ;;
	*)
	    # if not in emacs just run the original which displays your distro
	    old--get_distro_ascii
    esac
}
```

Now running `neofetch` inside any Emacs terminal (again, I recommend Akib's
[EAT](https://codeberg.org/akib/emacs-eat)+Eshell), or with the variable `$INSIDE_EMACS` set, will display the custom
Emacs logo. Running neofetch outside of Emacs should still display your distro's
logo, and running it <span class="underline">inside</span> Emacs but with `--ascii_distro arch` will display the
Arch logo.
