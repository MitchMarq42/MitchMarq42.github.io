+++
title = "A better cat for Eshell"
author = ["mitchmarq42"]
date = 2022-11-19
categories = ["emacs"]
draft = false
+++

**tl;dr: Scroll to bottom**


## Part 1: Awesome syntax-highlighted `cat` {#part-1-awesome-syntax-highlighted-cat}

A few weeks ago I was wasting time on Reddit. There was a post on r/emacs
recommending [AwEshell](https://github.com/manateelazycat/aweshell). There's several features listed, many of which I don't
particularly care about-- multiple buffers (`C-u` works for me); IDE-style window
placement (I treat eshell like a tmux pane or terminal window). But one struck
my eye:

```text
13. Make cat file with syntax highlight.
```

Now that's a cool idea.

Here's the function, with some minor modifications:

```elisp
  (defun aweshell-cat-with-syntax-highlight (filename)
    "Like cat(1) but with syntax highlighting.

Taken from https://github.com/manateelazycat/aweshell/blob/d246df619573ca3f46070cc0ac82d024271ed243/aweshell.el#L775"
    (let ((existing-buffer (get-file-buffer filename))
	(buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
	   (font-lock-ensure)
	 (with-no-warnings
	   (font-lock-fontify-buffer)))
       (let ((contents (buffer-string)))
	 (remove-text-properties 0 (length contents) '(read-only nil) contents)
	 contents)))
      (unless existing-buffer
      (kill-buffer buffer)) nil))
```


## Part 2: To display an image {#part-2-to-display-an-image}


###  {#d41d8c}

There's this Linux utility [catimg](https://github.com/posva/catimg) that converts images to true-color unicode
blocks and prints them to your terminal. It's nice to just view the rough image
in the terminal, without having to open another window or -- Kamisama forbid --
use the mouse.


###  {#d41d8c}

The Kitty terminal has [a module ("Kitten")](https://sw.kovidgoyal.net/kitty/kittens/icat/) for displaying images. It works very
similarly to `catimg`, but uses Kitty's custom graphics protocol, and thus can
show pixel-perfect data, not just chunks.

When I'm in Kitty for any significant period of time, I tend to

```shell
alias catimg="kitty +icat"
```

just to remember better.


###  {#d41d8c}

I recently signed up to Mastadon -- specifically the <https://emacs.ch>
instance. It's still very new, but there's a bunch of cool people on
there. Recently Xenodium (real name Álvaro Ramírez) posted a link to [this blog](https://xenodium.com/wizard-zines-comics-eshell-util/) in which he showcases a
custom function to view a specific image from a collection.

I thought it was odd that we don't have an `eshell/catimg` command to wrap, so I
did a little googling...


###  {#d41d8c}

... And immediately arrived at [this StackExchange question](https://emacs.stackexchange.com/questions/3432/display-images-in-eshell-with-iimage-mode) about `cat`-ing files
and images with just `cat`.

The first answer is about using `iimage-mode`, which is a whole 'nother thing, and
I probably need to check it out at some point.

The second answer, the accepted one, uses Advice and a similar method to
Xenodium's to display the image. Here's the magic:

```elisp
(add-text-properties 0 (length arg)
		     `(display ,(create-image file)
			       modification-hooks
			       (iimage-modification-hook))
		     arg)
(eshell-buffered-print arg)
```

Of course, that's in the middle of a long function that's meant to be run as
advice. I don't like Advice in principle, so I kept scrolling.

The third answer went on a tangent about inserting screenshots into Markdown
files. Which is cool, but not today.

The fourth answer should have been accepted. <br />
First, there's a basic snippet for displaying images. But the answerer notes
that big images don't fit. So they provide some more thorough functions that
resize files.

For our copying convenience, here's that whole snippet but with all the functions renamed
and credited:

```elisp
  (defun esh-catimg--imagep (filename)
    "Check if FILENAME is an image. Helper for `esh-catimg--image-print'.

Taken from https://emacs.stackexchange.com/questions/3432/display-images-in-eshell-with-iimage-mode "
    (let ((extension (file-name-extension filename))
	  (image-extensions '("png" "jpg" "bmp")))
      (member extension image-extensions)))
  (defun esh-catimg--image-width (filename)
    "Get the pixel (?) width of the image FILENAME, using imagemagick. Helper
for `esh-catimg--image-print'.

Taken from https://emacs.stackexchange.com/questions/3432/display-images-in-eshell-with-iimage-mode "
    (string-to-number
     (shell-command-to-string
      (format "convert '%s' -ping -format \"%%w\" info:" filename))))
  (defun esh-catimg--rescale-image (filename)
    "Rescale an image to a maximum width, or leave untouched if already small.
Returns the new file path. Helper for `esh-catimg--image-print'.

Taken from https://emacs.stackexchange.com/questions/3432/display-images-in-eshell-with-iimage-mode "
    (let ((file (make-temp-file "resized_emacs"))
	  (max-width 350))
      (if (> (esh-catimg--image-width filename) max-width)
	  (progn
	    (shell-command-to-string
	     (format "convert -resize %dx '%s' '%s'" max-width filename file))
	    file)
	filename)))
  (defun esh-catimg--image-print (file)
    "Print the single image FILE.

Taken from https://emacs.stackexchange.com/questions/3432/display-images-in-eshell-with-iimage-mode "
    (eshell/printnl (propertize " " 'display (create-image file))))
```


## Putting it all together {#putting-it-all-together}

So, to recap: We have

1.  `aweshell-cat-with-syntax-highlighting` to dump any text file to eshell with syntax highlighting
2.  `esh-catimg--image-print` to display any image to eshell in a reasonable size

So now it's time to put it all together. Here's `eshell/cat` with most of the guts
ripped out and replaced with our helper functions.

```elisp
  (defun eshell/cat (&rest args)
    "Wrapper around `aweshell-cat-with-syntax-highlight' for multiple ARGS.

Also, can cat images for some reason.

See:
https://github.com/manateelazycat/aweshell/blob/d246df619573ca3f46070cc0ac82d024271ed243/aweshell.el#L775
https://emacs.stackexchange.com/questions/3432/display-images-in-eshell-with-iimage-mode "
    (setq args (eshell-stringify-list (flatten-tree args)))
    (dolist (file args)
      (if (string= file "-")
	(throw 'eshell-external
	       (eshell-external-command "cat" args))
      (if (esh-catimg--imagep file)
	  (esh-catimg--image-print (esh-catimg--rescale-image file))
	(aweshell-cat-with-syntax-highlight file)))))
```

It iterates over each argument and applies the correct function.
