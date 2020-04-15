---
layout: post
title:  "Building a web app with functional programming - Haskell - part III"
date:   2020-03-31 00:00:00 +0100
categories: haskell
---

{% include building-a-web-app-with-functional-programming-summary.md %}

It's time for the 3rd and last Haskell part of this series. This article will focus on the tooling I used to build Patchgirl.

# Reproducible build

There are multiple way to do reproducible builds with Haskell. The most mainstream ones are probably Stack and Nix.
When I started Patchgirl I didn't know which one to pick so I gave both a try. Nix seemed powerful yet much more complexed because you need to learn a brand new language to use it. On the other hand Stack felt dead simple so I quickly went with it.

## Stack

When you need to third parties librairies, using only Cabal can quickly become a nightmare. Stack is the easiest solution I found to handle dependencies. It's a great piece of sofware that works really fine. Moreover, it has a well detailed [guide](https://docs.haskellstack.org/en/stable/README/).

Stack works on top of Cabal but doesn't replace it. Yet, I started Patchgirl with stack and haven't even once felt the need to check my cabal file as stack handles everything for me.

One of the feature I really like about Stack is the error message it provides. When facing with an error, Stack is human readable and sometimes give you a solution that usually works. Productivity wise it's quite efficient.

## production ready ?

Great guide, easy to use, super useful. :100:% Yes!!

## Conclusion

Stack was a great choice that I do not regret. Nonetheless, because I'm running Patchgirl on NixOS, Nix feels like a more adapted choice now. Even though, stack fulfil my needs, I'll probably try to replace it with Nix.

# Compilation

To compile while developing, I initially used Stack. When you have few files it works fine but will quickly show its limit. Compiling Haskell code takes a huge amount of time. It was cumbersome so I looked for a workaround.<br/>
I began using emacs [haskell-mode](https://github.com/haskell/haskell-mode) that amongst many [other things](http://haskell.github.io/haskell-mode/manual/latest/) provides you the ability to compile the current file. This was really fast (compiling a file would take roughly 2, 3s) and I could have stopped here but accidentally stumbled on Ghcid.

## Ghcid

[Ghcid](https://github.com/ndmitchell/ghcid) is Ghci as a daemon. Meaning that it can compile your code at the speed of ghci.
In a nutshell, Ghcid is a daemon that watches for files changes and automatically recompile your whole project. It's dead simple but so efficient! After installing it, it has become my de facto standard compilation tool.

I can't emphasize enough how great this tool is, especially for fast development feedback. I use it a lot to run my entire test suits on every file modifications.

Ghcid is just simply amazing, use and overuse it!

# Linter

If like me you like clean/efficient code, then you might like the idea of having a linter. I use HLint which seems to be the standard nowadays and I'm quite happy with it.


# Style?

It's important to me when switching from a code to another code to have the same consistency.

- ormolu
- hfmt
- haskell-formatter
- brittany
- stylish-haskell
- hindent

## Production ready?

## last word?


:cactus:
