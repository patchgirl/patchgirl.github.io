---
layout: post
title:  "Building static Haskell binary"
date:   2020-06-25 00:00:00 +0100
categories: haskell
---

In this post, I'll try to explain what are dynamic libraries and static executable. I'll also show how to create the latter with Nix on Linux.

# Introduction

PatchGirl is a rest client that works directly in your browser. But because browsers save users from security issues (i.e: [CORS](https://developer.mozilla.org/docs/Web/HTTP/CORS), [same origin policy](https://developer.mozilla.org/docs/Web/Security/Same_origin_policy_for_JavaScript)), some features couldn't be implemented in a web app.<br/>
So I created the **patchgirl-runner** app which is an executable that runs on the user computer and overcome those limitations.

The complete project looks like this:

![test](/assets/patchgirl-diagram.svg)

I wanted Patchgirl-runner to be easy to use. Ideally, you would just have to download it and run it. But because it is written in Haskell, it was natively compiled to a dynamic executable.

# Dynamic libraries

By default, when you compile your Haskell program to an executable it will require dynamic libraries to work. This means that your executable cannot work alone.

## Visualizing dynamic libraries

Let's take a simple example to explain how it works.
Let's create a basic project:<br/>
`stack new HelloWorld`<br/>

This project has 2 files:

```haskell
-- src/lib.hs

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```
```haskell
-- app/Main.hs

module Main where

import Lib

main :: IO ()
main = someFunc
```

Now let's build our project:
```bash
stack build
stack install # copy the generated executable to a folder in your $PATH (e.g: ~/.local/bin/HelloWorld-exe)
```

This executable is not a standalone binary, meaning that you can't just copy it to another computer and expect it to work. Indeed, it requires dynamic libraries. <br/>
If we want to show this executable's dependencies we can run the command `ldd` which given its manual *"print shared object dependencies"* (you can replace *shared object* by *dynamic library*).

So let's run it:

```shell
% ldd HelloWorld-exe
    linux-vdso.so.1 (0x00007ffc3fdba000)
    libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f419e709000)
    libgmp.so.10 => /lib/x86_64-linux-gnu/libgmp.so.10 (0x00007f419e688000)
    librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f419e67d000)
    libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f419e677000)
    libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f419e654000)
    libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f419e463000)
    /lib64/ld-linux-x86-64.so.2 (0x00007f419e871000)
```

This means that this executable requires `libm.so.6`, `libgmp.so.10`, `libc.so.6`,...
Those are dynamic libraries. <br/>
One way to tell whether a library is dynamic is the extension `.so` (i.e **s**hared **o**bject).

When you run your executable, these libraries will also be loaded and accessible to your program.
I'm not going to describe them all but to in a nutshell, `libm` provides mathematic functions like `abs`, `div` or `cos`...<br/>
`libgmp` provides arbitrary precision arithmetic, operating on signed integers, rational numbers, and floating-point numbers...
This libraries are part of a more global library `glibc` that was splitted.

## Dynamic libraries pros and cons

Dynamic libraries have some advantages. One of them is the executable size. <br/>
These libraries are shared by all executables which need them.
That means you can have lightweight executables because they doesn't include libraries.

An other nice advantage is maintainability. If many programs depends on a library with security issues or bugs, you will only need to upgrade the culprit library to fix them all.

On the other hand, you cannot distribute your executable easily to your customer. If you copy the executable on another computer, it will most likely fail to run because the dynamic libraries it requires are not present.

Which brings us to static libraries.

# Building a standalone executable

Dynamic libraries are not great when it comes to make application usage/installation easy. This is even more true on Linux distributions where each distribution has it own way of packaging a software (e.g: *dpgk*, *rpm*, *yum*, *snap*, *flatpak*...)<br/>
If we want to provide a standalone executable to simplify the developer and the customers' life, we should generate a static executable instead.

## Static executable

nb: When I refer to a **static executable**, I mean an executable which doesnt require dynamic libraries.

If we want to provide an executable without dependency, we'd rather make it completely static (i.e: running `ldd` on it should return nothing). One way of doing this is to tweak Cabal/Stack/whatever building tool you are using and set it up to build static binary.
But we unfortunately can't just stop here. Indeed, even if you build a static executable with this solution, you might not be able to ship your binary to another platform.

The reason is that your static binary will have been compiled against a specific version of glibc which might not be the same on your the targeted computer. That means that the API your executable is going to use could be incompatible with the kernel.

So can we overcome this issue ? On GNU/linux operating systems, we can thanks to musl.

## Musl

>[musl](https://musl.libc.org/) is an implementation of the C standard library built on top of the Linux system call API, including interfaces defined in the base language standard, POSIX, and widely agreed-upon extensions. musl is lightweight, fast, simple, free, and strives to be correct in the sense of standards-conformance and safety.

In a nutshell, musl is another implementation of the libc. It has the nice advantage of providing a single API so whatever program compiled statically against musl should theorically work on any GNU/Linux platforms.

Cool, so musl looks like a great solution! How do we use it in our project. Well GHC is traditionally compiled against glibc so every time you compile with GHC, it will make it glibc dependent... The solution is to compile GHC with musl!

This looks like a difficult job, Fortunately **@nh2** has already done the job with [static-haskell-nix](https://github.com/nh2/static-haskell-nix)

# static-haskell-nix

Static-haskell-nix's purpose is to build fully static haskell executables for linux. It uses a lot of Nix machinery so it might not be super easy for beginners.

It provides multiple solutions to generate your executable. The easiest one is to use [stack](https://github.com/nh2/static-haskell-nix#building-stack-projects) but I won't describe it. I tried it and [failed](https://github.com/nh2/static-haskell-nix/issues/95) because of some incompatibility with recent version of stack.

Instead, we are going to write some Nix code to use with static-haskell-nix.

# Building a fully static haskell executable

## Requirements

Alright, here is the requirements:
- simple project that uses a recent version of stack
- our project should be split in 2 packages, the library and the executable
- the executable package should depend on the library
- our project should use postgresql-simple (meaning we will have to generate an executable that embed the **libpq** library)

## Project architecture

Ok, so just like before let's generate a simple stack project by running:<br/>
`stack new HelloWorld`

Let's modify our `stack.yaml` so we have 2 packages and a recent resolver version:

```yaml
# stack.yaml

resolver: lts-15.13
packages:
- hello-world-lib/
- hello-world-app/
```

Let's create both packages's `package.yaml` file:
```yaml
#  hello-world-lib/package.yml

name: hello-world-lib

library:
  source-dirs:
    - src

dependencies:
  - base
  - postgresql-simple
```

```yaml
#  hello-world-app/package.yml

name: hello-world-app

executables:
  hello-world-app-exe:
    main: app/Main.hs
    dependencies:
    - hello-world-lib

dependencies:
  - base
```

So that was the easy part. We can check that everything works by running `stack build`.
We can also check that the executable generated isn't static by running:
```shell
stack install
ldd ~/.local/bin/hello-world-app-exe

	linux-vdso.so.1 (0x00007ffed50fb000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007fd92c9ce000)
	libpq.so.5 => /lib/x86_64-linux-gnu/libpq.so.5 (0x00007fd92c982000)
	librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007fd92c977000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fd92c6d5000)
	libssl.so.1.1 => /lib/x86_64-linux-gnu/libssl.so.1.1 (0x00007fd92c643000)
	libcrypto.so.1.1 => /lib/x86_64-linux-gnu/libcrypto.so.1.1 (0x00007fd92c36e000)
	libkrb5support.so.0 => /lib/x86_64-linux-gnu/libkrb5support.so.0 (0x00007fd92c1a5000)
    ... truncated for brevity
```

The output shows that adding `postgresql-simple` as a dependency added other dynamic libraries like `libpq`.
This executable works fine but we want it to be fully static. It's time to play with Nix and `static-haskell-nix`!

## Static build script with Nix

As we said before, we are not going to use the `stack` part of static-haskell-nix. Instead we are relying on the generated Cabal files (i.e: `hello-world-lib.cabal` and `hello-world-app.cabal`) from our Nix Script.

Our build script was inspired a lot by postgrest [build script](https://github.com/PostgREST/postgrest).

Our program will have two main scripts.

*default.nix* will pin nixpkgs and define where are our packages:

```nix
# default.nix

let
  # We are using lts-15.13 stack resolver which uses ghc883 (cf: https://www.stackage.org/lts-15.13)
  compiler = "ghc883";

  # pin nixpkgs for reproducible build
  nixpkgsVersion = import nix/nixpkgs-version.nix;
  nixpkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  # overlays define packages we need to build our project
  allOverlays = import nix/overlays;
  overlays = [
    allOverlays.gitignore # helper to use gitignoreSource
    (allOverlays.haskell-packages { inherit compiler; })
  ];

  pkgs = import nixpkgs { inherit overlays; };

  # We define our packages by giving them names and a list of source files
  hello-world-lib = {
    name = "hello-world-lib";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./hello-world-lib)[ ".cabal" ".hs" ".lhs" "LICENSE" ];
  };
  hello-world-app = {
    name = "hello-world-app";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./hello-world-app)[ ".cabal" ".hs" ".lhs" "LICENSE" ];
  };

  # Some patches are unfortunately necessary to work with libpq
  patches = pkgs.callPackage nix/patches {};

  lib = pkgs.haskell.lib;

  # call our script which add our packages to nh2/static-haskell-nix project
  staticHaskellPackage = import nix/static-haskell-package.nix { inherit nixpkgs compiler patches allOverlays; } hello-world-lib hello-world-app;
in
rec {
  inherit nixpkgs pkgs;

  hello-world-app-static = lib.justStaticExecutables (lib.dontCheck staticHaskellPackage.hello-world-app);
}
```

*static-haskell-package.nix* will define how to add our haskell packages to the *static-haskell-nix* build script.

```nix
# nix/static-haskell-package.nix

# Derive a fully static Haskell package based on musl instead of glibc.
{ nixpkgs, compiler, patches, allOverlays }:

# this file returns a function that takes in parameter the 2 package sources we want to build
hello-world-lib: hello-world-app:
let
  # pin nh2/static-haskell-nix project with a commit revision
  # this make sure we will always use the same version of the nh2/static-haskell-nix
  static-haskell-nix =
    let
      rev = "749707fc90b781c3e653e67917a7d571fe82ae7b";
    in
    builtins.fetchTarball {
      url = "https://github.com/nh2/static-haskell-nix/archive/${rev}.tar.gz";
      sha256 = "155spda2lww378bhx68w6dxwqd5y6s9kin3qbgl2m23r3vmk3m3w";
    };

  # package that deals with posgresql needs a little patch from
  # within nh2/patched-static-haskell-nix script
  patched-static-haskell-nix = patches.applyPatches
    "patched-static-haskell-nix"
    static-haskell-nix
    [
      patches.static-haskell-nix-patchgirl-openssl-linking-fix
    ];

  # Fix taken from https://github.com/PostgREST/postgrest/blob/43d71e95ac091aa77ac104de7fc881226d1a17f6/nix/static-haskell-package.nix
  # I'm not too sure if there are really needed for this simple project
  patchedNixpkgs = patches.applyPatches
    "patched-nixpkgs"
    nixpkgs
    [
      patches.nixpkgs-revert-ghc-bootstrap
      patches.nixpkgs-openssl-split-runtime-dependencies-of-static-builds
    ];

  lib = (import nixpkgs {}).haskell.lib;

  # We are defining our package by calling callCabal2nix on the `package.yml` generated by stack
  extraOverrides = final: prev:
    rec {
      "${hello-world-lib.name}" = lib.dontCheck (prev.callCabal2nix "${hello-world-lib.name}" hello-world-lib.src {});
      "${hello-world-app.name}" = prev.callCabal2nix "${hello-world-app.name}" hello-world-app.src {};
    };

  # We make sure our package will be integrated in the nh2/patched-static-haskell-nix project
  overlays = [
    (allOverlays.haskell-packages { inherit compiler extraOverrides; })
  ];

  normalPkgs = import patchedNixpkgs { inherit overlays; };

  # each version of GHC needs a specific version of Cabal.
  defaultCabalPackageVersionComingWithGhc = { ghc883 = "Cabal_3_2_0_0"; }."${compiler}";

  # The static-haskell-nix 'survey' derives a full static set of Haskell
  # packages, applying fixes where necessary.
  survey = import "${patched-static-haskell-nix}/survey" { inherit normalPkgs compiler defaultCabalPackageVersionComingWithGhc; };
in
{
  "${hello-world-lib.name}" = survey.haskellPackages."${hello-world-lib.name}";
  "${hello-world-app.name}" = survey.haskellPackages."${hello-world-app.name}";
}
```

For most case these should suffice. <br/>
If you deal with libpq, you are likely to run into [another issue](https://github.com/nh2/static-haskell-nix/issues/57). Because we use *postgresql-simple* we will need libpq.

So In order to fix it, we want to apply a patch. The patch is easy and solely consists in adding:

```nix
hello-world-lib =
  addStaticLinkerFlagsWithPkgconfig
    super.hello-world-lib
    [ final.openssl final.postgresql ]
    "--libs libpq";

hello-world-app =
  addStaticLinkerFlagsWithPkgconfig
    super.hello-world-app
    [ final.openssl final.postgresql ]
    "--libs libpq";
```

But this fix needs to be done to the nh2/static-haskell-nix build script.<br/>
So how can we edit it without polluting it with our dumb app? One solution is to fork it locally, edit it and save our edition to a patch file.
This is what we are going to do.

```shell
git clone https://github.com/nh2/static-haskell-nix
cd static-haskell-nix/survey/
```

let's edit `survey/default.nix` and add to our packages the static library openssl and postgresql like above.
Finally, let's save the diff in a patch file:<br/>
`git diff survey/default.nix > /home/$USER/HelloWorld/nix/patches/hello-world-openssl-linking-fix.patch`

Alright, we are done. We can now just build our static executable:<br/>
`nix-build -A hello-world-app-static`

Please note that the first time you run this command, static-haskell-nix will build GHC against libmusl. It took roughly 8h to complete this task on my computer. Once it's been compiled, it will be stored to your **/nix/store** and you won't have to go through this computation again!

Let's check it actually work:
```bash
ldd /nix/store/d191d3fwzhxi10qz0qbxlqisk86fqvlg-hello-world-app-0.0.0/bin/hello-world-app-exe
    not a dynamic executable
```
Yay, it worked!! This executable is fully static and can be run to any Linux platforms

This complete sample project is available on [this repo](https://github.com/patchgirl/static-haskell-binary-example).

# Conclusion

This solution was used to build the patchgirl-runner. You can check the full build script on [this repo](github.com/patchgirl/patchgirl).
patchgirl-runner can be [downloaded here](https://github.com/patchgirl/patchgirl/releases) and used on any X86_64 linux OS :penguin:

## Other platforms

Nix isn't supported by Windows platform so this solution can't be used.
For MacOS, it seems this field is quite recent and even if simple [examples](https://github.com/nh2/static-haskell-nix/issues/64#issuecomment-548846994) works, real world application won't probably be trivial to generate.

I want patchgirl-runner to be available on MacOS and Windows in a near future so I hope I'll find a solution and write a blog post about it :book:

## Last words

static-haskell-nix is a huge work that simplifies greatly the haskell static binary generation. It's a bit hard to grasp if you're not well versed into nix and haskell but I hope this article helped you understanding how to integrate your project with it.

ps: some resources were quite helpful to understand haskell static executable with nix

- [vaibhavsagar blog](https://vaibhavsagar.com/blog/2018/01/03/static-haskell-nix/)
- [postgrest repo](https://github.com/PostgREST/postgrest) which was the inspiration for our building script.


:cactus:
