---
layout: post
title:  "Create a simple Windows installer for your Haskell project"
date:   2020-10-30 00:00:00 +0100
categories: haskell
---

In this blog post, I'll try to explain how you can create a really simple installer for your Haskell software on Windows and how to deal with external dependencies.

# Introduction

Alright, let's get started. We are going to build a simple installer. It will install a desktop shortcut as well as a link shortcut in the start menu.
We will be using [Stack](https://docs.haskellstack.org/en/stable/README/) to build our Haskell project and [NSIS](https://nsis.sourceforge.io/Main_Page) to write the installer.

> Note: GHC 8.8.[2\|3\|4] as well as 8.10.[1\|2] are broken on windows so while we wait for 8.10.3 to fix everything, we will have to use 8.6.5 for now.

Distributing software with external dependencies is slightly more complicated but not uncommon. We will make our project use [postgresql-simple](https://github.com/lpsmith/postgresql-simple/) so we can learn how to deal with dependencies :books:

I will assume you have Postgresql, Stack, and NSIS installed already! Also, I'll be using Powershell as a terminal.

# Project creation

So let's start by creating a simple dummy project: `stack new hello-world-app`

As said before we want to use GHC 8.6.5 so let's fix the resolver in `stack.yml`:

```yml
resolver: lts-14.27 # use ghc 8.6.5

packages:
- .
```

We also want to use postgresql-simple so let's add a dependency in `package.yml`:

```yml
dependencies:
- base >= 4.7 && < 5
- postgresql-simple

...
```

Finally, we make our software use only one function which query our postgres DB just to make sure it's running:

```haskell
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( isPostgresRunning
    ) where

import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

isPostgresRunning :: IO ()
isPostgresRunning = do
  password <- getLine
  connection <- PG.connect PG.defaultConnectInfo { PG.connectDatabase = ""
                                                 , PG.connectUser = "postgres"
                                                 , PG.connectPort = 5432
                                                 , PG.connectPassword = password
                                                 }
  _ :: [PG.Only Int] <- PG.query_ connection [sql| SELECT 1 |]
  return ()

```

Alright, next we build it with `stack build` and it should... not work...
Indeed, stack will fail something like:
```
<command line>: can't load .so/.DLL for: C:/PROGRA~1/POSTGR~1/13/lib\libpq.dll (addDLL: C:\PROGRA~1\POSTGR~1\13\lib\libpq or dependencies not loaded. (Win32 er)

--  While building package hello-world-app-0.0.0 (scroll up to its section to see the error) using:
      C:\sr\setup-exe-cache\x86_64-windows\Cabal-simple_Z6RU0evB_2.4.0.1_ghc-8.6.5.exe --builddir=.stack-work\dist\e626a42b build lib:hello-world-app exe:hello-world-app-exe --ghc-options " -fdiagnostics-color=always"
    Process exited with code: ExitFailure 1
```

To build our project, stack is telling us that it needs `libpq.dll`. So let's add the missing dependencies path to our path.
with Powershell you can use this command: `$env:Path += ";C:\Program Files\PostgreSQL\13\bin"`. It will temporarily modify the path variable for the current terminal.

> Note: You can use `$env:path.split(";")` to print the current value for the PATH variable.

Ok, now it should build without errors! We can try to execute it as well with: `stack exec hello-world-app-exe` and... tadaaa :tada:, our project is running! We can save our work and call it a day? :blush:

Well, not really no. We still need to build our installer...

# NSIS

I picked NSIS but you might want to investigate other tools as well ([Inno Setup](https://jrsoftware.org/isinfo.php) to name one). NSIS documentation felt nice and the fact that it came with examples, tutorials and a simple compiler convinced me into using it.

The idea behind an installer is that you write a script that will explain how to install software. The syntax is quite low level but it's not that hard either.

So let's write a simple installer in `installer.nsi`:

```ruby
# we start by defining variables
!define APPNAME "Hello World"
!define COMPANYNAME "BestCompanyEver"

!define EXECUTABLE_NAME "hello-world-app-exe.exe"
!define ICON_NAME "icon.ico"

!define VERSIONMAJOR 1
!define VERSIONMINOR 0
!define VERSIONPATCH 0

# create a directory where we will put our assets (eg: image, executable, uninstaller, dependencies...)
InstallDir "$PROGRAMFILES\${COMPANYNAME}\${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONPATCH}" #(ie: C:\Program Files\BestCompanyEver\1.0.0\)

# Define the installer name
outFile "hello-world-app-installer.exe"

section "install"
	setOutPath $INSTDIR

    # copy the executable in the installation directory
    file ${EXECUTABLE_NAME}

	# create a start menu shortcut
	createShortCut "$SMPROGRAMS\${COMPANYNAME}\${APPNAME}.lnk" "$INSTDIR\${EXECUTABLE_NAME}" "" "$INSTDIR\${ICON_NAME}"
	# create a desktop shortcut
    createShortCut "$DESKTOP\${APPNAME}.lnk" "$INSTDIR\${EXECUTABLE_NAME}" "" "$INSTDIR\${ICON_NAME}"
sectionEnd
```

Quite concise indeed! We only need to compile it now. We can use the NSIS compiler that will create a `hello-world-app-installer.exe`.
We can now run the installer and check that our shortcut has been created in the Desktop folder.

Is it done now?

:heavy_check_mark: shortcuts have been created<br/>
:heavy_check_mark: Installation directory has been created and our executable has been paste inside it<br/>
:x: It doesn't run<br/>

Indeed, if you try to run the executable, a window should popup with a message like `The code execution cannot proceed because LIBPQ.dll was not found. Reinstalling the program may fix the problem.`. Which means it's time to dive into the world of DLLs.

But wait something's strange. When we executed our program earlier, it was working fine. So what happened meanwhile?<br/>
Well, the reason it worked before was that we prefixed the command with `stack exec` which made sure the executable would find its required dependencies.

We can verify this quite easily.
Let's copy our executable in a place we can access more easily first: `stack install`. <br/>
For me it copied the executable here: `C:\Users\iori\AppData\Roaming\local\bin`.<br/>

Let's cd inside this folder and run our executable without prefixing it with `stack exec`. We should see the same error message popping up.

Time to fix this by making sure our executable knows where to find the DLLs it depends on.

# DLL

`DLL` (dynamic link library) is our runtime dependencies. In our case, because we use `postgresql-simple`, our project requires some postgresql DLLs that we need to find.

## Finding the required DLL

Many tools can help you list dependencies on windows like [Dependencies](https://github.com/lucasg/Dependencies) or even `ldd` if you have [cygwin](https://cygwin.com/) installed.
I chose to go with [Process explorer](https://docs.microsoft.com/en-us/sysinternals/downloads/process-explorer) which can show the dependency of a running process.

So let's run our project with: `stack exec hello-world-app-exe`
Then we should find our instance in Process explorer.

![test](/assets/img/process-explorer.png)

By clicking **View** -> **DLL**, we can list our program DLLs. We don't need to take care of the DLLs that are in the `C:\Windows\System32`.
On the other hand, we see that our executable requires 5 postgresql DLL: *libpq*, *libcrypto*, *libiconv*, *libintl* and *libssl*.
Those are the DLLs we want for our program. So we can simply copy them into the root folder of our executable and now, running our program without prefixing with `stack exec` should work fine! :fireworks:

So are we done now? Not quite so, we still need to update the installer to take the DLLs into account.

## Copying DLLs with NSIS

We could copy them one by one with `File myDll.dll` but because we might have a lot of them, let's just put all our assets into a folder and copy that folder:

```nsi
section "install"
	setOutPath $INSTDIR

    file "assets\"

    ...
sectionEnd
```

We can then, put our DLLs, icon, and executable into the assets/ folder and Now we are truly done! :sparkler:

## Really? Is this the end?

Well, this post was intended to be simple but actually, you shouldn't stop here. You should add to your installer an uninstaller that will take care of removing all the folders/files, shortcuts, and whatnot.

I invite you to read NSIS documentation for that :blue_book:

This example is available in the repo [patchgirl/windows-haskell-installer-example](https://github.com/patchgirl/windows-haskell-installer-example)

# Last word?

For a simple case like this one, writing an installer isn't too hard. If you don't need shortcuts/icons and would satisfy with a simple executable that you can run from a terminal, you might want to investigate:
- [file-embed](https://hackage.haskell.org/package/file-embed) to embed any files (picture, documentation,...) within your executable
- [Vrom911 blog post](https://vrom911.github.io/blog/github-actions-releases) to generate executable from Github Action for MacOS/Windows/Linux
- [Haskell.nix](https://input-output-hk.github.io/haskell.nix/) to cross-compile your Haskell project with Nix

I hope this article helps you write more Haskell on Windows!!

:cactus:
