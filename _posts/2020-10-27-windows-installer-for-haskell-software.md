---
layout: post
title:  "Create a simple windows installer for your haskell project"
date:   2020-10-27 00:00:00 +0100
categories: haskell
published: false
---

I haven't had the chance to write anything for sometime now. Time to fix this!

In this blog post, I'll try to explain how you can create a really simple installer for your haskell software.

# Introduction

Disclaimer: I'm not a windows expert, I've spent most of my career on either MacOS or Linux so there might be better ways to achieve this.

Alright, let's get started. We are going to build a simple installer. It will install a desktop shortcut as well as a link shortcut in the start menu.
We will be using stack to build our Haskell project and [NSIS](https://nsis.sourceforge.io/Main_Page) to write the installer.

> Note: GHC 8.8.[2\|3\|4] as well as 8.10.[1\|2] are broken on windows so while we wait for 8.10.3 to fix everything, we will have to use 8.6.5 for now.

Last but not least, we will make our project use [postgresql-simple](https://github.com/lpsmith/postgresql-simple/). Postgresql-simple makes our project require external dependencies. This is going to be interesting to see how we can deal with them.

I will assume you have both postgresql and stack installed already!

# Project creation

So let's start by creating a simple dummy project: `stack new hello-world-app`

As said before we want to use GHC 8.6.5 so let's fix the resolver in `stack.yml`:

```yml
resolver: lts-14.27

packages:
- .
```

We also want to use postgresql-simple so let's add a dependency in `package.yml`:

```yml
dependencies:
- base >= 4.7 && < 5
- postgresql-simple
```

Alright, next we build it with `stack build` and Tadaaa :tada:, our project is done! We can save our work and call it a day :blush:

Well, not really no. We still need to build our installer...

# NSIS

I picked NSIS but you might want to investigate other tools ([Inno Setup](https://jrsoftware.org/isinfo.php) to name one) as well. NSIS documentation felt a bit nicer and the fact that it came with examples, tutorials and a simple compiler convinced me into using it.

The idea behind an installer is that you write a script that will explain how to install software. The syntax is quite low level but it's not that hard either.

So let's write a simple installer in `installer.nsi`:

```nsi
# we start by defining variables
!define APPNAME "Hello World"
!define COMPANYNAME "BestCompanyEver"

!define EXECUTABLE_NAME "hello-world-app-exe.exe"
!define ICON_NAME "icon.ico"

!define VERSIONMAJOR 1
!define VERSIONMINOR 0
!define VERSIONPATCH 0

# create a directory where our assets will lay (eg: image, executable, uninstaller, dependencies...)
InstallDir "$PROGRAMFILES\${COMPANYNAME}\${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONPATCH}" (ie: C:\Program Files\BestCompanyEver\1.0.0\)

# Define the installer name
outFile "hello-world-app-installer.exe"

section "install"
	setOutPath $INSTDIR

    # copy the executable in the installation directory
    file EXECUTABLE_NAME

	# create a start menu shortcut
	createShortCut "$SMPROGRAMS\${COMPANYNAME}\${APPNAME}.lnk" "$INSTDIR\${EXECUTABLE_NAME}" "" "$INSTDIR\${ICON_NAME}"
	# create a desktop shortcut
    createShortCut "$DESKTOP\${APPNAME}.lnk" "$INSTDIR\${EXECUTABLE_NAME}" "" "$INSTDIR\${ICON_NAME}"
sectionEnd
```

Quite concise for short examples! We only need to compile it now. We can use the NSIS compiler that will create a `hello-world-app-installer.exe` that we can run to make sure everything works.

Is it done ?
:heavy_check_mark: shortcuts have been created
:heavy_check_mark: Installation directory has been created and our executable has been copied in it
:x: It doesn't run

Indeed, if you try to run the executable, a window should popup with a message like `LibPQ.dll could not be found`. Which means it's time to dive into the world of DLLs.

# DLL

`DLL` (dynamic link library) are our runtime dependencies. In our case, because we use `postgresql-simple`, we need some DLL to run our app.
One simple way to find if your executable is going to require any DLL is to simply run the executable. In our case, if we run our executable without prefixing it with `stack exec`, we get something like:

```
import_dll Library LIBPQ.dll (which is needed by "C:\hello-world-app.exe") not found
```

What we need to do is first to find which DLL are required by our awesome project.

## Finding the required DLL

Many tools can help you list dependencies on windows like [Dependencies](https://github.com/lucasg/Dependencies) or even ldd (if you are more familiar with Linux) if you use [cygwin](https://cygwin.com/).
I have found that XXX is particulary nice as it list the dependencies and there location of a running executable.

So let's run our project with: `stack exec hello-world-app-exe`
Then we should find our instance in XXX.

`image`

By clicking **View** -> **DLL**, we can list our program DLLs and find that only 5 of them are not stored in the xxx folder.
Those are the DLL we want for our program. So we can simply copy them into the root of our executable and now, running our program without prefixing with `stack exec` should work fine :fireworks: !

So are we done now ? Not quite so, we still need to update the installer to take the DLL into account.

## Copying DLLs with NSIS

We could copy them one by one with `File myDll.dll` but because we might have a lot of them, let's just put all our assets into a folder and copy that folder:

```nsi
section "install"
	setOutPath $INSTDIR

    file "assets\"

    ...
sectionEnd
```

We can then, put our DLLs, icon and executable into the assets/ folder and Now we are truly done! :sparkler:

# Really? Is this the end?

Well this post was intended to be simple but actually you shouldn't stop there. You should write add to your installer an uninstaller which will take care of removing all the folders/files, shortcuts and whatnot.

I invite you to read NSIS documentation for that :blue_book:

## last word?

For simple case like this one, writting an installer isn't hard for any Haskell project. I hope this article helps you write more Haskell on Windows!!
I for one, am going back on Linux :penguin:

:cactus:
