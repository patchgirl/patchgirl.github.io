---
title:  "Testing Haskell code with Stack, Ghcid and Hspec"
date:   2020-07-21 00:00:00 +0100
categories: haskell
---

The bigger [PatchGirl](github.com/patchgirl/patchgirl) code base was getting the more tedious testing was. So, every once in a while, I looked for ways to improve my developer testing experience.<br/> I believe I have reached a satisfying setup but before I get to it, I'll try to show how you can test haskell code for smaller projects.

# Testing small project with Stack and Hspec

[Hspec](https://hspec.github.io/) is my favorite testing suite as it is quite simple, well documented, and beginner friendly.

For small projects, if you use Hspec, you can go with the bare `stack test` or `stack test --file-watch`.

Stack will fully rebuild your changes every time so, even if the compilation is incremental, it's still a bit slow.

If you already have few tests and it takes more than 5s to run them, then you can already improve your developer experience by creating a `.hspec` file at the root of your project and fill it with:

```bash
--fail-fast # stop testing as soon as one of the tests fails
--failure-report .hspec-failures # save all failures in .hspec-failures, this is needed for below
--rerun # rerun all examples that failed on the previous run
--rerun-all-on-success # this is explicit enough
```

For most small projects this will be more than enough and will probably make your development experience nicer. Nonetheless, when your project reaches a bigger size, the stack + hspec combination might not satisfy you enough. This is where Ghcid enters the ring.

# Testing large project with Ghcid

For bigger projects, we can significantly improve our experience by using more advanced solutions.<br/>
[Ghcid](https://github.com/ndmitchell/ghcid) is one of my favorite tools and I use it to run my test as it is the fastest solution I stumbled upon.

This is how I use it:

`ghcid --command 'stack ghci your-package --test --main-is your-package:test:spec' --test 'main' --warnings`

Ghcid will recompile your changes much faster than stack. If in addition of Ghcid, you configure hspec (cf above), then you get a great developer experience for testing.<br/>

# More optimizations

Given that you use the Ghcid + Hspec combination, you still need to run your whole suite at least once before hspec can focus on failing tests.
If your project is important, running all the tests takes time and if none of your tests fail, then you're good to rerun them all on the next change...

Most of the time, you will work on a specific module of your code that has its own spec file. Instead of running all the tests, you'd rather focus on the given spec module and not waste time on the others.<br/>
So how do we deal with this? It turns out hspec still has one ace up its sleeve.<br/>
Indeed, you can ask hspec to [focus](http://hackage.haskell.org/package/hspec-2.7.1/docs/Test-Hspec.html#g:5) on a specific spec or set of specs.

It's quite clever and dead simple to use. Instead of using `describe` or `it`, you can use `fdescribe`, `fit`, and `focus`. But enough explanations, show me how it works!

```haskell
    -- common way of writing test
    describe "Bool" $ do
      it "works with True" $
        True `shouldBe` True

      it "works with False" $
        False `shouldBe` False

      it "works with True and False" $
        True `shouldBe` not False


    -- let's focus on the first spec
    describe "Bool" $ do
      fit "works with True" $ -- only this spec will be tested
        True `shouldBe` True

      it "works with False" $
        False `shouldBe` False

      it "works with True and False" $
        True `shouldBe` not False


    -- focus on the first and last spec
    describe "Bool" $ do
      fit "works with True" $ -- will be tested
        True `shouldBe` True

      it "works with False" $
        False `shouldBe` False

      fit "works with True and False" $ -- will also be tested
        True `shouldBe` not False



    -- focus on all Bool tests
    fdescribe "Bool" $ do -- everything here will be tested
      fit "works with True" $
        True `shouldBe` True

      it "works with False" $
        False `shouldBe` False

      fit "works with True and False" $
        True `shouldBe` not False


    -- focus on all the tests defined here:
    focus $ -- everything under will be tested
      describe "Bool" $ do
        ...
      describe "Integer" $ do
        ...

```

Ok, I think we are all set now. Compilation is super fast thanks to Ghcid and we can focus on a specific set of specs thanks to hspec.

I hope you'll have a great developer experience testing with this combination!

# Bonus

Some of you won't feel too comfortable using hspec focus feature. Indeed, what if you forget to remove a `fit` somewhere in your code. Then your CI will only run the tests on the focused part which can cause a great deal of trouble.

This is an issue that can be solved in many ways. I tend to use git hooks to make sure my commits are sanitized.
This is a pre commit hook that will check that the words `fit`, `focus`, and `fdescribe` are not present in your tests.

```
#!/bin/bash
#
# Check that no specific haskell test is being focused
# i.e: we don't want any `fit` or `fdescribe` in **/test/**/*.hs

root=$(git rev-parse --show-toplevel)
test_files=$(find $root -path */test/*.hs)

focused_tests=$(rgrep "fit\|fdescribe\|focus" $test_files)

if [ -z "$focused_tests" ]
then
    exit 0
else

echo "Error: Some Haskell tests are focused:"
echo ""
printf '%s\n' "${focused_tests[@]}"
echo ""
echo "This is probably a mistake and should not be commit
ted nor pushed"
echo "If you know what you are doing you can still commit by using:"
echo "  git commit --no-verify"
    exit 1
fi
```
You just need to put this content in the file `.git/hooks/pre-commit` and make it executable (`chmod +x .git/hooks/pre-commit`).
Now, right before you git commit, git will make sure that you do not commit any focused test whatsoever!


:cactus:
