---
layout: post
title:  "Building a web app with functional programming - Haskell - part I"
date:   2020-03-09 00:00:00 +0100
categories: haskell
---

{% include building-a-web-app-with-functional-programming-summary.md %}


Howdy,

This post is part of the series [building a web app with functional programming languages][building-a-web-app-with-fp] and is the sequel to the Elm part.

Haskell was used to write the API that backs [patchgirl.io](https://patchgirl.io), a web-based rest client.
This post talks about my feedback with Haskell on this project and whether I consider it [production ready][production-ready].

# Introduction

Before building Patchgirl, I wasn't a Haskell beginner. I have played with it before (never professionally) and done some Scala professionally.

Haskell is great, powerful, fascinating and exciting. It represents why I love my job and find our industry one of the best in term of technical self improvement. I could rant for a while but you'll find plenty of blog posts about why Haskell is great. I thought I'd rather focus on where I feel there is room for improvement.

# The core

## Architecture

When it comes to designing the roots of your software, there exist multiple solutions:<br/>
- The **mtl** (or monad transformer) way which is the mainstream solution nowadays and consists in stacking useful *"features"* (eg: the possibility to access values from an environment/configuration, log messages, execute effectful computations, ...)
- The **interpreter** way which consists in writing a program and its interpreter separately. I won't go too deep into the details but it has the pros of having a nice readability and make writing tests easier.

But this doesn't end here as the interpreter way has 2 distinct sub families:

- Free Monad
- Extensible effects

Again, I won't go into the details but the latter has triggered lots of excitement in the Haskell community this past few years. So I initially wanted to write my app with it but I gave up shortly after due to many reasons:

- There are way too many libraries ([fused-effects](https://github.com/fused-effects/fused-effects), [polysemy](https://github.com/polysemy-research/polysemy), [eff](https://github.com/hasura/eff), ...) and none of them seem to have the upper hand at the time of writing
- There are no beginner accessible materials to learn how to work with extensible effects yet
- Extensible effect make use of advanced/recent Haskell features that I don't feel comfortable with yet

So I took the *easy* path and went for the mtl style that I already knew.

## Mtl style

The mtl style is probably the easiest to start with because it has many online materials you can learn from (I personaly like [that one](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md)) and existing examples you can copy. Nevertheless, there were few parts that I struggled with.

### Downsides

1. Learning

    You might not need to figure out the inner working but if you do, it'll take time. Understanding Monad transformers requires to initially understand the dreadful Monad concept. <br/>
    I said there was online materials to help you but unfortunately, nothing official so you will have to browse many blog posts of various quality until you find the one that works for you.


2. The setup

    Even though I understand how it works, I initially struggled to set it up with my stack. I spent a lot of time digging documentation and other's people code and eventually made it work. Not the greatest experience I admit.
    Once setup though, it's quite easy to work with though. <br/>
So depending on the library you use, it might not be a trivial task.

3. Other

    The mtl has other downsides (that you'll find in other blog posts like its boilerplate) that I personally didn't encounter (yet). I assume this is due to Patchgirl being quite simple so far (my monad transformer stack solely consist in being able to read from an environment and throw error).<br/>

## Technical flaws

I won't spend too much time talking about technical details as it doesn't feel much pertinent, but I wanted to talk at least about Haskell records.

### Record

It's not a deal breaker but Haskell records are not so pleasant to use. Most of its flaws can be solved with third parties solutions but [one remain](https://wiki.haskell.org/Name_clashes_in_record_fields). In Haskell, 2 record fields can not have the same name.

For example this would not compile:
```haskell
data User = User { name :: String }
data Car = Car { name :: String }
```

Because when calling `name` on a data type, the compiler can not infer which `name` to call. The most popular solution is to always prefix the field by the data type name, eg:

```haskell
data User = User { userName :: String }
data Car = Car { carName :: String }
```

When dealing with an API, many fields are usually redundant (`id`, `createdAt`, `updatedAt`, `deletedAt`) and prefixing this with the data type name creates a lot of noise. I personaly find it sometimes hard to read.
This will strike a lot of newcomers as there is no other languages (that I know of) that faces this.

It's tedious but don't spend time trying to find a solution. I've been there and it's just not worth it. Haskell records is one of the few technical pain that you will just have to accept.

# Learning resources

## Convention and best practices

Born and bred in the research field, Haskell is very different from the industry standards.<br/>
In terms of guidelines you are almost immediately thrown into the jungle. The experimented haskeller will quickly build a fishing rod, find water and build a campsite. The newcomer instead will either die of cold the first day or struggle to survive until they accumulate enough experience.
The analogy is a bit harsh but I think it's quite accurate (at least to me).

In terms of architecture, as explained above, Haskell is still quite experimental and you are pretty much left on your own to experiment whatever solutions suits you better.

There is not one official place to learn or be guided from. Instead, you'll find plenty of resources scattered in blog posts, talks and forums.
This makes it hard initially to figure out how to do things and how to do them correctly.

## Documentation

One thing to mention is that Haskellers rely **a lot** on the type system. So much that documentation might sometimes be quite succinct.
It's far from perfect but you get use to it and eventually learn to just *follow the types* :rabbit:<br/>
Nonetheless, depending on the library's documentation quality, you'll find out that understanding it can be time consuming.

While working on patchgirl I often had to browse libraries' source code for more details or just to understand it.
I personaly don't mind as browsing the source code is easy with hackage and because it's always enlightening to see other's people code.
But productivity wise, I've lost time and energy.

# Conclusion

## Learn and improve

On the one hand, Haskell is a language that provides many unique patterns (Monad, Functor, pure functions, composition and so much more...) to create your software. You'll learn so much from it and improve your programming skills.
You'll also understand better how to modelize a problem and design its solution thanks to its amazing type system.
On the other hand, you'll quickly be on your own. Time will be spent researching and experimenting and despite the toughness, you eventually won't regret any single second out of it.

## Production ready?

I'm going straight to hell :fire: for that but I'll say it anyway. It's hard to learn, there are no great official guides and best practices. The learning curve is super steep and using (almost any) library will require time and effort for beginners.
You'll often have to browse examples and usages on github to understand better a piece of code.

So no, I don't consider Haskell to be production ready yet.
That being said, it doesn't mean you shouldn't use it.

## So should I use it?

Why should I use it if it's not production ready? <br/>
You can consider it an investment. It will take time to learn and be efficient with but once you reached that level it will fundamentally change your way (for the better) of understanding and solving problems.<br/>
I can assure you that learning Haskell will make you a better developer.

Although, if you are completely new to functional programming, please consider giving [Elm][elm-part-I] a try before diving into Haskell.
I find it a much friendlier step into the world of functional programming.

## last word?

I could have talked about its great compiler, its helpful community, its powerful type system, its amazing possibilities of abstraction, its ease to refactor and maintain softwares or other countless great features but much have already been said on those matters.<br/>
Hence, I prefered to focus on where I struggled and where I think it has a potential of improvement (especially for beginners).

That being said, I love it :heart: It's by far my favorite programming language and wish it was more mainstream.

:cactus:

[building-a-web-app-with-fp]: {% post_url 2020-03-01-building-a-web-app-with-fp-intro %}
[production-ready]: {% post_url 2020-03-02-production-ready-definition %}
[elm-part-I]: {% post_url 2020-03-03-elm-part-I %}
[elm-ui]: https://github.com/mdgriffith/elm-ui
[elm-guide]: https://guide.elm-lang.org/
