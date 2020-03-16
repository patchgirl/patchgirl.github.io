---
layout: post
title:  "Building a web app with functional programming - Elm - part II"
date:   2020-03-06 00:00:00 +0100
categories: elm
---

{% include building-a-web-app-with-functional-programming-summary.md %}

Time to talk about Elm's ecosystem ! If you haven't read it yet, check out [part I][elm-part-I] of [Building a web app with functional programming][building-a-web-app-with-fp].

# Guide, documentations and best practices

It might sounds peculiar to talk about Elm's guide but it helped me get up to speed in few hours. This is quite a satisfying feeling to be writing real world code after a day or two. Thanks to its well written and beginner friendly guide, understanding how Elm's architecture works is a painless process.

The very same can be said about its documentation. Official libraries or not, I haven't found a documentation that was poorly written yet. To be fair, I don't use many [unofficial libraries](https://github.com/patchgirl/patchgirl/blob/8f2fb4f630a9264d119573ebe64dd909c67d6e6b/front/elm.json#L9-L25). But still, I believe writing good documentation is part of Elm's spirit.

Elm comes with tons of materials to learn from. Some content helped me understand and improve my overall skills. I would particularly recommend watching all of [Richard Feldman](https://www.youtube.com/results?search_query=richard+feldman+elm) talks which were all helpful designing better architecture.

# Community

Elm has a great community. When I had questions, answers are always helpful. It might not be that surprising but I think it's a huge deal. Learning from scratch a language that uses FP's concepts (sum types, pure function, Maybe...) for traditional developers is difficult so having an empathic community helps a lot !

# Tooling

I won't write much about the tooling as I almost don't use anything.
I write Elm with emacs without any plugins and it feels nice enough. I tried to install elm language server protocol (LSP) on emacs but failed miserably :pensive:. I will probably give it another try later this year but I don't feel the urge to have IDE features for now. The compiler itself is plenty enough for my needs.

# Elm-UI

I couldn't finish this part on Elm without talking about [Elm-ui][elm-ui]. <br/>
I have to admit beforehand that I loathe CSS. Hours of buttons and labels/inputs aligments triggers angers when I hear the CSS word. Even Flexbox and other famous frameworks (bootstrap and whatnot) could not completely ease the pain. <br/>
So when I had to start writing CSS again, I could feel the frustration rushing back.

Then, some day, I stumbled on elm-ui, a library designed to replace CSS. I gave it a try and quickly adopted it.<br/>
With Elm-ui, you won't have to write css ever again. You directly integrate the element properties (shape, alignment, color...) within your business code.

Initially I thought that not splitting the presentation, decoration from the business layer would be a terrible idea but so far I haven't found any downsides and it integrates nicely with my project without any maintainability costs. <br/>
Elm-ui saved me hundreds of hours of frustration, helped me design a [nice UI](https://patchgirl.io) (not mobile friendly in complete honesty) and made UI development fun again.

# Conclusion

## The downsides

There are many fields I haven't experimented yet. Javascript interaction with *Ports*, mobile UI/UX design, more advanced UI like modals, animations...
So I don't have the full picture of the language. But from my experience, it's hard to give any impactful downsides. After months of development, I have yet to found something that felt inherently bad or could be improved. <br/>
Sure it will feel a bit verbose for Haskellers and some concepts are hard to initially grasp for beginners (json encoder/decoder, url parsing, maybe) but that's hardly an inconvenience compared to the gain in productivity.

I will keep on writing elm and maybe I'll found downsides. In the meantime, give it a try, you won't be disappointed !

## Production ready?

So is it [production ready][production-ready] ? :100:% yes. The learning curve is quite shallow so your team will get up to speed in a matter of days. The benefits are huge:
- no more runtime error :heavy_check_mark:
- great maintainability and easy refactoring :heavy_check_mark:
- super fast development cycle :heavy_check_mark:

If it fits your need you should totally embrace Elm.

## Delightful language?

Because of its friendly community, its well written guides/documentation, its beginner focused approach and its fast and helpful compiler, Elm has become my go to language for front end application.

It is such a great product that I also advise people who want to learn about functional programming to avoid Scala or Haskell and learn Elm instead. In my opinion, Elm does a fabulous job at discovering the advantages of functional programming over more traditional paradigms.<br/>

So yeah, it doesn't come as a surprise that I find Elm a **delightful language** :heart:

:cactus:

[building-a-web-app-with-fp]: {% post_url 2020-03-01-building-a-web-app-with-fp-intro %}
[production-ready]: {% post_url 2020-03-02-production-ready-definition %}
[elm-part-I]: {% post_url 2020-03-03-elm-part-I %}
[elm-ui]: https://github.com/mdgriffith/elm-ui
[elm-guide]: https://guide.elm-lang.org/
