---
layout: post
title:  "Building a web app with functional programming - Haskell - part II"
date:   2020-03-22 00:00:00 +0100
categories: haskell
---

{% include building-a-web-app-with-functional-programming-summary.md %}


Howdy,

This post is part of the series [building a web app with functional programming languages][building-a-web-app-with-fp]. This part will focus on the libraries used while building the backend API.

I wanted to describe which [library](https://github.com/patchgirl/patchgirl/blob/59834f4200aaaeb9cffa69789e1bdf88b326530a/back/package.yaml#L40-L77) I used, why I picked them and whether I consider them production ready.
Please note that I barelly had any experience with any of them before so I'll approach them with a beginner state of mind.

# Web server

I heard you need to create a web API? Well my friend, before you start coding, you need to find your way through the many existing frameworks.<br/>

My requirements where only to have a framework with a well written guide and documentation because I didn't want to spend time figuring "simple" things. I started to play with the one that felt simpler (eg: Scotty, Spock...).
But I wasn't totally satisfied until I tried Servant and Yesod. Both diametrically opposed, Servant focus on the separation of concerns where Yesod comes with a full battery of tools (Orm, DB migration, html forms, web sessions, authentication...). <br/>
On the one hand the latter was appealing because it felt I wouldn't have to spend time searching other tools. On the other hand I tend to prefer software with a separation of concern philosophy.

Servant killer feature, its API based on type convinced me to go for it.

All other frameworks seemed or felt nice (and even simpler) but I couldn't find proper guides to help me setup the basics.<br/>

## Servant

Ok, about Servant now. First things first, it's got quite a steep learning curve. The idea behind Servant is that it lets you modelize your API with types. To achieve that, it uses a lot of Haskell advanced concepts (type families and whatnot). So even if you don't need to know how the inner works, you'll probably be initially surprised by its syntax. It takes time but I eventually got used to it thanks to its helpful [guide](https://docs.servant.dev/en/stable/).

### Pros

Servant is really powerful. Its type system can really help design endpoints and behaviours once you get used to it. I like its concision and separation of concerns.

When you design your API, Servant can generate a client for free. This is a big deal as it removes tons of stupid errors.<br/>
This feature felt more than welcome when implementing tests and when querying the API from the frontend. You're guaranteed to have a client that match your API.

### Cons

On a less positive note, Servant surprised me many times when faced with complete abstruse error messages. Because it uses complex Haskell features, the compiler often fails to give a simple explanation on what's wrong.

Moreover, when your API size starts to grow, a tiny mistake to one of your endpoint can be hard to trace. Indeed, dependending on the situation, the error messages I had sometimes didn't point me to the right location. <br/>
Sometimes it was just easier to ignore the compiler and find the bug on my own :flashlight:

### Conclusion

Servant is hard but worth it. I wouldn't recommend it if you've no to few experience in Haskell. Otherwise go for it, it's totally worth it.<br/>
It takes time but its guide and documentation will help you get up to speed without too much frustration.
I consider it production ready and think it was a great match to my project.

# Authentication

When I wanted to do some authentication with Servant, I was left with 3 choices:
1. Code my own solution
2. Use [servant-auth-cookie](https://github.com/zohl/servant-auth-cookie)
3. Use [servant-auth](https://github.com/haskell-servant/servant-auth)

I initially only needed a simple cookie based session but I didn't feel like writing my own solution. The second solution looked like the perfect fit for my need but it doesn't seem to be alive anymore. Out of options, I went with the 3rd solution.

## Servant-auth

Have I said that doing authentication with Servant is hard? Well at least to me it felt that way. It took me a while to fully understand how Servant-auth works and especially how to integrate it with the test.
The Readme do a great job at giving usage examples but nothing is mentioned for how to test your API.

Once setup on the other hand, it integrates nicely with Servant and works seamlessly.

## Conclusion

There aren't many available solutions to do authentication with Servant. Servant-auth is one but it isn't easy to setup/use.
I would have loved to have a bit more detailed explanations to make it more developer friendly but other than that it's a nice library that I consider production ready!

# Elm client generation

Servant gives you the ability to generate a client matching your API (given the generator for language you use exist). In my case, I needed to have an Elm client.
To convert Haskell to Elm code, many solutions exist:
- Elm-street
- Elm-bridge
- Elm-export
- Elminator
- Haskell-to-elm

I went with the combination of `servant-elm` and `elm-bridge`. It works fine but I wouldn't advise it.<br/>
Looking back at my option, I probably made a mistake in not trying more `elm-street` which seems to be much more documented and developer friendlier.

Regarding my choice, it took a lot of time to setup some corner cases. Because the documentation isn't explicit enough, I had to figure out how they work by reading its code source and other usages on github.

## Conclusion

I now have a fully working client generation but it wasn't an easy task and I'm still sometimes considering the switch to `elm-street`.
I think there are a lot of room for improvement especially regarding the documentation to make the combination `servant-elm`/`servant-bridge` production ready

# Metrics

I'm a bit used to prometheus so I was happily surprised to see that a it was [supported](https://github.com/fimad/prometheus-haskell) in Haskell.
It provides a Wai middleware hence almost all haskell web servers can be measured easily.

To set it up, I have found non official help in blog posts.
The only small downside I have found when setting the metrics is that there was no already available grafana dashboard.

But other than that, it works fine and was easy to setup. I consider it production ready!

# Json [de]serialization

Here again, nothing much to say. [Aeson](https://hackage.haskell.org/package/aeson) is a standard and works perfectly. The documentation is fine, yet beginners will probably find this [guide](https://artyom.me/aeson) to be helpful.
100% production ready!

# Postgresql object mapping

Pg is my go to database these days so I needed a library to query it. Most of the solutions I have found provide a way to create sql query statically which means that your queries will be type checked.
There are many solutions available:
- Selda
- Opaleye
- Beam
- Squeal
- Persistant
- ...

I have played a bit with Opaleye and even though I love the idea of having my sql queries type checked, I thought the complexity it brings wasn't worth it. Also, I've always liked writing SQL...

So I decided to go for a much simpler solution, Postgresql-simple. Its name says it all. You write functions that converts a Haskell structure to a pg one or the opposite. It's well documented and easy to play with.
Some corner cases are not documented and I wish the documentation was sometime more explicit but aside from that it's quite good.

It's a good compromise between quality and productivity.

# Test

For the tests, I quickly went with hspec. I started playing with it and it was super easy to setup and understand.
Big kudos to its authors for the awesome documentation!! Very helpful for a beginner.

I unfortunately didn't take the time to compare other libraries but there are nice alternatives especially if you want to play with property testing ([quickcheck](https://hackage.haskell.org/package/QuickCheck), [hedgehog](https://hackage.haskell.org/package/hedgehog)).
Otherwise, Hspec is great and I consider it production ready!!


# Conclusion

It took time to test many different solutions but eventually I think it was worth it. I have a better understanding of Haskell's ecosystem now and I'm quite happy with my stack.

## Production ready?

Here's the complete list of my stack if you are interested:

| Feature                | Library                     | Improvement I'd have liked                                                  | Production ready (max 5 :star:) |
|------------------------|-----------------------------|-----------------------------------------------------------------------------|---------------------------------|
| web server             | servant                     | Friendlier compiler error messages                                          | :star::star::star::star:        |
| authentication         | servant-auth                | More detailed documentation (especially on how to test with authentication) | :star::star::star:              |
| elm client generation  | servant-elm, servant-bridge | Much more documentation                                                     | :star::star:                    |
| metrics                | prometheus-haskell          | Official documentation on how to set it up                                  | :star::star::star::star:        |
| json [de]serialization | aeson                       |                                                                             | :star::star::star::star::star:  |
| database mapping       | postgresql-simple           | Beginner friendly examples, better detailed documentation                   | :star::star::star::star:        |
| test                   | hspec                       |                                                                             | :star::star::star::star::star:  |

Please note that this is highly opinionated and reflect my experience as a beginner with these libraries. I believe the tested libraries and their authors did a fabulous job and I'm grateful to them for providing them and saving me a lot of time.<br/>
I'm reporting what I felt using them and what could be improved especially regarding newcomer.

## last word?

I unfortunately didn't have the time to test everything but some are definitely on my bucket list of projects I need to give a try.

Thank you for reading this far and I hope this post will help you setup your stack so you can hack some Haskell!

:cactus:

[building-a-web-app-with-fp]: {% post_url 2020-03-01-building-a-web-app-with-fp-intro %}
[production-ready]: {% post_url 2020-03-02-production-ready-definition %}
[elm-part-I]: {% post_url 2020-03-03-elm-part-I %}
[elm-ui]: https://github.com/mdgriffith/elm-ui
[elm-guide]: https://guide.elm-lang.org/
