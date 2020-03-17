---
layout: post
title:  "Building a web app with functional programming - Haskell - part I"
date:   2020-03-09 00:00:00 +0100
categories: haskell
---

for regular real url:

url: Just { fragment = Nothing, host = "dev.patchgirl.io", path = "/session/callback", port_ = Nothing, protocol = Https, query = Just "code=test" }

for url with # in it:

url: Just { fragment = Just "session/callback?code=test", host = "dev.patchgirl.io", path = "/", port_ = Nothing, protocol = Https, query = Nothing }

the `?code=test` is in the fragment part

:cactus:

[building-a-web-app-with-fp]: {% post_url 2020-03-01-building-a-web-app-with-fp-intro %}
[production-ready]: {% post_url 2020-03-02-production-ready-definition %}
[elm-part-I]: {% post_url 2020-03-03-elm-part-I %}
[elm-ui]: https://github.com/mdgriffith/elm-ui
[elm-guide]: https://guide.elm-lang.org/
