---
layout: post
title:  "Building a web app with functional programming - Intro"
date:   2020-03-01 00:00:00 +0100
---

# The idea

While working at my previous company, I felt like building a Postman like web app, a dead simple rest client.

As a former Ruby/Scala developer, I also wanted to learn more about functional programming and I thought it'd be fun to create from scratch a web app using only FP languages. <br/>
And so is born the idea of [Patchgirl](https://patchgirl.io), a web app built solely with functional programming languages *(FP)*.

# Goals

In the process of building Patchgirl, I also wanted to write feedbacks on my journey to reflect on:
- What brought me pure joy
- What made me tear my hair out
- Explain whether I consider the technologies I've used as [production ready][production-ready]

Hopefully, this series will interest developers with no to few experience with FP.

# Topics

In this series of *Building a web app with FP*, I will talk about my experience with the languages I've picked to make Patchgirl. <br/>
The single page app is built with Elm. The backend API was written with Haskell/Servant and everything runs under NixOS. <br/>
All three will be discussed in this series.

Alright, we are set, let's start this series with the first part with the delightful [Elm][elm-part-I].


[production-ready]: {% post_url 2020-03-02-production-ready-definition %}
[elm-part-I]: {% post_url 2020-03-03-elm-part-I %}
