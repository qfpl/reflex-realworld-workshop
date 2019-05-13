# Workshop introduction

In this workshop, we'll be implementing the [Realworld / Conduit](https://github.com/gothinkster/realworld) (no relation to the haskell streaming library) spec inside an [obelisk](https://github.com/obsidiansystems/obelisk/) application. The reason why the realworld app is so interesting is that it actually has backend calls, auth, etc to give us a good idea of how a more substantial reflex-dom application can work. Right now, the example apps that are around are fairly simple and it is difficult to make the leap from examples to something like conduit.

This workshop intends to jump you ahead to demonstrate how you way structure a bigger reflex app. You will be missing fundamentals that may make things tricky, but the aim is to give you just enough of a taste test for you to motivate you to learn the fundamentals and anchor it to frontend concepts that you already know. David Laing's [workshop](https://github.com/qfpl/reflex-workshop) is a very good resource for learning the fundamentals, and you will almost certainly need to go back and do it after this workshop if you haven't already.

It's worth saying out loud that the jump from FP to FRP is almost a big of a mental leap as imperative programming to FP. Be kind to yourself, and if you get stuck please ask for help and we can talk about it and explain things in a targeted way in the workshop. I am fully expecting us to need to adapt on the day as we start figuring out where the sticky points are.

The backend is already all prewritten so the workshop will just be implementing the front end. Learning servant and beam is a whole other workshop/course, but the code is all here to peruse if you'd like to later. It should be considered more a learning tool than a best practice.

## Reflex Ecosystem

There are a fair number of moving parts to reflex, so here is a quick map of the territory:

- [GHCJs](https://github.com/ghcjs/ghcjs) is compiler that will compile GHC haskell into Javascript.
- [Reflex](https://github.com/reflex-frp/reflex) is the base FRP library that gives us Events, Behaviours and Dynamics
- [Reflex DOM](https://github.com/reflex-frp/reflex-dom) is a html frontend library written on top of reflex
- [Reflex Platform](https://github.com/reflex-frp/reflex-platform) is a bunch of nix, java and objective C to be able to build web/iOS/Android apps with reflex DOM. This uses a webview on mobile, much like react native. But it runs most of the code in native compilation from GHC and very minimal JS: much how the jsaddle-warp test setup works.
- [Obelisk](https://github.com/obsidiansystems/obelisk/) Is an opinionated framework that sits on top of platform, adding routing, server side rendering, static files, an out of the box ghcid based compile live recompilation and deployment.
- Jsaddle is the thing that gets used when you `ob run`. Instead of compiling to JS, most of the code is run in the native haskell runtime and commands are streamed over a websocket to interact with Javascript.

## What you should have already

You should have followed the instructions in the  [Setup Page](../SETUP.md). This should give you:
- A blank looking site on http://localhost:8001 . This should live recompile when you change the workshop code.
- An example app on http://localhost:8000 that is running my implementation. This is talking to the same DB, so you should be able to test them side by side.
- A hoogle server at http://localhost:8080 . This will be very handy. Just note that if you aren't using the VM, the reflex docs are actually linked via file links that the browser wont open for you. Copy the link address and paste it in a new tab. The VM has a hacky chromium plugin that disables this security feature, but I would not recommend installing that on your actual machine

**note**: The Jsaddle stuff only works in chromium, so you'll need that when you're developing. Install instructions should have covered that, but in case you missed it.

## How to progress through these exercises

- Do them in order as per the filename numbering.
- Given that this is a taste test rather than making you experts in 1.5 hours, don't feel like you're cheating by taking a peek at the example app to progress. It's much more important to get the full taste than get bogged down. You can always revisit later after doing Dave's course.
- Consult both quickrefs as they are invaluable.
  - reflex: [Internet Copy](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md) [LocalCopy](../docs/ReflexQuickref.md)
  - reflex-dom: [Internet Copy](https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md) [LocalCopy](../docs/ReflexDomQuickref.md)
  - And consult hoogle to search for docs for functions that you see: http://localhost:8080. If you are not using the vm, just be careful about the absolute file links that don't work/
  - Use type holes by putting in an \_ for an expression that you don't know how to write. Them follow the types via hoogle and the quickrefs.

## Overly ambitious Basic Workshop Outline

- 5  Mins: Welcome, orientation and getting people setup
- 15 Mins: Review [foldDyn and mtl style](./01-foldDyn.md) UI basics. This is what the talk covered. Ask lots of questions if you need.
- 10 Mins: [Login Form Exercise](./02-login.md) where we learn the basics of submitting a form via our client.
- 15 Mins: [Settings Form Exercise](./03-settings.md) where we have a form that requires data to be loaded first.
- 20 Minutes : [Home page exercise](./04.home.md) where we learn how to deal with displaying lists and using eventwriter.
- 20 Minutes : [Article Exercise](./05.article.md) where we do some higher order FRP and get into some interesting state management.
- 5 Minutes : Wrap up and questions
- Later: [Bonus Exercises](./9001-bonus.md) if you want to dig further after the workshop.

## Next Page

[01-foldDyn](./01-foldDyn.md)
