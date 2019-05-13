# Home Page

In this exercise, we are going to display a collection of things on the screen and use eventwriter / holdDyn ourselves.

Learning outcomes:
- Use holdDyn to store the last value of an event
- Learn about what abstractions reflex-dom has for displaying collections of things.
- Use event writer in the wild
- Our first recursive do based widget!

Checkout the login page on http://localhost:8000/ and open up the workshop code in frontend/src/Frontend/HomePage.hs. It's best to be logged out, because we aren't going to be implementing the feed tab right now.

## Technical Goal

Our UI has three pieces of local state that it needs to track:
- The list of loaded tags
- The list of loaded articles
- Whether the global feed tab is selected (the default) or whether a tag has been clicked.

But we can't load the articles until we have the selection wired up, as the selection changing will force the articles to be reloaded.
Our aim is to have three top level holdDyn calls to collect these states from events. We just have to go around circles to collect all of the state. :)

Note that the example is a bit more featured that what this exercise gets up to. So don't peek at the example, peek at [04-home/answer.md](./04-home/answer.md).

## Load Tags

Hook Client.allTags up to the postBuild event to load tags when our page is constructed.

Call holdDyn on the success result so that we hold onto a `Dynamic t (Namespace "tags" [Text])` for the last tags that we've loaded.

## Print out Tags

Now that we have a list of tags as a dynamic, we need to get around to printing them. For this we can use list:

```haskell
list
  :: (Ord k, Adjustable t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t (Map k v)      -- The collection of Vs indexed by K.
  -> (Dynamic t v -> m a)     -- the widget for each child
  -> m (Dynamic t (Map k a))  -- Returns the output of each widget inside a dynamic map.
```

This is much like react where giving rows of a collection an id helps the framework do the least updates to the DOM possible. It's almost always worth the trouble to transform things to a Map rather than to use simpleList or the like.

Map over the allTagsDyn to make it a Map. Map.fromList will help here. Call list on this dynamic with tagPill as the child widget.

You may be getting an error that looks like:
```haskell
 Couldn't match type ‘Dynamic t (Map.Map Text ())’ with ‘()’
      Expected type: m ()
        Actual type: m (Dynamic t (Map.Map Text ()))
```
Which means that the list display is returning more than we want to output from this function. Throw the value away by wrapping the call to list in a Data.Functor (void). Void is your friend in these cases.

You'll then be getting a message that looks like:

```haskell
Could not deduce (EventWriter t (NonEmpty HomePageSelected) m)
        arising from a use of ‘tagPill’
```

Our m for our homePage function has no capability to dispatch an event of that type, and we don't want this event to escape this widget so we don't want to change the function signature either. Instead, we want to peel off a concrete eventWriter like in the warmup foldDyn revision.

Figure out how and where to run the eventWriterT so that you have a new `Event t (NonEmpty HomePageSelected)` that you can use to start filter the articles that we are loading.

Your tag list probably isn't loading when you refresh. Change your input event from \_pbE to (\pbE <> void (updated _tokDyn_)). This is not your fault. It's something weird to do with the loading from local storage. Weird.

## Hold the selection

At this point you want to hold the selection so that you can use the selection to filter the backend call by a tag or not. The initial selection should be GlobalSelected.

You'll have to change the top level do to an mdo so that you can make a cycle in your graph.

Hold the selection so that you can feed the resulting dynamic into the Client.listArticles to fill in the tag or not. You can use the homePageSelected catamorphism to make a function from Selection -> [Text].

What should the triggering event to listArticles be? Have a look at what inputs are heading into the call and have a play. If you have a dynamic, you can use the following function to get an event for when the dynamic changes:

```haskell
updated :: Reflex t => Dynamic t a -> Event t a
```

## The final touches

Dispatch a GlobalSelected when the global tab is clicked so that the tag can be deselected. If you put the tellEvent right next to the button, you shouldn't have to change much at all.

When a tag is selected, we want to show a tab for the tag. This is our first taste of higher order FRP! Take a look at the type of dyn:

```haskell
dyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
```

A really ergonomic pattern with dyn is with ffor (flipped <$>). Put this snippet right beside the global feed tab and play around with the types to build another tab only with the selection is TagSelected. What this will do is that it will run that inner function everytime the selection changes and rebuild the dom beneath the dyn. There is no diffing, so if the widget inside the function was really expensive to redraw, we would not do it this way as we'd be redrawing far too much. It works well enough for this use case thougo.

```haskell
void . dyn . ffor selectedDyn $ \selection ->
  blank
```

The html that the tag link should be when active is:
```
<li class="nav-item"><button class="nav-link active">#$TAGNAME</button></li>
```
It doesn't need to trigger anything if clicked, because the only time it is visible is when it is active.

Also, change the global button from buttonClass to buttorDynClass and build the class to only be active if the selection is global.

At this point, your HomePage should be complete (sans the user feed) and you should have learned something. :)

## Next Page

[05-article](./05-article.md)
