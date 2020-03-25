# Home Page

In this exercise, we are going to display a collection of things on the screen and use `EventWriter` / `holdDyn` ourselves. The homepage will have a list of
articles on it. The list can be filtered by clicking the tags on the right-hand side.

Learning outcomes:
- Use `holdDyn` to store the last value of an event
- Learn about what abstractions `reflex-dom` has for displaying collections of things.
- Use event writer in the wild
- Our first recursive do based widget!

Checkout the login page on http://localhost:8001/ and open up the workshop code in `frontend/src/Frontend/HomePage.hs`. It's best to be logged out, because we aren't going to be implementing the feed tab right now.

## Technical Goal

Our UI has three pieces of local state that it needs to track:
- The list of loaded tags
- The list of loaded articles
- Whether the global feed tab is selected (the default) or whether a tag has been clicked.

But we can't load the articles until we have the selection wired up, as the selection changing will force the articles to be reloaded.
Our aim is to have three top level holdDyn calls to collect these states from events. We just have to go around circles to collect all of the state. :)

Note that the example is a bit more featured that what this exercise gets up to. So don't peek at the example, peek at [04-home/answer.md](./04-home/answer.md).

## Load Tags

The first piece of state we are going to build is the list of tags. This
is similar to getting the current user from the settings page. When the page
loads we will call `Client.allTags` to load the tags and then call `holdDyn`
on the resulting success event in order to create local state.

* Call `Client.allTags` to load the current tags when the post build event fires.

* Use `holdDyn` to create a Dynamic which holds the current list of tags. The
dynamic should have typed `Dynamic t (Namespace "tags" [Text])`

## Print out Tags

Now that we have a list of tags as a dynamic, we need to get around to printing them. For this we can use list:

```haskell
list
  :: (Ord k, Adjustable t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t (Map k v)      -- The collection of Vs indexed by K.
  -> (Dynamic t v -> m a)     -- the widget for each child
  -> m (Dynamic t (Map k a))  -- Returns the output of each widget inside a dynamic map.
```

This is much like react where giving rows of a collection an id helps the framework do the least updates to the DOM possible. It's almost always worth the trouble to transform things to a `Map` rather than to use `simpleList` or the like.

In this case, a suitable key for the tag is just the tag itself so the map which we pass to `list` should have type `Map Text Text`.

* Map over `allTagsDyn` to create a `Dynamic t (Map Text Text)` which can
  be passed to `list`.

Hint: Use `Map.fromList`.

Now we need to render the tags using `list`. The tags need to go in the `tag-list` div. The widget for each child is defined using `tagPill`.

* Try to render the tags list using `list` and `tagPill`. It won't work yet, keep reading.

You may be getting an error that looks like:
```haskell
 Couldn't match type ‘Dynamic t (Map.Map Text Text)’ with ‘()’
      Expected type: m ()
        Actual type: m (Dynamic t (Map.Map Text Text)
```
Which means that the list display is returning more than we want to output from this function.

* Use `void` in order to fix the type error. It still won't work, keep reading.

Hint: Apply `void` to the result of `list`.

One final type error. `tagPill` uses the implicit `EventWriter` in order
to inform the network about when a tag is selected. We have to eventually
handle this constraint on the homepage as otherwise the event never be linked
up to anything. All events are expected to be handled by the time we get to
the top-level so the following error occurs:


```haskell
Could not deduce (EventWriter t (NonEmpty HomePageSelected) m)
        arising from a use of ‘tagPill’
```

The solution is to use `runEventWriterT` at the point where we need to handle the event.

* Figure out how and where to run the eventWriterT so that you have a new `Event t (NonEmpty HomePageSelected)` that you can use to start filter the articles that we are loading.

Hint: Think about which DOM elements can cause selection events and make sure all those are wrapped.

Your tag list probably isn't loading when you refresh. Change your input event from `_pbE` to `\pbE <> void (updated _tokDyn_)`. This is not your fault. It's something weird to do with the loading from local storage. Weird.

## Hold the selection

At this point you want to hold the selection so that you can use the selection to filter the backend call by a tag or not. The initial selection should be `GlobalSelected`.

* Define a new dynamic which contains the current selection for the homepage.

Hint: Use `holdDyn` and the selection event from `runEventWriterT`.

Hint: You'll have to change the top level do to an mdo so that you can make a cycle in your graph.

### Loading the article list

Now we move onto using the selection to load the article list.

The `Client.listArticles` function can be used to load articles based on
the selection dynamic. It's type has quite a few parameters but most of them will be default values.

```
 listArticles
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (QParam Integer) -- limit
  -> Dynamic t (QParam Integer) -- offset
  -> Dynamic t [Text]           -- tags
  -> Dynamic t [Text]           -- favourited
  -> Dynamic t [Text]           -- authors
  -> Event t ()                 -- submit
  -> m (ClientRes t Articles)
```

What should the triggering event to `listArticles` be? Have a look at what inputs are heading into the call and have a play. If you have a dynamic, you can use the following function to get an event for when the dynamic changes:

```haskell
updated :: Reflex t => Dynamic t a -> Event t a
```

* Call `Client.listArticles` to load the articles based on the currently selected tags.

Hint: Use `homePageSelected` in order to create the tags argument. You can use
it to make a function `Selection -> [Text]`.

Hint: The triggering event is the event from the selection dynamic. Use `updated`.

### Rendering the article list

Now we have loaded the article list we need to store them into a dynamic and then render them.

* Create a dynamic which holds the currently selected articles.

Hint: The Dynamic is populated by the `listArticles` success event, the default value should be `Articles [] 0`.

Articles can be rendered using the provided rendering function. The first
argument is a dynamic which indicates whether the articles have finished loading
yet.
```
articlesPreview :: ... => Dynamic t Bool -> Dynamic t Articles -> m ()
```

* Render the articles using `articlesPreview`

Hint: Use the progress dynamic from the call to `listArticles`.

## The final touches

Let's finish off by adding a tab for a newly selected tag and the ability to
reset the selection to the global selection.

### Resetting the selection

Firstly, when the `Global`
tab is clicked, the `GlobalSelected` event should be emitted.

* Dispatch a `GlobalSelected` event when the global tab is clicked so that the tag can be deselected.

Hint: If you put the `tellEvent` right next to the button, you shouldn't have to change much at all.

### Displaying a new tab for the tag

When a tag is selected, we want to show a tab for the tag. This is our first taste of higher order FRP! Take a look at the type of `dyn`:

```haskell
dyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
```

We're going to use `dyn` with the selection dynamic to build a tab depending on what the current selection is. The type of `dyn` says that we need to construct
a `Dynamic` which produces a dom element. Therefore we need to map over
the selection dynamic in order to build a dom element depending on what
the current selection is.

A really ergonomic pattern with `dyn` is with `ffor` (flipped `<$>`). Put this snippet right beside the global feed tab and play around with the types to build another tab only with the selection is `TagSelected`. What this will do is that it will run that inner function everytime the selection changes and rebuild the dom beneath the `dyn`. There is no diffing, so if the widget inside the function was really expensive to redraw, we would not do it this way as we'd be redrawing far too much. It works well enough for this use case thougo.

```haskell
void . dyn . ffor selectedDyn $ \selection ->
  blank
```
* Modify the above snippet so that a new tab is drawn when a tag is selected

Hint: You need to write a function `Selection -> m ()` which decides whether to
draw the tab or not and map it over the selection dynamic.


The html that the tag link should be when active is:
```
<li class="nav-item"><button class="nav-link active">#$TAGNAME</button></li>
```
It doesn't need to trigger anything if clicked, because the only time it is visible is when it is active.

### Conditionally disable the global button

Finally we are going to change the global tab so it's disabled when another
tab is active. This is the first example where the class of an attribute item
is dependent on a dynamic value.

* Change the global button from `buttonClass` to `buttorDynClass` and build the class to only be active if the selection is global.

At this point, your home page should be complete (sans the user feed) and you should have learned something. :)

## Next Page

[05-article](./05-article.md)
