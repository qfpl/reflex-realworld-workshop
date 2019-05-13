# Bonus Exercises

If you finish early or are after different challenges. Try the following:

## Implement Form Disable while Submitting

Mark all inputs and the submit button as disabled while the backend call is pending.

## Implement the user feed tab on the home page

If logged in, the default homepage view should be the output of /api/articles/feed. This is tricky because you have to use higher order FRP to switch in a different backend call depending on the selection.

## Create Post Form

See if you can create the create post form from scratch.

For even more bonus marks, see if you can get a live preview from typing events in the markdown text area. You may need to throttle these events to get decent UX out of it.

## Client Side Validation

Use Data.Validation and Data.Functor.Compose together to validate as you collect form input values. Connect failed validations with onBlur events to display / hide them when things fail / are resolved.

## Implement Pagination

This hasn't been done in the example yet. But the article list calls take a limit and an offset and they return you a row count when they return. Can you run with this and implement pagination?

## Implement Favourite / UnFavourite of articles

This is hard because these calls aren't implemented in the API yet! You'll have to figure out all the servant layers to make this happen.

It's also a revision of the behaviour that we saw in the article comments: that we are updating our local state based on the backend call.

Same goes for profile follow / unfollow.
