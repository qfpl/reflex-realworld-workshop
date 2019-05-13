# Settings Page

In this exercise, it's much like the previous exercise but we need to load preexisting data into the form.

Learning outcomes:
- Learn about PostBuild
- Take the successful event for page load and set the form element values with the details.
- Revisit the form processing and submission with less training wheels this time.

Checkout the login page on http://localhost:8000/settings (you will need to login with dashy@mlp / Password ) and open up the workshop code in frontend/src/Frontend/Settings.hs.

## getPostBuild

In reflex, we can ask reflex to give us an event that will fire when this level of FRP network has been fully setup. Think of it as an onLoad for your widget!

If we have a constraint `PostBuild t m => m a` then we can call `getPostBuild :: PostBuild t m => m (Event t ())`.

Wrap the settings page in a userWidget helper so that this page will only be displayed if the user is logged in.

```haskell
userWidget $ \acct -> elClass "div" "settings-page" $ do
```

Then call getPostBuild to get an event (I often call this event pbE). Remember that this is a call that works in the m monad, so you have to bind it in a do block.

TODO: Hint

You now have the appropriate data to be able to call Client.getCurrentUser. To make a pure value a dynamic, use:

```haskell
constDyn :: Reflex t => a -> Dynamic t a
```
## Form setValue

If you've now called Client.getCurrentUser you should have an `Event t (Namespace "user" Account)` around. We can use this event to set the values of the form. Events have functor instances, so we can map over them and select bits of the event to make a new event with finer grained data in it. For example:

```haskell
let loadAccountE = unNamespace <$> loadSuccessE
let loadEmailE   = Account.email <$> loadAccountE
```

Use this pattern to fill in all of the never values in the setValue events of the form.

## Finish off the form

You will need to collect up the output of the form into something that Client.updateCurrentUser can accept.

Just be aware that the UpdateUser type is record which has Maybe values in it. If the maybe is not set, then the backend does not update that field. So you should convert the password value into a maybe and only send an update if the string is non empty. Control.Monad (mfilter) and Data.Text (null) are your friends here. Look them up in your hoogle instance!

TODO hint.

On successful update, redirect to the profile page. You can create a route to the profile page with:

```haskell
FrontendRoute_Profile :/ (Username $ ("username", Nothing)
```

You should be able to wire this up to the success event and connect it to setRoute

TODO diagram

## Logout Button

At the bottom of the form there is a logout that currently does nothing. Link the click event up to a tell event. Note to create an event with the classy prims on a constructor with no arguments, do (\_LogOut #).

## Next Page

[04-home](./04-home.md)
