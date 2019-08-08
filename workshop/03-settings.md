# Settings Page

In this exercise, we're going to work on the settings page. Each user account
has settings associated with it which the user can modify. When the settings page is loaded we need to prepopulate it with the user's existing settings
and then trigger an update in a similar way to the login page when they save
their preferences.

Learning outcomes:
- Learn about PostBuild, for performing actions on a page load
- Take the successful event for page load and set the form element values with the details.
- Revisit the form processing and submission with less training wheels this time.

Checkout the login page on http://localhost:8001/settings (you will need to login with dashy@mlp / Password ) and open up the workshop code in `frontend/src/Frontend/Settings.hs`.

## Populating the setting page

There are some differences between the settings page and the login page

1. The settings page should only be available to logged in users.
2. We need to prepopulate the settings with the user's current settings.

In a similar way to `noUserWidget` which we used in the login page to redirect
already logged in users, we can can use the `userWidget` to restrict access
to certain pages to logged in users.

The `userWidget` helper has a slightly different interface to `noUserWidget`
as it also supplies the currently logged in user as an argument which can
be used in the construction of the rest of the page.

```haskell
userWidget $ \acct -> elClass "div" "settings-page" $ do
```

* Wrap the settings page in a `userWidget` helper so that this page will only be displayed if the user is logged in.


## Performing events on page load

Now we need to preload the form with all the settings from the logged in user.

We can ask reflex to give us an event that will fire when this level of FRP network has been fully set up. Think of it as an `onLoad` for your widget!

If we have a constraint `PostBuild t m => m a` then we can call `getPostBuild :: PostBuild t m => m (Event t ())`. The returned event will fire when the
set up is completed.


* Call `getPostBuild` to get an event (I often call this event `pbE`).

Hint: The `getPostBuild` event should be called inside the `form` element.

When the page loads, and `pbE` fires, we need to load the current user. This
is what the `Client.getCurrentUser` function does.

```
getCurrentUser
    :: (Reflex t, Applicative m, Prerender js t m)
    => Dynamic t (Maybe Token)
    -> Event t ()
    -> m (ClientRes t (Namespace "user" Account))
```

* Call `Client.getCurrentUser` to load the current user's settings.

Hint: The argument supplied by `userWidget` is of type `Token`.
Hint: You can construct a constant dynamic by using `constDyn`.

```
constDyn :: Reflex t => a -> Dynamic t a
```

Like `Client.login`, `Client.getCurrentUser` returns the same triple of events which indicate success, failure and progress.

## Initialising form values

If you've now called `Client.getCurrentUser` you should have an `Event t (Namespace "user" Account)` around. We can use this event to set the values of the form. Events have functor instances, so we can map over them and select bits of the event to make a new event with finer grained data in it. For example:

```haskell
let loadAccountE = unNamespace <$> loadSuccessE
let loadEmailE   = Account.email <$> loadAccountE
```

* Replace all the values of `never` with appropiate events defined in terms of
  `loadAccountE`.

## Finish off the form

Now we have a working form, we need to collect together all the values so
that when the update button is clicked, the updates are sent to the server.

`Client.updateCurrentUser` is the method which is called to send an update to
the backend. `updateCurrentUser` takes two arguments, a `Dynamic` indicating the
current user to update, we already used this with `getCurrentUser` and ` Dynamic t (Either Text UpdateUser)` which is the update to send to the server.

The UpdateUser type is record which has `Maybe` values in it. If the maybe is not set, then the backend does not update that field.

```
data UpdateUser = UpdateUser
  { password :: Maybe Text
  , email    :: Maybe Text
  , username :: Maybe Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  } deriving Generic
```

* Create a `Dynamic t UpdateUser` which contains the values from each of
  the form fields. Note: Make sure that the `password` update field is Nothing if the `password` field is the empty string.

Hint: Construct `UpdateUser` like `Credentials` in the login page.

* Call `updateCurrentUser` with the current user dynamic and the `UpdateUser` dynamic.

On a successful update, it would be good to redirect to the user's profile page. The `setRoute` function can be used to redirect the user when an event fires. It is a function defined by `obelisk-route` and one of the advantages of
using `obelisk` to build and manage our application.

```
setRoute :: Event t (R FrontendRoute) -> m ()
```

The appropiate route can be constructing using a DSL for building routes. The
account returned by the success event of `updateCurrentUser` is used to
select the right user profile to redirect to.

```haskell
route :: Account.Account -> R FrontendRoute
route a = FrontendRoute_Profile :/ (Username $ (Account.username a, Nothing)
```
* Call `setRoute` with the success event of `updateCurrentUser` and `route` to
  redirect a user after updating their settings to their user account.

## Logout Button

At the bottom of the form there is a logout button that currently does nothing.
The general way to logout a user is to use `tellEvent` with a `_LogOut` event type.

* Make the logout button logout a user when it is clicked.

Hint: To construct the logout event type use `(\_LogOut #)`

## Next Page

[04-home](./04-home.md)
