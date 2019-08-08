# Login

In this exercise, we are going to get a feel for how we build up dynamics into a validated output from a form.

Learning outcomes:
- Put some basic dom structure in place.
- Show how we can create `reflex-dom` components and access their output dynamics / events.
- Make sure are comfortable with `Applicative` syntax and how we can combine dynamics with them.
- Submit an ajax request and handle the success and error cases.

Checkout the login page on http://localhost:8001/login and open up the workshop code in `frontend/src/Frontend/Login.hs`.

Most of the login page is already implemented. The part we are going to implement is combining together the different
parts of the user form and making the request to the server.

## Connect to client call

In order to login a user we will call the `Client.login` function.

```
Client.login
  :: Dynamic t (Namespace "user" Credentials)
     -> Event t ()
     -> m (Event t (Namespace "user" Account), Event t ClientError, Dynamic t Bool)
```

The `Client.login` function takes a dynamic which contains the credentials we
want to try logging in with and an event which will trigger the login attempt.
The return type is a triple containing

1. An event which will fire when a login succeeds.
2. An event which will fire when a login fails.
3. A dynamic which indicates whether the login is still in progress.

In order to call `Client.login` we firstly need to construct a `Dynamic`
holding the credentials input by the user in the form and then
connect it to the `_submitE` event.

## Building the Credentials Dynamic

Our first task is to create the `Dynamic` which holds the current user
credentials in the form. The form has already been constructed. Text fields
are constructed using `inputElement` and result in a value of type `InputElement` which
is a collection of various dynamics about the current state of the text field.
For example, there is a dynamic about whether the field currently have focus but
importantly for us, a `Dynamic` which indicates the current input in the field.

1. Use `_inputElement_value` to extract the `Dynamic t Text` which reports the
   current state of the email and password fields.

Hint: `_emailI :: InputElement er d t`

### Combining together Dynamics

Once you have extracted the relevant `Dynamic t Text` from the `InputElements`,
we need to combine them together to form a `Dynamic t (Namespace "user" Credentials)` to pass to `Client.login`.

The main way to combine together `Dynamic` values is by using the `Applicative`
interface. This means that you can use `liftA2` and friends in order to combine `Dynamic` values together. If you are unfamiliar with `Applicative`, it's best to use `liftA2`. See:

```haskell
:t liftA2 @(Dynamic __)
liftA2 @(Dynamic _)
  :: Applicative (Dynamic w) =>
     (a -> b -> c) -> Dynamic w a -> Dynamic w b -> Dynamic w c
liftA3 @(Dynamic _)
  :: Applicative (Dynamic w) =>
     (a -> b -> c -> d)
     -> Dynamic w a -> Dynamic w b -> Dynamic w c -> Dynamic w d
-- ... etc
```

2. Combine together the password and email dynamics to create a `Dynamic t Credentials`. Call this new value `credD`.

After creating the `Dynamic` containing the credentials we still have to turn
it into a `Dynamic t (Namespace "user" Credentials)`. Thankfully, this is
easy by mapping `Namespace` over `credD`.

3. Modify `credD` by mapping the `Namespace` constructor using `Functor`.

At this point you should have a `Dynamic t (Namespace "user" Credentials)`
which will contain the values that the user input into the form.

## Calling Client.login

4. Call `Client.login` with `credD` and `_submitE`

Now when the submit button is pressed, the `Client.login` function will attempt
to perform a login with the values currently in the form.

The client returns a triple with an `Event` for success, an `Event` for error and a `Dynamic` that represents whether we are waiting for the backend or not.


To test, use the email "dashy@mlp" and password "Password". Given that we have no error handling at the moment, it may be good to have the network tab open to see if the call is working. If the call returns `403 Forbidden` then you have probably made a mistake somewhere.

## Login on a successful call

The next part of this exercise is firing an event which indicates the login has been successful. Any other parts of our program which need to react to a user login will then update on successful logins.

If we have a successful login call, we need to tell our central state that there has been a login so that the token can be put into local storage. Note the two constraints on the login widget:

```haskell
     , EventWriter t (NonEmpty e) m  -- Our context can dispatch events of type NonEmpty e
     , AsFrontendEvent e             -- And e is constrained by a classy prism.
```

The `AsFrontendEvent` class means that we don't depend on a concrete event type but instead just depend on any type that can fit a frontend event into it. This allows our caller to have an event writer of a bigger event type than this level. To construct a `LogIn` event in this abstract way:

```haskell
> :t (_LogIn #)
(_LogIn #)
  :: AsFrontendEvent t =>
     Common.Conduit.Api.User.Account.Account -> t
```

5. Replace `never` in the call to `tellEvent` with the event which fires on a successful account login (returned by `Client.login`)

Hint: You will need to use `unNamespace` in order to remove the `Namespace` wrapper.

You can log yourself out by pressing "Ctrl-Shift-I" in chromium, going to Applications, expanding local storage in the left hand tree and then right clicking on http://localhost:8001 and clearing storage. You should be logged out without even refreshing the page.

## Redirect on Login

At this point, the login ought to work and the nav bar should change, but it just stays on the login page. This is not great! Thankfully, there is a very handy thing in the `FrontendState` module which will redirect logged in
users to the homepage.

```haskell
noUserWidget
  :: ( HasLoggedInAccount s
     , HasFrontendState t s m
     , SetRoute t (R FrontendRoute) m
     , DomBuilder t m
     , PostBuild t m
     )
  => m ()
  -> m ()
```

6. Wrap `noUserWidget` around the entire login page.

Refresh the page. You should get redirected to the home page. If you manually clear storage, you should be able to navigate to http://localhost:8001/login again. Then once the login call succeeds you should get redirected immediately.

This is the power of `reflex-dom`, you build abstractions that build DOM with respect to time varying state. Can you imagine writing such a painfree and reusable abstraction in other frontend frameworks? I find this pretty exciting! :)


## Next Page

[03-settings](./03-settings.md)