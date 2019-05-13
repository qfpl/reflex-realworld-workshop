# Login

In this exercise, we are going to get a feel for how we build up dynamics into a validated output from a form.

Learning outcomes:
- Put some basic dom structure in place.
- Show how we can create reflex-dom components and access their output dynamics / events.
- Make sure are comfortable with Applicative syntax and how we can combine dynamics with them.
- Submit an ajax request and handle the success and error cases.

Checkout the login page on http://localhost:8000/login and open up the workshop code in frontend/src/Frontend/Login.hs.

## Combine Dynamics with Applicative

Dynamic has an applicative instance so you can use liftA2 and friends and also <*>. If you are unfamiliar with applicative, it's best to use liftA2. See:

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

Note that:
```haskell
liftA2 f a1 a2 = f <$> a1 <*> a2
```

In case that makes the <*> style more intuitive to you.

Use applicative to transform the let creds to a `Dynamic t (Namespace "user" Credentials)` by connecting together the value of each form field.

To get a Dynamic t Text of the current value of an input element, use the function \_inputElement_value . E.g:

```haskell
:t \__inputElement_value emailI
Dynamic t Text
 ```

## Connect to client call

In the commented out Client.login call, you'll note that the inputs to that are the dynamic that we just made plus an event that tells the machinery to make a backend call. Even if the dynamics update due to form inputs, the backend wont fire until the input event is fired.

Connect the dynamic that we just made to the client, and attach the button click event to the client call.

The client returns a triple with an event for success, an event for error and a dynamic that represents whether we are waiting for the backend or not.

To test, use the email "dashy@mlp" and password "Password". Given that we have no error handling at the moment, it may be good to have the network tab open to see if the call is working.

## Login on a successful call

If we have a successful login call, we need to tell our central state that there has been a login so that the token can be put into local storage. Note the two constraints on the login widget:

```haskell
     , EventWriter t (NonEmpty e) m  -- Our context can dispatch events of type NonEmpty e
     , AsFrontendEvent e             -- And e is constrained by a classy prism.
```

The AsFrontendEvent class means that we don't depend on a concrete event type but instead just depend on any type that can fit a frontend event into it. This allows our caller to have an event writer of a bigger event type than this level. To construct a LogIn event in this abstract way:

```haskell
eT Frontend> :t (_LogIn #)
(_LogIn #)
  :: AsFrontendEvent t =>
     Common.Conduit.Api.User.Account.Account -> t
```

This means that we can tell event like the one that has a placeholder event of never. Connect it to the backend success event.

You can log yourself out by pressing "Ctrl-Shift-I" in chromium, going to Applications, expanding local storage in the left hand tree and then right clicking on http://localhost:8001 and clearing storage. You should be logged out without even refreshing the page.

## Redirect on Login

At this point, the login ought to work and the nav bar should change, but it just stays on the login page. This is not great! Thankfully, there is a very handy thing in the FrontendState module:

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

which will take a computation that can set route, build dom and has frontend state that contains a logged in account. If there is no user logged in, it will run the supplied domBuilder. Otherwise it redirects to the home page.

Wrap it around the entire login page and refresh the page. You should get redirected to home. If you manually clear storage, you should be able to navigate to http://localhost:8001/login again. Then once the login call succeeds you should get redirected immediately.

This is the power of reflex-dom. To build abstractions that build DOM with respect to time varying state. Can you imagine writing such a painfree and reusable abstraction in other frontend frameworks? I find this pretty exciting! :)

## Next Page

[03-settings](./03-settings.md)
