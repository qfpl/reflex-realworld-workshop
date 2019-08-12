# How does obelisk deal with routing?

Routing is one of the most fundamental aspects

A route

It is useful to think about two kinds of routes.

1. Backend routes, ones which must be handled by the backend server such as
   login requests, database queries and so on.
2. Virtual routes, routes which only influence how the frontend is rendered.
   These are handled directly by the generated javascript in the frontend.

The typical way to structure an application is to

Obelisk includes an opinionated way to deal with routing.

# Basic Routing

The basic building block for routing is called an `Encoder`.

In a common module one data type is defined which describes all
the possible routes.

## Defining the structure of your routes

`FrontendRoute a` defines the routes that a user will be expected to
access. It is a GADT with a single type parameter, the parameter indicates
what infromation from a URL will be collected.

```
data FrontendRoute a where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  FrontendRoute_Editor :: FrontendRoute (Maybe DocumentSlug)
  FrontendRoute_Article :: FrontendRoute DocumentSlug
  FrontendRoute_Profile :: FrontendRoute (Username, Maybe (R ProfileRoute)
```

For example, the homepage route will take no query parameters and so  the return
type for that route is `()`. However, the users will access articles by
accessing `//article/<name-of-article>` and so the return type for this route is
`DocumentSlug`. `DocumentSlug` is a newtype wrapper around `Text`. When we define the `Encoder` the return type will make sure that we define routes which provide the right information.

Up until this point, there is no information about what the actual routes will be, just a specification for what we should discover when each route is taken.

## Defining the routes

It is in the definition of the `Encoder` where this data type is mapped
to a query string. An `Encoder check parse decoded encoded` has four type
parameters. Our encoder will have the following concrete types.

```
Encoder (Either Text)
        Identity
        (R (Sum BackendRoute (ObeliskRoute FrontendRoute)))
        PageName
```

1. `Either Text` is the type of the monad which will be used to ensure that the
    encoder is valid. `checkEncoder` is used in the main function to ensure that
    the encoder is valid.
2. `Identity` is the monad which is needed to parse a route.
3. The decoded route, a raw route will be parsed into this type.
4. The encoded type, the type of a raw route, a `PageName` is a URL path and a query string

```
type PageName = ([Text], Map Text (Maybe Text))
```

So basically, an encoder is about turning `PageName`s into parsed routes and
back again. It is bidirectional.

## What is `R`?

`R` is used in the type of the decoded route. `R` is a type synonym for a dependent sum type.

```
type R f = DSum f Identity
```

### Defining the encoder

A common way to start defining an encoder is using the
`pathComponentEncoder`. Ignored the constraints for now, its type
says that it defines an encoded from raw `PageName`s to routes.

```
pathComponentEncoder :: forall check parse p. (...) =>
    (forall a. p a -> SegmentResult check parse a) -
    > Encoder check parse (R p) PageName
```

In order to define `pathComponentEncoder` you have to apply it to a function
which says how to turn each route we defined earlier into a `SegmentResult`.
These `SegmentResult`s are then combined together to form the encoder.

For example, for our `FrontendResult` specification, the necessary function
will have type:

```
encodeFrontend :: FrontendResult a -> SegmentResult check parse a
```

### What is a `SegmentResult`?

Now all we need to know is what a `SegmentResult` is. A `SegmentResult` will
have to tell us how to *decode* and *encode* a route. The decoding part is parsing a raw route into our route data type and encoding turns the route data
type into a raw route. By using the combinators for building encoders and segment results

A `SegmentResult` is a simple list like data type with two cases. One for the case where we want to parse more of the path and the other for the end of the path.

```
data SegmentResult check parse a =
    PathEnd (Encoder check parse a (Map Text (Maybe Text)))
    | PathSegment Text (Encoder check parse a PageName)
```

For the simplest example, the home route is already at the end of the path as it is accessed from `localhost:8000` and the return type is `()`. Therefore we can use `PathEnd` immediately to define the route.

```
pathComponentEncoder $ \case
    FrontendRoute_Home -> PathEnd (unitEncoder mempty)
```

`unitEncoder` constructs a trivial encoder from a given value. The parser for
this encoder will always return `()`. The encoder will always return the given value. In this case as the `PathEnd` constructor requires an encoder to produce a map, we can use `mempty` to produce an empty map. We could have also use `Map.empty`.

```
unitEncoder :: (Applicative check, MonadError Text parse, Show r, Eq r) => r -> Encoder check parse () r `
```

For a more complicated route like login, you have to use `PathSegment` instead to indicate that the route to this endpoint is `localhost:8000/login`.

```
FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
```

Again, in this case, no information from the route is expected so it is
appropiate to use the `unitEncoder`.

Finally we'll add the route for the `FrontendRoute_Article` constructor.
If you recall from earlier, `FrontendRoute_Article` required that the encoder
can parse a `DocumentSlug` from the route. The idea is the routes
for articles look like

```
article/my-article-1
article/another-article-about-obelisk
article/my-great-route-tutorial
```

and so on. So our encoder needs to parse the `article` prefix and then
return the rest of the route as the `DocumentSlug`.

`PathSegment  "article` parses the prefix. The argument then needs to
be an encoder of type `Encoder check parse DocumentSlug PageName`

Here is the complete implementation:

```
FrontendRoute_Article ->
    PathSegment "article" $ singlePathSegmentEncoder . unwrappedEncoder
```

The second argument to `PathSegment` appears quite magical. Let's explain it bit by bit. We'll start with the types of the two encoders
involved.

```
singlePathSegmentEncoder :: Encoder check parse Text PageName
unwrappedEncoder :: (Wrapped a, Applicative check, Applicative parse)                    => Encoder check parse a (Unwrapped a)
```

A `singlePathSegmentEncoder` succeeds parsing a path if there is just one segement in the path and returns it as `Text`.

An `unwrappedEncoder` is used to wrap the `Text` value into the `DocumentSlug` newtype wrapper.

There two encoders are composed to produce the necessary
`Encoder check parse DocumentSlug PageName`.

The composition `.` is not the usual function composition, it's the
composition operator from `Control.Category`. `Encoder`s can be composed using this operator to create a larger encoder.

```
(.) :: Encoder check parse b c
    -> Encoder check parse a b
    -> Encoder check parse a c
```

## What is an `ObeliskRoute`?

We split our routes up into the backend routes and the frontend routes.
The backend routes are handled by the backend, the frontend routes by the
frontend. However, the frontend also needs to handle some additional
routes which are needed by obelisk to work properly. Therefore the
encoder that we need to make is a choice of either backend route
or a wrapped frontend route.

```
(R (Sum backendRoute (ObeliskRoute route)))
```

We won't worry about what these routes are for now but you can lift your
normal encoder with the `obeliskRouteSegment` function which provides
some sensible defaults.


## Using an `Encoder`

It's all well and good building up this encoder but we need to use it.
The `encode` and `decode` functions can be used to extract an encoder or decoder from an `Encoder`. The `Encoder` first has to be checked by using `checkEncoder`.


### Handling backend routes

The `obelisk-backend` package defines the helper functions to handle backend routes.
A backend currently only needs two functions to be defined.

```
data Backend backendRoute frontendRoute = Backend
  { _backend_routeEncoder :: Encoder (Either Text) Identity (R (Sum backendRoute (ObeliskRoute frontendRoute))) PageName
  , _backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  }
```

The `routeEncoder` is the `Encoder` that we defined in the `common` module. 
The `_backend_run` function describes how each route should be handled.

In our simple application there are just two backend routes.

```
serve $ \case
    (BackendRoute_Missing :/ ()) -> return ()
    (BackendRoute_Api     :/ _)  -> 
        runConduitServerM env $ serveSnapWithContext api context serve
```

The `serve` function expects a function of type `R backendRoute -> Snap ()` so
for each route we have to say how to interpret it in the `Snap` monad. The backend for the server is just a normal snap application so the API requests are just
passed directly to that and missing routes are ignored.

### Handling frontend routes

It order to use the `Encoder` in our applications we pass it to the
`runFrontend` function. The whole `frontend` of the application
is parameterised by the permitted routes.

```
runFrontend :: forall backendRoute route. Encoder Identity Identity (R (Sum backendRoute (ObeliskRoute route))) PageName -> Frontend (R route) -> JSM ()
```

Notice that the `runFrontend` function only expects the frontend to deal with the
user routes. It handles the `ObeliskRoute` part of the routes itself.

The way routes are handled by the frontend is slightly more complicated than the backend. The main entry point for the frontend is called `frontend`. It defines
seperate methods which will draw the header and body of each page.

```
data Frontend route = Frontend
  { _frontend_head :: !(forall js t m. ObeliskWidget js t route m => RoutedT t route m ())
  , _frontend_body :: !(forall js t m. ObeliskWidget js t route m => RoutedT t route m ())
  }
```

In order to understand how these deal with routes, you have to understand what the
point of the `RoutedT` monad is. 

#### RoutedT monad

The best way to think about the `RoutedT` monad is just like a normal `reflex-dom`
monad but with access to a `Dynamic` which tells you what the current route is. 

The most primitive way to get hold of this dynamic is with `askRoute`.

```
askRoute :: Routed t r m => m (Dynamic t r)
```

In our application however we use the less primitive helper function `subRoute_` which has the following type:

```
subRoute_ :: (...) => (forall a. r a -> RoutedT t a m ()) -> RoutedT t (R r) m () 
```

So we say for each frontend route in turn how to render it. Each of our pages has 
it's own function which describes how to render it so the dispatching function
is a simple mapping from the relevant route to the function which draws the
specific page.

```
    pages r = case r of
      FrontendRoute_Home     -> homePage
      FrontendRoute_Login    -> login
      FrontendRoute_Register -> register
      FrontendRoute_Article  -> article
      FrontendRoute_Settings -> settings
      FrontendRoute_Profile  -> pathSegmentSubRoute profile
      FrontendRoute_Editor   -> editor
      FrontendRoute_Warmup   -> warmup
```

### Constructing routes

The final topic we will cover in this tutorial is how to construct routes
and create redirects. If we recall from the previous section the current
route is simply help in a `Dyanamic`, so setting the current route amounts to
creating an `Event`, connecting it to the `Dynamic` and firing the event to
update the route. Firstly though we need to know the correct way to
construct a route.

Say for example I want to redirect the user to the page `//articles/nearly-at-the-end`, what will the route look like?

Routes are constructed directly using the data type defined in `common`.

```
FrontendRoute_Article :/ DocumentSlug "nearly-at-the-end"
```

Each segment of the route is separated by the `:/` combinator.

```
(:/) :: f a -> a -> R f
```

If we recall from earlier the definition of `R = DSum f Identity`, it makes
sense that `:/` is in fact a pattern synonym for the `DSum` constructor.

```
pattern (:/) :: f a -> a -> R f
pattern a :/ b = a :=> Identity b
```

### Using `setRoute`

The ability to set the route of the context is already abstracted over by
the class `SetRoute`. The `SetRoute` constraint is provided by the super
`ObeliskWidget` constraint which means that you can call it anywhere in the application. Therefore performing a redirect is as easy as calling `setRoute` on 
a suitable event.

This redirection is from the editor and redirects the page to the article 
after you are done editing. The `successE` event fires when the
server reports that the article has been saved so when that fires, the redirect
will happen.

```
setRoute $
    (\a -> FrontendRoute_Article :/ (DocumentSlug (Article.slug a)))
    . unNamespace
    <$> successE
```









