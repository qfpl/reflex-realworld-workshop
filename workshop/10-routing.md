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

## TODO: What is an `ObeliskRoute`

## Using an `Encoder`

It's all well and good building up this encoder but we need to use it.
The `encode` and `decode` functions can be used to extract an encoder or decoder from an `Encoder`. The `Encoder` first has to be checked by using `checkEncoder`.

It order to use the `Encoder` in our applications we pass it to the
`runFrontend` function. The whole `frontend` of the application
is parameterised by the permitted routes.

I don't really understand how this bit works yet. There is this `RoutedT` monad which seems to play a role.

Likewise, the backend routes need to be handled by the backend. Therefore the Encoder is passed to the backend and something deals with that there. Again, I don't know yet.s

### Constructing routes

### Using `setRoute`s









