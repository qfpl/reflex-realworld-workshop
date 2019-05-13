# Article View

In this exercise, we are going to display a collection of things on the screen and use eventwriter / holdDyn ourselves.

Learning outcomes:
- Training wheels are off!
- There is a new thing to stumble upon, too. Good luck! :)

Checkout the login page on http://localhost:8000/article/how-to-throw-an-awesome-party and open up the workshop code in frontend/src/Frontend/Article.hs.

The sample html from the spec looks like the html at the bottom. You'll create everything yourself this time to give yourself a proper experience.

I recommend that you take the following path:

- Grab the document slug from the route using `askRoute :: Routed t r m => m (Dynamic t r)`
- Load the article using the slug and (pbE <> void (updated tokDyn)) (same bug as tags)
- Display the article details once the article has loaded (holdDyn a Maybe Article).
- Once the article is loaded, load the comments.
- Display the comments
- Make the create comment form
- Consider carefully what you should do when the comment creation is successful. How do you do that?
- Remember our good friend Endo from the [foldDyn intro](./01-foldDyn.md).
- Once you've got create sorted, delete is easy.
- Peek at the example if you get too stuck.

```html
<div class="article-page">

  <div class="banner">
    <div class="container">

      <h1>How to build webapps that scale</h1>

      <div class="article-meta">
        <a href=""><img src="http://i.imgur.com/Qr71crq.jpg" /></a>
        <div class="info">
          <a href="" class="author">Eric Simons</a>
          <span class="date">January 20th</span>
        </div>
        <button class="btn btn-sm btn-outline-secondary">
          <i class="ion-plus-round"></i>
          &nbsp;
          Follow Eric Simons <span class="counter">(10)</span>
        </button>
        &nbsp;&nbsp;
        <button class="btn btn-sm btn-outline-primary">
          <i class="ion-heart"></i>
          &nbsp;
          Favorite Post <span class="counter">(29)</span>
        </button>
      </div>

    </div>
  </div>

  <div class="container page">

    <div class="row article-content">
      <div class="col-md-12">
        <p>
        Web development technologies have evolved at an incredible clip over the past few years.
        </p>
        <h2 id="introducing-ionic">Introducing RealWorld.</h2>
        <p>It's a great solution for learning how other frameworks work.</p>
      </div>
    </div>

    <hr />

    <div class="article-actions">
      <div class="article-meta">
        <a href="profile.html"><img src="http://i.imgur.com/Qr71crq.jpg" /></a>
        <div class="info">
          <a href="" class="author">Eric Simons</a>
          <span class="date">January 20th</span>
        </div>

        <button class="btn btn-sm btn-outline-secondary">
          <i class="ion-plus-round"></i>
          &nbsp;
          Follow Eric Simons <span class="counter">(10)</span>
        </button>
        &nbsp;
        <button class="btn btn-sm btn-outline-primary">
          <i class="ion-heart"></i>
          &nbsp;
          Favorite Post <span class="counter">(29)</span>
        </button>
      </div>
    </div>

    <div class="row">

      <div class="col-xs-12 col-md-8 offset-md-2">

        <form class="card comment-form">
          <div class="card-block">
            <textarea class="form-control" placeholder="Write a comment..." rows="3"></textarea>
          </div>
          <div class="card-footer">
            <img src="http://i.imgur.com/Qr71crq.jpg" class="comment-author-img" />
            <button class="btn btn-sm btn-primary">
             Post Comment
            </button>
          </div>
        </form>

        <div class="card">
          <div class="card-block">
            <p class="card-text">With supporting text below as a natural lead-in to additional content.</p>
          </div>
          <div class="card-footer">
            <a href="" class="comment-author">
              <img src="http://i.imgur.com/Qr71crq.jpg" class="comment-author-img" />
            </a>
            &nbsp;
            <a href="" class="comment-author">Jacob Schmidt</a>
            <span class="date-posted">Dec 29th</span>
          </div>
        </div>

        <div class="card">
          <div class="card-block">
            <p class="card-text">With supporting text below as a natural lead-in to additional content.</p>
          </div>
          <div class="card-footer">
            <a href="" class="comment-author">
              <img src="http://i.imgur.com/Qr71crq.jpg" class="comment-author-img" />
            </a>
            &nbsp;
            <a href="" class="comment-author">Jacob Schmidt</a>
            <span class="date-posted">Dec 29th</span>
            <span class="mod-options">
              <i class="ion-edit"></i>
              <i class="ion-trash-a"></i>
            </span>
          </div>
        </div>

      </div>

    </div>

  </div>
</div>
```

## Next Page

Well done, you finished the workshop! If you are keen for more, catch the ideas in [??-bonus](??-bonus.md)
