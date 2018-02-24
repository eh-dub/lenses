# Lenses

## Bootstrap vs CSS Grid
I've created the first example of a bijection between Bootstrap and CSS Grid. There are probably still bugs. The bootstrap code is in the folder named `bootstrapStyle` and the native CSS code is in `nativeStyle` (lambda style anyone?).

I'm realizing there are a lot of freedoms in how the native style CSS/HTML is written and which will require some thought juice as to how to justify our decisions.

One thing that I've noticed is that Bootstrap is backed by Flexbox under the hood which is a useful model of 1D layouts that "wrap" into 2D. 

## Possible domains for lenses in the web world
- jQuery to native impls
- callbacks to promises (or w/e)
- XMLHTTPRequest to fetch (https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- Similarly flavored frontend frameworks e.g. Angular to Vue to React
- JavaScript to ReasonML
- CoffeeScript <> JavaScript
- CSS <> SASS <> LESS
- HTML Template syntax <> Other HTML Template syntax


## TODO
### Bootstrap
- Create more toy examples
- Grab snippets of "production" bootstrap and rewrite it 
- compare notes with: https://hacks.mozilla.org/2017/04/replace-bootstrap-layouts-with-css-grid/

### Callback code to promises
- Create a toy example

### jQuery to Native implementation
- Create a toy example
- http://youmightnotneedjquery.com/

### Angular to Vue to React (and all other flavours of frontend frameworks)
- Create a toy example (these are harder b/c you need to set up the dependencies too, but this is ignorable at first)
