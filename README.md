# dottie

# Why a new language?

There were a few goals here.  After working with F# for a while I enjoyed it quite a bit, but there were a few things that still bothered me.  First, I wanted the ability to add properties to records dynamically.  `with` syntax is great, but it'd be so much nicer if we could use it across similar record types or create dynamic anonymous record types addign a field to an existing record.

Additionally I'm looking at the features F# doesn't have.  Higher-kinded types is a big one.  On the Elm issues board, Evan mentioned a few ways that HKTs have been implemented.  I think another way could be just following function signatures.  Type classes allow you to tack functionality onto types.  But if you have the ability to add member functions directly onto your record, then perhaps that's all you need.  Much simpler, and it also fixes the issues from http://www.haskellforall.com/2012/05/scrap-your-type-classes.html because you're not doing *type* level coding; everything is just a value and the traits get carried around.  Also there's no extra concepts to understand like Scala's implicits: everything is *explicit* and you define the functions you want to add onto the record itself.  It's all just functions, which is what functional programming should be, right?

# How to make a new language?

## Step 0: Syntax

Have a general idea of your syntax.  This is an obvious step.  You don't have to hammer everything down right away.  Expect to change things in the future.  You'll start with the simple cases, and start with something that's fairly easy to parse.  That'll make it easier to get over the hump.  You'll have something instead of nothing.  Once you have something then you can add to it and it'll be real.  If you try to perfect it out of the box, you'll never get started.

## Step 0.1: Runtime

Another obvious thing, figure out what your runtime target is.  If you want to compile to native code, then you'll need some kind of garbage collection or the ability to manage memory.  I would like to do this eventually, but not yet: I don't have any particularly innovative ideas for memory management right now so time spent on that up front is just getting in the way.  Right now I'm building on an existing platform (JS) that has this built in.  I considered PyPy too, was interested in the whole auto-JIT from an interpreter idea, but looking at real world perf tests, it seems like V8 is still way faster than JITted PyPy.  So sticking with JS for first round.

## Step pre-1: JS

Figure out what the generated JS should look like.  Another farily obvious step, but this is the other thing that you'll have to do before you start coding.  If you don't know what you're generating, then you don't have a goal.  As above, start with the simple cases and expect some evolution.


## Step 1: FFI

Figure out what the ffi is going to look like.  My initial choice here was to have one .dottieffi file to one javascript file.  The JS would be the implementation, and the .dottieffi would contain the dottie spec of each function, to allow for type checking when calling those functions from another dottie module.  I mostly copied the approach from Purescript, though I think Purescript allows combining FFI and PS functions in a single module.  I chose to keep them separate initially for implimentation simplicity, but I think I'll maintain that approach because I think it's simpler for end users to understand too.

Why is this step 1?  Because before you can do anything, you're going to have to be able to write to the console.  The only way to write to the console is by calling JS console.log.  And you can't do that if you don't have an FFI.  So you'll need this before you can do anything else.

## Step 2: Create a sample FFI

Start with something simple.  Mine was string.concat.  First write the JS file that wraps string.concat, and then write the ffi file that declares that function.  Now you've got something to compile.

## Step 3: Write the compiler for the above.

The compiler can just copy the JS file over to the output directory (this gives you the opportunity to start defining your physical folder structure).  Then you can start writing the compiler for your ffi declaration file.  I went with the normal route of first tokenizing the chars, and then parsing the tokens.  As you do this you'll be defining your AST as the end result of your parser.  A functional language is really good for this, as defining ASTs with sum types / algebraic data types is much more intuitive than doing so with classes and inheritance.  I used F# since I'm familiar with .net, but may switch to Haskell or Purescript later.

I considered going the route of a parser generator like lex/yacc, but didn't.  I was curious how those work and have no experience with them.  I may look at them again in the future, but didn't go that way for a few reasons.  After reading some blogs, it seems that they're not real great if you want to generate useful error messages.  Also, it's just another level of abstraction on top of what I'm trying to do, and another thing to learn, and another thing to keep me from understanding what's going on under the hood.  So I think it's better just to start writing the compiler from scratch.

My feeling is that I'll probably end up wanting to combine the lexer and parser at some point because the lexer doesn't actually do much, so integrating it directly into the parser shouldn't be that much overhead, and it's sometimes impossible to know where token boundaries are without the context of the parser.

Anyway the AST for my ffi files should just contain the list of function names and the types that they accept.  So, it's a good opportunity to create a list of basic types your language deals with and how they relate.  Then you can write some tests on those results.
