# MiLang

The language by a crazy man, because he wanted to.

# What the langeuage shall be

## Functional core

~~Side effects are not done within actors themselves, but by calling out
to 'commands'. commands then report back to the actors.~~

Turns out that was untennable. Trying to make tasks the core, where a task
would wait on a message was looking verbose. Forcing all sending and processing
of the receipt as a massive state machine is metric ton of boilerplate.

So, we'll go with a bit of erlang and a bit of f#

## Actor Centric

~~Each module can be an actor, that can create other actors, or exit, at
any time.~~

Again, splitting this up ended up falling by the wayside. It made things far too
complex to actually implement and write.

So, an Actor is called a 'MessageProcessor' (a la F#).

```milang
-type MessageProcessor exit message.
```

A message processor is created thusly:

```milang
Concurrency.message_processor : (message -> exit) -> MessageProcessor exit message.
```

Essentially, we now have a process (pid, actor) we can send data of type
`message` to. If/when the function returns, the process exits with a reason of
type `reason`.

One of the things I've landed on is a function is never equal to another
function (since it's rather undecidable), so because a function is used to create
a message processor, we can create many different processors with the same
function.

Anyway, now that we have a process, let's fill out how we actually communicate
with it.

```milang
Concurrency.send : MessageProcessor exit message -> message -> Unit.
```

So yeah, the only thing we can do with that is realize that we get Unit back.
Oh goody. Of course, if the message happened to have a message processor within
it, it could use that to send a reply.

So here's the question. It's not very 'functional' to have an await in the middle
of a function, since passing in the 'same parameters' could give us a different
result. Eg, if the state of a message processor increments 1 each time it gets
a request, calling `send` to it twice will get 2 different integers.

However, requiring some function / message pass-back can get into callback hell.
Massive boilerplate, and all sorts of ugliness.

So: let's comprimise a bit.

1. MessageProcessors can never equal each other due to using a function within
   their internal definition. This means, technically, the recursion a message
   processor uses means we always have a new processor.
2. We're already going to be checking types galore, and message passing will be
   exactly the same. A message processor will not be allowed to send invalid
   types out, and this is checked at compile time.

```milang
-module Concurrency exposing [
	, MessageProcessor
	, message_processor
	, send
	, call
	, asyncCall
	, reap
	].

-type MessageProcessor exit message | MessageProcessor (message -> exit).

message_processor : (message -> exit) -> MessageProcessor exit message.

send : message -> MessageProcessor exit message -> ().

call : message -> MessageProcessor exit message -> reply.

asyncCall : message -> MessageProcessor exit message -> Seed reply.

reap : Timeout -> Seed reply -> Result TimeoutError reply. -doc but does it blend?
```

So, with reap we know or at least can figure out the type it should return. and
we know the message we can send to the actor. Do we require the actor to send
a reply immediately? Well, no, because it might need to ask something else. eg,
webclient -> webserver -> database -> and all the way back.

I'm also not sure if I'll have access to an actor...unless I make one.

F# uses the concept of a 'reply channel'. It's something that's kinda magically
created when using a call. ...And it all kinda works?

Well, f# has the concept of tasks, and has let! and match and other banged!
things to do asychronouse tasks.

Okay, so we have the concept of a mailbox that can be posted to, but in erlang
and f# only one thing can read from it, and that's the process that created it.

So, let's try something a little...different.

```milang
-module ConfigHolder exposing [
	, new
	, set_goober
	, set_pile
	, goober
	, pile
	].

-import Concurrency.

-type ConfigMessage [
	, SetGoober String
	, GetGoober (Mailbox String String)
	, SetPile Int
	, GetPile (Mailbox Int Int)
	].

-alias Config = Concurrency.Mailbox Unit ConfigMessage.

new : File.ConfigFile -> Config.
new configFile ->
	Concurrency.new_mailbox (mailbox_loop configFile).

set_goober : Config -> String -> Unit.
set_goober config val ->
	Concurrency.send config ( SetGoober val ).

set_pile : Config -> Int -> Unit.
set_pile config val ->
	Concurrency.send config ( SetPile val).

goober : Config -> String.
goober config ->
	Concurrency.request config (GetGoober, Concurrency.replyChannel).

pile : Config -> Int.
pile config ->
	Concurrency.request cofig (GetPile, Concurrency.replyChannel).

mailbox_loop : File.ConfigFile -> Concurrency.Mailbox Unit ConfigMessage -> Unit.
mailbox_loop config_file mailbox ->
	case Concurrency.receive_first mailbox of [
		, GetGoober reply ->
			Concurrency.reply reply (File.magic_read "goober")
			|> always (mailbox_loop config_file mailbox)
		, SetGoober new_goober ->
			File.magic_write "goober" new_goober
			|> always (mailbox_loop config_file mailbox)
		, GetPile reply ->
			Concurrency.reply reply (File.magic_read "pile")
			|> always (mailbox_loop config_file mailbox)
		, SetPile new_pile
			File.magic_write "pile" new_pile
			|> always (mailbox_loop config_file mailbox)
	].
```

So this is very much like the F# style. We have an actor with an initial function
to evaluate. It's that function's job to actually read things from the mailbox.
When the actor does, it can either recurse with the mailbox, or actually return
a value.

"but if we read a message from the mailbox twice, we'll get different results!
That's not functional!" Thing is, part of the mailbox definition is a function,
and since a function is never equal to another function (even itself), it is
never the same value being called. (I'm likely breaking some fundamental tenant
here, but it helps me sleep at night). (Also, erlang itself does let a function
equal another function).

So this lets us build up some things.

* A Task is an actor that never receives messages other than replies to messages
  it sends. Essentially a task is a `Mailbox return Never`.
* A reply channel receives exactly 1 message of a specific type, and exits.
* timeouts for replies.
* asynchonous replies; the initial call just gives  token that can be 'reaped'
  later. The later reap can also timeout (but keeps the token valid).
* We need to build a way to 'kill' actors outside the message passing semantics.
* We _will not_ have globally registered names. There are no hidden arguments.
  The only state a function knows about is that which is passed in. So yes, if
  you want to talk to a database, you need to pass the database connection actor
  to each thing that needs to talk to the database.

## Regular

There shall be 1, and only 1, way to express the vast majority of ideas.
This means that rather than a function being declarable differently 
depending on if it is anonymouse or not, there is only 1 way to do it.
Symbols have exactly 1 way to be written, and so forth.

Things in the language that are lists are presented as literal lists. Same as
records. For example, the constructors for a type is wrapped in a
`[,Const1,Const2]` just like a user would create a list of `[,1,2,3]`. Bindings
are declared as `{ , var1 = expres1, var2 = expres2 }` just as a user would
create a record of `{, var1 = VarType, var2 = Var2Type }`.

## Erlang runtime

Because I'm lazy.

# The language proper

Is a work in progress. What was here was wrong by the time it got pushed.


