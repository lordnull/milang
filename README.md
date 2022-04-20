# MiLang

The language by Micah Warren, because he wanted to.

# What the langeuage shall be

## Functional core

Side effects are not done within actors themselves, but by calling out
to 'commands'. commands then report back to the actors.

## Actor Centric

Each module can be an actor, that can create other actors, or exit, at
any time.

## Regular

There shall be 1, and only 1, way to express the vast majority of ideas.
This means that rather than a function being declarable differently 
depending on if it is anonymouse or not, there is only 1 way to do it.
Symbols have exactly 1 way to be written, and so forth.

## Erlang runtime

Because I'm lazy.

# The language proper

A program is made of Actors Modules, Command Modules, and Library Modules.

A Module is a series of statements, each statement ending in a dot `.`.

A statement declares a module type, a data type, a data typeclass, an alias,
or a function.

## module type

The first line of a file must be module statement. The module the module
name and list of module exports.

```file: src/Actor/Example.milang
-actor Actor/Example
	, start
	, stop
	, Performance
	.
```

Note the lack of enclosing brackets or braces. Lists and compound data
types are started with thier deliniator and terminated with a dot '.'.
This removes questions about hanging deliniators.

## literals

The usual suite of literals.

### Integer

* `2`
* `-7`
* `53234`
* `10_000_001`

### Float

* `0.1`
* `1.0`
* `-3.465e37`
* `-34_987_223.567_98`

### String

Strings are utf8.

* "hello"
* "î"
* ""

## lexical structures, or something.

These are the building blocks of the language. I'll do it by example.

### Hello world

```file: HelloWorld.milang
-docstart.
We declare this module as an actor. It will expose a 'main' function. There 
is nothing special about the 'main' function usually. For system start up,
however, you need to specify an actor module that exposes a function named
`main` with the signature `List String -> Actor stateType msgType`.
-docend.
-actor HelloWorld , main.

-docstart.
We import some supporting modules. Sys.Print is a command module that allows
us to send text to output streams.
-docend.
-import Sys.Print exposing , ln.

-docstart
Declare some types used later. Well, not in this case, but eventually.
-docend.
-type State | State
-type Msg | Msg

-docstart
The main function that will spawn and die.
-docend.
main : List String -> Actor State Msg
main _ ->
	Actor.new init update ,. .

-docstart.
A helper function that defines the initial state of the actor. In this case
it also defines the death.
-docend.
init : Actor.Life State Msg
init ->
	cmd = ln "hello world!"
	state = State
	Actor.normalExit state cmd

-docstart.
If our actor lived long enough, it would be able to mutate state based on
received messages here.
-docend.
update : Msg -> State -> ; State ; Command Msg .
update _ _ ->
	cmd = ln "will never be called"
	state = State
	Actor.normalExit state cmd
```

Okay, things I've realized: there's no need to declare a module as an actor
over a library. Maybe over command. This is because for the actor things to
work I need to define an actor command / library structure anyway.

### Countdown!

Given an input of a number, print a countdown. We'll do this in a silly way.

```file:Countdown.milang
-module Countdown exposing , main.

-import Sys.Print exposing , lnAndThen .

-alias State | Int.
-type Msg | Dec.
-type ExitReasons | NoNumbers | TooManyNumbers | InvalidNumber.

main : , String . -> Actor State Msg
main , text . ->
	init text.
main , . ->
	Actor.errorExit NoNumbers Cmd.none.
main _ ->
	exitReason = TooManyNumbers
	Actor.errorExit exitReason Cmd.none .

init : String -> Actor State Msg ExitReason
init text ->
	String.parseInt text
	|> Maybe.map buildNewActor
	|> Maybe.withDefault (Actor.errorExit InvalidNumber Cmd.none).

buildNewActor : Int -> Actor State Msg ExitReasons
buildNewActor count ->
	Actor.new count update , lnAndThen "Beginning countdown" Dec .

update : Msg -> State -> Actor.Life State Msg ExitReasons
update Dec state when state < 0 ->
	Actor.normalExit 0 , lnAndThen "Already blasted off" Dec .
update Dec state when state = 0 ->
	Actor.normalExit 0 , lnAndThen "Blast off!" Dec .
update Dec state ->
	output = String.format "~{count}!" : count = String.fromInt state
	newState = state - 1
	Actor.loop newState , lnAndThen output Dec.
```

Things seem messy. I'm still not 100% sold on the "delimiters are the
openers and all things are dot terminated" idea. Trying to cram the pattern
matting into the function heads feels a bit awkward.

The implicit let seems like it could use some work, but I can't really put
my finger on what.

The actor loop is shaping up. I should likley just do batch commands rather
than accept a list.

## Commands

This is the hard part. This is were we're getting into erlang land. So,
first let's do some erlang types.

We'll treat erlang terms as we would json: generic stuff we can parse on
later.

```file: System.Erlang
-module System.Erlang

-docstart.
Ooh, a new keyword? This is meant to indicate foreign data that only the
module that defines it may be able to address.
-opaque Pid.
-opaque Port.
-type Tuple | Tuple Int , ErlangTerm .
-type alias Int | Int.
-type alias Float | Float.
-type alias Binary | Binary.
-docstart.
And so forth.
-docend.

foreignCall : Symbol -> Symbol -> , ErlangTerm . -> Result ErlangTerm ErlangTerm
foreignCall module function args ->
	-doc Here we have the hard bit: we can't define this in terms of our
	-doc own code. Okay, an escape hatch. We'll do the elm thing were only
	-doc 'blessed' modules can do this.
	-erlangstart.
	try erlang:apply(module, function, args) of
		Result ->
			{ok, Result}
	catch
		W:Y:S ->
			{error, {W, Y, S}}
	end.
	-erlangend.
```

So this tells me that the fundamental parts that actually make this all work
are going to be native erlang wrapped in modules the expose milang types
and functions. Anything you want to do that may require raw erlang / system
access will need to be defined in some way. So the whole 'command'
seperation does make sense.

So let's try file operations.

```file: System.File.milang
-docstart.
The 'erlang' declaration instead of 'module' indicates functions will be
written in native erlang. This is 'bad mojo', but needed for things to work.
-docend.
-erlang System.File exposing , open, close, read, write , moveTo .

-opaque FileHandle
-type PosixError | ...
-type Mode | Read | Write | Both .

open : Mode -> Binary -> (Result PosixError FileHandle -> msg) -> Cmd msg
open mode filename msgBuilder =
	Cmd.execute msgBuilder (do_open mode filename)

do_open mode filename ->
	open_native (encode_mode mode) (encode_binary filename)
	|> decodeFileOpen rawTerms
	

-docstart
So here's how we'll do it. We have a declartion as 'native'. Under the hood
this will be wrapped in a 'try catch'. it is up to the callers to properly
decode the output.
-docend.
-erlang open_native Mode FileName = file:open(Mode, FileName).
```

Okay, I'm doing a lot of work on the language level to optimize avoid using
erlang:apply/2,3. This is likely not a good route to take, so we'll just
have a generic call_erlang/3 which will create a command for our erlang
function interface. Essentially, for this to work, there's going to be a
compiler trickery. I don't need to build how to write the erlang here.
System.Erlang* will be a 'virtual module' that is backed into the compiler.

```file:System.Erlang.milang
-module System.Erlang exposing , call, Term. .

-type Term 
	, Pid 
	, Port 
	, Integer Int
	, Float Float
	, Nil
	, Cons Term Term
	, Tuple (List Term)
	, Binary Binary
	, Atom Symbol
	, Reference 
	, Fun (List Term) Term
	, Map (List ; Term ; Term.)
	.

call : Symbol -> Symbol -> , Term . -> (Term -> msg) -> Cmd msg
call module funName args msgBuilder ->
	Cmd.identiy (msgBuilder Nil)
```
```file:System.Erlang.Decode.milang
-module System.Erlang.Decode exposing
	, Decoder 
	, pid
	, port
	, integer
	, float
	, nil
	, cons
	, tupleAt
	, binary
	, atom
	, reference
	, fun
	, mapKeyVal
	. .

-import System.Erlang exposing Term(..)

-alias DecodeError StringBuilder

-type Decoder a | Term -> Result DecodeError a

pid : Decoder Pid
pid ->
	pid_decoder

-doc and so forth.

```







-type TypeName argName1 argName2
	| VarianceName1
	| VarianceName2 SomeOtherType
	| VarianceName3 (SomeThirdType argName1)
	| VarianceName4 argName2
	| VarianceName5 ( SomeOtherType -> argName1 -> argName2)
	.

-alias TypeName argName1 | SomeThirdType argName1

-alias TypeName | [ SomeThirdType SomeOtherType ]

-alias TypeName | { SomeKeyType :: SomeValueType }

-class TypeName argName1 argName2
	| TypeName -> argName2
	| TypeName -> SomeOtherType
	| SomeThirdType argName1 -> argName1 -> SomeOtherType

functionName : T1 -> T2 a1 a2 -> T3 (T2 a3 a4) -> (T1 -> T2) -> Out
functionName a b c d ->
	callSomeFunc a b
	|> callSomeOtherFunc c d

functionName2 : Out
functionName2 -> LiteralCall 55

|>inFixName: T1 -> arg2 -> T3 arg2
|>inFixName a  b ->
	doit a b

main : T1 -> T2 -> T3 -> Program StateType MsgType
main a b c ->
	buildFirstState a b c

loop : MsgType -> StateType -> ProgramProgress StateType (Cmd MsgType)

subscriptions : StateType -> Sub MsgType

{doc
a block of "text" output specifically for documentation purposes.
}

-- line comment

{-
nestable block comments.
}

unit : ()
tuple : ( T , ...)
nil : []
cons : [T | T2]
list : [T, ...]
string : "string"
binary : #bit string syntax#
character : $a | $ Integer
symbol : 'string and so forth'


