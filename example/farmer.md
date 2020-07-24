The Wolf, Goat, and Cabbage Problem
===================================

This example shows how to use `lustre-sally` to solve a simple puzzle.
This example is derived from one of the tests for `jkind`, which is
another model-checker that uses Lustre as its input language.

We solve the "Wolf, goat, and cabage problem", described on
[Wikipedia](https://en.wikipedia.org/wiki/Wolf,_goat_and_cabbage_problem)
as follows:

> Once upon a time a farmer went to a market and purchased a wolf, a goat, and a
> cabbage. On his way home, the farmer came to the bank of a river and rented a
> boat. But crossing the river by boat, the farmer could carry only himself
> and a single one of his purchases: the wolf, the goat, or the cabbage.
>
> If left unattended together, the wolf would eat the goat, or the goat
> would eat the cabbage.
>
> The farmer's challenge was to carry himself and his purchases to the far bank
> of the river, leaving each purchase intact. How did he do it?

To model the problem, we first declare two Lustre enumeration types:
```lustre
type Character  = enum { Farmer, Wolf, Goat, Cabbage };
type Side       = enum { Left, Right };
```
Type type `Character` represents the characters in the story, and the type
`Side` models the characters' locations.

We start by encoding how moving characters affects their location:
```lustre
node boat(move, character: Character) returns (location : Side);
let
  location =
    Left ->
      if pre(move) = character
         then pre(otherSide(location))
         else pre(location);
tel;

node otherSide(side : Side) returns (other : Side);
let
  other = if side = Left then Right else Left;
tel;
```
The parameters to `boat` are two characters: the character being moved,
and the character whose location we'd like to know.  If these coincide,
then the caracter moves to the other side, otherwise it stays in place.



```lustre
node main(move : Character)
  returns (wolfLoc, goatLoc, cabbageLoc, farmerLoc : Side);
var

    solved        : bool;
    nothingEaten  : bool;
    validMove     : bool;
    prop          : bool;
let
    --%MAIN;

    farmerLoc     = Left -> pre(otherSide(farmerLoc));
    wolfLoc       = boat(move, Wolf);
    goatLoc       = boat(move, Goat);
    cabbageLoc    = boat(move, Cabbage);

    nothingEaten  = (wolfLoc = goatLoc    => farmerLoc = goatLoc) and
                    (goatLoc = cabbageLoc => farmerLoc = cabbageLoc);

    validMove     = (move = Wolf    => farmerLoc = wolfLoc) and
                    (move = Goat    => farmerLoc = goatLoc) and
                    (move = Cabbage => farmerLoc = cabbageLoc);

    solved        = wolfLoc    = Right and
                    goatLoc    = Right and
                    cabbageLoc = Right and
                    farmerLoc  = Right;

    prop = not (historically(nothingEaten and validMove) and solved);
    --%PROPERTY prop;
tel;
```


```lustre
node historically(x : bool) returns (holds : bool);
let
  holds = x and (true -> pre holds);
tel;
```


