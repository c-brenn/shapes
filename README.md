# Shapes

A DSL for describing shapes and a web server that renders them as SVGs.

![Screenshot](/web/screenshot.png?raw=true "Screenshot")

The text box will highlight red on invalid input. The SVG is updated
automatically when the input is valid.

# Compile & run

```bash
$ stack setup && stack build
$ stack exec shapes
```

# About

This is a solution to a college assignment aimed at learning about implementing
and using DSLs.

It implements custom _Shapes_ DSL for describing the shapes and their
attributes. The web service is implemented using Scotty - a Haskell package that
provides a DSL for describing simple web services.

The frontend uses Elm, making use of FRP which we covered earlier in the course.

## Shapes DSL

A `Drawing` is `[Object]`, an `Object` is a triple: `([Transform], Shape, [Style])`.

### Transform

The following transforms are available:

```haskell
data Transform
  = Identity                -- does nothing
  | Translate Double Double -- translates x and y
  | Scale Double Doublie    -- scales x and y
  | Rotate Double           -- rotates by an angle in degrees
  | Skew Double Double      -- skews x and y by angles in degrees
  | Transform :+: Transform -- combines two transformations
```

In a `[Transform]` any `Translate` should come after any `Scale`, as in the
other order, the `Scale` will also be applied to the translation.

### Shape

The following shapes are available:

```haskell
data Shape = Empty
           | Circle
           | Square
```

### Style

The following styles are available:

```haskell
data Style = Fill Colour
           | Stroke Colour
           | StrokeWidth Double
```

### Colours

The following colours are available:


```haskell
data Colour
  = Black
  | Blue
  | Gray
  | Green
  | Navy
  | Purple
  | Red
  | RGB Int Int Int
  | Silver
  | White
  | Yellow
````
