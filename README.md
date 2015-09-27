hexagon
=======
A convenient library for operations on 2D hexagonal grids.

Issues
---------
Feel free to send pull requests or raise an [<i class="icon-bullseye"></i>**issue**](https://github.com/j-rock/hexagon/issues) here on GitHub.

Motivation
------------
Some fine folks pointed me to the Red Blob Games [**blog post**](http://www.redblobgames.com/grids/hexagons/#hex-to-pixel) on "Hexagonal Grids." The awesomeness contained on that page inspired me to transcribe some of the functionality therein into a Haskell library. The blog talks about some common coordinate systems for hexagonal grids. In the hexagon library, I've chosen the cube coordinate system (trading memory for ease of implementation).

Hopefully this can aid Haskell developers looking to implement a hexagonal grid game.

Prior Art
------------
Check out the totally awesome [**grid**](https://hackage.haskell.org/package/grid) library.

> **grid** enables you to think about the grid itself, while **hexagon** is more focused on

Examples
-------------
From here on out, assume the following import has been made:
```haskell
import Data.Hexagon
```

Creating a hexagonal coordinate is pretty simple. Use the **toHexagon** function and specify a coordinate system.
```haskell
--| The origin of a hexagonal grid in which the hexagons are oriented to
--| to have their tops be flat.
gridOrigin :: Hexagon FlatTop
gridOrigin = toHexagon Axial (0,0)

--| How about a pointy-top orientation?
pointyHexagon :: Hexagon PointyTop
pointyHexagon = toHexagon Axial (5,3)
```
Meanwhile, you can retrieve your coordinates back with the **fromHexagon** function in a similar manner.
```haskell
-- GHCi session
Data.Hexagon> fromHexagon Axial pointyHexagon
(5,3)
```
These **Hexagons** are looking a bit lonely. Let's get them some friends.
```haskell
Data.Hexagon> map (fromHexagon Axial) $ neighbors pointyHexagon
[(4,4),(4,3),(5,4),(5,2),(6,3),(6,2)]
```
Err, uhh, what does that mean? Break it down with the **neighbor** function.
```haskell
 With orientation FlatTop/PointyTop         Give me a Hexagon
                        ^^^                   ^^^^^^^^^^^
neighbor :: Orientation ori => EdgeDir ori -> Hexagon ori -> Hexagon ori
                               vvvvvvvvvvv                   vvvvvvvvvvv
                         And a shared edge             I'll get its neighbor
                         in relative terms

Data.Hexagon> fromHexagon Axial $ neighbor West pointyHexagon
(4,3)
```
Be careful not to mix **EdgeDirs** between **FlatTop**/**PointyTop** **Hexagons**.
```haskell
Data.Hexagon> fromHexagon Axial $ neighbor Top gridOrigin
(0,-1)
Data.Hexagon> fromHexagon Axial $ neighbor Top pointyHexagon

<interactive>:3:30:
    Couldn't match type ‘EastWestDir’ with ‘TopBottomDir’
    Expected type: EdgeDir PointyTop
      Actual type: TopBottomDir
    In the first argument of ‘neighbor’, namely ‘Top’
    In the second argument of ‘($)’, namely
      ‘neighbor Top pointyHexagon’
```
> The error message says it expected **EdgeDir PointyTop** but got **TopBottomDir**.
>
> **EdgeDir** is a type family which essentially makes the following mapping:
>
>    - **EdgeDir PointyTop** <~> **EastWestDir** (**West**, **SouthWest**, **NorthWest**, **East**, **SouthEast**, **NorthEast**)
>    - **EdgeDir FlatTop** <~> **TopBottomDIr** (**Top**, **TopLeft**, **TopRight**, **Bottom**, **BottomLeft**, **BottomRight**)

Anyway, back to connecting the lonely **Hexagons**...
```haskell
Data.Hexagon> gridOrigin `lineTo` pointyHexagon

<interactive>:7:21:
    Couldn't match type ‘PointyTop’ with ‘FlatTop’
    Expected type: Hexagon FlatTop
      Actual type: Hexagon PointyTop
    In the second argument of ‘lineTo’, namely ‘pointyHexagon’
    In the expression: gridOrigin `lineTo` pointyHexagon
```
Darn it. Can't mix Hexagons of **FlatTop** and **PointyTop**. No matter. Let's make a new **Hexagon**.
```haskell
Data.Hexagon> let flatHex = toHexagon Axial (5,3) :: Hexagon FlatTop
Data.Hexagon> map (fromHexagon Axial) $ gridOrigin `lineTo` flatHex
[(0,0),(1,0),(1,1),(2,1),(2,2),(3,2),(4,2),(4,3),(5,3)]
```
If you remember, **gridOrigin** represented (0,0) and flatTop represented (5,3). **lineTo** gives us an inclusive path between the two points.

# . . . More to come . . .


