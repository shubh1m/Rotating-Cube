module Main where

import Prelude
import Math as Math
import Data.Array
import Data.Foldable
import Data.Maybe
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (logShow, log)
import DOM
import Graphics.Canvas
import Signal
import Signal.DOM
--import Data.List
import Partial.Unsafe (unsafePartial)

oRate :: Number
oRate = 0.999

fric :: Number
fric = 0.0

vertices :: Array Point3D
vertices = [
    Point3D (-100.0) 100.0 (-100.0)
    ,Point3D 100.0 100.0 (-100.0)
    ,Point3D 100.0 (-100.0) (-100.0)
    ,Point3D (-100.0) (-100.0) (-100.0)
    ,Point3D (-100.0) 100.0 100.0
    ,Point3D 100.0 100.0 100.0
    ,Point3D 100.0 (-100.0) 100.0
    ,Point3D (-100.0) (-100.0) 100.0
]

data Point3D = Point3D Number Number Number

data Angle3D = Angle3D
    {qx :: Number
    ,qy :: Number
    ,qz :: Number
    }

angle :: Angle3D
angle = Angle3D
    {qx: Math.pi/4.0
    ,qy: Math.pi/3.0
    ,qz: Math.pi/4.0
    }

data Face = Face Point3D Point3D Point3D Point3D

faces :: Array (Array Int)
faces = [
    [0,1,2,3],
    [1,5,6,2],
    [5,4,7,6],
    [4,0,3,7],
    [0,4,5,1],
    [3,2,6,7]
]

project3Dto2D :: Point3D -> Angle3D -> Point3D
project3Dto2D (Point3D x y z) (Angle3D ang) =
    let xRotQz = x * Math.cos ang.qz + y * Math.sin ang.qz
        yRotQz = y * Math.cos ang.qz + x * Math.sin ang.qz
        zRotQz = z
        xRotQzQx = xRotQz
        yRotQzQx = yRotQz * Math.cos ang.qx + zRotQz * Math.sin ang.qx
        zRotQzQx = zRotQz * Math.cos ang.qx - yRotQz * Math.sin ang.qx
        xRotQzQxQy = xRotQzQx * Math.cos ang.qy + zRotQzQx * Math.sin ang.qy
        yRotQzQxQy = yRotQzQx
    in
        (Point3D xRotQzQxQy yRotQzQxQy zRotQz)

projectAll3Dto2D :: (Array Point3D) -> Angle3D -> (Array Point3D)
projectAll3Dto2D vert3D ang =
    let
        vert2D = map (\v -> project3Dto2D v ang ) vert3D
    in
        vert2D

defVal1 :: Maybe Number -> Number
defVal1 val = fromMaybe 0.0 val

{-
defVal :: Maybe Point3D -> Point3D
defVal (Point3D x y z) =
    let
        dx = fromMaybe 0.0 x
        dy = fromMaybe 0.0 y
        dz = fromMaybe 0.0 z
    in
        (Point3D dx dy dz)
-}

drawFace :: forall e. Context2D -> Face -> Eff (canvas :: CANVAS | e) Unit
drawFace ctx (Face v1 v2 v3 v4) = strokePath ctx do
        ctx <- setStrokeStyle "#000000" ctx
        ctx <- moveTo ctx x1 y1
        ctx <- lineTo ctx x2 y2
        ctx <- lineTo ctx x3 y3
        ctx <- lineTo ctx x4 y4
        void $ closePath ctx
    where
        Point3D x1 y1 z1 = v1
        Point3D x2 y2 z2 = v2
        Point3D x3 y3 z3 = v3
        Point3D x4 y4 z4 = v4

drawCube :: forall e. Context2D -> (Array Point3D) -> Eff (canvas :: CANVAS | e) Unit
drawCube ctx vert3D = strokePath ctx do
    for_ faces $ \face -> do
        drawFace ctx Face(v1 v2 v3 v4)
        where
            v1 = vert2D(face !! 0)
            v2 = vert2D(face !! 1)
            v3 = vert2D(face !! 2)
            v4 = vert2D(face !! 3)
            Face v1 v2 v3 v4 = v1 v2 v3 v4
    where
        vert2D = projectAll3Dto2D vert3D angle

transform :: Point3D -> Point3D
transform (Point3D x y z) =
    let nx = x + 200.0
        ny = x + 200.0
    in
        (Point3D nx ny z)

--main :: forall eff. Eff(canvas :: CANVAS, dom :: DOM, console :: CONSOLE, timer :: TIMER | eff) Unit
main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "thecanvas"
    ctx <- getContext2D canvas
    animFrame <- animationFrame
    ctx <- setFillStyle "#0000FF" ctx
    --ctx <- fillRect{x: 0.0, y: 0.0, w: 500.0, h: 500.0} ctx
    drawCube ctx vertices
    --log("working")
