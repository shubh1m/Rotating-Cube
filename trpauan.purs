module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Math (pi, cos, sin)
import Graphics.Canvas (CANVAS, Context2D, getCanvasElementById, getContext2D, setFillStyle, fillRect, moveTo, lineTo, withContext, setStrokeStyle, beginPath, closePath, stroke, CanvasElement, getCanvasWidth, getCanvasHeight, clearRect)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import DOM.HTML.Window (requestAnimationFrame)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, Ref)


-- These are functions which are useful in general
clearCanvas :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Unit
clearCanvas canvas = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  ctx <- getContext2D canvas
  void $ clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }


loopAnimation :: forall e state.
  Window ->
  Ref state ->
  state ->
  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->
  Eff (ref :: REF, dom :: DOM | e) Unit
loopAnimation window ref state step =
  void $ requestAnimationFrame
    do loopAnimation window ref state step
       state <- readRef ref
       state <- step state
       writeRef ref state
    window

withAnimation :: forall e state.
  state ->
  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->
  Eff (ref :: REF, dom :: DOM | e) Unit
withAnimation state step = do
  window <- window
  ref <- newRef state
  loopAnimation window ref state step


withAnimateContext :: forall e state.
  String ->
  state ->
  (Context2D -> state -> Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) state) ->
  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
withAnimateContext name state draw = do
  canvas <- getCanvasElementById name

  case canvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      withAnimation state \state -> do
        clearCanvas canvas
        draw ctx state
    Nothing -> pure unit


withStroke :: forall e.
  Context2D ->
  String ->
  (Context2D -> Eff (canvas :: CANVAS | e) Context2D) ->
  Eff (canvas :: CANVAS | e) Context2D
withStroke ctx color draw = withContext ctx do
  ctx <- setStrokeStyle color ctx
  ctx <- beginPath ctx
  ctx <- draw ctx
  ctx <- closePath ctx
  stroke ctx


-- Define 3D and 2D Point objects
newtype Point3D = Point3D { x :: Number, y :: Number, z :: Number }

newtype Point2D = Point2D { x :: Number, y :: Number }

qx :: Number
qx = pi / 4.0

qy :: Number
qy = pi / 3.0

qz :: Number
qz = pi / 4.0

project :: Point3D -> Point2D
project (Point3D { x, y, z }) =
  let xRotQz = x * (cos qz) + y * (sin qz)
      yRotQz = y * (cos qz) - x * (sin qz)
      yRotQzQx = yRotQz * (cos qx) + z * (sin qx)
      zRotQzQx = z * (cos qx) - yRotQz * (sin qx)
      xRotQzQxQy = xRotQz * (cos qy) + zRotQzQx * (sin qy)
  in
    Point2D { x: xRotQzQxQy, y: yRotQzQx }

drawLine :: forall e. Context2D -> Point2D -> Point2D -> Eff (canvas :: CANVAS | e) Context2D
drawLine ctx (Point2D from) (Point2D to) = do
  ctx <- moveTo ctx from.x from.y
  lineTo ctx to.x to.y


newtype Cube = Cube
  { x :: Number
  , y :: Number
  , z :: Number
  , size :: Number
  , color :: String }

drawCube :: forall e. Context2D -> Cube -> Eff (canvas :: CANVAS | e) Context2D
drawCube ctx (Cube { color, x, y, z, size }) = do
  let half = size / 2.0
  let v1 = project $ Point3D { x: x - half, y: y - half, z: z - half }
  let v2 = project $ Point3D { x: x - half, y: y + half, z: z - half }
  let v3 = project $ Point3D { x: x - half, y: y - half, z: z + half }
  let v4 = project $ Point3D { x: x - half, y: y + half, z: z + half }
  let v5 = project $ Point3D { x: x + half, y: y - half, z: z - half }
  let v6 = project $ Point3D { x: x + half, y: y + half, z: z - half }
  let v7 = project $ Point3D { x: x + half, y: y - half, z: z + half }
  let v8 = project $ Point3D { x: x + half, y: y + half, z: z + half }
  withStroke ctx color \ctx -> do
    ctx <- drawLine ctx v1 v5
    ctx <- drawLine ctx v5 v6
    ctx <- drawLine ctx v6 v2
    ctx <- drawLine ctx v2 v1

    ctx <- drawLine ctx v3 v7
    ctx <- drawLine ctx v7 v8
    ctx <- drawLine ctx v8 v4
    ctx <- drawLine ctx v4 v3

    ctx <- drawLine ctx v1 v3
    ctx <- drawLine ctx v5 v7
    ctx <- drawLine ctx v6 v8
    drawLine ctx v2 v4


drawScene :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawScene ctx = do
  ctx <- setFillStyle "rgb(122,230,232)" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 500.0, h: 400.0 }


main :: forall e. Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
main =
  let
    state = { x: 300.0
            , y: 600.0
            , dx: 1.0
            , dy: 1.0 }
  in
    withAnimateContext "thecanvas" state \ctx state -> do
      ctx <- drawScene ctx
      void $ drawCube ctx $ Cube { x: state.x, y: state.y, z: 0.0, size: 200.0, color: "rgb(0,0,0)" }

      -- Update the state
      pure $ state { x = state.x + state.dx, y = state.y + state.dy }