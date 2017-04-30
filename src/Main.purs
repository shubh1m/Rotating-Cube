module Main where

import Prelude
import Math as Math
import Data.Array
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import DOM
import Graphics.Canvas
import Signal
import Signal.DOM
import Partial.Unsafe (unsafePartial)

oRate :: Number
oRate = 0.999

fric :: Number
fric = 0.0

data Point3D = Point3D
	{	x :: Number
	,	y :: Number
	,	z :: Number
	}

vertices :: Array Point3D
vertices =
	[ Point3D {x:(-100.0), y:100.0, z:(-100.0)}
		,Point3D {x:100.0, y:100.0, z:(-100.0)}
		,Point3D {x:100.0, y:(-100.0), z:(-100.0)}
		,Point3D {x:(-100.0), y:(-100.0), z:(-100.0)}
		,Point3D {x:(-100.0), y:100.0, z:100.0}
		,Point3D {x:100.0, y:100.0, z:100.0}
		,Point3D {x:100.0, y:(-100.0), z:100.0}
		,Point3D {x:(-100.0), y:(-100.0), z:100.0}
	]

data Angle3D = Angle3D
	{	qx :: Number
	, qy :: Number
	, qz :: Number
	}

angle :: Angle3D
angle = Angle3D{
		qx: Math.pi/4.0
	,	qy: Math.pi/3.0
	,	qz: Math.pi/4.0
}

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
project3Dto2D (Point3D p) (Angle3D ang) = 
	let	xRotQz = p.x * Math.cos ang.qz + p.y * Math.sin ang.qz
		yRotQz = p.y * Math.cos ang.qz + p.x * Math.sin ang.qz
		zRotQz = p.z
		xRotQzQx = xRotQz
		yRotQzQx = yRotQz * Math.cos ang.qx + zRotQz * Math.sin ang.qx
		zRotQzQx = zRotQz * Math.cos ang.qx - yRotQz * Math.sin ang.qx
		xRotQzQxQy = xRotQzQx * Math.cos ang.qy + zRotQzQx * Math.sin ang.qy
		yRotQzQxQy = yRotQzQx
	in
		Point3D {x:xRotQzQxQy, y:yRotQzQxQy, z:p.z}

drawCube :: Context2D -> Eff (canvas :: CANVAS) Context2D
drawCube ctx =
	fillPath ctx $ do
		moveTo ctx 100.0 100.0
		lineTo ctx 200.0 200.0
		lineTo ctx 150.0 150.0
		closePath ctx

transform :: Point3D -> Point3D
transform (Point3D p) =
	let	nx = p.x + 200.0
		ny = p.x + 200.0
	in
		Point3D {x:nx, y:ny, z:p.z}

{-forall eff.Eff( canvas :: CANVAS
						, dom :: DOM 
						, timer :: TIMER
						| eff
						) Unit
-}
main = void $ unsafePartial do
	Just canvas <- getCanvasElementById "thecanvas"
	ctx <- getContext2D canvas
	animFrame <- animationFrame
	--drawCube ctx
	setFillStyle "#0000FF" ctx
	--C.fillRect {x: 0.0, y: 0.0, w: 500.0, h: 500.0} ctx