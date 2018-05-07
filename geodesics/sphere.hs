
import Haste
import Haste.App
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.IORef

type Plane = (Double,Double)
type Sphere = (Double,Double,Double)
data State = State {line_obj::[(Sphere,Sphere)],click_store::Maybe (Int,Int)} deriving (Eq,Show)

type SO3 = ((Double,Double,Double)
            ,(Double,Double,Double)
            ,(Double,Double,Double))


data SO3_lie_base = Xaxis | Yaxis | Zaxis deriving (Eq,Show)

exp_SO3::SO3_lie_base -> Double -> SO3
exp_SO3 b p = case b of
                  Xaxis     -> ((1.0,0.0   ,0.0   )
                               ,(0.0,cos p ,-sin p)
                               ,(0.0,sin p , cos p))
                  Yaxis     -> (( cos p,0.0, sin p)
                               ,(0.0   ,1.0,0.0   )
                               ,(-sin p,0.0, cos p))
                  Zaxis     -> ((cos p,-sin p,0.0)
                               ,(sin p, cos p,0.0)
                               ,(0.0  ,0.0   ,1.0))

act_sphere::SO3 -> Sphere -> Sphere
act_sphere g (x,y,z) = let ((a1,b1,c1),(a2,b2,c2),(a3,b3,c3)) = g in
                        let x_ =  a1*x + b1*y + c1*z in
                        let y_ =  a2*x + b2*y + c2*z in
                        let z_ =  a3*x + b3*y + c3*z in
                        let r  = sqrt (x_^2+y_^2+z_^2) in
                        (x_/r,y_/r,z_/r)

transform::IORef State -> KeyData  ->  IO()
transform st_ptr k = do
  let so = case keyCode k of
            87 -> exp_SO3 Xaxis   0.02
            83 -> exp_SO3 Xaxis (-0.02)
            65 -> exp_SO3 Yaxis (-0.02)
            68 -> exp_SO3 Yaxis   0.02
            81 -> exp_SO3 Zaxis   0.05
            69 -> exp_SO3 Zaxis (-0.05)
            otherwise -> ((1.0,0.0,0.0),(0.0,1.0,0.0),(0.0,0.0,1.0))
  st<-readIORef st_ptr
  writeIORef st_ptr (State {line_obj =map (\ (s,g) -> (act_sphere so s,act_sphere so g)) (line_obj st) ,click_store = click_store st})

transform_with_draw::Canvas -> IORef State -> KeyData -> IO()
transform_with_draw cnvs st_ptr k = do
  transform st_ptr k
  draw_all cnvs st_ptr

draw_all::Canvas -> IORef State -> IO()
draw_all cnvs st_ptr = do
  st<-readIORef st_ptr
  render cnvs $ (color (RGB 255 0 0) (stroke (circle (250.0,250.0) 5.0)))
  renderOnTop cnvs $ (gen_pic st)

gen_pic::State -> Picture()
gen_pic st = do
  let ln = line_obj st
  mapM_ geodesics ln

projection::Sphere -> Plane
projection (x,y,z) = (x/(1.0-z),y/(1.0-z))
--stereographic projection


lift::Plane -> Sphere
lift (a,b) = let m = 1.0 + a^2 + b^2 in ((2.0 * a)/m , (2.0 * b)/m , ( a^2 + b^2 - 1.0 )/m)
--plane to sphere

ext_prod::(Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
ext_prod (x,y,z) (a,b,c) = (y*c - z*b , z*a - x*c , x*b - y*a )

adjuster::(Double,Double) -> (Double,Double)
adjuster (x,y) = ((x*100)+250.0,(y*100)+250.0)

dejuster::(Double,Double) -> (Double,Double)
dejuster (x,y) = ((x-250.0)/100.0,(y-250.0)/100.0)
--coordinates transform

init_state::State
init_state = State {line_obj =  [] ,click_store = Nothing}

main::IO()
main =do
  Just cnvs <- getCanvasById "test_canvas"
  st_ptr<-newIORef init_state
  onEvent document KeyDown (transform_with_draw cnvs st_ptr)
  onEvent cnvs MouseDown (clickdown st_ptr)
  onEvent cnvs MouseUp (clickup cnvs st_ptr)
  onEvent cnvs MouseOut (mouseout st_ptr)
  draw_all cnvs st_ptr
  return ()


clickdown::IORef State -> MouseData -> IO()
clickdown st_ptr m_dat = do
  st<-readIORef st_ptr
  writeIORef st_ptr (State {line_obj=line_obj st,click_store = Just (mouseCoords m_dat)})

clickup::Canvas -> IORef State -> MouseData -> IO()
clickup cnvs st_ptr m_dat = do
  st<-readIORef st_ptr
  case click_store st of
    Nothing -> return ()
    Just (x,y) -> do
      let x1 = fromIntegral x
      let y1 = fromIntegral y
      let x2 = fromIntegral (fst(mouseCoords m_dat))
      let y2 = fromIntegral (snd(mouseCoords m_dat))
      writeIORef st_ptr (State {line_obj = ((lift.dejuster)(x1,y1),(lift.dejuster)(x2,y2)):(line_obj st),click_store = Nothing})
      draw_all cnvs st_ptr

mouseout::IORef State -> MouseData -> IO()
mouseout st_ptr _  = do
  st<-readIORef st_ptr
  writeIORef st_ptr (State {line_obj = line_obj st,click_store = Nothing})
--calll backs

geodesics::(Sphere,Sphere) -> Picture()
geodesics ((x1,y1,z1),(x2,y2,z2)) = let (a1,b1) = projection (x1,y1,z1) in
                                    let (a2,b2) = projection (x2,y2,z2) in
                                    let (a,b,c) = ext_prod (x1,y1,z1) (x2,y2,z2) in
                                    let center  = ((-a)/c,(-b)/c) in
                                    let radias  = sqrt (1.0+(a/c)^2+(b/c)^2) in
                                    let arg1 = arg ((a1+(a/c)),(b1+(b/c))) in
                                    let arg2 = arg ((a2+(a/c)),(b2+(b/c))) in
                                    do (stroke (circle (adjuster center) (radias*100.0)))
--all geodesics on spehre are arcs

arg::(Double,Double)->Double
arg (x,y) = case (x>=0,y>=0,(abs x) > (abs y)) of
              (True,True,True)   -> asin y
              (True,True,False)  -> acos x
              (False,True,False) -> acos x
              (False,True,True)  -> (pi-(asin y))
              (False,False,True) -> (pi-(asin y))
              (False,False,False)-> (2*pi-(acos x))
              (True,False,False) -> (2*pi-(acos x))
              (True,False,True)  -> (2*pi+asin y)


arg_to_arc::(Double,Double)->Double -> Double -> Double -> Picture()
arg_to_arc (x,y) r arg1 arg2 = if arg1 < arg2 then do  (stroke (arc (adjuster (x,y)) (r*100.0) arg1 arg2   ))
                                              else do  (stroke (arc (adjuster (x,y)) (r*100.0) arg1 (2*pi) ))
                                                       (stroke (arc (adjuster (x,y)) (r*100.0) 0.0  arg2   ))

