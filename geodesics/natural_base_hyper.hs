import Haste
import Haste.App
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.IORef

data State = State {line_obj::[((Double,Double),(Double,Double))],click_store::Maybe (Int,Int)} deriving (Eq,Show)

type SL2R = ((Double,Double)
            ,(Double,Double))

data SL2R_natural_base = Rotate | Horizontal | Vertical

exp_SL2R::SL2R_natural_base -> Double -> SL2R
exp_SL2R b p = case b of
                  Rotate     ->let q = abs p in if p>0 then let a = 1 - q in ((a, sqrt (1 - a^2)),((-(sqrt (1 - a^2 ))),a))
                                                       else let a = 1 - q in ((a, (-(sqrt (1 - a^2 )))),(sqrt (1 - a^2),a))
                  Horizontal ->let q = abs p in if p>0 then let a = 1 + q in ((a,sqrt (a^2 - 1)),(sqrt (a^2 - 1),a))
                                                       else let a = 1 + q in ((a,(- (sqrt(a^2 - 1)))),((-(sqrt(a^2 - 1))),a))
                  Vertical   -> ((1+p,0.0),(0.0,1-p))
--exponential map of SL(2,R)


act_sl::SL2R -> (Double,Double) -> (Double,Double)
act_sl ((a,b),(c,d)) (x,y) = let z_1 = (a*x)+b in
                             let z_2 = (c*x)+d in
                             let z_3 = a*y in
                             let z_4 = c*y in
                             let m = (z_2*z_2) + (z_4*z_4) in
                             ( ((z_1*z_2)+(z_3*z_4)) / m , ((z_2*z_3)-(z_1*z_4)) / m )
--SL(2,R) acts Poincare half plane

transform_with_draw::Canvas -> IORef State -> KeyData -> IO()
transform_with_draw cnvs st_ptr key = do
  let sl = case keyCode key  of
            81 -> exp_SL2R Rotate (0.001)
            69 -> exp_SL2R Rotate (-0.001)
            87 -> exp_SL2R Vertical (0.01)
            83 -> exp_SL2R Vertical (-0.01)
            65 -> exp_SL2R Horizontal (0.001)
            68 -> exp_SL2R Horizontal (-0.001)
            otherwise -> ((1.0,0.0),(0.0,1.0))
  st<-readIORef st_ptr
  writeIORef st_ptr (State {line_obj= map (\(start,end) -> (act_sl sl start,act_sl sl end)) (line_obj st),click_store = click_store st })
  draw_all cnvs st_ptr
--Keyboard callback function: State-refelence passing style.

draw_all::Canvas -> IORef State -> IO()
draw_all cnvs st_ptr = do
  st<-readIORef st_ptr
  render cnvs $ (color (RGB 255 0 0) (stroke (circle (500.0,250.0) 5.0)))
  renderOnTop cnvs $ (gen_pic st)
-- view

gen_pic::State -> Picture()
gen_pic st = do
  let ln = line_obj st
  mapM_ geodesics ln
-- abstract shape

geodesics::((Double,Double),(Double,Double)) -> Picture()
geodesics ((x1,y1),(x2,y2)) =
  if abs((y1-y2)/(x1-x2))>10000 then do stroke (line (adjuster (x1,y1)) (adjuster (x2,y2)))
                                else do let center = ((x1+x2)/2.0)+((y1-y2)/(x1-x2))*((y1+y2)/2.0)
                                        let r1 =sqrt( (x1 - center)^2 + y1^2)
                                        let r2 =sqrt( (x2 - center)^2 + y2^2)
                                        let arg1 = acos ((x1-center)/r1)
                                        let arg2 = acos ((x2-center)/r2)
                                        stroke (arc (adjuster (center,0.0)) ((250.0*(r1+r2))/2.0) (min arg1 arg2) (max arg1 arg2))
--in Poincare half plane, all geodesics are arcs.

adjuster::(Double,Double) -> (Double,Double)
adjuster (x,y) = ((x+2.0)*250.0,y*250.0)

dejuster::(Double,Double) -> (Double,Double)
dejuster (x,y) = ((x-500.0)/250.0,y/250.0)
--coordinates transform utility.

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
      writeIORef st_ptr (State {line_obj = (dejuster(x1,y1),dejuster(x2,y2)):(line_obj st),click_store = Nothing})
      draw_all cnvs st_ptr

mouseout::IORef State -> MouseData -> IO()
mouseout st_ptr _  = do
  st<-readIORef st_ptr
  writeIORef st_ptr (State {line_obj = line_obj st,click_store = Nothing})
--mouse callback functions: State-refelence passing style.

init_state::State
init_state = State {line_obj = [],click_store = Nothing}
--initstate

main::IO()
main=do
  Just cnvs <- getCanvasById "test_canvas"
  st_ptr<-newIORef init_state
  onEvent document KeyDown (transform_with_draw cnvs st_ptr)
  onEvent cnvs MouseDown (clickdown st_ptr)
  onEvent cnvs MouseUp (clickup cnvs st_ptr)
  onEvent cnvs MouseOut (mouseout st_ptr)
  draw_all cnvs st_ptr
  return ()

--register callbacks,set init state.