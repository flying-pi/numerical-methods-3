module Main where

import Prelude
import Util
import Graphics.UI.GLUT
import Debug.Trace
import Methods
import MyMathUtil

enablePinkColor  =  color3f 0.349019607843137 0.094117647058824 0.47843137254902

func ::  Double -> Double
func x = x*x + 2*(log (x+1))
-- func x = x*x + 2*(log (x+1)) - 4

dFunc ::  Double -> Double
dFunc x = 2*x + 2/(x+1)

d_2Func ::  Double -> Double
d_2Func x = 2 - 2/((x+1)*(x+1))


from  = -0.5
-- from  = 1
to = 0.5
-- to = 2
e = 0.0001
roundMax = 3


main :: IO ()
main = do
    putStrLn ("method bisection :: \t\t"++ (show (roundFloat (halfMethod func from to e) roundMax)))
    case (tangentsMethod func dFunc d_2Func from to e) of
        (Result value) -> putStrLn ("method tangents :: \t\t"++ (show (roundFloat value roundMax)))
        _ -> putStrLn "can not get result"
    putStrLn ("method hord :: \t\t\t"++ (show (roundFloat (hordMethod func from to e) roundMax)))
    putStrLn ("method of combinations :: \t"++ (show (roundFloat (kombinationMetod func  dFunc d_2Func from to e) roundMax)))
    putStrLn ("method of approximation :: \t"++ (show (roundFloat (approximationMethod func  dFunc from e) roundMax)))
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "lab #3"
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  pointSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  pointSize $= 7
  drawSurface
  enablePinkColor
  lineWidth $= 2
  drawGrafic
  lineWidth $= 1
  flush

drawSurface = do
  color3f 0.4 0.4 0.4
  renderPrimitive Lines $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z ) template
  color3f 1 1 1

step = 0.1
size = 0.01

template :: [(Double,Double,Double)]
template = [(-1,0,0),(1,0,0),(0,-1,0),(0,1,0)] ++
   concat [[(k,-size,0),(k,size,0),(-size,k,0),(size,k,0)] | k<-[-1,-1 + step .. 1]]

label :: Double -> Double -> [(Double,Double,Double)]
label position step = [(position,-step,0),(position,step,0),(step,position,0),(-step,position,0)]

points = [(k*0.1,(func k)*0.1,0)|k<-[-10,-9.9 .. 10]]

drawGrafic = do
  renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z ) points