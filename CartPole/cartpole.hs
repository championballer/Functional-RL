module CartPole where

data Action = GoLeft 
            | GoRight
            deriving (Show, Eq)

data State = State
  { position  :: Float  -- ^ position of the cart on the track
  , angle     :: Float  -- ^ angle of the pole with the vertical
  , velocity  :: Float  -- ^ cart velocity
  , angleRate :: Float  -- ^ rate of change of the angle
  } deriving (Show, Eq, Ord)

data Configuration = Configuration
  { gravity    :: Float
  , masscart   :: Float
  , masspole   :: Float
  , poleLength :: Float -- ^ actually half the pole's length
  , forceMag   :: Float
  , tau        :: Float -- ^ seconds between state updates
  } deriving (Show)


conf = Configuration 
  { gravity = 9.8
  , masscart = 1.0
  , masspole = 0.1
  , poleLength = 0.5
  , forceMag = 10.0
  , tau = 0.02
  }

polemassLength :: Configuration -> Float
polemassLength s = masspole s * poleLength s

totalMass :: Configuration -> Float
totalMass s = masspole s + masscart s

-- | Angle at which to fail the episode
thetaThresholdRadians :: Float
thetaThresholdRadians = 12 * 2 * pi / 360

xThreshold :: Float
xThreshold = 2.4

hasFallen :: State -> Bool
hasFallen s
  =  position s < (-1 * xThreshold)
  || position s > xThreshold
  ||    angle s < (-1 * thetaThresholdRadians)
  ||    angle s > thetaThresholdRadians

step action = do

 let x     = position s
     xDot  = velocity s
     theta = angle    s
     thetaDot = angleRate s

 let force    = (if a == GoLeft then -1 else 1) * forceMag conf
     costheta = cos theta
     sintheta = sin theta

 let temp     = (force + polemassLength conf * (thetaDot ** 2) * sintheta) / totalMass conf
     thetaacc = (gravity conf * sintheta - costheta * temp)
                   / (poleLength conf * (4 / 3 - masspole conf * (costheta ** 2) / totalMass conf))
     xacc     = temp - polemassLength conf * thetaacc * costheta / totalMass conf

