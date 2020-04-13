module CartPole where

import Control.Monad.State

data ActionSpace = GoLeft 
            | GoRight
            deriving (Show, Eq)

data StateVariables = StateVariables
  { position  :: Float  -- ^ position of the cart on the track
  , angle     :: Float  -- ^ angle of the pole with the vertical
  , velocity  :: Float  -- ^ cart velocity
  , angleRate :: Float  -- ^ rate of change of the angle
  } deriving (Show, Eq, Ord)

data ConfigurationVariables = ConfigurationVariables
  { gravity    :: Float
  , masscart   :: Float
  , masspole   :: Float
  , poleLength :: Float -- ^ actually half the pole's length
  , forceMag   :: Float
  , tau        :: Float -- ^ seconds between state updates
  } deriving (Show)

data EnvironmentVariables = EnvironmentVariables
  { epNum   :: Integer
  , done    :: Bool
  , current :: StateVariables 
  } deriving (Show, Eq)

conf :: ConfigurationVariables
conf = ConfigurationVariables
  { gravity = 9.8
  , masscart = 1.0
  , masspole = 0.1
  , poleLength = 0.5
  , forceMag = 10.0
  , tau = 0.02
  }

initialEnvVar :: EnvironmentVariables
initialEnvVar = EnvironmentVariables 
              { epNum = 0
              , done = True
              , current = StateVariables 0 0 0 0
              } 

polemassLength :: ConfigurationVariables -> Float
polemassLength s = masspole s * poleLength s

totalMass :: ConfigurationVariables -> Float
totalMass s = masspole s + masscart s

-- | Angle at which to fail the episode
thetaThresholdRadians :: Float
thetaThresholdRadians = 12 * 2 * pi / 360

xThreshold :: Float
xThreshold = 2.4

hasFallen :: StateVariables -> Bool
hasFallen s
  =  position s < (-1 * xThreshold)
  || position s > xThreshold
  ||    angle s < (-1 * thetaThresholdRadians)
  ||    angle s > thetaThresholdRadians

-- reset

-- step action = do

--  let x     = position state
--      xDot  = velocity state
--      theta = angle    state
--      thetaDot = angleRate state

--  let force    = (if a == GoLeft then -1 else 1) * forceMag conf
--      costheta = cos theta
--      sintheta = sin theta

--  let temp     = (force + polemassLength conf * (thetaDot ** 2) * sintheta) / totalMass conf
--      thetaacc = (gravity conf * sintheta - costheta * temp)
--                    / (poleLength conf * (4 / 3 - masspole conf * (costheta ** 2) / totalMass conf))
--      xacc     = temp - polemassLength conf * thetaacc * costheta / totalMass conf

--  let nextState = StateVariables
--           { position  = x        + tau conf * xDot
--           , velocity  = xDot     + tau conf * xacc
--           , angle     = theta    + tau conf * thetaDot
--           , angleRate = thetaDot + tau conf * thetaacc
--           }

--  let fallen = hasFallen nextState




