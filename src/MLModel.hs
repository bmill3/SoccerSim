module MLModel
    ( LogisticModel
    , predictOutcomeProbabilities
    , trainLogisticModel
    ) where

import FeatureEngineering (MatchOutcome (..), TrainingExample (..), featureValues, trainingFeatures, trainingOutcome)
import Types (OutcomeProbabilities (..))

newtype LogisticModel = LogisticModel
    { modelWeights :: [[Double]]
    }
    deriving (Eq, Show)

trainLogisticModel :: [TrainingExample] -> LogisticModel
trainLogisticModel [] = LogisticModel (replicate 3 [])
trainLogisticModel examples =
    LogisticModel finalWeights
  where
    featureCount =
        length (featureValues (trainingFeatures (head examples)))
    initialWeights =
        replicate 3 (replicate featureCount 0)
    finalWeights =
        iterate trainEpoch initialWeights !! trainingEpochs
    trainEpoch weights =
        foldl updateWeights weights examples

trainingEpochs :: Int
trainingEpochs = 40

learningRate :: Double
learningRate = 0.035

regularization :: Double
regularization = 0.0008

updateWeights :: [[Double]] -> TrainingExample -> [[Double]]
updateWeights weights example =
    zipWith updateClass weights [0 ..]
  where
    values =
        featureValues (trainingFeatures example)
    probabilities =
        softmax (map (dot values) weights)
    actualClass =
        outcomeIndex (trainingOutcome example)

    updateClass classWeights classIndex =
        zipWith updateWeight classWeights values
      where
        expected =
            probabilities !! classIndex
        actual
            | classIndex == actualClass = 1
            | otherwise = 0
        errorValue =
            actual - expected

        updateWeight weight feature =
            weight + learningRate * (errorValue * feature - regularization * weight)

predictOutcomeProbabilities :: LogisticModel -> [Double] -> OutcomeProbabilities
predictOutcomeProbabilities (LogisticModel weights) values =
    case softmax (map (dot values) weights) of
        [homeWin, draw, awayWin] ->
            OutcomeProbabilities
                { homeWinProbability = homeWin
                , drawProbability = draw
                , awayWinProbability = awayWin
                }
        _ ->
            OutcomeProbabilities
                { homeWinProbability = 0.45
                , drawProbability = 0.25
                , awayWinProbability = 0.30
                }

outcomeIndex :: MatchOutcome -> Int
outcomeIndex outcome =
    case outcome of
        HomeWinClass -> 0
        DrawClass -> 1
        AwayWinClass -> 2

softmax :: [Double] -> [Double]
softmax [] = []
softmax values =
    map (/ total) exponentials
  where
    largestValue =
        maximum values
    exponentials =
        map (exp . subtract largestValue) values
    total =
        sum exponentials

dot :: [Double] -> [Double] -> Double
dot left right =
    sum (zipWith (*) left right)
