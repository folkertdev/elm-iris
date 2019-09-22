module Iris exposing
    ( data
    , Iris
    , Species(..), speciesToString
    , averageIndividuals
    )

{-| The [Iris flower data set](https://en.wikipedia.org/wiki/Iris_flower_data_set) as an elm data structure.

This package exposes the iris data set as a list of records, for easy experimentation with various classification methods.
The source is [here](http://archive.ics.uci.edu/ml/datasets/Iris).

@docs data
@docs Iris


## Species

@docs Species, speciesToString


## Helpers

@docs averageIndividuals

-}

import Length exposing (Length)
import Quantity exposing (Quantity(..))


{-| The three different species of iris in this data set
-}
type Species
    = Setosa
    | Versicolour
    | Virginica


{-| Turn a species into a string

    classToString Setosa
        --> "setosa"

-}
speciesToString : Species -> String
speciesToString class =
    case class of
        Setosa ->
            "setosa"

        Versicolour ->
            "versicolour"

        Virginica ->
            "virginica"


{-| An iris data point. Uses the `ianmackenzie/elm-units` [`Length`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length) type.
-}
type alias Iris =
    { sepal : { length : Length, width : Length }
    , petal : { length : Length, width : Length }
    , class : Species
    }


{-| The average individual (mean) of each class. Useful to get the error of your classification.
-}
averageIndividuals : { setosa : Iris, versicolour : Iris, virginica : Iris }
averageIndividuals =
    let
        setosaData =
            List.take 50 data

        versicolourData =
            List.take 50 (List.drop 50 data)

        virginicaData =
            List.drop 100 data

        findMean class classData =
            { sepal = { length = averageBy (.sepal >> .length) classData, width = averageBy (.sepal >> .width) classData }
            , petal = { length = averageBy (.petal >> .length) classData, width = averageBy (.petal >> .width) classData }
            , class = class
            }
    in
    { setosa = findMean Setosa setosaData
    , versicolour = findMean Versicolour versicolourData
    , virginica = findMean Virginica virginicaData
    }


averageBy : (a -> Quantity Float unit) -> List a -> Quantity Float unit
averageBy toQuantity items =
    average (List.map toQuantity items)


average : List (Quantity Float unit) -> Quantity Float unit
average items =
    case Quantity.sum items of
        Quantity v ->
            Quantity (v / toFloat (List.length items))


{-| The data records
-}
data : List Iris
data =
    [ { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 3.5 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.9, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.7, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 1.3, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.6, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 3.6 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.4, width = Length.centimeters 3.9 }, petal = { length = Length.centimeters 1.7, width = Length.centimeters 0.4 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.6, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.3 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.4, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.9, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.1 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.4, width = Length.centimeters 3.7 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.8, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.6, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.8, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.1 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.3, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 1.1, width = Length.centimeters 0.1 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.8, width = Length.centimeters 4.0 }, petal = { length = Length.centimeters 1.2, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.7, width = Length.centimeters 4.4 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.4 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.4, width = Length.centimeters 3.9 }, petal = { length = Length.centimeters 1.3, width = Length.centimeters 0.4 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 3.5 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.3 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.7, width = Length.centimeters 3.8 }, petal = { length = Length.centimeters 1.7, width = Length.centimeters 0.3 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 3.8 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.3 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.4, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.7, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 3.7 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.4 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.6, width = Length.centimeters 3.6 }, petal = { length = Length.centimeters 1.0, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 3.3 }, petal = { length = Length.centimeters 1.7, width = Length.centimeters 0.5 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.8, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.9, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 1.6, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.6, width = Length.centimeters 0.4 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.2, width = Length.centimeters 3.5 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.2, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.7, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 1.6, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.8, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 1.6, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.4, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.4 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.2, width = Length.centimeters 4.1 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.1 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.5, width = Length.centimeters 4.2 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.9, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.1 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 1.2, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.5, width = Length.centimeters 3.5 }, petal = { length = Length.centimeters 1.3, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.9, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.1 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.4, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 1.3, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 3.5 }, petal = { length = Length.centimeters 1.3, width = Length.centimeters 0.3 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.5, width = Length.centimeters 2.3 }, petal = { length = Length.centimeters 1.3, width = Length.centimeters 0.3 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.4, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 1.3, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 3.5 }, petal = { length = Length.centimeters 1.6, width = Length.centimeters 0.6 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 3.8 }, petal = { length = Length.centimeters 1.9, width = Length.centimeters 0.4 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.8, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.3 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 3.8 }, petal = { length = Length.centimeters 1.6, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 4.6, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.3, width = Length.centimeters 3.7 }, petal = { length = Length.centimeters 1.5, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 3.3 }, petal = { length = Length.centimeters 1.4, width = Length.centimeters 0.2 }, class = Setosa }
    , { sepal = { length = Length.centimeters 7.0, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 4.7, width = Length.centimeters 1.4 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.4, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 4.5, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.9, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 4.9, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.5, width = Length.centimeters 2.3 }, petal = { length = Length.centimeters 4.0, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.5, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 4.6, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.7, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 4.5, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 3.3 }, petal = { length = Length.centimeters 4.7, width = Length.centimeters 1.6 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 4.9, width = Length.centimeters 2.4 }, petal = { length = Length.centimeters 3.3, width = Length.centimeters 1.0 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.6, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 4.6, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.2, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 3.9, width = Length.centimeters 1.4 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 2.0 }, petal = { length = Length.centimeters 3.5, width = Length.centimeters 1.0 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.9, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.2, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.0, width = Length.centimeters 2.2 }, petal = { length = Length.centimeters 4.0, width = Length.centimeters 1.0 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.1, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 4.7, width = Length.centimeters 1.4 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.6, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 3.6, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.7, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 4.4, width = Length.centimeters 1.4 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.6, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.5, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.8, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 4.1, width = Length.centimeters 1.0 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.2, width = Length.centimeters 2.2 }, petal = { length = Length.centimeters 4.5, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.6, width = Length.centimeters 2.5 }, petal = { length = Length.centimeters 3.9, width = Length.centimeters 1.1 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.9, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 4.8, width = Length.centimeters 1.8 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.1, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 4.0, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 2.5 }, petal = { length = Length.centimeters 4.9, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.1, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 4.7, width = Length.centimeters 1.2 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.4, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 4.3, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.6, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.4, width = Length.centimeters 1.4 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.8, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 4.8, width = Length.centimeters 1.4 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.7, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.0, width = Length.centimeters 1.7 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.0, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 4.5, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.7, width = Length.centimeters 2.6 }, petal = { length = Length.centimeters 3.5, width = Length.centimeters 1.0 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.5, width = Length.centimeters 2.4 }, petal = { length = Length.centimeters 3.8, width = Length.centimeters 1.1 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.5, width = Length.centimeters 2.4 }, petal = { length = Length.centimeters 3.7, width = Length.centimeters 1.0 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.8, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 3.9, width = Length.centimeters 1.2 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.0, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 5.1, width = Length.centimeters 1.6 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.4, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.5, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.0, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 4.5, width = Length.centimeters 1.6 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.7, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 4.7, width = Length.centimeters 1.5 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 2.3 }, petal = { length = Length.centimeters 4.4, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.6, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.1, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.5, width = Length.centimeters 2.5 }, petal = { length = Length.centimeters 4.0, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.5, width = Length.centimeters 2.6 }, petal = { length = Length.centimeters 4.4, width = Length.centimeters 1.2 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.1, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.6, width = Length.centimeters 1.4 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.8, width = Length.centimeters 2.6 }, petal = { length = Length.centimeters 4.0, width = Length.centimeters 1.2 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.0, width = Length.centimeters 2.3 }, petal = { length = Length.centimeters 3.3, width = Length.centimeters 1.0 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.6, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 4.2, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.7, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.2, width = Length.centimeters 1.2 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.7, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 4.2, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.2, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 4.3, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.1, width = Length.centimeters 2.5 }, petal = { length = Length.centimeters 3.0, width = Length.centimeters 1.1 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 5.7, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 4.1, width = Length.centimeters 1.3 }, class = Versicolour }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 3.3 }, petal = { length = Length.centimeters 6.0, width = Length.centimeters 2.5 }, class = Virginica }
    , { sepal = { length = Length.centimeters 5.8, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 5.1, width = Length.centimeters 1.9 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.1, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.9, width = Length.centimeters 2.1 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 5.6, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.5, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.8, width = Length.centimeters 2.2 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.6, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 6.6, width = Length.centimeters 2.1 }, class = Virginica }
    , { sepal = { length = Length.centimeters 4.9, width = Length.centimeters 2.5 }, petal = { length = Length.centimeters 4.5, width = Length.centimeters 1.7 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.3, width = Length.centimeters 2.9 }, petal = { length = Length.centimeters 6.3, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.7, width = Length.centimeters 2.5 }, petal = { length = Length.centimeters 5.8, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.2, width = Length.centimeters 3.6 }, petal = { length = Length.centimeters 6.1, width = Length.centimeters 2.5 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.5, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 5.1, width = Length.centimeters 2.0 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.4, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 5.3, width = Length.centimeters 1.9 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.8, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.5, width = Length.centimeters 2.1 }, class = Virginica }
    , { sepal = { length = Length.centimeters 5.7, width = Length.centimeters 2.5 }, petal = { length = Length.centimeters 5.0, width = Length.centimeters 2.0 }, class = Virginica }
    , { sepal = { length = Length.centimeters 5.8, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 5.1, width = Length.centimeters 2.4 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.4, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 5.3, width = Length.centimeters 2.3 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.5, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.5, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.7, width = Length.centimeters 3.8 }, petal = { length = Length.centimeters 6.7, width = Length.centimeters 2.2 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.7, width = Length.centimeters 2.6 }, petal = { length = Length.centimeters 6.9, width = Length.centimeters 2.3 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.0, width = Length.centimeters 2.2 }, petal = { length = Length.centimeters 5.0, width = Length.centimeters 1.5 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.9, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 5.7, width = Length.centimeters 2.3 }, class = Virginica }
    , { sepal = { length = Length.centimeters 5.6, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 4.9, width = Length.centimeters 2.0 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.7, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 6.7, width = Length.centimeters 2.0 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 4.9, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.7, width = Length.centimeters 3.3 }, petal = { length = Length.centimeters 5.7, width = Length.centimeters 2.1 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.2, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 6.0, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.2, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 4.8, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.1, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.9, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.4, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 5.6, width = Length.centimeters 2.1 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.2, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.8, width = Length.centimeters 1.6 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.4, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 6.1, width = Length.centimeters 1.9 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.9, width = Length.centimeters 3.8 }, petal = { length = Length.centimeters 6.4, width = Length.centimeters 2.0 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.4, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 5.6, width = Length.centimeters 2.2 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 2.8 }, petal = { length = Length.centimeters 5.1, width = Length.centimeters 1.5 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.1, width = Length.centimeters 2.6 }, petal = { length = Length.centimeters 5.6, width = Length.centimeters 1.4 }, class = Virginica }
    , { sepal = { length = Length.centimeters 7.7, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 6.1, width = Length.centimeters 2.3 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 5.6, width = Length.centimeters 2.4 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.4, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 5.5, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.0, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 4.8, width = Length.centimeters 1.8 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.9, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 5.4, width = Length.centimeters 2.1 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.7, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 5.6, width = Length.centimeters 2.4 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.9, width = Length.centimeters 3.1 }, petal = { length = Length.centimeters 5.1, width = Length.centimeters 2.3 }, class = Virginica }
    , { sepal = { length = Length.centimeters 5.8, width = Length.centimeters 2.7 }, petal = { length = Length.centimeters 5.1, width = Length.centimeters 1.9 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.8, width = Length.centimeters 3.2 }, petal = { length = Length.centimeters 5.9, width = Length.centimeters 2.3 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.7, width = Length.centimeters 3.3 }, petal = { length = Length.centimeters 5.7, width = Length.centimeters 2.5 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.7, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.2, width = Length.centimeters 2.3 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.3, width = Length.centimeters 2.5 }, petal = { length = Length.centimeters 5.0, width = Length.centimeters 1.9 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.5, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.2, width = Length.centimeters 2.0 }, class = Virginica }
    , { sepal = { length = Length.centimeters 6.2, width = Length.centimeters 3.4 }, petal = { length = Length.centimeters 5.4, width = Length.centimeters 2.3 }, class = Virginica }
    , { sepal = { length = Length.centimeters 5.9, width = Length.centimeters 3.0 }, petal = { length = Length.centimeters 5.1, width = Length.centimeters 1.8 }, class = Virginica }
    ]
