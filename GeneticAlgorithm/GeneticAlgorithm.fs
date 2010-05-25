namespace SHiNKiROU.GeneticAlgorithm

open System
open System.Collections.Generic

module Functions =
  let gen = new Random()
  let randInt(x, y) = gen.Next(x, y)
  let randFloat() = gen.NextDouble()
  let randChar() = char(randInt(32, 127))

open Functions

type DNA =
  interface
    abstract Mutation : float -> DNA
    abstract CrossOver : DNA -> DNA
    abstract Clone : unit -> DNA
    abstract Fitness : unit -> float
  end

type GA(count : int, mutationRate : float, mutationProb : float, crossProb: float, random : unit -> DNA) =
  class
    let population : DNA[] = Array.zeroCreate<DNA>(count)
    let fitnessList : float[] = [| for i in 0 .. count - 1 -> 0.0 |]
    let mutable fitnessSum : float = 0.0

    do
      seq {
        for i in 0 .. count - 1 ->
          async {
            population.[i] <- random()
          }
      }
      |> Async.Parallel
      |> Async.RunSynchronously
      |> ignore

    member my.Fitness() =
      seq {
        for i in 0 .. count - 1 ->
          async {
            fitnessList.[i] <- population.[i].Fitness()
          }
      }
      |> Async.Parallel
      |> Async.RunSynchronously
      |> ignore

      fitnessSum <- Array.fold (+) 0.0 fitnessList

    member my.Generation() =
      let select() =
        let slice = randFloat() * fitnessSum
        let mutable chosen = population.[0]
        let mutable cur = 0.0

        let mutable i = 0
        while cur < slice && i < count do
          cur <- cur + fitnessList.[i]
          chosen <- population.[i]
          i <- i + 1
        chosen

      seq {
        for i in 0 .. count - 1 ->
          async {
            let op = randFloat()
            if op < mutationProb then
              population.[i] <- (select()).Mutation(mutationRate)
            elif op < mutationProb + crossProb then
              population.[i] <- (select()).CrossOver(select())
            else
              population.[i] <- select()
          }
      }
      |> Async.Parallel
      |> Async.RunSynchronously
      |> ignore


    member my.GetBest() =
      let mutable bestF = 0.0
      let mutable best = 0
      for i = 0 to fitnessList.Length - 1 do
        if fitnessList.[i] > bestF then
          bestF <- fitnessList.[i]
          best <- i
      (bestF, best)

    member my.Population with get() = population
    member my.FitnessList with get() = fitnessList
  end
  