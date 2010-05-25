namespace SHiNKiROU.GeneticAlgorithm.Examples
module TextEvolution =
  open System
  open SHiNKiROU.GeneticAlgorithm
  open SHiNKiROU.GeneticAlgorithm.Functions

  type StringDNA =
    class
      val chars : char[]
      val target : char[]
      val length : int

      new(chars, target) = {
        chars = chars;
        target = target;
        length = target.Length;
      }

      override my.ToString() = new String(my.chars)

      interface DNA with
        member my.Mutation(prob) =
          new StringDNA(Array.init my.length (fun i ->
            if randFloat() < prob then
              randChar()
            else
              my.chars.[i]
          ), my.target) :> DNA
        member my.CrossOver(b) =
          new StringDNA(Array.init my.length (fun i ->
            if randFloat() < 0.5 then
              (b :?> StringDNA).chars.[i]
            else
              my.chars.[i]
          ), my.target) :> DNA
        member my.Clone() =
          new StringDNA(Array.init my.length (fun i -> my.chars.[i]), my.target) :> DNA
        member my.Fitness() =
          let mutable fitness = 0.0
          for i = 0 to my.length - 1 do
            if my.chars.[i] = my.target.[i] then
              fitness <- fitness + 1.0
          fitness
    end

  let main() =
    let str = "Hello, world!"
    let ga = new GA(500, 0.1, 0.05, 0.95, (fun () -> new StringDNA(Array.init str.Length (fun i -> randChar()), str.ToCharArray()) :> DNA))
    ga.Fitness()
    let mutable go = true
    while go do
      ga.Generation()
      ga.Fitness()
      let (fit, idx) = ga.GetBest()
      printfn "%s\n\t%f" (ga.Population.[idx].ToString()) fit
      if int(fit) >= str.Length then go <- false

    Console.ReadKey() |> ignore

  main()
