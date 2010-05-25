namespace SHiNKiROU.GeneticAlgorithm.Examples
module CurveFitting =
  open System
  open System.Windows
  open System.Windows.Controls
  open System.Windows.Shapes
  open System.Windows.Media
  open System.Windows.Threading
  open SHiNKiROU.GeneticAlgorithm
  open SHiNKiROU.GeneticAlgorithm.Functions

  type Op =
    | Add
    | Sub
    | Mul
    | Div

  type Expr =
    | Node of Op * Expr * Expr
    | Terminal of float
    | X

  module ExprOps =
    let randomOp() = ([| Add; Sub; Mul; Div; |]).[randInt(0, 4)]
    let rec randomExpr(depth) : Expr =
      if depth = 0 then
        if randFloat() < 0.35 then
          Terminal(randFloat() * 10.0 - 4.0)
        else
          X
      else
        Node(randomOp(),
             randomExpr(depth - (if randFloat() < 0.95 then 1 else 0)),
             randomExpr(depth - (if randFloat() < 0.95 then 1 else 0)))

    let rec displayExpr expr =
      match expr with
        | Node(op, e1, e2) ->
          let opdisp =
            match op with
              | Add -> "+"
              | Sub -> "-"
              | Mul -> "*"
              | Div -> "/"
          sprintf "(%s %s %s)" (displayExpr(e1)) opdisp (displayExpr(e2))
        | Terminal(f) -> sprintf "%f" f
        | X -> "x"

  open ExprOps

  type ExprDNA =
    class
      val expr : Expr
      val curve : float -> float
      val xmin : float
      val xmax : float

      new(expr, curve, xmin, xmax) = {
        expr = expr;
        curve = curve;
        xmin = xmin;
        xmax = xmax;
      }

      override my.ToString() = displayExpr my.expr

      member my.Eval(expr, value) =
        match expr with
          | Node(Add, e1, e2) -> my.Eval(e1, value) + my.Eval(e2, value)
          | Node(Sub, e1, e2) -> my.Eval(e1, value) - my.Eval(e2, value)
          | Node(Mul, e1, e2) -> my.Eval(e1, value) * my.Eval(e2, value)
          | Node(Div, e1, e2) -> my.Eval(e1, value) / my.Eval(e2, value)
          | Terminal(x) -> x
          | X -> value
      interface DNA with
        member my.Mutation(prob) =
          let v = randFloat()
          let newDNA =
            match my.expr with
              | Node(op, e1, e2) ->
                if v < prob * 0.1 then
                  Node(randomOp(), e1, e2)
                elif v < prob * 0.3 then
                  Node(op, randomExpr(randInt(0, 5)), e2)
                elif v < prob * 0.5 then
                  Node(op, e1, randomExpr(randInt(0, 5)))
                elif v < prob then
                  randomExpr(randInt(0, 5))
                else
                  my.expr
              | Terminal(_) | X -> randomExpr(randInt(0, 5))
          new ExprDNA(newDNA, my.curve, my.xmin, my.xmax) :> DNA

        member my.CrossOver(b) =
          let b = b :?> ExprDNA
          let newDNA =
            match my.expr with
              | Node(op, e1, e2) ->
                let v = randFloat()
                let nodeB =
                  match b.expr with
                    | Node(_, b1, b2) ->
                      if v < 0.5 then
                        b1
                      else
                        b2
                    | Terminal(_) | X -> b.expr
                let v2 = randFloat()
                if v2 < 0.4 then
                  Node(op, nodeB, e2)
                elif v2 < 0.9 then
                  Node(op, e1, nodeB)
                else
                  Node(randomOp(), e1, e2)
              | Terminal(_) | X -> Terminal(randFloat() * 10.0 - 4.0)
          new ExprDNA(newDNA, my.curve, my.xmin, my.xmax) :> DNA

        member my.Clone() =
          new ExprDNA(my.expr, my.curve, my.xmin, my.xmax) :> DNA

        member my.Fitness() =
          let points = 50
          let dx = (my.xmax - my.xmin) / float(points)
          let mutable error = 0.0
          for i in 0 .. points do
            let x = my.xmin + float(i) * dx
            let diff = my.Eval(my.expr, x) - my.curve(x)
            error <- error + diff * diff
          if Double.IsNaN(error) || Double.IsInfinity(error) then
            0.0
          else
            let fitness = 9e10 - error
            if fitness < 0.0 then
              0.0
            else
              fitness
    end

  let WIDTH = 600.0
  let HEIGHT = 600.0

  type CurveFittingWindow(curve : float -> float, xmin : float, xmax : float, ymin : float, ymax : float, ga : GA) as my =
    class
      inherit Window(Title="Curve Fitting")

      let canvas = new Canvas(Width=WIDTH, Height=HEIGHT)
      let bc = new BrushConverter()

      do
        my.Content <- canvas
        my.SizeToContent <- SizeToContent.WidthAndHeight
        my.Loaded.Add (fun _ ->
          async {
            do! my.Advance()
          }
          |> Async.StartImmediate
        )

      member my.DrawGrid(sX, sY) =
        let dx = WIDTH / float(sX)
        for x = 0 to sX do
          let line = new Line()
          line.X1 <- float(x) * dx + 0.5
          line.Y1 <- 0.0
          line.X2 <- float(x) * dx + 0.5
          line.Y2 <- HEIGHT
          line.Stroke <- Brushes.Black
          line.Opacity <- 0.3
          canvas.Children.Add(line) |> ignore
        let dy = WIDTH / float(sY)
        for y = 0 to sY do
          let line = new Line()
          line.X1 <- 0.0
          line.Y1 <- float(y) * dy + 0.5
          line.X2 <- HEIGHT
          line.Y2 <- float(y) * dy + 0.5
          line.Stroke <- Brushes.Black
          line.Opacity <- 0.3
          canvas.Children.Add(line) |> ignore

      member my.PlotCurve(f, sX, sY, brush) =
        let dx = (xmax - xmin) / float(sX)
        let dy = (xmax - ymin) / float(sY)
        let dx1 = WIDTH / float(sX)
        let dy1 = HEIGHT / float(sY)
        let mutable x = xmin
        let mutable y0 = ymin - 1.0
        let mutable y1 = 0.0
        for i = 0 to sX do
          x <- x + dx
          y1 <- f(x)
          if Double.IsInfinity(y1) || Double.IsNaN(y1) then
            y1 <- ymin - 1.0

          let p0 = (y0 - ymin) / (ymax - ymin)
          let p1 = (y1 - ymin) / (ymax - ymin)
          let line = new Line()
          line.X1 <- float(i - 1) * dx1
          line.Y1 <- HEIGHT - p0 * HEIGHT
          line.X2 <- float(i) * dx1
          line.Y2 <- HEIGHT - p1 * HEIGHT
          line.Stroke <- brush
          canvas.Children.Add(line) |> ignore
          y0 <- y1

      member my.GetBest() =
        let fits = ga.FitnessList
        let mutable bestF = 0.0
        let mutable best = 0
        for i = 0 to fits.Length - 1 do
          if fits.[i] > bestF then
            bestF <- fits.[i]
            best <- i
        (bestF, best)

      member my.Advance() =
        async {
          ga.Fitness()
          while true do
            ga.Generation()
            ga.Fitness()
            let bestF, best = my.GetBest()
            canvas.Children.Clear()
            my.DrawGrid(20, 20)
            my.PlotCurve(curve, 50, 50, Brushes.Green)
            let bestDNA = ga.Population.[best] :?> ExprDNA
            my.PlotCurve((fun x ->
              bestDNA.Eval(bestDNA.expr, x)
            ), 50, 50, Brushes.Red)
            let label = new TextBlock()
            label.Text <- sprintf "Best: %f\nf(x) = %s" bestF (bestDNA.ToString())
            label.TextWrapping <- TextWrapping.Wrap
            label.Width <- WIDTH - 4.0
            label.Background <- bc.ConvertFrom("#88FFFFFF") :?> Brush
            label.HorizontalAlignment <- HorizontalAlignment.Left
            canvas.Children.Add(label) |> ignore
            do! Async.Sleep 10
        }
    end

  [<STAThread>]
  do
    let points = [| 0.04; 3.4; 5.34; 6.78; 8.43;
                    12.2; 17.3; 22.45; 28.66; 37.53;
                    45.53; 56.53; 73.44; 84.32; 96.53;
                    114.5; 133.2; 140.4; 186.43; 213.5;
                    243.56; 312.5; 367.35; 401.44; 442.45;
                    492.4; 542.4; 604.32; 655.6; 700.0 |]
    let curve(x : float) =
      let ind = int(x)
      let prop = x - float(ind)
      float(points.[ind]) + prop * float(points.[ind + 1] - points.[ind])
    let xmin = 0.0
    let xmax = float(points.Length) - 2.0
    let ga = new GA(2500, 0.5, 0.1, 0.1, (fun() -> new ExprDNA(randomExpr(3), curve, xmin, xmax) :> DNA))

    let app = new Application()
    app.Run(new CurveFittingWindow(curve, xmin, xmax, 0.0, 700.0, ga)) |> ignore

