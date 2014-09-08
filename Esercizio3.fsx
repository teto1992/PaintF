open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D


//Classe Astratta dei LightWeigth controls: 

[<AbstractClass>]
type LWControl() =
    let mutable position = new PointF()
    let mutable isValid = true
    let mutable captured = false

    let mousedown = new Event<MouseEventArgs>()
    let mousemove = new Event<MouseEventArgs>()
    let mouseup = new Event<MouseEventArgs>()

    member this.Captured = captured
    member this.MouseDown = mousedown.Publish
    member this.MouseMove = mousemove.Publish
    member this.MouseUp = mouseup.Publish

    member this.IsValid
        with get() = isValid
    
    member this.Position
        with get() = position
        and set(p) = position <- p

    member this.Invalidate() = isValid <- false

    member this.InternalPaint (g:Graphics) =
        let s = g.Save()
        g.TranslateTransform(position.X, position.Y)
        this.OnPaint g
        g.Restore(s)
        isValid <- true

    member this.HitTest (p:PointF) =
        this.IsInside(new PointF(p.X - position.X, p.Y - position.Y))

    abstract GetSize : unit -> SizeF
    abstract IsInside : PointF -> bool
    abstract OnPaint : Graphics -> unit
    abstract OnMouseDown : MouseEventArgs -> unit
    abstract OnMouseMove : MouseEventArgs -> unit
    abstract OnMouseUp : MouseEventArgs -> unit

    default this.OnMouseDown e =
        captured <- true
        mousedown.Trigger(e)

    default this.OnMouseMove e = mousemove.Trigger(e)

    default this.OnMouseUp e =
        captured <- false
        mouseup.Trigger(e)

// direzioni per lo scroll
type ScrollDir = Up | Down | Left | Right

// Bottone Scroll
type ScrollBottone (dir:ScrollDir) =
    inherit LWControl()

    let area = new Drawing2D.GraphicsPath()
    let mutable pressed = false

    do
        area.AddLines(
            match dir with
            | Left -> [| new Point(0, 25); new Point(25, 0); new Point(25, 50) |]
            | Right -> [| new Point(25, 25); new Point(0, 0); new Point(0, 50) |]
            | Up -> [| new Point(25, 0); new Point(0, 25); new Point(50, 25) |]
            | Down -> [| new Point(0, 0); new Point(50, 0); new Point(25, 25) |]
        )
    
    member this.Dir = dir

    override this.GetSize() =
        match dir with
        | Left | Right -> new SizeF(25.f, 50.f)
        | Up | Down -> new SizeF(50.f, 25.f)

    override this.IsInside p =
        let r = new Region(area)
        r.IsVisible(p)

    override this.OnPaint g =
        g.FillPath((if pressed then Brushes.Tomato else Brushes.Gold), area)

    override this.OnMouseDown e =
        base.OnMouseDown(e)
        pressed <- true; this.Invalidate()

    override this.OnMouseUp e =
        base.OnMouseUp(e)
        pressed <- false; this.Invalidate()

// Bottone con Immagine
type ImgBottone(src) = 
    inherit LWControl()

    let img = Image.FromFile(src)

    override this.GetSize () = new SizeF(single img.Size.Width, single img.Size.Height)
    override this.IsInside p = (new RectangleF(new PointF(), this.GetSize())).Contains(p)
    override this.OnPaint g = g.DrawImage(img, 0, 0)


// Bottone Campitura Colore
type ColBottone(c:Color) =
    inherit LWControl()

    let mutable selected = false

    let colour = c

    let area = new Drawing2D.GraphicsPath()

    do
        area.AddPolygon(
            [| new Point(0, 0); new Point(0, 25);  new Point(25, 25); new Point(25, 0); |]
        )
    
    
    member this.Colour = colour
  
    override this.GetSize() =
        new SizeF(25.f, 25.f)

    override this.IsInside p =
        let r = new Region(area)
        r.IsVisible(p)

    override this.OnPaint g =
        g.FillPath(new SolidBrush(c), area)
        if (selected) then
            g.DrawPath(new Pen(Color.Tomato, 3.f), area)
        else
            g.DrawPath(new Pen(Color.Gray, 1.f), area)

    override this.OnMouseDown e =
        base.OnMouseDown(e)
        selected <- not selected; this.Invalidate();

    override this.OnMouseUp e =
        base.OnMouseUp(e)
        selected <- not selected; this.Invalidate();

// tipo DrawingPoint: memorizza le coordinate, il colore e la dimensione
// della punta per un Punto che deve essere disegnato; nel caso si debba inserire 
// un'immagine la memorizza assieme alle coordinate
type DrawingPoint (p : Point, dim : int, c : Color, im : Image) =
    let color = c
    let point = p
    let dimension = dim

    member this.P = point
    member this.DIM = dimension
    member this.Colour = color
    member this.Image = im

// IL PROGRAMMA 

type Painter() as this =
    inherit UserControl()

    //lista contenenente i tratti disegnati come liste di punti
    let mutable pts : DrawingPoint List List = []

    //posizione temporanea del puntatore
    let mutable tmpPoint = new Point()

    //array dei comandi
    let toolbar : LWControl array = [|
        new ScrollBottone(Left);
        new ScrollBottone(Right);
        new ScrollBottone(Up);
        new ScrollBottone(Down);
        new ColBottone(Color.Red);
        new ColBottone(Color.Green);
        new ColBottone(Color.Blue);
        new ImgBottone(@"C:\Users\Stefano\Documents\Visual Studio 2013\Projects\FsEmptyWindowsApp4\small.png");
        new ImgBottone(@"C:\Users\Stefano\Documents\Visual Studio 2013\Projects\FsEmptyWindowsApp4\big.png");
        new ColBottone(Color.Black);
        new ColBottone(Color.White);
        new ColBottone(Color.Cyan);
        new ColBottone(Color.Magenta);
        new ColBottone(Color.Yellow);
        new ImgBottone(@"C:\Users\Stefano\Documents\Visual Studio 2013\Projects\FsEmptyWindowsApp4\zoomin.png");
        new ImgBottone(@"C:\Users\Stefano\Documents\Visual Studio 2013\Projects\FsEmptyWindowsApp4\zoomout.png");
    |]

    // file dialog per caricare l'immagine
    let fd = new OpenFileDialog()
    // dimensione della penna
    let mutable pensize = 10
    // booleano che indica se si sta trascinando il mouse con il tasto premuto
    let mutable dragging = false
    // booleano che indica se si sta trascinando un'immagine
    let mutable imgDragging = false
    //colore della penna
    let mutable pencolor = Color.Transparent
    
    //matrici
    let w2v = new Drawing2D.Matrix()
    let v2w = new Drawing2D.Matrix()

    //funzione per lo scrolling
    let move dir =
        match dir with
        | Left -> w2v.Translate(20.f, 0.f); v2w.Translate(-20.f, 0.f, Drawing2D.MatrixOrder.Append)
        | Right -> w2v.Translate(-20.f, 0.f); v2w.Translate(20.f, 0.f, Drawing2D.MatrixOrder.Append)
        | Up -> w2v.Translate(0.f, 20.f); v2w.Translate(0.f, -20.f, Drawing2D.MatrixOrder.Append)
        | Down -> w2v.Translate(0.f, -20.f); v2w.Translate(0.f, 20.f, Drawing2D.MatrixOrder.Append)

    let scrollt = new Timer(Interval = 100)

    do 
        this.SetStyle(ControlStyles.OptimizedDoubleBuffer ||| ControlStyles.AllPaintingInWmPaint, true)
        let scrolldir = ref Left
        scrollt.Tick.Add (fun _ -> move !scrolldir; this.Invalidate()) 
        toolbar.[0].MouseDown.Add(fun _ -> scrolldir := Left; scrollt.Start())
        toolbar.[1].MouseDown.Add(fun _ -> scrolldir := Right; scrollt.Start())
        toolbar.[2].MouseDown.Add(fun _ -> scrolldir := Up; scrollt.Start())
        toolbar.[3].MouseDown.Add(fun _ -> scrolldir := Down; scrollt.Start())
                
        for  idx in 0 .. 3 do
            toolbar.[idx].MouseUp.Add(fun _ -> scrollt.Stop())

        toolbar.[4].MouseDown.Add(fun _ -> pencolor <-  Color.Red )
        toolbar.[5].MouseDown.Add(fun _ -> pencolor <- Color.Green )
        toolbar.[6].MouseDown.Add(fun _ -> pencolor <- Color.Blue )
        toolbar.[9].MouseDown.Add(fun _ -> pencolor <- Color.Black ) 
        toolbar.[10].MouseDown.Add(fun _ -> pencolor <- Color.White ) 
        toolbar.[11].MouseDown.Add(fun _ -> pencolor <- Color.Cyan ) 
        toolbar.[12].MouseDown.Add(fun _ -> pencolor <- Color.Magenta ) 
        toolbar.[13].MouseDown.Add(fun _ -> pencolor <- Color.Yellow ) 

        toolbar.[7].MouseDown.Add(fun _ -> if pensize > 5 then pensize <- pensize - 5)
        toolbar.[8].MouseDown.Add(fun _ -> if pensize < 1000 then pensize <- pensize + 5)

        let scale  s =
            if pts <> [] then
                let l = pts.Head
    
                let pC = new Point(l.[0].P.X + (l.[l.Length - 1].P.X - l.[0].P.X) / 2,
                                    l.[0].P.Y + (l.[l.Length - 1].P.Y - l.[0].P.Y) / 2) 
                let p0 = [| pC |]
                w2v.TransformPoints(p0)

                w2v.Scale(s, s)
                v2w.Scale(1.f / s, 1.f / s, Drawing2D.MatrixOrder.Append)

                let p1 = [| pC |]
                w2v.TransformPoints(p1)

                let d = [| new Point(p0.[0].X - p1.[0].X, p0.[0].Y - p1.[0].Y) |]
                v2w.TransformVectors(d)
                w2v.Translate(single d.[0].X, single d.[0].Y)
                v2w.Translate(single -d.[0].X, single -d.[0].Y, Drawing2D.MatrixOrder.Append)

                this.Invalidate()

        toolbar.[14].MouseDown.Add(fun _ -> scale 1.25f)
        toolbar.[15].MouseDown.Add(fun _ -> scale (1.f / 1.25f))

    override this.OnResize _ =
        // scroll
        toolbar.[0].Position <- new PointF(0.f, single(this.Height / 2) - (toolbar.[0].GetSize().Height / 2.f))
        toolbar.[1].Position <- new PointF(single(this.Width) - toolbar.[1].GetSize().Width, single(this.Height / 2) - (toolbar.[0].GetSize().Height / 2.f))
        toolbar.[2].Position <- new PointF(single(this.Width / 2) - toolbar.[2].GetSize().Width / 2.f, 0.f)
        toolbar.[3].Position <- new PointF(single(this.Width / 2) - toolbar.[3].GetSize().Width / 2.f, single(this.Height) - toolbar.[3].GetSize().Height)
        this.Invalidate()

        // colore
        toolbar.[4].Position <- new PointF(0.f, 0.f)
        toolbar.[5].Position <- new PointF(30.f, 0.f)
        toolbar.[6].Position <- new PointF(60.f, 0.f)
        toolbar.[9].Position <- new PointF(90.f, 0.f)
        toolbar.[10].Position <- new PointF(120.f, 0.f)
        toolbar.[11].Position <- new PointF(150.f, 0.f)
        toolbar.[12].Position <- new PointF(180.f, 0.f)
        toolbar.[13].Position <- new PointF(210.f, 0.f)

        //punta
        toolbar.[7].Position <- new PointF(single this.Width - 61.f, single this.Height - 31.f)
        toolbar.[8].Position <- new PointF(single this.Width - 31.f, single this.Height - 31.f)
        
        //zoom
        toolbar.[14].Position <- new PointF(0.f, single this.Height - 65.f)
        toolbar.[15].Position <- new PointF(70.f, single this.Height - 65.f)


    override this.OnMouseDown e =
        let btn = toolbar |> Seq.tryFind (fun b -> b.HitTest(new PointF(single(e.X), single(e.Y))))
        match btn with
        | Some b ->
          if not dragging then
              b.OnMouseDown e
              if not b.IsValid then this.Invalidate() 
        | None ->
          let cp = [|e.Location|]  
          v2w.TransformPoints(cp)
          dragging <- true;
          if (not imgDragging) then
            pts <- [(new DrawingPoint(cp.[0], pensize, pencolor,null))]::pts 
          if (imgDragging) then
            pts <- [ new DrawingPoint(cp.[0], 0, Color.Transparent,Image.FromFile(fd.FileName)) ]::pts 
            imgDragging <- false
          tmpPoint <- Point(cp.[0].X, cp.[0].Y)
            
          this.Invalidate()
          
      
    override this.OnMouseMove e =
        toolbar 
            |> Seq.filter (fun b -> b.Captured || b.HitTest(new PointF(single(e.X), single(e.Y))))
            |> Seq.iter (fun b ->
                            b.OnMouseMove e
                            if not b.IsValid then this.Invalidate())    
        let cp = [|e.Location|]  
        v2w.TransformPoints(cp)
        if (dragging) then
            let ptsHead = pts.Head
            let newHead = ptsHead@[(new DrawingPoint(cp.[0], pensize, pencolor, null))]
            pts <- newHead::pts.Tail
        
        tmpPoint <- cp.[0]
        
        this.Invalidate()

    override this.OnMouseUp e =
        toolbar 
            |> Seq.filter (fun b -> b.Captured || b.HitTest(new PointF(single(e.X), single(e.Y))))
            |> Seq.iter (fun b ->
                            b.OnMouseUp e
                            if not b.IsValid then this.Invalidate())
        dragging <- false
    
    override this.OnKeyUp e =
        if e.KeyCode = Keys.I then
             fd.ShowDialog()|>ignore
             imgDragging <- true
        this.Invalidate()

    override this.OnPaint e =
        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality

        let tt = g.Transform
        g.Transform <- w2v

        for j = pts.Length - 1 downto 0 do
            let l = pts.Item(j)
            let pS: DrawingPoint = l.Item(0)
            if pS.Image <> null then
                g.DrawImage(pS.Image, pS.P)
            else
                g.FillEllipse(new SolidBrush(pS.Colour), pS.P.X - (pS.DIM)/2, pS.P.Y - (pS.DIM)/2, pS.DIM, pS.DIM)
                for i = 0 to l.Length - 2 do
                   let p: DrawingPoint = l.Item(i)
                   let pNext: DrawingPoint = l.Item(i+1)
                   g.DrawLine(new Pen(new SolidBrush(p.Colour), single p.DIM), p.P, pNext.P)
                   g.FillEllipse(new SolidBrush(pNext.Colour), pNext.P.X - (pNext.DIM)/2, pNext.P.Y - (pNext.DIM)/2, pNext.DIM, pNext.DIM)
           
        
        if (not (toolbar.[0].Captured || toolbar.[1].Captured || toolbar.[2].Captured || toolbar.[3].Captured || imgDragging) ) then
            g.FillEllipse(new SolidBrush(pencolor), tmpPoint.X - (pensize)/2, tmpPoint.Y - (pensize)/2, pensize, pensize)
            if pencolor = Color.White then 
                g.DrawEllipse(new Pen(System.Drawing.Color.LightGray),  tmpPoint.X - (pensize)/2, tmpPoint.Y - (pensize)/2, pensize, pensize)
        elif (imgDragging) then
            let tmpImg: Image = Image.FromFile(fd.FileName)
            g.DrawImage(tmpImg, tmpPoint)
        
        g.Transform <- tt
    
        toolbar |> Seq.iter (fun b -> b.InternalPaint(g)) 


let f = new Form(Text="LWPaint - F#",TopMost=true)
let p = new Painter(Dock=DockStyle.Fill)
f.BackColor <- Color.White


f.Controls.Add(p)

f.Show()


                