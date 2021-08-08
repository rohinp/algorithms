package algo.dynamic

import scala.util.chaining._

/* 
Robot in a Grid: Imagine a robot sitting on the upper left corner of grid with r rows and c columns.
The robot can only move in two directions, right and down, but certain cells are "off limits" such that
the robot cannot step on them. Design an algorithm to find a path for the robot from the top left to
the bottom right.

[] = []

0*0
    = []

1*1
[r] = [(0,0)]

3*1     if only one column and none of the cells are offlimit then answer is [[2,0]]
[r]     else []
[-]     
[ ]

1*3
[r][ ][ ] if onliy one row and multiple columns with no offlimit cells then [[0,2]]
            else []


2*2
[r][ ]   if all possitions offlimit or bottom right is offlimits then [] 
[ ][-]   

[r][-]   if pos [1,0] and [1,1] are not offlimits then result is [[1,0],[1,1]] 
[ ][ ]    
         
[r][ ]   if pos [0,1] and [1,1] are not offlimits then result is [[1,0],[1,1]] 
[-][ ]   

3*3
[r][ ][ ] possible solutions based on if there are offlimits 
[ ][ ][ ]   1. [[0,1],[0,2],[1,2],[2,2]]
[ ][ ][ ]   2. [[0,1],[1,1],[1,2],[2,2]]
            3. [[1,0],[1,1],[1,2],[2,2]]
            4. [[0,1],[1,1],[2,1],[2,2]]
            5. [[1,0],[1,1],[2,1],[2,2]]
            6. [[1,0],[2,0],[2,1],[2,2]]
2*3
[r][ ][ ] possible solutions based on if there are offlimits 
[ ][ ][ ]   1. [[0,1],[0,2],[1,2]]
            2. [[0,1],[1,1],[1,2]]
            4. [[1,0],[1,1],[1,2]]



*/

case class Point(x:Int,y:Int)

//false for offlimits
def routes(grid:Vector[Vector[Boolean]]):List[Point] =
    def goRight(r:Int, c:Int, maxCols:Int):Option[(Int,Int)] =
        if c < maxCols then Some((r, c + 1)) else None

    def goDown(r:Int, c:Int, maxRows:Int):Option[(Int,Int)] =
        if r < maxRows then Some((r + 1, c)) else None


    def loop(row:Int, col:Int, lastRow:Int, lastCol:Int):List[Point] = 
        (goRight(row,col,lastCol), goDown(row,col,lastRow)) match
            case (None, None) => List(Point(row,col))
            case (None, Some(t)) if grid(row)(col) => 
                Point(row,col) :: loop(t._1,t._2,lastRow, lastCol)
            case (None, Some(t)) => List()
            case (Some(t), None) if grid(row)(col) => 
                Point(row,col) :: loop(t._1,t._2,lastRow, lastCol)
            case (Some(t), None) => List()
            case (Some(t1), Some(t2)) => 
                if grid(t1._1)(t1._2) && grid(t2._1)(t2._2) then 
                    (Point(row,col) :: loop(t1._1,t1._2,lastRow, lastCol)) ++
                    (Point(row,col) :: loop(t2._1,t2._2,lastRow, lastCol))
                else if grid(t1._1)(t1._2) then Point(row,col) :: loop(t1._1,t1._2,lastRow, lastCol)
                else if grid(t2._1)(t2._2) then Point(row,col) :: loop(t2._1,t2._2,lastRow, lastCol)
                else List()
    
    
    if grid.length == 0 ||grid(0).length == 0 then List() else 
        val row = grid.length - 1
        val col = grid(0).length - 1
        if(grid(row)(col)) then loop(0,0, row, col) else List()


/*  

This is a less verbose solution but the above solution is the one which gave the idea of below one.
The idea is convert the multiple pattern match cases into a simple forexpression.
Let the flatMap and map manage the invalid senarios i.e. taking care of all wiring.

The below solution makes sure that if multiple paths available it will give the result respectively.
*/
def routes2(grid:Vector[Vector[Boolean]]):List[List[Point]] =
    if grid.length == 0 || grid(0).length == 0 then List() else 
        val row = grid.length - 1
        val col = grid(0).length - 1
        val goRight:Int => Int => Option[Point] = r => c => if c < col then Some(Point(r, c + 1)) else None
        val goDown:Int => Int => Option[Point] = r => c => if r < row then Some(Point(r + 1, c)) else None
        if(grid(row)(col)) then 
            def loop(row:Int, col:Int):List[List[Point]] = 
                (goRight(row)(col), goDown(row)(col)) match
                    case (None, None) => List(List(Point(row,col)))
                    case (right, down) =>
                        for 
                            optionPoint <- List(right, down)
                            point <- optionPoint.toList
                            if(grid(row)(col))
                            remaining <- loop(point.x,point.y)
                        yield Point(row,col) :: remaining
            loop(0,0) 
        else List()

@main def problem2 =
    println(s"${"-"*20}{ [] }${"-"*20}")
    routes(Vector.empty) tap println
    println(s"${"-"*20}{ 0*0 }${"-"*20}")
    routes(Vector(Vector.empty)) tap println
    println(s"${"-"*20}{ 1*1 with offsite}${"-"*20}")
    routes(Vector(Vector(false))) tap println
    println(s"${"-"*20}{ 1*1 }${"-"*20}")
    routes(Vector(Vector(true))) tap println
    println(s"${"-"*20}{ 1*3 with offsite }${"-"*20}")
    routes(Vector(Vector(true,false,true))) tap println
    println(s"${"-"*20}{ 1*3}${"-"*20}")
    routes(Vector(Vector(true,true,true))) tap println
    println(s"${"-"*20}{ 3*1 with offsite }${"-"*20}")
    routes(Vector(Vector(true),Vector(false),Vector(true))) tap println
    println(s"${"-"*20}{ 3*1}${"-"*20}")
    routes(Vector(Vector(true),Vector(true),Vector(true))) tap println
    /* 3*3
    [r][ ][-]
    [-][ ][ ]
    [-][-][ ] */
    println(s"${"-"*20}{ 3*3}${"-"*20}")
    routes(Vector(Vector(true,true,false),Vector(false,true,true),Vector(false,false,true))) tap println
    routes2(Vector(Vector(true,true,false),Vector(false,true,true),Vector(false,false,true))) tap println
    /* 4*4
    [r][ ][-][ ]
    [-][ ][ ][ ]
    [-][-][ ][ ]
    [-][-][ ][ ] 
    
    In this case there are multiple solutions
    so the expected out put is this.

    List(Point(0,0), Point(0,1), Point(1,1), Point(1,2), Point(1,3), Point(2,3), Point(3,3))
    List(Point(0,0), Point(0,1), Point(1,1), Point(1,2), Point(2,2), Point(2,3), Point(3,3))
    List(Point(0,0), Point(0,1), Point(1,1), Point(1,2), Point(2,2), Point(3,2), Point(3,3))
    */
    println(s"${"-"*20}{ 4*4}${"-"*20}")
    routes2(Vector(
        Vector(true,true,false,true),
        Vector(false,true,true,true),
        Vector(false,false,true,true),
        Vector(false,false,true,true)
    )).mkString("\n") tap println
