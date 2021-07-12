package algo.dynamic

/* 
Robot in a Grid: Imagine a robot sitting on the upper left corner of grid with r rows and c columns.
The robot can only move in two directions, right and down, but certain cells are "off limits" such that
the robot cannot step on them. Design an algorithm to find a path for the robot from the top left to
the bottom right.

0*0
    = []

1*1
[r] = [[0,0]]

2*2
[r][-]   if pos [1,0] and [1,1] are not offlimits then result is [[1,0],[1,1]] 
[ ][ ]    
         
[r][ ]   if pos [0,1] and [1,1] are not offlimits then result is [[1,0],[1,1]] 
[-][ ]   

[r][ ]   if all possitions offlimit or bottom right is offlimits then [] 
[ ][-]   

3*3
[r][ ][ ] possible solutions based on if there are offlimits 
[ ][ ][ ]
[ ][ ][ ]

*/