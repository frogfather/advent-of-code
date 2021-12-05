
2020
Tried solving these using different languages so organizing as separate folders in the same repo.
- Day 1: Lazarus/fpc 
- Day 2: JavaScript
- Day 3: C#
- Day 4: Ruby
- Day 5: Python
- Day 6: Go
- Day 7: Rust
- Day 8: ?
- Day 9: Kotlin

2021
This year I'm sticking to fpc/Lazarus. Until 2016 Pascal/Object Pascal was the only language I knew. 

I used a similar development environment to Lazarus for years while barely understanding how it worked - it's interesting to revisit it now that I can (sort of) write proper code.

There is one overall project called 'advent of code' 
The main unit is advent.pas. 

For the early puzzles the solution is a single method named dayXpart1 or dayXpart2

From day 3 there are a few helper methods in addition to these

Day 4: 
The TBingoCard class creates a 3d string array (fCard) (which should be an integer array)
where location [x][y][0] is the number at that location and [x][y][1] is a flag which
indicates if the number has been called. 
It has a single method 'call' which 
- accepts an integer (the number called) 
- updates any entries on fCard which match the number
- checks to see if any rows or columns are matched
- calls the event handler (fNotifyCardWin) if the new number causes the card to become a winning card.

This could be improved a lot! Adding a 'game' class which handles the creation of cards, maintains the list of cards and runs the calling of numbers would be an improvement.

Day 5:
This was a fun puzzle!

TPoint is a built in class which represents an X,Y coordinate.  

RPath is a record type which has two members: start and finish, both of which are TPoint.

APathArray is an array of RPath records.
 
The TVentMap class has properties 

Properties:
fVentMap: a 2d array of integer
fPathArray: an APathArray

Public methods:
- constructor accepts the puzzle input as a TStringlist, sets up the integer array with dimensions based on the maximum x and y values in the puzzle, converts the input lines (x1,y1 -> x2,y2) to APathArray and sets fPathArray to this value  
- calculateVents has an optional boolean to control whether we include diagonal lines. For each path in the puzzle input it calculates which points in fVentMap that path will pass through. 
- getOverlapCount does pretty much what you'd expect it to do :)


Converting the puzzle input into an array of RPath records makes the rest of the logic a bit simpler.




