2022
Puzzles in FreePascal/Lazarus

There are a lot of files here. Most can be ignored. Here are the ones that may be of interest. 

Puzzle interface iAdvent in file iaoc.pas
Has a method run which runs the puzzle and a couple of methods to get and set the results

Puzzle parent class TAocPuzzle in file aocpuzzle.pas
- Inherits from TInterfacedObject and implements iAdvent. 
- Adds two virtual abstract methods runPartOne and runPartTwo which do pretty much what you'd expect them to do.

Each puzzle is in a class with the day number e.g. TDayOne in file day1.pas
- Inherits from TAocPuzzle
- Implements the two virtual abstract methods above.

The main form, disappointingly called 'MainForm' is in aoc_22.pas (form file aoc_22.lfm). It displays the puzzle instructions, has a dropdown to select the puzzle and a listbox to show the results.

There's also a form fVisualise in visualise.pas (form file visualise.lfm) which might get used for drawing stuff in the event that a picture might help with the solution. There were a couple of instances last year when it was helpful. For the moment it can be ignored. 

adventOfCode.lpr is the project file which runs the application - like main in a Java application but... different.
adventOfCode.lpi is project config stuff.

The folders input and puzzle_description contain the puzzle data and the text of the puzzle respectively.

There are some common methods for file and array handling including some array helpers in ../common.
