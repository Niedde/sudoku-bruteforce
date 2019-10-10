
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## create some example puzzles to test the algorithm

puzzle1= matrix(data= c(9, 2, 7, "", 1, "", "", "", 4,
						"", "", 1, 7, "", "", "", "", 3,
						"", "", "", 8, "", 9, "", 6, 7,
						"", "", 9, "", "", "", 6, 7, "",
						8, "", "", "", "", "", "", "", 9,
						"", 7, 3, "", "", "", 2, "", "",
						5, 3, "", 2, "", 7, "", "", "",
						2, "", "", "", "", 6, 7, "", "",
						7, "", "", "", 3, "", 4, 9, 2), nrow= 9) #easy level, 32 known cells (49 to solve)

puzzle2= matrix(data= c(5, 3, "", "", 6, "", 9, "", 1,
						"", "", "", "", 8, "", "", "", 7,
						9, "", "", 3, "", "", "", "", "",
						"", "", "", "", 2, "", 6, "", "",
						7, 1, "", "", "", "", "", 4, 3,
						"", "", 3, "", 4, "", "", "", "",
						"", "", "", "", "", 1, "", "", 5,
						6, "", "", "", 3, "", "", "", "",
						3, "", 8, "", 7, "", "", 9, 4), nrow= 9) #medium level, 26 known cells (55 to solve)

puzzle3= matrix(data= c("", "", 5, "", "", "", 1, "", "", 
						"", 6, 1, "", "", "", 2, "", "",
						"", "", "", 3, 8, "", "", "", "", 
						"", 2, "", "", "", "", "", "", 4,
						"", "", "", "", 3, "", "", "", 9,
						"", 1, 3, 5, "", "", "", "", 2,
						9, "", "", "", "", 2, "", 4, "",
						"", "", "", "", "", "", "", 7, "",
						4, "", "", "", 5, 9, "", "", 3), nrow= 9) #expert level, 23 known cells (58 to solve)

puzzle4= matrix(data= c(1, 7, "", "", "", "", "", "", "", 
						"", "", "", "", "", "", 1, "", 9,
						5, "", "", 2, 8, "", "", "", "", 
						"", "", "", "", 5, 8, "", 2, "",
						"", 3, 7, "", "", "", "", "", "", 
						"", "", "", "", 1, 6, "", 8, "",
						4, "", "", 1, 6, "", "", "", "", 
						"", "", "", "", "", "", 2, "", 3,
						2, 8, "", "", "", "", "", "", ""), nrow= 9)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##  define all needed functions


transpose= function(puzzle) {
	# function to transpose a matrix/ puzzle
	for (i in 1:9) {
 		for (j in (i:9)) {
 			a= puzzle[i, j]
 			puzzle[i, j]= puzzle[j, i]
 			puzzle[j, i]= a
 		}
 	}

	return (puzzle)
}

isInRow= function(digit, puzzle, rowNumber) {
	# checks if the digit is already present in a given row
	if ((digit< 0)|(digit> 9)) {return ("nope")}
	if ((rowNumber< 0)|(rowNumber> 9)) {return ("nope")}

	for (i in 1:9) {
		if (puzzle[rowNumber, i]== digit) {return (TRUE)}
	}

	return (FALSE)
}


isInColumn= function(digit, puzzle, colNumber) {
	# checks if the digit is already present in a given column
	if ((digit< 0)|(digit> 9)) {return ("nope")}
	if ((colNumber< 0)|(colNumber> 9)) {return ("nope")}

	for (i in 1:9) {
		if (puzzle[i, colNumber]== digit) {return (TRUE)}
	}

	return (FALSE)
}


getSquareNumber= function(rowNumber, colNumber) {
	# in a puzzle, there are 9 squares (sized 3x3) ordered like this:
	# 1 2 3
	# 4 5 6
	# 7 8 9

	if ((rowNumber< 0)|(colNumber< 0)) {return ("nope")}
	if ((rowNumber> 9)|(colNumber> 9)) {return ("nope")}

	if (rowNumber<= 3) {
		if (colNumber<= 3) {return (1)}
		if ((colNumber> 3)&(colNumber<= 6)) {return (2)}
		if (colNumber> 6) {return (3)}
	}

	if ((rowNumber> 3)&(rowNumber<= 6)) {
		if (colNumber<= 3) {return (4)}
		if ((colNumber> 3)&(colNumber<= 6)) {return (5)}
		if (colNumber> 6) {return (6)}
	}

	if (rowNumber> 6) {
		if (colNumber<= 3) {return (7)}
		if ((colNumber> 3)&(colNumber<= 6)) {return (8)}
		if (colNumber> 6) {return (9)}
	}
}


isInSquare= function(digit, puzzle, squareNumber) {
	# checks if the digit is already present in a given 3x3 square
	if ((digit< 0)|(digit> 9)) {return ("nope")}
	if ((squareNumber< 0)|(squareNumber> 9)) {return ("nope")}

	if (squareNumber<= 3) {indexesRow= c(1, 2, 3)}
	if ((squareNumber> 3)&(squareNumber<= 6)) {indexesRow= c(4, 5, 6)}
	if (squareNumber> 6) {indexesRow= c(7, 8, 9)}

	if (squareNumber %in% c(1, 4, 7)) {indexesCol= c(1, 2, 3)}
	if (squareNumber %in% c(2, 5, 8)) {indexesCol= c(4, 5, 6)}
	if (squareNumber %in% c(3, 6, 9)) {indexesCol= c(7, 8, 9)}

	for (i in indexesRow) {
		for (j in indexesCol) {
			if (puzzle[i, j]== digit) {return (TRUE)}
		}
	}

	return (FALSE)
}


findAvailable= function(rowNumber, colNumber, puzzle) {
	# Returns a list of digits that are suitable for a given cell, based on all other digits already known in the puzzle
	result= rep(1, 9)
	if(puzzle[rowNumber, colNumber]!= "") {return (puzzle[rowNumber, colNumber])}
	for (i in 1:9) {
		if (isInRow(i, puzzle, rowNumber)== TRUE) {result[i]= 0}
		if (isInColumn(i, puzzle, colNumber)== TRUE) {result[i]= 0}
		if (isInSquare(i, puzzle, getSquareNumber(rowNumber, colNumber))== TRUE) {result[i]= 0}
	}

	result= result* (1:9)
	return (result[which(result> 0)])
}


findNextCell= function(rowNumber, colNumber) {
	# returns a 2-value vector containing the coordinates of the next cell 
	if ((rowNumber< 0)|(colNumber< 0)) {return ("nope")}
	if ((rowNumber> 9)|(colNumber> 9)) {return ("nope")}
	if ((rowNumber== 9)&(colNumber== 9)) {return (c(9, 10))} #voluntarily off limits

	if(colNumber== 9) {return (c(rowNumber+1, 1))}
	return (c(rowNumber, colNumber+ 1))
}


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## solving


trivialSolve= function(puzzle) {# simplifies the puzzle by finding trivial solutions for some cells
	for (i in 1:9) {
		for (j in 1:9) { # for every cell
			if ((puzzle[i,j]== "")&(length(findAvailable(i, j, puzzle))== 1)) { # if there is only one possible digit to fill this cell
				puzzle[i, j]= findAvailable(i, j, puzzle)[1] # put that digit in the cell
			}
		}
	}

	return (puzzle)
}

# possible= 1
# for (i in 1:9) {
# 	for (j in 1:9) {possible= possible* length(findAvailable(i, j, puzzle1))} # maximal number of combinations that can be tested
# }


soFarSoGood= function(rowNumber, colNumber, puzzle) {
	# recursive function testing all the possibilities starting from a given cell
	# Every tested digit is treated as a new constraint having impact on next cells.
	# Returns TRUE if you can reach last cell without breaking any constraint.

	if (rowNumber+ colNumber> 18) { # last cell reached
		print (puzzle)
		return (TRUE)
	}

	if (puzzle[rowNumber, colNumber]!= "") { # if there is already a digit in this cell, jump directly to next cell
		rowNumber_next= findNextCell(rowNumber, colNumber)[1]
		colNumber_next= findNextCell(rowNumber, colNumber)[2]
		return (soFarSoGood(rowNumber_next, colNumber_next, puzzle))
	}

	listPossibleDigits= findAvailable(rowNumber, colNumber, puzzle)
	if (length(listPossibleDigits)== 0) {return (FALSE)} # means that there is no valid Sudoku puzzle featuring all these constraints
	else { 
		for (digit in listPossibleDigits) { 
			puzzle[rowNumber, colNumber]= digit # try the digit
			rowNumber_next= findNextCell(rowNumber, colNumber)[1]
			colNumber_next= findNextCell(rowNumber, colNumber)[2]
			if (soFarSoGood(rowNumber_next, colNumber_next, puzzle)== TRUE) {return (TRUE)} # recursive condition : jump to next cell
			else { 
			 	puzzle[rowNumber, colNumber]= ""
			 	next # wasn't the right digit
			}

		}
	}

	return (FALSE)
}

bruteSolve= function(puzzle) { # a clean function with only one argument
	return (soFarSoGood(1, 1, puzzle))
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##  speed test


startTime= Sys.time()
bruteSolve(puzzle1)
endTime= Sys.time()
endTime- startTime

startTime= Sys.time()
bruteSolve(puzzle2)
endTime= Sys.time()
endTime- startTime

startTime= Sys.time()
bruteSolve(puzzle3)
endTime= Sys.time()
endTime- startTime
