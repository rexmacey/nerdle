# nerdle
For the analysis of and solution to Nerdle games

todos: function to deal with multiple instances of same number.

1/30/2023. Added nerdleFunctions.r
1/30/2023. Added nerdle_CreateEqForms.r to create possible equations.
ValidEquations.rds are all identified valid equations - has 2 variable the digits in the equation and the Form.
dfEqChar.rds is a data frame with the equations in the form of 8 variable each representing a character in the equation.
nerdle_solver.r is used to solve Nerdle while playing live. nerdle_simulator.r is under development to test the approach by simulating games.
nerdle_analysis.rmd examines the valid equations to make observations about them.
