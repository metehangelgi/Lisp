# 6.001

Solutions to the projects from the Spring 2007 iteration of MIT Course 6.001. All solutions are written in the RSR5 version of Scheme, in the DrScheme development environment.

## Project 1
The first project explores the application of procedures to numeric data. Through the construction of an RSA encryption system, the project covers:

1. higher-order procedures and procedural abstraction,
2. procedural complexity (in both time and space), 
3. recursive and iterative processes, and the
4. substitution model of evaluation.

## Project 2
This project emphasizes higher-order procedures and abstract data types (ADTs). Through the construction of a pattern-matching machine, you learn about: 

1. pairs and lists as building blocks for more elaborate ADTs,
2. the interplay between processes and data types, and
3. symbolic data.

## Project 3
Mutation! Up until now, the programming has been purely functional; that is, a given procedure would always return the same result as output when passed the same expressions as inputs. Mutation (or assingment) changes all of that. In particular, the simple substitution model no longer holds. In its place we introduce the environment model of evaluation. Most intro programming courses elide the issues that mutation/assingment raise, but 6.001 has a way of problematizing things that other courses take for granted. This is a good thing. This project explores these ideas through the construction of a Sudoku solver. 

## Project 4
Object orientation. In this project we join the 21st century and implement a quirky object-oriented system. In Scheme, this means modeling objects with state as procedures with internal state variables. Objects interact through message-passing. An action-adventure game is implemented. 

## Project 5
Have you ever struggled to express your thoughts in a given language? Then change the language! In Project 5 we get meta, and implement an evaluator for Scheme in Scheme itself, a so-called meta-circular evaluator. Possible applications are explored, including functions that document their own evaluation.
