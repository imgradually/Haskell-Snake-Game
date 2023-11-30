# Haskell-Snake-Game
This is our final project for UCSD FA23 CSE230

## Team member

|Name          |PID        | Github account|
|--------------|-----------|---------------|
|Nai-En Kuo    |A59025053  |imgradually    |
|Mingxuan Li   |A69027684  |HunterLep      |
|Brian Chen    |A59016113  |zychen5186     |
|Jensen Chang  |A59024883  |ycchang99      |

## Requirements
- [ghc](https://www.haskell.org/ghcup/)
- [brick](https://github.com/jtdaugherty/brick/tree/master)
  - To clone and build locally:
    ```
    $ git clone https://github.com/jtdaugherty/brick.git
    $ cd brick
    $ cabal new-build
    ```
## Execution
To run the executable enter the project’s directory and run it, by inputting the following commands:
```
cd Haskell-Snake-Game
cabal run Haskell-Snake-Game
```

## Modification from example snake project by samtay
 - mkVty: dependencies should include vty-crossplatform, need to add `import Graphics.Vty.CrossPlatform (mkVty)` in `UI.hs`
 - `Next` and `continue` deprecated after [brick 1.0]: changes in `handleEvent`, should add `import Control.Monad.State.Class (modify)` in `UI.hs`
 - AttrName: need `attrName` to convert String to AttrName, should add `import Brick.AttrMap (attrName)` in `UI.hs`
 - Other minor changes: should add parantheses after `return` at around line 56 in `UI.hs`

 - TO BE FIXED: No dead currently, i.e., the game will never stop unless you press `q`

### Project proposal
In this project, We decided to reproduce the classic snake game, with the primary aim to create a two-player gaming experience within the confines of the command line interface.
These are the game functionality we will include in our project
1. Starting Point:
The game starts with a snake for each player with a score of 0 and a piece of food on a grid.
2. Movement:
The player can control the direction of the snake using arrow keys for P1 and WASD for P2. The snake moves continuously in the specified direction.
3. Objective:
The main goal is to guide the snake to eat the food, which causes the snake to grow longer and earns point with each consumed piece.
4. Grid Boundaries:
The snake moves within the boundaries of a defined grid. If it hits the edge of the grid, it dies.
5. Eating and Growing:
When the snake eats a piece of food, its length and speed increases. The goal is to eat as much food as possible without colliding.
6. Game Over:
The player must avoid having the collision with itself, the components body, or the boundaries, if any of the above happens, it dies. When both players' snake dies, the game ends.
7. Scorekeeping:
Points are awarded for each piece of food eaten. The Player with the higher score at the end of the game wins.
