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

## Milestone 1: Proposal
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

## Milestone 2: Updates

#### Key components of our application
- **Game State:** The **`Game`** type represents the state of the Snake game, including the snake's position, direction, score, etc.
- **Rendering:** The **`drawUI`** function combines widgets to create the user interface, including the score display, game over message, and the grid with the snake and food.
- **Input Handling:** The **`handleEvent`** function responds to various events, such as ticks (for game progression), arrow key presses (for snake movement), and other key presses (e.g., 'r' for restarting and 'q' for quitting).
- **Initialization:** The **`initGame`** function initializes the game state, and the **`main`** function sets up the Brick application, event channel, and starts the main loop.
- **Game Logic:** The game logic is encapsulated in the **`step`** and **`turn`** functions, which handle the progression of the game state based on ticks and user input.
- **Brick App Definition:** The **`app`** definition configures the Brick application, specifying the drawing, event handling, cursor behavior, and attribute mapping.
- **Attributes and Styles:** The **`theMap`** defines attribute mappings for different elements, such as the snake, food, and game over message. These attributes are used in rendering to style the elements.

#### Do we expect to meet our goals until the deadline?

- Yes

## Reference and Extension

### Referenced project
https://github.com/samtay/snake.git

### Feature Extension
Brick version compatibility : `next` and `continue` are not supported in current version of brick library. Many other features need to be fixed as well, such as handling the collision to the border and the pause (restart) function when `p` (`r`) is pressed.
Second player (snake) : Extend the game from a single-player game to a double-player game. This involves creating a second snake and defining a new conditions for game-over in this multiplayer setting.
Start page : Insert a new **brick App** at the beginning of `main` to ask users to choose whether they are going to play in a single-player or double-player format.
Freezer : Create a new food-like object such that one can freeze its opponent for a while. Appears only in double-player mode.
Reverse : Allow user to press `g` or `.` to switch its head and tail.
Help : Show help window aside when playing.
