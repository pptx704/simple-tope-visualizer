# Simple Tope Visualizer
A tool to visualize simple tope layer of Riehl and Shulman type theory(RSTT) [1](#references).

## About
This tool is a simple visualizer for the tope layer of RSTT. It is written in Haskell and uses [Codeworld](https://code.world/haskell) for rendering. Currently it can display cubical shapes and it's faces with in 3D. It is possible to modify the visualization by adding and removing faces and see the result in real time.

## Usage
### Requirements
The project is tested on the following versions of the tools:
* `Stack 2.9.*`
* `GHC 8.8.*`
* `Cabal 3.6.*`
It is recommended to use [GHCUP](https://www.haskell.org/ghcup/) to have easier management of the Haskell toolchain.

### Installation
* Clone the repository and update submodules
```bash
git clone git@github.com:pptx704/simple-tope-visualizer.git
cd simple-tope-visualizer
git submodule init
git submodule update
```
* Build the project. The project uses `stack` for building.
```bash
stack build
```
* Run the project
```bash
stack exec simple-tope-visualizer
```
Alternatively,
```bash
stack run
```
* The project is now running on `localhost:3000`. Open the link in your browser.

### Usage
The project looks as following when it is opened in the browser.
![Initial view](https://i.postimg.cc/KzYgtRNM/image.png)

The visualization is divided into 3 parts-
* On left, the cube is displayed. The cube is a 3D representation of the cubical shape. The faces of the cube will be colored according to the faces selected for visualization.
* On top of the cube, the equation is displayed. It changes when a face is added or removed.
* On right, available faces are displayed. User can select a face to add or remove it from the visualization.

Key bindings:
* WASD - To move the cursor and select a face.
* Space - To add or remove the selected face from the visualization.
* Arrow keys - To rotate the cube.
    * Up/Down - To rotate the cube around the X axis.
    * Left/Right - To rotate the cube around the Z axis.
* Enter - To reset the cube to the initial position.

![Modified view](https://i.postimg.cc/g0c9pZdQ/image.png)

### Troubleshoting

**Getting the error *The program alex is required but not found* while building the project.**
    
Install `alex` and `happy` using `stack`.
```bash
stack install alex happy
```

## Contributing
Known issues:
- Project breaks on Safari browser. (It is recommended to Firefox. Chrome is not tested.)
- `BasicShape` with 4 points does not render properly on selection panels. Although the visualization is correct, they are not displayed on specific position.
- It takes a lot of time removing faces from the visualization. This is probably a problem with theorem prover but optimization is possible from the visualization side too.

Features needed:
- Adding or removing edges and points.
- Selecting faces using mouse instead of keyboard.
- Adding intermediate points to the edges and visualizing them.

## References
1. Emily Riehl and Michael Shulman. A type theory for synthetic ∞-categories. Higher Structures, 1, 2017. https://arxiv.org/abs/1705.07442
