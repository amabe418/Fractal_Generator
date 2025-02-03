   
***Authors:***  
- Rodrigo Mederos González   
- Alejandro Echevarría Brunet  
- Amalia Beatriz Valiente Hinojosa  

***Group:*** CC-312.


This project implements a dynamic fractal generator created in haskell with a GTK-based user interface.  


***Features:***  

- Interactive Controls:  

    Switch between Mandelbrot and Julia sets
    Adjust maximum iterations using a slider
    Pan by left-clicking and dragging
    Zoom in by right-clicking


- Visualization:

    600x600 pixel canvas
    Smooth coloring based on escape iterations
    Real-time updates when parameters change


- Technical Details:

    Uses the GI.Gtk bindings for GTK
    Cairo for rendering
    Complex number calculations for fractal generation
    State management using IORef
   


***How To Run It?***

- You will need to install the following dependencies:

        cabal install gi-gtk gi-cairo haskell-gi-base

- Run  **cabal build**  and then **cabal run**