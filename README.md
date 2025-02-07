   
***Authors:***  
- Rodrigo Mederos González   
- Alejandro Echevarría Brunet  
- Amalia Beatriz Valiente Hinojosa  

***Group:*** CC-312.


This project implements a fractal generator created in haskell with a Gloss user interface.  


***Features:***  

- **Interactive Controls:**  
    The program starts by default with mandelbrot fractal  
    Pressing the following numbers will show you different fractals:
     - 1 : MandelBrot
     - 2 : Julia   
     - 3 : Burning Ship
     - 4 : Tricorn
     - 5 : Newton
     - 6 : Julia (a variation)
     - 7 : Perpendicular Burning Ship
     - 8 : Sierpinski  
    
- **Zooming:**  
  - Pressing `+` decreases the `zoomFactor` (zoom in).  
  - Pressing `-` increases the `zoomFactor` (zoom out).  

- **Moving the view:**  
  - Left arrow (`←`): Moves left by 10% of the zoom factor.  
  - Right arrow (`→`): Moves right by 10% of the zoom factor.  
  - Up arrow (`↑`): Moves up by 10% of the zoom factor.  
  - Down arrow (`↓`): Moves down by 10% of the zoom factor.  


- Visualization:

    1920x1080 pixel canvas
    Smooth coloring based on escape iterations
    Real-time updates when parameters change

- Technical Details:

    Uses gloss
    Cairo for rendering to drae the fractals
    Complex number calculations for fractal generation
    
   


***How To Run It?***

- You will need to install gloss if runing the bellow commands do not work
- Run  **cabal build**  and then **cabal run**


