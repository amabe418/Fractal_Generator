    
***Authors:***  
- Rodrigo Mederos González   
- Alejandro Echevarría Brunet  
- Amalia Beatriz Valiente Hinojosa  

***Group:*** CC-312.


This project implements a dynamic fractal generator created in haskell with a GTK-based user interface.  


***Features:***  

- The Haskell backend:
    Provides a REST API endpoint at /generate  
    Supports both Mandelbrot and Julia set generation  
    Uses efficient algorithms for fractal calculation  
    Handles parameter validation and CORS     
- The React frontend:
    Provides an interactive interface for fractal generation  
    Uses HTML Canvas for rendering  
    Includes controls for:   
        Fractal type selection (Mandelbrot/Julia)
        Maximum iterations
        Zoom level


Uses shadcn/ui components for a polished look

***How To Run It?***

- start the backend:  
    stack build  
    stack exec fractal-backend  
- start the frontend:    
    npm install  
    npm run dev 