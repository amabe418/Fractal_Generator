<div>
  <p>Hello, World!</p>
</div>

// import React, { useState, useEffect, useRef } from 'react';
// import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
// import { Button } from "@/components/ui/button";
// import { Input } from "@/components/ui/input";
// import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
// import { Slider } from "@/components/ui/slider";
// import { Download, ZoomIn, ZoomOut, Move } from 'lucide-react';

// const colorPalettes = {
//   rainbow: (iter) => {
//     if (iter === 0) return [0, 0, 0];
//     const hue = (iter % 360) / 360;
//     const saturation = 0.8;
//     const value = 0.9;
//     let c = value * saturation;
//     let x = c * (1 - Math.abs((hue * 6) % 2 - 1));
//     let m = value - c;
//     let [r, g, b] = [0, 0, 0];
//     if (hue < 1/6) [r, g, b] = [c, x, 0];
//     else if (hue < 2/6) [r, g, b] = [x, c, 0];
//     else if (hue < 3/6) [r, g, b] = [0, c, x];
//     else if (hue < 4/6) [r, g, b] = [0, x, c];
//     else if (hue < 5/6) [r, g, b] = [x, 0, c];
//     else [r, g, b] = [c, 0, x];
//     return [
//       Math.round((r + m) * 255),
//       Math.round((g + m) * 255),
//       Math.round((b + m) * 255)
//     ];
//   },
//   fire: (iter) => {
//     if (iter === 0) return [0, 0, 0];
//     return [
//       Math.min(255, iter * 8),
//       Math.min(255, iter * 4),
//       Math.min(255, iter)
//     ];
//   },
//   ocean: (iter) => {
//     if (iter === 0) return [0, 0, 0];
//     return [
//       Math.min(255, iter),
//       Math.min(255, iter * 2),
//       Math.min(255, iter * 4)
//     ];
//   },
//   grayscale: (iter) => {
//     if (iter === 0) return [0, 0, 0];
//     const value = Math.min(255, iter * 4);
//     return [value, value, value];
//   }
// };

// const FractalGenerator = () => {
//   const [params, setParams] = useState({
//     width: 800,
//     height: 600,
//     maxIter: 1000,
//     xMin: -2,
//     xMax: 1,
//     yMin: -1,
//     yMax: 1,
//     fractalType: 'mandelbrot',
//     colorPalette: 'rainbow'
//   });

//   const canvasRef = useRef(null);
//   const [isLoading, setIsLoading] = useState(false);
//   const [isDragging, setIsDragging] = useState(false);
//   const [dragStart, setDragStart] = useState({ x: 0, y: 0 });
//   const [viewportOffset, setViewportOffset] = useState({ x: 0, y: 0 });

//   const renderFractal = (data) => {
//     const canvas = canvasRef.current;
//     const ctx = canvas.getContext('2d');
//     const imageData = ctx.createImageData(params.width, params.height);

//     for (let y = 0; y < params.height; y++) {
//       for (let x = 0; x < params.width; x++) {
//         const index = (y * params.width + x) * 4;
//         const color = colorPalettes[params.colorPalette](data[y][x]);
//         imageData.data[index] = color[0];     // R
//         imageData.data[index + 1] = color[1]; // G
//         imageData.data[index + 2] = color[2]; // B
//         imageData.data[index + 3] = 255;      // A
//       }
//     }

//     ctx.putImageData(imageData, 0, 0);
//   };

//   const generateFractal = async () => {
//     setIsLoading(true);
//     try {
//       const response = await fetch('http://localhost:3000/generate', {
//         method: 'POST',
//         headers: {
//           'Content-Type': 'application/json',
//         },
//         body: JSON.stringify(params),
//       });
//       const data = await response.json();
//       renderFractal(data);
//     } catch (error) {
//       console.error('Error generating fractal:', error);
//     }
//     setIsLoading(false);
//   };

//   useEffect(() => {
//     generateFractal();
//   }, []);

//   const handleMouseDown = (e) => {
//     const canvas = canvasRef.current;
//     const rect = canvas.getBoundingClientRect();
//     setIsDragging(true);
//     setDragStart({
//       x: e.clientX - rect.left,
//       y: e.clientY - rect.top
//     });
//   };

//   const handleMouseMove = (e) => {
//     if (!isDragging) return;
    
//     const canvas = canvasRef.current;
//     const rect = canvas.getBoundingClientRect();
//     const x = e.clientX - rect.left;
//     const y = e.clientY - rect.top;
    
//     const dx = (x - dragStart.x) * (params.xMax - params.xMin) / params.width;
//     const dy = (y - dragStart.y) * (params.yMax - params.yMin) / params.height;
    
//     setParams(prev => ({
//       ...prev,
//       xMin: prev.xMin - dx,
//       xMax: prev.xMax - dx,
//       yMin: prev.yMin - dy,
//       yMax: prev.yMax - dy
//     }));
    
//     setDragStart({ x, y });
//   };

//   const handleMouseUp = () => {
//     if (isDragging) {
//       setIsDragging(false);
//       generateFractal();
//     }
//   };

//   const handleZoom = (factor) => {
//     const centerX = (params.xMin + params.xMax) / 2;
//     const centerY = (params.yMin + params.yMax) / 2;
//     const newWidth = (params.xMax - params.xMin) * factor;
//     const newHeight = (params.yMax - params.yMin) * factor;
    
//     setParams(prev => ({
//       ...prev,
//       xMin: centerX - newWidth / 2,
//       xMax: centerX + newWidth / 2,
//       yMin: centerY - newHeight / 2,
//       yMax: centerY + newHeight / 2
//     }));
    
//     generateFractal();
//   };

//   const handleExport = () => {
//     const canvas = canvasRef.current;
//     const link = document.createElement('a');
//     link.download = `fractal-${params.fractalType}-${Date.now()}.png`;
//     link.href = canvas.toDataURL();
//     link.click();
//   };

//   return (
//     <div className="p-4 max-w-6xl mx-auto">
//       <Card>
//         <CardHeader>
//           <CardTitle>Fractal Generator</CardTitle>
//         </CardHeader>
//         <CardContent>
//           <div className="space-y-4">
//             <div className="flex gap-4">
//               <Select
//                 value={params.fractalType}
//                 onValueChange={(value) => setParams({...params, fractalType: value})}
//               >
//                 <SelectTrigger>
//                   <SelectValue placeholder="Select fractal type" />
//                 </SelectTrigger>
//                 <SelectContent>
//                   <SelectItem value="mandelbrot">Mandelbrot Set</SelectItem>
//                   <SelectItem value="julia">Julia Set</SelectItem>
//                   <SelectItem value="burningship">Burning Ship</SelectItem>
//                   <SelectItem value="newton">Newton Fractal</SelectItem>
//                 </SelectContent>
//               </Select>
              
//               <Select
//                 value={params.colorPalette}
//                 onValueChange={(value) => setParams({...params, colorPalette: value})}
//               >
//                 <SelectTrigger>
//                   <SelectValue placeholder="Select color palette" />
//                 </SelectTrigger>
//                 <SelectContent>
//                   <SelectItem value="rainbow">Rainbow</SelectItem>
//                   <SelectItem value="fire">Fire</SelectItem>
//                   <SelectItem value="ocean">Ocean</SelectItem>
//                   <SelectItem value="grayscale">Grayscale</SelectItem>
//                 </SelectContent>
//               </Select>

//               <Input
//                 type="number"
//                 value={params.maxIter}
//                 onChange={(e) => setParams({...params, maxIter: parseInt(e.target.value)})}
//                 placeholder="Max iterations"
//                 className="w-32"
//               />
//             </div>

//             <div className="flex gap-2">
//               <Button onClick={() => handleZoom(0.5)} variant="outline">
//                 <ZoomIn className="w-4 h-4 mr-2" />
//                 Zoom In
//               </Button>
//               <Button onClick={() => handleZoom(2)} variant="outline">
//                 <ZoomOut className="w-4 h-4 mr-2" />
//                 Zoom Out
//               </Button>
//               <Button onClick={handleExport} variant="outline">
//                 <Download className="w-4 h-4 mr-2" />
//                 Export PNG
//               </Button>
//             </div>

//             <div className="border rounded-lg p-2">
//               <canvas
//                 ref={canvasRef}
//                 width={params.width}
//                 height={params.height}
//                 className="w-full cursor-move"
//                 onMouseDown={handleMouseDown}
//                 onMouseMove={handleMouseMove}
//                 onMouseUp={handleMouseUp}
//                 onMouseLeave={handleMouseUp}
//               />
//             </div>
//           </div>
//         </CardContent>
//       </Card>
//     </div>
//   );
// };

// export default FractalGenerator;