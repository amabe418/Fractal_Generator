import { useState, useEffect, useRef } from 'react';
import { colorPalettes } from '../colorPalettes';

export const useFractalCanvas = (params) => {
  const canvasRef = useRef(null);
  const [isLoading, setIsLoading] = useState(false);
  const [isDragging, setIsDragging] = useState(false);
  const [dragStart, setDragStart] = useState({ x: 0, y: 0 });

  const renderFractal = (data) => {
    const canvas = canvasRef.current;
    const ctx = canvas.getContext('2d');
    const imageData = ctx.createImageData(params.width, params.height);

    for (let y = 0; y < params.height; y++) {
      for (let x = 0; x < params.width; x++) {
        const index = (y * params.width + x) * 4;
        const color = colorPalettes[params.colorPalette](data[y][x]);
        imageData.data[index] = color[0];     // R
        imageData.data[index + 1] = color[1]; // G
        imageData.data[index + 2] = color[2]; // B
        imageData.data[index + 3] = 255;      // A
      }
    }

    ctx.putImageData(imageData, 0, 0);
  };

  const generateFractal = async (updatedParams = params) => {
    setIsLoading(true);
    try {
      const response = await fetch('http://localhost:3000/generate', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(updatedParams),
      });
      const data = await response.json();
      renderFractal(data);
    } catch (error) {
      console.error('Error generating fractal:', error);
    }
    setIsLoading(false);
  };

  const handleMouseDown = (e) => {
    const canvas = canvasRef.current;
    const rect = canvas.getBoundingClientRect();
    setIsDragging(true);
    setDragStart({
      x: e.clientX - rect.left,
      y: e.clientY - rect.top
    });
  };

  const handleMouseMove = (e, setParams) => {
    if (!isDragging) return;
    
    const canvas = canvasRef.current;
    const rect = canvas.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    
    const dx = (x - dragStart.x) * (params.xMax - params.xMin) / params.width;
    const dy = (y - dragStart.y) * (params.yMax - params.yMin) / params.height;
    
    setParams(prev => ({
      ...prev,
      xMin: prev.xMin - dx,
      xMax: prev.xMax - dx,
      yMin: prev.yMin - dy,
      yMax: prev.yMax - dy
    }));
    
    setDragStart({ x, y });
  };

  const handleMouseUp = (setParams) => {
    if (isDragging) {
      setIsDragging(false);
      generateFractal();
    }
  };

  const handleZoom = (factor, setParams) => {
    const centerX = (params.xMin + params.xMax) / 2;
    const centerY = (params.yMin + params.yMax) / 2;
    const newWidth = (params.xMax - params.xMin) * factor;
    const newHeight = (params.yMax - params.yMin) * factor;
    
    setParams(prev => ({
      ...prev,
      xMin: centerX - newWidth / 2,
      xMax: centerX + newWidth / 2,
      yMin: centerY - newHeight / 2,
      yMax: centerY + newHeight / 2
    }));
    
    generateFractal();
  };

  const handleExport = () => {
    const canvas = canvasRef.current;
    const link = document.createElement('a');
    link.download = `fractal-${params.fractalType}-${Date.now()}.png`;
    link.href = canvas.toDataURL();
    link.click();
  };

  // Genera el fractal inicial cuando el componente se monta
  useEffect(() => {
    generateFractal();
  }, [params.fractalType, params.colorPalette]);

  return {
    canvasRef,
    isLoading,
    isDragging,
    generateFractal,
    handleMouseDown,
    handleMouseMove,
    handleMouseUp,
    handleZoom,
    handleExport
  };
};