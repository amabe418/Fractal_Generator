import React, { useState } from 'react';
import { 
  Card, 
  Select, 
  Button, 
  NumberInput,
  Flex,
  Stack
} from '@mantine/core';
import { IconZoomIn, IconZoomOut, IconDownload } from '@tabler/icons-react';
import { useFractalCanvas } from './hooks/useFractalCanvas';

const FractalGenerator = () => {
  const [params, setParams] = useState({
    width: 800,
    height: 600,
    maxIter: 1000,
    xMin: -2,
    xMax: 1,
    yMin: -1,
    yMax: 1,
    fractalType: 'mandelbrot',
    colorPalette: 'rainbow'
  });

  const {
    canvasRef,
    handleMouseDown,
    handleMouseMove,
    handleMouseUp,
    handleZoom,
    handleExport
  } = useFractalCanvas(params);

  return (
    <Card shadow="sm" padding="lg" radius="md" withBorder>
      <Stack>
        <Flex gap="md">
          <Select
            label="Fractal Type"
            value={params.fractalType}
            onChange={(value) => setParams({...params, fractalType: value})}
            data={[
              { value: 'mandelbrot', label: 'Mandelbrot Set' },
              { value: 'julia', label: 'Julia Set' },
              { value: 'burningship', label: 'Burning Ship' },
              { value: 'newton', label: 'Newton Fractal' }
            ]}
          />

          <Select
            label="Color Palette"
            value={params.colorPalette}
            onChange={(value) => setParams({...params, colorPalette: value})}
            data={[
              { value: 'rainbow', label: 'Rainbow' },
              { value: 'fire', label: 'Fire' },
              { value: 'ocean', label: 'Ocean' },
              { value: 'grayscale', label: 'Grayscale' }
            ]}
          />

          <NumberInput
            label="Max Iterations"
            value={params.maxIter}
            onChange={(value) => setParams({...params, maxIter: value})}
          />
        </Flex>

        <Flex gap="md">
          <Button 
            leftSection={<IconZoomIn />} 
            onClick={() => handleZoom(0.5, setParams)}
          >
            Zoom In
          </Button>
          <Button 
            leftSection={<IconZoomOut />} 
            onClick={() => handleZoom(2, setParams)}
          >
            Zoom Out
          </Button>
          <Button 
            leftSection={<IconDownload />} 
            onClick={handleExport}
          >
            Export PNG
          </Button>
        </Flex>

        <canvas
          ref={canvasRef}
          width={params.width}
          height={params.height}
          style={{ cursor: 'move', width: '100%' }}
          onMouseDown={handleMouseDown}
          onMouseMove={(e) => handleMouseMove(e, setParams)}
          onMouseUp={() => handleMouseUp(setParams)}
          onMouseLeave={() => handleMouseUp(setParams)}
        />
      </Stack>
    </Card>
  );
};

export default FractalGenerator;