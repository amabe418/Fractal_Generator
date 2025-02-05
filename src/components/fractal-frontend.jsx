import React, { useState, useEffect, useRef } from "react";
import {
  Card,
  CardBody,
  CardHeader,
  Typography,
  Button,
  Select,
  Option,
  Input,
} from "@material-tailwind/react";
import { Download, ZoomIn, ZoomOut } from "lucide-react";

const FractalGenerator = () => {
  const [params, setParams] = useState({
    width: 800,
    height: 600,
    maxIter: 1000,
    xMin: -2,
    xMax: 1,
    yMin: -1,
    yMax: 1,
    fractalType: "mandelbrot",
    colorPalette: "rainbow",
  });

  const canvasRef = useRef(null);

  const generateFractal = async () => {
    try {
      const response = await fetch("http://localhost:3000/generate", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(params),
      });
      const data = await response.json();
      renderFractal(data);
    } catch (error) {
      console.error("Error generating fractal:", error);
    }
  };

  const renderFractal = (data) => {
    const canvas = canvasRef.current;
    const ctx = canvas.getContext("2d");
    const imageData = ctx.createImageData(params.width, params.height);

    for (let y = 0; y < params.height; y++) {
      for (let x = 0; x < params.width; x++) {
        const index = (y * params.width + x) * 4;
        const color = [data[y][x] % 256, data[y][x] % 128, data[y][x] % 64];
        imageData.data.set([...color, 255], index);
      }
    }

    ctx.putImageData(imageData, 0, 0);
  };

  useEffect(() => {
    generateFractal();
  }, []);

  return (
    <Card className="w-full max-w-4xl shadow-lg rounded-xl p-4">
      <CardHeader>
        <Typography variant="h2" className="mb-6 text-blue-600 font-bold">
          Fractal Generator
        </Typography>
      </CardHeader>

      <CardBody className="flex gap-8">
        <div>
          {/* Select para el tipo de fractal */}
          <label
            htmlFor="fractalType"
            className="block text-sm font-medium text-gray-700"
          >
            Fractal Type
          </label>
          <select
            id="fractalType"
            value={params.fractalType}
            onChange={(e) =>
              setParams({ ...params, fractalType: e.target.value })
            }
            className="w-full p-2 border border-gray-300 rounded-md"
          >
            <option value="mandelbrot">Mandelbrot</option>
            <option value="julia">Julia Set</option>
            <option value="burningship">Burning Ship</option>
            <option value="newton">Newton Fractal</option>
          </select>

          {/* Select para la paleta de colores */}
          <label
            htmlFor="colorPalette"
            className="block text-sm font-medium text-gray-700 mt-4"
          >
            Color Palette
          </label>
          <select
            id="colorPalette"
            value={params.colorPalette}
            onChange={(e) =>
              setParams({ ...params, colorPalette: e.target.value })
            }
            className="w-full p-2 border border-gray-300 rounded-md"
          >
            <option value="rainbow">Rainbow</option>
            <option value="fire">Fire</option>
            <option value="ocean">Ocean</option>
            <option value="grayscale">Grayscale</option>
          </select>
        </div>

        {/* Input para iteraciones */}
        

        {/* Canvas con bordes y sombreado */}
        <div className="border-2 border-gray-300 rounded-lg shadow-md overflow-hidden">
          <canvas
            ref={canvasRef}
            width={params.width}
            height={params.height}
            className="w-full"
          />
        </div>
        {/* Controles de zoom y exportación */}
        <Input
          type="number"
          label="Max Iterations"
          value={params.maxIter}
          onChange={(e) =>
            setParams({ ...params, maxIter: parseInt(e.target.value) })
          }
          className="w-1/3"
        />
        <div className="mt-15 flex gap-4">
          <Button
            color="blue"
            onClick={() => generateFractal()}
            variant="outlined"
            className="w-5 h-5"
          >
            Generate
          </Button>
          <Button
            color="green"
            onClick={() => console.log("Zoom In")}
            variant="outlined"
          >
            <ZoomIn className="w-5 h-5" />
          </Button>
          <Button
            color="red"
            onClick={() => console.log("Zoom Out")}
            variant="outlined"
          >
            <ZoomOut className="w-5 h-5" />
          </Button>
          <Button
            color="purple"
            onClick={() => console.log("Download")}
            variant="outlined"
          >
            <Download className="w-5 h-5" />
          </Button>
        </div>
      </CardBody>
    </Card>
  );
};

export default FractalGenerator;
