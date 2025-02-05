export const colorPalettes = {
    rainbow: (iter) => {
      if (iter === 0) return [0, 0, 0];
      const hue = (iter % 360) / 360;
      const saturation = 0.8;
      const value = 0.9;
      let c = value * saturation;
      let x = c * (1 - Math.abs((hue * 6) % 2 - 1));
      let m = value - c;
      let [r, g, b] = [0, 0, 0];
      if (hue < 1/6) [r, g, b] = [c, x, 0];
      else if (hue < 2/6) [r, g, b] = [x, c, 0];
      else if (hue < 3/6) [r, g, b] = [0, c, x];
      else if (hue < 4/6) [r, g, b] = [0, x, c];
      else if (hue < 5/6) [r, g, b] = [x, 0, c];
      else [r, g, b] = [c, 0, x];
      return [
        Math.round((r + m) * 255),
        Math.round((g + m) * 255),
        Math.round((b + m) * 255)
      ];
    },
    fire: (iter) => {
      if (iter === 0) return [0, 0, 0];
      return [
        Math.min(255, iter * 8),
        Math.min(255, iter * 4),
        Math.min(255, iter)
      ];
    },
    ocean: (iter) => {
      if (iter === 0) return [0, 0, 0];
      return [
        Math.min(255, iter),
        Math.min(255, iter * 2),
        Math.min(255, iter * 4)
      ];
    },
    grayscale: (iter) => {
      if (iter === 0) return [0, 0, 0];
      const value = Math.min(255, iter * 4);
      return [value, value, value];
    }
  };