import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { resolve } from "path";
import { readdirSync } from "fs";
import svgr from "@svgr/rollup";

// Automatically find all day*.html files
const dayFiles = readdirSync(__dirname)
  .filter((file) => file.match(/^day\d+\.html$/))
  .reduce((acc, file) => {
    const name = file.replace(".html", "");
    acc[name] = resolve(__dirname, file);
    return acc;
  }, {});

export default defineConfig({
  plugins: [react(), svgr()],
  build: {
    rollupOptions: {
      input: {
        main: resolve(__dirname, "index.html"),
        ...dayFiles,
      },
    },
  },
});
