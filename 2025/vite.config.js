import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { resolve } from "path";

export default defineConfig({
  plugins: [react()],
  build: {
    rollupOptions: {
      input: {
        main: resolve(__dirname, "index.html"),
        day1: resolve(__dirname, "day1.html"),
        day2: resolve(__dirname, "day2.html"),
        day3: resolve(__dirname, "day3.html"),
      },
    },
  },
});
