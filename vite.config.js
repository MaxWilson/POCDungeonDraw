import { defineConfig } from 'vite'

export default defineConfig({
  root: "src",
  build: {
    outDir: "../publish",
    emptyOutDir: true,
    sourcemap: true
  }
});
