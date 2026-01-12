import { defineConfig } from "vite";
import melangePlugin from "vite-plugin-melange";
import tailwindcss from '@tailwindcss/vite';

export default defineConfig({
  plugins: [
    tailwindcss(),
    melangePlugin({
      emitDir: "src",
      buildCommand: "opam exec -- dune build @app",
      watchCommand: "opam exec -- dune build --watch @app",
    }),
  ],
});
