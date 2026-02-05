import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["tests/integration/**/*.test.ts"],
    environment: "node",
    sequence: {
      concurrent: false,
    },
    hookTimeout: 240_000,
    testTimeout: 300_000,
  },
});
