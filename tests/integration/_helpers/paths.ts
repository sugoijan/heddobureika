import path from "node:path";
import { fileURLToPath } from "node:url";

const currentDir = path.dirname(fileURLToPath(import.meta.url));

function parsePort(raw: string | undefined, fallback: number): number {
  const value = Number(raw);
  if (!Number.isInteger(value) || value <= 0 || value > 65535) {
    return fallback;
  }
  return value;
}

export const repoRoot = path.resolve(currentDir, "../../..");
export const wranglerHost = "127.0.0.1";
export const wranglerPort = parsePort(process.env.INTEGRATION_WRANGLER_PORT, 8797);
export const adminToken = "foobar";
export const wsBaseUrl = `ws://${wranglerHost}:${wranglerPort}/ws/`;
export const privatePuzzlePath = path.join(repoRoot, "puzzles/raora-by-noy.avif");
