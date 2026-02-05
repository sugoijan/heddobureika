import { execa } from "execa";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import {
  getWranglerOutput,
  startWranglerDev,
  stopWranglerDev,
} from "./_helpers/dev-server";
import {
  adminToken,
  privatePuzzlePath,
  repoRoot,
  wsBaseUrl,
} from "./_helpers/paths";

describe("CLI private image upload", () => {
  beforeAll(async () => {
    await startWranglerDev();
  });

  afterAll(async () => {
    await stopWranglerDev();
  });

  it("runs the same create/upload flow as manual CLI usage", async () => {
    const result = await execa(
      "cargo",
      [
        "run",
        "-p",
        "heddobureika-cli",
        "--",
        "rooms",
        "create",
        "--base-url",
        wsBaseUrl,
        "--admin-token",
        adminToken,
        "--puzzle-file",
        privatePuzzlePath,
      ],
      {
        cwd: repoRoot,
        reject: false,
        timeout: 240_000,
      }
    );

    expect(result.exitCode, [
      "CLI command failed",
      `stdout:\n${result.stdout}`,
      `stderr:\n${result.stderr}`,
      `wrangler:\n${getWranglerOutput()}`,
    ].join("\n\n")).toBe(0);

    expect(result.stdout).toContain("room_id:");
  });
});
