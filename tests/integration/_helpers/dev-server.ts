import { execa, type ExecaChildProcess } from "execa";
import waitOn from "wait-on";
import {
  adminToken,
  repoRoot,
  wranglerHost,
  wranglerPort,
} from "./paths";

let wranglerProc: ExecaChildProcess | undefined;
const wranglerOutput: string[] = [];
const wranglerBootTimeoutMs = Number(
  process.env.INTEGRATION_WRANGLER_BOOT_TIMEOUT_MS ?? "240000"
);

function appendOutput(prefix: string, chunk: unknown): void {
  const text = chunk instanceof Buffer ? chunk.toString("utf8") : String(chunk);
  wranglerOutput.push(`${prefix}${text}`);
  if (wranglerOutput.length > 500) {
    wranglerOutput.splice(0, wranglerOutput.length - 500);
  }
}

export function getWranglerOutput(): string {
  return wranglerOutput.join("");
}

export async function startWranglerDev(): Promise<void> {
  if (wranglerProc) {
    return;
  }

  wranglerProc = execa(
    "pnpm",
    [
      "exec",
      "wrangler",
      "dev",
      "--local",
      "--host",
      wranglerHost,
      "--port",
      String(wranglerPort),
      "--persist-to",
      ".wrangler/state",
      "--show-interactive-dev-session=false",
      "--var",
      `ADMIN_TOKEN:${adminToken}`,
    ],
    {
      cwd: repoRoot,
      reject: false,
      env: {
        ...process.env,
        WRANGLER_LOG_PATH: ".wrangler/logs",
      },
    }
  );

  wranglerProc.stdout?.on("data", (chunk) => appendOutput("[wrangler] ", chunk));
  wranglerProc.stderr?.on("data", (chunk) => appendOutput("[wrangler] ", chunk));
  void wranglerProc.catch(() => {
    // Failure is surfaced by connection timeout or explicit exit checks.
  });

  try {
    await waitOn({
      resources: [`tcp:${wranglerHost}:${wranglerPort}`],
      timeout: wranglerBootTimeoutMs,
      interval: 500,
      window: 500,
    });
  } catch (error) {
    const output = getWranglerOutput();
    throw new Error(
      [
        `Failed to start wrangler dev on ${wranglerHost}:${wranglerPort}`,
        `boot timeout ms: ${wranglerBootTimeoutMs}`,
        `error: ${String(error)}`,
        "wrangler output:",
        output || "(no output captured)",
      ].join("\n")
    );
  }
}

export async function stopWranglerDev(): Promise<void> {
  if (!wranglerProc) {
    return;
  }
  const proc = wranglerProc;
  wranglerProc = undefined;
  proc.kill("SIGTERM");
  await proc.catch(() => {
    // Ignore process exit errors during shutdown.
  });
}
