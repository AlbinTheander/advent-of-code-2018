let enabled = false;

export function setEnabled(value: boolean): void {
  enabled = value;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function log(...args: any[]): void {
  if (enabled) console.log(...args);
}
