export type SortDirection = "asc" | "desc";

export function sortRows<T>(
  rows: T[],
  getValue: (row: T) => string | number | null,
  direction: SortDirection,
): T[] {
  return rows.map((row, index) => ({ row, index })).sort((a, b) => {
    const left = getValue(a.row);
    const right = getValue(b.row);
    if (left === null || (typeof left === "number" && !Number.isFinite(left))) return right === null || (typeof right === "number" && !Number.isFinite(right)) ? a.index - b.index : 1;
    if (right === null || (typeof right === "number" && !Number.isFinite(right))) return -1;
    const order = typeof left === "number" && typeof right === "number"
      ? left - right
      : String(left).localeCompare(String(right), undefined, { numeric: true, sensitivity: "base" });
    return (direction === "asc" ? order : -order) || a.index - b.index;
  }).map(item => item.row);
}
