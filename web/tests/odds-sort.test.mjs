import assert from "node:assert/strict";
import test from "node:test";
import { sortRows } from "../app/odds-sort.ts";

test("odds sorting compares prices numerically and keeps unavailable values last", () => {
  const rows = [
    { agency: "TAB", price: 9 },
    { agency: "Sportsbet", price: null },
    { agency: "BetRight", price: 12 },
    { agency: "Pointsbet", price: 9 },
  ];
  assert.deepEqual(sortRows(rows, row => row.price, "desc").map(row => row.agency), ["BetRight", "TAB", "Pointsbet", "Sportsbet"]);
  assert.deepEqual(sortRows(rows, row => row.price, "asc").map(row => row.agency), ["TAB", "Pointsbet", "BetRight", "Sportsbet"]);
  assert.deepEqual(sortRows(rows, row => row.agency, "asc").map(row => row.agency), ["BetRight", "Pointsbet", "Sportsbet", "TAB"]);
});
