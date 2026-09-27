import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

test("web odds export contains usable prices from active agencies", async () => {
  const odds = JSON.parse(await readFile("public/data/nbl-odds.json", "utf8"));
  const agencies = new Set(["BetRight", "Pointsbet", "Sportsbet", "TAB"]);
  assert.equal(odds.metadata.season, "2026-2027");
  assert.deepEqual(new Set(odds.metadata.agencies), agencies);
  assert.ok(odds.headToHead.length > 0);
  assert.ok(odds.props.length > 0);
  for (const row of [...odds.headToHead, ...odds.totals, ...odds.props]) {
    assert.ok(agencies.has(row.agency));
    assert.ok(row.match && row.homeTeam && row.awayTeam);
  }
  for (const row of odds.props) {
    assert.ok(row.player && row.market && Number.isFinite(row.line));
    assert.ok(row.overPrice > 1 || row.underPrice > 1);
    assert.ok(row.hitCurrent === null || row.hitCurrent >= 0 && row.hitCurrent <= 1);
    assert.ok(row.hitLast10 === null || row.hitLast10 >= 0 && row.hitLast10 <= 1);
    // Model columns are optional (absent when model pricing fails) but must be valid when present.
    assert.ok(row.modelProbOver == null || row.modelProbOver > 0 && row.modelProbOver < 1);
    assert.ok(row.modelMean == null || row.modelMean >= 0);
    assert.ok(row.betSide == null || row.betSide === "Over" || row.betSide === "Under");
  }
});
