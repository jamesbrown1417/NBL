import assert from "node:assert/strict";
import test from "node:test";
import { edge, edgeBand, fairPrice, sideProbability } from "../app/model-edge.ts";

test("side probabilities flip P(over) for unders and reject invalid values", () => {
  assert.equal(sideProbability(0.6, "Over"), 0.6);
  assert.ok(Math.abs(sideProbability(0.6, "Under") - 0.4) < 1e-12);
  assert.equal(sideProbability(null, "Over"), null);
  assert.equal(sideProbability(1, "Over"), null);
});

test("fair price and edge follow from the model probability", () => {
  assert.equal(fairPrice(0.5), 2);
  assert.equal(fairPrice(null), null);
  assert.ok(Math.abs(edge(0.55, 2) - 0.1) < 1e-12);
  assert.equal(edge(0.55, null), null);
  assert.equal(edge(0.55, 1), null);
});

test("edge bands split at +10%, +3%, -3% and -15%", () => {
  assert.equal(edgeBand(0.12), "strong-positive");
  assert.equal(edgeBand(0.05), "positive");
  assert.equal(edgeBand(0.0), "neutral");
  assert.equal(edgeBand(-0.05), "negative");
  assert.equal(edgeBand(-0.3), "strong-negative");
  assert.equal(edgeBand(null), null);
});
