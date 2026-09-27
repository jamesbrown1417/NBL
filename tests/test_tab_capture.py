import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "tab_capture", Path(__file__).resolve().parents[1] / "OddsScraper/TAB/get-TAB-response.py"
)
capture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(capture)


class TabCaptureTests(unittest.TestCase):
    def test_browser_html_entities_are_decoded(self):
        page = '<html><pre>{"matches": [], "name": "NBL &amp; More"}</pre></html>'
        self.assertEqual(capture.parse_response(page)["name"], "NBL & More")

    def test_errors_are_not_valid_responses(self):
        for page in ['{"error": "denied"}', '<html>Access denied</html>', '{"matches": [{}]}']:
            with self.subTest(page=page), self.assertRaises(ValueError):
                capture.parse_response(page)

    def test_replace_and_preserve_on_write_failure(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "response.json"
            capture.save_response({"matches": []}, path)
            self.assertEqual(json.loads(path.read_text()), {"matches": []})
            with self.assertRaises(TypeError):
                capture.save_response({"bad": object()}, path)
            self.assertEqual(json.loads(path.read_text()), {"matches": []})
            self.assertEqual(list(Path(tmp).glob("*.tmp")), [])


if __name__ == "__main__":
    unittest.main()
