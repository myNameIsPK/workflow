import unittest
from colorsync import config

class TestFileIO(unittest.TestCase):
    """Test config file IO"""

    def test_config_file(self):
        config_options = config.get_config("tests/test_files/config.json")
        self.assertEqual(config_options["color_path"], "color.json")
        self.assertFalse(config_options["light"])

    def test_colors_file(self):
        colors_data = config.get_color("tests/test_files/color.json")
        self.assertEqual(colors_data["dark0"], "#32302f")
        self.assertEqual(colors_data["red"], "#cc241d")

if __name__ == "__main__":
    unittest.main()
