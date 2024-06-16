import unittest
from colorsync import colors
from colorsync.colors import Color

class TestColor(unittest.TestCase):
    """Test color data"""

    @unittest.expectedFailure
    def test_color_incorrect1(self):
        Color("#1234")

    @unittest.expectedFailure
    def test_color_incorrect2(self):
        Color("ZZZZZZ")

    @unittest.expectedFailure
    def test_color_incorrect3(self):
        Color("ee889684")

    @unittest.expectedFailure
    def test_color_incorrect4(self):
        Color("##189668")

    def test_color_correct(self):
        self.assertEqual(str(Color("#15a")), "#1155aa")
        self.assertEqual(str(Color("15a")), "#1155aa")
        self.assertEqual(str(Color("152594")), "#152594")
        self.assertEqual(str(Color("#1a2594")), "#1a2594")
        self.assertEqual(str(Color("#AAAAAA")), "#aaaaaa")
        self.assertEqual(str(Color("BBBBBB")), "#bbbbbb")
        self.assertEqual(str(Color("#cccccc")), "#cccccc")
        self.assertEqual(str(Color("#c86E9e")), "#c86e9e")


    def test_color_schema(self):
        colors_dict = {
            "dark0"       : "#32302f",

            "light0"      : "#f2e5bc",

            "red"    : "#cc241d",
            "green"  : "#98971a",
            "yellow" : "#d79921",
            "blue"   : "#458588",
            "purple" : "#b16286",
            "aqua"   : "#689d6a",
            "orange" : "#d65d0e",
        }

        self.assertTrue(colors.check_schema(colors_dict))

        colors_dict = colors_dict | {
            "dark1"       : "#3c3836",
            "dark2"       : "#504945",
            "dark3"       : "#665c54",
            "dark4"       : "#7c6f64",

            "light1"      :"#ebdbb2",
            "light2"      :"#d5c4a1",
            "light3"      :"#bdae93",
            "light4"      :"#a89984",
        }

        self.assertTrue(colors_dict["dark1"], "#3c3836")
        self.assertTrue(colors.check_schema(colors_dict))

        colors_dict = colors_dict | {

            "gray"    : "#928374",

            "red_bright"    : "#fb4934",
            "green_bright"  : "#b8bb26",
            "yellow_bright" : "#fabd2f",
            "blue_bright"   : "#83a598",
            "purple_bright" : "#d3869b",
            "aqua_bright"   : "#8ec07c",
            "orange_bright" : "#fe8019",

            "red_faded"      : "#9d0006",
            "green_faded"    : "#79740e",
            "yellow_faded"   : "#b57614",
            "blue_faded"     : "#076678",
            "purple_faded"   : "#8f3f71",
            "aqua_faded"     : "#427b58",
            "orange_faded"   : "#af3a03"
        }
        self.assertTrue(colors_dict["gray"], "#928374")
        self.assertTrue(colors.check_schema(colors_dict))

        colors_dict = colors_dict | { "gray": None }
        self.assertEqual(colors_dict["gray"], None)
        self.assertTrue(colors.check_schema(colors_dict))

        fail_dict = {
            "red": "#123456"
        }
        self.assertFalse(colors.check_schema(fail_dict))

        fail_dict = colors_dict | { "red": None }
        self.assertEqual(fail_dict["red"], None)
        self.assertFalse(colors.check_schema(fail_dict))

        fail_dict = colors_dict | { "green": 42 }
        self.assertEqual(fail_dict["green"], 42)
        self.assertFalse(colors.check_schema(fail_dict))

        fail_dict = colors_dict | { "fail": False }
        self.assertEqual(fail_dict["fail"], False)
        self.assertFalse(colors.check_schema(fail_dict))

    def test_load_color(self):
        colors_dict = {
            "dark0"       : "#32302f",

            "light0"      : "#f2e5bc",

            "red"    : "#cc241d",
            "green"  : "#98971a",
            "yellow" : "#d79921",
            "blue"   : "#458588",
            "purple" : "#b16286",
            "aqua"   : "#689d6a",
            "orange" : "#d65d0e",

            "dark1"       : "#3c3836",
            "dark2"       : "#504945",
            "dark3"       : "#665c54",
            "dark4"       : "#7c6f64",

            "light1"      :"#ebdbb2",
            "light2"      :"#d5c4a1",
            "light3"      :"#bdae93",
            "light4"      :"#a89984",

            "gray"    : "#928374",

            "red_bright"    : "#fb4934",
            "green_bright"  : "#b8bb26",
            "yellow_bright" : "#fabd2f",
            "blue_bright"   : "#83a598",
            "purple_bright" : "#d3869b",
            "aqua_bright"   : "#8ec07c",
            "orange_bright" : "#fe8019",

            "red_faded"      : "#9d0006",
            "green_faded"    : "#79740e",
            "yellow_faded"   : "#b57614",
            "blue_faded"     : "#076678",
            "purple_faded"   : "#8f3f71",
            "aqua_faded"     : "#427b58",
            "orange_faded"   : "#af3a03"
        }
        color_data = colors.load_colors(colors_dict)
        self.assertTrue(color_data["red_faded"])
        self.assertTrue(color_data["red_bright"])
        self.assertTrue(color_data["gray"])
        self.assertTrue(color_data["dark0"])
        self.assertTrue(color_data["dark0"])
        self.assertTrue(color_data["light4"])
        self.assertTrue(color_data["light4"])
        self.assertTrue(color_data["bg"])
        self.assertTrue(color_data["foreground"])
        self.assertTrue(color_data["color00"])
        self.assertTrue(color_data["color01"])
        self.assertTrue(color_data["color1"])
        self.assertTrue(color_data["color15"])

if __name__ == "__main__":
    unittest.main()
