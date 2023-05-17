import re

COLOR_SCHEMA = {
    "alpha" : int | None,

    "dark0"  : str,
    "dark1"  : str | None,
    "dark2"  : str | None,
    "dark3"  : str | None,
    "dark4"  : str | None,

    "gray" : str | None,

    "light0" : str,
    "light1" : str | None,
    "light2" : str | None,
    "light3" : str | None,
    "light4" : str | None,

    "red"    : str,
    "green"  : str,
    "yellow" : str,
    "blue"   : str,
    "purple" : str,
    "aqua"   : str,
    "orange" : str,

    "red_bright"    : str | None,
    "green_bright"  : str | None,
    "yellow_bright" : str | None,
    "blue_bright"   : str | None,
    "purple_bright" : str | None,
    "aqua_bright"   : str | None,
    "orange_bright" : str | None,

    "red_faded"    : str | None,
    "green_faded"  : str | None,
    "yellow_faded" : str | None,
    "blue_faded"   : str | None,
    "purple_faded" : str | None,
    "aqua_faded"   : str | None,
    "orange_faded" : str | None,
}

class Color():

    def __init__(self, raw_hex: str) -> None:
        self.hex = get_hex(raw_hex)

    def __str__(self) -> str:
        return self.hex

def get_hex(raw_hex: str) -> str:
    if re.match(r'^#?(?:[0-9a-fA-F]{3}){1,2}$', raw_hex) != None:
        raw_hex = re.sub(r'^#', '', raw_hex)
        if len(raw_hex) == 3:
            r = raw_hex[0]
            g = raw_hex[1]
            b = raw_hex[2]
            hex = f"#{r}{r}{g}{g}{b}{b}"
        else:
            hex = f"#{raw_hex.lower()}"
        return hex
    else:
        raise Exception("Invalid Color Hex code")

def check_schema(colors_dict: dict) -> bool:
    """check json schema"""
    for k, v  in colors_dict.items():
        if not k in COLOR_SCHEMA.keys():
            return False
        if not isinstance(v, COLOR_SCHEMA[k]):
            return False
    for k, v in COLOR_SCHEMA.items():
        if not isinstance(None, v):
            if not k in colors_dict.keys():
                return False
    return True

# TODO: generate optional color
def load_colors(colors_dict: dict, light: bool = False) -> dict[str, Color]:
    result = {}

    for k, v in colors_dict.items():
        if type(v) == str:
            result[k] = v

    BASE16_ORDER = [
        "black",
        "red",
        "green",
        "yellow",
        "blue",
        "purple",
        "aqua",
        "white"
    ]

    bg_color = "light" if light else "dark"
    fg_color = "dark" if light else "light"

    result["black"] = result[f"{bg_color}0"]
    result["black_bright"] = result[f"{bg_color}3"]
    result["black_faded"] = result["black_bright"]

    result["white"] = result[f"{fg_color}3"]
    result["white_bright"] = result[f"{fg_color}0"]
    result["white_faded"] = result["white_bright"]

    alt = "faded" if light else "bright"
    for i, c in enumerate(BASE16_ORDER):
        result[f"color{i}"] = result[c]

    for i, c in enumerate(BASE16_ORDER, start=8):
        result[f"color{i}"] = result[f"{c}_{alt}"]

    for i in range(0, 10):
        result[f"color0{i}"] = result[f"color{i}"]

    result["background"] = result[f"{bg_color}0"]
    result["bg"] = result[f"{bg_color}0"]
    result["bg0"] = result[f"{bg_color}0"]
    result["bg1"] = result[f"{bg_color}1"]
    result["bg2"] = result[f"{bg_color}2"]
    result["bg4"] = result[f"{bg_color}4"]

    result["foreground"] = result[f"{fg_color}0"]
    result["fg"] = result[f"{fg_color}0"]
    result["fg0"] = result[f"{fg_color}0"]
    result["fg1"] = result[f"{fg_color}1"]
    result["fg2"] = result[f"{fg_color}2"]
    result["fg3"] = result[f"{fg_color}3"]
    result["fg4"] = result[f"{fg_color}4"]

    result["cursor"] = result["red"]

    return result
