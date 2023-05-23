import json

from .colors import Color

FILE_MAP = {
    "json": "colors.json",
    "sh": "colors.sh",
}

def save_colors(path: str, colors_data: dict[str, Color]) -> None:
    """save colors in the given directory path"""
    save_json(path, colors_data)
    save_sh(path, colors_data)

def save_json(path: str, colors_data: dict[str, Color]) -> None:
    with open(path + "/" + FILE_MAP["json"], "w") as dump_file:
        json.dump(colors_data, dump_file)

def save_sh(path: str, colors_data: dict[str, Color]) -> None:
    with open(path + "/" + FILE_MAP["sh"], "w") as dump_file:
        for k, v in colors_data.items():
            print(f"{k}=\"{v}\"", file=dump_file)
