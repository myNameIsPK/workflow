import os

from . import config
from . import colors
from . import export

DEFAULT_CONFIG_PATH = os.environ["XDG_CONFIG_HOME"] + "/colorsync/config.json"

def main():
    config_opts = config.get_config(DEFAULT_CONFIG_PATH)
    colors_opts = config.get_color(config_opts["color_path"])
    colors_data = colors.load_colors(colors_opts, light=config_opts["light"])
    export.save_colors(config_opts["cache_path"], colors_data)

if __name__ == "__main__":
    main()
