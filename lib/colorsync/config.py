import json
import os

def get_config(path: str) -> dict:
    with open(path) as config_file:
        config_dict = json.load(config_file)
        result = {}
        for k, v in config_dict.items():
            if type(v) == str:
                result[k] = os.path.expandvars(v)
            else:
                result[k] = v
        return result

def get_color(path: str) -> dict:
    with open(path) as colors_json:
        return json.load(colors_json)
