import json

def dict_flatten(d: dict) -> dict:
    new_dict = {}
    for key, val in d.items():
        if isinstance(val, dict):
            new_dict.update(dict_flatten(val))
        else:
            new_dict[key] = val
    return new_dict

def get() -> dict:
    """return color config"""
    with open("color.json") as config_file:
        return dict_flatten(json.load(config_file))
