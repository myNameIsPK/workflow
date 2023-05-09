import json

def get():
    """Get color config"""
    with open("color.json") as config_file:
        return json.load(config_file)
