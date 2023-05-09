import os

def xrdb(config: dict):
    """Export xrdb file"""
    with open(f'{os.environ["XDG_CONFIG_HOME"]}/x11/xresources.d/test', "w") as xrdb_file:
        print("! this gen from color file", file=xrdb_file)
        print(f'*.background: {config["special"]["background"]}', file=xrdb_file)
