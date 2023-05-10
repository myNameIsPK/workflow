import os

from jinja2 import Environment, PackageLoader, select_autoescape
env = Environment(
    loader=PackageLoader("colorsync"),
    autoescape=select_autoescape()
)

CACHE_DIRECTORY=f'{os.environ["XDG_CACHE_HOME"]}/colorsync'

if not os.path.exists(CACHE_DIRECTORY):
   os.makedirs(CACHE_DIRECTORY)

def xrdb(config: dict):
    """Export xrdb file"""
    with open(f'{CACHE_DIRECTORY}/colorsync.xresources', "w") as xrdb_file:
        template = env.get_template("colorsync.xresources")
        print(template.render(config), file=xrdb_file)

def all(config: dict):
    """Export to all file type"""
    print(config)
    xrdb(config)
