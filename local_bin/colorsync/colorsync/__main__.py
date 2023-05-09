import subprocess

from . import export
from . import colors

def reload():
    """Run scripts to reload all program's colorsheme config"""
    # subprocess.run(["sh", "./scripts/reload.sh"])
    subprocess.run(["cat", "./scripts/reload.sh"])

def main():

    config = colors.get()
    export.xrdb(config)
    reload()

if __name__ == "__main__":
    main()
