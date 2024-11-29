#!/usr/bin/env python3

import argparse
import os
import subprocess
import re
from pathlib import Path

def run_clang_format(clang_format_path, directories):
    """Runs clang-format on all .hpp and .cpp files in the given directories recursively."""
    file_list = []
    for directory in directories:
        for root, _, files in os.walk(directory):
            has_printed_root = False
            for file in files:
                if file.endswith(('.hpp', '.cpp')):
                    file_path = Path(root) / file
                    file_list.append(file_path)
                    if not has_printed_root:
                        print(f"Formatting {root}/*")

    try:
        subprocess.run(
            [clang_format_path, "-i", "--style=file"] + file_list,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        print(f"Error formatting {file_list}: {e}")
    except FileNotFoundError:
        print(f"clang-format not found at: {clang_format_path}")
        return


def main():
    file_path = Path(os.path.realpath(__file__)).parent
    parser = argparse.ArgumentParser(
        description="Run clang-format on .hpp and .cpp files in specified directories recursively."
    )
    parser.add_argument(
        "--clang-format",
        default="clang-format",
        help="Path to clang-format executable (default: system-installed clang-format).",
    )

    directories = ["c++-src", "examples/c++", "tests"]
    directories = [Path(directory) for directory in directories]

    args = parser.parse_args()

    clang_format_path = args.clang_format

    clang_version_res = subprocess.run([clang_format_path, "--version"], check=True, stdout=subprocess.PIPE, text=True)
    pattern = re.compile("version ([0-9]+)\.")
    version_match = pattern.search(clang_version_res.stdout)
    if version_match:
        version = int(version_match.group(1))
        if version < 18:
            print(f"This library requires clang-format to be >= 18.x.x (at {clang_format_path})")
            return
        else:
            print(f"Using clang-format: {clang_format_path}, version: {version}")
    else:
        print(f"Could not parse the version from clang-format: {clang_version_res.stdout}")
        return
    for directory in directories:
        if not directory.is_dir():
            print(f"Error: {directory} is not a valid directory.")
            return

    run_clang_format(clang_format_path, directories)


if __name__ == "__main__":
    main()
