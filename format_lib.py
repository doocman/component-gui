#!/usr/bin/env python3

import argparse
import os
import subprocess
from pathlib import Path


def run_clang_format(clang_format_path, directories):
    """Runs clang-format on all .hpp and .cpp files in the given directories recursively."""
    for directory in directories:
        for root, _, files in os.walk(directory):
            for file in files:
                if file.endswith(('.hpp', '.cpp')):
                    file_path = Path(root) / file
                    print(f"Formatting: {file_path}")
                    try:
                        subprocess.run(
                            [clang_format_path, "-i", str(file_path), "--style=file"],
                            check=True,
                        )
                    except subprocess.CalledProcessError as e:
                        print(f"Error formatting {file_path}: {e}")
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
    directories = [file_path / Path(directory) for directory in directories]

    args = parser.parse_args()

    clang_format_path = args.clang_format

    print(f"Using clang-format: {clang_format_path}")
    for directory in directories:
        if not directory.is_dir():
            print(f"Error: {directory} is not a valid directory.")
            return

    run_clang_format(clang_format_path, directories)


if __name__ == "__main__":
    main()
