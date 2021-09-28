"""
Command line tools used in this project
"""
import argparse
import json
import sys
from contextlib import contextmanager
from typing import Optional, Tuple

from .json_to_dot import convert
from .make_p_to_json import parse_make_p


def get_parser():
    parser = argparse.ArgumentParser(
        description="To build a workflow diagram, run:\n\n\tmake -pq | tplcli db-to-json | tplcli json-to-dot | dot -Tpdf > workflow.pdf"
    )

    subparsers = parser.add_subparsers(dest="subparser_name")

    db_to_json_parser = subparsers.add_parser(
        "db-to-json", help="Convert Makefile db to json"
    )
    db_to_json_parser.add_argument(
        "filename",
        nargs="?",
        help="The filename to read the database from. Default is stdin",
    )
    db_to_json_parser.add_argument(
        "--output",
        "-o",
        nargs=1,
        help="The filename to save the database to. Default is stdout",
    )

    json_to_dot_parser = subparsers.add_parser(
        "json-to-dot",
        help="Convert json to dot",
    )
    json_to_dot_parser.add_argument(
        "filename",
        nargs="?",
        help="The filename to read the json from. Default is stdin",
    )
    json_to_dot_parser.add_argument(
        "--output",
        "-o",
        nargs=1,
        help="The filename to save the dotfile to. Default is stdout",
    )

    return parser


@contextmanager
def _open_or_yield(filename: Optional[str], mode: str = "rt"):
    if not filename or filename == "-":
        if "r" in mode:
            yield sys.stdin
        elif "w" in mode:
            yield sys.stdout
        else:
            raise ValueError(f"mode must contain r or w")
    else:
        with open(filename, mode) as infile:
            yield infile


def db_to_json_command(database: Optional[str], output: Optional[str]):
    """
    Convert a Makefile database into a json file representing the database
    """
    with _open_or_yield(database) as infile:
        retval = parse_make_p(infile)

    with _open_or_yield(output, "wt") as outfile:
        json.dump(retval, outfile)


def json_to_dot_command(jsonfile: Optional[str], output: Optional[str]):
    """
    Convert a json representation of a Makefile database to a dot representation
    """
    with _open_or_yield(jsonfile) as infile:
        with _open_or_yield(output, "wt") as outfile:
            convert(infile, outfile)


def main():
    parser = get_parser()
    args = parser.parse_args()

    if args.subparser_name == "db-to-json":
        db_to_json_command(args.filename, args.output[0] if args.output else "-")
    elif args.subparser_name == "json-to-dot":
        json_to_dot_command(args.filename, args.output[0] if args.output else "-")
    else:
        parser.print_help()

if __name__ == "__main__":
    main()
