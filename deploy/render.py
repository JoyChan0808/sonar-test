#!/usr/bin/python

import re
import argparse
from datetime import datetime

# Copy from: https://stackoverflow.com/a/50456924/1965356
envre = re.compile(r'''^([^\s=]+)=(?:[\s"']*)(.+?)(?:[\s"']*)$''')


def work(args):
    now = datetime.now()
    build_time = now.strftime("%Y-%m-%d %H:%M:%S")

    payload = {}
    with open(args.env) as fp:
        for line in fp:
            match = envre.match(line)
            if match is None:
                continue
            payload[match.group(1)] = match.group(2)

    payload["BUILD_TIME"] = build_time
    payload["SERVICE_NAME"] = args.name
    payload["IMAGE"] = args.image
    payload["VERSION"] = args.version

    tpl = open(args.tpl).read()
    print(tpl.format(**payload))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Render k8s deployment Template')
    parser.add_argument('--tpl', '-t', help='template file')
    parser.add_argument('--env', '-e', help='env file, use `=` to separate key and value')
    parser.add_argument('--name', '-n', help='service name')
    parser.add_argument('--image', '-i', help='docker image name')
    parser.add_argument('--version', '-v', help='github version')

    args = parser.parse_args()
    work(args)
