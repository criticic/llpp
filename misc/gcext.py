import argparse
import os
import xml.etree.ElementTree as ET

parser = argparse.ArgumentParser()
parser.add_argument(
    '-i',
    dest='input',
    type=str,
    metavar='SOURCE',
    help='path to your llpp.conf; default to ~/.config/llpp.conf',
    default='~/.config/llpp.conf')
parser.add_argument(
    '-d',
    dest='timestamp',
    type=int,
    help='entries saved earlier than this date will be removed')
parser.add_argument(
    '-k',
    dest='keyword',
    type=str,
    help='entries with path matching the keyword will be removed')
parser.add_argument(dest='dest',
                    type=str,
                    metavar='DEST',
                    help='path to output config file')

args = parser.parse_args()
source = args.input
cts = args.timestamp
keyword = args.keyword
dest = args.dest

tree = ET.parse(os.path.expanduser(source))
root = tree.getroot()

print('Removing the following entries in llpp.conf:')
i = 0
for doc in root.findall('doc'):
    # skip entries with bookmarks
    if doc.find('bookmarks'):
        continue
    # remove entries older than 2019.9.1
    ts = int(doc.get('last-visit'))
    if cts is not None and ts < cts:
        print(doc.get('path'))
        root.remove(doc)
        i += 1
        continue
    # remove entries whose path contains the keyword
    if keyword is not None and keyword in doc.get('path'):
        print(doc.get('path'))
        root.remove(doc)
        i += 1

print(i, 'entries have been removed.',
      'The new configuration file has been saved to', dest)
tree.write(dest)
