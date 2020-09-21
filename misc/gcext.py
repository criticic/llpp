import xml.etree.ElementTree as ET
from datetime import date
tree = ET.parse('llpp.conf')
root = tree.getroot()

for doc in root.findall('doc'):
    # skip entries with bookmarks
    if doc.find('bookmarks'):
        continue
    # remove entries older than 2019.9.1
    ts = int(doc.get('last-visit'))
    dt = date.fromtimestamp(ts)
    if dt < date(2019, 9, 1):
        root.remove(doc)
    # remove entries whose path contains the keyword
    if "keyword" in doc.get('path'):
        root.remove(doc)

tree.write('llpp_new.conf')
