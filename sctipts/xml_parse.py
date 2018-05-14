from lxml import etree
from fileinput import FileInput
import pandas as pd


def fix_unicode(xmlfile):
    print("Fixing unicode...")
    with FileInput(xmlfile, inplace=True, backup='.bak') as file:
        for line in file:
            for pos in range(0, len(line)):
                if ord(line[pos]) < 32:
                    line = line[:pos] + " " + line[pos + 1:]
            print(line)


def parse_releases(parser, outfile, target_genre):
    print("Parsing xml...")
    data = []
    for event, element in parser:
        # Filtering duplicate releases
        master = element.xpath('master_id')
        master_id = master[0].attrib['is_main_release'] if len(master) > 0 else 'false'
        # Get release title
        release = element.xpath('title')[0].text + "[RELEASE]"
        genres = [g.text for g in element.xpath('styles/style')]
        if target_genre in genres and master_id == 'true':
            artists = [a.xpath('name')[0].text for a in element.xpath('artists/artist')]
            if len(artists) > 1:
                for artist in artists:
                    data.append([artist, release])
        element.clear()
    df = pd.DataFrame(data, columns=["release", "artist"])
    df.to_csv(outfile, index=False)


def add_members(infile, outfile):
    artist = input("artist: ")
    members = input("members: ").split(",")

    collabs = pd.read_csv(infile)
    mem_rows = []
    entries = collabs[collabs['artist'] == artist]
    for index, row in entries.iterrows():
        for member in members:
            mem_rows.append([member, row['release']])
    mem_rows_df = pd.DataFrame(mem_rows, columns=['artist','release'])
    df = pd.concat([collabs,mem_rows_df])
    df.to_csv(outfile, index=False)


def parse_artists(parser, infile, outfile):
    collabs = pd.read_csv(infile)
    print("Parsing xml...")
    mem_rows = []
    for event, element in parser:
        name = element.xpath('name')[0].text
        members = [m.text for m in element.xpath('members/name')]
        entries = collabs[collabs['artist'] == name]
        for index, row in entries.iterrows():
            for member in members:
                mem_rows.append([member, row['release']])
        element.clear()
    mem_rows_df = pd.DataFrame(mem_rows, columns=['artist','release'])
    df = pd.concat([collabs,mem_rows_df])
    df.to_csv(outfile, index=False)


def query_members(parser, artist):
    print("Searching...")
    root = parser.getroot()
    query = 'artist/name[text()="{}"]/parent::*'.format(artist)
    artist_node = root.xpath(query)[0]
    print(artist_node.xpath('name')[0].text)
    members = [m.text for m in artist_node.xpath('members/name')]
    for m in members:
        print("\t", m)


def get_release_dates(parser, artists_list):
    artists_dic = dict(zip(artists_list, [None] * len(artists_list)))
    for event, element in parser:
        year = element.xpath('released')[0].text[:4]
        artists = [a.xpath('name')[0].text for a in element.xpath('artists/artist')]
        for artist in artists:
            try:
                if artists_dic[artist] is None or artists_dic[artist] > year:
                    artists_dic[artists_dic] = year
            except KeyError:
                pass
    data = []
    for key, val in artists_dic.items():
        data.append([key, val])

    return data


if __name__ == "__main__":

    xmlfile = "raw/releases.xml"
    infile = "data/groups_release_2.csv"
    outfile = "data/techno_artist_release.csv"

    parser = etree.iterparse(xmlfile, tag="release")
    parse_releases(parser, outfile, "Techno")

    # Run before parsing an xml file
    # print("fixing unicode...")
    # fix_unicode(xmlfile)

    # parser = etree.iterparse(xmlfile, tag="artist")
    # print("Loading XML...")
    # parser = etree.parse(xmlfile)
    # query_members(parser, "Wu-Tang Clan")

