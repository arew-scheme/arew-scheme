#!/usr/bin/env python3
from io import BytesIO
from warcio.warcwriter import WARCWriter
from html2text import HTML2Text
from libzim.reader import File as ZIMFile
from urllib.parse import quote


handler = HTML2Text()
handler.ignore_links = True
handler.images_to_alt = True
html2text = handler.handle

with open('example.warc.wet.gz', 'wb') as output:
    writer = WARCWriter(output, gzip=True)
    with ZIMFile("data/wikipedia_en_simple_all_nopic_2020-12.zim") as reader:
        for uid in range(0, reader.article_count):
            if uid % 10_000 == 0:
                print("{} out of {}".format(uid, reader.article_count))

            article = reader.get_article_by_id(uid)
            try:
                if article.mimetype != "text/html":
                    continue
            except RuntimeError:
                continue

            if article.is_redirect:
                continue

            url = 'https://simple.wikipedia.org/wiki/{}'.format(quote(article.url))
            html = bytes(article.content).decode('utf8')
            text = html2text(html)
            payload = BytesIO(text.encode('utf8'))

            record = writer.create_warc_record(
                url,
                'conversion',
                payload=payload,
            )

            writer.write_record(record)
