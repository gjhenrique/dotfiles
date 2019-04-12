#!/usr/bin/python

import os
import buku

location = '~/.config/chromium/Default/Bookmarks'
bookmarks_database = os.path.expanduser(location)
db = buku.BukuDb()
db.load_chrome_database(bookmarks_database, buku.gen_auto_tag(), True)
